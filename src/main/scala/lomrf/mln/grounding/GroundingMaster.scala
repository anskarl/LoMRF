/*
 * o                        o     o   o         o
 * |             o          |     |\ /|         | /
 * |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 * |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 * O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *             |
 *          o--o
 * o--o              o               o--o       o    o
 * |   |             |               |    o     |    |
 * O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 * |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 * o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 * Logical Markov Random Fields.
 *
 * Copyright (C) 2012  Anastasios Skarlatidis.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package lomrf.mln.grounding

import java.util.concurrent.CountDownLatch

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import gnu.trove.map.TIntObjectMap
import gnu.trove.set.TIntSet
import gnu.trove.set.hash.TIntHashSet
import lomrf._
import lomrf.logic.{AtomSignature, AtomicFormula, Clause, Variable}
import lomrf.mln.model.MLN
import lomrf.util.Logging

import scala.collection.breakOut

/**
 * @author Anastasios Skarlatidis
 */
private final class GroundingMaster(mln: MLN, latch: CountDownLatch, noNegWeights: Boolean) extends Actor with Logging {

  private val _variables2Cliques = new Array[TIntObjectMap[TIntHashSet]](processors)
  private val _cliques = new Array[TIntObjectMap[CliqueEntry]](processors)
  private val _queryAtomIDs = new Array[TIntSet](processors)
  private val atomsDB = new Array[TIntSet](processors)


  private var clauseCounter = -mln.queryAtoms.size

  private var cliqueBatchesCounter = 0
  private var atomBatchesCounter = 0
  private var atomIDBatchesCounter = 0
  private var clausesBatchSize = 0
  private var completed = false

  private var workerIdx = 0
  private var cliqueStartID = 0
  private var groundingIterations = 1
  private var remainingClauses: Set[Clause] = mln.clauses
  private var atomSignatures: Set[AtomSignature] = mln.queryAtoms.toSet


  private val atomRegisters: Array[ActorRef] = (
    for (i <- 0 until processors)
    yield context.actorOf(Props(new AtomRegisterWorker(i, this.self)), name = "atom_register-" + i)
    )(breakOut)

  private val cliqueRegisters: Array[ActorRef] = (
    for (i <- 0 until processors)
    yield context.actorOf(Props(new CliqueRegisterWorker(i, this.self, atomRegisters)), name = "clique_register-" + i)
    )(breakOut)

  private val clauseGroundingWorkers: Array[ActorRef] = {
    val n = mln.queryAtoms.size + mln.clauses.size
    (for (i <- 0 until (if (n <= processors) n else processors))
    yield context.actorOf(Props(new GroundingWorker(mln, cliqueRegisters, noNegWeights)), name = "grounding_worker-" + i)
      )(breakOut)
  }

  override def preStart() {
    info("Number of processors: " + lomrf.processors
      + "\n\tTotal atom registry workers to use: " + atomRegisters.length
      + "\n\tTotal clique registry workers to use: " + cliqueRegisters.length
      + "\n\tTotal grounding workers to use: " + clauseGroundingWorkers.length)

    // To make sure that all ground query predicates will be stored in the network,
    // insert all ground query predicates as zero weighted unit clauses
    for (signature <- mln.queryAtoms) {
      val terms =
        mln.schema(signature).view.zipWithIndex.map {
          case (argType: String, idx: Int) => Variable("v" + idx, argType, idx)
        }.toList
      remainingClauses += Clause(0, AtomicFormula(signature.symbol, terms))
    }

    performGrounding()
  }

  private def performGrounding() {
    var remaining = Set[Clause]()
    var counter = 0
    clauseCounter = 0
    for (clause <- remainingClauses) {
      if (clause.literals.exists(literal => atomSignatures.contains(literal.sentence.signature))) {
        clauseGroundingWorkers(workerIdx) ! Ground(clause, atomSignatures, atomsDB)
        workerIdx = if (workerIdx == clauseGroundingWorkers.length - 1) 0 else workerIdx + 1
        counter += 1
      }
      else remaining = remaining + clause

    }

    debug("(MASTER) Grounding iteration " + groundingIterations + " --- total " + counter + " clause(s) selected to ground.")
    groundingIterations += 1


    remainingClauses = remaining
    clausesBatchSize = counter

    if (counter == 0) {
      // Start Phase 2 :
      cliqueRegisters.foreach(_ ! GRND_Completed)
    }
  }

  def receive = {

    case ClauseGroundingCompleted(clause, collectedSignatures) =>
      clauseCounter += 1
      atomSignatures = collectedSignatures ++ atomSignatures

      if (clauseCounter == clausesBatchSize)
        atomRegisters.foreach(_ ! GRND_Iteration_Completed)


    case CollectedAtomIDs(index, atomIDs) =>
      atomIDBatchesCounter += 1
      atomsDB(index) = atomIDs
      if (atomIDBatchesCounter == atomRegisters.length) {
        atomIDBatchesCounter = 0
        if (remainingClauses.isEmpty) {
          // Start Phase 2 :
          cliqueRegisters.foreach(_ ! GRND_Completed)
        }
        else {
          //continue with the remaining clauses
          debug(
            "(MASTER) Continue with the remaining clauses..." +
              "\n(MASTER) AtomSignatures to focus grounding: " + atomSignatures.map(_.toString).reduceLeft(_ + ", " + _) +
              "\n(MASTER) Remaining clauses to ground: " + remainingClauses.size + " {" +
              remainingClauses.view.zipWithIndex.foldLeft("\n")((rest, entry) => rest + "\n" + entry._2 + " --- " + entry._1) +
              "\n"
          )
          performGrounding()
        }
      }

    case NumberOfCliques(index, size) =>
      sender ! StartID(cliqueStartID)
      sender ! PoisonPill
      cliqueStartID += size

    case CollectedCliques(index, cliques) =>
      _cliques(index) = cliques
      cliqueBatchesCounter += 1
      if (cliqueBatchesCounter == processors) atomRegisters.foreach(_ ! PoisonPill)

    case AtomsBatch(index, registry, queryAtoms) =>
      _variables2Cliques(index) = registry
      _queryAtomIDs(index) = queryAtoms
      atomBatchesCounter += 1

      if (atomBatchesCounter == processors) killAll()


    case REQUEST_RESULTS => if (completed) sender ! Result(_cliques, _variables2Cliques, _queryAtomIDs) else sender ! None
    case msg => fatal("Master --- Received an unknown message '" + msg + "' from " + sender)
  }

  private def killAll() {
    cliqueRegisters.foreach(_ ! PoisonPill)
    atomRegisters.foreach(_ ! PoisonPill)
    clauseGroundingWorkers.foreach(_ ! PoisonPill)
    completed = true
    latch.countDown()
  }

}