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
import auxlib.log.Logging
import gnu.trove.map.TIntObjectMap
import gnu.trove.set.TIntSet
import gnu.trove.set.hash.TIntHashSet
import lomrf._
import lomrf.logic.{AtomSignature, AtomicFormula, Clause, Variable}
import lomrf.mln.model.MLN
import lomrf.util.collection.PartitionedData
import lomrf.util.collection.mutable.{PartitionedData => MPartitionedData}

/**
 * GroundingMaster orchestrates the grounding procedure, i.e., the procedure in which an MLN theory is translated to a
 * ground Markov Random Field.
 *
 * @param mln the MLN theory instance
 * @param latch countdown latch variable to reduce its value when grounding is completed
 * @param noNegWeights when its true, all negative weights are eliminated (using de Morgans law)
 * @param eliminateNegatedUnit when its true, unit clauses having a negated literal are ellimintated (using de Morgans law)
 * @param createDependencyMap when its true, a dependency map is created during grounding (useful for weight learning)
 * @param parRatio parallelization ratio to use. Should be at least 1.0f, otherwise will be ignored. This value affects
 *                 the amount of partition/actors to instantiate per worker type.
 */
private final class GroundingMaster(mln: MLN,
                                    latch: CountDownLatch,
                                    noNegWeights: Boolean = false,
                                    eliminateNegatedUnit: Boolean = false,
                                    createDependencyMap: Boolean = false,
                                    parRatio: Float = 1f) extends Actor with Logging {

  /**
   * Number of parallel instances to create per worker type. Also the number of partition to
   * create for each PartitionedData instance
   */
  private val nPar = math.max((parRatio * processors).toInt, processors)

  /**
   * Partitioned Map that associates variables (i.e., ground atoms) with the set of cliques (i.e., ground clauses) that
   * they appear. Variables and cliques are represented by their unique ID (integer number).
   */
  private val variables2Cliques = MPartitionedData[TIntObjectMap[TIntHashSet]](nPar)

  /**
   * Partitioned Map that associates clique IDs with CliqueEntries (object representing a clique --- i.e., ground clause)
   */
  private val cliques =  MPartitionedData[TIntObjectMap[CliqueEntry]](nPar)

  /**
   * Partitioned Set of collected query variable IDs (i.e., ground query atoms)
   */
  private val queryAtomIDs = MPartitionedData[TIntSet](nPar)

  /**
   * Partitioned Set of collected variable IDs (i.e., ground atoms)
   */
  private val atomsDB = MPartitionedData[TIntSet](nPar)

  /**
   * Partitioned Map that represents the relations between FOL clauses and their groundings. It is useful for
   * Machine Learing
   */
  private lazy val dependencyMap = MPartitionedData[DependencyMap](nPar)

  /**
   * Utility counter variable to keep track the number of clauses to ground.
   */
  private var clauseCounter = 0// -mln.queryAtoms.size

  /**
   * Utility counter variable to keep track the number of colleted clique batches
   */
  private var cliqueBatchesCounter = 0

  /**
   * Utility counter variable to keep track the number of colleted atom batches
   */
  private var atomBatchesCounter = 0

  /**
   * Utility counter variable to keep track the number of colleted ground atom ID batches
   */
  private var atomIDBatchesCounter = 0

  /**
   * Number of clause batches
   */
  private var clausesBatchSize = 0

  /**
   * Utility flag that indicates when the grounding is completed
   */
  private var completed = false

  /**
   * Utility variable holding the current index of a worker
   */
  private var workerIdx = 0

  /**
   *
   */
  private var cliqueStartID = 0

  /**
   *
   */
  private var groundingIterations = 1

  /**
   *
   */
  private var remainingClauses: Vector[(Clause, Int)] = mln.clauses.zipWithIndex

  /**
   *
   */
  private var atomSignatures: Set[AtomSignature] = mln.queryAtoms.toSet

  private implicit val master = this.self

  /**
   * A partitioned collection of instatiated actor references for registering ground atoms
   */
  private val atomRegisters: PartitionedData[ActorRef]  =
    PartitionedData(processors,
      partitionIndex =>
        context.actorOf(
          Props(AtomRegisterWorker(partitionIndex)), name = "atom_register-" + partitionIndex))

  /**
   * A partitioned collection of instatiated actor references for registering ground clauses (cliques)
   */
  private val cliqueRegisters: PartitionedData[ActorRef] =
    PartitionedData(processors,
      partitionIndex =>
        context.actorOf(
          Props(CliqueRegisterWorker(partitionIndex, atomRegisters, createDependencyMap)), name = "clique_register-" + partitionIndex))

  /**
   * A partitioned collection of instatiated actor references of clause grounders
   */
  private val clauseGroundingWorkers: PartitionedData[ActorRef] =
    PartitionedData(processors,
      partitionIndex =>
        context.actorOf(
          Props(GroundingWorker(mln, cliqueRegisters, noNegWeights, eliminateNegatedUnit)), name = "grounding_worker-" + partitionIndex))


  /**
   * Initially
   */
  override def preStart() {

    info("Number of processors: " + lomrf.processors
      + "\n\tParallelization size is set to: " + nPar
      + "\n\tTotal atom registry workers to use: " + atomRegisters.length
      + "\n\tTotal clique registry workers to use: " + cliqueRegisters.length
      + "\n\tTotal grounding workers to use: " + clauseGroundingWorkers.length)

    val numberOfClauses = remainingClauses.size

    // To make sure that all ground query predicates will be stored in the network,
    // insert all ground query predicates as zero weighted unit clauses
    for ((signature, qidx) <- mln.queryAtoms.zipWithIndex) {
      val terms =
        mln
          .predicateSchema(signature)
          .view
          .zipWithIndex
          .map {
            case (argType: String, idx: Int) => Variable("v" + idx, argType, idx)
          }
          .toVector

      remainingClauses :+= (Clause(0, AtomicFormula(signature.symbol, terms)), numberOfClauses + qidx)
    }

    performGrounding()
  }

  /**
   * Grounding step
   */
  private def performGrounding() {
    var remaining = Vector[(Clause, Int)]()
    var counter = 0
    clauseCounter = 0
    for ((clause, clauseIndex) <- remainingClauses) {
      if (clause.literals.exists(literal => atomSignatures.contains(literal.sentence.signature))) {
        clauseGroundingWorkers(workerIdx) ! Ground(clause, clauseIndex, atomSignatures, atomsDB)
        workerIdx = if (workerIdx == clauseGroundingWorkers.length - 1) 0 else workerIdx + 1
        counter += 1
      }
      else remaining :+=(clause, clauseIndex)


    }

    debug("(MASTER) Grounding iteration " + groundingIterations + " --- total " + counter + " clause(s) selected to ground.")
    groundingIterations += 1

    remainingClauses = remaining
    clausesBatchSize = counter

    if (counter == 0) {
      // Start Phase 2 :
      cliqueRegisters.partitions.foreach(_ ! GRND_Completed)
    }
  }

  def receive = {

    case ClauseGroundingCompleted(clause, collectedSignatures) =>
      clauseCounter += 1
      atomSignatures = collectedSignatures ++ atomSignatures

      if (clauseCounter == clausesBatchSize)
        atomRegisters.partitions.foreach(_ ! GRND_Iteration_Completed)


    case CollectedAtomIDs(index, atomIDs) =>
      atomIDBatchesCounter += 1
      atomsDB(index) = atomIDs
      if (atomIDBatchesCounter == atomRegisters.length) {
        atomIDBatchesCounter = 0
        if (remainingClauses.isEmpty) {
          // Start Phase 2 :
          cliqueRegisters.partitions.foreach(_ ! GRND_Completed)
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

    case CollectedCliques(index, cliquesPart, dependencyMapPart) =>
      this.cliques(index) = cliquesPart

      if(createDependencyMap)
        this.dependencyMap(index) = dependencyMapPart.getOrElse(fatal("dependencyMap is note defined."))

      cliqueBatchesCounter += 1
      if (cliqueBatchesCounter == processors) atomRegisters.partitions.foreach(_ ! PoisonPill)

    case AtomsBatch(index, registry, queryAtoms) =>
      variables2Cliques(index) = registry
      queryAtomIDs(index) = queryAtoms
      atomBatchesCounter += 1

      /**
       * The grounding process is complete!!!
       * Stop all workers, mark the internal state as completed and reduce the latch.
       */
      if (atomBatchesCounter == processors) {

        cliqueRegisters.partitions.foreach(_ ! PoisonPill)
        atomRegisters.partitions.foreach(_ ! PoisonPill)
        clauseGroundingWorkers.partitions.foreach(_ ! PoisonPill)

        completed = true
        latch.countDown()
      }


    case REQUEST_RESULTS =>
      if (completed) sender ! Result(cliques, variables2Cliques, queryAtomIDs, if(createDependencyMap) Some(dependencyMap) else None)
      else sender ! None

    case msg => debug("Master --- Received an unknown message '" + msg + "' from " + sender)
  }


}
