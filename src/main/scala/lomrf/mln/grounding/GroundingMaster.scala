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
import scalaxy.streams.optimize

/**
 * GroundingMaster orchestrates the grounding procedure, i.e., the procedure in which an MLN theory is translated to a
 * ground Markov Random Field.
 *
 * @param mln the MLN theory instance
 * @param latch countdown latch variable to reduce its value when grounding is completed
 * @param noNegWeights when its true, all negative weights are eliminated (using de Morgans law)
 * @param eliminateNegatedUnit when its true, unit clauses having a negated literal are eliminated (using de Morgans law)
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

  import messages._

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
   * Machine Learning
   */
  private lazy val dependencyMap = MPartitionedData[DependencyMap](nPar)

  /**
   * Utility counter variable to keep track the number of clauses to ground in the current grounding step.
   * @see [[GroundingMaster#performGrounding]]
   */
  private var clauseCounter = -mln.queryAtoms.size

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
   * The number with which the index of produced cliques (i.e., ground clauses) begins
   */
  private var cliqueStartID = 0

  /**
   * Counts the number of grounding iterations that have been processed.
   */
  private var groundingIterations = 1

  /**
   * This variable contains a collection of clauses (accompanied by their index in the MLN theory) that are not yet
   * grounded. Initially, this variable contains all MLN clauses.
   */
  private var remainingClauses: Vector[(Clause, Int)] = mln.clauses.zipWithIndex

  /**
   * Collected set of atom signatures. In the beginning, this set contains the signatures of query atoms.
   * In the end of the grounding process, this set should contain the signatures of FOL atoms that have
   * at least one grounding in the ground Network.
   */
  private var atomSignatures: Set[AtomSignature] = mln.queryAtoms.toSet

  // implicit definition master actor reference, it is required for some functions below.
  private implicit val master = this.self

  /**
   * A partitioned collection of instantiated actor references for registering ground atoms
   */
  private val atomRegisters =
    PartitionedData(nPar,
      partitionIndex =>
        context.actorOf(
          Props(AtomRegisterWorker(partitionIndex)), name = "atom_register-" + partitionIndex))

  /**
   * A partitioned collection of instantiated actor references for registering ground clauses (cliques)
   */
  private val cliqueRegisters =
    PartitionedData(nPar,
      partitionIndex =>
        context.actorOf(
          Props(CliqueRegisterWorker(partitionIndex, atomRegisters, createDependencyMap)), name = "clique_register-" + partitionIndex))

  /**
   * A partitioned collection of instantiated actor references of clause grounders
   */
  private val clauseGroundingWorkers =
    PartitionedData(nPar,
      partitionIndex =>
        context.actorOf(
          Props(GroundingWorker(mln, cliqueRegisters, noNegWeights, eliminateNegatedUnit)), name = "grounding_worker-" + partitionIndex))


  /**
   * Before starting the grounding phase:
   * <ul>
   *   <li> we log some useful information (e.g., the number of instantiated actors per worker type),</li>
   *   <li> and we finally add all query FOL atoms as unit clauses with zero weights. With that addition, we make
   *   sure that the groundings of all query atoms will appear in the MRF. Consider, for example, that a specific
   *   ground clause becomes tautological (e.g, by an evidence fact) and thus is been eliminated by the final MRF.
   *   In situations where a grounding of a query atom appears only in this ground clause, that specific ground atom
   *   will also eliminated from the final MRF. By adding these utility unit clauses we simply force to have its
   *   corresponding random variable in the MRF state.
   *   </li>
   * </ul>
   *
   * Thereafter, we begin grounding :)
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
   * This function performs a single grounding step.
   */
  private def performGrounding() {

    // utility var to hold remaining clauses, i.e., clauses that are not being ground in this phase
    var remaining = Vector[(Clause, Int)]()

    // counter to keep track how many clauses are send for grounding
    var counter = 0

    // reset the global clause counter
    clauseCounter = 0

    optimize{
      for ((clause, clauseIndex) <- remainingClauses) {

        // When the signature of at least one literal is included in the collection of atom signatures (see the
        // var atomSignatures), then this clause is send for grounding.
        if (clause.literals.exists(literal => atomSignatures.contains(literal.sentence.signature))) {

          // send the clause to the corresponding worker
          clauseGroundingWorkers.indexOf(workerIdx) ! Ground(clause, clauseIndex, atomSignatures, atomsDB)

          // reset the index to position zero when the worker index is pointing to the last worker,
          // otherwise increment by one
          workerIdx = if (workerIdx == clauseGroundingWorkers.length - 1) 0 else workerIdx + 1

          // increment the number of clauses being send for grounding
          counter += 1
        }
        // Otherwise, add the current clause to the collection of remaining clauses to be considered for grounding
        // in the next call for grounding
        else remaining :+=(clause, clauseIndex)
      }
    }

    debug("(MASTER) Grounding iteration " + groundingIterations + " --- total " + counter + " clause(s) selected to ground.")

    // increment by one the global counter of grounding iterations.
    groundingIterations += 1

    // set global remaining clauses collection
    remainingClauses = remaining

    // the batch size of the clauses send for grounding
    clausesBatchSize = counter

    // When nothing is send for grounding start the second phase, since the grounding of the MLN is being completed.
    // Please note that if counter is zero and the collection of remaining clauses is not empty, then the remaining ones
    // are not associated directly or indirectly with query atoms.
    if (counter == 0) optimize{
      remaining.foreach {
        case (clause,idx) =>
          warn(s"Clause '$clause' is being ignored, since is not associated directly or indirectly with query atoms.")
      }
      cliqueRegisters.partitions.foreach(_ ! GRND_Completed)
    }
  }

  def receive = {

    case ClauseGroundingCompleted(clause, collectedSignatures) =>
      clauseCounter += 1
      atomSignatures = collectedSignatures ++ atomSignatures

      if (clauseCounter == clausesBatchSize) optimize {
        atomRegisters.partitions.foreach(_ ! GRND_Iteration_Completed)
      }


    case CollectedAtomIDs(index, atomIDs) =>
      atomIDBatchesCounter += 1
      atomsDB(index) = atomIDs
      if (atomIDBatchesCounter == atomRegisters.length) {
        atomIDBatchesCounter = 0
        if (remainingClauses.isEmpty) optimize{
          // Start Phase 2 :
          cliqueRegisters.partitions.foreach(_ ! GRND_Completed)
        }
        else {
          //continue with the remaining clauses
          debug(
            "(MASTER) Continue with the remaining clauses..." +
              "\n(MASTER) AtomSignatures to focus grounding: " + atomSignatures.mkString(", ") +
              "\n(MASTER) Remaining clauses to ground: " + remainingClauses.size + " {" +
              remainingClauses.view.zipWithIndex.foldLeft("\n")((rest, entry) => rest + "\n" + entry._2 + " --- " + entry._1) +
              "\n"
          )
          performGrounding()
        }
      }

    case NumberOfCliques(partitionIndex, size) =>
      sender ! StartID(cliqueStartID)
      sender ! PoisonPill
      cliqueStartID += size

    case CollectedCliques(partitionIndex, cliquesPart, dependencyMapPart) =>
      this.cliques(partitionIndex) = cliquesPart

      if(createDependencyMap)
        this.dependencyMap(partitionIndex) = dependencyMapPart.getOrElse(fatal("dependencyMap is note defined."))

      cliqueBatchesCounter += 1
      if (cliqueBatchesCounter == nPar) optimize{
        atomRegisters.partitions.foreach(_ ! PoisonPill)
      }

    case AtomsBatch(partitionIndex, registry, queryAtoms) =>
      variables2Cliques(partitionIndex) = registry
      queryAtomIDs(partitionIndex) = queryAtoms
      atomBatchesCounter += 1

      /**
       * The grounding process is complete and we collected everything from workers.
       * Stop all workers, mark the internal state as completed and reduce the latch.
       */
      if (atomBatchesCounter == nPar) {

        optimize{
          for(i <- 0 until nPar){
            cliqueRegisters.indexOf(i) ! PoisonPill
            atomRegisters.indexOf(i) ! PoisonPill
            clauseGroundingWorkers.indexOf(i) ! PoisonPill
          }
        }
        completed = true
        latch.countDown()
      }


    case REQUEST_RESULTS =>
      if (completed) sender ! Result(cliques, variables2Cliques, queryAtomIDs, if(createDependencyMap) Some(dependencyMap) else None)
      else sender ! None

    case msg =>
      debug("Master --- Received an unknown message '" + msg + "' from " + sender)
  }


}
