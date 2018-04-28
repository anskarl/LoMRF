/*
 *
 *  o                        o     o   o         o
 *  |             o          |     |\ /|         | /
 *  |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 *  |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 *  O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *              |
 *           o--o
 *  o--o              o               o--o       o    o
 *  |   |             |               |    o     |    |
 *  O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 *  |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 *  o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 *  Logical Markov Random Fields (LoMRF).
 *     
 *
 */

package lomrf.mln.grounding


import akka.actor.{Actor, ActorRef}
import gnu.trove.map.hash.TIntObjectHashMap
import gnu.trove.set.hash.TIntHashSet
import com.typesafe.scalalogging.LazyLogging


/**
 * AtomRegisterWorker collects a partition of ground atoms, represented by integer values, as well as in which ground
 * clauses the they appear.
 *
 * @param index the worker index (since we have multiple AtomRegisterWorker instances), it also represents the
 *              partition index.
 * @param master reference to master actor, it is required in order to send the results back to master actor.
 */
final class AtomRegisterWorker private(val index: Int, master: ActorRef) extends Actor with LazyLogging {

  import messages._

  /**
   * A collection that keeps the relation between ground atoms and ground clauses.
   */
  private lazy val atomID2CliqueID = new TIntObjectHashMap[TIntHashSet]()

  /**
   * A collection of ground atom ids that have been produced by query predicates.
   */
  private val queryAtomIDs = new TIntHashSet()

  /**
   * Collection of ground atom ids, discovered in previous iterations. Initially, the set is empty.
   */
  private val atomIDs = new TIntHashSet()

  /**
   * Collection of ground atom ids, discovered in the current iteration. Initially, the set is empty.
   */
  private var buffer = new TIntHashSet()

  /**
   * AtomRegisterWorker actor behaviour:
   * <ul>
   *   <li>Collects ground query atoms from GroundingWorkers</li>
   *   <li>Collects relations between ground atoms from CliqueRegisters</li>
   * </ul>
   *
   * @return a partial function with the actor logic for collecting ground atoms and their relations to ground clauses.
   */
  def receive = {

    /**
     * Collect a grounding of a query atom directly from a GroundingWorker
     */
    case QueryVariable(atomID) => queryAtomIDs.add(atomID)

    /**
     * When a grounding iteration is completed (that, is determined by the grounding Master), Master sends this message
     * and this worker responds by sending back all collected atom ids.
     *
     * [Master] -> GRND_Iteration_Completed
     * CollectedAtomIDs -> [Master]
     */
    case ITERATION_COMPLETED =>
      atomIDs.addAll(buffer)
      master ! CollectedAtomIDs(index, atomIDs)
      buffer = new TIntHashSet()

    /**
     * (1) Collect an integer that represents a possible grounding of an atom, from a CliqueRegister. All integer
     * values are accepted except zero, since it is the reserved value for representing the absence of a ground atom
     * in the MRF.
     *
     * (2) Collect the relation between a ground atom and a ground clause.
     *
     */
    case RegisterAtom(atomID, cliqueID) =>
      assert(atomID != 0, "atomID cannot be equal to zero.")

      buffer.add(atomID)

      val cliqueSet = atomID2CliqueID.get(atomID)
      if (cliqueSet == null) {
        val set = new TIntHashSet()
        set.add(cliqueID)
        atomID2CliqueID.put(atomID, set)
      }
      else cliqueSet.add(cliqueID)


    case msg =>
      logger.error("AtomRegister[" + index + "] --- Received an unknown message: " + msg)
  }

  /**
   * Before AtomRegistryWorker is stopped, it will first sent the results to the master actor.
   */
  override def postStop() {
    master ! AtomsBatch(index, atomID2CliqueID, queryAtomIDs)
  }
}

private object AtomRegisterWorker {

  def apply(index: Int)(implicit master: ActorRef) = new AtomRegisterWorker(index, master)

}
