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

import java.{util => jutil}

import akka.actor.{Actor, ActorRef}
import com.typesafe.scalalogging.LazyLogging
import gnu.trove.list.array.TIntArrayList
import gnu.trove.map.TIntFloatMap
import gnu.trove.map.hash.{TIntFloatHashMap, TIntObjectHashMap}
import lomrf._
import lomrf.util.collection.IndexPartitioned
import lomrf.util.logging.Implicits._
import scala.language.postfixOps
import scalaxy.streams.optimize

/**
 * CliqueRegisterWorker stores a partition of ground clauses that result from grounding workers.
 *
 * @param index the worker index (since we have multiple CliqueRegisterWorker instances),
 *              it also represents the partition index.
 * @param master reference to master actor, it is required in order to send the results back to master actor.
 * @param atomRegisters partitioned collection of AtomRegisterWorker actors, in order to send messages about ground
 *                      atom ids and their relation to ground clauses.
 * @param createDependencyMap when it is true the worker stores additional information about the relations between
 *                            FOL clauses and their groundings.
 */
final class CliqueRegisterWorker private(val index: Int,
                                         master: ActorRef,
                                         atomRegisters: IndexPartitioned[ActorRef],
                                         createDependencyMap: Boolean) extends Actor with LazyLogging {

  import messages._
  import context._

  private var hashCode2CliqueIDs = new TIntObjectHashMap[TIntArrayList]()
  private var cliques = new TIntObjectHashMap[CliqueEntry](DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)


  /**
   * This structure is useful when Machine Learning is used, it represents the relations between FOL clauses and their
   * groundings. Specifically, the structure stores for each ground clause the id of the FOL clause that becomes, as
   * well as how many times the this ground class is generated by the same FOL clause (freq). In a nutshell, the
   * structure of the 'cliqueDependencyMap' is the following:
   * {{{
   *  Map [ groundClause[ID:Int] -> Map [Clause[ID:Int] -> Freq:Int]]
   * }}}
   *
   * Please note that when the 'Freq' number is negative, then we implicitly declare that the  weight of the
   * corresponding FOL 'Clause[ID:Int]' has been inverted during the grounding process.
   */
  private var dependencyMap: DependencyMap = _

  // utility variable to count ground clauses, and to use as an internal identity.
  private var cliqueID = 0


  /**
   * At the beginning, initialize the local dependency map (if it is set to store clause dependencies)
   */
  override def preStart(): Unit = {
    if (createDependencyMap)
      dependencyMap = new TIntObjectHashMap[TIntFloatMap](DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)
  }

  def receive = storeCliques

  /**
   * Accepts messages about ground clauses, in order to store them.
   *
   * @return a partial function with the actor logic for collecting ground clauses.
   */
  def storeCliques = ({
    /**
     * There are two procedures that can be performed for storing a ground clause:
     * <ul>
     * <li>(1) When the ground clause has a non-zero weight value, it is going to be stored.</li>
     * <li>(2) When the ground clause is unit and has weight equals to zero, then it is assumed as a ground query atom.
     *         In that case the ground query atom is simple forwarded to the corresponding atom register.</li>
     * </ul>
     * Otherwise, the weight is zero and the clause will be ignored.
     */
    case ce: CliqueEntry =>

      logger.debug(s"CliqueRegister[$index] received '$ce' message.")

      // (1) Clause with some weight value, store the clause.
      // (2) Unit clause with zero weight is a query ground atom, thus forward to the corresponding atom register.
      if (ce.weight != 0) store(ce)
      else if (ce.weight == 0 && ce.variables.length == 1)
        atomRegisters(ce.variables(0)) ! QueryVariable(ce.variables(0))

    case ITERATION_COMPLETED  =>
      val iterator = cliques.iterator()
      while (iterator.hasNext) {
        iterator.advance()
        registerAtoms(iterator.value().variables, iterator.key())
      }
      //atomRegisters(index) ! GRND_Iteration_Completed
      master ! REGISTRATION_COMPLETED

    /**
     * When the grounding of the MRF is complete, change the actor behaviour to 'results' and sent the total number of
     * collected ground clauses to master.
     */
    case GROUNDING_COMPLETED =>
      logger.debug(s"CliqueRegister[$index] collected total ${cliques.size} cliques.")
      become(results) //change the actor logic
      sender ! NumberOfCliques(index, cliques.size())

  }: Receive) orElse handleUnknownMessage

  /**
   * Master sends the offset, with which the collected clauses should be
   *
   * @return a partial function with the actor logic for clause re-indexing.
   */
  def results = ({
    case StartID(offset: Int) =>
      logger.debug("CliqueRegister[" + index + "] received 'StartID(" + offset + ")' message.")

      val collectedCliques =
        if (offset == 0) {
          // Do not adjust clique ids
          CollectedCliques(index, cliques, if (createDependencyMap) Some(dependencyMap) else None)
        }
        else {
          // Adjust clique ids
          hashCode2CliqueIDs = null //not needed anymore (allow GC to delete it)

          val resultingCliques = new TIntObjectHashMap[CliqueEntry](cliques.size() + 1, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)

          val resultingDependencyMap: Option[DependencyMap] =
            if (createDependencyMap)
              Some(new TIntObjectHashMap[TIntFloatMap](DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY))
            else None

          val iterator = cliques.iterator()
          var currentClique: CliqueEntry = null
          var finalID = NO_ENTRY_KEY
          var oldID = NO_ENTRY_KEY

          while (iterator.hasNext) {
            iterator.advance()

            oldID = iterator.key()
            finalID = oldID + offset
            currentClique = iterator.value()

            // Store clique mappings with the new 'final' id as key
            resultingCliques.put(finalID, currentClique)

            // When we create a dependency map
            resultingDependencyMap.foreach(_.put(finalID, dependencyMap.get(oldID)))
          }

          // Not needed anymore (allow GC to delete it)
          cliques = null
          dependencyMap = null

          CollectedCliques(index, resultingCliques, resultingDependencyMap)
        }

      logger.debug(s"CliqueRegister[$index] sending to master the CollectedCliques message, " +
            s"containing ${collectedCliques.cliques.size} cliques.")

      // send the collected and re-indexed partition of ground clauses to master.
      master ! collectedCliques

  }: Receive) orElse handleUnknownMessage

  /**
   * Reports as an error when an unknown message is being received.
   *
   * @return a partial function with the actor logic when an unknown message is being received.
   */
  private def handleUnknownMessage: Receive = {
    case msg =>
      logger.error(s"CliqueRegister[$index] received an unknown message '$msg' from ${sender()}")
  }


  private def registerAtoms(variables: Array[Int], cliqueID: Int): Unit = optimize {
    // Register (atomID -> cliqueID) mappings
    for (variable <- variables; atomID = math.abs(variable))
      atomRegisters(atomID) ! RegisterAtom(atomID, cliqueID)
  }

  /**
   * Attempt to store the specified ground clause (i.e., constraint/clique). The specified grounding may either:
   * <ul>
   *   <li> Merged with another ground clause that has the same literals, but possibly different weight value.</li>
   *   <li> Or otherwise, store as it is, since is unique.</li>
   * </ul>
   *
   * @param current the ground clause to store.
   */
  private def store(current: CliqueEntry) {

    @inline def fetchClique(fid: Int): CliqueEntry = cliques.get(fid)

    @inline def put(fid: Int, clique: CliqueEntry) = cliques.put(fid, clique)

    @inline def addToDependencyMap(cliqueID: Int, cliqueEntry: CliqueEntry): Unit = if (createDependencyMap) {
      val clauseStats = new TIntFloatHashMap(DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY, 0)
      clauseStats.put(cliqueEntry.clauseID, cliqueEntry.freq)
      dependencyMap.put(cliqueID, clauseStats)
    }

    // Check if we have constrained with similar hash codes. In that case it may be possible to have constraints with
    // the same literals.
    val storedCliqueIDs = hashCode2CliqueIDs.get(current.hashKey)

    // Check for a stored constraint with the same literals. If a similar constraint is found, then merge their weight
    // values. Otherwise store this constraint.
    if (storedCliqueIDs ne null) {

      // Iterator to iterate through constraints of the same group (i.e., they have the same hashKey,
      // but different literals).
      val iterator = storedCliqueIDs.iterator()

      var merged = false // flag to indicate that merge is completed
      var storedId = -1 // utility variable to temporally keep the ground clause id of a stored constraint

      while (iterator.hasNext && !merged) {
        storedId = iterator.next()
        val storedClique = fetchClique(storedId)

        if (jutil.Arrays.equals(storedClique.variables, current.variables)) {

          // (*) When the stored constraint is not hard-constrained (i.e., does not have infinite weight value), then
          //     merge both constraints (stored and current)
          // (*) When both stored and current constraints are hard-constrained, then they should have the same sign.
          //     If not, then the grounding process cannot continue (wrong MLN theory or, even worse, a possible bug).
          // (*) Otherwise, we assume that either (1) the stored constraint is hard and the current is soft or (2) both
          //     are constraints are hard, but with the same sign to their weights. In both cases, the resulting merge
          //     produces exactly the same constraint with the already stored constraint. Therefore, we virtually assume
          //     that merge is performed, without changing anything.
          if (!storedClique.weight.isInfinite) {
            // Merge these constraints (i.e., ground clauses):
            // (*) When the stored constraint (from a previous run/iteration) is soft and
            //     the new one is hard; then the resulting constraint will be hard.
            // (*) Or when both stored and new constraints are soft. The resulting constraint will have weight the sum
            //     of the two constraints.
            storedClique.weight =
              if (current.weight.isInfinite) current.weight
              else storedClique.weight + current.weight
          }
          else if(storedClique.weight.isInfinite && storedClique.weight != storedClique.weight )
            logger.fatal(
              "Cannot merge hard-constrained ground clauses with opposite infinite weights (-Inf with +Inf). " +
                "Something may wrong in the given MLN theory.")

          // Otherwise, the stored constrain is hard, do not change anything and
          // thus ignore the current constraint.

          // the stored constraint is merged.
          merged = true
        }
      }


      if (!merged) {
        // The current constraint is not merged, thus we simply store it inside the same group.
        put(cliqueID, current)
        storedCliqueIDs.add(cliqueID)

        // add to dependencyMap:
        addToDependencyMap(cliqueID, current)

        // next cliqueID
        cliqueID += 1
      }
      else if (createDependencyMap) {
        // Add or adjust the corresponding frequency in the dependencyMap
        dependencyMap.get(storedId).adjustOrPutValue(current.clauseID, current.freq, current.freq)
      }

    }
    else {
      // Store the current constraint is not similar to any other stored constrain, therefore store it in a new group.
      if (current.weight != 0) {

        put(cliqueID, current)
        val newEntries = new TIntArrayList()
        newEntries.add(cliqueID)
        hashCode2CliqueIDs.put(current.hashKey, newEntries)

        // add to dependencyMap:
        addToDependencyMap(cliqueID, current)

        // next cliqueID
        cliqueID += 1
      }

    }
  }

}

/**
 * Companion object for creating CliqueRegisterWorkers.
 */
object CliqueRegisterWorker {

  /**
   * Creates a CliqueRegisterWorker instance.
   *
   * @param index the worker index (since we have multiple CliqueRegisterWorker instances),
   *              it also represents the partition index.
   * @param atomRegisters partitioned collection of AtomRegisterWorker actors, in order to send messages about ground
   *                      atom ids and their relation to ground clauses.
   * @param createDependencyMap when it is true the worker stores additional information about the relations between
   *                            FOL clauses and their groundings.
   * @param master reference to master actor, it is required in order to send the results back to master actor.
   *
   * @return a CliqueRegisterWorker instance
   */
  def apply(index: Int, atomRegisters: IndexPartitioned[ActorRef], createDependencyMap: Boolean = false)(implicit master: ActorRef) =
    new CliqueRegisterWorker(index, master, atomRegisters, createDependencyMap)
}