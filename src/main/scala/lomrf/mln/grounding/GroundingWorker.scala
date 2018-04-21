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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.mln.grounding

import akka.actor.{Actor, ActorRef}
import com.typesafe.scalalogging.LazyLogging
import lomrf.mln.model.MLN
import lomrf.util.collection.IndexPartitioned

/**
 * Actor responsible for running grounding for each FOL clause in the MLN theory.
 *
 * @param mln MLN instance, containing the collection of clauses to ground.
 * @param cliqueRegisters a partitioned collection of all clique register actors.
 * @param noNegWeights flag to eliminate negative weights. If it is true the sign of negative weights in clauses is
 *                     inverted, as well as all disjunctions become conjunctions (de Morgan's law).
 * @param eliminateNegatedUnit When it is true, unit clauses with negative literal become unit clauses with positive
 *                             literal and inverted sign in their corresponding weight.
 */
final class GroundingWorker private(mln: MLN,
                                    cliqueRegisters: IndexPartitioned[ActorRef],
                                    noNegWeights: Boolean,
                                    eliminateNegatedUnit: Boolean) extends Actor with LazyLogging {

  import messages._

  /**
   * @return a partial function with the actor logic for clause grounding.
   */
  def receive = {

    case Ground(clause, clauseIndex, atomSignatures, atomsDB) =>
      val grounder = new ClauseGrounderImpl(clause, clauseIndex, mln, cliqueRegisters, atomSignatures, atomsDB, noNegWeights, eliminateNegatedUnit)
      grounder.computeGroundings()
      logger.debug("Grounding completed for clause " + clause)
      sender ! Signatures(grounder.collectedSignatures)

    case msg =>
      logger.error(s"GroundingWorker --- Received an unknown message '$msg' from ${sender().toString()}")
  }

}

private object GroundingWorker {

  /**
   * Creates a new GroundingWorker instance.
   *
   * @param mln MLN instance, containing the collection of clauses to ground.
   * @param cliqueRegisters a partitioned collection of all clique register actors.
   * @param noNegWeights flag to eliminate negative weights. If it is true the sign of negative weights in clauses is
   *                     inverted, as well as all disjunctions become conjunctions (de Morgan's law).
   * @param eliminateNegatedUnit When it is true, unit clauses with negative literal become unit clauses with positive
   *                             literal and inverted sign in their corresponding weight.
   * @return a new GroundingWorker instance.
   */
  def apply(mln: MLN, cliqueRegisters: IndexPartitioned[ActorRef], noNegWeights: Boolean = false, eliminateNegatedUnit: Boolean = false) =
    new GroundingWorker(mln,cliqueRegisters,noNegWeights, eliminateNegatedUnit)
}
