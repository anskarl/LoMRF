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

import akka.actor.{Actor, ActorRef}
import auxlib.log.Logging
import lomrf.mln.model.MLN
import lomrf.util.collection.PartitionedData

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
                                    cliqueRegisters: PartitionedData[ActorRef],
                                    noNegWeights: Boolean,
                                    eliminateNegatedUnit: Boolean) extends Actor with Logging {

  import messages._

  /**
   * @return a partial function with the actor logic for clause grounding.
   */
  def receive = {

    case Ground(clause, clauseIndex, atomSignatures, atomsDB) =>
      val grounder = new ClauseGrounderImpl(clause, clauseIndex, mln, cliqueRegisters, atomSignatures, atomsDB, noNegWeights, eliminateNegatedUnit)
      grounder.computeGroundings()
      debug("Grounding completed for clause " + clause)
      sender ! Signatures(grounder.collectedSignatures)

    case msg =>
      error(s"GroundingWorker --- Received an unknown message '$msg' from ${sender().toString()}")
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
  def apply(mln: MLN, cliqueRegisters: PartitionedData[ActorRef], noNegWeights: Boolean = false, eliminateNegatedUnit: Boolean = false) =
    new GroundingWorker(mln,cliqueRegisters,noNegWeights, eliminateNegatedUnit)
}
