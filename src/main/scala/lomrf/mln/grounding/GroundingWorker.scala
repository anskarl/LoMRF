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
import lomrf.mln.model.MLN
import lomrf.util.Logging

/**
 * @author Anastasios Skarlatidis
 */
private final class GroundingWorker(mln: MLN, cliqueRegisters: Array[ActorRef], noNegWeights: Boolean, eliminateNegatedUnit: Boolean, experimentalGrounder: Boolean = false) extends Actor with Logging {

  def receive = {
    case Ground(clause, atomSignatures, atomsDB) =>
      val grounder =
        if(experimentalGrounder) new ClauseGrounderImplNew(clause, mln, cliqueRegisters, atomSignatures, atomsDB, noNegWeights, eliminateNegatedUnit)
        else new ClauseGrounderImpl(clause, mln, cliqueRegisters, atomSignatures, atomsDB, noNegWeights, eliminateNegatedUnit)
      grounder.computeGroundings()
      debug("Grounding completed for clause " + clause)
      sender ! ClauseGroundingCompleted(clause, grounder.collectedSignatures)

    case msg => fatal("GroundingWorker --- Received an unknown message '" + msg + "' from " + sender)
  }


}