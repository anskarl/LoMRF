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
import gnu.trove.list.array.TIntArrayList
import gnu.trove.map.hash.TIntObjectHashMap
import gnu.trove.set.hash.TIntHashSet

/**
 *
 */
private final class AtomRegisterWorker(val index: Int, master: ActorRef) extends Actor with Logging {

  private lazy val atomID2CliqueID = new TIntObjectHashMap[TIntHashSet]()
  private val queryAtomIDs = new TIntHashSet()
  private val atomIDs = new TIntHashSet()
  private var buffer = new TIntArrayList()

  def receive = {

    case QueryVariable(atomID) => queryAtomIDs.add(atomID)
    case atomID: Int =>
      //println("AtomRegister[" + index + "].received: " + atomID)
      assert(atomID != 0, "atomID cannot be equal to zero.")

      if (!atomIDs.contains(atomID)) buffer.add(atomID)

    case GRND_Iteration_Completed =>
      atomIDs.addAll(buffer)
      sender ! CollectedAtomIDs(index, atomIDs)
      buffer = new TIntArrayList()

    case Register(atomID, cliqueID) =>
      assert(atomID != 0, "atomID cannot be equal to zero.")

      val cliqueSet = atomID2CliqueID.get(atomID)
      if (cliqueSet == null) {
        val set = new TIntHashSet()
        set.add(cliqueID)
        atomID2CliqueID.put(atomID, set)
      }
      else {
        cliqueSet.add(cliqueID)
      }

    case msg => fatal("AtomRegister[" + index + "] --- Received an unknown message: " + msg)
  }

  /**
   * When the AtomRegistryWorker is stopped, it sent the results to the master actor.
   */
  override def postStop() {
    master ! AtomsBatch(index, atomID2CliqueID, queryAtomIDs)
  }
}

private object AtomRegisterWorker {

  def apply(index: Int)(implicit master: ActorRef) = new AtomRegisterWorker(index, master)

}
