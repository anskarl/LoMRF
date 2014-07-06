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

import java.{util => jutil}

import akka.actor.{Actor, ActorRef}
import gnu.trove.list.array.TIntArrayList
import gnu.trove.map.hash.TIntObjectHashMap
import lomrf._
import lomrf.util.Logging

import scala.language.postfixOps
import scalaxy.loops._

/**
 * @author Anastasios Skarlatidis
 */
private final class CliqueRegisterWorker(val index: Int, master: ActorRef, atomRegisters: Array[ActorRef]) extends Actor with Logging {

  private var hashCode2CliqueIDs = new TIntObjectHashMap[TIntArrayList]()
  private var cliques = new TIntObjectHashMap[CliqueEntry](DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)

  private val numOfAtomBatches = atomRegisters.length

  private var cliqueID = 0


  def receive = {
    case ce: CliqueEntry =>
      //println("CliqueRegister[" + index + "].received: " + ce)
      if (ce.weight == 0 && ce.variables.length == 1) {
        atomRegisters(ce.variables(0) % numOfAtomBatches) ! QueryVariable(ce.variables(0))
      } else if (ce.weight != 0) storeClique(ce)
    case GRND_Completed => sender ! NumberOfCliques(index, cliques.size())
    case StartID(offset: Int) =>
      hashCode2CliqueIDs = null //not needed anymore (allow GC to delete it)

      val resultingCliques = new TIntObjectHashMap[CliqueEntry](cliques.size() + 1, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY)

      val iterator = cliques.iterator()
      var currentClique: CliqueEntry = null
      var currentCliqueID = 0

      while (iterator.hasNext) {
        iterator.advance()
        currentCliqueID = iterator.key() + offset
        currentClique = iterator.value()
        resultingCliques.put(currentCliqueID, currentClique)
        for (variable <- currentClique.variables) {
          val atomID = math.abs(variable)
          atomRegisters(atomID % numOfAtomBatches) ! Register(atomID, currentCliqueID)
        }
      }

      cliques = null //not needed anymore (allow GC to delete it)

      master ! CollectedCliques(index, resultingCliques)

    case msg => fatal("CliqueRegister[" + index + "] --- Received an unknown message '" + msg + "' from " + sender)
  }


  private def storeClique(cliqueEntry: CliqueEntry) {
    //statReceived += 1

    @inline def fetchClique(fid: Int): CliqueEntry = cliques.get(fid)

    @inline def put(fid: Int, clique: CliqueEntry) = cliques.put(fid, clique)

    val storedCliqueIDs = hashCode2CliqueIDs.get(cliqueEntry.hashKey)
    // (1) check for a stored clique with the same variables
    if (storedCliqueIDs ne null) {

      val iterator = storedCliqueIDs.iterator()
      var merged = false
      while (iterator.hasNext && !merged) {
        val storedId = iterator.next()
        val storedClique = fetchClique(storedId)
        if (jutil.Arrays.equals(storedClique.variables, cliqueEntry.variables)) {
          if (storedClique.weight != Double.PositiveInfinity) {
            // merge these cliques
            if (cliqueEntry.weight == Double.PositiveInfinity) {
              // When the stored constraint (from a previous run/iteration) is soft and
              // the new one is hard; then the resulting constraint will be hard.
              storedClique.weight = Double.PositiveInfinity
            }
            else {
              // When both stored and new constraints are soft, then merge these constraints
              storedClique.weight += cliqueEntry.weight
            }
          }
          // Otherwise, the stored constrain is hard, do not change anything and
          // thus ignore the current constraint.

          //state that a merging operation is performed.
          merged = true
          //statMerged += 1
        }
      } // while

      if (!merged) {
        // The constraint is not merged, thus we simply store it.
        put(cliqueID, cliqueEntry)
        storedCliqueIDs.add(cliqueID)
        registerVariables(cliqueEntry.variables)
        cliqueID += 1
      }
    }
    else {
      // (2) Otherwise store this clique
      if (cliqueEntry.weight != 0) {
        put(cliqueID, cliqueEntry)
        val newEntries = new TIntArrayList()
        newEntries.add(cliqueID)
        hashCode2CliqueIDs.put(cliqueEntry.hashKey, newEntries)
        cliqueID += 1
      }
      registerVariables(cliqueEntry.variables)

    }
  } // store(...)

  @inline private def registerVariables(variables: Array[Int]) {
    for (i <- (0 until variables.length).optimized) {
      val atomID = math.abs(variables(i))
      atomRegisters(atomID % numOfAtomBatches) ! atomID
    }
  }

}
