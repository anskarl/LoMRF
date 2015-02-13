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

package lomrf.mln

import java.{util => jutil}

import gnu.trove.map.{TIntFloatMap, TIntIntMap, TIntObjectMap}
import gnu.trove.map.hash.{TIntIntHashMap, TIntObjectHashMap}
import gnu.trove.set.TIntSet
import gnu.trove.set.hash.TIntHashSet
import lomrf.logic.{AtomSignature, Clause}

/**
 * @author Anastasios Skarlatidis
 */
package object grounding {

  // ----------------------------------------
  // Types
  // ----------------------------------------
  type DependencyMap = TIntObjectMap[TIntFloatMap]


  // ----------------------------------------
  // Messages
  // ----------------------------------------

  /**
   * Message for requesting the final ground MRF from the Master actor.
   */
  private[grounding] case object REQUEST_RESULTS

  private[grounding] case object GRND_Iteration_Completed

  private[grounding] case object GRND_Completed

  private[grounding] case class Result(
                                        cliques: Array[TIntObjectMap[CliqueEntry]],
                                        atom2Cliques: Array[TIntObjectMap[TIntHashSet]],
                                        queryAtomIDs: Array[TIntSet],
                                        dependencyMap: Array[DependencyMap])

  private[grounding] case class ClauseGroundingCompleted(clause: Clause, collectedSignatures: Set[AtomSignature])

  // Master -> GroundingWorker
  private[grounding] case class Ground(clause: Clause, clauseIndex: Int, atomSignatures: Set[AtomSignature], atomsDB: Array[TIntSet])

  // GroundingWorker -> CliqueRegister
  private[grounding] case class CliqueEntry(hashKey: Int, var weight: Double, variables: Array[Int], clauseID: Int, freq: Int) {


    override def hashCode() = hashKey

    override def equals(obj: Any): Boolean = obj match {
      case other: CliqueEntry =>
        other.hashKey == hashKey && other.weight == weight && jutil.Arrays.equals(other.variables, variables)
      case _ => false
    }
  }


  // Master -> AtomRegister
  private[grounding] case class CollectedAtomIDs(atomRegisterIdx: Int, atomIDs: TIntSet)

  // AtomRegister -> Master
  private[grounding] case class AtomsBatch(index: Int, registry: TIntObjectHashMap[TIntHashSet], queryAtomIDs: TIntSet)

  // CliqueRegister -> AtomRegister
  private[grounding] case class Register(atomID: Int, cliqueID: Int)

  // CliqueRegister -> Master
  private[grounding] case class CollectedCliques(index: Int, cliques: TIntObjectMap[CliqueEntry], dependencyMap: TIntObjectMap[TIntFloatMap])

  private[grounding] case class StartID(id: Int)

  private[grounding] case class NumberOfCliques(index: Int, size: Int)

  private[grounding] case class QueryVariable(atomID: Int)

}
