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
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */

package lomrf.mln

import java.{util => jutil}

import gnu.trove.map.{TIntFloatMap, TIntObjectMap}
import gnu.trove.map.hash.TIntObjectHashMap
import gnu.trove.set.TIntSet
import gnu.trove.set.hash.TIntHashSet
import lomrf.logic.{AtomSignature, Clause}
import lomrf.util.collection.IndexPartitioned

package object grounding {

  // ----------------------------------------
  // Types
  // ----------------------------------------
  type DependencyMap = TIntObjectMap[TIntFloatMap]


  // ----------------------------------------
  // Messages
  // ----------------------------------------

  private[grounding] object messages {

    /**
     * Message for requesting the final ground MRF from the Master actor.
     */
    case object REQUEST_RESULTS

    /**
     * Notification for the completion of a grounding iteration
     */
    case object ITERATION_COMPLETED

    /**
     * Notification for the completion of the grounding procedure
     */
    case object GROUNDING_COMPLETED

    /**
     * Notification that a Clique Register have send all Atom IDs to AtomRegister workers
     */
    case object REGISTRATION_COMPLETED

    case class Result(
                       cliques: IndexPartitioned[TIntObjectMap[CliqueEntry]],
                       atom2Cliques: IndexPartitioned[TIntObjectMap[TIntHashSet]],
                       queryAtomIDs: IndexPartitioned[TIntSet],
                       dependencyMap: Option[IndexPartitioned[DependencyMap]] = None)

    // GroundingWorker -> Master
    case class Signatures(collectedSignatures: Set[AtomSignature])

    // Master -> GroundingWorker
    case class Ground(clause: Clause, clauseIndex: Int, atomSignatures: Set[AtomSignature], atomsDB: IndexPartitioned[TIntSet])

    // GroundingWorker -> CliqueRegister
    case class CliqueEntry(hashKey: Int, var weight: Double, variables: Array[Int], clauseID: Int, freq: Int) {


      override def hashCode() = hashKey

      override def equals(obj: Any): Boolean = obj match {
        case other: CliqueEntry =>
          other.hashKey == hashKey && other.weight == weight && jutil.Arrays.equals(other.variables, variables)
        case _ => false
      }

      override def toString: String =
        s"CliqueEntry(hashKey=$hashKey, weight=$weight, variables=[${variables.mkString(",")}], clauseID=$clauseID, freq=$freq)"
    }

    // Master -> AtomRegister
    case class CollectedAtomIDs(atomRegisterIdx: Int, atomIDs: TIntSet)

    // AtomRegister -> Master
    case class AtomsBatch(index: Int, registry: TIntObjectHashMap[TIntHashSet], queryAtomIDs: TIntSet)

    // CliqueRegister -> AtomRegister
    case class RegisterAtom(atomID: Int, cliqueID: Int)

    // CliqueRegister -> Master
    case class CollectedCliques(index: Int, cliques: TIntObjectMap[CliqueEntry], dependencyMap: Option[DependencyMap] = None)

    case class StartID(id: Int)

    case class NumberOfCliques(index: Int, size: Int)

    case class QueryVariable(atomID: Int)

  }

}
