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

package lomrf.tests

import lomrf.logic.AtomSignature
import lomrf.logic.predef._
import lomrf.mln.model.{ConstantsSet, FunctionMapper}

/**
 * Simple function-free EC domain for testing
 */
object ECExampleDomain1 {

  val LAST_TIME_POINT = 10

  lazy val constants = Map[String, ConstantsSet](
    "time" -> ConstantsSet((1 to LAST_TIME_POINT).map(_.toString): _* ),
    "event" -> ConstantsSet("Walking", "Running", "Active", "Inactive", "Exit", "Enter"),
    "fluent" -> ConstantsSet("Move", "Meet")
  )

  lazy val predicateSchema = Map[AtomSignature, Vector[String]](
    AtomSignature("InitiatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("TerminatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Happens", 2) -> Vector("event", "time"),
    AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Next", 2) -> Vector("time", "time")
  )

  lazy val functionsSchema =  Map.empty[AtomSignature, (String, Vector[String])] //Map(AtomSignature("next", 1) -> ("time", Vector("time")))

  lazy val dynamicAtoms = dynAtoms

  lazy val dynamicFunctions = dynFunctions

  lazy val functionMappers =  Map.empty[AtomSignature, FunctionMapper]

  lazy val queryAtoms = Set(AtomSignature("HoldsAt", 2))

  lazy val hiddenAtoms = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

  lazy val cwa = Set(AtomSignature("Next", 2), AtomSignature("Happens", 2))

  lazy val owa = Set(AtomSignature("HoldsAt", 2), AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))
  
  lazy val probabilisticAtoms = Set.empty[AtomSignature]

  lazy val tristateAtoms = Set.empty[AtomSignature]

}
