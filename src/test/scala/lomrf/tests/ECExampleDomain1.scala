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

package lomrf.tests

import lomrf.logic.AtomSignature
import lomrf.util.{FunctionMapper, ConstantsSet}

/**
 * Simple function-free EC domain for testing
 *
 * @author Anastasios Skarlatidis
 */
object ECExampleDomain1 {

  val LAST_TIME_POINT = 1000

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

  lazy val functionsSchema = Map.empty[AtomSignature, (String, Vector[String])]

  lazy val dynamicAtoms = Map.empty[AtomSignature, Vector[String] => Boolean]

  lazy val dynamicFunctions = Map.empty[AtomSignature, Vector[String] => String]

  lazy val functionMappers =  Map.empty[AtomSignature, FunctionMapper]

  lazy val queryAtoms = Set(AtomSignature("HoldsAt", 2))

  lazy val hiddenAtoms = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

  lazy val cwa = Set(AtomSignature("Next", 2), AtomSignature("Happens", 2))

  lazy val owa = Set(AtomSignature("HoldsAt", 2), AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))
  
  lazy val probabilisticAtoms = Set.empty[AtomSignature]

  lazy val tristateAtoms = Set.empty[AtomSignature]



}
