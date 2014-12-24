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
import lomrf.util.{AtomIdentityFunction, FunctionMapper, ConstantsSet}

/**
 * @author Anastasios Skarlatidis
 */
object ECExampleDomain {

  val constants = Map[String, ConstantsSet](
    "time" -> ConstantsSet((1 to 100).map(_.toString): _* ),
    "event" -> ConstantsSet("Walking", "Running", "Active", "Inactive", "Exit", "Enter"),
    "fluent" -> ConstantsSet("Move", "Meet"),
    "dist" -> ConstantsSet("24", "30", "35"),
    "id" -> ConstantsSet("ID1", "ID2", "ID3")
  )

  val predicateSchema = Map[AtomSignature, Vector[String]](
    AtomSignature("InitiatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("TerminatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Happens", 2) -> Vector("event", "time"),
    AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Next", 2) -> Vector("time", "time"),
    AtomSignature("Close", 2) -> Vector("dist", "time"),
    AtomSignature("OrientationMove", 3) -> Vector("id", "id", "time")
  )

  val functionsSchema = Map[AtomSignature, (String, Vector[String])](
    AtomSignature("walking", 1) ->("event", Vector("id")),
    AtomSignature("running", 1) ->("event", Vector("id")),
    AtomSignature("active", 1) ->("event", Vector("id")),
    AtomSignature("inactive", 1) ->("event", Vector("id")),
    AtomSignature("exit", 1) ->("event", Vector("id")),
    AtomSignature("move", 2) ->("fluent", Vector("id", "id")),
    AtomSignature("meet", 2) ->("fluent", Vector("id", "id"))
  )

  val dynamicAtoms = Map.empty[AtomSignature, Vector[String] => Boolean]

  val dynamicFunctions = Map.empty[AtomSignature, Vector[String] => String]

  val functionMappers =  Map.empty[AtomSignature, FunctionMapper]

  val queryAtoms = Set(AtomSignature("HoldsAt", 2))

  val cwa = Set(AtomSignature("Next", 2), AtomSignature("Close", 2), AtomSignature("OrientationMove", 3))

  val owa = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

  val probabilisticAtoms = Set.empty[AtomSignature]

  val tristateAtoms = Set.empty[AtomSignature]


  val identityFunctions: Map[AtomSignature, AtomIdentityFunction] = predicateSchema.map {
    case (signature, schema) =>
      signature -> AtomIdentityFunction(signature, schema, schema.map(s => s -> constants(s)).toMap, 0)
  }





}
