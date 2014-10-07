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
 * Copyright (C) 2012 Anastasios Skarlatidis.
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

package lomrf.logic

import lomrf.logic.dynamic.{DynSuccFunctionBuilder, DynPlusFunctionBuilder}
import lomrf.util.ConstantsSet
import org.scalatest.{Matchers, FunSpec}

/**
 * @author Anastasios Skarlatidis
 */
final class KBParserSpecTest extends FunSpec with Matchers {

  private val constants = Map[String, ConstantsSet](
    "time" -> ConstantsSet("1", "2", "3", "4"),
    "event" -> ConstantsSet("Abrupt", "Walking", "Running", "Active", "Inactive", "Exit"),
    "fluent" -> ConstantsSet("Fight", "Move", "Meet", "Leaving_object"),
    "dist" -> ConstantsSet("24", "30", "35"),
    "id" -> ConstantsSet("ID1", "ID2", "ID3"),

    "a" -> ConstantsSet((1 to 4).map(_.toString): _*),
    "b" -> ConstantsSet("B1", "B2")
  )

  private val predicateSchema = Map[AtomSignature, List[String]](
    AtomSignature("InitiatedAt", 2) -> List("fluent", "time"),
    AtomSignature("TerminatedAt", 2) -> List("fluent", "time"),
    AtomSignature("Initiates", 3) -> List("event", "fluent", "time"),
    AtomSignature("Terminates", 3) -> List("event", "fluent", "time"),
    AtomSignature("Happens", 2) -> List("event", "time"),
    AtomSignature("HoldsAt", 2) -> List("fluent", "time"),
    AtomSignature("Next", 2) -> List("time", "time"),
    AtomSignature("Close", 2) -> List("dist", "time"),
    AtomSignature("Close", 4) -> List("id", "id", "dist", "time"),
    AtomSignature("OrientationMove", 1) -> List("time"),
    AtomSignature("StartedAt", 3) -> List("event", "fluent", "time"),
    AtomSignature("StoppedAt", 3) -> List("event", "fluent", "time"),

    AtomSignature("Predicate1", 1) -> List("a"),
    AtomSignature("Predicate2", 2) -> List("a", "b"),
    AtomSignature("Predicate3", 1) -> List("b")
  )

  private val functionsSchema = Map[AtomSignature, (String, List[String])](
    AtomSignature("walking", 1) ->("event", List("id")),
    AtomSignature("abrupt", 1) ->("event", List("id")),
    AtomSignature("running", 1) ->("event", List("id")),
    AtomSignature("active", 1) ->("event", List("id")),
    AtomSignature("inactive", 1) ->("event", List("id")),
    AtomSignature("exit", 1) ->("event", List("id")),
    AtomSignature("fight", 2) ->("fluent", List("id", "id")),
    AtomSignature("move", 2) ->("fluent", List("id", "id")),
    AtomSignature("meet", 2) ->("fluent", List("id", "id")),
    AtomSignature("leaving_object", 2) ->("fluent", List("id", "id"))
  )


  private val parser = new KBParser(predicateSchema, functionsSchema)
  private val functionPlus = DynPlusFunctionBuilder()
  private val functionSucc = DynSuccFunctionBuilder()


  // Test formulas:
  // (string representation, instance of the formula, number of constants, number of variables, number of functions)
  val formulaList = List(
    // Happens(Walking, t) => InitiatedAt(Moving,t).
    ("Happens(Walking, t) => InitiatedAt(Moving,t).",
      WeightedFormula(Double.PositiveInfinity, Implies(
        AtomicFormula("Happens", List(Constant("Walking"), Variable("t", "time"))),
        AtomicFormula("InitiatedAt", List(Constant("Moving"), Variable("t", "time"))))
    ), 2, 1, 0),

    // Happens(walking(person1), t) ^ Happens(walking(person2), t) => InitiatedAt(move(person1, person2), t).
    ("Happens(walking(person1), t) ^ Happens(walking(person2), t) => InitiatedAt(move(person1,person2),t).",
      WeightedFormula(Double.PositiveInfinity, Implies(
        And(
          AtomicFormula("Happens", List(TermFunction("walking", List(Variable("person1", "id")), "event"), Variable("t", "time"))),
          AtomicFormula("Happens", List(TermFunction("walking", List(Variable("person2", "id")), "event"), Variable("t", "time")))),
        AtomicFormula("InitiatedAt", List(TermFunction("move", List(Variable("person1", "id"), Variable("person2", "id")), "fluent"), Variable("t", "time"))))
      ), 0, 3, 3),

    // InitiatedAt(f, t) => HoldsAt(f, t + 1).
    ("InitiatedAt(f, t) => HoldsAt(f, t + 1).",
      WeightedFormula(Double.PositiveInfinity, Implies(
        AtomicFormula("InitiatedAt", List(Variable("f", "fluent"), Variable("t", "time"))),
        AtomicFormula("HoldsAt", List(Variable("f", "fluent"), functionPlus(Variable("t", "time"), Constant("1"), "time"))))
      ), 1, 2, 1),

    // InitiatedAt(f, t) => HoldsAt(f, t++).
    ("InitiatedAt(f, t) => HoldsAt(f, t++).",
      WeightedFormula(Double.PositiveInfinity, Implies(
        AtomicFormula("InitiatedAt", List(Variable("f", "fluent"), Variable("t", "time"))),
        AtomicFormula("HoldsAt", List(Variable("f", "fluent"), functionSucc(Variable("t", "time"), "time"))))
      ), 0, 2, 1)
  )

  for( (strFormula, formula, nConst, nVar, nFun) <- formulaList ) describe("Sentence '" + strFormula + "'"){

    val result = parser.parseFormula(strFormula)

    it("should be a valid MLN formula"){
      result shouldEqual formula
    }

    it("should contain '"+nConst+"' constant(s)"){
      result.constants.size shouldEqual nConst
    }

    it("should contain '"+nVar+"' variable(s)"){
      result.variables.size shouldEqual nVar
    }

    it("should contain '"+nFun+" function(s)"){
      result.functions.size shouldEqual nFun
    }
  }


}
