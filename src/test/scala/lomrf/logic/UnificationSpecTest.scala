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
package lomrf.logic

import org.scalatest.{Matchers, FunSpec}

/**
 * A series of spec tests regarding the computation of the unification between a pair of atomic formulas.
 */
final class UnificationSpecTest extends FunSpec with Matchers {

  private val typePerson = "person"
  private val typeTime = "time"
  private val typeFluent = "fluent"
  private implicit val predicateSchema = Map[AtomSignature, Vector[String]](AtomSignature("InitiatedAt", 2) -> Vector(typeFluent, typeTime))
  private implicit val functionSchema = Map[AtomSignature, (String, Vector[String])](AtomSignature("meet", 2) -> (typeFluent, Vector(typePerson, typePerson)))


  val k1 = AtomicFormula("Knows", Vector[Term](Constant("John"), Variable("x", typePerson)))
  val k2 = AtomicFormula("Knows", Vector[Term](Constant("John"), Constant("Jane")))
  checkUnify(k1, k2)
  checkUnify(k2, k1)

  val k3 = AtomicFormula("Knows", Vector[Term](Variable("y", typePerson), Constant("Bill")))
  checkUnify(k1, k3)
  checkUnify(k3, k1)

  val k4 = AtomicFormula("Knows", Vector[Term](Variable("y", typePerson),
    TermFunction("motherOf", Vector[Term](Variable("y", typePerson)), typePerson)))
  checkUnify(k1, k4)
  checkUnify(k4, k1)

  val k5 = AtomicFormula("Knows", Vector[Term](Variable("x", typePerson), Constant("Elizabeth")))
  checkUnify(k1, k5, expected = false)
  checkUnify(k5, k1, expected = false)

  val k6 = AtomicFormula("Knows", Vector[Term](Variable("y", typePerson),
    TermFunction("parentOf", Vector[Term](
      TermFunction("motherOf", Vector[Term](Variable("y", typePerson)), typePerson)), typePerson)))
  checkUnify(k1, k6)
  checkUnify(k6, k1)

  val k7 = AtomicFormula("Knows", Vector[Term](Variable("y", typePerson),
    TermFunction("parentOf", Vector[Term](
      TermFunction("motherOf", Vector[Term](Variable("x", typePerson)), typePerson)), typePerson)))
  checkUnify(k1, k7, expected = false)
  checkUnify(k7, k1, expected = false)

  val k8 = AtomicFormula("Knows", Vector[Term](Variable("y", typePerson),
    TermFunction("functionOf",
      Vector[Term](
        TermFunction("motherOf", Vector[Term](Variable("y", typePerson)), typePerson),
        TermFunction("fatherOf", Vector[Term](Variable("y", typePerson)), typePerson),
        Variable("y", typePerson),
        Constant("Something")
      ), typePerson)))
  checkUnify(k1, k8)
  checkUnify(k8, k1)


  val k9 = AtomicFormula("InitiatedAt",
    Vector[Term](
      TermFunction(
        "meet",
        Vector[Term](Variable("x", typePerson), Variable("y", typePerson)),
        typeFluent),
      Variable("t", typeTime)
    ))

  val k10 = AtomicFormula("InitiatedAt", Vector[Term](
    Variable("f", typeFluent),
    Variable("t", typeTime)
  ))
  checkUnify(k9, k10)
  checkUnify(k10, k9)


  val k11 = AtomicFormula("Alpha",
    Vector[Term](
      Variable("x", typeTime),
      Variable("y", typeTime)
    ))
  val k12 = AtomicFormula("Alpha",
    Vector[Term](
      TermFunction("func", Vector[Term](Variable("y", typeTime)), typeTime),
      Constant("10")
    ))
  checkUnify(k11, k12)


  // k1 = InitiatedAt(meet(x,y),t)
  val ka1 = AtomicFormula("InitiatedAt", Vector[Term](TermFunction("meet", Vector[Term](Variable("x", typePerson), Variable("y", typePerson)), typeFluent), Variable("t", typeTime)))

  // k2 = InitiatedAt(meet(A,y),t)
  val ka2 = AtomicFormula("InitiatedAt", Vector[Term](TermFunction("meet", Vector[Term](Constant("A"), Variable("y", typePerson)), typeFluent), Variable("t", typeTime)))

  // k3 = InitiatedAt(meet(A,B),t)
  val ka3 = AtomicFormula("InitiatedAt", Vector[Term](TermFunction("meet", Vector[Term](Constant("A"), Constant("B")), typeFluent), Variable("t", typeTime)))

  // k4 = InitiatedAt(meet(x,B),t)
  val ka4 = AtomicFormula("InitiatedAt", Vector[Term](TermFunction("meet", Vector[Term](Variable("x", typePerson), Constant("B")), typeFluent), Variable("t", typeTime)))

  //k5 = InitiatedAt(meet(C,B),t)
  val ka5 = AtomicFormula("InitiatedAt", Vector[Term](TermFunction("meet", Vector[Term](Constant("C"), Constant("B")), typeFluent), Variable("t", typeTime)))

  // g = InitiatedAt(f,t)
  val g = AtomicFormula("InitiatedAt", Vector[Term](Variable("f", typeFluent), Variable("t", typeTime)))

  //Test 1 mgp(InitiatedAt(meet(x,y),t), InitiatedAt(f,t)) = InitiatedAt(f,t)
  checkMGP(ka1, g, g)

  //Test 2 mgp(InitiatedAt(f,t), InitiatedAt(meet(x,y),t)) = InitiatedAt(f,t)
  checkMGP(g, ka1, g)

  //Test 3 mgp(InitiatedAt(f,t), InitiatedAt(f,t)) = InitiatedAt(f,t)
  checkMGP(g, g, g)

  //Test 4 mgp(InitiatedAt(meet(x,y),t), InitiatedAt(meet(x,y),t)) = InitiatedAt(meet(x,y),t)
  checkMGP(ka1, ka1, ka1)

  //Test 5 mgp(InitiatedAt(meet(A,y),t), InitiatedAt(meet(A,B),t)) = InitiatedAt(meet(A,y),t)
  checkMGP(ka2, ka3, ka2)

  //Test 6 mgp(InitiatedAt(meet(A,B),t), InitiatedAt(meet(A,y),t)) = InitiatedAt(meet(A,y),t)
  checkMGP(ka3, ka2, ka2)

  //Test 7 mgp(InitiatedAt(meet(A,B),t), InitiatedAt(f,t)) = InitiatedAt(f,t)
  checkMGP(ka3, g, g)

  //Test 8 mgp(InitiatedAt(f,t), InitiatedAt(meet(A,y),t)) = InitiatedAt(f,t)
  checkMGP(g, ka2, g)

  //Test 9 mgp(InitiatedAt(meet(x,B),t), InitiatedAt(meet(A,y),t)) = InitiatedAt(meet(x,y),t)
  checkMGP(ka4, ka2, ka1)

  //Test 10 mgp(InitiatedAt(meet(C,B),t), InitiatedAt(meet(A,y),t)) = InitiatedAt(meet(x,y),t)
  describe("Atoms " + ka5.toText + " and " + ka2.toText) {
    they("have " + ka1.toText + " as MGP") {
      assert(generalisation(ka5, ka2).getOrElse(sys.error("mgp failed")) =~= ka1)
    }
  }



  private def checkUnify(x: AtomicFormula, y: AtomicFormula, expected: Boolean = true): Unit = {
    describe("Atoms " + x.toText + " and " + y.toText) {
      val msg = if (expected) "should unify" else "should not unify"

      they(msg) {
        assert(Unify(x, y).isDefined == expected)
      }
    }
  }

  private def checkMGP(x: AtomicFormula, y: AtomicFormula, g: AtomicFormula): Unit = {
    describe("Atoms " + x.toText + " and " + y.toText) {
      they("have " + g.toText + " as MGP") {
        assert(generalisation(x, y).getOrElse(sys.error("MGP failed")) == g)
      }
    }
  }


}
