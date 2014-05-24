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

package tests.logic

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import lomrf.logic._
import lomrf.util.Logging


/**
 * @author Anastasios Skarlatidis
 */

class BasicFOLTests  extends AssertionsForJUnit with Logging{

  @Test def testUnification1(){
    val typePerson = "person"
    val k1 = AtomicFormula("Knows", List[Term](Constant("John"), Variable("x",typePerson)))

    val k2 = AtomicFormula("Knows", List[Term](Constant("John"), Constant("Jane")))
    assertTrue(unify(k1,k2))
    assertTrue(unify(k2,k1))

    val k3 = AtomicFormula("Knows", List[Term](Variable("y",typePerson), Constant("Bill")))
    assertTrue(unify(k1,k3))
    assertTrue(unify(k3,k1))

    val k4 = AtomicFormula("Knows", List[Term](Variable("y",typePerson),
      Function("motherOf", List[Term](Variable("y", typePerson)), typePerson)))
    assertTrue(unify(k1,k4))
    assertTrue(unify(k4,k1))

    val k5 = AtomicFormula("Knows", List[Term](Variable("x",typePerson), Constant("Elizabeth")))
    assertFalse(unify(k1,k5))
    assertFalse(unify(k5,k1))

    val k6 = AtomicFormula("Knows", List[Term](Variable("y",typePerson),
      Function("parentOf", List[Term](
        Function("motherOf", List[Term](Variable("y", typePerson)), typePerson)), typePerson)))
    assertTrue(unify(k1,k6))
    assertTrue(unify(k6,k1))


    val k7 = AtomicFormula("Knows", List[Term](Variable("y",typePerson),
      Function("parentOf", List[Term](
        Function("motherOf", List[Term](Variable("x", typePerson)), typePerson)), typePerson)))
    assertFalse(unify(k1,k7))
    assertFalse(unify(k7,k1))


    val k8 = AtomicFormula("Knows", List[Term](Variable("y",typePerson),
      Function("functionOf",
        List[Term](
            Function("motherOf", List[Term](Variable("y", typePerson)), typePerson),
            Function("fatherOf", List[Term](Variable("y", typePerson)), typePerson),
            Variable("y",typePerson),
            Constant("Something")
        ) , typePerson)))
    assertTrue(unify(k1,k8))
    assertTrue(unify(k8,k1))

    //InitiatedAt(meet(p1, p2),10) == InitiatedAt(f,t)
    val typeTime = "time"
    val typeFluent = "fluent"
    val k9 = AtomicFormula("InitiatedAt",
      List[Term](
        Function(
          "meet",
          List[Term](Variable("x", typePerson), Variable("y", typePerson)),
          typeFluent),
        Variable("t", typeTime)
    ))
    //List[Term](Variable("x", typePerson), Variable("y", typePerson)),

    val k10 = AtomicFormula("InitiatedAt", List[Term](
          Variable("f", typeFluent),
          Variable("t", typeTime)
        ))
    assertTrue(unify(k9,k10))
    assertTrue(unify(k10,k9))


    val k11 = AtomicFormula("Alpha",
      List[Term](
        Variable("x", typeTime),
        Variable("y",typeTime)
      ))
    val k12 = AtomicFormula("Alpha",
          List[Term](
            Function("func", List[Term](Variable("y", typeTime)), typeTime),
            Constant("10")
          ))
    assertTrue(unify(k11,k12))
  }

  @Test def testMGP(){

    val typePerson = "person"
    val typeTime = "time"
    val typeFluent = "fluent"

    implicit val predicateSchema = Map[AtomSignature, List[String]](AtomSignature("InitiatedAt", 2) -> List(typeFluent, typeTime))

    implicit val functionSchema  = Map[AtomSignature, (String, List[String])](AtomSignature("meet", 2) ->(typeFluent, List(typePerson, typePerson)))



    // k1 = InitiatedAt(meet(x,y),t)
    val k1 = AtomicFormula("InitiatedAt",List[Term](Function("meet",List[Term](Variable("x", typePerson), Variable("y", typePerson)),typeFluent),Variable("t", typeTime)))

    // k2 = InitiatedAt(meet(A,y),t)
    val k2 = AtomicFormula("InitiatedAt",List[Term](Function("meet",List[Term](Constant("A"), Variable("y", typePerson)),typeFluent),Variable("t", typeTime)))

    // k3 = InitiatedAt(meet(A,B),t)
    val k3 = AtomicFormula("InitiatedAt",List[Term](Function("meet",List[Term](Constant("A"), Constant("B")),typeFluent),Variable("t", typeTime)))

    // k4 = InitiatedAt(meet(x,B),t)
    val k4 = AtomicFormula("InitiatedAt",List[Term](Function("meet",List[Term](Variable("x", typePerson), Constant("B")),typeFluent),Variable("t", typeTime)))

    //k5 = InitiatedAt(meet(C,B),t)
    val k5 = AtomicFormula("InitiatedAt",List[Term](Function("meet",List[Term](Constant("C"), Constant("B")),typeFluent),Variable("t", typeTime)))

    // g = InitiatedAt(f,t)
    val g = AtomicFormula("InitiatedAt", List[Term](Variable("f", typeFluent),Variable("t", typeTime)))

    //Test 1 mgp(InitiatedAt(meet(x,y),t), InitiatedAt(f,t)) = InitiatedAt(f,t)
    //println("Test 1: "+mgp(k1,k2))
    assertTrue(generalisation(k1,g).getOrElse(fatal("mgp failed")) == g)

    //Test 2 mgp(InitiatedAt(f,t), InitiatedAt(meet(x,y),t)) = InitiatedAt(f,t)
    //println("Test 2: "+mgp(k2,k1))
    assertTrue(generalisation(g,k1).getOrElse(fatal("mgp failed")) == g)

    //Test 3 mgp(InitiatedAt(f,t), InitiatedAt(f,t)) = InitiatedAt(f,t)
    //println("Test 3: "+mgp(k2,k2))
    assertTrue(generalisation(g,g).getOrElse(fatal("mgp failed")) == g)

    //Test 4 mgp(InitiatedAt(meet(x,y),t), InitiatedAt(meet(x,y),t)) = InitiatedAt(meet(x,y),t)
    //println("Test 4: "+mgp(k1,k1))
    assertTrue(generalisation(k1,k1).getOrElse(fatal("mgp failed")) == k1)

    //Test 5 mgp(InitiatedAt(meet(A,y),t), InitiatedAt(meet(A,B),t)) = InitiatedAt(meet(A,y),t)
    //println(mgp(k2,k3))
    assertTrue(generalisation(k2,k3).getOrElse(fatal("mgp failed")) == k2)

    //Test 6 mgp(InitiatedAt(meet(A,B),t), InitiatedAt(meet(A,y),t)) = InitiatedAt(meet(A,y),t)
    //println(mgp(k3,k2))
    assertTrue(generalisation(k3,k2).getOrElse(fatal("mgp failed")) == k2)

    //Test 7 mgp(InitiatedAt(meet(A,B),t), InitiatedAt(f,t)) = InitiatedAt(f,t)
    assertTrue(generalisation(k3,g).getOrElse(fatal("mgp failed")) == g)

    //Test 8 mgp(InitiatedAt(f,t), InitiatedAt(meet(A,y),t)) = InitiatedAt(f,t)
    assertTrue(generalisation(g,k2).getOrElse(fatal("mgp failed")) == g)

    //Test 9 mgp(InitiatedAt(meet(x,B),t), InitiatedAt(meet(A,y),t)) = InitiatedAt(meet(x,y),t)
    //println(mgp(k4,k2))
    assertTrue(generalisation(k4,k2).getOrElse(fatal("mgp failed")) == k1)

    //Test 10 mgp(InitiatedAt(meet(C,B),t), InitiatedAt(meet(A,y),t)) = InitiatedAt(meet(x,y),t)
    //println(mgp(k5,k2))
    assertTrue(generalisation(k5,k2).getOrElse(fatal("mgp failed")).isSimilarTo(k1))

  }


  

  private def unify(x: AtomicFormula, y: AtomicFormula):Boolean ={
      Unify(x,y) match {
        case Some(mgu) =>
          info("UNIFY{"+x+", "+y+"} = "+mgu.toString)
          true
        case None =>
          info("UNIFY{"+x+", "+y+"} = failure")
          false
      }
    }
  
}