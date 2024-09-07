/*
 *
 *  o                        o     o   o         o
 *  |             o          |     |\ /|         | /
 *  |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 *  |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 *  O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *              |
 *           o--o
 *  o--o              o               o--o       o    o
 *  |   |             |               |    o     |    |
 *  O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 *  |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 *  o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 *  Logical Markov Random Fields (LoMRF).
 *
 *
 */

package lomrf.mln.learning.supervision.metric

import lomrf.logic._
import org.scalatest.{ FunSpec, Matchers }

final class AtomMetricSpecTest extends FunSpec with Matchers {

  describe("Distances over predicates having NO functions and ONLY constants.") {

    val metric = AtomMetric(HungarianMatcher)

    val predicateA =
      AtomicFormula(
        "Friends",
        Vector("A", "B").map(Constant))
    val predicateB =
      AtomicFormula(
        "Friends",
        Vector("B", "A").map(Constant))
    val predicateC =
      AtomicFormula(
        "Friends",
        Vector("B", "C").map(Constant))
    val predicateD =
      AtomicFormula(
        "Enemies",
        Vector("B", "C").map(Constant))
    val predicateE =
      AtomicFormula(
        "Person",
        Vector("George", "Tall", "Thin", "Young", "Handsome").map(Constant))
    val predicateF =
      AtomicFormula(
        "Person",
        Vector("Pedro", "Tall", "Thin", "Old", "Ugly").map(Constant))
    val predicateG =
      AtomicFormula(
        "Person",
        Vector("Jason", "Short", "Thin", "Young", "Handsome").map(Constant))

    it("Distance of Friends(A, B) to itself should be 0") {
      metric.distance(predicateA, predicateA) shouldEqual 0
    }

    it("Distance of Friends(A, B) to Friends(B, A) should be 0.5") {
      metric.distance(predicateA, predicateB) shouldEqual 0.5
    }

    it("Distance of Friends(B, A) to Friends(B, C) should be 0.25") {
      metric.distance(predicateB, predicateC) shouldEqual 0.25
    }

    it("Distance of Friends(B, C) to Enemies(B, C) should be 1") {
      metric.distance(predicateC, predicateD) shouldEqual 1
    }

    it("Distance of Person(George, Tall, Thin, Young, Handsome) to Person(Pedro, Tall, Thin, Old, Ugly) should be 0.3") {
      metric.distance(predicateE, predicateF) shouldEqual 0.3 +- 1E-6
    }

    it("Distance of Person(George, Tall, Thin, Young, Handsome) to Person(Jason, Tall, Fat, Young, Handsome) should be 0.2") {
      metric.distance(predicateE, predicateG) shouldEqual 0.2 +- 1E-6
    }

    it("Associative property should hold for any distance over predicates") {
      List(predicateA, predicateB, predicateC, predicateD, predicateE, predicateF, predicateG)
        .sliding(2).foreach { pair =>
          metric.distance(pair.head, pair.last) shouldEqual metric.distance(pair.last, pair.head)
        }
    }
  }

  describe("Distances over predicates having NO functions and BOTH constants and variables.") {

    val metric = AtomMetric(HungarianMatcher)

    val predicateA =
      AtomicFormula(
        "Friends",
        Vector("x", "y").map(Variable(_)))
    val predicateB =
      AtomicFormula(
        "Friends",
        Vector("y", "x").map(Variable(_)))
    val predicateC =
      AtomicFormula(
        "Friends",
        Vector("y", "z").map(Variable(_)))
    val predicateD =
      AtomicFormula(
        "Enemies",
        Vector("y", "z").map(Variable(_)))
    val predicateE =
      AtomicFormula(
        "Person",
        Vector(Variable("x")) ++ Vector("Tall", "Thin", "Young", "Handsome").map(Constant))
    val predicateF =
      AtomicFormula(
        "Person",
        Vector(Variable("y")) ++ Vector("Tall", "Thin", "Old", "Ugly").map(Constant))
    val predicateG =
      AtomicFormula(
        "Person",
        Vector(Variable("z")) ++ Vector("Short", "Thin", "Young", "Handsome").map(Constant))

    it("Distance of Friends(x, y) to itself should be 0") {
      metric.distance(predicateA, predicateA) shouldEqual 0
    }

    it("Distance of Friends(x, y) to Friends(y, x) should be 0") {
      metric.distance(predicateA, predicateB) shouldEqual 0
    }

    it("Distance of Friends(y, x) to Friends(y, z) should be 0") {
      metric.distance(predicateB, predicateC) shouldEqual 0
    }

    it("Distance of Friends(y, z) to Enemies(y, z) should be 1") {
      metric.distance(predicateC, predicateD) shouldEqual 1
    }

    it("Distance of Person(x, Tall, Thin, Young, Handsome) to Person(y, Tall, Thin, Old, Ugly) should be 0.2") {
      metric.distance(predicateE, predicateF) shouldEqual 0.25 +- 1E-6
    }

    it("Distance of Person(x, Tall, Thin, Young, Handsome) to Person(z, Tall, Fat, Young, Handsome) should be 0.1") {
      metric.distance(predicateE, predicateG) shouldEqual 0.125 +- 1E-6
    }

    it("Associative property should hold for any distance over predicates") {
      List(predicateA, predicateB, predicateC, predicateD, predicateE, predicateF, predicateG)
        .sliding(2).foreach { pair =>
          metric.distance(pair.head, pair.last) shouldEqual metric.distance(pair.last, pair.head)
        }
    }
  }

  describe("Distances over predicates having one level functions and ONLY constants.") {

    val metric = AtomMetric(HungarianMatcher)

    val predicateA =
      AtomicFormula(
        "Happens",
        Vector(TermFunction("walking", Vector(Constant("A")))) ++ Vector("5").map(Constant))
    val predicateB =
      AtomicFormula(
        "Happens",
        Vector(TermFunction("walking", Vector(Constant("A")))) ++ Vector("6").map(Constant))
    val predicateC =
      AtomicFormula(
        "Happens",
        Vector(TermFunction("walking", Vector(Constant("B")))) ++ Vector("5").map(Constant))
    val predicateD =
      AtomicFormula(
        "Happens",
        Vector(TermFunction("walking", Vector(Constant("B")))) ++ Vector("6").map(Constant))
    val predicateE =
      AtomicFormula(
        "Happens",
        Vector(TermFunction("inactive", Vector(Constant("A")))) ++ Vector("5").map(Constant))
    val predicateF =
      AtomicFormula(
        "Happens",
        Vector(TermFunction("inactive", Vector(Constant("A")))) ++ Vector("6").map(Constant))

    it("Distance of Happens(walking(A), 5) to itself should be 0") {
      metric.distance(predicateA, predicateA) shouldEqual 0
    }

    it("Distance of Happens(walking(A), 5) to Happens(walking(A), 6) should be 0.25") {
      metric.distance(predicateA, predicateB) shouldEqual 0.25
    }

    it("Distance of Happens(walking(A), 5) to Happens(walking(B), 5) should be 0.125") {
      metric.distance(predicateA, predicateC) shouldEqual 0.125
    }

    it("Distance of Happens(walking(A), 5) to Happens(walking(B), 6) should be 0.375") {
      metric.distance(predicateA, predicateD) shouldEqual 0.375
    }

    it("Distance of Happens(walking(A), 5) to Happens(inactive(A), 5) should be 0.25") {
      metric.distance(predicateA, predicateE) shouldEqual 0.25
    }

    it("Distance of Happens(walking(A), 5) to Happens(inactive(A), 6) should be 0.5") {
      metric.distance(predicateA, predicateF) shouldEqual 0.5
    }

    it("Associative property should hold for any distance over predicates") {
      List(predicateA, predicateB, predicateC, predicateD, predicateE, predicateF)
        .sliding(2).foreach { pair =>
          metric.distance(pair.head, pair.last) shouldEqual metric.distance(pair.last, pair.head)
        }
    }
  }

  describe("Distances over predicates having one level functions and BOTH constants and variables.") {

    val metric = AtomMetric(HungarianMatcher)

    val predicateA =
      AtomicFormula(
        "Happens",
        Vector(TermFunction("walking", Vector(Constant("A")))) ++ Vector(Variable("t")))
    val predicateB =
      AtomicFormula(
        "Happens",
        Vector(TermFunction("walking", Vector(Constant("B")))) ++ Vector(Variable("t")))
    val predicateC =
      AtomicFormula(
        "Happens",
        Vector(TermFunction("inactive", Vector(Constant("A")))) ++ Vector(Variable("t")))
    val predicateD =
      AtomicFormula(
        "Happens",
        Vector(TermFunction("inactive", Vector(Constant("B")))) ++ Vector(Variable("t")))

    it("Distance of Happens(walking(A), t) to itself should be 0") {
      metric.distance(predicateA, predicateA) shouldEqual 0
    }

    it("Distance of Happens(walking(A), t) to Happens(walking(B), t) should be 0.125") {
      metric.distance(predicateA, predicateB) shouldEqual 0.25
    }

    it("Distance of Happens(walking(A), t) to Happens(inactive(A), t) should be 0.25") {
      metric.distance(predicateA, predicateC) shouldEqual 0.5
    }

    it("Distance of Happens(walking(A), t) to Happens(inactive(B), t) should be 0.25") {
      metric.distance(predicateA, predicateD) shouldEqual 0.5
    }

    it("Distance of Happens(inactive(A), t) to Happens(inactive(B), t) should be 0.125") {
      metric.distance(predicateC, predicateD) shouldEqual 0.25
    }

    it("Associative property should hold for any distance over predicates") {
      List(predicateA, predicateB, predicateC, predicateD)
        .sliding(2).foreach { pair =>
          metric.distance(pair.head, pair.last) shouldEqual metric.distance(pair.last, pair.head)
        }
    }
  }

}
