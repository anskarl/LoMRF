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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.logic

import org.scalatest.{FunSpec, Matchers}

/**
  * A series of specification tests for clauses and literals.
  *
  * @see [[lomrf.logic.Clause]]
  * @see [[lomrf.logic.Literal]]
  */
final class ClauseSpecTest extends FunSpec with Matchers {

  /*
   * Test literals
   */
  describe("Positive Literal: Happens(Boom, 10)") {
    val literalHappensBoom = Literal.asPositive {
      AtomicFormula("Happens", Vector(Constant("Boom"), Constant("10")))
    }

    it ("should have arity 2") {
      literalHappensBoom.arity shouldEqual 2
    }

    it ("should be positive") {
      literalHappensBoom.isPositive shouldBe true
      literalHappensBoom.isNegative shouldBe false
    }

    it ("its negation should be negative") {
      literalHappensBoom.negate.isNegative shouldBe true
    }

    it ("should be similar to itself") {
      literalHappensBoom =~= literalHappensBoom shouldBe true
    }

    it ("prints as 'Happens(Boom,10)'") {
      literalHappensBoom.toText shouldEqual "Happens(Boom,10)"
    }
  }

  describe("Negative Literal: !Friends(Bob, x)") {
    val literalNotFriends = Literal.asNegative {
      AtomicFormula("Friends", Vector(Constant("Bob"), Variable("x")))
    }

    it ("should have arity 2") {
      literalNotFriends.arity shouldEqual 2
    }

    it ("should be negative") {
      literalNotFriends.isPositive shouldBe false
      literalNotFriends.isNegative shouldBe true
    }

    it ("its negation should be positive") {
      literalNotFriends.negate.isPositive shouldBe true
    }

    it ("should be similar to '!Friends(Bob, y)'") {
      val other = Literal.asNegative {
        AtomicFormula("Friends", Vector(Constant("Bob"), Variable("y")))
      }

      literalNotFriends =~= other shouldBe true
    }

    it ("should not be similar to '!Friends(Anna, x)'") {
      val other = Literal.asNegative {
        AtomicFormula("Friends", Vector(Constant("Anna"), Variable("x")))
      }

      literalNotFriends =~= other shouldBe false
    }

    it ("prints as '!Friends(Bob,x)'") {
      literalNotFriends.toText shouldEqual "!Friends(Bob,x)"
    }
  }

  /*
   * Test clauses
   */
  describe("Clause: 1.5 !Happens(Boom, x)") {
    val clause = Clause.unit(
      Literal.asNegative {
        AtomicFormula("Happens", Vector(Constant("Boom"), Variable("x")))
      },
      1.5
    )

    it ("should have 1 variable: 'x'") {
      clause.variables.size shouldEqual 1
      clause.variables shouldEqual Set(Variable("x"))
    }

    it ("should have 1 constant: 'Boom'") {
      clause.constants.size shouldEqual 1
      clause.constants shouldEqual Set(Constant("Boom"))
    }

    it ("should have no functions") {
      clause.functions.size shouldEqual 0
      clause.functions shouldEqual Set.empty
    }

    it ("should NOT be hard constrained") {
      clause.isHard shouldBe false
    }

    it ("should NOT be ground") {
      clause.isGround shouldBe false
    }

    it ("should be a unit clause") {
      clause.isUnit shouldBe true
      clause.isEmpty shouldBe false
    }

    it ("should have 1 literals") {
      clause.size shouldEqual 1
    }

    it ("should be similar and equal to itself") {
      clause =~= clause shouldBe true
      clause shouldEqual clause
    }
  }

  describe("Clause: !Friends(x, y) v !Smokes(x) v Smokes(y).") {
    val clause = Clause(Set(
      Literal.asNegative {
        AtomicFormula("Friends", Vector(Variable("x"), Variable("y")))
      },
      Literal.asNegative {
        AtomicFormula("Smokes", Vector(Variable("x")))
      },
      Literal.asPositive {
        AtomicFormula("Smokes", Vector(Variable("y")))
      }
    ))

    it ("should have 2 variables: 'x' and 'y'") {
      clause.variables.size shouldEqual 2
      clause.variables shouldEqual Set(Variable("x"), Variable("y"))
    }

    it ("should have no constants") {
      clause.constants.size shouldEqual 0
      clause.constants shouldEqual Set.empty
    }

    it ("should have no functions") {
      clause.functions.size shouldEqual 0
      clause.functions shouldEqual Set.empty
    }

    it ("should be hard constrained") {
      clause.isHard shouldBe true
    }

    it ("should NOT be ground") {
      clause.isGround shouldBe false
    }

    it ("should NOT be a unit clause") {
      clause.isUnit shouldBe false
      clause.isEmpty shouldBe false
    }

    it ("should have 3 literals") {
      clause.size shouldEqual 3
    }

    it ("should be similar and equal to itself") {
      clause =~= clause shouldBe true
      clause shouldEqual clause
    }
  }

  describe("Clause: !Person(Bob) v !Likes(Bob, Scala) v Programmer(Bob, lang(Scala)).") {
    val clause = Clause(Set(
      Literal.asNegative {
        AtomicFormula("Person", Vector(Constant("Bob")))
      },
      Literal.asNegative {
        AtomicFormula("Likes", Vector(Constant("Bob"), Constant("Scala")))
      },
      Literal.asPositive {
        AtomicFormula("Programmer", Vector(Constant("Bob"), TermFunction("lang", Vector(Constant("Scala")))))
      }
    ))

    it ("should have 0 variables") {
      clause.variables.size shouldEqual 0
      clause.variables shouldEqual Set.empty
    }

    it ("should have 2 constants") {
      clause.constants.size shouldEqual 2
      clause.constants shouldEqual Set(Constant("Bob"), Constant("Scala"))
    }

    it ("should have 1 function") {
      clause.functions.size shouldEqual 1
      clause.functions shouldEqual Set(TermFunction("lang", Vector(Constant("Scala"))))
    }

    it ("should be hard constrained") {
      clause.isHard shouldBe true
    }

    it ("should be ground") {
      clause.isGround shouldBe true
    }

    it ("should NOT be a unit clause") {
      clause.isUnit shouldBe false
      clause.isEmpty shouldBe false
    }

    it ("should have 3 literals") {
      clause.size shouldEqual 3
    }

    it ("should be similar and equal to itself") {
      clause =~= clause shouldBe true
      clause shouldEqual clause
    }
  }
}
