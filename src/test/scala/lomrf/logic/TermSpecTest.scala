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

package lomrf.logic

import org.scalatest.{ Matchers, FunSpec }

/**
  * A series of specification tests for FOL terms.
  *
  * @see [[lomrf.logic.Term]]
  */
final class TermSpecTest extends FunSpec with Matchers {

  /**
    * Test constant terms
    */
  describe("The instance of Constant(Foo) is a constant, which") {
    val resultingTerm = Constant("Foo")

    it("is ground") {
      assert(resultingTerm.isGround)
    }

    it("has type String") {
      assert(resultingTerm.symbol.isInstanceOf[String])
    }

    it("has symbol 'Foo'") {
      resultingTerm.symbol should be("Foo")
    }

    it("is equal with another instance of Constant(Foo)") {
      resultingTerm should equal(Constant("Foo"))
    }

    it("is not equal with Constant(bar)") {
      resultingTerm should not equal Constant("Bar")
    }

    it("prints as 'Foo'") {
      resultingTerm.toText should be("Foo")
    }
  }

  /**
    * Test variable terms (untyped)
    */
  describe("The instance of Variable(x) is a variable, which") {
    val resultingTerm = Variable("x")

    it("has symbol 'x'") {
      resultingTerm.symbol should be("x")
    }

    it("is equal with another instance of Variable(x)") {
      resultingTerm should equal(Variable("x"))
    }

    it("is not equal with Variable(y)") {
      resultingTerm should not equal Variable("y")
    }

    it("is not equal with Constant(Foo)") {
      resultingTerm should not equal Constant("Foo")
    }

    it("prints as 'x'") {
      resultingTerm.toText should be("x")
    }

    it("has domain: " + Variable.UNDEFINED_DOMAIN) {
      resultingTerm.domain shouldEqual Variable.UNDEFINED_DOMAIN
    }
  }

  /**
    * Test variable terms (typed)
    */
  describe("The instance of Variable(t,time) is a variable, which") {
    val resultingTerm = Variable("t", "time")

    it("has symbol 't'") {
      resultingTerm.symbol should be("t")
    }

    it("is equal with another instance of Variable(t,time)") {
      resultingTerm should equal(Variable("t", "time"))
    }

    it("is not equal with Variable(t, another_domain)") {
      resultingTerm should not equal Variable("t", "another_domain")
    }

    it("is not equal with Constant(Foo)") {
      resultingTerm should not equal Constant("Foo")
    }

    it("prints as 't'") {
      resultingTerm.toText should be("t")
    }

    it("toString returns 't:time'") {
      resultingTerm.toString should be("t:time")
    }

    it("has domain: time") {
      resultingTerm.domain shouldEqual "time"
    }
  }

  /**
    * Test variable terms (typed and indexed)
    */
  describe("The instance of Variable(t,time,10) is a variable, which") {
    val resultingTerm = Variable("t", "time", 10)

    it("has symbol 't'") {
      resultingTerm.symbol should be("t")
    }

    it("is equal with another instance of Variable(t, time, 10)") {
      resultingTerm should equal(Variable("t", "time", 10))
    }

    it("is not equal with Variable(t, another_domain, 10)") {
      resultingTerm should not equal Variable("t", "another_domain")
    }
    it("is not equal with Variable(t, time, 11)") {
      resultingTerm should not equal Variable("t", "time", 11)
    }

    it("is not equal with Constant(Foo)") {
      resultingTerm should not equal Constant("Foo")
    }

    it("prints as 't_10'") {
      resultingTerm.toText should be("t_10")
    }

    it("toString returns 'x$10:time'") {
      resultingTerm.toString should be("t$10:time")
    }

    it("has domain: time") {
      resultingTerm.domain shouldEqual "time"
    }

    it("has index: 10") {
      resultingTerm.index shouldEqual 10
    }
  }

  /**
    * Test function terms
    *
    * functions description:
    * (TermFunction instance, string representation, arity, number of constants, number of variables )
    */
  val functionsDescription = List(
    (TermFunction("Foo", Vector(Constant("Bar"))), "Foo(Bar)", 1, 1, 0),
    (TermFunction("Foo", Vector(Variable("x"), Constant("Bar"))), "Foo(x, Bar)", 2, 1, 1),
    (TermFunction("Foo", Vector(Variable("x"), Constant("Bar"), Variable("y"))), "Foo(x, Bar, y)", 3, 1, 2),
    (TermFunction("Foo", Vector(Variable("x"), Constant("Bar"), TermFunction("F", Vector(Variable("y"))))), "Foo(x, Bar, F(y))", 3, 1, 2),
    (TermFunction("Foo", Vector(Variable("x"), Constant("Bar"), TermFunction("F", Vector(Variable("y"), Constant("G"))))), "Foo(x, Bar, F(y, G))", 3, 2, 2))

  for ((termFunction, textFunction, arity, nConstant, nVariables) <- functionsDescription) {
    describe("The instance of '" + termFunction.toText + "' is a function, which") {

      it("has symbol 'Foo'") {
        termFunction.symbol should be("Foo")
      }

      it("has arity '" + arity + "'") {
        termFunction.arity should be(arity)
      }

      it("has '" + nConstant + "' constant(s)") {
        termFunction.constants.size should be(nConstant)
      }

      it("has '" + nVariables + "' variables(s)") {
        termFunction.variables.size should be(nVariables)
      }

      it(s"prints as $textFunction") {
        textFunction shouldEqual termFunction.toText
      }
    }
  }

}
