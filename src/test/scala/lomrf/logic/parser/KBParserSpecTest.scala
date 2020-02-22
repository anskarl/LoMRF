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

package lomrf.logic.parser

import lomrf.logic._
import lomrf.logic.dynamic._
import org.scalatest.{ FunSpec, Matchers }

/**
  * A series of spec tests for knowledge base parsing, e.g., predicates, formulas, weighted formulas, etc.
  */
final class KBParserSpecTest extends FunSpec with Matchers {

  private val predicateSchema = Map[AtomSignature, Vector[String]](
    AtomSignature("InitiatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("TerminatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Initiates", 3) -> Vector("event", "fluent", "time"),
    AtomSignature("Terminates", 3) -> Vector("event", "fluent", "time"),
    AtomSignature("Happens", 2) -> Vector("event", "time"),
    AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Next", 2) -> Vector("time", "time"),
    AtomSignature("Close", 2) -> Vector("dist", "time"),
    AtomSignature("Close", 4) -> Vector("id", "id", "dist", "time"),
    AtomSignature("OrientationMove", 1) -> Vector("time"),
    AtomSignature("StartedAt", 3) -> Vector("event", "fluent", "time"),
    AtomSignature("StoppedAt", 3) -> Vector("event", "fluent", "time"))

  private val functionsSchema = Map[AtomSignature, (String, Vector[String])](
    AtomSignature("walking", 1) -> ("event", Vector("id")),
    AtomSignature("abrupt", 1) -> ("event", Vector("id")),
    AtomSignature("running", 1) -> ("event", Vector("id")),
    AtomSignature("active", 1) -> ("event", Vector("id")),
    AtomSignature("inactive", 1) -> ("event", Vector("id")),
    AtomSignature("exit", 1) -> ("event", Vector("id")),
    AtomSignature("fight", 2) -> ("fluent", Vector("id", "id")),
    AtomSignature("move", 2) -> ("fluent", Vector("id", "id")),
    AtomSignature("meet", 2) -> ("fluent", Vector("id", "id")))

  private val parser = new KBParser(predicateSchema, functionsSchema)

  // Dynamic function builders
  private val functionPlus = new DynPlusFunctionBuilder
  private val functionMinus = new DynMinusFunctionBuilder
  private val functionTimes = new DynTimesFunctionBuilder
  private val functionDiv = new DynDividedByFunctionBuilder
  private val functionMod = new DynModFunctionBuilder
  private val functionNext = new DynSuccFunctionBuilder
  private val functionPrev = new DynPrecFunctionBuilder
  private val functionConcat = new DynConcatFunctionBuilder

  // Dynamic predicate builders
  private val equals = new DynEqualsBuilder
  private val notEquals = new DynNotEqualsBuilder
  private val greaterThan = new DynGreaterThanBuilder
  private val greaterThanEq = new DynGreaterThanEqBuilder
  private val lessThan = new DynLessThanBuilder
  private val lessThanEq = new DynLessThanEqBuilder
  private val substring = new DynSubstringBuilder

  describe("Atomic formulas") {

    val atom1 = "HoldsAt(f, t)"
    it(s"$atom1 should be a valid atomic formula") {
      val parsed = parser.parsePredicate(atom1)

      parsed shouldEqual
        AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), Variable("t", "time")))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 0
    }

    val atom2 = "Happens(e, t)"
    it(s"$atom2 should be a valid atomic formula") {
      val parsed = parser.parsePredicate(atom2)

      parsed shouldEqual
        AtomicFormula("Happens", Vector(Variable("e", "event"), Variable("t", "time")))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 0
    }

    val atom3 = "Happens(e, 5)"
    it(s"$atom3 should be a valid atomic formula") {
      val parsed = parser.parsePredicate(atom3)

      parsed shouldEqual
        AtomicFormula("Happens", Vector(Variable("e", "event"), Constant("5")))

      parsed.variables.size shouldEqual 1
      parsed.constants.size shouldEqual 1
      parsed.functions.size shouldEqual 0
    }

    val atom4 = "Happens(active(x), t)"
    it(s"$atom4 should be a valid atomic formula") {
      val parsed = parser.parsePredicate(atom4)

      parsed shouldEqual
        AtomicFormula("Happens", Vector(
          TermFunction("active", Vector(Variable("x", "id")), "event"),
          Variable("t", "time")))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 1
    }

    val atom5 = "Happens(active(ID1), t)"
    it(s"$atom5 should be a valid atomic formula") {
      val parsed = parser.parsePredicate(atom5)

      parsed shouldEqual
        AtomicFormula("Happens", Vector(
          TermFunction("active", Vector(Constant("ID1")), "event"),
          Variable("t", "time")))

      parsed.variables.size shouldEqual 1
      parsed.constants.size shouldEqual 1
      parsed.functions.size shouldEqual 1
    }

    val atom6 = "Initiates(inactive(x), meet(x, y), t)"
    it(s"$atom6 should be a valid atomic formula") {
      val parsed = parser.parsePredicate(atom6)

      parsed shouldEqual
        AtomicFormula("Initiates", Vector(
          TermFunction("inactive", Vector(Variable("x", "id")), "event"),
          TermFunction("meet", Vector(Variable("x", "id"), Variable("y", "id")), "fluent"),
          Variable("t", "time")))

      parsed.variables.size shouldEqual 3
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 2
    }

    val atom7 = "Happens(inactive(active(x)), t)"
    it(s"$atom7 should not be a valid atomic formula") {
      parser.parse(parser.atomicFormula, atom7).successful shouldBe false
      info("Nested functions are not supported!")
    }
  }

  describe("Definite clauses") {

    val clause1 = "InitiatedAt(f, t) :- Happens(e1, t) ^ Happens(e2, t)"
    it(s"$clause1 should be a valid definite clause") {
      val parsed = parser.parseDefiniteClause(clause1)

      parsed shouldEqual
        WeightedDefiniteClause(
          Double.NaN, DefiniteClause(
            AtomicFormula("InitiatedAt", Vector(Variable("f", "fluent"), Variable("t", "time"))),
            And(
              AtomicFormula("Happens", Vector(Variable("e1", "event"), Variable("t", "time"))),
              AtomicFormula("Happens", Vector(Variable("e2", "event"), Variable("t", "time"))))))

      parsed.variables.size shouldEqual 4
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 0
    }

    val clause2 = "InitiatedAt(Meet, t) :- Happens(inactive(x), t) ^ Happens(Active, t)."
    it(s"$clause2 should be a valid definite clause") {
      val parsed = parser.parseDefiniteClause(clause2)

      parsed shouldEqual
        WeightedDefiniteClause(
          Double.PositiveInfinity, DefiniteClause(
            AtomicFormula("InitiatedAt", Vector(Constant("Meet"), Variable("t", "time"))),
            And(
              AtomicFormula("Happens", Vector(
                TermFunction("inactive", Vector(Variable("x", "id")), "event"),
                Variable("t", "time"))),
              AtomicFormula("Happens", Vector(Constant("Active"), Variable("t", "time"))))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 2
      parsed.functions.size shouldEqual 1
    }

    val clause3 = "1.256 InitiatedAt(meet(x, y), t) :- Happens(inactive(x), t) ^ Happens(inactive(y), t)"
    it(s"$clause3 should be a valid definite clause") {
      val parsed = parser.parseDefiniteClause(clause3)

      parsed shouldEqual
        WeightedDefiniteClause(
          1.256, DefiniteClause(
            AtomicFormula("InitiatedAt", Vector(
              TermFunction("meet", Vector(Variable("x", "id"), Variable("y", "id")), "fluent"),
              Variable("t", "time"))),
            And(
              AtomicFormula("Happens", Vector(
                TermFunction("inactive", Vector(Variable("x", "id")), "event"),
                Variable("t", "time"))),
              AtomicFormula("Happens", Vector(
                TermFunction("inactive", Vector(Variable("y", "id")), "event"),
                Variable("t", "time"))))))

      parsed.variables.size shouldEqual 3
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 3
    }

    val clause4 = "-2.5 InitiatedAt(meet(x, y), t) :- !(Happens(inactive(x), t) ^ Happens(inactive(y), t))"
    it(s"$clause4 should be a valid definite clause") {
      val parsed = parser.parseDefiniteClause(clause4)

      parsed shouldEqual
        WeightedDefiniteClause(
          -2.5, DefiniteClause(
            AtomicFormula("InitiatedAt", Vector(
              TermFunction("meet", Vector(Variable("x", "id"), Variable("y", "id")), "fluent"),
              Variable("t", "time"))),
            Not(And(
              AtomicFormula("Happens", Vector(
                TermFunction("inactive", Vector(Variable("x", "id")), "event"),
                Variable("t", "time"))),
              AtomicFormula("Happens", Vector(
                TermFunction("inactive", Vector(Variable("y", "id")), "event"),
                Variable("t", "time")))))))

      parsed.variables.size shouldEqual 3
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 3
    }

    val clause5 = "1554 InitiatedAt(meet(x, y), t) :- Happens(inactive(x), t) ^ !Happens(exit(ID0), t)"
    it(s"$clause5 should be a valid definite clause") {
      val parsed = parser.parseDefiniteClause(clause5)

      parsed shouldEqual
        WeightedDefiniteClause(
          1554, DefiniteClause(
            AtomicFormula("InitiatedAt", Vector(
              TermFunction("meet", Vector(Variable("x", "id"), Variable("y", "id")), "fluent"),
              Variable("t", "time"))),
            And(
              AtomicFormula("Happens", Vector(
                TermFunction("inactive", Vector(Variable("x", "id")), "event"),
                Variable("t", "time"))),
              Not(AtomicFormula("Happens", Vector(
                TermFunction("exit", Vector(Constant("ID0")), "event"),
                Variable("t", "time")))))))

      parsed.variables.size shouldEqual 3
      parsed.constants.size shouldEqual 1
      parsed.functions.size shouldEqual 3
    }
  }

  describe("FOL sentences") {

    val formula1 = "Happens(Walking, t) => InitiatedAt(Moving, t)."
    it(s"$formula1 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(formula1)

      parsed shouldEqual
        WeightedFormula(
          Double.PositiveInfinity, Implies(
            AtomicFormula("Happens", Vector(Constant("Walking"), Variable("t", "time"))),
            AtomicFormula("InitiatedAt", Vector(Constant("Moving"), Variable("t", "time")))))

      parsed.variables.size shouldEqual 1
      parsed.constants.size shouldEqual 2
      parsed.functions.size shouldEqual 0
    }

    val formula2 = "Happens(walking(p1), t) ^ Happens(walking(p2), t) => InitiatedAt(move(p1, p2), t)."
    it(s"$formula2 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(formula2)

      parsed shouldEqual
        WeightedFormula(
          Double.PositiveInfinity, Implies(
            And(
              AtomicFormula("Happens", Vector(
                TermFunction("walking", Vector(Variable("p1", "id")), "event"),
                Variable("t", "time"))),
              AtomicFormula("Happens", Vector(
                TermFunction("walking", Vector(Variable("p2", "id")), "event"),
                Variable("t", "time")))),
            AtomicFormula("InitiatedAt", Vector(
              TermFunction("move", Vector(Variable("p1", "id"), Variable("p2", "id")), "fluent"),
              Variable("t", "time")))))

      parsed.variables.size shouldEqual 3
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 3
    }

    val formula3 = "InitiatedAt(move(p2, p1), t) <=> InitiatedAt(move(p1, p2), t)."
    it(s"$formula3 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(formula3)

      parsed shouldEqual
        WeightedFormula(
          Double.PositiveInfinity, Equivalence(
            AtomicFormula("InitiatedAt", Vector(
              TermFunction("move", Vector(Variable("p2", "id"), Variable("p1", "id")), "fluent"),
              Variable("t", "time"))),
            AtomicFormula("InitiatedAt", Vector(
              TermFunction("move", Vector(Variable("p1", "id"), Variable("p2", "id")), "fluent"),
              Variable("t", "time")))))

      parsed.variables.size shouldEqual 3
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 2
    }

    val formula4 = "124.2 InitiatedAt(f, t) => !HoldsAt(f, t)"
    it(s"$formula4 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(formula4)

      parsed shouldEqual
        WeightedFormula(
          124.2, Implies(
            AtomicFormula("InitiatedAt", Vector(
              Variable("f", "fluent"),
              Variable("t", "time"))),
            Not(AtomicFormula("HoldsAt", Vector(
              Variable("f", "fluent"),
              Variable("t", "time"))))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 0
    }
  }

  describe("Quantifiers") {

    val formula1 = "5.2 Exist f InitiatedAt(f, t) => !HoldsAt(f, t)"
    it(s"$formula1 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(formula1)

      parsed shouldEqual
        WeightedFormula(
          5.2, ExistentialQuantifier(Variable("f", "fluent"), Implies(
            AtomicFormula("InitiatedAt", Vector(
              Variable("f", "fluent"),
              Variable("t", "time"))),
            Not(AtomicFormula("HoldsAt", Vector(
              Variable("f", "fluent"),
              Variable("t", "time")))))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 0
    }

    val formula2 = "FORALL f,t InitiatedAt(f, t) => !HoldsAt(f, t)."
    it(s"$formula2 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(formula2)

      parsed shouldEqual
        WeightedFormula(
          Double.PositiveInfinity,
          UniversalQuantifier(Variable("f", "fluent"), UniversalQuantifier(Variable("t", "time"), Implies(
            AtomicFormula("InitiatedAt", Vector(
              Variable("f", "fluent"),
              Variable("t", "time"))),
            Not(AtomicFormula("HoldsAt", Vector(
              Variable("f", "fluent"),
              Variable("t", "time"))))))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 0
    }

    val formula3 = "Forall t !(Exist f InitiatedAt(f, t) => HoldsAt(f, t))"
    it(s"$formula3 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(formula3)

      parsed shouldEqual
        WeightedFormula(
          Double.NaN,
          UniversalQuantifier(
            Variable("t", "time"),
            Not(ExistentialQuantifier(Variable("f", "fluent"), Implies(
              AtomicFormula("InitiatedAt", Vector(
                Variable("f", "fluent"),
                Variable("t", "time"))),
              AtomicFormula("HoldsAt", Vector(
                Variable("f", "fluent"),
                Variable("t", "time"))))))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 0
    }
  }

  describe("Dynamic atomic formulas") {

    val dynamicFormula1 = "f1 = f2"
    it(s"$dynamicFormula1 should be a valid FOL sentence") {
      val parsed = parser.parsePredicate(dynamicFormula1)

      parsed shouldEqual equals(Vector(Variable("f1"), Variable("f2")))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 0
    }

    val dynamicFormula2 = "f = Meet"
    it(s"$dynamicFormula2 should be a valid FOL sentence") {
      val parsed = parser.parsePredicate(dynamicFormula2)

      parsed shouldEqual equals(Vector(Variable("f"), Constant("Meet")))

      parsed.variables.size shouldEqual 1
      parsed.constants.size shouldEqual 1
      parsed.functions.size shouldEqual 0
    }

    val dynamicFormula3 = "f != Move"
    it(s"$dynamicFormula3 should be a valid FOL sentence") {
      val parsed = parser.parsePredicate(dynamicFormula3)

      parsed shouldEqual notEquals(Vector(Variable("f"), Constant("Move")))

      parsed.variables.size shouldEqual 1
      parsed.constants.size shouldEqual 1
      parsed.functions.size shouldEqual 0
    }

    val dynamicFormula4 = "HoldsAt(f, t1) ^ HoldsAt(f, t2) ^ t1 != t2."
    it(s"$dynamicFormula4 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula4)

      parsed shouldEqual
        WeightedFormula(
          Double.PositiveInfinity, And(
            And(
              AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), Variable("t1", "time"))),
              AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), Variable("t2", "time")))),
            notEquals(Vector(Variable("t1", "time"), Variable("t2", "time"))))
        )

      parsed.variables.size shouldEqual 3
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 0
    }

    val dynamicFormula5 = "HoldsAt(f, t) ^ t > 5 ^ t < 24."
    it(s"$dynamicFormula5 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula5)

      parsed shouldEqual
        WeightedFormula(
          Double.PositiveInfinity, And(
            And(
              AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), Variable("t", "time"))),
              greaterThan(Vector(Variable("t", "time"), Constant("5")))),
            lessThan(Vector(Variable("t", "time"), Constant("24")))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 2
      parsed.functions.size shouldEqual 0
    }

    val dynamicFormula6 = "HoldsAt(f, t) ^ t >= 5 ^ t =< 24."
    it(s"$dynamicFormula6 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula6)

      parsed shouldEqual
        WeightedFormula(
          Double.PositiveInfinity, And(
            And(
              AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), Variable("t", "time"))),
              greaterThanEq(Vector(Variable("t", "time"), Constant("5")))),
            lessThanEq(Vector(Variable("t", "time"), Constant("24")))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 2
      parsed.functions.size shouldEqual 0
    }

    val dynamicFormula7 = "HoldsAt(f, t) <=> Substr(Meet, f)."
    it(s"$dynamicFormula7 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula7)

      parsed shouldEqual
        WeightedFormula(
          Double.PositiveInfinity, Equivalence(
            AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), Variable("t", "time"))),
            substring(Vector(Constant("Meet"), Variable("f", "fluent")))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 1
      parsed.functions.size shouldEqual 0
    }
  }

  describe("Dynamic functions") {

    val dynamicFormula1 = "InitiatedAt(f, t) => HoldsAt(f, t + 1)."
    it(s"$dynamicFormula1 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula1)

      parsed shouldEqual
        WeightedFormula(Double.PositiveInfinity, Implies(
          AtomicFormula("InitiatedAt", Vector(Variable("f", "fluent"), Variable("t", "time"))),
          AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), functionPlus(Variable("t", "time"), Constant("1"), "time")))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 1
      parsed.functions.size shouldEqual 1
    }

    val dynamicFormula2 = "InitiatedAt(f, t - 1) => HoldsAt(f, t)."
    it(s"$dynamicFormula2 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula2)

      parsed shouldEqual
        WeightedFormula(Double.PositiveInfinity, Implies(
          AtomicFormula("InitiatedAt", Vector(Variable("f", "fluent"), functionMinus(Variable("t", "time"), Constant("1"), "time"))),
          AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), Variable("t", "time")))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 1
      parsed.functions.size shouldEqual 1
    }

    val dynamicFormula3 = "InitiatedAt(f, t) => HoldsAt(f, t++)."
    it(s"$dynamicFormula3 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula3)

      parsed shouldEqual
        WeightedFormula(Double.PositiveInfinity, Implies(
          AtomicFormula("InitiatedAt", Vector(Variable("f", "fluent"), Variable("t", "time"))),
          AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), functionNext(Variable("t", "time"), "time")))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 1
    }

    val dynamicFormula4 = "InitiatedAt(f, t--) => HoldsAt(f, t)."
    it(s"$dynamicFormula4 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula4)

      parsed shouldEqual
        WeightedFormula(Double.PositiveInfinity, Implies(
          AtomicFormula("InitiatedAt", Vector(Variable("f", "fluent"), functionPrev(Variable("t", "time"), "time"))),
          AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), Variable("t", "time")))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 1
    }

    val dynamicFormula5 = "1.2 InitiatedAt(f, t) => HoldsAt(f, t * 5)"
    it(s"$dynamicFormula5 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula5)

      parsed shouldEqual
        WeightedFormula(1.2, Implies(
          AtomicFormula("InitiatedAt", Vector(Variable("f", "fluent"), Variable("t", "time"))),
          AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), functionTimes(Variable("t", "time"), Constant("5"), "time")))))

      parsed.variables.size shouldEqual 2
      parsed.constants.size shouldEqual 1
      parsed.functions.size shouldEqual 1
    }

    val dynamicFormula6 = "Initiates(e, f, t1) ^ Terminates(e, f, t2) <=> (Happens(e, t1 / t2) v Happens(e, t1 % t2)) ^ t1 < t2"
    it(s"$dynamicFormula6 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula6)

      parsed shouldEqual
        WeightedFormula(
          Double.NaN, Equivalence(
            And(
              AtomicFormula("Initiates", Vector(Variable("e", "event"), Variable("f", "fluent"), Variable("t1", "time"))),
              AtomicFormula("Terminates", Vector(Variable("e", "event"), Variable("f", "fluent"), Variable("t2", "time")))),
            And(Or(
              AtomicFormula("Happens", Vector(Variable("e", "event"), functionDiv(Variable("t1", "time"), Variable("t2", "time"), "time"))),
              AtomicFormula("Happens", Vector(Variable("e", "event"), functionMod(Variable("t1", "time"), Variable("t2", "time"), "time")))),
                lessThan(Vector(Variable("t1", "time"), Variable("t2", "time"))))))

      parsed.variables.size shouldEqual 4
      parsed.constants.size shouldEqual 0
      parsed.functions.size shouldEqual 2
    }

    val dynamicFormula7 = "HoldsAt(meet(BOB, ALICE), t) <=> Close(concat(B, OB), ALICE, 25, t)"
    it(s"$dynamicFormula7 should be a valid FOL sentence") {
      val parsed = parser.parseLogicalSentence(dynamicFormula7)

      parsed shouldEqual
        WeightedFormula(
          Double.NaN, Equivalence(
            AtomicFormula("HoldsAt", Vector(TermFunction("meet", Vector(Constant("BOB"), Constant("ALICE")), "fluent"), Variable("t", "time"))),
            AtomicFormula("Close", Vector(functionConcat(Constant("B"), Constant("OB"), "id"), Constant("ALICE"), Constant("25"), Variable("t", "time")))
          )
        )

      parsed.variables.size shouldEqual 1
      parsed.constants.size shouldEqual 5
      parsed.functions.size shouldEqual 2
    }
  }
}
