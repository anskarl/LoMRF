package lomrf.mln.model

import lomrf.logic._
import lomrf.logic.LogicOps._
import org.scalatest.{FunSpec, Matchers}

/**
 * Logic Formatter specification test.
 */
final class LogicFormatterSpecTest extends FunSpec with Matchers {

  private val eventCalculusSchema = Map(
    AtomSignature("InitiatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Happens", 2) -> Vector("event", "time"),
    AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"),
    AtomSignature(s"${lomrf.AUX_PRED_PREFIX}foo", 2) -> Vector("event", "id"),
    AtomSignature(s"${lomrf.AUX_PRED_PREFIX}bar",3) -> Vector("event", "id", "id"),
    AtomSignature(s"${lomrf.AUX_PRED_PREFIX}qax",3) -> Vector("fluent", "id", "id"))

  private val sampleFunctionsSchema = Map(
    AtomSignature("foo", 1) -> ("event", Vector("id")),
    AtomSignature("bar", 2) -> ("event", Vector("id", "id")),
    AtomSignature("qax", 2) -> ("fluent", Vector("id", "id")))

  private val parser = new KBParser(eventCalculusSchema, sampleFunctionsSchema)

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Clause Formatter (clauses having functions)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Clause formatter operations on clauses having functions (function introduction and elimination") {

    val clauses = NormalForm.compileCNF(Seq(
      "1.0 Happens(foo(id1), t) v HoldsAt(qax(id1, id2), t)",
      "1.0 !Happens(foo(person1), t) v Happens(foo(person2), t)",
      "1.0 Happens(bar(p1, p2), t) v Happens(foo(p2), t) v !HoldsAt(qax(p1, p2), t)",
      "1.0 Happens(bar(Anna, Bob), t) v Happens(foo(Bob), t) v !Happens(foo(Anna), t)"
    ).map(parser.parseLogicalSentence))

    info(s"Initial set of clauses:\n${clauses.mkString("\n")}")

    info("-- Performing function elimination --")
    val eliminated = LogicFormatter.ClauseFormatter.eliminateFunctions(clauses)

    it("should not contain any functions and should contain as many auxiliary predicates as functions of the original clause") {
      eliminated.forall(_.functions.isEmpty) shouldBe true
      eliminated zip clauses forall { case (e, c) =>
        e.literals.count(_.sentence.symbol.contains(lomrf.AUX_PRED_PREFIX)) == c.functions.size
      } shouldBe true
    }

    info(s"Clauses without functions:\n${eliminated.mkString("\n")}")

    info("-- Performing function introduction --")
    val introduced = LogicFormatter.ClauseFormatter.introduceFunctions(eliminated)

    it("should not contain auxiliary predicates and contain as many functions as the original clause") {
      introduced.forall(!_.literals.exists(_.sentence.symbol.contains(lomrf.AUX_PRED_PREFIX))) shouldBe true
      introduced zip clauses forall { case (i, c) =>
        i.functions.size ==  c.functions.size
      } shouldBe true
    }

    info(s"Clauses with functions:\n${introduced.mkString("\n")}")
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Clause Formatter (Unsupported case: Having positive auxiliary predicates)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Unsupported clause formatter operations (introduce functions when having positive auxiliary predicates)") {

    val clauses = NormalForm.compileCNF(Seq(
      s"1.0 Happens(${lomrf.FUNC_RET_VAR_PREFIX}0, t) v ${lomrf.AUX_PRED_PREFIX}foo(${lomrf.FUNC_RET_VAR_PREFIX}0, id1)" +
      s" v HoldsAt(${lomrf.FUNC_RET_VAR_PREFIX}1, t) v ${lomrf.AUX_PRED_PREFIX}qax(${lomrf.FUNC_RET_VAR_PREFIX}1, id1, id2)"
    ).map(parser.parseLogicalSentence))

    info(s"Initial set of clauses:\n${clauses.mkString("\n")}")

    info("-- Performing function introduction --")
    it("should not be able to perform function introduction for clauses having positive auxiliary predicates") {
      intercept[IllegalArgumentException] {
        LogicFormatter.ClauseFormatter.introduceFunctions(clauses)
      }
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Definite Clause Formatter (definite clauses having functions)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Definite clause formatter operations on clauses having functions (function introduction and elimination") {

    val definiteClauses = Seq(
      "InitiatedAt(Qax,t) :- Happens(Foo, t)",
      "InitiatedAt(qax(person1, person2),t) :- Happens(foo(person1), t) ^ Happens(foo(person2), t)",
      "InitiatedAt(qax(id1, id2), t) :- HoldsAt(qax(id1, id2), t)"
    ).map(parser.parseDefiniteClause)

    info(s"Initial set of definite clauses:\n${definiteClauses.mkString("\n")}")

    info("-- Performing function elimination --")
    val eliminated = LogicFormatter.WeightedDefiniteClauseFormatter
      .eliminateFunctions(definiteClauses)

    it("should not contain any functions and should contain as many auxiliary predicates as functions of the original clause") {
      eliminated.forall(_.functions.isEmpty) shouldBe true
      eliminated zip definiteClauses forall { case (e, c) =>
        e.clause.literals.count(_.sentence.symbol.contains(lomrf.AUX_PRED_PREFIX)) == c.functions.size
      } shouldBe true
    }

    info(s"Definite clauses without functions:\n${eliminated.mkString("\n")}")

    info("-- Performing function introduction --")
    val introduced = LogicFormatter.WeightedDefiniteClauseFormatter
      .introduceFunctions(eliminated)

    it("should not contain auxiliary predicates and contain as many functions as the original clause") {
      introduced.forall(!_.clause.literals.exists(_.sentence.symbol.contains(lomrf.AUX_PRED_PREFIX))) shouldBe true
      introduced zip definiteClauses forall { case (i, c) =>
        i.functions.size ==  c.functions.size
      } shouldBe true
    }

    info(s"Definite clauses with functions:\n ${introduced.mkString("\n")}")
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Definite Clause Formatter (Unsupported case: Having negated auxiliary predicates)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Unsupported definite clause formatter operations (introduce functions when having negated auxiliary predicates)") {

    val definiteClauses = Seq(
      s"InitiatedAt(${lomrf.FUNC_RET_VAR_PREFIX}1, t) :- !${lomrf.AUX_PRED_PREFIX}qax(${lomrf.FUNC_RET_VAR_PREFIX}1, id1, id2)" +
        s"^ Happens(${lomrf.FUNC_RET_VAR_PREFIX}2, t) ^ !${lomrf.AUX_PRED_PREFIX}foo(${lomrf.FUNC_RET_VAR_PREFIX}2, id1)"
    ).map(parser.parseDefiniteClause)

    info(s"Initial set of definite clauses:\n${definiteClauses.mkString("\n")}")

    info("-- Performing function introduction --")
    it("should not be able to perform function introduction for definite clauses having negated auxiliary predicates") {
      intercept[IllegalArgumentException] {
        LogicFormatter.WeightedDefiniteClauseFormatter.introduceFunctions(definiteClauses)
      }
    }
  }

}
