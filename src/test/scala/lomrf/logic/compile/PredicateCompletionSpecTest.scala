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

package lomrf.logic.compile

import lomrf.logic.{ AtomSignature, WeightedDefiniteClause, WeightedFormula }
import lomrf.logic.parser.KBParser
import org.scalatest.{ FunSpec, Matchers }

final class PredicateCompletionSpecTest extends FunSpec with Matchers {

  private val predicateSchema = Map(
    AtomSignature("InitiatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("TerminatedAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Happens", 2) -> Vector("event", "time"),
    AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Close", 4) -> Vector("id", "id", "dist", "time"),
  )

  private val functionsSchema = Map(
    AtomSignature("walking", 1) -> ("event", Vector("id")),
    AtomSignature("abrupt", 1) -> ("event", Vector("id")),
    AtomSignature("active", 1) -> ("event", Vector("id")),
    AtomSignature("inactive", 1) -> ("event", Vector("id")),
    AtomSignature("fight", 2) -> ("fluent", Vector("id", "id")),
    AtomSignature("move", 2) -> ("fluent", Vector("id", "id")),
    AtomSignature("meet", 2) -> ("fluent", Vector("id", "id"))
  )

  private val parser = new KBParser(predicateSchema, functionsSchema)

  private def standard(set: Set[WeightedDefiniteClause]): Set[WeightedFormula] =
    PredicateCompletion(Set.empty, set, PredicateCompletionMode.Standard)(predicateSchema, functionsSchema, Map.empty)

  private def decomposed(set: Set[WeightedDefiniteClause]): Set[WeightedFormula] =
    PredicateCompletion(Set.empty, set, PredicateCompletionMode.Decomposed)(predicateSchema, functionsSchema, Map.empty)

  private def simplify(formulas: Set[WeightedFormula], set: Set[WeightedDefiniteClause]): Set[WeightedFormula] =
    PredicateCompletion(formulas, set, PredicateCompletionMode.Simplification)(predicateSchema, functionsSchema, Map.empty)

  /*
   * Predicate completion for the definite clauses:
   *
   * 1. InitiatedAt(fight(x,y), t) :- Happens(abrupt(x),t) ^ Happens(active(y),t)
   * 2. TerminatedAt(fight(x,y), t) :- Happens(walking(x),t) ^ Happens(walking(y),t)
   */

  val clauseSetA: Set[WeightedDefiniteClause] = Set(
    parser.parseDefiniteClause("InitiatedAt(fight(x,y), t) :- Happens(abrupt(x),t) ^ Happens(active(y),t)"),
    parser.parseDefiniteClause("TerminatedAt(fight(x,y), t) :- Happens(walking(x),t) ^ Happens(walking(y),t)")
  )

  describe(s"Standard completion for ${clauseSetA.map(_.toText).mkString(" and ")}") {

    it("should produce one hard-constrained equivalence") {
      val expected = Set(
        parser.parseWeightedFormula(
          """
            | InitiatedAt(fight(x,y), t) <=> Happens(abrupt(x),t) ^ Happens(active(y),t).
          """.stripMargin
        ),
        parser.parseWeightedFormula(
          """
            | TerminatedAt(fight(x,y), t) <=> Happens(walking(x),t) ^ Happens(walking(y),t).
          """.stripMargin
        )
      )

      standard(clauseSetA) shouldEqual expected
    }
  }

  describe(s"Decomposed completion for ${clauseSetA.map(_.toText).mkString(" and ")}") {

    it("should produce one hard-constrained implication and a pair of weighed implications") {
      val expected = Set(
        parser.parseWeightedFormula(
          """
            | Happens(abrupt(x),t) ^ Happens(active(y),t) => InitiatedAt(fight(x,y), t)
          """.stripMargin
        ),
        parser.parseWeightedFormula(
          """
            | Happens(walking(x),t) ^ Happens(walking(y),t) => TerminatedAt(fight(x,y), t)
          """.stripMargin
        ),
        parser.parseWeightedFormula(
          """
            | InitiatedAt(fight(x,y), t) => Happens(abrupt(x),t) ^ Happens(active(y),t).
          """.stripMargin
        ),
        parser.parseWeightedFormula(
          """
            | TerminatedAt(fight(x,y), t) => Happens(walking(x),t) ^ Happens(walking(y),t).
          """.stripMargin
        )
      )

      decomposed(clauseSetA) shouldEqual expected
    }
  }

  describe(s"Simplified completion for ${clauseSetA.map(_.toText).mkString(" and ")}") {

    val axioms: Set[WeightedFormula] = Set(
      parser.parseWeightedFormula("InitiatedAt(f, t) => HoldsAt(f, t++)."),
      parser.parseWeightedFormula("!HoldsAt(f,t) ^ !InitiatedAt(f,t) => !HoldsAt(f,t++)"),
      parser.parseWeightedFormula("TerminatedAt(f, t) => !HoldsAt(f, t++)."),
      parser.parseWeightedFormula("HoldsAt(f,t) ^ !TerminatedAt(f,t) => HoldsAt(f,t++)")
    )

    val expected = Set(
      parser.parseWeightedFormula(
        """
          | Happens(abrupt(x),t) ^ Happens(active(y),t) => HoldsAt(fight(x, y), t++).
        """.stripMargin
      ),
      parser.parseWeightedFormula(
        """
          | Happens(walking(x),t) ^ Happens(walking(y),t) => !HoldsAt(fight(x, y), t++).
        """.stripMargin
      ),
      parser.parseWeightedFormula(
        """
          | !HoldsAt(fight(x, y),t) ^ !(Happens(abrupt(x),t) ^ Happens(active(y),t)) => !HoldsAt(fight(x, y),t++)
        """.stripMargin
      ),
      parser.parseWeightedFormula(
        """
          | HoldsAt(fight(x, y),t) ^ !(Happens(walking(x),t) ^ Happens(walking(y),t)) => HoldsAt(fight(x, y),t++)
        """.stripMargin
      )
    )

    it("should produce a pair of simplified implications, one for each axiom") {
      simplify(axioms, clauseSetA) shouldEqual expected
    }
  }

  /*
   * Predicate completion for the definite clauses:
   *
   * 1. InitiatedAt(meet(x,y), t) :- Happens(active(x),t) ^ Happens(inactive(y),t)
   * 2. InitiatedAt(meet(x,y), t) :- Happens(inactive(x),t) ^ !Happens(walking(y),t1) ^ t1 < t
   */

  val clauseSetB: Set[WeightedDefiniteClause] = Set(
    parser.parseDefiniteClause("InitiatedAt(meet(x,y), t) :- Happens(active(x),t) ^ Happens(inactive(y),t)"),
    parser.parseDefiniteClause("InitiatedAt(meet(x,y), t) :- Happens(inactive(x),t) ^ !Happens(walking(y),t1) ^ t1 < t")
  )

  describe(s"Standard completion for ${clauseSetB.map(_.toText).mkString(" and ")}") {

    it("should produce one hard-constrained equivalence") {
      val expected = Set(
        parser.parseWeightedFormula(
          """
            | InitiatedAt(meet(x,y), t) <=>
            | (Happens(active(x),t) ^ Happens(inactive(y),t)) v
            | (Exist t1 Happens(inactive(x),t) ^ !Happens(walking(y),t1) ^ t1 < t).
          """.stripMargin
        )
      )

      standard(clauseSetB) shouldEqual expected
    }
  }

  describe(s"Decomposed completion for ${clauseSetB.map(_.toText).mkString(" and ")}") {

    it("should produce one hard-constrained implication and a pair of weighed implications") {
      val expected = Set(
        parser.parseWeightedFormula(
          """
            | Happens(active(x),t) ^ Happens(inactive(y),t) => InitiatedAt(meet(x,y), t)
          """.stripMargin
        ),
        parser.parseWeightedFormula(
          """
            | Happens(inactive(x),t) ^ !Happens(walking(y),t1) ^ t1 < t => InitiatedAt(meet(x,y), t)
          """.stripMargin
        ),
        parser.parseWeightedFormula(
          """
            | InitiatedAt(meet(x,y), t) =>
            | (Happens(active(x),t) ^ Happens(inactive(y),t)) v
            | (Exist t1 Happens(inactive(x),t) ^ !Happens(walking(y),t1) ^ t1 < t).
          """.stripMargin
        )
      )

      decomposed(clauseSetB) shouldEqual expected
    }
  }

  describe(s"Simplified completion for ${clauseSetB.map(_.toText).mkString(" and ")}") {

    val axioms: Set[WeightedFormula] = Set(
      parser.parseWeightedFormula("InitiatedAt(f, t) => HoldsAt(f, t++)."),
      parser.parseWeightedFormula("!HoldsAt(f,t) ^ !InitiatedAt(f,t) => !HoldsAt(f,t++)")
    )

    val expected = Set(
      parser.parseWeightedFormula(
        """
          | (Exist t1 Happens(inactive(x),t) ^ !Happens(walking(y),t1) ^ LessThan(t1,t)) v
          | (Happens(active(x),t) ^ Happens(inactive(y),t)) =>
          | HoldsAt(meet(x, y), t++).
        """.stripMargin
      ),
      parser.parseWeightedFormula(
        """
          | !HoldsAt(meet(x, y),t) ^
          | !((Exist t1 Happens(inactive(x),t) ^ !Happens(walking(y),t1) ^ LessThan(t1,t)) v
          | (Happens(active(x),t) ^ Happens(inactive(y),t))) =>
          | !HoldsAt(meet(x, y),t++)
        """.stripMargin
      )
    )

    it("should produce a pair of simplified implications, one for each axiom") {
      simplify(axioms, clauseSetB) shouldEqual expected
    }
  }
}
