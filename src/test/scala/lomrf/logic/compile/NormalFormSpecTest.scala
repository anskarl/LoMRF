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

import lomrf.logic.parser.KBParser
import lomrf.logic.{ AtomSignature, Clause, Literal }
import lomrf.mln.model.ConstantsSet
import org.scalatest.{ FunSpec, Matchers }

/**
  * A series of specification tests for the computation of normal forms (e.g., CNF, NNF, etc).
  */
final class NormalFormSpecTest extends FunSpec with Matchers {

  private implicit val constants: Map[String, ConstantsSet] = Map(
    "time" -> ConstantsSet("1", "2", "3", "4"),
    "event" -> ConstantsSet("Abrupt", "Walking", "Running", "Active", "Inactive", "Exit"),
    "fluent" -> ConstantsSet("Fight", "Move", "Meet", "Leaving_object"),
    "dist" -> ConstantsSet("24", "30", "35"),
    "id" -> ConstantsSet("ID1", "ID2", "ID3"),
    "a" -> ConstantsSet((1 to 4).map(_.toString): _*),
    "b" -> ConstantsSet("B1", "B2")
  )

  private val predicateSchema = Map(
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
    AtomSignature("StoppedAt", 3) -> Vector("event", "fluent", "time"),

    AtomSignature("Predicate1", 1) -> Vector("a"),
    AtomSignature("Predicate2", 2) -> Vector("a", "b"),
    AtomSignature("Predicate3", 1) -> Vector("b")
  )

  private val functionsSchema = Map(
    AtomSignature("walking", 1) -> ("event", Vector("id")),
    AtomSignature("abrupt", 1) -> ("event", Vector("id")),
    AtomSignature("running", 1) -> ("event", Vector("id")),
    AtomSignature("active", 1) -> ("event", Vector("id")),
    AtomSignature("inactive", 1) -> ("event", Vector("id")),
    AtomSignature("exit", 1) -> ("event", Vector("id")),
    AtomSignature("fight", 2) -> ("fluent", Vector("id", "id")),
    AtomSignature("move", 2) -> ("fluent", Vector("id", "id")),
    AtomSignature("meet", 2) -> ("fluent", Vector("id", "id")),
    AtomSignature("leaving_object", 2) -> ("fluent", Vector("id", "id"))
  )

  private val kbParser = new KBParser(predicateSchema, functionsSchema)

  describe("Formula 'InitiatedAt(Fight,t) => Happens(Abrupt, t).'") {

    val formula = kbParser.parseLogicalSentence("InitiatedAt(Fight,t) => Happens(Abrupt, t).")
    val clauses = NormalForm.toCNF(formula)
    val fastClauses = NormalForm.toFastCNF(formula)

    it("should result to a single valid clause") {
      clauses.size shouldEqual 1
      clauses.head.size shouldBe 2
      assert(clauses.head.literals.contains(kbParser.parseLiteral("!InitiatedAt(Fight,t)")))
      assert(clauses.head.literals.contains(kbParser.parseLiteral("Happens(Abrupt, t)")))

      fastClauses.size shouldEqual 1
      fastClauses.head.size shouldBe 2
      assert(fastClauses.head.literals.contains(kbParser.parseLiteral("!InitiatedAt(Fight,t)")))
      assert(fastClauses.head.literals.contains(kbParser.parseLiteral("Happens(Abrupt, t)")))
    }
  }

  describe("Formula 'InitiatedAt(Fight,t) <=> Happens(Abrupt, t).'") {

    val formula = kbParser.parseLogicalSentence("InitiatedAt(Fight,t) <=> Happens(Abrupt, t).")
    val clauses = NormalForm.toCNF(formula)
    val fastClauses = NormalForm.toFastCNF(formula)

    it("should produce two valid clauses") {
      clauses.size shouldEqual 2
      assertContainsClause(clauses, "{!InitiatedAt(Fight,t) | Happens(Abrupt, t)}")
      assertContainsClause(clauses, "{InitiatedAt(Fight,t) | !Happens(Abrupt, t)}")

      fastClauses.size shouldEqual 2
      assertContainsClause(fastClauses, "{!InitiatedAt(Fight,t) | Happens(Abrupt, t)}")
      assertContainsClause(fastClauses, "{InitiatedAt(Fight,t) | !Happens(Abrupt, t)}")
    }
  }

  describe("Formula 'InitiatedAt(Fight,t) <=> Happens(Abrupt, t) ^ Close(10,t).'") {
    val formula = kbParser.parseLogicalSentence("InitiatedAt(Fight,t) <=> Happens(Abrupt, t) ^ Close(10,t).")
    val clauses = formula.toCNF(constants)
    val fastClauses = NormalForm.toFastCNF(formula)(constants)

    it("should produce three valid clauses") {
      clauses.size shouldBe 3
      assertContainsClause(clauses, "{InitiatedAt(Fight, t) | !Happens(Abrupt, t) | !Close(10,t)}")
      assertContainsClause(clauses, "{Happens(Abrupt,t) | !InitiatedAt(Fight, t)}")
      assertContainsClause(clauses, "{Close(10,t) | !InitiatedAt(Fight, t)}")

      fastClauses.size shouldBe 3
      assertContainsClause(fastClauses, "{InitiatedAt(Fight, t) | !Happens(Abrupt, t) | !Close(10,t)}")
      assertContainsClause(fastClauses, "{Happens(Abrupt,t) | !InitiatedAt(Fight, t)}")
      assertContainsClause(fastClauses, "{Close(10,t) | !InitiatedAt(Fight, t)}")
    }
  }

  describe("Formula 'Happens(walking(id1),t) => Initiates(walking(id1), move(id,id2), t).'") {
    val f = kbParser.parseLogicalSentence("Happens(walking(id1),t) => Initiates(walking(id1), move(id,id2), t).")
    val clauses = f.toCNF(constants)
    val fastClauses = NormalForm.toFastCNF(f)(constants)

    it("should produce a single clause") {
      clauses.size shouldEqual 1
      fastClauses.size shouldEqual 1
    }
  }

  describe("Formulas: " +
    "'InitiatedAt(Fight,t) => Happens(Abrupt, t) ^ Close(10,t).' and " +
    "'Happens(Abrupt, t) ^ Close(10,t) => InitiatedAt(Fight,t).'") {

    val f1 = kbParser.parseLogicalSentence("InitiatedAt(Fight,t) => Happens(Abrupt, t) ^ Close(10,t).")
    val f2 = kbParser.parseLogicalSentence("Happens(Abrupt, t) ^ Close(10,t) => InitiatedAt(Fight,t).")
    val clauses = f1.toCNF(constants) ++ f2.toCNF(constants)
    val fastClauses = NormalForm.compileFastCNF(Set(f1, f2))(constants)

    they("should produce three valid clauses") {
      clauses.size shouldBe 3

      assertContainsClause(clauses, "{InitiatedAt(Fight, t) | !Happens(Abrupt, t) | !Close(10,t)}")
      assertContainsClause(clauses, "{Happens(Abrupt,t) | !InitiatedAt(Fight, t)}")
      assertContainsClause(clauses, "{Close(10,t) | !InitiatedAt(Fight, t)}")

      fastClauses.size shouldBe 3

      assertContainsClause(fastClauses, "{InitiatedAt(Fight, t) | !Happens(Abrupt, t) | !Close(10,t)}")
      assertContainsClause(fastClauses, "{Happens(Abrupt,t) | !InitiatedAt(Fight, t)}")
      assertContainsClause(fastClauses, "{Close(10,t) | !InitiatedAt(Fight, t)}")
    }

    they("should produce the same clauses as the formula 'InitiatedAt(Fight,t) <=> Happens(Abrupt, t) ^ Close(10,t).'") {
      val formula = kbParser.parseLogicalSentence("InitiatedAt(Fight,t) <=> Happens(Abrupt, t) ^ Close(10,t).")
      val formulaClauses = formula.toCNF(constants)
      val fastFormulaClauses = NormalForm.toFastCNF(formula)(constants)

      clauses.foreach(c => formulaClauses.contains(c) shouldEqual true)
      clauses.foreach(c => fastFormulaClauses.contains(c) shouldEqual true)
    }
  }

  describe("Weighted formulas: " +
    "'1 Happens(Abrupt, t) ^ Close(24,t) ^ !Happens(Inactive,t ) => InitiatedAt(Fight, t)' and " +
    "'3 InitiatedAt(Fight,t) => Happens(Abrupt, t) ^ !Happens(Inactive, t) ^ Close(24,t)'") {

    val f0 = kbParser.parseLogicalSentence("1 Happens(Abrupt, t) ^ Close(24,t) ^ !Happens(Inactive,t ) => InitiatedAt(Fight, t)")
    val f1 = kbParser.parseLogicalSentence("3 InitiatedAt(Fight,t) => Happens(Abrupt, t) ^ !Happens(Inactive, t) ^ Close(24,t)")

    val clauses01 = f0.toCNF(constants) ++ f1.toCNF(constants)
    val fastClauses01 = NormalForm.compileFastCNF(Set(f0, f1))(constants)

    they("should produce four clauses") {
      clauses01.size shouldEqual 4
      fastClauses01.size shouldEqual 4
    }

    they("should produce the same clauses as the weighed formula '4 Happens(Abrupt, t) ^ Close(24,t) ^ !Happens(Inactive,t ) <=> InitiatedAt(Fight, t)'") {
      val f3 = kbParser.parseLogicalSentence("4 Happens(Abrupt, t) ^ Close(24,t) ^ !Happens(Inactive,t ) <=> InitiatedAt(Fight, t)")
      val clauses3 = f3.toCNF(constants)
      val fastClauses3 = NormalForm.toFastCNF(f3)(constants)

      clauses3.size shouldEqual 4
      clauses01.foreach(c => clauses3.contains(c) shouldEqual true)

      fastClauses3.size shouldEqual 4
      clauses01.foreach(c => fastClauses3.contains(c) shouldEqual true)
    }
  }

  describe("Formula 'Exist x,t Happens(walking(x), t).'") {
    val f = kbParser.parseLogicalSentence("Exist x,t Happens(walking(x), t).")
    val clauses = f.toCNF(constants)
    val fastClauses = NormalForm.toFastCNF(f)(constants)

    it("should produce a single clause") {
      assert(clauses.size == 1)
      assert(fastClauses.size == 1)
    }

    it("should have 12 literals") {
      assert(clauses.head.size == 12)
      assert(fastClauses.head.size == 12)
    }
  }

  describe("Weighted formulas: " +
    "1 Happens(abrupt(p1), t) ^ Close(p1,p2,24,t) ^ !Happens(inactive(p2),t ) => InitiatedAt(fight(p1,p2), t) and " +
    "3 InitiatedAt(fight(p1,p2),t) => Happens(abrupt(p1), t) ^ !Happens(inactive(p2), t) ^ Close(p1,p2,24,t)") {

    val f0 = kbParser.parseLogicalSentence("1 Happens(abrupt(p1), t) ^ Close(p1,p2,24,t) ^ !Happens(inactive(p2),t ) => InitiatedAt(fight(p1,p2), t)")
    val f1 = kbParser.parseLogicalSentence("3 InitiatedAt(fight(p1,p2),t) => Happens(abrupt(p1), t) ^ !Happens(inactive(p2), t) ^ Close(p1,p2,24,t)")
    val clauses01 = f0.toCNF(constants) ++ f1.toCNF(constants)
    val fastClauses01 = NormalForm.compileFastCNF(Set(f0, f1))(constants)

    they("should produce four clauses") {
      assert(clauses01.size == 4)
      assert(fastClauses01.size == 4)
    }

    they("should produce the same clauses with formula '4 Happens(abrupt(p1), t) ^ Close(p1,p2,24,t) ^ !Happens(inactive(p2),t ) <=> InitiatedAt(fight(p1,p2), t)'") {
      val f3 = kbParser.parseLogicalSentence("4 Happens(abrupt(p1), t) ^ Close(p1,p2,24,t) ^ !Happens(inactive(p2),t ) <=> InitiatedAt(fight(p1,p2), t)")
      val clauses3 = f3.toCNF(constants)
      val fastClause3 = NormalForm.toFastCNF(f3)(constants)

      assert(clauses3.size == 4)
      clauses01.foreach(c => assert(clauses3.contains(c)))

      assert(fastClause3.size == 4)
      clauses01.foreach(c => assert(fastClause3.contains(c)))
    }
  }

  describe("Discrete Event Calculus dialects:") {
    val constants = Map(
      "time" -> ConstantsSet("1", "2", "3", "4"),
      "event" -> ConstantsSet("Abrupt", "Walking", "Running", "Active", "Inactive", "Exit"),
      "fluent" -> ConstantsSet("Fight", "Move", "Meet", "Leaving_object"),
      "dist" -> ConstantsSet("24", "30", "35")
    )

    val decTests = List(
      //DEC v1
      ("Next(t1,t0) ^ Happens(e,t0) ^ Initiates(e,f,t0) => HoldsAt(f,t1).", 1),
      ("Next(t1,t0) ^ Happens(e,t0) ^ Terminates(e,f,t0) => !HoldsAt(f,t1).", 1),
      ("Next(t1,t0) ^ HoldsAt(f,t0) ^ !(Exist e Happens(e,t0) ^ Terminates(e,f,t0) ) => HoldsAt(f,t1).", 64),
      ("Next(t1,t0) ^ !HoldsAt(f,t0) ^ !(Exist e Happens(e,t0) ^ Initiates(e,f,t0) ) => !HoldsAt(f,t1).", 64),
      //DEC v2
      ("Happens(e,t0) ^ Initiates(e,f,t0) => StartedAt(e,f,t0).", 1),
      ("Happens(e,t0) ^ Terminates(e,f,t0) => StoppedAt(e,f,t0).", 1),
      ("Next(t1,t0) ^ StartedAt(e,f,t0) => HoldsAt(f,t1).", 1),
      ("Next(t1,t0) ^ StoppedAt(e,f,t0) => !HoldsAt(f,t1).", 1),
      ("Next(t1,t0) ^ HoldsAt(f,t0) ^ !(Exist e StoppedAt(e,f,t0) ) => HoldsAt(f,t1).", 1),
      ("Next(t1,t0) ^ !HoldsAt(f,t0) ^ !(Exist e StartedAt(e,f,t0) ) => !HoldsAt(f,t1).", 1),
      //DEC v3
      ("Next(t1,t0) ^ InitiatedAt(f,t0) => HoldsAt(f,t1).", 1),
      ("Next(t1,t0) ^ TerminatedAt(f,t0) => !HoldsAt(f,t1).", 1),
      ("Next(t1,t0) ^ HoldsAt(f,t0) ^ !TerminatedAt(f,t0) => HoldsAt(f,t1).", 1),
      ("Next(t1,t0) ^ !HoldsAt(f,t0) ^ !InitiatedAt(f,t0) => !HoldsAt(f,t1).", 1))

    for ((axiom, numClauses) <- decTests) describe("Axiom '" + axiom + "'") {
      val dec = kbParser.parseLogicalSentence(axiom)
      it("should produce " + numClauses + " clause(s)") {
        val clauses = dec.toCNF(constants)
        val fastClauses = NormalForm.toFastCNF(dec)(constants)

        assert(clauses.size == numClauses)
        assert(fastClauses.size == numClauses)
      }
    }

  }

  describe("Formula 'InitiatedAt(f,t) => !(Happens(e, t) ^ Exist x Close(x,t))'") {

    val src = "InitiatedAt(f,t) => !(Happens(e, t) ^ Exist x Close(x,t))"
    val formula = kbParser.parseLogicalSentence(src)

    it("should produce the NNF clause '!InitiatedAt(f,t) v !Happens(e,t) v (Forall x !Close(x,t))'") {
      val nnf = NormalForm.toNNF(formula)
      assert(nnf.toText == "!InitiatedAt(f,t) v !Happens(e,t) v (Forall x !Close(x,t))")
    }
  }

  describe("Formula 'InitiatedAt(f,t) => !(Happens(e, t) ^ Exist x Close(x,t))'") {

    val src = "InitiatedAt(f,t) => !(Happens(e, t) ^ Exist x Close(x,t))"
    val formula = kbParser.parseLogicalSentence(src)

    it("should produce the PNF clause '(Forall x !InitiatedAt(f,t) v !Happens(e,t) v !Close(x,t))'") {
      val pnf = NormalForm.toPNF(formula)
      assert(pnf.toText == "(Forall x !InitiatedAt(f,t) v !Happens(e,t) v !Close(x,t))")
    }
  }

  private def assertContainsClause(clauses: Iterable[Clause], src: String) {
    val txtLiterals = src.replace("{", " ").replace("}", " ").split('|')
    var literals = Set[Literal]()

    for (lit <- txtLiterals) literals += kbParser.parseLiteral(lit)
    assert {
      clauses.find(c => c.literals == literals) match {
        case Some(_) => true
        case _       => false
      }
    }
  }
}
