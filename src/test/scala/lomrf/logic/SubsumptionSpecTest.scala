package lomrf.logic

import lomrf.logic.LogicOps._
import org.scalatest.{Matchers, FunSpec}

/**
 * Specification test for subsumption operator over clauses.
 */
final class SubsumptionSpecTest extends FunSpec with Matchers {

  // ------------------------------------------------------------------------------------------------------------------
  // --- 1st Example:
  // ---------------- Father(x,John) v Male(x) v Male(John) v Parent(x,John)
  // ---------------- Father(y,z) v Male(y) v Parent(y,z)
  // ------------------------------------------------------------------------------------------------------------------

  val father1 = AtomicFormula("Father", Vector(Variable("x"), Constant("John")))

  val father2 = AtomicFormula("Father", Vector(Variable("y"), Variable("z")))

  val male1 = AtomicFormula("Male", Vector(Variable("x")))

  val male2 = AtomicFormula("Male", Vector(Variable("y")))

  val male3 = AtomicFormula("Male", Vector(Constant("John")))

  val parent1 = AtomicFormula("Parent", Vector(Variable("x"), Constant("John")))

  val parent2 = AtomicFormula("Parent", Vector(Variable("y"), Variable("z")))

  val c1 = Clause(Set(PositiveLiteral(father1), PositiveLiteral(male1), PositiveLiteral(male3), PositiveLiteral(parent1)))

  val c2 = Clause(Set(PositiveLiteral(father2), PositiveLiteral(male2), PositiveLiteral(parent2)))

  describe(s"Checking ${c1.toText()} and ${c2.toText()}") {

    it(s"${c2.toText()} should subsume ${c1.toText()}") {
      c2 subsumes c1 shouldBe true
    }

    it(s"${c1.toText()} should NOT subsume ${c2.toText()}") {
      c1 subsumes c2 shouldBe false
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- 2nd Example:
  // ---------------- Nat(x) v Nat(s(x))
  // ---------------- Nat(s(s(0))) v Nat(s(0))
  // ------------------------------------------------------------------------------------------------------------------

  val nat1 = AtomicFormula("Nat", Vector(Variable("x")))

  val nat2 = AtomicFormula("Nat", Vector(TermFunction("s", Vector(Variable("x")))))

  val nat3 = AtomicFormula("Nat", Vector(TermFunction("s", Vector(TermFunction("s", Vector(Constant("0")))))))

  val nat4 = AtomicFormula("Nat", Vector(TermFunction("s", Vector(Constant("0")))))

  val c3 = Clause(Set(PositiveLiteral(nat1), PositiveLiteral(nat2)))

  val c4 = Clause(Set(PositiveLiteral(nat3), PositiveLiteral(nat4)))

  describe(s"Checking ${c3.toText()} and ${c4.toText()}") {

    it(s"${c3.toText()} should subsume ${c4.toText()}") {
      c3 subsumes c4 shouldBe true
    }

    it(s"${c4.toText()} should NOT subsume ${c3.toText()}") {
      c4 subsumes c3 shouldBe false
    }
  }
  // ------------------------------------------------------------------------------------------------------------------
  // --- 3rd Example:
  // ---------------- P(x,y,z) v Q(y)
  // ---------------- P(u,u,u) v Q(u) v R(a)
  // ------------------------------------------------------------------------------------------------------------------

  val p1 = AtomicFormula("P", Vector(Variable("x"), Variable("y"), Variable("x")))

  val p2 = AtomicFormula("P", Vector(Variable("u"), Variable("u"), Variable("u")))

  val q1 = AtomicFormula("Q", Vector(Variable("y")))

  val q2 = AtomicFormula("Q", Vector(Variable("u")))

  val r = AtomicFormula("R", Vector(Constant("a")))

  val c5 = Clause(Set(PositiveLiteral(p1), PositiveLiteral(q1)))

  val c6 = Clause(Set(PositiveLiteral(p2), PositiveLiteral(q2), PositiveLiteral(r)))

  describe(s"Checking ${c5.toText()} and ${c6.toText()}") {

    it(s"${c5.toText()} should subsume ${c6.toText()}") {
      c5 subsumes c6 shouldBe true
    }

    it(s"${c6.toText()} should NOT subsume ${c5.toText()}") {
      c6 subsumes c5 shouldBe false
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- 4th Example:
  // ---------------- O(x) v L(x,y) v L(y,x)
  // ---------------- O(u) v L(u,u)
  // ------------------------------------------------------------------------------------------------------------------

  val o1 = AtomicFormula("O", Vector(Variable("x")))

  val o2 = AtomicFormula("O", Vector(Variable("k")))

  val l1 = AtomicFormula("L", Vector(Variable("x"), Variable("y")))

  val l2 = AtomicFormula("L", Vector(Variable("y"), Variable("x")))

  val l3 = AtomicFormula("L", Vector(Variable("k"), Variable("k")))

  val c7 = Clause(Set(PositiveLiteral(o1), PositiveLiteral(l1), PositiveLiteral(l2)))

  val c8 = Clause(Set(PositiveLiteral(o2), PositiveLiteral(l3)))

  describe(s"Checking ${c7.toText()} and ${c8.toText()}") {

    it(s"${c7.toText()} should subsume ${c8.toText()}") {
      c7 subsumes c8 shouldBe true
    }

    it(s"${c8.toText()} should NOT subsume ${c7.toText()}") {
      c8 subsumes c7 shouldBe false
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- 5th Example:
  // ---------------- O(x) v L(x,y)
  // ---------------- O(x) v L(y,x)
  // ------------------------------------------------------------------------------------------------------------------

  val c9 = Clause(Set(PositiveLiteral(o1), PositiveLiteral(l1)))

  val c10 = Clause(Set(PositiveLiteral(o1), PositiveLiteral(l2)))

  describe(s"Checking ${c9.toText()} and ${c10.toText()}") {

    it(s"${c9.toText()} should NOT subsume ${c10.toText()}") {
      c9 subsumes c10 shouldBe false
    }

    it(s"${c10.toText()} should NOT subsume ${c9.toText()}") {
      c10 subsumes c9 shouldBe false
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- 6th Example:
  // ---------------- H(x0) v B1(x0,x1) v B1(x0,x2) v B1(x0,x3) v B2(x1,x2) v B2(x1,x3)
  // ---------------- H(c0) v B1(c0,c1) v B1(c0,c2) v B2(c1,c2)
  // ------------------------------------------------------------------------------------------------------------------

  val h1 = AtomicFormula("H", Vector(Variable("x0")))

  val b1 = AtomicFormula("B1", Vector(Variable("x0"), Variable("x1")))

  val b2 = AtomicFormula("B1", Vector(Variable("x0"), Variable("x2")))

  val b3 = AtomicFormula("B1", Vector(Variable("x0"), Variable("x3")))

  val b4 = AtomicFormula("B2", Vector(Variable("x1"), Variable("x2")))

  val b5 = AtomicFormula("B2", Vector(Variable("x1"), Variable("x3")))

  val c11 = Clause(Set(PositiveLiteral(h1), PositiveLiteral(b1), PositiveLiteral(b2), PositiveLiteral(b3), PositiveLiteral(b4), PositiveLiteral(b5)))

  val h2 = AtomicFormula("H", Vector(Variable("c0")))

  val b6 = AtomicFormula("B1", Vector(Variable("c0"), Variable("c1")))

  val b7 = AtomicFormula("B1", Vector(Variable("c0"), Variable("c2")))

  val b8 = AtomicFormula("B2", Vector(Variable("c1"), Variable("c2")))

  val c12 = Clause(Set(PositiveLiteral(h2), PositiveLiteral(b6), PositiveLiteral(b7), PositiveLiteral(b8)))

  describe(s"Checking ${c11.toText()} and ${c12.toText()}") {

    it(s"${c11.toText()} should subsume ${c12.toText()}") {
      c11 subsumes c12 shouldBe true
    }

    it(s"${c12.toText()} should subsume ${c11.toText()}") {
      c12 subsumes c11 shouldBe true
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- 7th Example:
  // ---------------- H(x) v K(x,y,z,t)
  // ---------------- H(y) v K(y,x,t,z)
  // ------------------------------------------------------------------------------------------------------------------

  val h3 = AtomicFormula("H", Vector(Variable("x")))

  val h4 = AtomicFormula("H", Vector(Variable("y")))

  val k1 = AtomicFormula("K", Vector(Variable("x"), Variable("y"), Variable("z"), Variable("t")))

  val k2 = AtomicFormula("K", Vector(Variable("y"), Variable("x"), Variable("t"), Variable("z")))

  val c13 = Clause(Set(PositiveLiteral(h3), PositiveLiteral(k1)))

  val c14 = Clause(Set(PositiveLiteral(h4), PositiveLiteral(k2)))

  describe(s"Checking ${c13.toText()} and ${c14.toText()}") {

    it(s"${c13.toText()} should subsume ${c14.toText()}") {
      c13 subsumes c14 shouldBe true
    }

    it(s"${c14.toText()} should subsume ${c13.toText()}") {
      c14 subsumes c13 shouldBe true
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- 8th Example:
  // ---------------- H(x) v K(x,y,z,t)
  // ---------------- H(x) v K(y,x,t,z)
  // ------------------------------------------------------------------------------------------------------------------

  val c15 = Clause(Set(PositiveLiteral(h3), PositiveLiteral(k1)))

  val c16 = Clause(Set(PositiveLiteral(h3), PositiveLiteral(k2)))

  describe(s"Checking ${c15.toText()} and ${c16.toText()}") {

    it(s"${c15.toText()} should NOT subsume ${c16.toText()}") {
      c15 subsumes c16 shouldBe false
    }

    it(s"${c16.toText()} should NOT subsume ${c15.toText()}") {
      c16 subsumes c15 shouldBe false
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- 8th Example:
  // ---------------- !N(t0,t1) v !H(walking(id1),t0) v !H(active(id2),t0) v Hat(meet(id1,id2),t1)
  // ---------------- !N(t0,t1) v !H(walking(id2),t0) v !H(active(id1),t0) v !H(inactive(id2),t0) v Hat(meet(id2,id1),t1)
  // ------------------------------------------------------------------------------------------------------------------

  val next = AtomicFormula("N", Vector(Variable("t0"), Variable("t1")))

  val holds1 = AtomicFormula("Hat", Vector(TermFunction("meet", Vector(Variable("id1"), Variable("id2"))), Variable("t1")))

  val holds2 = AtomicFormula("Hat", Vector(TermFunction("meet", Vector(Variable("id2"), Variable("id1"))), Variable("t1")))

  val hw1 = AtomicFormula("H", Vector(TermFunction("walking", Vector(Variable("id1"))), Variable("t0")))

  val hw2 = AtomicFormula("H", Vector(TermFunction("walking", Vector(Variable("id2"))), Variable("t0")))

  val ha1 = AtomicFormula("H", Vector(TermFunction("active", Vector(Variable("id1"))), Variable("t0")))

  val ha2 = AtomicFormula("H", Vector(TermFunction("active", Vector(Variable("id2"))), Variable("t0")))

  val hi2 = AtomicFormula("H", Vector(TermFunction("inactive", Vector(Variable("id2"))), Variable("t0")))

  val c17 = Clause(Set(NegativeLiteral(next), NegativeLiteral(hw1), NegativeLiteral(ha2), PositiveLiteral(holds1)))

  val c18 = Clause(Set(NegativeLiteral(next), NegativeLiteral(hw2), NegativeLiteral(ha1), NegativeLiteral(hi2), PositiveLiteral(holds2)))

  describe(s"Checking ${c17.toText()} and ${c18.toText()}") {

    it(s"${c17.toText()} should subsume ${c18.toText()}") {
      c17 subsumes c18 shouldBe true
    }

    it(s"${c18.toText()} should NOT subsume ${c17.toText()}") {
      c18 subsumes c17 shouldBe false
    }
  }
}
