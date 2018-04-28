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

import org.scalatest.{FunSpec, Matchers}
import lomrf.logic.AtomSignatureOps._
import scala.util.Success

/**
  * A series of specification test for atom signatures.
  *
  * @see [[lomrf.logic.AtomSignature]]
  */
final class AtomSignatureSpecTest extends FunSpec with Matchers {

  describe("Atom Signature of predicates and functions") {

    it ("P/1 predicate should have a valid atom signature") {
      val atomP = AtomSignature("P", 1)

      atomP shouldEqual
        AtomSignature(AtomicFormula(
          "P", Vector(Variable("x"))
        ))

      Success(atomP) shouldEqual
        AtomSignature.parseString("P/1")

      "P/1".signature shouldEqual
        AtomSignature.parseString("P/1")
    }

    it ("Q/4 predicate should have a valid atom signature") {
      val atomQ = AtomSignature("Q", 4)

      atomQ shouldEqual
        AtomSignature(AtomicFormula(
          "Q", (1 to 4).map(i => Variable(s"x$i")).toVector
        ))

      Success(atomQ) shouldEqual
        AtomSignature.parseString("Q/4")

      "Q/4".signature shouldEqual
        AtomSignature.parseString("Q/4")
    }

    it ("R/100 predicate should have a valid atom signature") {
      val atomR = AtomSignature("R", 100)

      atomR shouldEqual
        AtomSignature(AtomicFormula(
          "R", (1 to 100).map(i => Variable(s"x$i")).toVector
        ))

      Success(atomR) shouldEqual
        AtomSignature.parseString("R/100")

      "R/100".signature shouldEqual
        AtomSignature.parseString("R/100")
    }

    it ("foo/2 function should have a valid atom signature") {
      val atomFoo = AtomSignature("foo", 2)

      atomFoo shouldEqual
        AtomSignature(TermFunction(
          "foo", (1 to 2).map(i => Variable(s"x$i")).toVector
        ))

      Success(atomFoo) shouldEqual
        AtomSignature.parseString("foo/2")

      "foo/2".signature shouldEqual
        AtomSignature.parseString("foo/2")
    }

    it ("bar/65 function should have a valid atom signature") {
      val atomBar = AtomSignature("bar", 65)

      atomBar shouldEqual
        AtomSignature(TermFunction(
          "bar", (1 to 65).map(i => Variable(s"x$i")).toVector
        ))

      Success(atomBar) shouldEqual
        AtomSignature.parseString("bar/65")

      "bar/65".signature shouldEqual
        AtomSignature.parseString("bar/65")
    }
  }
}
