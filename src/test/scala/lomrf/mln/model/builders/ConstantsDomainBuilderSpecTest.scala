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

package lomrf.mln.model.builders

import org.scalatest.{ FunSpec, Matchers }

/**
  * A series of specification test for constants domain builder.
  *
  * @see [[lomrf.mln.model.builders.ConstantsDomainBuilder]]
  */
final class ConstantsDomainBuilderSpecTest extends FunSpec with Matchers {

  describe("Insertion of constant symbols") {

    describe("Incremental addition of domain 'time' with constants from 1 to 10") {
      val builder = ConstantsDomainBuilder()

      for (timePoint <- 1 to 10)
        builder += ("time", timePoint.toString)

      it("contain 10 constant symbols for domain 'time'") {
        builder("time").size shouldEqual 10
      }

      it("contain 10 constant symbols for domain 'time', after re-adding the same constant symbols") {
        for (timePoint <- 1 to 10)
          builder += ("time", timePoint.toString)
      }

      it("contains all the incrementally inserted constants [1, 10]") {
        val constantsDomain = builder.result()

        assert((1 to 10).forall(t => constantsDomain("time").contains(t.toString)))
      }
    }

    describe("Batch addition of domain 'time' with constants from 1 to 10") {
      val builder = ConstantsDomainBuilder()

      builder ++= ("time", (1 to 10).map(_.toString))

      it("contains 10 constant symbols for domain 'time'") {
        builder("time").size shouldEqual 10
      }

      it("contain 10 constant symbols for domain 'time', after re-adding the same constant symbols") {
        builder ++= ("time", (1 to 10).map(_.toString))
      }

      it("contains all the batched inserted constants [1, 10]") {

        val constantsDomain = builder.result()

        assert((1 to 10).forall(t => constantsDomain("time").contains(t.toString)))
      }
    }

    describe("Incremental addition of domains 'time' and 'person'") {
      val builder = ConstantsDomainBuilder()

      for (timePoint <- 1 to 10)
        builder += ("time", timePoint.toString)

      for (person <- List("Agamemnon", "Odysseus", "Achilles", "Menelaus"))
        builder += ("person", person)

      it("contain 10 constant symbols for domain 'time'") {
        builder("time").size shouldEqual 10
      }

      it("contains 4 constant symbols for domain 'person'") {
        builder("person").size shouldEqual 4
      }

      it("contain 10 constant symbols for domain 'time', after re-adding the same constant symbols") {
        for (timePoint <- 1 to 10)
          builder += ("time", timePoint.toString)
      }

      it("contain 4 constant symbols for domain 'person', after re-adding the same constant symbols") {
        builder ++= ("person", List("Agamemnon", "Odysseus", "Achilles", "Menelaus"))
      }

      it("contains all the incrementally inserted constants for both 'time' and 'person' domains") {
        val constantsDomain = builder.result()

        assert((1 to 10).forall(t => constantsDomain("time").contains(t.toString)))
        assert(List("Agamemnon", "Odysseus", "Achilles", "Menelaus").forall(p => constantsDomain("person").contains(p)))
      }
    }

    describe("Batch addition of domains 'time' and 'person'") {
      val builder = ConstantsDomainBuilder()

      builder ++= ("time", (1 to 10).map(_.toString))
      builder ++= ("person", List("Agamemnon", "Odysseus", "Achilles", "Menelaus"))

      it("contains 10 constant symbols for domain 'time'") {
        builder("time").size shouldEqual 10
      }

      it("contains 4 constant symbols for domain 'person'") {
        builder("person").size shouldEqual 4
      }

      it("contain 10 constant symbols for domain 'time', after re-adding the same constant symbols") {
        builder ++= ("time", (1 to 10).map(_.toString))
      }

      it("contain 4 constant symbols for domain 'person', after re-adding the same constant symbols") {
        builder ++= ("person", List("Agamemnon", "Odysseus", "Achilles", "Menelaus"))
      }

      it("contains all the batched inserted constants for both 'time' and 'person' domains") {

        val constantsDomain = builder.result()

        assert((1 to 10).forall(t => constantsDomain("time").contains(t.toString)))
        assert(List("Agamemnon", "Odysseus", "Achilles", "Menelaus").forall(p => constantsDomain("person").contains(p)))
      }
    }

    describe("Creation two constant domains (Domain1 and Domain2), using the same constants domain builder") {
      val builder = ConstantsDomainBuilder()

      // Create the first KB, having time domain with symbols 1 to 10
      builder ++= ("time", (1 to 10).map(_.toString))
      val domain1 = builder.result()
      val domain1TimeSize = domain1("time").size

      // Thereafter, create the second KB by adding the new constant symbols (100 to 1000)
      builder ++= ("time", (100 to 1000).map(_.toString))
      val domain2 = builder.result()
      domain1("time").foreach(println)
      val domain2TimeSize = domain2("time").size

      // As a result, the KB1 should contain only the first batch of symbols (total 10)
      val domain1TimeTotal = 10
      // While the KB2, should contain the same and the additional symbols (total 10 + 901)
      val domain2TimeTotal = 911

      it(s"Domain1 contains $domain1TimeTotal symbols in domain 'time'") {
        domain1TimeSize shouldEqual domain1TimeTotal
      }

      it(s"Domain2 contains $domain2TimeTotal symbols in domain 'time'") {
        domain2TimeSize shouldEqual domain2TimeTotal
      }

      it(s"Domain2 contains all Domain1 symbols in domain 'time'") {
        assert(domain1("time").forall(domain2("time").contains))
      }
      it(s"Domain1 does not contain all Domain2 symbols in domain 'time'") {
        assert(!domain2("time").forall(domain1("time").contains))
      }
    }

  }
}
