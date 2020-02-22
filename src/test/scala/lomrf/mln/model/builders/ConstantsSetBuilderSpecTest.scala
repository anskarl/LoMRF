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

import scala.util.Random

/**
  * A series of specification test for constants set builder.
  *
  * @see [[lomrf.mln.model.builders.ConstantsSetBuilder]]
  */
final class ConstantsSetBuilderSpecTest extends FunSpec with Matchers {

  describe("An empty builder") {
    val builder = ConstantsSetBuilder()

    it ("should be empty") {
      builder.isEmpty shouldEqual true
    }

    it ("should have zero size") {
      builder.size shouldEqual 0
    }

    it("should have an empty iterator") {
      builder.iterator.isEmpty shouldEqual true
    }

    it("should create copies of empty builders") {
      builder.copy().isEmpty shouldEqual true
    }

    it ("should result in an empty constants set") {
      builder.result().isEmpty shouldBe true
    }
  }

  describe("A builder holding a single constant symbol") {
    val builder = ConstantsSetBuilder("Symbol")

    it ("should NOT be empty") {
      builder.nonEmpty shouldEqual true
    }

    it ("should have size 1") {
      builder.size shouldEqual 1
    }

    it("should have a NON empty iterator") {
      builder.iterator.nonEmpty shouldEqual true
    }

    it("should create copies of NON empty builders") {
      builder.copy().nonEmpty shouldEqual true
    }

    it ("should result in a constants set holding a single constant") {
      val constantsSet = builder.result()
      constantsSet.nonEmpty shouldEqual true
      constantsSet.contains("Symbol") shouldEqual true
    }
  }

  describe("A builder holding a more constant symbols") {

    val constants =
      for (idx <- 1 to 10) yield s"C${idx}" + Random.alphanumeric.take(5).toString()

    val builder = ConstantsSetBuilder(constants)

    it ("should NOT be empty") {
      builder.nonEmpty shouldEqual true
    }

    it (s"should have size ${constants.size}") {
      builder.size shouldEqual constants.size
    }

    it("should have a NON empty iterator") {
      builder.iterator.nonEmpty shouldEqual true
    }

    it("should create copies of NON empty builders") {
      builder.copy().nonEmpty shouldEqual true
    }

    it ("should result in a constants set holding all given constants") {
      val constantsSet = builder.result()
      constantsSet.nonEmpty shouldEqual true
      constants.forall(constantsSet.contains)
    }

    it ("should be empty after clear function is called") {
      builder.clear()
      builder.isEmpty shouldEqual true
    }
  }
}
