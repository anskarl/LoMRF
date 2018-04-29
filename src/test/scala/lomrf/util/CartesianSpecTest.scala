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

package lomrf.util

import lomrf.util.Cartesian.CartesianIteratorArithmeticImpl
import org.scalatest.{ Matchers, FunSpec }

/**
  *
  */
class CartesianSpecTest extends FunSpec with Matchers {

  // Note: the given domains should always be above zero
  private val domainList = List(
    Array(10, 5, 2),
    Array(10, 1, 2),
    Array(1, 1, 10),
    Array(1, 10),
    Array(5, 10),
    Array(10),
    Array(1))

  require(domainList.forall(_.forall(_ > 0)))

  for (domain <- domainList; (l, iteration) <- domain.permutations.zipWithIndex) {
    val elements = l.map(_ - 1)
    val expectedIterations = l.product // this is the correct number of products

    describe("Cartesian product of domains [" + elements.map(_.toString).reduceLeft(_ + ", " + _) + "]") {

      val iterator = new CartesianIteratorArithmeticImpl(elements)
      val result = iterator.map(_.toString).toSet

      info("iteration: " + iteration + "\n" +
        "\telements = [" + elements.map(_.toString).reduceLeft(_ + ", " + _) + "]\n" +
        "\texpected = " + expectedIterations + "\n" +
        "\tproduced = " + result.size)

      it("produces " + expectedIterations + " distinct Cartesian products") {
        assert(expectedIterations == result.size)
      }
    }
  }

}
