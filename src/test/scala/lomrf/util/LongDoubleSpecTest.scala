/*
 * o                        o     o   o         o
 * |             o          |     |\ /|         | /
 * |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 * |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 * O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *             |
 *          o--o
 * o--o              o               o--o       o    o
 * |   |             |               |    o     |    |
 * O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 * |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 * o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 * Logical Markov Random Fields.
 *
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */

package lomrf.util

import org.scalatest.{Matchers, FunSpec}

/**
 * Specification test for LongDouble numbers used for very high
 * precision operations.
 */
final class LongDoubleSpecTest extends FunSpec with Matchers {

  import lomrf.util.LongDoubleConversions._

  val fivePointSix = new LongDouble(5.6)
  val onePointSeven = new LongDouble(1.7)
  val zeroPointSeven = new LongDouble(0.7)
  val minusOnePointNine = new LongDouble(-1.9)

  describe("Operators") {

    it("-5.6 should have identical results to 0 - 5.6") {
      assert(-fivePointSix === ZERO - fivePointSix)
    }

    it("-(-1.9) should have identical results to 0 - (-1.9)") {
      assert(-minusOnePointNine === ZERO - minusOnePointNine)
    }

    it("1.7 - 1.7 should be equals to 0") {
      assert(onePointSeven - onePointSeven === ZERO)
    }

    it("5.6 + 1.7 should be equal to 1.7 + 5.6") {
      assert(fivePointSix + onePointSeven === onePointSeven + fivePointSix)
    }

    it("1.7 - 0.7 should be equal to 1") {
      assert(onePointSeven - zeroPointSeven === ONE)
    }

    it("1.7 shoud be equal to (1.7 * 1.7) / 1.7") {
      assert(onePointSeven === (onePointSeven * onePointSeven) / onePointSeven)
    }

    it("5.6 should be greater than 1.7 and obviously 1.7 less than 5.6") {
      assert(fivePointSix > onePointSeven)
      assert(onePointSeven < fivePointSix)
    }

    it("5.6 should be greater or equal to itself and 1.7 should be less or equal to itself") {
      assert(fivePointSix >= fivePointSix && onePointSeven <= onePointSeven)
    }
  }
}
