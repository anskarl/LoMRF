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
 */

package lomrf.mln.learning.supervision.graphs

import breeze.linalg.DenseVector
import breeze.stats.distributions.Uniform
import org.scalatest.{FunSpec, Matchers}

final class GraphConnectorSpecTest extends FunSpec with Matchers {

  describe("Fully connected graphs.") {

    val connector = FullConnector
    val neighbors = DenseVector.rand(10, Uniform(0, 1))

    it("A fully connected neighborhood should always be identical to the original.") {
      neighbors shouldEqual connector(neighbors)
    }
  }

  describe("1NN connected graphs.") {

    val connector = kNNConnector(1)

    val zeros = DenseVector.zeros[Double](10)
    val ones = DenseVector.ones[Double](10)

    it("Uniform neighborhood should remain unchanged.") {
      zeros shouldEqual connector(zeros)
      ones shouldEqual connector(ones)
    }

    val neighbors = DenseVector.rangeD(1, 6)

    it("Only one neighbor should remain connected.") {
      DenseVector.vertcat(
        DenseVector.fill(4, UNCONNECTED),
        DenseVector[Double](5)
      ) shouldEqual connector(neighbors)
    }

    val neighborsDuplicates = DenseVector.vertcat(DenseVector.rangeD(1, 6), DenseVector.fill[Double](4, 5))

    it("Five neighbors having the highest cost should remain connected.") {
      DenseVector.vertcat(
        DenseVector.fill(4, UNCONNECTED),
        DenseVector.fill[Double](5, 5)
      ) shouldEqual connector(neighborsDuplicates)
    }
  }

  describe("2NN connected graphs.") {

    val connector = kNNConnector(2)

    val zeros = DenseVector.zeros[Double](10)
    val ones = DenseVector.ones[Double](10)

    it("Uniform neighborhood should remain unchanged.") {
      zeros shouldEqual connector(zeros)
      ones shouldEqual connector(ones)
    }

    val neighbors = DenseVector.rangeD(1, 6)

    it("Only two neighbors should remain connected.") {
      DenseVector.vertcat(
        DenseVector.fill(3, UNCONNECTED),
        DenseVector[Double](4, 5)
      ) shouldEqual connector(neighbors)
    }

    val neighborsDuplicates = DenseVector.vertcat(DenseVector.rangeD(1, 6), DenseVector.fill(4, 5D))

    it("Six neighbors having the highest costs should remain connected.") {
      DenseVector.vertcat(
        DenseVector.fill(3, UNCONNECTED),
        DenseVector[Double](4, 5, 5, 5, 5, 5)
      ) shouldEqual connector(neighborsDuplicates)
    }

  }

  describe("0.2NN connected graphs.") {

    val connector = eNNConnector(0.2)

    val zeros = DenseVector.zeros[Double](10)

    it("Zero neighborhood should always be unconnected.") {
      DenseVector.fill(10, UNCONNECTED) shouldEqual connector(zeros)
    }

    val ones = DenseVector.ones[Double](10)

    it("One neighborhood should always be fully connected.") {
      ones shouldEqual connector(ones)
    }

    val neighbors = DenseVector.rangeD(0, 1.1, 0.1).map { n =>
      math.round(n * 100D) / 100D
    }

    it("Only two neighbors should be unconnected.") {
      DenseVector.vertcat(
        DenseVector.fill(2, UNCONNECTED),
        DenseVector.rangeD(0.2, 1.1, 0.1).map(n => math.round(n * 100D) / 100D)
      ) shouldEqual connector(neighbors)
    }
  }

  describe("0.9NN connected graphs.") {

    val connector = eNNConnector(0.9)

    val zeros = DenseVector.zeros[Double](10)

    it("Zero neighborhood should always be unconnected.") {
      DenseVector.fill(10, UNCONNECTED) shouldEqual connector(zeros)
    }

    val ones = DenseVector.ones[Double](10)

    it("One neighborhood should always be fully connected.") {
      ones shouldEqual connector(ones)
    }

    val neighbors = DenseVector.rangeD(0, 1.1, 0.1).map { n =>
      math.round(n * 100D) / 100D
    }

    it("Nine neighbors should be unconnected.") {
      DenseVector.vertcat(
        DenseVector.fill(9, UNCONNECTED),
        DenseVector.rangeD(0.9, 1.1, 0.1).map(n => math.round(n * 100D) / 100D)
      ) shouldEqual connector(neighbors)
    }
  }
}

