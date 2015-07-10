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

package lomrf.util.evaluation

import org.scalatest.{Matchers, FunSpec}

/**
 * Specification test for various metrics used for automated evaluation over
 * learning results.
 */
final class MetricsSpecTest extends FunSpec with Matchers {

  // Precision is the fraction of retrieved instances that are relevant
  describe("Precision") {

    it("when we only have true positives precision should be 1.0") {
      Metrics.precision(100, 0, 0) shouldBe 1.0
    }

    it("when true positives are equal to false positives precision should be 0.5") {
      Metrics.precision(55, 55, 0) shouldBe 0.5
      Metrics.precision(100, 100, 5) shouldBe 0.5
    }

    it("when both recognised positives and annotation positives are zero precision should be 1.0") {
      Metrics.precision(0, 0, 0) shouldBe 1.0
    }

    it("when recognised positives are zero but annotation positives are greater than zero precision should be 0.0") {
      Metrics.precision(0, 0, 1) shouldBe 0.0
    }
  }

  // Recall is the fraction of relevant instances that are retrieved
  describe("Recall") {

    it("when we only have true positives recall should be 1.0") {
      Metrics.recall(100, 0, 0) shouldBe 1.0
    }

    it("when true positives are equal to false negatives recall should be 0.5") {
      Metrics.recall(55, 0, 55) shouldBe 0.5
      Metrics.recall(100, 5, 100) shouldBe 0.5
    }

    it("when both recognised positives and annotation positives are zero recall should be 1.0") {
      Metrics.recall(0, 0, 0) shouldBe 1.0
    }

    it("when recognised positives are greater than zero but annotation positives are zero recall should be 0.0") {
      Metrics.recall(0, 1, 0) shouldBe 0.0
    }
  }

  // Accuracy is the proportion of true results (true positives and true negatives) among the total number of cases examined
  describe("Accuracy") {

    it("when true positives and true negatives are zero accuracy should be 0.0") {
      Metrics.accuracy(0, 10, 0 ,10) shouldBe 0.0
    }

    it("when false positives and false negatives are zero accuracy should be 1.0") {
      Metrics.accuracy(5, 0, 5 ,0) shouldBe 1.0
    }

    it("when true positives and negatives are equal to false positives and negatives accuracy should be 0.5") {
      Metrics.accuracy(5, 5, 5 ,5) shouldBe 0.5
    }
  }

  // False positive ratio refers to the probability of falsely rejecting the null hypothesis
  describe("False Positive Rate") {

    it("when false positives are zero FPT should be 0.0") {
      Metrics.fpr(0, 1) shouldBe 0.0
    }

    it("when false positives are equal to true negatives FPT should be 0.5") {
      Metrics.fpr(5, 5) shouldBe 0.5
    }

    it("when true negatives are zero FPT should be 1.0") {
      Metrics.fpr(10, 0) shouldBe 1.0
    }
  }

  // F measure is a measure of a test accuracy
  describe("Fmeasure") {

    it("when both recognised positives and annotation positives are zero Fmeasure should be 1.0") {
      Metrics.f1(0, 0, 0) shouldBe 1.0
    }

    it("when recognised positives are zero but annotation positives are greater than zero Fmeasure should be 0.0") {
      Metrics.f1(0, 0, 1) shouldBe 0.0
    }

    it("when recognised positives are greater than zero but annotation positives are zero Fmeasure should be 0.0") {
      Metrics.f1(0, 1, 0) shouldBe 0.0
    }

    it("when recognised and annotation positives are greater than zero but true positives are zero Fmeasure should be 0.0") {
      Metrics.f1(0, 5, 5) shouldBe 0.0
    }

  }

}
