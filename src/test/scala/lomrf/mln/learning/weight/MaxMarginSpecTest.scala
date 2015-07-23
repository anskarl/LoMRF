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

package lomrf.mln.learning.weight

import org.scalatest.{PrivateMethodTester, Matchers, FunSpec}
import lomrf.logic.AtomSignature
import lomrf.mln.model.{AtomIdentityFunctionOps, MLN}
import lomrf.mln.model.mrf.MRF
import AtomIdentityFunctionOps._

/**
 * Specification test for Max-Margin learner
 */
final class MaxMarginSpecTest extends FunSpec with Matchers with PrivateMethodTester {

  private val sep = System.getProperty("file.separator")
  private val prefix = System.getProperty("user.dir") + sep + "Examples" + sep + "data" + sep + "tests" + sep + "learning" + sep
  private val mlnFile = prefix + "smoking.mln"
  private val trainFile = prefix + "train.db"

  private val nonEvidenceAtoms = Set(AtomSignature("Smokes", 1), AtomSignature("Cancer", 1), AtomSignature("TransmitCancer", 2))

  val (mln, annotationDB) = MLN.forLearning(mlnFile, List(trainFile), nonEvidenceAtoms)

  describe(s"The MLN theory in '$mlnFile'"){

    /*it("should contain 3 formulas") {
      mln.formulas.size shouldBe 3
    }*/

    it("should contain 1 constant set (domain)") {
      mln.evidence.constants.size shouldBe 1
    }

    it("should contain 4 predicate schemas") {
      mln.schema.predicates.size shouldBe 4
    }

    it("should not contain any function schemas") {
      mln.schema.functions.size shouldBe 0
    }
  }

  val mrf = MRF.build(mln, createDependencyMap = true)

  it(s"should contain 80 ground atoms") {
    mrf.numberOfAtoms shouldBe 80
  }

  it(s"should contain 40 ground clauses") {
    mrf.numberOfConstraints shouldBe 40
  }

  val learner = new MaxMarginLearner(mrf = mrf, annotationDB = annotationDB, nonEvidenceAtoms = nonEvidenceAtoms, lossAugmented = true)

  val countGroundings = PrivateMethod('countGroundings)
  val updateConstraintWeights = PrivateMethod('updateConstraintWeights)
  val calculateError = PrivateMethod('calculateError)

  describe("Checking count true groundings functionality") {
    val trueCounts:Array[Int] = learner invokePrivate countGroundings()

    val clause0 = mln.clauses(0).toText()
    it(s"clause '$clause0' should have 6 true counts"){
      trueCounts(0) shouldBe 6
    }

    val clause1 = mln.clauses(1).toText()
    it(s"clause '$clause1' should have 16 true counts"){
      trueCounts(1) shouldBe 16
    }

    val clause2 = mln.clauses(2).toText()
    it(s"clause '$clause2' should have 15 true counts"){
      trueCounts(2) shouldBe 15
    }

    val clause3 = mln.clauses(3).toText()
    it(s"clause '$clause3' should have 15 true counts"){
      trueCounts(3) shouldBe 15
    }
  }

  describe("Checking update constraint weights functionality") {

    val dependencyMap = mrf.dependencyMap.getOrElse(sys.error("Dependency map does not exist!"))

    // Set manually dummy weights to the learner
    for (i <- learner.weights.indices) learner.weights(i) = (i + 1) * 2
    info("Weights: [" + learner.weights.deep.mkString(", ") + "]")

    // Update ground constraints using the weights
    learner invokePrivate updateConstraintWeights()

    val constraintsIterator = mrf.constraints.iterator()
    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()

      val string = constraint.literals.map {
        l => l.decodeLiteral(mrf.mln).getOrElse(sys.error("Cannot decode literal: " + l))
      }.mkString(" v ")

      if (constraint.isHardConstraint) {
        it(s"constraint { $string } should be having a hard weight") {
          constraint.getWeight shouldBe mrf.weightHard
        }
      }
      else {
        val iterator = dependencyMap.get(constraint.id).iterator()
        var result = 0.0
        while (iterator.hasNext) {
          iterator.advance()
          result += learner.weights(iterator.key()) * iterator.value()
        }
        it(s"constraint { $string } should be having weight equal to $result") {
          constraint.getWeight shouldBe result
        }
      }
    }
  }

  // Because initial we have an annotated state loss should be zero
  it("Calculated loss should be zero") {
    val loss: Double = learner invokePrivate calculateError()
    loss shouldBe 0.0
  }
}
