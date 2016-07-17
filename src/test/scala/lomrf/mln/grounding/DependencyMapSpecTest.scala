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

package lomrf.mln.grounding

import lomrf.logic._
import lomrf.mln.model.{AtomIdentityFunctionOps, MLN}
import lomrf.mln.model.mrf.MRF
import AtomIdentityFunctionOps._
import org.scalatest.{Matchers, FunSpec}
import lomrf.tests.TestData
import lomrf.util.io._

/**
 * Specification test for dependency map produced by grounding procedure. It is used by learning algorithms in order to
 * reconstruct the ground network without rerunning the grounding procedure in each iteration.
 */
class DependencyMapSpecTest extends FunSpec with Matchers {

  private implicit def str2AtomSignature(txt: String): AtomSignature = {
    val elements = txt.split("/")
    assert(elements.length == 2)
    AtomSignature(elements(0), elements(1).toInt)
  }

  private val sep = System.getProperty("file.separator")
  private val prefix = TestData.TestFilesPath / "DependencyMap"  //System.getProperty("user.dir") + sep + "Examples" + sep + "data" + sep + "tests" + sep + "DependencyMap" + sep

  private val mlnFile = prefix / "DependencyMap.mln"
  private val evidenceFile = prefix / "Empty.db"


  implicit val mln = MLN.fromFile(mlnFile, queryAtoms = Set("S/1", "C/1", "K/1", "M/1", "F/2"), evidenceFile)

  describe(s"The MLN theory in '$mlnFile'"){

    /*it("should contain 7 formulas") {
      mln.formulas.size shouldBe 7
    }*/

    it("should contain 1 constant set (domain)") {
      mln.evidence.constants.size shouldBe 1
    }

    it("should contain 5 predicate schemas") {
      mln.schema.predicates.size shouldBe 5
    }

    it("should not contain any function schemas") {
      mln.schema.functions.size shouldBe 0
    }
  }


  describe(s"The produced MRF when negative weights are allowed") {
    checkScenario(noNegWeights = true, expectedNumberOfAtoms = 12, expectedNumberOfConstraints = 19 )
  }

  describe(s"The produced MRF when negative weights are not allowed") {
    checkScenario(noNegWeights = false, expectedNumberOfAtoms = 12, expectedNumberOfConstraints = 17 )
  }


  private def checkScenario(noNegWeights: Boolean, expectedNumberOfAtoms: Int, expectedNumberOfConstraints: Int ): Unit ={

    val mrf = MRF.build(mln, noNegWeights, createDependencyMap = true)

    it(s"should contain $expectedNumberOfAtoms ground atoms") {
      mrf.numberOfAtoms shouldBe expectedNumberOfAtoms
    }

    it(s"should contain $expectedNumberOfConstraints ground clauses") {
      mrf.numberOfConstraints shouldBe expectedNumberOfConstraints
    }

    val dependencyMap = mrf.dependencyMap.getOrElse(sys.error("Dependency map does not exists."))

    describe("The produced dependency map"){
      val dmIterator = dependencyMap.iterator()

      while (dmIterator.hasNext) {
        dmIterator.advance()
        val constraintID = dmIterator.key()
        val statsIterator = dmIterator.value.iterator()

        val constraintWeight = mrf.constraints.get(constraintID).getWeight
        var clauseWeight = 0.0
        var total = 0.0

        while (statsIterator.hasNext) {
          statsIterator.advance()
          val cid = statsIterator.key()
          val freq = statsIterator.value()
          clauseWeight = if (mln.clauses(cid).isHard) mrf.weightHard else mln.clauses(cid).weight
          total += clauseWeight * freq
        }

        val constraint = mrf.constraints.get(constraintID)
        it(s"has the constraint $constraintID -> ${constraint.decodeFeature()(mln).getOrElse("Failed to decode constraint")}, which can be reconstructed") {
          constraintWeight shouldBe total
        }
      }
    }

  }
}

