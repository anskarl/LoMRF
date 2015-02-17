package lomrf.mln.grounding

import lomrf.logic._
import lomrf.mln.model.MLN
import lomrf.mln.model.mrf.MRF
import lomrf.util.decodeFeature
import org.scalatest.{Matchers, FunSpec}

/**
 * Specification test for dependency map produced by grounding
 * procedure. It is used by learning algorithms in order to
 * reconstruct the ground network without rerunning the grounding
 * procedure in each iteration.
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
class DependencyMapSpecTest extends FunSpec with Matchers {

  private implicit def str2AtomSignature(txt: String): AtomSignature = {
    val elements = txt.split("/")
    assert(elements.length == 2)
    AtomSignature(elements(0), elements(1).toInt)
  }

  private val sep = System.getProperty("file.separator")
  private val prefix = System.getProperty("user.dir") + sep + "Examples" + sep + "data" + sep + "tests" + sep + "DependencyMap" + sep
  private val mlnFile = prefix + "DependencyMap.mln"
  private val evidenceFile = prefix + "Empty.db"


  implicit val mln = MLN(mlnFile, evidenceFile, queryAtoms = Set("S/1", "C/1", "K/1", "M/1", "F/2"))

  describe(s"The MLN theory in '$mlnFile'"){

    it("should contain 7 formulas") {
      mln.formulas.size shouldBe 7
    }

    it("should contain 1 constant set (domain)") {
      mln.constants.size shouldBe 1
    }

    it("should contain 5 predicate schemas") {
      mln.predicateSchema.size shouldBe 5
    }

    it("should not contain any function schemas") {
      mln.functionSchema.size shouldBe 0
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

        val constraintWeight = mrf.constraints.get(constraintID).weight
        var clauseWeight = 0.0
        var total = 0.0

        while (statsIterator.hasNext) {
          statsIterator.advance()
          val cid = statsIterator.key()
          val freq = statsIterator.value()
          clauseWeight = if (mln.clauses(cid).isHard) 18.4 else mln.clauses(cid).weight //TODO: explain 18.4
          total += clauseWeight * freq
        }

        it(s"has the constraint $constraintID -> ${decodeFeature(mrf.constraints.get(constraintID)).getOrElse("Failed to decode constraint")}, which can be reconstructed") {
          constraintWeight shouldBe total
        }
      }
    }

  }
}

