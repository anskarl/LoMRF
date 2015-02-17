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

  implicit def str2AtomSignature(txt: String): AtomSignature = {
    val elements = txt.split("/")
    assert(elements.length == 2)
    AtomSignature(elements(0), elements(1).toInt)
  }

  private val sep = System.getProperty("file.separator")
  private val prefix = System.getProperty("user.dir") + sep + "Examples" + sep + "data" + sep + "tests" + sep + "DependencyMap" + sep
  private val mlnFile = prefix + "DependencyMap.mln"
  private val evidenceFile = prefix + "Empty.db"

  implicit val mln = MLN(
    mlnFile,
    evidenceFile,
    queryAtoms = Set("S/1", "C/1", "K/1", "M/1", "F/2"),
    cwa = Set.empty,
    owa = Set.empty)

  info("Found " + mln.formulas.size + " formulas")
  info("Found " + mln.constants.size + " constant types")
  info("Found " + mln.predicateSchema.size + " predicate schemas")
  info("Found " + mln.functionSchema.size + " function schemas")

  it("should contain 7 formulas") {
    mln.formulas.size should be(7)
  }

  it("should constants 1 constants sets (domains)") {
    mln.constants.size should be(1)
  }

  it("should contain 5 predicate schemas") {
    mln.predicateSchema.size should be(5)
  }

  it("should contain 0 function schemas") {
    mln.functionSchema.size should be(0)
  }

  describe("Checking dependency map when no negative weights are allowed") {

    val mrfNoNeg = MRF.build(mln, noNegWeights = true, createDependencyMap = true)

    info("Created " + mrfNoNeg.numberOfAtoms + " ground atoms")
    info("Created " + mrfNoNeg.numberOfConstraints + " ground clauses")

    it("should contain 12 ground atoms") {
      mrfNoNeg.numberOfAtoms should be(12)
    }

    it("should contain 19 ground clauses") {
      mrfNoNeg.numberOfConstraints should be(19)
    }

    println("\n-----------------------------------------------------------")

    val dmIterator = mrfNoNeg.dependencyMap.getOrElse(sys.error("Dependency map does not exist!")).iterator()

    while (dmIterator.hasNext) {
      dmIterator.advance()
      val constraintID = dmIterator.key()
      println("constraint: " + constraintID + " -> " + decodeFeature(mrfNoNeg.constraints.get(constraintID)).getOrElse("Failed to decode constraint"))
      val statsIterator = dmIterator.value.iterator()

      val constraintWeight = mrfNoNeg.constraints.get(constraintID).weight
      var clauseWeight = 0.0
      var total = 0.0

      while (statsIterator.hasNext) {
        statsIterator.advance()
        val cid = statsIterator.key()
        val freq = statsIterator.value()
        clauseWeight = if (mln.clauses(cid).isHard) 18.4 else mln.clauses(cid).weight
        total += clauseWeight * freq
        println("\t{clause: " + cid + " -> " + mln.clauses(cid) + ", frequency = " + freq + "}")
      }

      it("Constraint " + constraintID + " -> " + decodeFeature(mrfNoNeg.constraints.get(constraintID)).getOrElse("Failed to decode constraint") + " can be reconstructed") {
        constraintWeight should be(total)
      }

      println()
    }

    println("-----------------------------------------------------------\n")

  }

  describe("Checking dependency map when negative weights are allowed") {

    val mrfNeg = MRF.build(mln, createDependencyMap = true)

    info("Created " + mrfNeg.numberOfAtoms + " ground atoms")
    info("Created " + mrfNeg.numberOfConstraints + " ground clauses")

    it("should contain 10 ground atoms") {
      mrfNeg.numberOfAtoms should be(10)
    }

    it("should contain 11 ground clauses") {
      mrfNeg.numberOfConstraints should be(11)
    }

    println("\n-----------------------------------------------------------")

    val dmIterator = mrfNeg.dependencyMap.getOrElse(sys.error("Dependency map does not exist!")).iterator()

    while (dmIterator.hasNext) {
      dmIterator.advance()
      val constraintID = dmIterator.key()
      println("constraint: " + constraintID + " -> " + decodeFeature(mrfNeg.constraints.get(constraintID)).getOrElse("Failed to decode constraint"))
      val statsIterator = dmIterator.value.iterator()

      val constraintWeight = mrfNeg.constraints.get(constraintID).weight
      var clauseWeight = 0.0
      var total = 0.0

      while (statsIterator.hasNext) {
        statsIterator.advance()
        val cid = statsIterator.key()
        val freq = statsIterator.value()
        clauseWeight = if (mln.clauses(cid).isHard) 18.4 else mln.clauses(cid).weight
        total += clauseWeight * freq
        println("\t{clause: " + cid + " -> " + mln.clauses(cid) + ", frequency = " + freq + "}")
      }

      it("Constraint " + constraintID + " -> " + decodeFeature(mrfNeg.constraints.get(constraintID)).getOrElse("Failed to decode constraint") + " can be reconstructed") {
        constraintWeight should be(total)
      }

      println()
    }

    println("-----------------------------------------------------------\n")

  }
}

