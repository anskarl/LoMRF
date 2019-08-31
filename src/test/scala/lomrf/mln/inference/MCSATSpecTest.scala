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

package lomrf.mln.inference

import java.io.{ File, FileOutputStream, PrintStream }

import lomrf.logic.AtomSignature
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.model.MLN
import lomrf.tests.TestData
import lomrf.util.io._
import org.scalatest.{ FunSpec, Matchers }

import scala.io.Source

/**
  * Specification test for MC-SAT algorithm used for marginal inference.
  */
final class MCSATSpecTest extends FunSpec with Matchers {

  private val mainPath = TestData.TestFilesPath / "inference" / "caviar" / "DN"

  val queryAtoms = Set(AtomSignature("HoldsAt", 2))

  val cwa = Set(
    AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2),
    AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1))

  val inertiaConfiguration = "HI"

  for {
    fold <- 0 to 9

    currentPath = new File(mainPath / inertiaConfiguration / "meet" / "fold_" + fold)
    if currentPath.exists

    mlnFile = findFirstFile(currentPath, _.getName.endsWith(".mln"))
      .getOrElse(sys.error("Cannot find MLN in '" + currentPath + "'"))

    expectedResultFiles = findFiles(currentPath, _.getName.endsWith(".mcsat.golden"))

    dbFile <- findFiles(currentPath, _.getName.endsWith(".db"))
  } describe("Loading MLN theory from file '" + mlnFile + "', with evidence from file '" + dbFile) {

    val mln = MLN.fromFile(mlnFile.getAbsolutePath, queryAtoms, dbFile.getAbsolutePath, cwa)

    val stats = Source
      .fromFile(dbFile.getAbsolutePath.replace(".db", ".statistics"))
      .getLines()
      .map(line => line.split('='))
      .map(entries => entries(0) -> entries(1))
      .toMap

    /*it(s"should contain ${stats("mln.formulas.size")} formulas") {
      mln.formulas.size should be(stats("mln.formulas.size").toInt)
    }*/

    it(s"should constants ${stats("mln.constants.size")} constants sets (domains)") {
      mln.evidence.constants.size should be(stats("mln.constants.size").toInt)
    }

    it(s"should contain ${stats("mln.predicateSchema.size")} predicate schemas") {
      mln.schema.predicates.size should be(stats("mln.predicateSchema.size").toInt)
    }

    it(s"should contain ${stats("mln.functionSchema.size")} function schemas") {
      mln.schema.functions.size should be(stats("mln.functionSchema.size").toInt)
    }

    info("Creating MRF...")
    val mrfBuilder = new MRFBuilder(mln, createDependencyMap = false)
    val mrf = mrfBuilder.buildNetwork

    describe("The constructed MRF") {
      it(s"should contain ${stats("mrf.atoms.size")} ground atoms") {
        mrf.atoms.size should be(stats("mrf.atoms.size").toInt)
      }

      it(s"should contain ${stats("mrf.constraints.size")} ground clauses") {
        mrf.constraints.size should be(stats("mrf.constraints.size").toInt)
      }

      it(s"should have ${stats("mrf.weightHard")} as hard weight value") {
        mrf.weightHard should be(stats("mrf.weightHard").toDouble)
      }
    }

    describe("The result of marginal inference using MCSAT") {

      val prefix = mlnFile.getParent.getAbsolutePath / dbFile.getName.split(".db")(0)

      val golden = expectedResultFiles
        .find(f => f.getName.contains(dbFile.getName.split(".db")(0)))
        .getOrElse(sys.error("Failed to locate golden standard file."))

      val solver = MCSAT(mrf)
      solver.infer
      solver.writeResults(new PrintStream(new FileOutputStream(prefix + ".mcsat.result"), true))

      info("Inspecting result file: '" + prefix + ".mcsat.result'")

      val inferredResults = Source.fromFile(prefix + ".mcsat.result").getLines()

      // Create a Map [predicate -> value] that contains the expected output (Golden Standard)
      val expectedResultsMap = Source
        .fromFile(golden.getAbsolutePath)
        .getLines()
        .map(_.split(' '))
        .map(entries => entries(0).trim -> entries(1).trim.toDouble)
        .toMap

      var majorDifferences = 0
      var totalError = 0.0
      var countedResults = 0

      for ((inferred, lineNumber) <- inferredResults.zipWithIndex) {

        val slittedLine = inferred.split(' ')
        val inferredPredicateSrc = slittedLine(0)
        val inferredValueSrc = slittedLine(1)
        val inferredPredicate = inferredPredicateSrc.trim
        val inferredValue = inferredValueSrc.trim.toDouble
        val expectedValueOpt = expectedResultsMap.get(inferredPredicate)

        countedResults += 1

        assert(expectedValueOpt.isDefined)
        val expectedValue = expectedValueOpt.get

        if (inferredValue != expectedValue) {
          val currentError = math.abs(expectedValue - inferredValue)

          totalError += currentError

          if (currentError >= 0.2) {
            majorDifferences += 1
            info(s"\tLine '$lineNumber': The estimated probability of '$inferredPredicate' is '$inferredValue' which differs significantly (>=0.2 p.p.) from the expected probability '" + expectedValue + "'")
          }
        }

      }

      it(s"produces marginal probabilities for ${expectedResultsMap.size} ground query predicates.") {
        countedResults shouldBe expectedResultsMap.size
      }

      it("has average error less than 0.01") {
        assert((totalError / countedResults) <= 0.01)
      }

      it("has maximum error less than 0.2") {
        majorDifferences shouldBe 0
      }

    }

  }

}
