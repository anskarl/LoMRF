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
 * Copyright (C) 2012  Anastasios Skarlatidis.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package lomrf.mln.inference

import java.io.{File, FileOutputStream, PrintStream}
import lomrf.logic.AtomSignature
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.model.MLN
import lomrf.util.Utilities.io._
import org.scalatest.{FunSpec, Matchers}
import scala.io.Source

/**
 * Specification test for MaxWalkSAT algorithm used for MAP inference.
 */
final class MaxWalkSATSpecTest extends FunSpec with Matchers {

  private val sep = System.getProperty("file.separator")

  private val mainPath = System.getProperty("user.dir") + sep +
    "Examples" + sep + "data" + sep + "tests" + sep + "inference" + sep + "caviar" + sep + "DN"

  val queryAtoms = Set(AtomSignature("HoldsAt", 2))

  val cwa = Set(
    AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2),
    AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1))

  for {
    inertiaConfiguration <- List("HI", "SI", "SI_h")

    fold <- 0 to 9

    currentPath = new File(mainPath + sep + inertiaConfiguration + sep + "meet" + sep + "fold_" + fold)
    if currentPath.exists

    mlnFile = findFirstFile(currentPath, _.getName.endsWith(".mln"))
      .getOrElse(sys.error("Cannot find MLN in '"+currentPath+"'"))

    expectedResultFiles = findFiles(currentPath, _.getName.endsWith(".mws.golden"))

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
      mln.constants.size should be(stats("mln.constants.size").toInt)
    }

    it(s"should contain ${stats("mln.predicateSchema.size")} predicate schemas") {
      mln.schema.predicateSchema.size should be(stats("mln.predicateSchema.size").toInt)
    }

    it(s"should contain ${stats("mln.functionSchema.size")} function schemas") {
      mln.schema.functionSchema.size should be(stats("mln.functionSchema.size").toInt)
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

      it(s"should has ${stats("mrf.weightHard")} as hard weight value") {
        mrf.weightHard should be(stats("mrf.weightHard").toDouble)
      }
    }

    describe("The result of MAP inference using MaxWalkSAT") {

      val prefix = mlnFile.getParent.getAbsolutePath + sep + dbFile.getName.split(".db")(0)

      val golden = expectedResultFiles
        .find(f => f.getName.contains(dbFile.getName.split(".db")(0)))
        .getOrElse(sys.error("Failed to locate golden standard file."))

      val resultsWriter = new PrintStream(new FileOutputStream(prefix + ".mws.result"), true)

      val solver = new MaxWalkSAT(mrf)
      solver.infer()
      solver.writeResults(resultsWriter)

      info("Inspecting result file: '" + prefix + ".mws.result'")
      val inferredResults = Source.fromFile(prefix + ".mws.result").getLines()

      // Create a Map [predicate -> value] that contains the expected output (Golden Standard)
      val expectedResultsMap = Source
        .fromFile(golden.getAbsolutePath)
        .getLines()
        .map(_.split(' '))
        .map(entries => entries(0).trim -> entries(1).trim.toInt)
        .toMap

      var differences = 0
      var countedResults = 0

      for ((inferred, lineNumber) <- inferredResults.zipWithIndex) {

        val slittedLine = inferred.split(' ')
        val inferredPredicateSrc = slittedLine(0)
        val inferredValueSrc = slittedLine(1)
        val inferredPredicate = inferredPredicateSrc.trim
        val inferredValue = inferredValueSrc.trim.toInt
        val expectedValueOpt = expectedResultsMap.get(inferredPredicate)

        countedResults += 1

        assert(expectedValueOpt.isDefined)
        val expectedValue = expectedValueOpt.get

        if (inferredValue != expectedValue) {
          differences += 1
          info(s"\tLine '$lineNumber' the output ground predicate '$inferredPredicate' should be '" + expectedValue + "'")
        }

      }

      it(s"produces MAP results for ${expectedResultsMap.size} ground query predicates."){
        countedResults shouldBe expectedResultsMap.size
      }

      it("has identical output with the corresponding golden standard result file") {
        differences should equal(0)
      }

    }

  }

}
