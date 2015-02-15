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

import java.io.{FileOutputStream, PrintStream}
import lomrf.logic.AtomSignature
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.model.MLN
import org.scalatest.{Matchers, FunSpec}
import scala.io.Source
import lomrf.util.Utilities.io.{findFiles, strToFile}

/**
 * Specification test for MaxWalkSAT algorithm used for MAP inference.
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
final class MaxWalkSATSpecTest extends FunSpec with Matchers {

  private val sep = System.getProperty("file.separator")
  private val testFilesPath = System.getProperty("user.dir") + sep +
    "Examples" + sep + "data" + sep + "tests" + sep + "inference" + sep

  private val mlnFiles = findFiles(testFilesPath, f => f.getName.contains(".mln"))
  assert(mlnFiles.size > 0, "Failed to find input MLN files (*.mln) in '" + testFilesPath + "'")

  private val dbFilesList = findFiles(testFilesPath, f => f.getName.contains(".db"))
  assert(dbFilesList.size > 0, "Failed to find input DB files (*.db) in '" + testFilesPath + "'")

  private val goldenFilesList = findFiles(testFilesPath, f => f.getName.contains(".mws.golden"))
  assert(goldenFilesList.size > 0, "Failed to find input golden standard files (*.mws.golden) in '" + testFilesPath + "'")


  val queryAtoms = Set(AtomSignature("HoldsAt", 2))
  val cwa = Set(AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2), AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1))

  for {
    weightType <- List("HI", "SI", "SI_h")

    fold <- 0 to 9

    mlnFile = mlnFiles
      .filter(f => f.getAbsolutePath.contains("fold_" + fold) && f.getAbsolutePath.contains(sep + weightType + sep))

    goldenFiles = goldenFilesList
      .filter(f => f.getAbsolutePath.contains("fold_" + fold) && f.getAbsolutePath.contains(sep + weightType + sep))

    db <- dbFilesList.filter(f => f.getAbsolutePath.contains("fold_" + fold) && f.getAbsolutePath.contains(sep + weightType + sep))

  } describe("MLN from file '" + mlnFile(0) + "' with evidence from file '" + db) {

    val mln = MLN(mlnFile(0).getAbsolutePath, db.getAbsolutePath, queryAtoms, cwa)

    info {
      s"""Found ${mln.formulas.size} formulas
              Found ${mln.constants.size} constant types
              Found ${mln.predicateSchema.size} predicate schemas
              Found ${mln.functionSchema.size} function schemas"""
    }

    it("should contain 25 formulas") {
      mln.formulas.size should be(25)
    }

    it("should constants 5 constants sets (domains)") {
      mln.constants.size should be(5)
    }

    it("should contain 6 predicate schemas") {
      mln.predicateSchema.size should be(6)
    }

    it("should contain 7 function schemas") {
      mln.functionSchema.size should be(7)
    }

    describe("Creating MRF from previous MLN") {

      info("Creating MRF...")
      val mrfBuilder = new MRFBuilder(mln, createDependencyMap = true)
      val mrf = mrfBuilder.buildNetwork

      info("Created " + mrf.numberOfAtoms + " ground atoms")
      info("Created " + mrf.numberOfConstraints + " ground clauses")

      describe("Running MAP inference using MaxWalkSAT") {

        val prefix = mlnFile(0).getParent.getAbsolutePath + sep + db.getName.split(".db")(0)

        val golden = goldenFiles
          .find(f => f.getName.contains(db.getName.split(".db")(0)))
          .getOrElse(sys.error("Failed to locate golden standard file."))

        val resultsWriter = new PrintStream(new FileOutputStream(prefix + ".mws.result"), true)

        val solver = new MaxWalkSAT(mrf)
        solver.infer()
        solver.writeResults(resultsWriter)


        it("should be identical to the golden standard") {
          val inferredResults = Source.fromFile(prefix + ".mws.result").getLines()

          // Create a Map [predicate -> value] that contains the expected output (Golden Standard)
          val expectedResultsMap = Source
            .fromFile(golden.getAbsolutePath)
            .getLines()
            .map(line => line.splitAt(line.lastIndexOf(' ')))
            .map { case (expectedPredicate, expectedValue) => expectedPredicate.trim -> expectedValue.trim.toInt}
            .toMap

          println("Inspecting result file: '" + prefix + ".mws.result'")
          var differences = 0
          var countedResults = 0

          for {

            (inferred, lineNumber) <- inferredResults.zipWithIndex
            (inferredPredicateSrc, inferredValueSrc) = inferred.splitAt(inferred.lastIndexOf(' '))
            inferredPredicate = inferredPredicateSrc.trim
            inferredValue = inferredValueSrc.trim.toInt
            expectedValueOpt = expectedResultsMap.get(inferredPredicate)
          } {

            countedResults += 1

            assert(expectedValueOpt.isDefined)
            val expectedValue = expectedValueOpt.get

            if (inferredValue != expectedValue) {
              differences += 1
              println(s"\tLine '$lineNumber' the output ground predicate '$inferredPredicate' should be '" + expectedValue + "'")
            }


          }

          differences should equal(0)

          countedResults should equal(expectedResultsMap.size)
        }

      }
    }
  }

}