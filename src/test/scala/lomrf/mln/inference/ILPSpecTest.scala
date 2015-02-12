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
import lomrf.util.Utilities.io.{findFiles, strToFile}
import scala.collection.immutable.HashMap
import scala.io.Source

/**
 * Specification test for ILP algorithm used for MAP inference.
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
final class ILPSpecTest extends FunSpec with Matchers {

  private val sep = System.getProperty("file.separator")
  private val testFilesPath = System.getProperty("user.dir") + sep + "Examples" + sep + "data" + sep +
    "tests" + sep + "inference" + sep + "caviar" + sep + "MM" + sep

  private val mlnFiles = findFiles(strToFile(testFilesPath), f => f.getName.contains(".mln"))
  private val dbFilesList = findFiles(strToFile(testFilesPath), f => f.getName.contains(".db"))
  private val goldenFilesList = findFiles(strToFile(testFilesPath), f => f.getName.contains(".ilp.golden"))

  println(mlnFiles.mkString(", "))
  println(dbFilesList.mkString(", "))
  println(goldenFilesList.mkString(", "))
  describe("Caviar max-margin test in path: '" + testFilesPath + "'") {

    for(weightType <- List("HI", "SI", "SI_h")) {
      for (fold <- 0 to 9) {
        val mlnFile = mlnFiles.filter(f => f.getAbsolutePath.contains("fold_" + fold) &&
          f.getAbsolutePath.contains(sep + weightType + sep))
        val dbFiles = dbFilesList.filter(f => f.getAbsolutePath.contains("fold_" + fold) &&
          f.getAbsolutePath.contains(sep + weightType + sep))
        val goldenFiles = goldenFilesList.filter(f => f.getAbsolutePath.contains("fold_" + fold) &&
          f.getAbsolutePath.contains(sep + weightType + sep))

        for(db <- dbFiles) {
          describe("MLN from file '" + mlnFile(0) + "' with evidence from file '" + db) {
            val mln = MLN.apply(
              mlnFileName = mlnFile(0).getAbsolutePath,
              evidenceFileName = db.getAbsolutePath,
              queryAtoms = Set(AtomSignature("HoldsAt", 2)),
              cwa = Set(AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2),
                AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1)))

            info("Found " + mln.formulas.size + " formulas")
            info("Found " + mln.constants.size + " constant types")
            info("Found " + mln.predicateSchema.size + " predicate schemas")
            info("Found " + mln.functionSchema.size + " function schemas")

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
              val mrfBuilder = new MRFBuilder(mln)
              val mrf = mrfBuilder.buildNetwork

              info("Created " + mrf.numberOfAtoms + " ground atoms")
              info("Created " + mrf.numberOfConstraints + " ground clauses")

              describe("Running MAP inference using ILP") {

                val prefix = db.getName.split(".db")(0)
                val golden = goldenFiles.find(f => f.getName.contains(prefix)).get

                val resultsWriter = new PrintStream(
                                    new FileOutputStream(
                                    mlnFile(0).getParent.getAbsolutePath + sep + prefix + ".ilp.result"), true)

                val solver = new ILP(mrf)
                solver.infer()
                solver.writeResults(resultsWriter)

                var results = HashMap[String, Int]()
                for (line <- Source.fromFile(mlnFile(0).getParent.getAbsolutePath + sep + prefix + ".ilp.result").getLines()) {
                  val element = line.split(" ")
                  results += ((element(0), element(1).toInt))
                }

                var standard = HashMap[String, Int]()
                for (line <- Source.fromFile(golden.getAbsolutePath).getLines()) {
                  val element = line.split(" ")
                  standard += ((element(0), element(1).toInt))
                }

                var total = 0
                for( (atom, value) <- results)
                  total += math.abs(value - standard.get(atom).get)

                info("Number of differences: " + total)

                it("should be less or equal than 5") {
                  assert(total <= 5)
                }
              }

            }
          }
        }

      }
    }
  }

}