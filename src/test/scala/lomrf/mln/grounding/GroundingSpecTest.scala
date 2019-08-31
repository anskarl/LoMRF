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

package lomrf.mln.grounding

import java.io.File

import lomrf.logic.AtomSignature
import lomrf.mln.model.MLN
import lomrf.util.time.{ measureTime, msecTimeToText }
import org.scalatest.{ FunSpec, Matchers }
import lomrf.tests.TestData
import lomrf.util.io._

import scala.io.Source

/**
  * Specification test regarding the grounding process (i.e., the creation of MRF from an MLN)
  */
final class GroundingSpecTest extends FunSpec with Matchers {

  private val mainPath = TestData.TestFilesPath / "inference" / "caviar" / "DN"

  val queryAtoms = Set(AtomSignature("HoldsAt", 2))

  val cwa = Set(
    AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2),
    AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1))

  var totalTime = 0L
  var iterations = 0

  for {
    inertiaConfiguration <- List("HI", "SI", "SI_h")

    fold <- 0 to 9

    currentPath = new File(mainPath / inertiaConfiguration / "meet" / "fold_" + fold)
    if currentPath.exists

    mlnFile = findFirstFile(currentPath, _.getName.endsWith(".mln"))
      .getOrElse(sys.error("Cannot find MLN in '" + currentPath + "'"))

    expectedResultFiles = findFiles(currentPath, _.getName.endsWith(".mws.golden"))

    dbFile <- findFiles(currentPath, _.getName.endsWith(".db"))
  } describe("Loading MLN theory from file '" + mlnFile + "', with evidence from file '" + dbFile) {

    val mln = MLN.fromFile(mlnFile.getAbsolutePath, queryAtoms, dbFile.getAbsolutePath, cwa)

    val stats = {
      val inSource = Source.fromFile(dbFile.getAbsolutePath.replace(".db", ".statistics"))
      try {
        inSource.getLines()
          .map { line =>
            val entries = line.split('=')
            entries(0) -> entries(1)
          }
          .toMap
      } finally {
        inSource.close()
      }

    }

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

    val (time, mrf) = measureTime(mrfBuilder.buildNetwork)
    totalTime += time
    iterations += 1

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

    it("has continuous index of ground clause ids") {
      val keys = mrf.constraints.keys()
      var fail = false
      java.util.Arrays.sort(keys)

      for ((key, idx) <- keys.zipWithIndex) {
        if (key != idx) {
          info(key + " != " + idx)
          fail = true
        }
      }

      fail shouldEqual false
    }

  }

  info(msecTimeToText("Total time spend for grounding : ", totalTime))
  info(msecTimeToText("Average time spend for grounding : ", totalTime / iterations))

}
