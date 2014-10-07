package lomrf.inference

import java.io.{FileOutputStream, PrintStream}
import lomrf.logic.AtomSignature
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.MaxWalkSAT
import lomrf.mln.model.MLN
import org.scalatest.{Matchers, FunSpec}
import scala.collection.immutable.HashMap
import scala.io.Source

/**
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
final class MaxWalkSATSpecTest extends FunSpec with Matchers {

  private val sep = System.getProperty("file.separator")
  private val testFilesPath = System.getProperty("user.dir") + sep + "Examples" + sep + "data" + sep

  private val resultFile = "infer_map.result"
  private val goldenStandard = "golden_standard"

  describe("Caviar video 27 in path: '" + testFilesPath + "'") {
    val dataPath = "caviar" + sep + "video27" + sep

    describe("MLN from file '" + dataPath + "HI/meet_trained_func.mln' with evidence from file '" + dataPath +
      "HI/27-Fight_OneManDown2.id2_id6.func.db'") {

      val mln = MLN(
        mlnFileName = testFilesPath + dataPath +"HI/meet_trained_func.mln",
        evidenceFileName = testFilesPath + dataPath + "HI/27-Fight_OneManDown2.id2_id6.func.db",
        queryAtoms = Set(AtomSignature("HoldsAt", 2)),
        cwa = Set(AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2),
          AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1)))

      info("Found " + mln.formulas.size + " formulas")
      info("Found " + mln.constants.size + " constant types")
      info("Found " + mln.schema.size + " predicate schemas")
      info("Found " + mln.functionSchema.size + " function schemas")

      it("should contain 25 formulas") {
        mln.formulas.size should be(25)
      }

      it("should constants 5 constants sets (domains)") {
        mln.constants.size should be(5)
      }

      it("should contain 6 predicate schemas") {
        mln.schema.size should be(6)
      }

      it("should contain 7 function schemas") {
        mln.functionSchema.size should be(7)
      }

      describe("Creating MRF from previous MLN") {

        val resultsWriter = new PrintStream(new FileOutputStream(testFilesPath + dataPath + "HI" + sep + resultFile), true)

        info("Creating MRF...")
        val mrfBuilder = new MRFBuilder(mln)
        val mrf = mrfBuilder.buildNetwork

        info("Created " + mrf.numberOfAtoms + " ground atoms")
        info("Created " + mrf.numberOfConstraints + " ground clauses")

        it("should contain 3836 ground atoms") {
          mrf.numberOfAtoms should be(3836)
        }

        it("should contain 9315 ground clauses") {
          mrf.numberOfConstraints should be(9315)
        }

        describe("Running MAP inference using MaxWalkSAT") {

          val solver = new MaxWalkSAT(mrf)
          solver.infer()
          solver.writeResults(resultsWriter)

          var results = HashMap[String, Int]()
          for (line <- Source.fromFile(testFilesPath + dataPath + "HI" + sep + resultFile).getLines()) {
            val element = line.split(" ")
            results += ((element(0), element(1).toInt))
          }

          var standard = HashMap[String, Int]()
          for (line <- Source.fromFile(testFilesPath + dataPath + "HI" + sep + goldenStandard).getLines()) {
            val element = line.split(" ")
            standard += ((element(0), element(1).toInt))
          }

          it("should be identical to the golden standard") {
            assert(results == standard)
          }

        }
      }
    }

    describe("MLN from file '" + dataPath + "SI/meet_trained_func.mln' with evidence from file '" + dataPath +
      "SI/27-Fight_OneManDown2.id2_id6.func.db'") {

      val mln = MLN(
        mlnFileName = testFilesPath + dataPath +"SI/meet_trained_func.mln",
        evidenceFileName = testFilesPath + dataPath + "SI/27-Fight_OneManDown2.id2_id6.func.db",
        queryAtoms = Set(AtomSignature("HoldsAt", 2)),
        cwa = Set(AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2),
          AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1)))

      info("Found " + mln.formulas.size + " formulas")
      info("Found " + mln.constants.size + " constant types")
      info("Found " + mln.schema.size + " predicate schemas")
      info("Found " + mln.functionSchema.size + " function schemas")

      it("should contain 25 formulas") {
        mln.formulas.size should be(25)
      }

      it("should constants 5 constants sets (domains)") {
        mln.constants.size should be(5)
      }

      it("should contain 6 predicate schemas") {
        mln.schema.size should be(6)
      }

      it("should contain 7 function schemas") {
        mln.functionSchema.size should be(7)
      }

      describe("Creating MRF from previous MLN") {

        val resultsWriter = new PrintStream(new FileOutputStream(testFilesPath + dataPath + "SI" + sep + resultFile), true)

        info("Creating MRF...")
        val mrfBuilder = new MRFBuilder(mln)
        val mrf = mrfBuilder.buildNetwork

        info("Created " + mrf.numberOfAtoms + " ground atoms")
        info("Created " + mrf.numberOfConstraints + " ground clauses")

        it("should contain 3836 ground atoms") {
          mrf.numberOfAtoms should be(3836)
        }

        it("should contain 9315 ground clauses") {
          mrf.numberOfConstraints should be(9315)
        }

        describe("Running MAP inference using MaxWalkSAT") {

          val solver = new MaxWalkSAT(mrf)
          solver.infer()
          solver.writeResults(resultsWriter)

          var results = HashMap[String, Int]()
          for (line <- Source.fromFile(testFilesPath + dataPath + "SI" + sep + resultFile).getLines()) {
            val element = line.split(" ")
            results += ((element(0), element(1).toInt))
          }

          var standard = HashMap[String, Int]()
          for (line <- Source.fromFile(testFilesPath + dataPath + "SI" + sep + goldenStandard).getLines()) {
            val element = line.split(" ")
            standard += ((element(0), element(1).toInt))
          }

          it("should be identical to the golden standard") {
            assert(results == standard)
          }

        }
      }
    }

    describe("MLN from file '" + dataPath + "SI_h/meet_trained_func.mln' with evidence from file '" + dataPath +
      "SI_h/27-Fight_OneManDown2.id2_id6.func.db'") {

      val mln = MLN(
        mlnFileName = testFilesPath + dataPath +"SI_h/meet_trained_func.mln",
        evidenceFileName = testFilesPath + dataPath + "SI_h/27-Fight_OneManDown2.id2_id6.func.db",
        queryAtoms = Set(AtomSignature("HoldsAt", 2)),
        cwa = Set(AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2),
          AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1)))

      info("Found " + mln.formulas.size + " formulas")
      info("Found " + mln.constants.size + " constant types")
      info("Found " + mln.schema.size + " predicate schemas")
      info("Found " + mln.functionSchema.size + " function schemas")

      it("should contain 25 formulas") {
        mln.formulas.size should be(25)
      }

      it("should constants 5 constants sets (domains)") {
        mln.constants.size should be(5)
      }

      it("should contain 6 predicate schemas") {
        mln.schema.size should be(6)
      }

      it("should contain 7 function schemas") {
        mln.functionSchema.size should be(7)
      }

      describe("Creating MRF from previous MLN") {

        val resultsWriter = new PrintStream(new FileOutputStream(testFilesPath + dataPath + "SI_h" + sep + resultFile), true)

        info("Creating MRF...")
        val mrfBuilder = new MRFBuilder(mln)
        val mrf = mrfBuilder.buildNetwork

        info("Created " + mrf.numberOfAtoms + " ground atoms")
        info("Created " + mrf.numberOfConstraints + " ground clauses")

        it("should contain 3836 ground atoms") {
          mrf.numberOfAtoms should be(3836)
        }

        it("should contain 9315 ground clauses") {
          mrf.numberOfConstraints should be(9315)
        }

        describe("Running MAP inference using MaxWalkSAT") {

          val solver = new MaxWalkSAT(mrf)
          solver.infer()
          solver.writeResults(resultsWriter)

          var results = HashMap[String, Int]()
          for (line <- Source.fromFile(testFilesPath + dataPath + "SI_h" + sep + resultFile).getLines()) {
            val element = line.split(" ")
            results += ((element(0), element(1).toInt))
          }

          var standard = HashMap[String, Int]()
          for (line <- Source.fromFile(testFilesPath + dataPath + "SI_h" + sep + goldenStandard).getLines()) {
            val element = line.split(" ")
            standard += ((element(0), element(1).toInt))
          }

          it("should be identical to the golden standard") {
            assert(results == standard)
          }

        }
      }
    }
  }

}