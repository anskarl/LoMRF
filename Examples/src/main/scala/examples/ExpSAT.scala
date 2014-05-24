package examples

import lomrf.mln.model._
import lomrf.logic.AtomSignature
import lomrf.mln.inference.{MRF, MaxWalkSAT, MRFBuilder}
import java.io.{FileOutputStream, PrintStream}
import lomrf.util.{Utilities, Logging}


object ExpSAT extends Logging {
  val sep = System.getProperty("file.separator")
  val path_prefix = System.getProperty("user.dir") + sep + "data" + sep + "yale_shooting" + sep

  def main(args: Array[String]) {
    val (mrf, outputStream) = example1a()

    val solver = new MaxWalkSAT(mrf)
    val startInferTime = System.currentTimeMillis()
    solver.infer()
    val endInferTime = System.currentTimeMillis()
    println(Utilities.msecTimeToText("Total MaxWalkSAT time: ", endInferTime - startInferTime))
    solver.writeResults(outputStream)

  }

  def example1(): (MRF, PrintStream) = {
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep + "yale_shooting" + sep

    val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
    val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Next", 2))
    val owa = Set[AtomSignature](AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

    info("Loading an MLN instance from data.")
    info("Theory: " + path_prefix + "theory.mln")
    info("Evidence: " + path_prefix + "narrative.db")
    implicit val mln = MLN(path_prefix + "theory.mln", path_prefix + "narrative.db", queryAtoms, cwa, owa)

    info("\n" + mln + "\n")
    info("Number of CNF clauses = " + mln.clauses.size)


    val kbmc = new MRFBuilder(mln)
    val startTime = System.nanoTime
    val mrf = kbmc.buildNetwork
    val endTime = System.nanoTime
    info("Total grounding time: " + Utilities.nsecTimeToText(endTime - startTime))
    //return
    (mrf, System.out)
  }

  def example1a(): (MRF, PrintStream) = {
      val path_prefix = System.getProperty("user.dir") + sep + "data" + sep + "yale_shooting" + sep

      val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
      val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Next", 2))
      val owa = Set[AtomSignature](AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

      info("Loading an MLN instance from data.")
      info("Theory: " + path_prefix + "theory.mln")
      info("Evidence: " + path_prefix + "narrative.db")
      implicit val mln = MLN(path_prefix + "theory.mln", path_prefix + "narrative.db", queryAtoms, cwa, owa)

      info("\n" + mln + "\n")
      info("Number of CNF clauses = " + mln.clauses.size)


      val kbmc = new MRFBuilder(mln)
      val startTime = System.nanoTime
      val mrf = kbmc.buildNetwork
      val endTime = System.nanoTime
      info("Total grounding time: " + Utilities.nsecTimeToText(endTime - startTime))
      //return
      (mrf, System.out)
    }

  def example2(): (MRF, PrintStream) = {
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep + "caviar" + sep + "video25_id6_id7_next_small" + sep
    val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
    val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1), AtomSignature("Next", 2))
    val owa = Set[AtomSignature](AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

    info("Loading an MLN instance from data.")
    implicit val mln = MLN(
      path_prefix + "dec7a.mln",
      path_prefix + "fra1gt_evidence.db",
      queryAtoms, cwa, owa)

    info("\n" + mln + "\n")
    info("Number of CNF clauses = " + mln.clauses.size)


    val kbmc = new MRFBuilder(mln)
    val startTime = System.nanoTime
    val mrf = kbmc.buildNetwork
    val endTime = System.nanoTime
    info("Total grounding time: " + Utilities.nsecTimeToText(endTime - startTime))

    val output = new PrintStream(new FileOutputStream(path_prefix+"lomrf-output.result"), true)

    //return
    (mrf, output)
  }

}
