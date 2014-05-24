package examples

import lomrf.mln.model.MLN
import lomrf.logic.AtomSignature
import lomrf.mln.inference.{MRF, MCSAT, MRFBuilder}
import java.io.{FileOutputStream, PrintStream}
import lomrf.logic.PredicateCompletionMode._
import lomrf.util.Logging

object ExpMCSAT extends Logging {
  val sep = System.getProperty("file.separator")

  def main(args: Array[String]) {
    //val (mrf, outputStream) = example4(Decomposed)
    //val (mrf, outputStream) = example4(Simplification)

    //val (mrf, outputStream) = inertia1()
    //val (mrf, outputStream) = uniform()
    //val (mrf, outputStream) = binomial()
    //val (mrf, outputStream) = multinomial()

    val (mrf, outputStream) = example5(Decomposed)

    val solver = new MCSAT(mrf, unitPropagation = true, samples = 1000, pBest = 0.5, lateSA = true, saTemperature = 0.1)
    solver.infer()
    solver.writeResults(outputStream)

  }

  def uniform(): (MRF, PrintStream) = {
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep
    val queryAtoms = Set[AtomSignature](AtomSignature("Heads", 1))
    val mln = MLN(path_prefix + "uniform.mln", path_prefix + "empty.db", queryAtoms, Set())
    info("Number of CNF clauses = " + mln.clauses.size)


    val kbmc = new MRFBuilder(mln)
    val mrf = kbmc.buildNetwork

    (mrf, System.out)
  }

  def binomial(): (MRF, PrintStream) = {
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep
    val queryAtoms = Set[AtomSignature](AtomSignature("Heads", 1))
    val mln = MLN(path_prefix + "binomial.mln", path_prefix + "empty.db", queryAtoms, Set())
    info("Number of CNF clauses = " + mln.clauses.size)


    val kbmc = new MRFBuilder(mln)
    val mrf = kbmc.buildNetwork

    (mrf, System.out)
  }

  def multinomial(): (MRF, PrintStream) = {
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep
    val queryAtoms = Set[AtomSignature](AtomSignature("Outcome", 2))
    val cwa = Set[AtomSignature](AtomSignature("Eq", 2))
    val mln = MLN(path_prefix + "multinomial.mln", path_prefix + "multinomial.db", queryAtoms, cwa)
    /*println("Clauses:")
    mln.clauses.foreach(c => println(c))
    println("--- --- --- ---")*/
    info("Number of CNF clauses = " + mln.clauses.size)


    val kbmc = new MRFBuilder(mln)
    val mrf = kbmc.buildNetwork

    //val output = new PrintStream(new FileOutputStream(path_prefix + "lomrf-multinomial.result"), true)
    //(mrf, output)
    (mrf, System.out)
  }

  def inertia1(): (MRF, PrintStream) = {
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep + "inertia_diagrams" + sep + "100points" + sep + "One" + sep
    val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
    val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Next", 2), AtomSignature("Initially", 1))
    info("Loading an MLN instance from data.")

    val mln = MLN(path_prefix + "example_si.mln", path_prefix + "evidence.db", queryAtoms, cwa)
    info("\n" + mln + "\n")
    info("Number of CNF clauses = " + mln.clauses.size)


    val kbmc = new MRFBuilder(mln)
    val mrf = kbmc.buildNetwork

    (mrf, System.out)
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
    val mrf = kbmc.buildNetwork

    (mrf, System.out)
  }

  def example2(): (MRF, PrintStream) = {
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep + "caviar" + sep + "video25_id6_id7_next_small" + sep
    val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
    val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1), AtomSignature("Next", 2))
    val owa = Set[AtomSignature](AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

    info("Loading an MLN instance from data.")
    implicit val mln = MLN(path_prefix + "dec7a.mln", path_prefix + "fra1gt_evidence.db", queryAtoms, cwa, owa)

    info("\n" + mln + "\n")
    info("Number of CNF clauses = " + mln.clauses.size)


    val kbmc = new MRFBuilder(mln)
    val mrf = kbmc.buildNetwork

    val output = new PrintStream(new FileOutputStream(path_prefix + "lomrf-output.result"), true)

    (mrf, output)
  }

  def example3(): (MRF, PrintStream) = {
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep + "caviar" + sep + "video25_id6_id7_next_small_neg" + sep
    val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
    val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1), AtomSignature("Next", 2))
    val owa = Set[AtomSignature](AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

    info("Loading an MLN instance from data.")
    implicit val mln = MLN(path_prefix + "dec7a.mln", path_prefix + "fra1gt_evidence.db", queryAtoms, cwa, owa)

    info("\n" + mln + "\n")
    info("Number of CNF clauses = " + mln.clauses.size)

    val kbmc = new MRFBuilder(mln)
    val mrf = kbmc.buildNetwork

    val output = new PrintStream(new FileOutputStream(path_prefix + "lomrf-output.result"), true)

    (mrf, output)
  }

  def example4(pcmMode: PredicateCompletionMode = Decomposed): (MRF, PrintStream) = {
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep + "caviar" + sep + "video25_id6_id7_next_small" + sep
    val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
    val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1), AtomSignature("Next", 2))
    val owa = Set[AtomSignature](AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))
    //val owa = Set[AtomSignature]()

    info("Loading an MLN instance from data.")
    implicit val mln = MLN(path_prefix + "dec7a_ext.mln", path_prefix + "fra1gt_evidence.db", queryAtoms, cwa, owa, pcm = pcmMode)

    info("\n" + mln + "\n")
    info("Number of CNF clauses = " + mln.clauses.size)

    val kbmc = new MRFBuilder(mln)
    val mrf = kbmc.buildNetwork

    val mode = pcmMode.toString.toLowerCase
    val output = new PrintStream(new FileOutputStream(path_prefix + "lomrf-output-" + mode + ".result"), true)

    (mrf, output)
  }

  def example5(pcmMode: PredicateCompletionMode = Decomposed): (MRF, PrintStream) = {
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep + "caviar" + sep + "video25_id6_id7_next_small" + sep
    val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
    val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("OrientationMove", 3), AtomSignature("StartTime", 1))
    val owa = Set[AtomSignature](AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))

    info("Loading an MLN instance from data.")
    implicit val mln = MLN(path_prefix + "dec7a_ext2.mln", path_prefix + "fra1gt_evidence_ext2.db", queryAtoms, cwa, owa, pcm = pcmMode)

    info("\n" + mln + "\n")
    info("Number of CNF clauses = " + mln.clauses.size)

    val kbmc = new MRFBuilder(mln)
    val mrf = kbmc.buildNetwork


    val mode = pcmMode.toString.toLowerCase
    val output = new PrintStream(new FileOutputStream(path_prefix + "lomrf-output-ext2-" + mode + ".result"), true)

    (mrf, output)
  }
}
