package examples

import lomrf.logic.AtomSignature
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.model.MLN
import lomrf.mln.inference.MCSAT
import java.io.PrintStream
import lomrf.mln.model.mrf.MRF
import lomrf.util.Logging

object ExpDynamic extends Logging {
  val sep = System.getProperty("file.separator")

  def main(args: Array[String]){
    val (mrf, outputStream) = multinomial()
    val solver = new MCSAT(mrf, unitPropagation = true, samples = 1000, pBest = 0.5, lateSA = true, saTemperature = 0.1)
    solver.infer()
    solver.writeResults(outputStream)
  }

  def multinomial(): (MRF, PrintStream) ={
    val path_prefix = System.getProperty("user.dir") + sep + "data" + sep
    val queryAtoms = Set[AtomSignature](AtomSignature("Outcome", 2))
    val mln = MLN(path_prefix + "multinomial-dyn.mln", path_prefix + "empty.db", queryAtoms)
    info("Number of CNF clauses = " + mln.clauses.size)

    val kbmc = new MRFBuilder(mln)
    val mrf = kbmc.buildNetwork

    (mrf, System.out)
  }

}
