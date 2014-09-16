package lomrf.app

import lomrf.util.{Logging, OptionParser}
import scala.Some
import lomrf.mln.model.MLN
import lomrf.logic.AtomSignature
import java.io.FileWriter

/**
 * Command-line tool for knowledge base difference checking. In particular with this tool we can perform
 * difference checking given two knowledge bases.
 *
 * @author Vagelis Michelioudakis
 */
object KBDifferenceCLI extends Logging {

  def main(args: Array[String]) {

    println(lomrf.ASCIILogo)
    println(lomrf.BuildVersion)

    val opt = new KBCOptions
    if (args.length == 0) println(opt.usage)
    else if (opt.parse(args)) {

      compile(
        opt.inputFileName.getOrElse(fatal("Please define the input files.")),
        opt.evidenceFileName.getOrElse(fatal("Please define the evidence files.")),
        opt.outputFileName.getOrElse(fatal("Please define the output file."))
      )

    }
  }

  def compile(source: Array[String], evidence: Array[String], target: String) {

    if(source.length != 2)
      fatal("Exactly two input files are required, in order to perform difference operation.")
    else if(source(0) == target || source(1) == target)
      fatal("Target file cannot be the same with either of the source files.")

    if(evidence.length != 2)
      fatal("Exactly two evidence files are required, in order to perform difference operation.")
    else if(evidence(0) == target || evidence(1) == target)
      fatal("Target file cannot be the same with either of the evidence files.")

    val mln_1 = MLN(source(0), evidence(0), Set[AtomSignature]())
    val mln_2 = MLN(source(1), evidence(1), Set[AtomSignature]())

    // TODO: Check CNF
    info("Checking CNF...")

    info(
      "\nSource KB: " + source(0) + "\n" +
        "\tFound " + mln_1.formulas.size + " formulas.\n" +
        "\tFound " + mln_1.schema.size + " predicates.\n" +
        "\tFound " + mln_1.functionSchema.size + " functions.\n" +
      "\nSource KB: " + source(1) + "\n" +
        "\tFound " + mln_2.formulas.size + " formulas.\n" +
        "\tFound " + mln_2.schema.size + " predicates.\n" +
        "\tFound " + mln_2.functionSchema.size + " functions.")

    val fileWriter = new FileWriter(target)
    var identical = true

    val diff2 = mln_2.clauses.par.filter(x=> !mln_1.clauses.contains(x))
    val diff1 = mln_1.clauses.par.filter(x=> !mln_2.clauses.contains(x))

    if(diff1.nonEmpty){
      fileWriter.write("KB 1 (" + source(0) + ") does not contain the following clauses:\n\n")
      diff1.seq.foreach(clause => fileWriter.write(clause.toString + "\n"))
    }

    if(diff2.nonEmpty){
      fileWriter.write("KB 2(" + source(1) + ") does not contain the following clauses:\n\n")
      diff2.seq.foreach(clause => fileWriter.write(clause.toString + "\n"))
    }


    /*fileWriter.write("KB 1 (" + source(0) + ") does not contain the following clauses:\n\n")
    for(clause <- mln_2.clauses) {
      if(!mln_1.clauses.contains(clause)) {
        fileWriter.write(clause.toString + "\n")
        identical = false
      }
    }*/
    /*fileWriter.write("\nKB 2 (" + source(1) + ") does not contain the following clauses:\n\n")
    for(clause <- mln_1.clauses) {
      if(!mln_2.clauses.contains(clause)) {
        fileWriter.write(clause.toString + "\n")
        identical = false
      }
    }*/
    fileWriter.flush()
    fileWriter.close()

    //if(identical) info("KBs are exactly the same!")
  }

  private class KBCOptions extends OptionParser {

    var inputFileName: Option[Array[String]] = None
    var evidenceFileName: Option[Array[String]] = None
    var outputFileName: Option[String] = None

    // TODO: Fix trim function
    opt("i", "input", "<files>", "Two comma separated input files", {
      v: String => if (!v.isEmpty) inputFileName = Some(v.split(',').map(_.trim))
    })

    opt("e", "evidence", "<db files>", "Two comma separated evidence database files", {
      v: String => if (!v.isEmpty) evidenceFileName = Some(v.split(',').map(_.trim))
    })

    opt("o", "output", "<diff file>", "Output difference file", {
      v: String => outputFileName = Some(v)
    })

    flagOpt("h", "help", "Print usage options.", {
      println(usage)
      sys.exit(0)
    })
  }

}