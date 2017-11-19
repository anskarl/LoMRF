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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.app

import java.io.{FileWriter, BufferedWriter}
import java.text.DecimalFormat
import auxlib.log.Logging
import auxlib.opt.OptionParser
import lomrf.logic._
import lomrf.logic.AtomSignatureOps._
import lomrf.mln.model.AtomIdentityFunctionOps
import AtomIdentityFunctionOps._
import lomrf.logic.PredicateCompletionMode._
import lomrf.logic.dynamic.{DynamicFunctionBuilder, DynamicAtomBuilder}
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.model.MLN
import lomrf.mln.model.mrf.MRF
import lomrf.util._

import scala.collection.mutable.ArrayBuffer

/**
 * Command line tool for exporting ground MRF into various formats.
 */
object MRFWriterCLI extends Logging {

  private lazy val numFormat = new DecimalFormat("0.#########")

  def main(args: Array[String]) {
    println(lomrf.ASCIILogo)
    println(lomrf.BuildVersion)

    val opt = new MRFWOptions
    if (args.length == 0) print(opt.usage)
    else if (opt.parse(args)) {

      val strMLNFileName = opt.mlnFileName.getOrElse(fatal("Please specify an input MLN file."))
      val strEvidenceFileName = opt.evidenceFileName.getOrElse(fatal("Please specify an input evidence file."))

      info("Parameters:"
        + "\n\t(q) Query predicate(s): " + opt.query.map(_.toString).reduceLeft((left, right) => left + "," + right)
        + "\n\t(cwa) Closed-world assumption predicate(s): " + (if (opt.cwa.isEmpty) "empty" else opt.cwa.map(_.toString).mkString(","))
        + "\n\t(owa) Open-world assumption predicate(s): " + (if (opt.owa.isEmpty) "empty" else opt.owa.map(_.toString).mkString(","))
        + "\n\t(noNegWeights) Eliminate negative weights: " + opt._noNeg
        + "\n\t(noNegatedUnit) Eliminate negated ground unit clauses: " + opt._eliminateNegatedUnit
      )

      val mln = opt.implPaths match {
        case Some(paths) =>
          val implFinder = ImplFinder(classOf[DynamicAtomBuilder], classOf[DynamicFunctionBuilder])
          implFinder.searchPaths(paths)
          MLN.fromFile(strMLNFileName, opt.query, strEvidenceFileName, opt.cwa, opt.owa, pcm = Decomposed, dynamicDefinitions = Some(implFinder.result))
        case None => MLN.fromFile(strMLNFileName, opt.query, strEvidenceFileName, opt.cwa, opt.owa, pcm = Decomposed)
      }

      info(mln.toString)
      debug(mln.clauses.map(_.toText(weighted = true)).mkString("\n"))

      val outputFilePath = opt.outputFileName.getOrElse(fatal("Please specify an output file"))
      val builder = new MRFBuilder(mln = mln, noNegWeights = opt._noNeg, eliminateNegatedUnit = opt._eliminateNegatedUnit)
      val mrf = builder.buildNetwork
      opt.outputType match {
        case DIMACS => writeDIMACS(mrf, outputFilePath)
        case GROUND_CNF => writeNetwork(mrf, outputFilePath)
        case FACTOR_GRAPH => writeFactorGraph(mrf, outputFilePath)
      }

    }
  }

  /**
   * Write the MRF into the DIMACS format.
   *
   * @param mrf input ground Markov Network
   * @param filePath the output path
   */
  def writeDIMACS(mrf: MRF, filePath: String) {

    val out = new BufferedWriter(new FileWriter(filePath))

    // comment lines
    out.write("c\n")
    out.write("c weighted ground clauses\n")
    out.write("c\n")
    // p line indicating the exact number of variables and clauses contained in the file
    out.write("p wcnf " + mrf.numberOfAtoms + " " + mrf.numberOfConstraints)
    out.newLine()

    val constraintsIterator = mrf.constraints.iterator()

    /*
     * Each constraint is a sequence of distinct non-null numbers ending with 0 on the
     * same line; it cannot contain the opposite literals simultaneously. Positive numbers
     * denote the corresponding variables. Negative numbers denote the negations of the
     * corresponding variables.
     */
    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()
      out.write(
        numFormat.format(
          if (constraint.getWeight == Double.PositiveInfinity) mrf.weightHard else constraint.getWeight
        )
      )
      out.write(" ")
      out.write(constraint.literals.map(_.toString).reduceLeft(_ + " " + _))
      out.newLine()
    }
    out.flush()
    out.close()
  }

  /**
   * Write the MRF ground network.
   *
   * @param mrf input ground Markov Network
   * @param filePath the output path
   */
  def writeNetwork(mrf: MRF, filePath: String) {

    implicit val mln = mrf.mln
    val out = new BufferedWriter(new FileWriter(filePath))
    out.write("// weighted ground clauses\n")
    out.newLine()

    val constraintsIterator = mrf.constraints.iterator()
    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()

      // begin -- write weight value (if the feature is soft-constrained)
      if (!constraint.getWeight.isInfinite && !constraint.getWeight.isNaN && constraint.getWeight != mrf.weightHard)
        out.write(numFormat.format(constraint.getWeight) + " ")

      // write ground literals
      val clause = constraint.literals.map(l => l.decodeLiteral.getOrElse(sys.error("Cannot decode literal: " + l))).mkString(" v ")

      out.write(clause)

      if (constraint.getWeight.isInfinite || constraint.getWeight == mrf.weightHard) out.write(".")
      // end -- change line
      out.newLine()
    }
    out.flush()
    out.close()
  }

  /**
   * Write the MRF as factor graph file format.
   *
   * @param mrf input ground Markov Network
   * @param filePath output path
   *
   * @see https://staff.fnwi.uva.nl/j.m.mooij/libDAI/doc/fileformats.html
   */
  def writeFactorGraph(mrf: MRF, filePath: String) {

    implicit val mln = mrf.mln
    val fgOutput = new BufferedWriter(new FileWriter(filePath))
    fgOutput.write("# Factor graph")
    fgOutput.newLine()
    fgOutput.write(mrf.constraints.size().toString)
    fgOutput.newLine()

    val constraintsIterator = mrf.constraints.iterator()
    while (constraintsIterator.hasNext) {
      fgOutput.newLine()
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()
      val literals = constraint.literals
      val weight = constraint.getWeight
      require(!weight.isNaN && !weight.isInfinite)

      // write ground clause as comment for evaluation
      val txtLiterals = constraint.literals.map(l => l.decodeLiteral.getOrElse(sys.error("Cannot decode literal: " + l))).mkString(" v ")

      fgOutput.write("# " + (if (weight == mrf.weightHard) txtLiterals + "." else weight.toString + " " + txtLiterals))
      fgOutput.newLine()
      // write the number of variables of the factor
      fgOutput.write("# number of variables:")
      fgOutput.newLine()
      fgOutput.write(literals.length.toString)
      fgOutput.newLine()

      // write the variables
      fgOutput.write("# variables:")
      fgOutput.newLine()
      literals.foreach(lit => fgOutput.write((math.abs(lit) - 1).toString + " "))
      fgOutput.newLine()

      // write possible values, in our case all variables are simply binary
      fgOutput.write("# all variables are binary:")
      fgOutput.newLine()
      literals.indices.foreach(_ => fgOutput.write("2 "))
      fgOutput.newLine()

      // compute the factor table
      var nonZeroEntriesCounter = 0
      val entries = ArrayBuffer[String]()


      val occurrence: Array[Boolean] = literals.map(lit => lit > 0)
      val cartesianIterator = Cartesian.CartesianIterator(literals.map(_ => List(false, true)))

      var tableIndex = 0
      while (cartesianIterator.hasNext) {
        val stateEntry = cartesianIterator.next()

        stateEntry
          .view
          .zipWithIndex
          .find {
            case (state: Boolean, index: Int) => occurrence(index) == state
          } match {
            case Some(_) =>
              entries += tableIndex.toString + " " + weight
              nonZeroEntriesCounter += 1
            case None =>
              entries += "#" + tableIndex + " 0.0"
        }
        tableIndex += 1
      }

      // write the number of nonzero entries in the factor table
      fgOutput.write("# number of nonzero entries in the table:")
      fgOutput.newLine()
      fgOutput.write(nonZeroEntriesCounter.toString)
      fgOutput.newLine()

      // write the factor table
      fgOutput.write("# factor table:")
      fgOutput.newLine()

      for(entry <- entries){
        fgOutput.write(entry)
        fgOutput.newLine()
      }

      fgOutput.flush()
    }

    fgOutput.newLine()
    fgOutput.close()

    // write the ground atoms in a separate file:
    val mOut = new BufferedWriter(new FileWriter(filePath + ".description"))
    mOut.write("# Ground atoms:")
    mOut.newLine()
    val atomsIterator = mrf.atoms.iterator()
    while (atomsIterator.hasNext) {
      atomsIterator.advance()
      val atomID = atomsIterator.key()
      val txtAtom = atomID.decodeAtom.getOrElse(sys.error(s"Failed to decode atom id: '$atomID' (possible bug?)."))
      mOut.write((atomID - 1) + " " + txtAtom)
      mOut.newLine()
    }
    mOut.flush()
    mOut.close()

  }

  private class MRFWOptions extends OptionParser {

    // The path to the input MLN file
    var mlnFileName: Option[String] = None

    // The path to the output MLN file
    var outputFileName: Option[String] = None

    // Input evidence file(s) (path)
    var evidenceFileName: Option[String] = None

    // Output type for the MRF (default is DIMACS)
    var outputType: OutputFormatType = DIMACS

    // The set of query atoms (in the form of AtomName/Arity)
    var query = Set[AtomSignature]()

    //  The set of open-world assumption atoms (in the form of AtomName/Arity)
    var owa = Set[AtomSignature]()

    // The set of closed-world assumption atoms (in the form of AtomName/Arity)
    var cwa = Set[AtomSignature]()

    // Eliminate negative weights, i.e. convert the clause:
    // -2 A(x) v B(x)
    // into the following two clauses:
    // 1 !A(x)
    // 1 !B(x)
    var _noNeg = false

    // Eliminate negated unit clauses
    // For example:
    // 2 !A(x) becomes -2 A(x)
    var _eliminateNegatedUnit = false

    var implPaths: Option[Array[String]] = None

    opt("i", "input", "<mln filename>", "Input Markov Logic file", {
      v: String => mlnFileName = Some(v)
    })

    opt("e", "evidence", "<db file>", "Evidence database file", {
      v: String => evidenceFileName = Some(v)
    })

    opt("q", "query", "<string>", "Comma separated query predicates", _.split(',').foreach(v => addQueryAtom(v)))

    opt("cwa", "closed-world-assumption", "<string>",
      "Specified non-evidence atoms (comma-separated with no space) are closed world, otherwise, all non-evidence atoms are open world.", _.split(",").foreach(v => addCWA(v)))

    opt("owa", "open-world-assumption", "<string>",
      "Specified evidence atoms (comma-separated with no space) are open world, while other evidence atoms are closed-world.", _.split(",").foreach(v => addOWA(v)))

    opt("o", "output", "<output filename>", "Output filename", {
      v: String => outputFileName = Some(v)
    })

    opt("f", "format", "<format type>", "Output format type (DIMACS, GROUND_CNF, FACTOR_GRAPH)", {
      v: String => outputType = v.toUpperCase match {
        case "DIMACS" => DIMACS
        case "GROUND_CNF" => GROUND_CNF
        case "FACTOR_GRAPH" => FACTOR_GRAPH
        case _ => fatal("Unknown output format type")
      }
    })

    flagOpt("noNegWeights", "eliminate-negative-weights", "Eliminate negative weight values from ground clauses.", {
      _noNeg = true
    })

    flagOpt("noNegatedUnit", "eliminate-negated-unit", "Eliminate negated unit ground clauses.", {
      _eliminateNegatedUnit = true
    })

    opt("dynamic", "dynamic-implementations", "<string>", "Comma separated paths to search recursively for dynamic predicates/functions implementations (*.class and *.jar files).", {
      path: String => if (!path.isEmpty) implPaths = Some(path.split(','))
    })

    flagOpt("h", "help", "Print usage options.", {
      println(usage)
      sys.exit(0)
    })

    private def addQueryAtom(atom: String) {
      query += atom.signature.getOrElse(fatal("Cannot parse the arity of query atom: " + atom))
    }

    private def addCWA(atom: String) {
      cwa += atom.signature.getOrElse(fatal("Cannot parse the arity of CWA atom: " + atom))
    }

    private def addOWA(atom: String) {
      owa += atom.signature.getOrElse(fatal("Cannot parse the arity of OWA atom: " + atom))
    }
  }

  sealed trait OutputFormatType

  /**
   * Exports a DIMACS file with the resulting CNF clauses.
   */
  case object DIMACS extends OutputFormatType

  /**
   * Exports an MLN file with the resulting ground CNF clauses.
   */
  case object GROUND_CNF extends OutputFormatType

  /**
   * Exports a libDAI compatible factor-graph file, that represents the resulting ground MLN.
   *
   * @see [[http://cs.ru.nl/~jorism/libDAI/doc/fileformats.html#fileformats-factorgraph]] for details.
   */
  case object FACTOR_GRAPH extends OutputFormatType

}
