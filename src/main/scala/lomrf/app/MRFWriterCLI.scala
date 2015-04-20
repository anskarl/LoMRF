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

package lomrf.app

import java.io.{FileWriter, BufferedWriter}
import java.text.DecimalFormat
import auxlib.log.Logging
import auxlib.opt.OptionParser
import lomrf.logic.AtomSignature
import lomrf.logic.PredicateCompletionMode._
import lomrf.logic.dynamic.{DynamicFunctionBuilder, DynamicAtomBuilder}
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.model.MLN
import lomrf.mln.model.mrf.MRF
import lomrf.util._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Commandline tool for exporting ground MRF into various formats.
 *
 *
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

      val mln = opt.implPaths match {
        case Some(paths) =>
          val implFinder = ImplFinder(classOf[DynamicAtomBuilder], classOf[DynamicFunctionBuilder])
          implFinder.searchPaths(paths)
          MLN(strMLNFileName, opt.query, Some(strEvidenceFileName), opt.cwa, opt.owa, pcm = Decomposed, dynamicDefinitions = Some(implFinder.result))
        case None => MLN(strMLNFileName, opt.query, Some(strEvidenceFileName), opt.cwa, opt.owa, pcm = Decomposed)
      }


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

  def writeDIMACS(mrf: MRF, filepath: String) {
    val out = new BufferedWriter(new FileWriter(filepath))
    out.write("c\n")
    out.write("c weighted ground clauses\n")
    out.write("c\n")
    out.write("p wcnf " + mrf.numberOfAtoms + " " + mrf.numberOfConstraints)
    out.newLine()

    val constraintsIterator = mrf.constraints.iterator()

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

  def writeNetwork(mrf: MRF, filepath: String) {
    implicit val mln = mrf.mln
    val out = new BufferedWriter(new FileWriter(filepath))
    out.write("// weighted ground clauses\n")
    out.newLine()

    val constraintsIterator = mrf.constraints.iterator()
    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()
      //begin -- write weight value (if the feature is soft-constrained)
      if (!constraint.getWeight.isInfinite && !constraint.getWeight.isNaN && constraint.getWeight != mrf.weightHard) out.write(numFormat.format(constraint.getWeight) + " ")
      //write ground literals
      val clause = constraint.literals.map {
        literal =>
          decodeLiteral(literal) match {
            case Some(litTXT) => litTXT
            case None => sys.error("Cannot decode literal: " + literal)
          }
      }.reduceLeft(_ + " v " + _)
      out.write(clause)

      if (constraint.getWeight.isInfinite || constraint.getWeight == mrf.weightHard) out.write(".")
      //end -- change line
      out.newLine()
    }
    out.flush()
    out.close()
  }

  /**
   * Write as factor graph file format. For more details visit:
   * https://staff.fnwi.uva.nl/j.m.mooij/libDAI/doc/fileformats.html
   *
   * @param mrf input ground Markov Network
   * @param filePath output path
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
      val txtLiterals = constraint.literals.map {
        literal =>
          decodeLiteral(literal) match {
            case Some(litTXT) => litTXT
            case None => sys.error("Cannot decode literal: " + literal)
          }
      }.reduceLeft(_ + " v " + _)
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
      (0 until literals.length).foreach(_ => fgOutput.write("2 "))
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

    // Write the ground atoms in a separate file:
    val mOut = new BufferedWriter(new FileWriter(filePath + ".description"))
    mOut.write("# Ground atoms:")
    mOut.newLine()
    val atomsIterator = mrf.atoms.iterator()
    while (atomsIterator.hasNext) {
      atomsIterator.advance()
      val atomID = atomsIterator.key()
      decodeAtom(atomID) match {
        case Some(txtAtom) =>
          mOut.write((atomID - 1) + " " + txtAtom)
          mOut.newLine()
        case None => sys.error("Failed to decode atom id " + atomID + " (possible bug?).")
      }

    }
    mOut.flush()
    mOut.close()

  }

  private class MRFWOptions extends OptionParser {

    var mlnFileName: Option[String] = None
    var outputFileName: Option[String] = None
    var evidenceFileName: Option[String] = None

    var outputType: OutputFormatType = DIMACS

    var query = Set[AtomSignature]()
    var owa = Set[AtomSignature]()
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

    opt("q", "query", "<string>", "Comma separated query predicates", _.split(',').foreach(v => addQueryAtom(v)))

    opt("cwa", "closed-world-assumption", "<string>",
      "Specified non-evidence atoms (comma-separated with no space) are closed world, otherwise, all non-evidence atoms are open world.", _.split(",").foreach(v => addCWA(v)))

    opt("owa", "open-world-assumption", "<string>",
      "Specified evidence atoms (comma-separated with no space) are open world, while other evidence atoms are closed-world.", _.split(",").foreach(v => addOWA(v)))

    opt("e", "evidence", "<db file>", "Evidence database file", {
      v: String => evidenceFileName = Some(v)
    })

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
      query += parseAtomSignature(atom).getOrElse(fatal("Cannot parse the arity of query atom: " + atom))
    }

    private def addCWA(atom: String) {
      cwa += parseAtomSignature(atom).getOrElse(fatal("Cannot parse the arity of CWA atom: " + atom))
    }

    private def addOWA(atom: String) {
      owa += parseAtomSignature(atom).getOrElse(fatal("Cannot parse the arity of OWA atom: " + atom))
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
