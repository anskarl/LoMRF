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
 * Copyright (C) 2012 Anastasios Skarlatidis.
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

import lomrf.mln.model.MLN
import lomrf.logic.AtomSignature
import java.io.FileWriter

/**
 * Command-line tool for knowledge base difference checking. In particular with this tool we can perform
 * difference checking given two knowledge bases.
 */
object KBDifferenceCLI extends CLIApp {

  private def compare(source: IndexedSeq[String], evidence: IndexedSeq[String], prefixOpt: Option[String]) {

    if (source.size != evidence.size)
      fatal("The number of input files and evidence files must be the same.")

    // Iterator[(Seq[Source, Evidence], Index)]
    val combinations = source.view.zip(evidence).combinations(2).zipWithIndex

    val queryAtoms = Set.empty[AtomSignature]

    val prefix = prefixOpt.map(_.trim) match {
      case Some(p) if p.nonEmpty => p + "-"
      case _ => ""
    }

    for {
      (combination, index) <- combinations
      (sourceAlpha, evidenceAlpha) = combination.head
      (sourceBeta, evidenceBeta) = combination.last
      mlnAlpha = MLN(sourceAlpha, queryAtoms, Some(evidenceAlpha))
      mlnBeta = MLN(sourceBeta, queryAtoms, Some(evidenceBeta))} {

      info(
        "\nSource KB 1: " + sourceAlpha + "\n" +
          "\tFound " + mlnAlpha.formulas.size + " formulas.\n" +
          "\tFound " + mlnAlpha.schema.predicateSchema.size + " predicates.\n" +
          "\tFound " + mlnAlpha.schema.functionSchema.size + " functions.\n" +
          "\nSource KB 2: " + sourceBeta + "\n" +
          "\tFound " + mlnBeta.formulas.size + " formulas.\n" +
          "\tFound " + mlnBeta.schema.predicateSchema.size + " predicates.\n" +
          "\tFound " + mlnBeta.schema.functionSchema.size + " functions.")

      val targetFileName = prefix +
        sourceAlpha.substring(0, sourceAlpha.lastIndexOf('.')) + "-" +
        sourceBeta.substring(0, sourceBeta.lastIndexOf('.'))

      val fileWriter = new FileWriter(targetFileName)

      val diff1 = mlnAlpha.clauses.par.filter(x => !mlnBeta.clauses.contains(x))
      val diff2 = mlnBeta.clauses.par.filter(x => !mlnAlpha.clauses.contains(x))

      if (diff1.nonEmpty) {
        fileWriter.write(s"KB 1 ($sourceAlpha) does not contain the following clauses:\n\n")
        diff1.seq.foreach(clause => fileWriter.write(clause.toString + "\n"))
      }

      if (diff2.nonEmpty) {
        fileWriter.write(s"KB 2 ($sourceBeta ) does not contain the following clauses:\n\n")
        diff2.seq.foreach(clause => fileWriter.write(clause.toString + "\n"))
      }

      if (diff1.isEmpty && diff2.isEmpty) info("KBs are exactly the same!")

      fileWriter.flush()
      fileWriter.close()

    }
  }

  var inputFileName: Option[IndexedSeq[String]] = None
  var evidenceFileName: Option[IndexedSeq[String]] = None
  var prefixOpt: Option[String] = None

  opt("i", "input", "<files>", "At least two comma separated input files", {
    v: String => if (v.nonEmpty) {
      val fileNames = v.split(',').map(_.trim)

      if (fileNames.length < 2)
        fatal("At least two input files are required, in order to perform difference operation.")

      inputFileName = Some(fileNames)
    }
  })

  opt("e", "evidence", "<db files>", "At least two comma separated evidence database files", {
    v: String => if (v.nonEmpty) {
      val fileNames = v.split(',').map(_.trim)

      if (fileNames.length < 2)
        fatal("At least two evidence files are required, in order to perform difference operation.")

      evidenceFileName = Some(fileNames)
    }
  })

  opt("p", "prefix", "<string>", "Prefix for the output difference files (<prefix>-input_filename_1-input_filename_2.diff)", {
    v: String => prefixOpt = Some(v.trim)
  })

  flagOpt("h", "help", "Print usage options.", {
    println(usage)
    sys.exit(0)
  })

  if (args.length == 0) println(usage)
  else if (parse(args)) {
    compare(
      inputFileName.getOrElse(fatal("Please define the input files.")),
      evidenceFileName.getOrElse(fatal("Please define the evidence files.")),
      prefixOpt
    )

  }

}
