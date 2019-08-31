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

package lomrf.app

import lomrf.mln.model.MLN
import lomrf.logic.AtomSignature
import java.io.PrintStream
import lomrf.util.logging.Implicits._
import scala.collection.parallel.CollectionConverters._

/**
  * Command line tool for knowledge base difference checking.
  */
object KBDifferenceCLI extends CLIApp {

  // Input file(s) (path)
  var inputFileName: Option[IndexedSeq[String]] = None

  // Evidence file(s) (path)
  var evidenceFileName: Option[IndexedSeq[String]] = None

  // Prefix for the output difference files
  var prefixOpt: Option[String] = None

  opt("i", "input", "<files>", "At least two comma separated input files", {
    v: String =>
      if (v.nonEmpty) {
        val fileNames = v.split(',').map(_.trim)

        if (fileNames.length < 2)
          logger.fatal("At least two input files are required in order to compute difference.")

        inputFileName = Some(fileNames)
      }
  })

  opt("e", "evidence", "<db files>", "At least two comma separated evidence database files", {
    v: String =>
      if (v.nonEmpty) {
        val fileNames = v.split(',').map(_.trim)

        if (fileNames.length < 2)
          logger.fatal("At least two evidence files are required in order to compute difference.")

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

  // Main:
  if (args.length == 0) println(usage)
  else if (parse(args)) compare(
    inputFileName.getOrElse(logger.fatal("Please define the input files.")),
    evidenceFileName.getOrElse(logger.fatal("Please define the evidence files.")),
    prefixOpt
  )

  private def compare(source: IndexedSeq[String], evidence: IndexedSeq[String], prefixOpt: Option[String]) {

    if (source.size != evidence.size)
      logger.fatal("The number of input files and evidence files must be the same.")

    val combinations = source.zip(evidence).combinations(2).zipWithIndex

    val prefix = prefixOpt.map(_.trim) match {
      case Some(p) if p.nonEmpty => p + "-"
      case _                     => ""
    }

    for {
      (combination, _) <- combinations
      (sourceAlpha, evidenceAlpha) = combination.head
      (sourceBeta, evidenceBeta) = combination.last
      mlnAlpha = MLN.fromFile(sourceAlpha, Set.empty[AtomSignature], evidenceAlpha)
      mlnBeta = MLN.fromFile(sourceBeta, Set.empty[AtomSignature], evidenceBeta)
    } {

      logger.info(s"\nSource KB 1: $sourceAlpha$mlnAlpha\nSource KB 2: $sourceBeta$mlnBeta")

      val targetFileName = prefix +
        sourceAlpha.substring(0, sourceAlpha.lastIndexOf('.')) + "-" +
        sourceBeta.substring(0, sourceBeta.lastIndexOf('.'))

      val output = new PrintStream(targetFileName)

      val diff1 = mlnAlpha.clauses.par.filter(!mlnBeta.clauses.contains(_))
      val diff2 = mlnBeta.clauses.par.filter(!mlnAlpha.clauses.contains(_))

      if (diff1.nonEmpty) {
        output.println(s"\nKB 1 ($sourceAlpha) does not contain the following clauses:\n")
        diff1.seq.foreach(clause => output.println(clause.toText()))
      }

      if (diff2.nonEmpty) {
        output.println(s"\nKB 2 ($sourceBeta) does not contain the following clauses:\n")
        diff2.seq.foreach(clause => output.println(clause.toText()))
      }

      if (diff1.isEmpty && diff2.isEmpty) logger.info("KBs are identical!")

      output.flush()
      output.close()
    }
  }
}
