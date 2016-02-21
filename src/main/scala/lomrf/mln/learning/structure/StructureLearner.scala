package lomrf.mln.learning.structure

import java.io.PrintStream
import java.text.DecimalFormat
import auxlib.log.Logging
import lomrf.logic.Clause
import lomrf.mln.model._

/**
 * Structure learner describes the specification of structure learning procedure
 * and should be extended by any structure learning algorithm implementation.
 */
trait StructureLearner extends Logging {

  // tolerance threshold for discarding poor clauses
  protected val tolerance: Double

  // knowledge base definition used for learning clauses
  protected val knowledgeBase: KB

  /**
   * @return a vector of all learned clauses
   */
  def getLearnedClauses: Vector[Clause]

  /**
   * @return a vector of all learned weights
   */
  def getLearnedWeights: Array[Double]

  /**
   * Should revise the current theory and return clauses learned for this
   * training evidence as a vector of clauses.
   *
   * @param trainingEvidence the training evidence (includes annotation)
   *
   * @return a vector of learned clauses for the given training evidence
   */
  def reviseTheory(trainingEvidence: TrainingEvidence): Vector[Clause]

  /**
   * Write all learned clauses and their corresponding learned weights in the given
   * output file together with the predicate and function schema if any exists.
   *
   * @param out the selected output stream (default is console)
   */
  def writeResults(out: PrintStream = System.out) = {

    val numFormat = new DecimalFormat("0.############")

    // Statistics about the resulting structure
    out.println(s"// -- Learned clauses: ${getLearnedClauses.length}")
    out.println(s"// -- Zero clauses: ${getLearnedWeights.count(_ == 0.0)}")
    out.println(s"// -- Tolerance: $tolerance")
    out.println("// -- Histogram:")
    out.println("// ===================")

    // Construct histogram
    val bins = Array.fill[Int](14)(0)
    getLearnedWeights.foreach { w =>
      if (w <= -3.0) bins(0) += 1
      else if (w <= -2.5) bins(1) += 1
      else if (w <= -2.0) bins(2) += 1
      else if (w <= -1.5) bins(3) += 1
      else if (w <= -1.0) bins(4) += 1
      else if (w <= -0.5) bins(5) += 1
      else if (w <= 0.0) bins(6) += 1
      else if (w <= 0.5) bins(7) += 1
      else if (w <= 1.0) bins(8) += 1
      else if (w <= 1.5) bins(9) += 1
      else if (w <= 2.0) bins(10) += 1
      else if (w <= 2.5) bins(11) += 1
      else if (w <= 3.0) bins(12) += 1
      else if (w > 3.0) bins(13) += 1
    }

    out.println(s"// (-inf, -3] : ${bins(0)}")
    out.println(s"// (-3, -2.5] : ${bins(1)}")
    out.println(s"// (-2.5, -2] : ${bins(2)}")
    out.println(s"// (-2, -1.5] : ${bins(3)}")
    out.println(s"// (-1.5, -1] : ${bins(4)}")
    out.println(s"// (-1, -0.5] : ${bins(5)}")
    out.println(s"// (-0.5, 0.0] : ${bins(6)}")
    out.println(s"// (0.0, 0.5] : ${bins(7)}")
    out.println(s"// (0.5, 1] : ${bins(8)}")
    out.println(s"// (1, 1.5] : ${bins(9)}")
    out.println(s"// (1.5, 2] : ${bins(10)}")
    out.println(s"// (2, 2.5] : ${bins(11)}")
    out.println(s"// (2.5, 3] : ${bins(12)}")
    out.println(s"// (3, +inf) : ${bins(13)}")
    out.println("// ===================")

    out.println("\n// Predicate definitions")
    for ((signature, args) <- knowledgeBase.predicateSchema if !signature.symbol.contains(lomrf.AUX_PRED_PREFIX)) {
      val line = signature.symbol + (
        if (args.isEmpty) "\n"
        else s"(${args.mkString(",")})\n")
      out.print(line)
    }

    if(knowledgeBase.functionSchema.nonEmpty) {
      out.println("\n// Functions definitions")
      for ((signature, (retType, args)) <- knowledgeBase.functionSchema) {
        val line = s"$retType ${signature.symbol}(${args.mkString(",")})\n"
        out.print(line)
      }
    }

    out.println("\n// Clauses")
    for(clauseIdx <- getLearnedClauses.indices) {
      if(getLearnedClauses(clauseIdx).isHard) out.println(s"${getLearnedClauses(clauseIdx).toText()}\n")
      else if (math.abs(getLearnedWeights(clauseIdx)) >= tolerance)
        out.println(s"${numFormat.format(getLearnedWeights(clauseIdx))} ${getLearnedClauses(clauseIdx).toText(weighted = false)}\n")
    }
  }
}
