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

package lomrf.util.evaluation

import java.io.PrintStream

object Metrics {

  /**
    * Report the results of metrics into a selected output stream.
    *
    * @param out output stream for results (default is console)
    */
  def report(evaluationStats: EvaluationStats, out: PrintStream = System.out): Unit = {

    val (tp, tn, fp, fn) = evaluationStats

    val accuracy = this.accuracy(tp, fp, tn, fn)
    val precision = this.precision(tp, fp, fn)
    val recall = this.recall(tp, fp, fn)
    val f1 = this.f1(tp, fp, fn)

    out.println(s"TP: $tp TN: $tn FP: $fp FN: $fn")
    out.println(s"Accuracy: $accuracy")
    out.println(s"Precision: $precision")
    out.println(s"Recall: $recall")
    out.println(s"F1-Score: $f1")
  }

  /**
    * Computation of precision based on true positives, false positives.
    * Precision is a measure of quality or exactness.
    *
    * @return the value of precision
    */
  def precision(tp: Long, fp: Long, fn: Long): Double = {
    val recognised = tp + fp
    val positives = tp + fn

    if (recognised == 0 && positives == 0) 1.0
    else if (recognised == 0 && positives > 0) 0.0
    else {
      val denominator = tp + fp
      assert(denominator > 0, "Precision: the denominator (tp + fp) is not > 0.")
      tp.toDouble / denominator
    }

  }

  /**
    * Computation of recall based on true positives and false negatives.
    * Recall is a measure of quantity or completeness
    *
    * @return the value of recall
    */
  def recall(tp: Long, fp: Long, fn: Long): Double = {
    val recognised = tp + fp
    val positives = tp + fn

    if (recognised == 0 && positives == 0) 1.0
    else if (recognised > 0 && positives == 0) 0.0
    else {
      val denominator = tp + fn
      assert(denominator > 0, "Recall/TPR: the denominator (tp + fn) is not > 0.")
      tp.toDouble / denominator
    }

  }

  /**
    * Computation of F-measure based on tp, fp, fn and a specified beta value.
    *
    * @param tp number of true positives
    * @param fp number of false positives
    * @param fn number of false negatives
    * @param beta controls the relative importance of recall versus precision
    *
    * @return the value of f-measure
    */
  def fMeasure(tp: Long, fp: Long, fn: Long, beta: Double): Double = {
    val recognised = tp + fp
    val positives = tp + fn

    if (recognised == 0 && positives == 0) 1.0
    else if ((recognised == 0 && positives > 0)
      || (positives == 0 && recognised > 0)
      || (positives > 0 && recognised > 0 && tp == 0)) 0.0
    else {
      val pr = precision(tp, fp, fn)
      val rec = recall(tp, fp, fn)
      val b2 = math.pow(beta, 2)
      val result = ((1 + b2) * (pr * rec)) / ((b2 * pr) + rec)

      assert(!result.isNaN, "F-measure (with beta = " + beta + ") cannot be NaN. prec: " + pr + " rec: " + rec)
      assert(!result.isInfinity, "F-measure (with beta = " + beta + ") cannot be Infinite.")

      result
    }
  }

  /**
    * Gives the F1 score, i.e., the harmonic mean of precision and recall, given the
    * number of true positives, false positives and false negatives.
    *
    * @param tp number of true positives
    * @param fp number of false positives
    * @param fn number of false negatives
    *
    * @return the calculated F1 score
    */
  def f1(tp: Long, fp: Long, fn: Long): Double = fMeasure(tp, fp, fn, 1.0)

  /**
    * Gives the F2 score, given the number of true positives, false positives and false negatives.
    * F2 score weights recall higher than precision.
    *
    * @param tp number of true positives
    * @param fp number of false positives
    * @param fn number of false negatives
    *
    * @return the calculated F2 score
    */
  def f2(tp: Long, fp: Long, fn: Long): Double = fMeasure(tp, fp, fn, 2.0)

  /**
    * Gives the F_{0.5} score, given the number of true positives, false positives and false negatives.
    * F_{0.5} score puts more emphasis on precision than recall.
    *
    * @param tp number of true positives
    * @param fp number of false positives
    * @param fn number of false negatives
    *
    * @return the calculated F_{0.5} score
    */
  def f05(tp: Long, fp: Long, fn: Long): Double = fMeasure(tp, fp, fn, 0.5)

  /**
    * Gives the true positive rate, with respect to the given true positives, false positives and false negatives.
    *
    * @param tp number of true positives
    * @param fp number of false positives
    * @param fn number of false negatives
    *
    * @note true positive rate is equal to recall (see [[Metrics#recall()]])
    *
    * @return the calculated true positive rate
    */
  def tpr(tp: Long, fp: Long, fn: Long): Double = recall(tp, fp, fn)

  /**
    * Gives the false positive rate, with respect to the given false positives and false negatives.
    *
    * @param fp number of false positives
    * @param tn number of true negatives
    *
    * @return the calculated false positive rate
    */
  def fpr(fp: Long, tn: Long): Double = {
    val denominator = fp + tn
    assert(denominator > 0, "FPR: the denominator (fp + tn) is not > 0.")
    fp.toDouble / denominator
  }

  /**
    * Gives the accuracy, with respect to the given true positives, false positives, true negatives  and false negatives.
    *
    * @param tp number of true positives
    * @param fp number of false positives
    * @param tn number of true negatives
    * @param fn number of false negatives
    *
    * @return the calculated accuracy
    */
  def accuracy(tp: Long, fp: Long, tn: Long, fn: Long): Double = {
    val denominator = tp + fp + tn + fn
    assert(denominator > 0, "Accuracy: the denominator (tp + fp + tn + fn) is not > 0.")
    (tp + tn).toDouble / denominator
  }

  /**
    * @param curve an iterable collection of points (x: Recall, y: Precision), representing a Precision-Recall curve
    * @return the area under Precision-Recall curve
    */
  def areaUnderPRC(curve: Iterable[(Double, Double)]): Double = areaUnder(curve, 0.0, 1.0, 1.0, 0.0)

  /**
    * @param curve an iterable collection of points (x: FPR, y: TPR), representing a Receiver operating characteristic curve.
    * @return the area under Receiver operating characteristic curve
    */
  def areaUnderCurve(curve: Iterable[(Double, Double)]): Double = areaUnder(curve, 0.0, 0.0, 1.0, 1.0)

  def areaUnder(curve: Iterable[(Double, Double)], x0: Double, y0: Double, xN: Double, yN: Double): Double = {

      @inline def calcArea(x1: Double, y1: Double, x2: Double, y2: Double): Double = (y1 + y2) * (x2 - x1) / 2.0

    var area = 0.0
    var xPrev = x0
    var yPrev = y0

    for ((x, y) <- curve) {
      area += calcArea(xPrev, yPrev, x, y)
      xPrev = x
      yPrev = y
    }

    area += calcArea(xPrev, yPrev, xN, yN)
    area
  }

}
