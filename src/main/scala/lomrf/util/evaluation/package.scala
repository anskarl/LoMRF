package lomrf.util

import lomrf.logic.{FALSE, TRUE, TriState}


package object evaluation {

  /**
   * A tuple composed of the counts of true positives,
   * true negatives, false positives and false negatives
   */
  type EvaluationStats = (Long, Long, Long, Long)

  private[evaluation] def combine(a: EvaluationStats, b: EvaluationStats): EvaluationStats =
    (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4)

  private[evaluation] def evaluateSingle(inferredState: Boolean, annotationState: TriState ): EvaluationStats = {
    (annotationState, inferredState) match {
      case (TRUE, true) => (1, 0, 0, 0)
      case (FALSE, false) => (0, 1, 0, 0)
      case (FALSE, true) => (0, 0, 1, 0)
      case _ => (0, 0, 0, 1)
    }
  }

}
