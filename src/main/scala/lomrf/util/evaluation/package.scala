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

package lomrf.util

import lomrf.logic.{FALSE, TRUE, TriState}

package object evaluation {

  /**
    * A tuple composed of the counts for true positives, true negatives,
    * false positives and false negatives respectively.
    */
  type EvaluationStats = (Long, Long, Long, Long)

  private[evaluation] def combine(a: EvaluationStats, b: EvaluationStats): EvaluationStats =
    (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4)

  private[evaluation] def evaluateSingle(inferredState: Boolean, annotationState: TriState): EvaluationStats = {
    (annotationState, inferredState) match {
      case (TRUE, true) => (1, 0, 0, 0)
      case (FALSE, false) => (0, 1, 0, 0)
      case (FALSE, true) => (0, 0, 1, 0)
      case _ => (0, 0, 0, 1)
    }
  }
}
