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
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */

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
