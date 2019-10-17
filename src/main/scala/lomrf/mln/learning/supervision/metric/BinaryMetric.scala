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

package lomrf.mln.learning.supervision.metric

import lomrf.logic.AtomicFormula
import lomrf.mln.learning.supervision.metric.features.Feature

/**
  * A binary metric is a very simple distance for atomic formulas where an atom
  * has zero distance to another atom only if they are identical. Otherwise, their
  * distance is always one.
  *
  * @param matcher a matcher function
  */
case class BinaryMetric(
    matcher: Matcher,
    featureWeights: Option[Map[Feature, Double]] = None) extends StructureMetric[AtomicFormula] {

  /**
    * Normalize distance using the given feature importance weights.
    *
    * @note For features that do not exist in the map the given
    *       default value will be used.
    *
    * @param weights a map from features to weight values
    * @return a normalized metric
    */
  override def normalizeWith(weights: Map[Feature, Double]): StructureMetric[AtomicFormula] =
    copy(featureWeights = Some(weights))

  /**
    * Distance for atoms. The function must obey to the following properties:
    *
    * {{{
    * 1. d(x, y) >= 0 for all x, y and d(x, y) = 0 if and only if x = y
    * 2. d(x, y) = d(y, x) for all x, y
    * 3. d(x, y) + d(y, z) >= d(x, z) for all x, y, z (triangle inequality)
    * }}}
    *
    * @param xAtom an atom
    * @param yAtom another atom
    * @return a distance for the given atoms
    */
  override def distance(xAtom: AtomicFormula, yAtom: AtomicFormula): Double =
    if (xAtom.signature != yAtom.signature || xAtom.constants != yAtom.constants) 1 else 0
}
