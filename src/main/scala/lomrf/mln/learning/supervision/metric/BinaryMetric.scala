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
    featureWeights: Option[Map[Feature, Int]] = None) extends StructureMetric[AtomicFormula] {

  /**
    * A reduced metric using only selected features for computing
    * the distance. Everything else is ignored.
    *
    * @note All weights should be either 0 or 1.
    *
    * @param weights a map from features to binary values
    * @return a weighted metric
    */
  override def havingWeights(weights: Map[Feature, Double]): StructureMetric[AtomicFormula] = {
    require(weights.forall { case (_, w) => w == 0 || w == 1 }, "All weights should be 0 or 1.")
    copy(featureWeights = Some(weights.mapValues(_.toInt)))
  }

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
