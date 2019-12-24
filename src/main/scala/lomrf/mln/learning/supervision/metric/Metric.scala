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
import lomrf.mln.model.Evidence
import lomrf.mln.learning.supervision.metric.features.Feature

/**
  * A metric for atomic formulas is defined by a distance function over atoms
  * and a distance function over sequences of atoms.
  */
trait Metric[A <: AtomicFormula] {

  /**
    * @return the absolute normalized distance
    */
  protected def distance(x: Double, y: Double): Double =
    if (x == 0 && y == 0) 0
    else math.abs(x - y) / (x + y)

  /**
    * Distance for atoms. The function may obey to the following properties:
    *
    * {{{
    * 1. d(x, y) >= 0 for all x, y and d(x, y) = 0 if and only if x = y
    * 2. d(x, y) = d(y, x) for all x, y
    * 3. d(x, y) + d(y, z) >= d(x, z) for all x, y, z (triangle inequality)
    * }}}
    *
    * @see [[lomrf.logic.AtomicFormula]]
    * @param xAtom an atom
    * @param yAtom another atom
    * @return a distance for the given atoms
    */
  def distance(xAtom: A, yAtom: A): Double

  /**
    * Distance over sequences of atoms.
    *
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @return a distance for the given sequences of atoms
    */
  def distance(xAtomSeq: IndexedSeq[A], yAtomSeq: IndexedSeq[A]): Double

  /**
    * Weight metric using the given feature weights.
    *
    * @param weights a map from features to weight values
    * @return a weighted metric
    */
  def havingWeights(weights: Map[Feature, Double]): Metric[A] = this

  /**
    * Append evidence information to the metric.
    *
    * @note It should be extended by metrics that can
    *       exploit evidence information (data driven).
    *
    * @param evidence an evidence database
    * @return an updated metric
    */
  def ++(evidence: Evidence): Metric[A] = this

  /**
    * Append information from atom sequences to the metric.
    *
    * @note It should be extended by metrics that can
    *       exploit atom sequences (data driven).
    *
    * @param atomSeqSeq a sequence of atom sequences.
    * @return an updated metric
    */
  def ++(atomSeqSeq: Seq[Seq[AtomicFormula]]): Metric[A] = this
}

/**
  * A structure metric for atomic formulas is defined by a distance function over
  * atoms and a distance function over sequences of atoms by specifying a matcher.
  *
  * @note It should be extended by metrics that compare the structure of
  *       the given atomic formulas.
  *
  * @tparam A the type of atomic formula
  */
trait StructureMetric[A <: AtomicFormula] extends Metric[A] {

  // Matcher used for finding a mapping between atoms sequences
  val matcher: Matcher

  // Binary weights used for feature selection
  val featureWeights: Option[Map[Feature, Int]]

  /**
    * Distance over sequences of atoms.
    *
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @return a distance for the given sequences of atoms
    */
  final def distance(xAtomSeq: IndexedSeq[A], yAtomSeq: IndexedSeq[A]): Double = {

    // Swap atom sequences
    val (longAtomSeq, shortAtomSeq) =
      if (xAtomSeq.length >= yAtomSeq.length) (xAtomSeq, yAtomSeq)
      else (yAtomSeq, xAtomSeq)

    // Compute the distance matrix for each pair of atoms
    val distanceMatrix = longAtomSeq.map(x => shortAtomSeq.map(distance(x, _)))

    // Compute a matching and a total cost
    val (matches, unweightedDistance) = matcher(distanceMatrix)

    featureWeights match {
      case Some(weights) =>

        var totalScore = 0.0
        val num = longAtomSeq.zipWithIndex.map {
          case (atom, i) =>
            if (matches(i) == -1) {
              totalScore += weights.getOrElse(atom, 1)
              weights.getOrElse(atom, 1)
            } else {
              val bx = weights.getOrElse(atom, 1)
              val by = weights.getOrElse(shortAtomSeq(matches(i)), 1)
              totalScore += bx | by
              if (bx != by) 1 else (bx | by) * distanceMatrix(i)(matches(i))
            }
        }.sum

        if (totalScore == 0) 1 else num / totalScore

      case None => unweightedDistance
    }
  }
}

/**
  * A hybrid metric for atomic formulas. It combines multiple metrics by
  * averaging their corresponding distances.
  *
  * @param metrics a set of metrics to combine
  */
case class HybridMetric(metrics: Set[Metric[AtomicFormula]]) extends Metric[AtomicFormula] {

  lazy val numberOfMetrics: Int = metrics.size

  /**
    * Distance for atoms. The function may obey to the following properties:
    *
    * {{{
    * 1. d(x, y) >= 0 for all x, y and d(x, y) = 0 if and only if x = y
    * 2. d(x, y) = d(y, x) for all x, y
    * 3. d(x, y) + d(y, z) >= d(x, z) for all x, y, z (triangle inequality)
    * }}}
    *
    * @see [[lomrf.logic.AtomicFormula]]
    * @param xAtom an atom
    * @param yAtom another atom
    * @return a distance for the given atoms
    */
  def distance(xAtom: AtomicFormula, yAtom: AtomicFormula): Double =
    metrics.foldLeft(0.0) { case (sum, metric) => sum + metric.distance(xAtom, yAtom) } / numberOfMetrics

  /**
    * Distance over sequences of atoms.
    *
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @return a distance for the given sequences of atoms
    */
  def distance(xAtomSeq: IndexedSeq[AtomicFormula], yAtomSeq: IndexedSeq[AtomicFormula]): Double =
    metrics.foldLeft(0.0) { case (sum, metric) => sum + metric.distance(xAtomSeq, yAtomSeq) } / numberOfMetrics

  /**
    * Weight metric using the given feature weights.
    *
    * @param weights a map from features to weight values
    * @return a weighted metric
    */
  override def havingWeights(weights: Map[Feature, Double]): Metric[AtomicFormula] =
    HybridMetric(metrics.map(_ havingWeights weights))

  /**
    * Append evidence information to the metric.
    *
    * @note It should be extended by metrics that can
    *       exploit evidence information.
    *
    * @param evidence an evidence database
    * @return an updated metric
    */
  override def ++(evidence: Evidence): Metric[AtomicFormula] =
    HybridMetric(metrics.map(_ ++ evidence))

  /**
    * Append information from atom sequences to the metric.
    *
    * @note It should be extended by metrics that can
    *       exploit atom sequences (bottom clauses).
    *
    * @param atomSeqSeq a sequence of atom sequences.
    * @return an updated metric
    */
  override def ++(atomSeqSeq: Seq[Seq[AtomicFormula]]): Metric[AtomicFormula] =
    HybridMetric(metrics.map(_ ++ atomSeqSeq))
}

object HybridMetric {

  /**
    * @param metrics a sequence of metrics
    * @return a HybridMetric instance
    */
  def apply(metrics: Metric[AtomicFormula]*): HybridMetric = HybridMetric(metrics.toSet)
}
