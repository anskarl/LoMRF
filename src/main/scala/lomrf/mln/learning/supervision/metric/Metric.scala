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
    * Append evidence information to the metric.
    *
    * @note It should be extended by metrics that can
    *       exploit evidence information.
    *
    * @param evidence an evidence database
    * @return an updated metric
    */
  def ++(evidence: Evidence): Metric[A] = this
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
  protected val matcher: Matcher

  /**
    * Distance over sequences of atoms.
    *
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @return a distance for the given sequences of atoms
    */
  final def distance(xAtomSeq: IndexedSeq[A], yAtomSeq: IndexedSeq[A]): Double = matcher {
    xAtomSeq map (x => yAtomSeq map (y => distance(x, y)))
  }
}

/**
  * A hybrid metric for atomic formulas. It combines multiple metrics by
  * averaging their corresponding distances.
  *
  * @param metrics a set of metrics to combine
  * @tparam A the type of atomic formula
  */
case class HybridMetric[A <: AtomicFormula](metrics: Set[Metric[A]]) extends Metric[A] {

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
  def distance(xAtom: A, yAtom: A): Double =
    metrics.foldLeft(0.0) { case (sum, metric) => sum + metric.distance(xAtom, yAtom) } / metrics.size

  /**
    * Distance over sequences of atoms.
    *
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @return a distance for the given sequences of atoms
    */
  def distance(xAtomSeq: IndexedSeq[A], yAtomSeq: IndexedSeq[A]): Double =
    metrics.foldLeft(0.0) { case (sum, metric) => sum + metric.distance(xAtomSeq, yAtomSeq) } / metrics.size
}
