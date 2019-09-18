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

import lomrf.logic.{ AtomSignature, AtomicFormula }

final class MassMapMetric(
    signatures: Set[AtomSignature],
    table: Map[Set[AtomSignature], Double],
    size: Double) extends Metric[AtomicFormula] {

  private def expand(signatures: Set[AtomSignature]): Set[AtomSignature] =
    signatures ++ signatures.diff(signatures).map(s => AtomSignature(s"!${s.symbol}", s.arity))

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
  override def distance(xAtom: AtomicFormula, yAtom: AtomicFormula): Double =
    table(expand(Set(xAtom.signature, yAtom.signature))) / size

  /**
    * Distance over sequences of atoms.
    *
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @return a distance for the given sequences of atoms
    */
  override def distance(xAtomSeq: IndexedSeq[AtomicFormula], yAtomSeq: IndexedSeq[AtomicFormula]): Double = {
    val x = expand(xAtomSeq.map(_.signature).toSet).intersect(expand(yAtomSeq.map(_.signature).toSet))
    if (x.nonEmpty) table(x) / size
    else 1.0
  }

  /**
    * Append information from atom sequences to the metric.
    *
    * @note It should be extended by metrics that can
    *       exploit atom sequences (bottom clauses).
    *
    * @param atomSeqSeq a sequence of atom sequences.
    * @return an updated metric
    */
  override def ++(atomSeqSeq: Seq[Seq[AtomicFormula]]): Metric[AtomicFormula] = {
    var map = table

    for (seq <- atomSeqSeq) {
      val signatures = expand(seq.map(_.signature).toSet).toSeq

      for (n <- 1 to signatures.size) {
        signatures.combinations(n).foreach { combination =>
          val set = combination.toSet
          map += set -> (map(set) + 1)
        }
      }
    }
    new MassMapMetric(signatures, map, size + atomSeqSeq.length)
  }
}

object MassMapMetric {

  /**
    * Creates an empty mass metric, using an underlying hash map,
    * from a set of atom signatures.
    *
    * @param signatures a set of atom signatures
    * @return a MassMapMetric instance
    */
  def apply(signatures: Set[AtomSignature]): MassMapMetric = {

    var table = Map(signatures.map(s => AtomSignature(s"!${s.symbol}", s.arity)) -> 0.0)

    for (n <- 1 to signatures.size) {
      signatures.toSeq.combinations(n).foreach { seq =>

        val signaturesSet =
          seq.toSet ++ signatures.withFilter(s => !seq.contains(s)).map(s => AtomSignature(s"!${s.symbol}", s.arity))

        table += (signaturesSet -> 0.0)

        for (m <- 1 to signaturesSet.size) {
          signaturesSet.toSeq.combinations(m).foreach { seq =>
            table += (seq.toSet -> 0.0)
          }
        }
      }
    }

    new MassMapMetric(signatures, table, 0)
  }
}

case class MassTreeMetric(forest: IsolationForest[AtomSignature]) extends Metric[AtomicFormula] {

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
  override def distance(xAtom: AtomicFormula, yAtom: AtomicFormula): Double =
    forest.mass(Seq(xAtom.signature), Seq(yAtom.signature))

  /**
    * Distance over sequences of atoms.
    *
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @return a distance for the given sequences of atoms
    */
  override def distance(xAtomSeq: IndexedSeq[AtomicFormula], yAtomSeq: IndexedSeq[AtomicFormula]): Double =
    forest.mass(xAtomSeq.map(_.signature), yAtomSeq.map(_.signature))

  /**
    * Normalize distance based on the given feature scores.
    *
    * @param featureScores a map from atom signature to scores
    * @return a normalized metric
    */
  override def normalize(featureScores: Map[AtomSignature, Double]): Metric[AtomicFormula] =
    new MassTreeMetric(forest.reBalance(featureScores))

  /**
    * Append information from atom sequences to the metric.
    *
    * @note It should be extended by metrics that can
    *       exploit atom sequences (bottom clauses).
    *
    * @param atomSeqSeq a sequence of atom sequences.
    * @return an updated metric
    */
  override def ++(atomSeqSeq: Seq[Seq[AtomicFormula]]): Metric[AtomicFormula] = {
    atomSeqSeq.foreach(seq => forest.updateCounts(seq.map(_.signature)))
    new MassTreeMetric(forest)
  }
}

object MassTreeMetric {

  /**
    * @see [[lomrf.mln.learning.supervision.metric.IsolationForest]]
    *
    * @param trees a sequence of isolation trees
    * @return a MassTreeMetric instance
    */
  def apply(trees: Seq[IsolationTree[AtomSignature]]): MassTreeMetric =
    MassTreeMetric(IsolationForest(trees))

  /**
    * Creates an empty mass metric, using an underlying isolation forest,
    * from a set of atom signatures.
    *
    * @see [[lomrf.mln.learning.supervision.metric.IsolationForest]]
    *
    * @param signatures a set of atom signatures
    * @param numberOfTrees number of tree in the forest (default is 100)
    * @return a MassTreeMetric instance
    */
  def apply(signatures: Set[AtomSignature], numberOfTrees: Int = 100): MassTreeMetric =
    new MassTreeMetric(IsolationForest(signatures.toIndexedSeq, numberOfTrees))

  /**
    * Creates an empty mass metric, using an underlying isolation forest,
    * from a set of atom signatures.
    *
    * @see [[lomrf.mln.learning.supervision.metric.IsolationForest]]
    *
    * @param featureScores a map from atom signatures to scores
    * @return a MassTreeMetric instance
    */
  def apply(featureScores: Map[AtomSignature, Double]) =
    new MassTreeMetric(IsolationForest(featureScores))
}
