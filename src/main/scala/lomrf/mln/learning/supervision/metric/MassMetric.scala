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
import lomrf.mln.model.{ ConstantsDomain, ModeDeclarations, PredicateSchema }
import lomrf.util.Cartesian.CartesianIterator

case class MassMetric(forest: IsolationForest[Feature]) extends Metric[AtomicFormula] {

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
    forest.mass(Seq(xAtom), Seq(yAtom))

  /**
    * Distance over sequences of atoms.
    *
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @return a distance for the given sequences of atoms
    */
  override def distance(xAtomSeq: IndexedSeq[AtomicFormula], yAtomSeq: IndexedSeq[AtomicFormula]): Double =
    forest.mass(xAtomSeq.map(Feature.fromAtomicFormula), yAtomSeq.map(Feature.fromAtomicFormula))

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
    atomSeqSeq.foreach(seq => forest.updateCounts(seq.map(Feature.fromAtomicFormula)))
    new MassMetric(forest)
  }
}

object MassMetric {

  /**
    * @see [[lomrf.mln.learning.supervision.metric.IsolationForest]]
    *
    * @param trees a sequence of isolation trees
    * @return a MassTreeMetric instance
    */
  def apply(trees: Seq[IsolationTree[Feature]]): MassMetric =
    MassMetric(IsolationForest(trees))

  /**
    * Creates an empty mass metric, using an underlying isolation forest,
    * from a set of atom signatures.
    *
    * @see [[lomrf.mln.learning.supervision.metric.IsolationForest]]
    *
    * @param signatures a set of atom signatures
    * @param numberOfTrees number of tree in the forest
    * @return a MassTreeMetric instance
    */
  def apply(signatures: Set[AtomSignature], numberOfTrees: Int): MassMetric =
    new MassMetric(IsolationForest(signatures.map(Feature.fromAtomSignature).toIndexedSeq, numberOfTrees))

  /**
    * Creates an empty mass metric, using an underlying isolation forest,
    * from a set of atom signatures.
    *
    * @see [[lomrf.mln.learning.supervision.metric.IsolationForest]]
    *
    * @param featureScores a map from atom signatures to scores
    * @return a MassTreeMetric instance
    */
  def apply(featureScores: Map[Feature, Double]): MassMetric =
    new MassMetric(IsolationForest(featureScores))

  /**
    * Creates an empty mass metric, using an underlying isolation forest,
    * from a set of atom signatures and mode declarations.
    *
    * @see [[lomrf.mln.learning.supervision.metric.IsolationForest]]
    *
    * @param signatures a set of atom signatures
    * @param modes mode declarations
    * @param numberOfTrees number of tree in the forest
    * @return a MassTreeMetric instance
    */
  def apply(signatures: Set[AtomSignature], modes: ModeDeclarations, numberOfTrees: Int): MassMetric = {
    val maxRecall = modes
      .withFilter { case (_, mode) => mode.recall < Int.MaxValue }
      .map { case (_, mode) => mode.recall }.max

    val finiteRecall = modes.map {
      case (signature, mode) =>
        Feature.fromAtomSignature(signature) -> (if (mode.recall < Int.MaxValue) mode.recall else maxRecall)
    }

    new MassMetric(IsolationForest(
      signatures.map(Feature.fromAtomSignature).toIndexedSeq, finiteRecall, numberOfTrees, finiteRecall.values.sum))
  }

  /**
    * Creates an empty mass metric, using an underlying isolation forest,
    * from a set of atom signatures and mode declarations.
    *
    * @see [[lomrf.mln.learning.supervision.metric.IsolationForest]]
    *
    * @param signatures a set of atom signatures
    * @param predicateSchema a predicate schema
    * @param constants a constants domain
    * @param modes mode declarations
    * @param numberOfTrees number of tree in the forest
    * @param maxHeight maximum number of height per tree (default is 10)
    * @return a MassTreeMetric instance
    */
  def apply(
      signatures: Set[AtomSignature],
      predicateSchema: PredicateSchema,
      constants: ConstantsDomain,
      modes: ModeDeclarations,
      numberOfTrees: Int,
      maxHeight: Int = 10): MassMetric = {

    val maxRecall = modes
      .withFilter { case (_, mode) => mode.recall < Int.MaxValue }
      .map { case (_, mode) => mode.recall }.max

    var finiteRecall = Map.empty[Feature, Int]

    val features = signatures.flatMap { signature =>

      val domainConstants = modes(signature).placeMarkers.zip(predicateSchema(signature))
        .withFilter { case (placeMarker, _) => placeMarker.isConstant }
        .map { case (_, argDomain) => constants(argDomain) }

      if (domainConstants.forall(_.isEmpty)) {
        val feature = Feature.fromAtomSignature(signature)
        val signatureRecall = modes(signature).recall
        finiteRecall += feature -> (if (signatureRecall < Int.MaxValue) signatureRecall else maxRecall)
        IndexedSeq(feature)
      } else {
        val constantsCombinations = CartesianIterator(domainConstants)
        constantsCombinations.map { c =>
          val feature = Feature(signature, c.toSet)
          val signatureRecall = modes(signature).recall
          finiteRecall += feature -> (if (signatureRecall < Int.MaxValue) signatureRecall else maxRecall)
          feature
        }.toIndexedSeq
      }
    }

    val height = math.min(maxHeight, finiteRecall.values.sum)
    new MassMetric(IsolationForest(features.toIndexedSeq, finiteRecall, numberOfTrees, height))
  }
}
