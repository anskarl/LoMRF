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

case class MassMetric(forest: IsolationForest[Feature], usePriorityTrees: Boolean) extends Metric[AtomicFormula] {

  private lazy val hasSignatureFeaturesOnly: Boolean = forest.features.forall(_.constantArgs.isEmpty)

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
    * A priority mass metric that uses selected features higher on the isolation trees
    * in order to split important atoms earlier.
    *
    * @note All weights should be either 0 or 1.
    *
    * @param weights a map from features to binary values
    * @return a weighted metric
    */
  override def havingWeights(weights: Map[Feature, Double]): Metric[AtomicFormula] = {
    require(weights.forall { case (_, w) => w == 0 || w == 1 }, "All weights should be 0 or 1.")

    if (usePriorityTrees) {
      val tree = forest.trees.head
      val priorityForest = IsolationForest(forest.features, weights, forest.recall, forest.numberOfTrees)
      priorityForest.trees.foreach(_.updateCounts(tree))
      MassMetric(priorityForest, usePriorityTrees)
    } else this
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
    if (hasSignatureFeaturesOnly) // in case features use only signatures
      atomSeqSeq.foreach(seq => forest.updateCounts(seq.map(atom => Feature.fromAtomSignature(atom.signature))))
    else atomSeqSeq.foreach(seq => forest.updateCounts(seq.map(Feature.fromAtomicFormula)))
    MassMetric(forest, usePriorityTrees)
  }
}

object MassMetric {

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
    MassMetric(
      IsolationForest(signatures.map(Feature.fromAtomSignature).toIndexedSeq, numberOfTrees),
      usePriorityTrees = false
    )

  /**
    * Creates an empty mass metric, using an underlying isolation forest,
    * from a set of atom signatures and mode declarations.
    *
    * @note Signature trees do not make use of feature selection.
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

    MassMetric(
      IsolationForest(
        signatures.map(Feature.fromAtomSignature).toIndexedSeq, finiteRecall, numberOfTrees, finiteRecall.values.sum),
      usePriorityTrees = false
    )
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
    * @param usePriorityTrees create priority trees
    * @param maxHeight maximum number of height per tree (default is 10)
    * @return a MassTreeMetric instance
    */
  def apply(
      signatures: Set[AtomSignature],
      predicateSchema: PredicateSchema,
      constants: ConstantsDomain,
      modes: ModeDeclarations,
      numberOfTrees: Int,
      usePriorityTrees: Boolean,
      maxHeight: Int = 15): MassMetric = {

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
    MassMetric(IsolationForest(features.toIndexedSeq, finiteRecall, numberOfTrees, height), usePriorityTrees)
  }
}
