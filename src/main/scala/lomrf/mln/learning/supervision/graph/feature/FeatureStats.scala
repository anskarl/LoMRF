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

package lomrf.mln.learning.supervision.graph.feature

import com.typesafe.scalalogging.LazyLogging
import lomrf.logic.{ AtomSignature, Constant }
import lomrf.mln.learning.supervision.graph.Node
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.util.logging.Implicits._

/**
  * @param signature an atom signature
  * @param constants a sequence of constants
  */
case class Feature(signature: AtomSignature, constants: Seq[Constant])

case class FeatureStats(
    examples: Long,
    positives: Long,
    negatives: Long,
    featureCounts: Map[AtomSignature, Double],
    featureCountsLabelled: Map[AtomSignature, Double],
    featureClassCounts: Map[AtomSignature, Array[Double]]) extends LazyLogging {

  private def selfInformation(p: Double): Double = if (p == 0) 0 else math.log10(p) / math.log10(2)

  private def entropy(prob: Double*): Double = prob.map(p => -p * selfInformation(p)).sum

  private def entropy(prob: Array[Double]): Double = prob.map(p => -p * selfInformation(p)).sum

  lazy val features: Set[AtomSignature] = featureCounts.keySet

  lazy val targetEntropy: Double = {
    val totalKnownExamples = positives + negatives
    val Pp = positives.toDouble / totalKnownExamples
    val Pn = negatives.toDouble / totalKnownExamples
    entropy(Pp, Pn)
  }

  lazy val featureEntropy: Map[AtomSignature, Double] = featureCounts.map {
    case (sig, counts) =>
      val prob = counts / examples
      sig -> entropy(prob, 1 - prob)
  }

  lazy val featureConditionalEntropy: Map[AtomSignature, Double] = features.map { f =>
    val ProbOfFeature = featureCountsLabelled(f) / (positives + negatives)
    val ProbOfNotFeature = 1 - ProbOfFeature

    val a = featureClassCounts(f).take(2).sum
    val b = featureClassCounts(f).drop(2).sum

    val EntropyOfFeature =
      if (a == 0 && b == 0) 0
      else if (a == 0) ProbOfNotFeature * entropy(featureClassCounts(f).drop(2).map(_ / featureClassCounts(f).drop(2).sum))
      else if (b == 0) ProbOfFeature * entropy(featureClassCounts(f).take(2).map(_ / featureClassCounts(f).take(2).sum))
      else ProbOfFeature * entropy(featureClassCounts(f).take(2).map(_ / featureClassCounts(f).take(2).sum)) +
        ProbOfNotFeature * entropy(featureClassCounts(f).drop(2).map(_ / featureClassCounts(f).drop(2).sum))

    f -> EntropyOfFeature
  }.toMap

  // X, Y | X, not Y | not X, Y | not X, not Y
  /*lazy val XXX = features.map { f =>
    val ProbOfFeature = featureCountsLabelled(f) / (positives + negatives)
    val ProbOfNotFeature = 1 - ProbOfFeature

    val suppX = featureClassCounts(f).take(2).sum
    val suppNotX = featureClassCounts(f).drop(2).sum
    val suppY = featureClassCounts(f).head + featureClassCounts(f)(2)
    val suppNotY = featureClassCounts(f)(1) + featureClassCounts(f)(3)

    val CCP_XY =

  }*/

  lazy val featureIG: Map[AtomSignature, Double] =
    featureConditionalEntropy.map { case (signature, h) => signature -> (targetEntropy - h) }

  lazy val uncertaintyCoeff: Map[AtomSignature, Double] =
    featureIG.map { case (f, ig) => f -> ig / featureEntropy(f) }

  lazy val uncertaintyCoeff2: Map[AtomSignature, Double] =
    featureIG.map { case (f, ig) => f -> featureEntropy(f) / ig }

  lazy val mul: Map[AtomSignature, Double] =
    featureIG.map { case (f, ig) => f -> featureEntropy(f) * ig }

  def +(f: Seq[AtomSignature]): FeatureStats =
    new FeatureStats(examples + 1, positives, negatives, f.foldLeft(featureCounts) {
      case (map, x) => map.updated(x, featureCounts.getOrElse(x, 0.0) + 1.0)
    }, featureCountsLabelled, featureClassCounts)

  def ++(nodes: Seq[Node]): FeatureStats = {

    var updatedCounts = featureCounts
    var updatedCountsLabelled = featureCountsLabelled
    nodes.foreach { node =>
      updatedCounts = node.atoms.foldLeft(updatedCounts) {
        case (map, atom) =>
          map.updated(atom.signature, updatedCounts.getOrElse(atom.signature, 0.0) + 1.0)
      }

      if (node.isLabeled) {
        updatedCountsLabelled = node.atoms.foldLeft(updatedCountsLabelled) {
          case (map, atom) =>
            map.updated(atom.signature, updatedCountsLabelled.getOrElse(atom.signature, 0.0) + 1.0)
        }
      }

    }

    val (labelled, _) = nodes.partition(_.isLabeled)

    val updatedPositives = positives + labelled.count(_.isPositive)
    val updatedNegatives = negatives + labelled.count(_.isNegative)
    val totalExamples = examples + nodes.length

    val currentFeatures = updatedCounts.keySet
    var updatedFeatureClassCounts = featureClassCounts
    currentFeatures.foreach { f =>
      if (!updatedFeatureClassCounts.contains(f))
        updatedFeatureClassCounts += f -> Array(0.0, 0.0, positives, negatives)

      if (!updatedCountsLabelled.contains(f))
        updatedCountsLabelled += f -> 0.0

      //if (updatedFeatureClassCounts.get(f).isEmpty)
      //updatedFeatureClassCounts += f -> Array.fill(4)(0.0)

      labelled.foreach { node =>
        if (node.atoms.exists(_.signature == f)) {
          if (node.isPositive) updatedFeatureClassCounts(f)(0) += 1
          else if (node.isNegative) updatedFeatureClassCounts(f)(1) += 1
        } else {
          if (node.isPositive) updatedFeatureClassCounts(f)(2) += 1
          else if (node.isNegative) updatedFeatureClassCounts(f)(3) += 1
        }
      }
    }

    //println(updatedCountsLabelled.mkString("\n"))
    //println(updatedFeatureClassCounts.mapValues(_.deep).mkString("\n"))

    new FeatureStats(totalExamples, updatedPositives, updatedNegatives, updatedCounts, updatedCountsLabelled, updatedFeatureClassCounts)
  }

  /**
    *
    * @param nodes
    * @return
    */
  def computeEntropy(nodes: Seq[Node]): Map[AtomSignature, Double] =
    nodes.foldLeft(Map.empty[AtomSignature, Double]) {
      case (counts, node) =>
        node.atoms.foldLeft(counts) {
          case (map, atom) =>
            map.updated(atom.signature, counts.getOrElse(atom.signature, 0.0) + 1.0)
        }
    }

  /**
    *
    * @param nodes
    * @return
    */
  def computeIG(nodes: Seq[Node], cache: Option[NodeCache] = None): Map[AtomSignature, Double] = {
    require(nodes.forall(_.isLabeled), logger.fatal("All nodes should be labeled in order to compute IG!"))

    val positives = nodes.withFilter(_.isPositive).map(n => cache.map(_.getOrElse(n, 0).toDouble).getOrElse(1.0)).sum
    val negatives = nodes.map(n => cache.map(_.getOrElse(n, 0).toDouble).getOrElse(1.0)).sum - positives
    val total = positives + negatives
    val targetEntropy = entropy(positives / total, negatives / total)

    val featureCounts = nodes.foldLeft(Map.empty[AtomSignature, Double]) {
      case (counts, node) =>
        val freq = cache.map(_.getOrElse(node, 0).toDouble).getOrElse(1.0)
        node.signatures.foldLeft(counts) {
          case (map, sig) =>
            map.updated(sig, counts.getOrElse(sig, 0.0) + freq)
        }
    }

    featureCounts.keySet.map { f =>
      val conditionalCounts = Array.fill(4)(0.0)
      nodes.foreach { node =>
        val freq = cache.map(_.getOrElse(node, 0).toDouble).getOrElse(1.0)
        if (node.signatures.exists(_ == f)) {
          if (node.isPositive) conditionalCounts(0) += freq
          else if (node.isNegative) conditionalCounts(1) += freq
        } else {
          if (node.isPositive) conditionalCounts(2) += freq
          else if (node.isNegative) conditionalCounts(3) += freq
        }
      }

      val PFeature = featureCounts(f) / total
      val PNotFeature = 1 - PFeature

      val a = conditionalCounts.take(2).sum
      val b = conditionalCounts.drop(2).sum

      val conditionalEntropy =
        if (a == 0 && b == 0) 0
        else if (a == 0) PNotFeature * entropy(conditionalCounts.drop(2).map(_ / b))
        else if (b == 0) PFeature * entropy(conditionalCounts.take(2).map(_ / a))
        else PFeature * entropy(conditionalCounts.take(2).map(_ / a)) +
          PNotFeature * entropy(conditionalCounts.drop(2).map(_ / b))

      f -> (targetEntropy - conditionalEntropy)
    }.toMap
  }

  def computeRedundancy(nodes: Seq[Node], cache: Option[NodeCache]): Map[AtomSignature, Map[AtomSignature, Double]] = {

    val featureCounts = nodes.foldLeft(Map.empty[AtomSignature, Double]) {
      case (counts, node) =>
        val freq = cache.map(_.getOrElse(node, 0).toDouble).getOrElse(1.0)
        node.signatures.foldLeft(counts) {
          case (map, sig) =>
            map.updated(sig, counts.getOrElse(sig, 0.0) + freq)
        }
    }

    val total = nodes.map(n => cache.map(_.getOrElse(n, 0).toDouble).getOrElse(1.0)).sum

    featureCounts.keys.toList.sortBy(_.symbol).map { f1 =>
      var map = Map.empty[AtomSignature, Double]

      val pf1 = featureCounts(f1) / total
      val targetEntropy = entropy(pf1, 1 - pf1)

      featureCounts.keys.toList.sortBy(_.symbol).filter(_ != f1).zipWithIndex.foreach {
        case (f2, i) =>

          val conditionalCounts = Array.fill(4)(0.0)
          nodes.foreach { node =>
            val freq = cache.map(_.getOrElse(node, 0).toDouble).getOrElse(1.0)
            if (node.signatures.contains(f2)) {
              if (node.signatures.contains(f1)) conditionalCounts(0) += freq
              else conditionalCounts(1) += freq
            } else {
              if (node.signatures.contains(f1)) conditionalCounts(2) += freq
              else conditionalCounts(3) += freq
            }
          }

          val PFeature = featureCounts(f2) / total
          val PNotFeature = 1 - PFeature

          val a = conditionalCounts.take(2).sum
          val b = conditionalCounts.drop(2).sum

          val conditionalEntropy =
            if (a == 0 && b == 0) 0
            else if (a == 0) PNotFeature * entropy(conditionalCounts.drop(2).map(_ / b))
            else if (b == 0) PFeature * entropy(conditionalCounts.take(2).map(_ / a))
            else PFeature * entropy(conditionalCounts.take(2).map(_ / a)) +
              PNotFeature * entropy(conditionalCounts.drop(2).map(_ / b))

          map += f2 -> (targetEntropy - conditionalEntropy)
      }

      f1 -> map
    }.toMap
  }

  def computeConstraintScore(nodes: Seq[Node]): Map[AtomSignature, Double] = ???
}

object FeatureStats {
  def empty: FeatureStats = new FeatureStats(0, 0, 0, Map.empty, Map.empty, Map.empty)
}
