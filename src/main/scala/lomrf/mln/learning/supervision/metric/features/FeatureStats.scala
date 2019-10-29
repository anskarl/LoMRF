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

package lomrf.mln.learning.supervision.metric.features

import com.typesafe.scalalogging.LazyLogging
import lomrf.logic.{ AtomSignature, Constant }
import lomrf.mln.learning.supervision.graph.Node
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.util.logging.Implicits._

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

  def computeEntropy(nodes: Seq[Node]): Map[AtomSignature, Double] =
    nodes.foldLeft(Map.empty[AtomSignature, Double]) {
      case (counts, node) =>
        node.atoms.foldLeft(counts) {
          case (map, atom) =>
            map.updated(atom.signature, counts.getOrElse(atom.signature, 0.0) + 1.0)
        }
    }

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
        if (node.signatures.contains(f)) {
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

  def computeIG_F(nodes: Seq[Node], cache: Option[NodeCache] = None): Map[Feature, Double] = {
    require(nodes.forall(_.isLabeled), logger.fatal("All nodes should be labeled in order to compute IG!"))

    val positives = nodes.withFilter(_.isPositive).map(n => cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum
    val negatives = nodes.map(n => cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum - positives
    val total = positives + negatives
    val targetEntropy = entropy(positives / total, negatives / total)

    val featureCounts = nodes.foldLeft(Map.empty[Feature, Double]) {
      case (counts, node) =>
        val freq = cache.map(_.getOrElse(node, 0).toDouble).getOrElse(1.0)
        node.atoms.foldLeft(counts) {
          case (map, atom) =>
            map.updated(atom, counts.getOrElse(atom, 0.0) + freq)
        }
    }

    featureCounts.keySet.map { f =>
      val conditionalCounts = Array.fill(4)(0.0)
      nodes.foreach { node =>
        val freq = cache.map(_.getOrElse(node, 0).toDouble).getOrElse(1.0)
        if (node.atoms.map(Feature.atom2Feature).contains(f)) {
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

  def computeEntropy(nodes: Seq[Node], cache: Option[NodeCache] = None): Map[AtomSignature, Double] = {

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
      val pf1 = featureCounts(f1) / total
      f1 -> entropy(pf1, 1 - pf1)
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

      featureCounts.keys.toList.filter(_ != f1).sortBy(_.symbol).zipWithIndex.foreach {
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

  def computeRedundancy_F(nodes: Seq[Node], cache: Option[NodeCache]): Map[Feature, Map[Feature, Double]] = {

    val featureCounts = nodes.foldLeft(Map.empty[Feature, Double]) {
      case (counts, node) =>
        val freq = cache.map(_.getOrElse(node, 0).toDouble).getOrElse(1.0)
        node.atoms.foldLeft(counts) {
          case (map, f) =>
            map.updated(f, counts.getOrElse(f, 0.0) + freq)
        }
    }

    val total = nodes.map(n => cache.map(_.getOrElse(n, 0).toDouble).getOrElse(1.0)).sum

    featureCounts.keys.toList.sortBy(_.signature.symbol).map { f1 =>
      var map = Map.empty[Feature, Double]

      val pf1 = featureCounts(f1) / total
      val targetEntropy = entropy(pf1, 1 - pf1)

      featureCounts.keys.toList.filter(_ != f1).sortBy(_.signature.symbol).zipWithIndex.foreach {
        case (f2, i) =>

          val conditionalCounts = Array.fill(4)(0.0)
          nodes.foreach { node =>
            val freq = cache.map(_.getOrElse(node, 0).toDouble).getOrElse(1.0)
            if (node.atoms.map(Feature.atom2Feature).contains(f2)) {
              if (node.atoms.map(Feature.atom2Feature).contains(f1)) conditionalCounts(0) += freq
              else conditionalCounts(1) += freq
            } else {
              if (node.atoms.map(Feature.atom2Feature).contains(f1)) conditionalCounts(2) += freq
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

  def computeConstraintScore(nodes: Seq[Node], cache: Option[NodeCache]): Map[AtomSignature, Double] = {
    require(nodes.forall(_.isLabeled), logger.fatal("All nodes should be labeled in order to compute IG!"))

    val (positives, negatives) = nodes.partition(_.isPositive)

    nodes.flatMap(_.signatures).distinct.map { signature =>

      // MUST LINK
      var sum_a = 0L
      val a = positives.combinations(2).map { pair =>
        sum_a += cache.map(_.getOrElse(pair.head, 1L)).getOrElse(1L) * cache.map(_.getOrElse(pair.last, 1L)).getOrElse(1L)
        if (pair.head.signatures.contains(signature) == pair.last.signatures.contains(signature)) 0L
        else cache.map(_.getOrElse(pair.head, 1L)).getOrElse(1L) * cache.map(_.getOrElse(pair.last, 1L)).getOrElse(1L)
      }.sum

      var sum_b = 0L
      val b = negatives.combinations(2).map { pair =>
        sum_b += cache.map(_.getOrElse(pair.head, 1L)).getOrElse(1L) * cache.map(_.getOrElse(pair.last, 1L)).getOrElse(1L)
        if (pair.head.signatures.contains(signature) == pair.last.signatures.contains(signature)) 0L
        else cache.map(_.getOrElse(pair.head, 1L)).getOrElse(1L) * cache.map(_.getOrElse(pair.last, 1L)).getOrElse(1L)
      }.sum

      val ml_size = sum_a + sum_b

      // CANNOT LINK
      var sum_c = 0L
      val c = positives.map { x =>
        negatives.map { y =>
          // TODO use cache to compute the numbers
          val n = cache.map(_.getOrElse(x, 1L)).getOrElse(1L) * cache.map(_.getOrElse(y, 1L)).getOrElse(1L)
          sum_c += n
          if (x.signatures.contains(signature) == y.signatures.contains(signature)) 0L else n
        }.sum
      }.sum

      println(s"${signature}: ${a + b} / $ml_size // $c / $sum_c")

      if (sum_c == 0) {
        assert(c == 0)
        sum_c = 1L
      }

      //signature -> (((a + b) / ml_size.toDouble) / (c / sum_c.toDouble))
      signature -> (a + b) / c.toDouble
    }.toMap
  }

  def computeConstraintScore_F(nodes: Seq[Node], cache: Option[NodeCache]): Map[Feature, Double] = {
    require(nodes.forall(_.isLabeled), logger.fatal("All nodes should be labeled in order to compute IG!"))

    val (positives, negatives) = nodes.partition(_.isPositive)

    nodes.flatMap(_.atoms.map(Feature.atom2Feature)).distinct.map { feature =>

      // MUST LINK
      var sum_a = 0L
      val a = positives.combinations(2).map { pair =>
        sum_a += cache.map(_.getOrElse(pair.head, 1L)).getOrElse(1L) * cache.map(_.getOrElse(pair.last, 1L)).getOrElse(1L)
        if (pair.head.atoms.map(Feature.atom2Feature).contains(feature) == pair.last.atoms.map(Feature.atom2Feature).contains(feature)) 0L
        else cache.map(_.getOrElse(pair.head, 1L)).getOrElse(1L) * cache.map(_.getOrElse(pair.last, 1L)).getOrElse(1L)
      }.sum

      var sum_b = 0L
      val b = negatives.combinations(2).map { pair =>
        sum_b += cache.map(_.getOrElse(pair.head, 1L)).getOrElse(1L) * cache.map(_.getOrElse(pair.last, 1L)).getOrElse(1L)
        if (pair.head.atoms.map(Feature.atom2Feature).contains(feature) == pair.last.atoms.map(Feature.atom2Feature).contains(feature)) 0L
        else cache.map(_.getOrElse(pair.head, 1L)).getOrElse(1L) * cache.map(_.getOrElse(pair.last, 1L)).getOrElse(1L)
      }.sum

      val ml_size = sum_a + sum_b

      // CANNOT LINK
      var sum_c = 0L
      val c = positives.map { x =>
        negatives.map { y =>
          // TODO use cache to compute the numbers
          val n = cache.map(_.getOrElse(x, 1L)).getOrElse(1L) * cache.map(_.getOrElse(y, 1L)).getOrElse(1L)
          sum_c += n
          if (x.atoms.map(Feature.atom2Feature).contains(feature) == y.atoms.map(Feature.atom2Feature).contains(feature)) 0L else n
        }.sum
      }.sum

      //println(s"${feature}: ${a + b} / $ml_size // $c / $sum_c")

      if (sum_c == 0) {
        assert(c == 0)
        sum_c = 1L
      }

      //signature -> (((a + b) / ml_size.toDouble) / (c / sum_c.toDouble))
      feature -> (a + b) / c.toDouble
    }.toMap
  }

  def computeBetaDependencyDegree_F(
      beta: Double,
      P: Set[Feature],
      nodes: Seq[Node],
      cache: Option[NodeCache]): Double = {

    require(beta >= 0 && beta <= 0.5)
    require(nodes.forall(_.isLabeled))

      def size(nodes: Iterable[Node]): Double =
        nodes.map(n => cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum

    val (positives, negatives) = nodes.partition(_.isPositive)
    val U_size = size(nodes)

      def c(X: Set[Node], Y: Set[Node]): Double =
        if (X.isEmpty) 0 else 1 - size(X intersect Y) / size(X)

    val P_partition = nodes.foldLeft(Map.empty[Set[Feature], Set[Node]]) {
      case (partitions, node) =>
        val key = node.atoms.map(Feature.atom2Feature).toSet.intersect(P)
        partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
    }.values.toList

    /*println(P.mkString(" | "))
    println {
      P_partition.map(_.map(_.toText).mkString("\n")).mkString("\n+++++++++++++++++++++++\n")
    }
    println("++++++++++++++++++++")*/

    var POS = 0.0
    P_partition.foreach { p =>
      if (c(positives.toSet, p) <= beta) POS += size(p)
      if (c(negatives.toSet, p) <= beta) POS += size(p)
    }

    POS / U_size
  }

  def wComputeBetaDependencyDegree_F(
     beta: Double,
     P: Set[Feature],
     nodes: Seq[Node],
     weights: Map[Feature, Double],
     cache: Option[NodeCache]): Double = {

    require(beta >= 0 && beta <= 0.5)
    require(nodes.forall(_.isLabeled))

    def size(nodes: Iterable[Node]): Double =
      nodes.map(n => n.atoms.map(Feature.atom2Feature).map(weights.getOrElse(_, 1.0)).sum / n.size *
        cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum

    val (positives, negatives) = nodes.partition(_.isPositive)
    val U_size = size(nodes)

    def c(X: Set[Node], Y: Set[Node]): Double =
      if (X.isEmpty) 0 else 1 - size(X intersect Y) / size(X)

    val P_partition = nodes.foldLeft(Map.empty[Set[Feature], Set[Node]]) {
      case (partitions, node) =>
        val key = node.atoms.map(Feature.atom2Feature).toSet.intersect(P)
        partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
    }.values.toList

    /*println(P.mkString(" | "))
    println {
      P_partition.map(_.map(_.toText).mkString("\n")).mkString("\n+++++++++++++++++++++++\n")
    }
    println("++++++++++++++++++++")*/

    var POS = 0.0
    P_partition.foreach { p =>
      if (c(positives.toSet, p) <= beta) POS += size(p)
      if (c(negatives.toSet, p) <= beta) POS += size(p)
    }

    POS / U_size
  }

  def computeAccuracy_F(
      beta: Double,
      P: Set[Feature],
      nodes: Seq[Node],
      cache: Option[NodeCache]): Double = {

    require(beta >= 0 && beta <= 0.5)
    require(nodes.forall(_.isLabeled))

      def size(nodes: Iterable[Node]): Double =
        nodes.map(n => cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum

    val (positives, negatives) = nodes.partition(_.isPositive)
    val U_size = size(nodes)

      def c(X: Set[Node], Y: Set[Node]): Double =
        if (X.isEmpty) 0 else 1 - size(X intersect Y) / size(X)

    val P_partition = nodes.foldLeft(Map.empty[Set[Feature], Set[Node]]) {
      case (partitions, node) =>
        val key = node.atoms.map(Feature.atom2Feature).toSet.intersect(P)
        partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
    }.values.toList

    /*println(P.mkString(" | "))
    println {
      P_partition.map(_.map(_.toText).mkString("\n")).mkString("\n+++++++++++++++++++++++\n")
    }
    println("++++++++++++++++++++")*/

    var lower = 0.0
    P_partition.foreach { p =>
      if (c(positives.toSet, p) <= beta) lower += size(p)
      if (c(negatives.toSet, p) <= beta) lower += size(p)
    }

    println(s"Lower: $lower")

    var upper = 0.0
    P_partition.foreach { p =>
      if (c(positives.toSet, p) <= 1 - beta) upper += size(p)
      if (c(negatives.toSet, p) <= 1 - beta) upper += size(p)
    }

    println(s"Upper: $upper")

    lower / upper
  }

  def test(beta: Double, nodes: Seq[Node], cache: Option[NodeCache]): Set[Feature] = {
    val features = nodes.flatMap(_.atoms.map(Feature.atom2Feature).toSet).toSet

    val pF = nodes.filter(_.isPositive).flatMap(_.atoms.map(Feature.atom2Feature).toSet).toSet
    val nF = nodes.filter(_.isNegative).flatMap(_.atoms.map(Feature.atom2Feature).toSet).toSet

    val best = (2 to features.size).map { len =>
      val k = features.toList.combinations(len).withFilter(f => pF.intersect(f.toSet).nonEmpty && nF.intersect(f.toSet).nonEmpty).map {
        f =>
          val j = f -> computeBetaDependencyDegree_F(beta, f.toSet, nodes, cache)
          println(j)
          j
      }.maxBy(_._2)
      //println(k)
      k
    }.maxBy(_._2)._1.toSet

    best
  }

  def roughSet(beta: Double, nodes: Seq[Node], cache: Option[NodeCache], weighted: Boolean = false): Set[Feature] = {

    val weights = computeIG_F(nodes, cache)
    var sorted = weights.toList.sortBy(_._2).reverse
    val features = weights.keySet
    var prevScore = wComputeBetaDependencyDegree_F(beta, features, nodes, weights, cache)

    if (features.size < 2) return features

//    println("-----------------------------")
//    var set = Set.empty[Feature]
//    sorted.map(_._1).foreach { f =>
//      set += f
//      val x = computeBetaDependencyDegree_F(0.1, set, nodes, cache)
//      println(set -> x)
//    }
//    println("-----------------------------")

    var reduct = sorted.take(2).map(_._1).toSet
    var nextFeature = sorted.drop(1).take(1).head._1
    sorted = sorted.drop(2)
    var nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)

    println("Full score: " + prevScore)
    println(reduct)
    println("Next score: " + nextScore)

    if (prevScore == nextScore) return features

    while (nextScore > prevScore) { // SOFT COMPARISON: prevScore - nextScore <= 0.1
      prevScore = nextScore
      nextFeature = sorted.head._1
      reduct += sorted.head._1
      sorted = sorted.tail
      nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)
      println(reduct)
      println("Next score: " + nextScore)
    }

    reduct - nextFeature
  }
}

object FeatureStats {
  def empty: FeatureStats = new FeatureStats(0, 0, 0, Map.empty, Map.empty, Map.empty)
}
