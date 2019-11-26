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

import breeze.linalg.DenseVector
import com.typesafe.scalalogging.LazyLogging
import lomrf.logic.{ AtomSignature, AtomicFormula }
import lomrf.mln.learning.supervision.graph.{ GraphConnector, Node }
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.metric.{ HybridMetric, Metric }
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

      /*node.atoms.filter(_.arity > 2).map(Feature.atom2Feature).toSet.foldLeft(temp) {
          case (map, atom) =>
            map.updated(atom, counts.getOrElse(atom, 0.0) + freq)
        }*/
    }

    //println(featureCounts.toList.sortBy(_._2).reverse.mkString("\n"))

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

      //println(f + " " + PFeature + " " + conditionalCounts.deep)

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

  def computeLaplacianScore_F(
      connector: GraphConnector,
      metric: Metric[_ <: AtomicFormula],
      nodes: IndexedSeq[Node]): Map[Feature, Double] = {

    require(nodes.forall(_.isLabeled))

    val features = nodes.flatMap(_.atoms.map(Feature.atom2Feature)).toSet

    val (s, d) = connector.fullyConnect(nodes)(metric.asInstanceOf[HybridMetric].metrics.head)
    val l = d - s
    val ones = DenseVector.ones[Double](nodes.length)

    var result = Map.empty[Feature, Double]
    features.foreach { f =>
      val fr = DenseVector(nodes.map(n => if (n.atoms.map(Feature.atom2Feature).contains(f)) 1.0 else 0.0).toArray)
      val fr_tilde = fr - ((fr.t * d * ones) / (ones.t * d * ones)) * ones
      val score = (fr_tilde.t * l * fr_tilde) / (fr_tilde.t * d * fr_tilde)
      result += f -> score
    }

    result
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

      def size(nodes1: Iterable[Node]): Double =
        nodes1.toList.map { n => if (cache.isDefined) cache.get.getOrElse(n, 0L).toDouble else 1.0 }.sum

    /*cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)*/
    /*nodes.map(n => n.atoms.map(Feature.atom2Feature).map(weights.getOrElse(_, 1.0)).sum / n.size *
        cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum*/

    val (positives, negatives) = nodes.partition(_.isPositive)
    val U_size = size(nodes)

      def c(X: Set[Node], Y: Set[Node]): Double =
        if (X.isEmpty) 0 else 1 - size(X intersect Y) / size(X) // TODO This may be a problem !!!

    val P_partition = nodes.foldLeft(Map.empty[IndexedSeq[Feature], Set[Node]]) {
      case (partitions, node) =>
        val key = node.atoms.map(Feature.atom2Feature).filter(P.contains).sortBy(_.toString)
        //val key = node.atoms.map(Feature.atom2Feature).toSet.intersect(P)
        if (key.isEmpty) partitions
        else partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
    }.values.toList

    /*println(P.mkString(" | "))
    println {
      P_partition.map(_.map(_.toText).mkString("\n")).mkString("\n+++++++++++++++++++++++\n")
    }
    println("++++++++++++++++++++")*/

    var acceptedP = 0
    var POSet = Set.empty[Node]
    P_partition.foreach { p =>
      if (c(p, positives.toSet) <= beta || c(p, negatives.toSet) <= beta) {
        POSet ++= p
        acceptedP += 1
      }
      // if (c(p, negatives.toSet) <= beta) POSet ++= p
    }

    //println(acceptedP / 2.0)

    size(POSet) / U_size
  }

  def computeLambdaDegree_F(
      beta: Double,
      P: Set[Feature],
      nodes: Seq[Node],
      cache: Option[NodeCache]): Double = {

    require(beta >= 0 && beta <= 0.5)
    require(nodes.forall(_.isLabeled))

      def size(nodes1: Iterable[Node]): Double =
        nodes1.toList.map { n => if (cache.isDefined) cache.get.getOrElse(n, 0L).toDouble else 1.0 }.sum

    val (positives, negatives) = nodes.partition(_.isPositive)

      def c(X: Set[Node], Y: Set[Node]): Double =
        if (X.isEmpty) 0 else 1 - size(X intersect Y) / size(X) // TODO This may be a problem !!!

    val P_partition = nodes.foldLeft(Map.empty[IndexedSeq[Feature], Set[Node]]) {
      case (partitions, node) =>
        val key = node.atoms.map(Feature.atom2Feature).filter(P.contains)
        //val key = node.atoms.map(Feature.atom2Feature).toSet.intersect(P)
        if (key.isEmpty) partitions
        else partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
    }.values.toList

    var acceptedP = 0
    P_partition.foreach { p =>
      if (c(p, positives.toSet) <= beta || c(p, negatives.toSet) <= beta) acceptedP += 1
    }
    acceptedP
  }

  def strictComputeBetaDependencyDegree_F(
      P: Set[Feature],
      nodes: Seq[Node],
      weights: Map[Feature, Double],
      cache: Option[NodeCache]): Double = {

    require(nodes.forall(_.isLabeled))

      def size(nodes: Iterable[Node]): Double = nodes.map(n => cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum
    /*nodes.map(n => n.atoms.map(Feature.atom2Feature).map(weights.getOrElse(_, 1.0)).sum / n.size *
      cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum*/

    val (positives, negatives) = nodes.partition(_.isPositive)
    val U_size = size(nodes)

      def c(X: Set[Node], Y: Set[Node]): Boolean = X.forall(Y.contains)

    val P_partition = nodes.foldLeft(Map.empty[IndexedSeq[Feature], Set[Node]]) {
      case (partitions, node) =>
        val key = node.atoms.map(Feature.atom2Feature).filter(P.contains)
        //val key = node.atoms.map(Feature.atom2Feature).toSet.intersect(P)
        partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
    }.values.toList

    /*println(P.mkString(" | "))
    println {
      P_partition.map(_.map(_.toText).mkString("\n")).mkString("\n+++++++++++++++++++++++\n")
    }
    println("++++++++++++++++++++")*/

    var POSet = Set.empty[Node]
    P_partition.foreach { p =>
      if (c(p, positives.toSet)) POSet ++= p
      if (c(p, negatives.toSet)) POSet ++= p
    }

    println(s"${size(POSet)} / $U_size")
    size(POSet) / U_size
  }

  def lower(
      beta: Double,
      P: Set[Feature],
      nodes: Seq[Node],
      cache: Option[NodeCache]): Set[Node] = {

    require(beta >= 0 && beta <= 0.5)

      def size(nodes: Iterable[Node]): Double =
        nodes.map(n => cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum

      def c(X: Set[Node], Y: Set[Node]): Double =
        if (X.isEmpty) 0 else 1 - size(X intersect Y) / size(X)

    val P_partition = nodes.foldLeft(Map.empty[IndexedSeq[Feature], Set[Node]]) {
      case (partitions, node) =>
        val key = node.atoms.map(Feature.atom2Feature).filter(P.contains)
        //val key = node.atoms.map(Feature.atom2Feature).toSet.intersect(P)
        partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
    }.values.toList

    P_partition.foldLeft(Set.empty[Node]) {
      case (l, p) =>
        if (c(p, nodes.toSet) <= beta) l ++ p
        else l
    }
  }

  def upper(
      beta: Double,
      P: Set[Feature],
      nodes: Seq[Node],
      cache: Option[NodeCache]): Set[Node] = {

    require(beta >= 0 && beta <= 0.5)

      def size(nodes: Iterable[Node]): Double =
        nodes.map(n => cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum

      def c(X: Set[Node], Y: Set[Node]): Double =
        if (X.isEmpty) 0 else 1 - size(X intersect Y) / size(X)

    val P_partition = nodes.foldLeft(Map.empty[IndexedSeq[Feature], Set[Node]]) {
      case (partitions, node) =>
        val key = node.atoms.map(Feature.atom2Feature).filter(P.contains)
        //val key = node.atoms.map(Feature.atom2Feature).toSet.intersect(P)
        partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
    }.values.toList

    P_partition.foldLeft(Set.empty[Node]) {
      case (l, p) =>
        if (c(p, nodes.toSet) < 1 - beta) l ++ p
        else l
    }
  }

  def bnd(
      beta: Double,
      P: Set[Feature],
      nodes: Seq[Node],
      cache: Option[NodeCache]): Set[Node] =
    upper(beta, P, nodes, cache) -- lower(beta, P, nodes, cache)

  def computeAccuracy_F(
      beta: Double,
      P: Set[Feature],
      nodes: Seq[Node],
      cache: Option[NodeCache]): Double = {

    require(beta >= 0 && beta <= 0.5)
    require(nodes.forall(_.isLabeled))

    val (positives, negatives) = nodes.partition(_.isPositive)

      def size(nodes: Iterable[Node]): Double =
        nodes.map(n => cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum

      def c(X: Set[Node], Y: Set[Node]): Double =
        if (X.isEmpty) 0 else 1 - size(X intersect Y) / size(X)

    val P_partition = nodes.foldLeft(Map.empty[IndexedSeq[Feature], Set[Node]]) {
      case (partitions, node) =>
        val key = node.atoms.map(Feature.atom2Feature).filter(P.contains)
        //val key = node.atoms.map(Feature.atom2Feature).toSet.intersect(P)
        partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
    }.values.toList

    var lowerSet = Set.empty[Node]
    P_partition.foreach { p =>
      if (c(p, positives.toSet) <= beta) lowerSet ++= p
      if (c(p, negatives.toSet) <= beta) lowerSet ++= p
    }

    var upperSet = Set.empty[Node]
    P_partition.foreach { p =>
      if (c(p, positives.toSet) <= 1 - beta) upperSet ++= p
      if (c(p, negatives.toSet) <= 1 - beta) upperSet ++= p
    }

    size(lowerSet) / size(upperSet)
  }

  def test(beta: Double, nodes: Seq[Node], cache: Option[NodeCache]): Set[Feature] = {
    val features = nodes.flatMap(_.atoms.map(Feature.atom2Feature).toSet).toSet

    val pF = nodes.filter(_.isPositive).flatMap(_.atoms.map(Feature.atom2Feature).toSet).toSet
    val nF = nodes.filter(_.isNegative).flatMap(_.atoms.map(Feature.atom2Feature).toSet).toSet

    if (pF.isEmpty || nF.isEmpty) return features

    val best = (2 to features.size).map { len =>
      val k = features.toList.combinations(len).withFilter(f => pF.intersect(f.toSet).nonEmpty && nF.intersect(f.toSet).nonEmpty).map {
        f =>
          val j = f -> wComputeBetaDependencyDegree_F(beta, f.toSet, nodes, Map.empty, cache)
          println(j)
          j
      }.maxBy(_._2)
      //println(k)
      k
    }.maxBy(_._2)._1.toSet

    best
  }

  def optimization(nodes: Seq[Node], cache: Option[NodeCache]): Set[Feature] = {

    require(nodes.forall(_.isLabeled))

    val weights = computeIG_F(nodes, cache)
    val inconsistency = weights.keySet.map(k => k -> inconsistency_F(Set(k), nodes, cache)).toMap

      def computeDegrees(
          P: Set[Feature],
          nodes: Seq[Node],
          cache: Option[NodeCache]): (Double, Double, Double, Double, Double) = {

          def size(nd: Iterable[Node]): Double =
            nd.toList.map { n => if (cache.isDefined) cache.get.getOrElse(n, 0L).toDouble else 1.0 }.sum
          //nd.size

          def c(X: Set[Node], Y: Set[Node]): Double =
            if (X.isEmpty) 0 else 1 - size(X intersect Y) / size(X)

        val (positives, negatives) = nodes.partition(_.isPositive)
        val U_size = size(nodes)
        val U_size_freq = nodes.toList.map(n => cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum

        var penalty = 0.0
        val P_partition = nodes.foldLeft(Map.empty[IndexedSeq[Feature], Set[Node]]) {
          case (partitions, node) =>
            val key = node.atoms.map(Feature.atom2Feature).filter(P.contains).sortBy(_.toString)
            if (key.isEmpty) {
              penalty += cache.map(_.getOrElse(node, 0L).toDouble).getOrElse(1.0)
              partitions
            } else partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
        }.values.toList

        var acceptedP = 0.0
        var POSet = Set.empty[Node]
        P_partition.foreach { p =>
          if (c(p, positives.toSet) == 0 || c(p, negatives.toSet) == 0) {
            POSet ++= p
            acceptedP += 1.0
          }
        }

        (penalty / U_size_freq, acceptedP / nodes.size, size(POSet) / U_size, P.map(weights(_)).sum / P.size, P.map(inconsistency(_)).sum / P.size)
      }

      def unitExist(P: Set[Feature], nodes: Seq[Node], cache: Option[NodeCache]): Boolean = {
        nodes.exists(n => n.atoms.map(Feature.atom2Feature).filter(P.contains).distinct.length == 1) // TODO this is buggy
      }

    val features = nodes.flatMap(_.atoms.map(Feature.atom2Feature).toSet).toSet
    val fullSig = wComputeBetaDependencyDegree_F(0, features, nodes, weights, cache)

    if (!nodes.exists(_.isPositive) || !nodes.exists(_.isNegative)) return features

    var paretoSet = Set.empty[(List[Feature], (Double, Double, Double, Double, Double, Double))]
    val outerResults = (2 to features.size /* TODO - 1*/ ).flatMap { len =>

      val results = features.toList.combinations(len) /*.withFilter { featureSubset =>
        !unitExist(featureSubset.toSet, nodes, cache)
      }*/ .map { featureSubset =>
          //println("SIG of " + featureSubset.toSet)
          val sig = 1 - (wComputeBetaDependencyDegree_F(0, features -- featureSubset.toSet, nodes, weights, cache) / fullSig)
          val (a, b, c, d, e) = computeDegrees(featureSubset.toSet, nodes, cache)
          featureSubset -> (a, b, c, d, e, sig)
        }.filter { case (_, (_, pt, degree, _, _, _)) => degree > 0.5 /*&& pt > 1 / nodes.length*/ }.toList

      /*results.foreach { case (f, (penalty, partitions, degree, meanMI, sig)) =>
        println(s"${f.mkString(", ")} | penalty: $penalty partitions: $partitions degree $degree mean_mi: $meanMI SIG: $sig")
      }*/

      if (results.isEmpty) None
      else {

        results.foreach {
          case entry @ (_, (a, b, c, d, e, f)) =>

            val updated = paretoSet + entry
            paretoSet = updated.filter {
              case (_, (aa, bb, cc, dd, ee, ff)) =>
                !updated.exists {
                  case (_, (x, y, z, w, q, r)) =>
                    x < aa && y < bb && (1 - z) < (1 - cc) && (1 - w) < (1 - dd) && (1 - q) < (1 - ee) && (1 - r) < (1 - ff)
                }
            }

          /*paretoSet = paretoSet.filter { case (_, (aa, bb, cc, dd)) =>
            (aa < a || bb < b || (1 - cc) < (1 - c) || (1 - dd) < (1 - d)) || (aa == a && bb == b && cc == c && dd == d)
          }

          val add = !paretoSet.exists { case (_, (aa, bb, cc, dd)) =>
            (aa < a && bb < b && (1 - cc) < (1 - c)) || (aa == a && bb == b && (1 - cc) < (1 - c)) ||
              (aa == a && bb < b && (1 - cc) == (1 - c)) || (aa < a && bb == b && (1 - cc) == (1 - c)) ||
              (aa == a && bb < b && (1 - cc) < (1 - c)) || (aa < a && bb == b && (1 - cc) < (1 - c)) ||
              (aa < a && bb < b && (1 - cc) == (1 - c))
          }

          if (add) paretoSet += entry*/
        }

        // TODO works for meet 1,2,3,4,5,6,10
        // TODO MEASURE PARTITION MASS INSTEAD OF JUST THE NUMBER OF PARTITIONS
        Some(results.map(x => x._1 -> (x._2._1 + x._2._2 + (1 - x._2._3) /*+ (1 - x._2._4)*/ + (1 - x._2._5) + (1 - x._2._6))).minBy(_._2))
        //val minPartitions = results.map(_._2._2).distinct.min
        //Some(results.filter(_._2._2 == minPartitions).minBy(_._2._1))
      }
    }

    println
    println("PARETO SET")
    paretoSet.toList.sortBy(_._2._4).reverse.foreach {
      case (f, (penalty, partitions, degree, meanMI, incon, sig)) =>
        println(s"${f.mkString(", ")} | penalty: $penalty partitions: $partitions degree $degree sig: $sig mean_mi: $meanMI incon: $incon")
    }

    outerResults.minBy(_._2)._1.toSet
    //val minPartitions = outerResults.map(_._2._2).distinct.min
    //outerResults.filter(_._2._2 == minPartitions).minBy(_._2._1)._1.toSet
  }

  def optimization_clusters(clusters: Set[Set[Node]], cache: Option[NodeCache]): Set[Feature] = {

    val nodes = clusters.flatten.toIndexedSeq
    require(nodes.forall(_.isLabeled))

    val weights = computeIG_F(nodes, cache)
    val inconsistency = weights.keySet.map(k => k -> inconsistency_F(Set(k), nodes, cache)).toMap

      def computeDegrees(P: Set[Feature]): (Double, Double, Double, Double, Double) = {

          def size(nd: Iterable[Node]): Double =
            nd.toList.map { n => if (cache.isDefined) cache.get.getOrElse(n, 0L).toDouble else 1.0 }.sum

          def c(X: Set[Node], Y: Set[Node]): Double =
            if (X.isEmpty) 0 else 1 - size(X intersect Y) / size(X)

        var U_size = size(nodes)
        val U_size_freq = nodes.toList.map(n => cache.map(_.getOrElse(n, 0L).toDouble).getOrElse(1.0)).sum

        var penalty = 0.0
        val P_partition = nodes.foldLeft(Map.empty[IndexedSeq[Feature], Set[Node]]) {
          case (partitions, node) =>
            val key = node.atoms.map(Feature.atom2Feature).filter(P.contains).sortBy(_.toString)
            if (key.isEmpty) {
              penalty += cache.map(_.getOrElse(node, 0L).toDouble).getOrElse(1.0)
              partitions
            } else partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
        }.values.toList

        var acceptedP = 0.0
        var POSet = Set.empty[Node]
        P_partition.foreach { p =>
          if (clusters.exists(cl => c(p, cl) == 0)) {
            POSet ++= p
            acceptedP += 1.0
          }
        }

        U_size = size(P_partition.flatten)

        (penalty / U_size_freq, acceptedP / U_size, size(POSet) / U_size, P.map(weights(_)).sum / P.size, P.map(inconsistency(_)).sum / P.size)
      }

    val features = nodes.flatMap(_.atoms.map(Feature.atom2Feature).toSet).toSet
    //val fullSig = wComputeBetaDependencyDegree_F(0, features, nodes, weights, cache)

    if (!nodes.exists(_.isPositive) || !nodes.exists(_.isNegative)) return features

    var paretoSet = Set.empty[(List[Feature], (Double, Double, Double, Double, Double, Double))]
    val outerResults = (2 to features.size).flatMap { len =>

      val results = features.toList.combinations(len).map { featureSubset =>
        //println("SIG of " + featureSubset.toSet)
        val sig = 1.0 ///1 - (wComputeBetaDependencyDegree_F(0, features -- featureSubset.toSet, nodes, weights, cache) / fullSig)
        val (a, b, c, d, e) = computeDegrees(featureSubset.toSet)
        featureSubset -> (a, b, c, d, e, sig)
      }.filter { case (_, (_, pt, degree, _, _, _)) => degree > 0.5 /*&& pt > 1 / nodes.length*/ }.toList

      results.foreach {
        case (f, (penalty, partitions, degree, meanMI, meanInc, sig)) =>
          println(s"${f.mkString(", ")} | penalty: $penalty partitions: $partitions degree $degree mean_mi: $meanMI  mean_ic $meanInc SIG: $sig")
      }

      if (results.isEmpty) None
      else {

        results.foreach {
          case entry @ (_, (a, b, c, d, e, f)) =>

            val updated = paretoSet + entry
            paretoSet = updated.filter {
              case (_, (aa, bb, cc, dd, ee, ff)) =>
                !updated.exists {
                  case (_, (x, y, z, w, q, r)) =>
                    x < aa && y < bb && (1 - z) < (1 - cc) && (1 - w) < (1 - dd) && (1 - q) < (1 - ee) && (1 - r) < (1 - ff)
                }
            }

          /*paretoSet = paretoSet.filter { case (_, (aa, bb, cc, dd)) =>
            (aa < a || bb < b || (1 - cc) < (1 - c) || (1 - dd) < (1 - d)) || (aa == a && bb == b && cc == c && dd == d)
          }

          val add = !paretoSet.exists { case (_, (aa, bb, cc, dd)) =>
            (aa < a && bb < b && (1 - cc) < (1 - c)) || (aa == a && bb == b && (1 - cc) < (1 - c)) ||
              (aa == a && bb < b && (1 - cc) == (1 - c)) || (aa < a && bb == b && (1 - cc) == (1 - c)) ||
              (aa == a && bb < b && (1 - cc) < (1 - c)) || (aa < a && bb == b && (1 - cc) < (1 - c)) ||
              (aa < a && bb < b && (1 - cc) == (1 - c))
          }

          if (add) paretoSet += entry*/
        }

        // TODO works for meet 1,2,3,4,5,6,10
        // TODO MEASURE PARTITION MASS INSTEAD OF JUST THE NUMBER OF PARTITIONS
        Some(results.map(x => x._1 -> (x._2._1 + x._2._2 + (1 - x._2._3) /*+ (1 - x._2._4)*/ /*+ (1 - x._2._5) + (1 - x._2._6)*/ )).minBy(_._2))
        //val minPartitions = results.map(_._2._2).distinct.min
        //Some(results.filter(_._2._2 == minPartitions).minBy(_._2._1))
      }
    }

    println
    println("PARETO SET")
    paretoSet.toList.sortBy(_._2._4).reverse.foreach {
      case (f, (penalty, partitions, degree, meanMI, incon, sig)) =>
        println(s"${f.mkString(", ")} | penalty: $penalty partitions: $partitions degree $degree sig: $sig mean_mi: $meanMI incon: $incon")
    }

    outerResults.minBy(_._2)._1.toSet
    //val minPartitions = outerResults.map(_._2._2).distinct.min
    //outerResults.filter(_._2._2 == minPartitions).minBy(_._2._1)._1.toSet
  }

  def inconsistency_F(P: Set[Feature], nodes: Seq[Node], cache: Option[NodeCache]): Double = {
    require(nodes.forall(_.isLabeled))

      def size(nd: Iterable[Node]): Double =
        nd.toList.map { n => if (cache.isDefined) cache.get.getOrElse(n, 0L).toDouble else 1.0 }.sum

    val P_partition = nodes.foldLeft(Map.empty[IndexedSeq[Feature], Set[Node]]) {
      case (partitions, node) =>
        val key = node.atoms.map(Feature.atom2Feature).filter(P.contains).sortBy(_.toString)
        if (key.isEmpty) partitions
        else partitions + (key -> (partitions.getOrElse(key, Set.empty[Node]) + node))
    }.values.toList

    /*P_partition.map { nodes =>
      val (p, n) = nodes.partition(_.isPositive)
      nodes.size - (if (p.size > n.size) p.size else n.size)
    }.sum / nodes.length.toDouble*/

    P_partition.map { nodes =>
      val (p, n) = nodes.partition(_.isPositive)
      size(nodes) - (if (size(p) > size(n)) size(p) else size(n))
    }.sum / size(nodes)
  }

  def roughSetFS(beta: Double, nodes: Seq[Node], cache: Option[NodeCache], weighted: Boolean = false): Set[Feature] = {

    val weights = computeIG_F(nodes, cache)
    var sorted = weights.toList.sortBy(_._2).reverse
    var features = weights.keySet
    val initialGamma = wComputeBetaDependencyDegree_F(beta, features, nodes, weights, None /*cache*/ )
    println(initialGamma)
    var nextGamma = 0.0
    var reduct = Set.empty[Feature]

    while (nextGamma <= initialGamma && features.nonEmpty) {

      val (f, g) = features.map { f =>
        /*println(f)
        println(wComputeBetaDependencyDegree_F(beta, reduct + f, nodes, weights, cache))
        println(invWComputeBetaDependencyDegree_F(beta, reduct + f, nodes, weights, cache))*/
        f -> wComputeBetaDependencyDegree_F(beta, reduct + f, nodes, weights, None /*cache*/ )
      }.maxBy(_._2)
      features -= f
      nextGamma = g
      println(f)
      println(nextGamma)
      println("==============")
      reduct += f
    }

    reduct
  }

  def core(nodes: IndexedSeq[Node]): Set[Feature] = {
    val (positives, negatives) = nodes.partition(_.isPositive)

    val matrix = positives.map { p =>
      negatives.map { n =>
        val a = p.atoms.map(Feature.atom2Feature).toSet
        val b = n.atoms.map(Feature.atom2Feature).toSet
        a.union(b) -- a.intersect(b)
      }
    }

    //println(matrix.map(_.mkString(" | ")).mkString("\n"))

    if (matrix.isEmpty || matrix.forall(_.isEmpty) || matrix.flatMap(_.filter(_.size == 1)).isEmpty) Set.empty[Feature]
    else matrix.flatMap(_.filter(_.size == 1)).reduce(_ ++ _)
  }

  def core2(nodes: IndexedSeq[Node]): Set[Feature] = {
    val (positives, negatives) = nodes.partition(_.isPositive)

    val matrix = positives.map { p =>
      negatives.map { n =>
        val a = p.atoms.map(Feature.atom2Feature).toSet
        val b = n.atoms.map(Feature.atom2Feature).toSet
        a.union(b) -- a.intersect(b)
      }
    }

    //println(matrix.map(_.mkString(" | ")).mkString("\n"))

    if (matrix.isEmpty || matrix.forall(_.isEmpty)) Set.empty[Feature]
    else {
      val m = matrix.minBy(_.size).size
      matrix.flatMap(_.filter(_.size == m)).reduce(_ ++ _)
    }
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

    if (prevScore == nextScore) return reduct

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

  def roughSet2(beta: Double, nodes: Seq[Node], cache: Option[NodeCache], weighted: Boolean = false): Set[Feature] = {

    val weights = computeIG_F(nodes, cache)
    var sorted = weights.toList.sortBy(_._2).reverse
    val features = weights.keySet
    var prevScore = wComputeBetaDependencyDegree_F(beta, features, nodes, weights, cache)

    if (features.size <= 2) return features

    //    println("-----------------------------")
    //    var set = Set.empty[Feature]
    //    sorted.map(_._1).foreach { f =>
    //      set += f
    //      val x = computeBetaDependencyDegree_F(0.1, set, nodes, cache)
    //      println(set -> x)
    //    }
    //    println("-----------------------------")

    var reduct = sorted.take(3).map(_._1).toSet
    var nextFeature = sorted.drop(2).take(1).head._1
    sorted = sorted.drop(3)
    var nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)

    println("Full score: " + prevScore)
    println(reduct)
    println("Next score: " + nextScore)

    if (prevScore > nextScore) return features

    while (sorted.nonEmpty && prevScore - nextScore <= 0.01 /*nextScore >= prevScore*/ ) { // SOFT COMPARISON: prevScore - nextScore <= 0.1
      prevScore = nextScore
      nextFeature = sorted.head._1
      reduct += sorted.head._1
      sorted = sorted.tail
      nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)
      println(reduct)
      println("Next score: " + nextScore)
      println(s"Subtraction: ${prevScore - nextScore}")
    }

    reduct - nextFeature
  }

  def roughSetLaplacian(beta: Double, nodes: IndexedSeq[Node], cache: Option[NodeCache], connector: GraphConnector, metric: Metric[_ <: AtomicFormula]): Set[Feature] = {

    val weights = computeLaplacianScore_F(connector, metric, nodes)
    var sorted = weights.toList.sortBy(_._2)
    val features = weights.keySet
    var prevScore = wComputeBetaDependencyDegree_F(beta, features, nodes, weights, cache)

    if (features.size <= 2) return features

    //    println("-----------------------------")
    //    var set = Set.empty[Feature]
    //    sorted.map(_._1).foreach { f =>
    //      set += f
    //      val x = computeBetaDependencyDegree_F(0.1, set, nodes, cache)
    //      println(set -> x)
    //    }
    //    println("-----------------------------")

    var reduct = sorted.take(3).map(_._1).toSet
    var nextFeature = sorted.drop(2).take(1).head._1
    sorted = sorted.drop(3)
    var nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)

    println("Full score: " + prevScore)
    println(reduct)
    println("Next score: " + nextScore)

    //if (prevScore == nextScore) return features

    while (sorted.nonEmpty && prevScore - nextScore <= 0.01 /*nextScore >= prevScore*/ ) { // SOFT COMPARISON: prevScore - nextScore <= 0.1
      prevScore = nextScore
      nextFeature = sorted.head._1
      reduct += sorted.head._1
      sorted = sorted.tail
      nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)
      println(reduct)
      println("Next score: " + nextScore)
      println(s"Subtraction: ${prevScore - nextScore}")
    }

    reduct - nextFeature
  }

  def roughSetGrouped(beta: Double, nodes: Seq[Node], cache: Option[NodeCache], weighted: Boolean = false): Set[Feature] = {

    val weights = computeIG_F(nodes, cache)
    var sorted = weights.groupBy(_._2).toList.sortBy(_._1).reverse.map(x => x._2.keySet)
    val features = weights.keySet
    var prevScore = wComputeBetaDependencyDegree_F(beta, features, nodes, weights, cache)

    if (sorted.size <= 2) return features

    var reduct = sorted.take(3).flatten.toSet
    var nextFeatures = sorted.drop(2).take(1).head
    sorted = sorted.drop(3)

    var nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)

    println("Full score: " + prevScore)
    println(reduct)
    println("Next score: " + nextScore)

    //if (prevScore == nextScore) return features

    while (sorted.nonEmpty && prevScore - nextScore <= 0.01 /*nextScore >= prevScore*/ ) { // SOFT COMPARISON: prevScore - nextScore <= 0.1
      prevScore = nextScore
      nextFeatures = sorted.head
      reduct ++= sorted.head
      sorted = sorted.tail
      nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)
      println(reduct)
      println("Next score: " + nextScore)
      println(s"Subtraction: ${prevScore - nextScore}")
    }

    reduct -- nextFeatures
  }

  def roughSetBF(beta: Double, nodes: Seq[Node], cache: Option[NodeCache], weighted: Boolean = false): Set[Feature] = {

    val weights = computeIG_F(nodes, cache)
    val features = weights.keySet

    val positives = nodes.withFilter(_.isPositive).map(_.atoms.map(Feature.atom2Feature))
    if (positives.isEmpty) return features

    val (pFeatures, rest) = features.partition(f => positives.exists(_.contains(f)))

    println(s"Positive features: $pFeatures")
    println(s"Rest: $rest")

    var sortedPFreatures = weights.toList.filter(x => pFeatures.contains(x._1)).sortBy(_._2).reverse
    var prevScore = wComputeBetaDependencyDegree_F(beta, pFeatures, nodes, weights, cache)

    println(s"ALL: $prevScore")
    pFeatures.foreach { f =>
      val s = wComputeBetaDependencyDegree_F(beta, pFeatures - f, nodes, weights, cache)
      println(s"REMOVING $f YIELDS $s")
    }

    var reduct = sortedPFreatures.dropRight(1).map(_._1).toSet
    var removed = sortedPFreatures.takeRight(1).head._1
    sortedPFreatures = sortedPFreatures.dropRight(1)
    var nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)

    println("Full score: " + prevScore)
    println(reduct)
    println("Reduct score: " + nextScore)

    if (nextScore != 0.0 && prevScore == nextScore) return features

    while (nextScore > prevScore) { // SOFT COMPARISON: prevScore - nextScore <= 0.1
      prevScore = nextScore
      removed = sortedPFreatures.takeRight(1).head._1
      reduct = sortedPFreatures.dropRight(1).map(_._1).toSet
      sortedPFreatures = sortedPFreatures.dropRight(1)
      nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)
      println(reduct)
      println("Reduct score: " + nextScore)
    }

    val be = reduct + removed

    var sortedRestFreatures = weights.toList.filter(x => rest.contains(x._1)).sortBy(_._2).reverse
    prevScore = wComputeBetaDependencyDegree_F(beta, be, nodes, weights, cache)

    reduct = be ++ sortedRestFreatures.take(1).map(_._1).toSet
    var nextFeature = sortedRestFreatures.take(1).head._1
    sortedRestFreatures = sortedRestFreatures.drop(1)
    nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)

    println("BE score: " + prevScore)
    println(reduct)
    println("Reduct score: " + nextScore)

    if (prevScore == nextScore) return be

    while (nextScore > prevScore) { // SOFT COMPARISON: prevScore - nextScore <= 0.1
      prevScore = nextScore
      nextFeature = sortedRestFreatures.take(1).head._1
      reduct = reduct ++ sortedRestFreatures.take(1).map(_._1).toSet
      sortedRestFreatures = sortedRestFreatures.drop(1)
      nextScore = wComputeBetaDependencyDegree_F(beta, reduct, nodes, weights, cache)
      println(reduct)
      println("Reduct score: " + nextScore)
    }

    reduct - nextFeature
  }
}

object FeatureStats {
  def empty: FeatureStats = new FeatureStats(0, 0, 0, Map.empty, Map.empty, Map.empty)
}
