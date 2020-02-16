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

package lomrf.mln.learning.supervision.graph

import lomrf.logic._
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.graph.selection.{ Clustering, LargeMarginNN }
import lomrf.mln.learning.supervision.metric._
import lomrf.mln.model._
import lomrf.util.time._
import scala.language.existentials
import lomrf.logic.LogicOps._
import lomrf.mln.model.builders.EvidenceBuilder

/**
  * NN graph represents a nearest neighbor graph having nodes for a given query signature. These
  * nodes contain a single ground query atom and a sequence of evidence atoms sharing
  * constants to the corresponding query atom. Nodes can be either labeled (the ground query
  * atom is TRUE or FALSE) or unlabeled. The graph is connected using a specified connector
  * strategy and can be solved in order to label the unlabeled ground query atoms.
  *
  * @param nodes an indexed sequence of nodes. Labeled nodes appear before unlabelled
  * @param querySignature the query signature of interest
  * @param connector a graph connector
  * @param metric a metric for atomic formula
  * @param supervisionBuilder a supervision evidence builder that contains the completed annotation
  * @param nodeCache a node cache for storing labelled nodes
  * @param minNodeSize minimum node size
  * @param minNodeOcc minimum node occurrences
  * @param enableClusters enables clustering of unlabeled examples
  * @param enableSelection enables feature selection
  * @param enableHardSelection enables hard feature selection
  * @param maxDensity clusters maximum density
  */
final class ExtNNGraph private[graph] (
    nodes: IndexedSeq[Node],
    querySignature: AtomSignature,
    connector: GraphConnector,
    metric: Metric[_ <: AtomicFormula],
    supervisionBuilder: EvidenceBuilder,
    nodeCache: NodeCache,
    minNodeSize: Int,
    minNodeOcc: Int,
    enableClusters: Boolean,
    enableSelection: Boolean,
    enableHardSelection: Boolean,
    maxDensity: Double)
  extends SupervisionGraph(nodes, querySignature, connector, metric, supervisionBuilder, nodeCache) {

  protected def optimize(potentials: Map[EvidenceAtom, Double]): Set[EvidenceAtom] = {

    logger.info {
      s"""
         |Supervision graph has $numberOfNodes nodes:
         |\t- Labeled Nodes: $numberOfLabeled
         |\t- Unlabeled Nodes: $numberOfUnlabeled
         |\t- Query Signature: $querySignature
      """.stripMargin
    }

    val startGraphConnection = System.currentTimeMillis
    val encodedGraph = FullConnector.fullyConnect(nodes)(metric)
    val W = encodedGraph._1
    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis

    val indexedNodes = nodes.zipWithIndex
    val (indexedLabeled, indexedUnlabeled) = indexedNodes.partition { case (node, _) => node.isLabeled }
    val (indexedPos, indexedNeg) = indexedLabeled.partition { case (node, _) => node.isPositive }
    val P = indexedPos.length
    val N = indexedNeg.length

    val labeledEvidenceAtoms = indexedUnlabeled.flatMap {
      case (node, j) =>

        logger.debug(s"Labeling node: ${node.toText}")

        val n = node.toNegative
        val p = node.toPositive

        // Generalized class-wise statistic for negative class when node is assumed to be negative
        val Tnn = (indexedNeg :+ (n, j)).map {
          case (_, i) =>
            val nearest = connector.makeSparse(W(i, ::).t(0 until numberOfLabeled).toArray :+ W(i, j)).toArray

            nearest.zip(labeledNodes :+ n).filter {
              case (w, x) => w > 0 && x.isNegative
            }.foldLeft(0.0)(_ + _._1) / ((N + 1) * nearest.count(_ > 0))
        }.sum

        // Generalized class-wise statistic for negative class when node is assumed to be positive
        val Tpn = indexedPos.map {
          case (_, i) =>
            val nearest = connector.makeSparse(W(i, ::).t(0 until numberOfLabeled).toArray :+ W(i, j)).toArray

            nearest.zip(labeledNodes :+ n).filter {
              case (w, x) => w > 0 && x.isPositive
            }.foldLeft(0.0)(_ + _._1) / (P * nearest.count(_ > 0)).toDouble
        }.sum

        // Generalized class-wise statistic for positive class when node is assumed to be positive
        val Tpp = (indexedPos :+ (p, j)).map {
          case (_, i) =>
            val nearest = connector.makeSparse(W(i, ::).t(0 until numberOfLabeled).toArray :+ W(i, j)).toArray

            nearest.zip(labeledNodes :+ p).filter {
              case (w, x) => w > 0 && x.isPositive
            }.foldLeft(0.0)(_ + _._1) / ((P + 1) * nearest.count(_ > 0)).toDouble
        }.sum

        // Generalized class-wise statistic for positive class when node is assumed to be negative
        val Tnp = indexedNeg.map {
          case (_, i) =>
            val nearest = connector.makeSparse(W(i, ::).t(0 until numberOfLabeled).toArray :+ W(i, j)).toArray

            nearest.zip(labeledNodes :+ p).filter {
              case (w, x) => w > 0 && x.isNegative
            }.foldLeft(0.0)(_ + _._1) / (N * nearest.count(_ > 0)).toDouble
        }.sum

        logger.debug(s"Tnn: $Tnn Tpn: $Tpn Tnp: $Tnp Tpp: $Tpp")

        if (Tnn + Tpn >= Tnp + Tpp) node.labelUsingValue(false)
        else node.labelUsingValue(true)
    }

    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    supervisionBuilder.evidence ++= labeledEvidenceAtoms
    labeledEvidenceAtoms.toSet
  }

  def ++(mln: MLN, annotationDB: EvidenceDB, modes: ModeDeclarations): ExtNNGraph = {

    // Group the given data into nodes, using the domains of the existing graph
    val currentNodes = connector match {
      case _: kNNTemporalConnector | _: eNNTemporalConnector | _: aNNTemporalConnector =>
        SupervisionGraph.partition(mln, modes, annotationDB, querySignature)
      case _ =>
        SupervisionGraph.partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = enableClusters)
    }

    // Partition nodes into labeled and unlabeled. Then find empty unlabeled nodes.
    val (labeled, unlabeled) = currentNodes.partition(_.isLabeled)
    val (nonEmptyUnlabeled, emptyUnlabeled) = unlabeled.partition(_.size >= minNodeSize)

    // Remove empty labelled nodes or nodes subsumed by the background knowledge
    val pureLabeledNodes = labeled.filterNot { node =>
      node.isEmpty || mln.clauses.exists(_.subsumes(node.clause.get))
    }.flatMap(_.augment)

    // Labeled query atoms and empty unlabeled query atoms as FALSE.
    val labeledEntries =
      labeled.map(_.query) ++ emptyUnlabeled.flatMap(_.labelUsingValue(FALSE))

    if (emptyUnlabeled.nonEmpty)
      logger.warn(s"Found ${emptyUnlabeled.length} empty unlabeled nodes. Set them to FALSE.")

    val pureNodes = pureLabeledNodes ++ nonEmptyUnlabeled
    logger.info(s"Found ${pureNodes.length} pure labelled and unlabeled nodes.")

    /*
     * Create an annotation builder and append every query atom that is TRUE or FALSE,
     * or every UNKNOWN query atom that has no evidence atoms, everything else
     * should be labeled by the supervision graph.
     */
    val annotationBuilder =
      EvidenceBuilder(
        mln.schema.predicates.filter { case (sig, _) => sig == querySignature },
        Set(querySignature),
        Set.empty,
        mln.evidence.constants
      ).withCWAForAll().evidence ++= labeledEntries

    /*
     * In case no labeled nodes exist in the given data, then reuse the old ones. In any other
     * case try to separate old labeled nodes that are dissimilar to the ones in the current batch
     * of data (the current supervision graph). Moreover remove noisy nodes using the Hoeffding bound.
     */
    if (pureLabeledNodes.isEmpty) {

      val mixed = labeledNodes.exists(_.isPositive) && labeledNodes.exists(_.isNegative)
      val (selectedNodes, updatedMetric) =
        if (mixed && nodeCache.hasChanged && nonEmptyUnlabeled.nonEmpty && (enableSelection || enableHardSelection)) {
          nodeCache.hasChanged = false
          logger.info("Performing feature selection.")
          val clusters = Clustering(maxDensity).cluster(labeledNodes, nodeCache)
          val (weights, selectedNodes) =
            if (enableHardSelection) LargeMarginNN(1, 0.5).optimizeTogether(clusters, modes, nodeCache)(AtomMetric(HungarianMatcher))
            else LargeMarginNN(1, 0.5).optimizeAloneAndMerge(clusters, nodeCache)(AtomMetric(HungarianMatcher))
          selectedNodes -> metric.havingWeights(weights)
        } else (labeledNodes, metric)

      new ExtNNGraph(
        selectedNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        updatedMetric ++ mln.evidence ++ pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms)),
        annotationBuilder,
        nodeCache,
        minNodeSize,
        minNodeOcc,
        enableClusters,
        enableSelection,
        enableHardSelection,
        maxDensity)
    } else {
      /*
       * Update the cache using only non empty labeled nodes, i.e., nodes having at least
       * one evidence predicate in their body
       *
       * Cache stores only unique nodes (patterns) along their counts.
       */
      val startCacheUpdate = System.currentTimeMillis

      var updatedNodeCache = nodeCache
      updatedNodeCache ++= pureLabeledNodes
      val cleanedUniqueLabeled = updatedNodeCache.collectNodes
        .filter(node => node.size >= minNodeSize && nodeCache.getOrElse(node, 0) >= minNodeOcc)

      logger.info(msecTimeToTextUntilNow(s"Cache updated in: ", startCacheUpdate))
      logger.info(s"${cleanedUniqueLabeled.length}/${numberOfLabeled + labeled.length} unique labeled nodes kept.")
      logger.debug(updatedNodeCache.toString)

      updatedNodeCache.hasChanged = true
      val mixed = cleanedUniqueLabeled.exists(_.isPositive) && cleanedUniqueLabeled.exists(_.isNegative)
      val (selectedNodes, updatedMetric) =
        if (mixed && nonEmptyUnlabeled.nonEmpty && (enableSelection || enableHardSelection)) {
          updatedNodeCache.hasChanged = false
          logger.info("Performing feature selection.")
          val clusters = Clustering(maxDensity).cluster(cleanedUniqueLabeled, updatedNodeCache)
          val (weights, selectedNodes) =
            if (enableHardSelection) LargeMarginNN(1, 0.5).optimizeTogether(clusters, modes, nodeCache)(AtomMetric(HungarianMatcher))
            else LargeMarginNN(1, 0.5).optimizeAloneAndMerge(clusters, nodeCache)(AtomMetric(HungarianMatcher))
          selectedNodes -> metric.havingWeights(weights)
        } else (cleanedUniqueLabeled, metric)

      // Labeled nodes MUST appear before unlabeled!
      new ExtNNGraph(
        selectedNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        updatedMetric ++ mln.evidence ++ pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms)),
        annotationBuilder,
        updatedNodeCache,
        minNodeSize,
        minNodeOcc,
        enableClusters,
        enableSelection,
        enableHardSelection,
        maxDensity)
    }
  }
}
