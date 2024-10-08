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
import breeze.linalg.DenseVector
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.graph.selection.{ Clustering, LargeMarginNN }
import lomrf.mln.learning.supervision.metric._
import lomrf.mln.model._
import lomrf.util.time._
import lomrf.mln.model.builders.EvidenceBuilder
import scala.language.existentials
import lomrf.logic.LogicOps._

/**
  * SPLICE supervision graph represents a graph having nodes for a given query signature. These
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
  * @param solver a graph solver for supervision completion
  * @param edgeReWeighting re-weight edges using cache frequency
  * @param minNodeSize minimum node size
  * @param minNodeOcc minimum node occurrences
  * @param augment augment examples
  * @param enableClusters enables clustering of unlabeled examples
  * @param enableSelection enables feature selection
  * @param enableHardSelection enables hard feature selection
  * @param maxDensity clusters maximum density
  */
final class SPLICE private[graph] (
    nodes: IndexedSeq[Node],
    querySignature: AtomSignature,
    connector: GraphConnector,
    metric: Metric[_ <: AtomicFormula],
    supervisionBuilder: EvidenceBuilder,
    nodeCache: NodeCache,
    solver: GraphSolver,
    edgeReWeighting: Boolean,
    labelReWeighting: Boolean,
    minNodeSize: Int,
    minNodeOcc: Int,
    augment: Boolean,
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
    val cache = if (edgeReWeighting) Some(nodeCache) else None
    val encodedGraph = solver match {
      case _: HFc => connector.smartConnect(nodes, unlabeledNodes, cache)(metric)
      case _      => connector.fullyConnect(nodes, cache)(metric)
    }

    val W = encodedGraph._1
    val D = encodedGraph._2
    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis

    // Vector holding the labeled values

    val fl =
      if (labelReWeighting) {
        var (positiveFactor, negativeFactor) = (0L, 0L)
        labeledNodes.foreach { node =>
          if (node.isPositive)
            positiveFactor += node.size * nodeCache.getOrElse(node, 0)
          else
            negativeFactor += node.size * nodeCache.getOrElse(node, 0)
        }

        DenseVector(labeledNodes.map { node =>
          if (node.isPositive) node.value * node.size * nodeCache.getOrElse(node, 0) / positiveFactor
          else node.value * node.size * nodeCache.getOrElse(node, 0) / negativeFactor
        }.toArray)
      } else DenseVector(labeledNodes.map(_.value).toArray)

    val fullSolution = solver.solve(W, D, fl).toArray
    val solution = fullSolution.slice(numberOfLabeled, numberOfNodes)
    val truthValues = solution.map(value => if (value <= UNCONNECTED) FALSE else TRUE)

    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    logger.whenDebugEnabled {
      logger.debug {
        (unlabeledNodes zip solution).sortBy { case (node, _) => node }
          .map { case (node, state) => s"${node.query} = $state" }.mkString("\n")
      }
    }

    val labeledEvidenceAtoms = unlabeledNodes.zip(truthValues).flatMap {
      case (node, value) => node.labelUsingValue(value)
    }

    supervisionBuilder.evidence ++= labeledEvidenceAtoms
    labeledEvidenceAtoms.toSet
  }

  def ++(mln: MLN, annotationDB: EvidenceDB, modes: ModeDeclarations): SPLICE = {

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
    val pureLabeledNodes = labeled.filterNot { n =>
      n.isEmpty || mln.clauses.exists(_.subsumes(n.clause.get))
    }.flatMap(_.augment)

    // Labeled query atoms and empty unlabeled query atoms as FALSE.
    val labeledEntries =
      labeled.flatMap(x => x.similarNodeQueryAtoms + x.query) ++
        emptyUnlabeled.flatMap(_.labelUsingValue(FALSE))

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
      if (!mixed) logger.info("Labeled nodes contain either only positive or only negative examples.")

      val (selectedNodes, weightedMetric) =
        if (mixed && nodeCache.hasChanged && nonEmptyUnlabeled.nonEmpty && (enableSelection || enableHardSelection)) {
          nodeCache.hasChanged = false
          logger.info("Performing feature selection.")
          val startSelection = System.currentTimeMillis
          val clusters = Clustering(maxDensity).cluster(labeledNodes, nodeCache)
          val (weights, selectedNodes) =
            if (enableHardSelection) LargeMarginNN(1, 0.5).optimizeTogether(clusters, modes, nodeCache)(AtomMetric(HungarianMatcher))
            else LargeMarginNN(1, 0.5).optimizeAloneAndMerge(clusters, nodeCache)(AtomMetric(HungarianMatcher))
          logger.info(msecTimeToTextUntilNow(s"Feature selection completed in: ", startSelection))
          if (augment) selectedNodes.flatMap(_.augment) -> metric.havingWeights(weights)
          else selectedNodes -> metric.havingWeights(weights)
        } else (labeledNodes, metric)

      val startMetricUpdate = System.currentTimeMillis
      val updatedMetric =
        weightedMetric ++
          mln.evidence ++
          pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms))
      logger.info(msecTimeToTextUntilNow(s"Metric updated in: ", startMetricUpdate))

      new SPLICE(
        selectedNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        updatedMetric,
        annotationBuilder,
        nodeCache,
        solver,
        edgeReWeighting,
        labelReWeighting,
        minNodeSize,
        minNodeOcc,
        augment,
        enableClusters,
        enableSelection,
        enableHardSelection,
        maxDensity
      )
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

      val mixed = cleanedUniqueLabeled.exists(_.isPositive) && cleanedUniqueLabeled.exists(_.isNegative)
      if (!mixed) logger.info("Labeled nodes contain either only positive or only negative examples.")

      updatedNodeCache.hasChanged = true
      val (selectedNodes, weightedMetric) =
        if (mixed && nonEmptyUnlabeled.nonEmpty && (enableSelection || enableHardSelection)) {
          updatedNodeCache.hasChanged = false
          logger.info("Performing feature selection.")
          val startSelection = System.currentTimeMillis
          val clusters = Clustering(maxDensity).cluster(cleanedUniqueLabeled, updatedNodeCache)
          val (weights, selectedNodes) =
            if (enableHardSelection) LargeMarginNN(1, 0.5).optimizeTogether(clusters, modes, nodeCache)(AtomMetric(HungarianMatcher))
            else LargeMarginNN(1, 0.5).optimizeAloneAndMerge(clusters, nodeCache)(AtomMetric(HungarianMatcher))
          logger.info(msecTimeToTextUntilNow(s"Feature selection completed in: ", startSelection))
          if (augment) selectedNodes.flatMap(_.augment) -> metric.havingWeights(weights)
          else selectedNodes -> metric.havingWeights(weights)
        } else (cleanedUniqueLabeled, metric)

      val startMetricUpdate = System.currentTimeMillis
      val updatedMetric =
        weightedMetric ++
          mln.evidence ++
          pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms))
      logger.info(msecTimeToTextUntilNow(s"Metric updated in: ", startMetricUpdate))

      // Labeled nodes MUST appear before unlabeled!
      new SPLICE(
        selectedNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        updatedMetric,
        annotationBuilder,
        updatedNodeCache,
        solver,
        edgeReWeighting,
        labelReWeighting,
        minNodeSize,
        minNodeOcc,
        augment,
        enableClusters,
        enableSelection,
        enableHardSelection,
        maxDensity)
    }
  }
}
