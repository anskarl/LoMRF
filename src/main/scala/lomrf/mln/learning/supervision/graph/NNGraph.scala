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
import lomrf.util.logging.Implicits._
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.metric._
import lomrf.mln.model._
import lomrf.util.time._
import scala.language.existentials
import lomrf.logic.LogicOps._

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
  * @param enableClusters enables clustering of unlabeled examples
  */
final class NNGraph private[graph] (
    nodes: IndexedSeq[Node],
    querySignature: AtomSignature,
    connector: GraphConnector,
    metric: Metric[_ <: AtomicFormula],
    supervisionBuilder: EvidenceBuilder,
    nodeCache: NodeCache,
    enableClusters: Boolean)
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
    val W = connector.biConnect(unlabeledNodes, labeledNodes)(metric)
    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis

    val labeledEvidenceAtoms = unlabeledNodes.zipWithIndex.flatMap {
      case (node, i) =>

        val nearest = W(i, ::).inner.findAll(_ != UNCONNECTED).map { ii =>
          val node = labeledNodes(ii)
          val counts = nodeCache.getOrElse(node, logger.fatal(s"Pattern ${node.clause.get.toText()} not found."))
          W(i, ii) -> (node.isPositive, counts)
        }

        val (positives, negatives) = nearest.partition { case (_, (tv, _)) => tv }

        val value =
          if (nearest.isEmpty) false
          else {
            val posCounts = positives.map(_._2._2).sum
            val negCounts = negatives.map(_._2._2).sum
            if (posCounts > negCounts) true
            else if (negCounts > posCounts) false
            else nearest.maxBy(_._1)._2._1
          }

        node.labelUsingValue(value)
    }

    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    supervisionBuilder.evidence ++= labeledEvidenceAtoms
    labeledEvidenceAtoms.toSet
  }

  def ++(mln: MLN, annotationDB: EvidenceDB, modes: ModeDeclarations): NNGraph = {

    // Group the given data into nodes, using the domains of the existing graph
    val currentNodes = connector match {
      case _: kNNTemporalConnector | _: eNNTemporalConnector | _: aNNTemporalConnector =>
        SupervisionGraph.partition(mln, modes, annotationDB, querySignature)
      case _ =>
        SupervisionGraph.partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = enableClusters)
    }

    // Partition nodes into labeled and unlabeled. Then find empty unlabeled nodes.
    val (labeled, unlabeled) = currentNodes.partition(_.isLabeled)
    val (nonEmptyUnlabeled, emptyUnlabeled) = unlabeled.partition(_.nonEmpty)

    // Use background knowledge to remove uninteresting or empty labelled nodes
    val cleanedLabeled = labeled.filterNot { n =>
      n.isEmpty || mln.clauses.exists(_.subsumes(n.clause.get))
    }

    // Labeled query atoms and empty unlabeled query atoms as FALSE.
    val labeledEntries =
      labeled.map(_.query) ++ emptyUnlabeled.flatMap(_.labelUsingValue(FALSE))

    if (emptyUnlabeled.nonEmpty)
      logger.warn(s"Found ${emptyUnlabeled.length} empty unlabeled nodes. Set them to FALSE.")

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
    if (cleanedLabeled.isEmpty)
      new NNGraph(
        labeledNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence ++ currentNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms)),
        annotationBuilder,
        nodeCache,
        enableClusters)
    else {
      /*
       * Update the cache using only non empty labeled nodes, i.e., nodes having at least
       * one evidence predicate in their body
       *
       * Cache stores only unique nodes (patterns) along their counts.
       */
      val startCacheUpdate = System.currentTimeMillis

      var updatedNodeCache = nodeCache
      updatedNodeCache ++= cleanedLabeled
      val cleanedUniqueLabeled = updatedNodeCache.collectNodes

      logger.info(msecTimeToTextUntilNow(s"Cache updated in: ", startCacheUpdate))
      logger.info(s"${cleanedUniqueLabeled.length}/${numberOfLabeled + labeled.length} unique labeled nodes kept.")
      logger.debug(updatedNodeCache.toString)

      // Labeled nodes MUST appear before unlabeled!
      new NNGraph(
        cleanedUniqueLabeled ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence ++ currentNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms)),
        annotationBuilder,
        updatedNodeCache,
        enableClusters)
    }
  }
}
