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
final class ENNGraph private[graph] (
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

    import breeze.linalg.{ DenseMatrix, DenseVector }
    import spire.syntax.cfor._

    val parallelIndices = nodes.indices.par
    val W = DenseMatrix.fill[Double](numberOfNodes, numberOfNodes)(UNCONNECTED)

    cfor(0)(_ < numberOfNodes, _ + 1) { i =>
      for (j <- parallelIndices if i != j) { // A node cannot be connected to itself

        // W is symmetric and therefore avoid computing both upper and lower triangular
        if (i > j) W(i, j) = W(j, i)
        else W(i, j) = connector.connect(nodes(i), nodes(j))(metric)
      }
    }

    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis

    val S = nodes.zipWithIndex.map { case (x, i) => i -> x -> W(i, ::).t }
    val (positives, negatives) = S.filter(_._1._2.isLabeled).partition(_._1._2.isPositive)
    val unlabeled = S.filter(_._1._2.isUnlabeled)
    val P = positives.length
    val N = negatives.length
    val K = connector.asInstanceOf[kNNConnector].k

    val labeledEvidenceAtoms = unlabeled.flatMap {
      case z @ ((idx, x), hood) =>

        println(x.body.get.toText())

        // j = 0
        val un = x.toNegative

        val T00 = (negatives :+ ((idx, un), hood)).map {
          case (_, neighborhood) =>
            val nearest = connector.makeSparse(DenseVector.vertcat(neighborhood(0 until numberOfLabeled), neighborhood(idx to idx)))
            nearest.toArray.zip(labeledNodes :+ un).filter { // TODO OR count without the map for simple version
              case (w, n) => w > 0 && n.isNegative
            }.map(_._1).sum / ((N + 1) * nearest.toArray.count(_ > 0)).toDouble
        }.sum

        val T10 = positives.map {
          case (_, neighborhood) =>
            val nearest = connector.makeSparse(DenseVector.vertcat(neighborhood(0 until numberOfLabeled), neighborhood(idx to idx)))
            nearest.toArray.zip(labeledNodes :+ un).filter {
              case (w, n) => w > 0 && n.isPositive
            }.map(_._1).sum / (P * nearest.toArray.count(_ > 0)).toDouble
        }.sum

        // j = 1
        val up = x.toPositive

        val T11 = (positives :+ ((idx, up), hood)).map {
          case (_, neighborhood) =>
            val nearest = connector.makeSparse(DenseVector.vertcat(neighborhood(0 until numberOfLabeled), neighborhood(idx to idx)))
            nearest.toArray.zip(labeledNodes :+ up).filter {
              case (w, n) => w > 0 && n.isPositive
            }.map(_._1).sum / ((P + 1) * nearest.toArray.count(_ > 0)).toDouble
        }.sum

        val T01 = negatives.map {
          case (_, neighborhood) =>
            val nearest = connector.makeSparse(DenseVector.vertcat(neighborhood(0 until numberOfLabeled), neighborhood(idx to idx)))
            nearest.toArray.zip(labeledNodes :+ up).filter {
              case (w, n) => w > 0 && n.isNegative
            }.map(_._1).sum / (N * nearest.toArray.count(_ > 0)).toDouble
        }.sum

        logger.info(s"T00: $T00 T10: $T10 T01: $T01 T11: $T11")

        if (T00 + T10 >= T01 + T11) x.labelUsingValue(false)
        else x.labelUsingValue(true)
    }

    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    supervisionBuilder.evidence ++= labeledEvidenceAtoms
    labeledEvidenceAtoms.toSet
  }

  def ++(mln: MLN, annotationDB: EvidenceDB, modes: ModeDeclarations): ENNGraph = {

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
      new ENNGraph(
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
      logger.info(updatedNodeCache.toString)

      // Labeled nodes MUST appear before unlabeled!
      new ENNGraph(
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
