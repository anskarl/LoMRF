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

import spire.syntax.cfor._
import breeze.linalg.{ DenseMatrix, DenseVector, sum }
import lomrf.logic.{ AtomSignature, AtomicFormula, EvidenceAtom, FALSE, TRUE }
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.metric.Metric
import lomrf.mln.model.{ EvidenceBuilder, EvidenceDB, MLN, ModeDeclarations }
import lomrf.util.time.msecTimeToTextUntilNow

final class StreamingGraph private[graph] (
    nodes: IndexedSeq[Node],
    querySignature: AtomSignature,
    connector: GraphConnector,
    metric: Metric[_ <: AtomicFormula],
    supervisionBuilder: EvidenceBuilder,
    nodeCache: NodeCache,
    solver: GraphSolver,
    storedUnlabeled: IndexedSeq[Node],
    previousGraph: GraphMatrix,
    memory: Int)
  extends SupervisionGraph(nodes, querySignature, connector, metric, supervisionBuilder, nodeCache) {

  private var W = previousGraph
  private val numberOfStoredUnlabeled = storedUnlabeled.length

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

    val encodedGraph = connector.smartConnect(nodes, unlabeledNodes, Some(nodeCache))(metric)
    val WW = encodedGraph._1

    W = DenseMatrix.horzcat(
      DenseMatrix.vertcat(W, DenseMatrix.zeros[Double](numberOfUnlabeled, W.rows)),
      DenseMatrix.zeros[Double](W.cols + numberOfUnlabeled, numberOfUnlabeled)
    )

    cfor(0)(_ < numberOfUnlabeled, _ + 1) { i =>
      val WWi = numberOfLabeled + i
      val Wi = 2 + storedUnlabeled.length + i

      cfor(0)(_ < numberOfNodes, _ + 1) { j =>
        if (j < numberOfLabeled) {
          val clusterId = if (labeledNodes(j).isNegative) 0 else 1
          W(Wi, clusterId) += WW(WWi, j)
          W(clusterId, Wi) += WW(j, WWi)
        } else {
          val Wj = 2 + storedUnlabeled.length + (numberOfLabeled - j)
          W(Wi, Wj) = WW(WWi, j)
          W(Wj, Wi) = WW(j, WWi)
        }
      }

      cfor(0)(_ < storedUnlabeled.length, _ + 1) { j =>
        val weight = connector.connect(unlabeledNodes(i), storedUnlabeled(j))(metric)
        W(Wi, j + 2) = weight
        W(j + 2, Wi) = weight
      }
    }

    // Delete old nodes that do not fit into memory
    W = connector.synopsisOf(W, 2, memory)

    val D = DenseMatrix.zeros[Double](W.rows, W.cols)
    cfor(0)(_ < W.rows, _ + 1) { i => D(i, i) = sum(W(i, ::)) }

    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis

    // Vector holding the labeled values
    val fl = DenseVector(-1d, 1d)

    val solution = solver.solve(W, D, fl).toArray.slice(2, numberOfNodes)
    val truthValues = solution.map(value => if (value <= UNCONNECTED) FALSE else TRUE)

    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    logger.whenDebugEnabled {
      logger.debug {
        (unlabeledNodes.map(_.query) zip solution.takeRight(numberOfUnlabeled))
          .map { case (atom, state) => s"$atom = $state" }.mkString("\n")
      }
    }

    val labeledEvidenceAtoms = unlabeledNodes.zip(truthValues.takeRight(numberOfUnlabeled)).flatMap {
      case (node, value) => node.labelUsingValue(value)
    }

    supervisionBuilder.evidence ++= labeledEvidenceAtoms
    labeledEvidenceAtoms.toSet
  }

  def ++(mln: MLN, annotationDB: EvidenceDB, modes: ModeDeclarations): StreamingGraph = {

    // Group the given data into nodes, using the domains of the existing graph
    val currentNodes = SupervisionGraph.partition(mln, modes, annotationDB, querySignature)

    // Partition nodes into labeled and unlabeled. Then find empty unlabeled nodes.
    val (labeled, unlabeled) = currentNodes.partition(_.isLabeled)
    val (nonEmptyUnlabeled, emptyUnlabeled) = unlabeled.partition(_.nonEmpty)

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

    val updatedStoredUnlabeled =
      if (numberOfStoredUnlabeled + numberOfUnlabeled > memory)
        storedUnlabeled.drop(numberOfStoredUnlabeled + numberOfUnlabeled - memory) ++ unlabeledNodes
      else storedUnlabeled ++ unlabeledNodes

    /*
     * In case no labeled nodes exist in the given data, then reuse the old ones. In any other
     * case try to separate old labeled nodes that are dissimilar to the ones in the current batch
     * of data (the current supervision graph). Moreover remove noisy nodes using the Hoeffding bound.
     */
    if (labeled.isEmpty)
      new StreamingGraph(
        labeledNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence ++ currentNodes.map(_.atoms),
        annotationBuilder,
        nodeCache,
        solver,
        updatedStoredUnlabeled,
        W.copy,
        memory)
    else {
      /*
       * Update the cache using only non empty labeled nodes, i.e., nodes having at least
       * one evidence predicate in their body
       *
       * Cache stores only unique nodes (patterns) along their counts.
       */
      val startCacheUpdate = System.currentTimeMillis

      var updatedNodeCache = nodeCache
      updatedNodeCache ++= labeled.filter(_.nonEmpty)
      val cleanedUniqueLabeled = updatedNodeCache.collectNodes

      logger.info(msecTimeToTextUntilNow(s"Cache updated in: ", startCacheUpdate))
      logger.info(s"${cleanedUniqueLabeled.length}/${numberOfLabeled + labeled.length} unique labeled nodes kept.")
      logger.debug(updatedNodeCache.toString)

      // Labeled nodes MUST appear before unlabeled!
      new StreamingGraph(
        cleanedUniqueLabeled ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence ++ currentNodes.map(_.atoms),
        annotationBuilder,
        updatedNodeCache,
        solver,
        updatedStoredUnlabeled,
        W.copy,
        memory
      )
    }
  }
}
