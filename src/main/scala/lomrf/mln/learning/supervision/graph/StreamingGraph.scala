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
import lomrf.mln.model.{ EvidenceDB, MLN, ModeDeclarations }
import lomrf.mln.learning.supervision.graph.selection.{ Clustering, LargeMarginNN }
import lomrf.mln.learning.supervision.metric.{ AtomMetric, HungarianMatcher, Metric }
import lomrf.util.time.msecTimeToTextUntilNow
import lomrf.logic.LogicOps._
import lomrf.mln.model.builders.EvidenceBuilder

final class StreamingGraph private[graph] (
    nodes: IndexedSeq[Node],
    querySignature: AtomSignature,
    connector: GraphConnector,
    metric: Metric[_ <: AtomicFormula],
    supervisionBuilder: EvidenceBuilder,
    nodeCache: NodeCache,
    solver: GraphSolver,
    edgeReWeighting: Boolean,
    storedUnlabeled: IndexedSeq[Node],
    previousGraph: GraphMatrix,
    memory: Int,
    minNodeSize: Int,
    minNodeOcc: Int,
    enableSelection: Boolean,
    enableHardSelection: Boolean,
    maxDensity: Double)
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

    val cache = if (edgeReWeighting) Some(nodeCache) else None
    val encodedGraph = solver match {
      case _: HFc => connector.smartConnect(nodes, unlabeledNodes, cache)(metric)
      case _      => connector.fullyConnect(nodes, cache)(metric)
    }

    val WW = encodedGraph._1

    logger.debug {
      s"""
        |Connected
        |${WW.mkString()}
      """.stripMargin
    }

    W = DenseMatrix.horzcat(
      DenseMatrix.vertcat(W, DenseMatrix.zeros[Double](numberOfUnlabeled, W.rows)),
      DenseMatrix.zeros[Double](W.cols + numberOfUnlabeled, numberOfUnlabeled)
    )

    logger.debug {
      s"""
         |Connected
         |${W.mkString()}
      """.stripMargin
    }

    cfor(0)(_ < numberOfUnlabeled, _ + 1) { i =>
      val WWi = numberOfLabeled + i
      val Wi = 2 + storedUnlabeled.length + i // TODO switch 2 to number of labeled

      cfor(0)(_ < numberOfNodes, _ + 1) { j =>
        if (j < numberOfLabeled) {
          val clusterId = if (labeledNodes(j).isNegative) 0 else 1
          W(Wi, clusterId) += WW(WWi, j) // TODO switch clusterId to j
          W(clusterId, Wi) += WW(j, WWi)
        } else {
          val Wj = 2 + storedUnlabeled.length + (j - numberOfLabeled) // TODO switch 2 to number of labeled
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

    logger.debug {
      s"""
         |Connected
         |${W.mkString()}
      """.stripMargin
    }

    // WARN: SYNOPSIS WAS HERE AT THE BEGINNING. IS THERE A DIFFERENCE?

    val D = DenseMatrix.zeros[Double](W.rows, W.cols)
    cfor(0)(_ < W.rows, _ + 1) { i => D(i, i) = sum(W(i, ::)) }

    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis

    // Vector holding the labeled values
    val fl = DenseVector(-1d, 1d)

    val s1 = solver.solve(W, D, fl).toArray
    val solution = solver.solve(W, D, fl).toArray.drop(2) // TODO switch 2 to number of labeled
    val truthValues = solution.map(value => if (value <= UNCONNECTED) FALSE else TRUE)

    // Delete old nodes that do not fit into memory
    W = connector.synopsisOf(W, 2, memory) // TODO switch 2 to number of labeled

    logger.debug {
      s"""
         |After synopsis
         |${W.mkString()}
      """.stripMargin
    }

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
    val (nonEmptyUnlabeled, emptyUnlabeled) = unlabeled.partition(_.size >= minNodeSize)

    // Remove empty labelled nodes or nodes subsumed by the background knowledge
    val pureLabeledNodes = labeled.filterNot { n =>
      n.isEmpty || mln.clauses.exists(_.subsumes(n.clause.get))
    }.flatMap(_.augment)

    // Labeled query atoms and empty unlabeled query atoms as FALSE.
    val labeledEntries =
      labeled.map(_.query) ++ emptyUnlabeled.flatMap(_.labelUsingValue(FALSE))

    if (emptyUnlabeled.nonEmpty)
      logger.warn(s"Found ${emptyUnlabeled.length} empty unlabeled nodes. Set them to FALSE.")

    val pureNodes = pureLabeledNodes ++ nonEmptyUnlabeled
    logger.info(s"Found ${pureLabeledNodes.length} pure labelled and unlabeled nodes.")

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
        (storedUnlabeled ++ unlabeledNodes).drop(numberOfStoredUnlabeled + numberOfUnlabeled - memory)
      else storedUnlabeled ++ unlabeledNodes

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

      new StreamingGraph(
        selectedNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        updatedMetric ++ mln.evidence ++ pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms)),
        annotationBuilder,
        nodeCache,
        solver,
        edgeReWeighting,
        updatedStoredUnlabeled,
        W.copy,
        memory,
        minNodeSize,
        minNodeOcc,
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

      /*val Wnew = if (cleanedUniqueLabeled.length - numberOfLabeled > 0) {
        val A = W(0 until numberOfLabeled, 0 until numberOfLabeled)
        val B = DenseMatrix.vertcat(A, DenseMatrix.zeros[Double](cleanedUniqueLabeled.length - numberOfLabeled, numberOfLabeled))
        val C = DenseMatrix.horzcat(B, DenseMatrix.zeros[Double](cleanedUniqueLabeled.length, cleanedUniqueLabeled.length - numberOfLabeled))
        C
      } else W.copy*/

      // Labeled nodes MUST appear before unlabeled!
      new StreamingGraph(
        selectedNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        updatedMetric ++ mln.evidence ++ pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms)),
        annotationBuilder,
        updatedNodeCache,
        solver,
        edgeReWeighting,
        updatedStoredUnlabeled,
        W.copy,
        memory,
        minNodeSize,
        minNodeOcc,
        enableSelection,
        enableHardSelection,
        maxDensity
      )
    }
  }
}
