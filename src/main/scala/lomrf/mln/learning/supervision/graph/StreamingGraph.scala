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
    labelReWeighting: Boolean,
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
  private val numberOfClusters = if (labelReWeighting) numberOfLabeled else 2
  private var bagOfThings = Vector.empty[Node]

  protected def optimize(potentials: Map[EvidenceAtom, Double]): Set[EvidenceAtom] = {

    logger.info {
      s"""
         |Supervision graph has $numberOfNodes nodes:
         |\t- Labeled Nodes: $numberOfLabeled
         |\t- Unlabeled Nodes: $numberOfUnlabeled
         |\t- Query Signature: $querySignature
      """.stripMargin
    }

    logger.info {
      s"""
        |Stored in memory: ${storedUnlabeled.map(_.query).mkString(", ")}
        |Current unlabelled: ${unlabeledNodes.head.query} ... ${unlabeledNodes.last.query}
      """.stripMargin
    }

    /*
     * NOTE: In order to use all labelled examples instead of 2 clusters,
     * change '2' to 'numberOfLabeled' and 'clusterId' to 'j'.
     */

    val startGraphConnection = System.currentTimeMillis

    val cache = if (edgeReWeighting) Some(nodeCache) else None
    val encodedGraph = solver match {
      case _: HFc => connector.smartConnect(nodes, unlabeledNodes, cache)(metric)
      case _      => connector.fullyConnect(nodes, cache)(metric)
    }

    val WW = encodedGraph._1

    logger.debug {
      s"""
        |1) Labelled data were connected to incoming unlabelled data (${WW.rows} x ${WW.cols}):
        |${WW.mkString()}
      """.stripMargin
    }

    W = DenseMatrix.horzcat(
      DenseMatrix.vertcat(W, DenseMatrix.zeros[Double](numberOfUnlabeled, W.rows)),
      DenseMatrix.zeros[Double](W.cols + numberOfUnlabeled, numberOfUnlabeled)
    )

    logger.debug {
      s"""
         |2) Stored W is zero padded to hold all incoming unlabelled data (${W.rows} x ${W.cols}):
         |${W.mkString()}
      """.stripMargin
    }

    cfor(0)(_ < numberOfUnlabeled, _ + 1) { i =>
      val WWi = numberOfLabeled + i
      val Wi = numberOfClusters + numberOfStoredUnlabeled + i

      cfor(0)(_ < numberOfNodes, _ + 1) { j =>
        if (j < numberOfLabeled) {
          // if label re-weighting is enabled, then do not use clusters
          val clusterId = if (labelReWeighting) j else if (labeledNodes(j).isNegative) 0 else 1
          W(Wi, clusterId) += WW(WWi, j)
          W(clusterId, Wi) += WW(j, WWi)
        } else {
          val Wj = numberOfClusters + numberOfStoredUnlabeled + (j - numberOfLabeled)
          W(Wi, Wj) = WW(WWi, j)
          W(Wj, Wi) = WW(j, WWi)
        }
      }

      cfor(0)(_ < numberOfStoredUnlabeled, _ + 1) { j =>
        val weight = connector.connect(unlabeledNodes(i), storedUnlabeled(j))(metric)
        W(Wi, numberOfClusters + j) = weight
        W(numberOfClusters + j, Wi) = weight
      }
    }

    logger.debug {
      s"""
         |3) W is filled using the values computed in step (1):
         |${W.mkString()}
      """.stripMargin
    }

    val D = DenseMatrix.zeros[Double](W.rows, W.cols)
    cfor(0)(_ < W.rows, _ + 1) { i => D(i, i) = sum(W(i, ::)) }

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

        val x = DenseVector(labeledNodes.map { node =>
          if (node.isPositive) node.value * node.size * nodeCache.getOrElse(node, 0) / positiveFactor
          else node.value * node.size * nodeCache.getOrElse(node, 0) / negativeFactor
        }.toArray)

        //        labeledNodes.zip(x.toArray).foreach { case (n, v) =>
        //          println(s"${n.toText} -> $v")
        //        }
        x
      } else DenseVector(-1d, 1d)

    val solution = solver.solve(W, D, fl).toArray.drop(numberOfClusters)

    unlabeledNodes.zip(solution).withFilter { case (n, _) => nodeCache.get(n.toPositive).nonEmpty || nodeCache.get(n.toNegative).nonEmpty }.foreach {
      case (n, v) =>
        //println(s"${n.toText} -> ${v}")
        if (nodeCache.get(n.toPositive).nonEmpty && math.abs(v) > 0.1) bagOfThings :+= n.toPositive
        else if (math.abs(v) > 0.1) bagOfThings :+= n.toNegative
    }

    val truthValues = solution.map(value => if (value <= UNCONNECTED) FALSE else TRUE)
    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    // Delete old nodes that do not fit into memory
    W = connector.synopsisOf(W, numberOfClusters, memory)

    logger.debug {
      s"""
         |4) W after synopsis is performed (${W.rows} x ${W.cols}):
         |${W.mkString()}
      """.stripMargin
    }

    logger.whenDebugEnabled {
      logger.debug {
        (unlabeledNodes zip solution.takeRight(numberOfUnlabeled)).sortBy { case (node, _) => node }
          .map { case (node, state) => s"${node.query} = $state" }.mkString("\n")
      }
    }

    val labeledEvidenceAtoms = unlabeledNodes.zip(truthValues.takeRight(numberOfUnlabeled)).flatMap {
      case (node, value) => node.labelUsingValue(value)
    }

    supervisionBuilder.evidence ++= labeledEvidenceAtoms
    labeledEvidenceAtoms.toSet
  }

  def ++(mln: MLN, annotationDB: EvidenceDB, modes: ModeDeclarations): StreamingGraph = {

    // Group the given data into nodes, using the domains of the existing graph (TLP requires sorting)
    val currentNodes = SupervisionGraph.partition(mln, modes, annotationDB, querySignature).sorted

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

    val updatedStoredUnlabeled =
      if (storedUnlabeled.isEmpty && (labeledNodes.isEmpty || unlabeledNodes.isEmpty))
        IndexedSeq.empty[Node]
      else if (numberOfStoredUnlabeled + numberOfUnlabeled > memory)
        (storedUnlabeled ++ unlabeledNodes).takeRight(memory)
      else storedUnlabeled ++ unlabeledNodes

    /*
     * In case no labeled nodes exist in the given data, then reuse the old ones. In any other
     * case try to separate old labeled nodes that are dissimilar to the ones in the current batch
     * of data (the current supervision graph). Moreover remove noisy nodes using the Hoeffding bound.
     */
    if (pureLabeledNodes.isEmpty && bagOfThings.isEmpty) {

      val mixed = labeledNodes.exists(_.isPositive) && labeledNodes.exists(_.isNegative)
      if (!mixed) logger.info("Labeled nodes contain either only positive or negative examples.")

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
          selectedNodes -> metric.havingWeights(weights)
        } else (labeledNodes, metric)

      val startMetricUpdate = System.currentTimeMillis
      val updatedMetric =
        weightedMetric ++
          mln.evidence ++
          pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms))
      logger.info(msecTimeToTextUntilNow(s"Metric updated in: ", startMetricUpdate))

      new StreamingGraph(
        selectedNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        updatedMetric,
        annotationBuilder,
        nodeCache,
        solver,
        edgeReWeighting,
        labelReWeighting,
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
       * Update the cache using only non-empty labeled nodes, i.e., nodes having at least
       * one evidence predicate in their body
       *
       * Cache stores only unique nodes (patterns) along their counts.
       */
      val startCacheUpdate = System.currentTimeMillis

      var updatedNodeCache = nodeCache
      updatedNodeCache ++= bagOfThings // TODO
      updatedNodeCache ++= pureLabeledNodes
      val cleanedUniqueLabeled = updatedNodeCache.collectNodes
        .filter(node => node.size >= minNodeSize && nodeCache.getOrElse(node, 0) >= minNodeOcc)

      logger.info(msecTimeToTextUntilNow(s"Cache updated in: ", startCacheUpdate))
      logger.info(s"${cleanedUniqueLabeled.length}/${numberOfLabeled + labeled.length} unique labeled nodes kept.")
      logger.info(updatedNodeCache.toString)

      val mixed = cleanedUniqueLabeled.exists(_.isPositive) && cleanedUniqueLabeled.exists(_.isNegative)
      if (!mixed) logger.info("Labeled nodes contain either only positive or negative examples.")

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
          selectedNodes -> metric.havingWeights(weights)
        } else (cleanedUniqueLabeled, metric)

      //                  println("##################")
      //                  println(labeledNodes.zipWithIndex.map { case (n, i) => s"${i + 1}. ${n.toText}" }.foreach(println))
      //                  println("##################")
      //                  println(cleanedUniqueLabeled.zipWithIndex.map { case (n, i) => s"${i + 1}. ${n.toText}" }.foreach(println))
      //
      //                  println(s"W: ${W.rows} x ${W.cols}")
      //                  println(s"${numberOfLabeled} ${cleanedUniqueLabeled.length}, ${updatedStoredUnlabeled.length}, U ${numberOfUnlabeled}")

      val updatedW = if (labelReWeighting && updatedStoredUnlabeled.nonEmpty && cleanedUniqueLabeled.length - numberOfLabeled > 0) {
        // expand the W matrix to contain zeros rows/columns for the incoming labelled examples
        val A = W(0 until numberOfLabeled, 0 until numberOfLabeled)
        val B = DenseMatrix.vertcat(A, DenseMatrix.zeros[Double](cleanedUniqueLabeled.length - numberOfLabeled, numberOfLabeled))
        val C = DenseMatrix.horzcat(B, DenseMatrix.zeros[Double](cleanedUniqueLabeled.length, cleanedUniqueLabeled.length - numberOfLabeled))

        //                        println(s"C: ${C.rows} x ${C.cols}")

        // expand the C matrix to contain zero rows/columns for the previous stored unlabelled examples
        val D = DenseMatrix.vertcat(C, DenseMatrix.zeros[Double](updatedStoredUnlabeled.length, cleanedUniqueLabeled.length))
        val E = DenseMatrix.horzcat(D, DenseMatrix.zeros[Double](cleanedUniqueLabeled.length + updatedStoredUnlabeled.length, updatedStoredUnlabeled.length))

        //                        println(s"E: ${E.rows} x ${E.cols}")

        // restore the edge values of the stored unlabelled examples
        E(cleanedUniqueLabeled.length until E.rows, 0 until numberOfLabeled) := W(numberOfLabeled until W.rows, 0 until numberOfLabeled)
        E(0 until numberOfLabeled, cleanedUniqueLabeled.length until E.cols) := W(0 until numberOfLabeled, numberOfLabeled until W.cols)

        //                println(s"done")

        E
      } else if (labelReWeighting && updatedStoredUnlabeled.nonEmpty && cleanedUniqueLabeled.length - numberOfLabeled < 0) {
        // reduce the W matrix to less rows/columns for the labelled examples
        val sub = numberOfLabeled - cleanedUniqueLabeled.length
        val A = W(0 until numberOfLabeled - sub, 0 until numberOfLabeled - sub)

        //                println(s"A: ${A.rows} x ${A.cols}")

        // expand the A matrix to contain zero rows/columns for the previous stored unlabelled examples
        val B = DenseMatrix.vertcat(A, DenseMatrix.zeros[Double](updatedStoredUnlabeled.length, numberOfLabeled - sub))
        val C = DenseMatrix.horzcat(B, DenseMatrix.zeros[Double](numberOfLabeled - sub + updatedStoredUnlabeled.length, updatedStoredUnlabeled.length))

        //                println(s"C: ${C.rows} x ${C.cols}")

        // restore the edge values of the stored unlabelled examples
        C(numberOfLabeled - sub until C.rows, 0 until numberOfLabeled - sub) := W(numberOfLabeled until W.rows, 0 until numberOfLabeled - sub)
        C(0 until numberOfLabeled - sub, numberOfLabeled - sub until C.cols) := W(0 until numberOfLabeled - sub, numberOfLabeled until W.cols)

        //                println(s"done")

        C
      } else if (labelReWeighting && updatedStoredUnlabeled.isEmpty)
        // if we only have more labelled examples, just increase the zero matrix (WARNING: This does not work for label spreading)
        DenseMatrix.zeros[Double](cleanedUniqueLabeled.length, cleanedUniqueLabeled.length)
      else W.copy

      //                  println(s"uW: ${updatedW.rows} x ${updatedW.cols}")

      val startMetricUpdate = System.currentTimeMillis
      val updatedMetric =
        weightedMetric ++
          mln.evidence ++
          pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms))
      logger.info(msecTimeToTextUntilNow(s"Metric updated in: ", startMetricUpdate))

      // Labeled nodes MUST appear before unlabeled!
      new StreamingGraph(
        selectedNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        updatedMetric,
        annotationBuilder,
        updatedNodeCache,
        solver,
        edgeReWeighting,
        labelReWeighting,
        updatedStoredUnlabeled,
        updatedW,
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
