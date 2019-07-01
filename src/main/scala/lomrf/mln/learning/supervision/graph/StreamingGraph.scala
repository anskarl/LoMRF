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

import breeze.linalg.{ Axis, DenseMatrix, DenseVector, sum }
import com.typesafe.scalalogging.LazyLogging
import lomrf.logic.{ AtomSignature, AtomicFormula, EvidenceAtom, FALSE, TRUE }
import lomrf.mln.learning.supervision.graph.caching.{ FastNodeCache, NodeCache }
import lomrf.mln.learning.supervision.metric.{ EvidenceMetric, Metric }
import lomrf.mln.model.{ Evidence, EvidenceBuilder, EvidenceDB, MLN, ModeDeclarations }
import lomrf.util.time.msecTimeToTextUntilNow

final class StreamingGraph private (
    currentUnlabeledNodes: IndexedSeq[Node],
    storedLabeled: IndexedSeq[Node],
    storedUnlabeled: IndexedSeq[Node],
    querySignature: AtomSignature,
    connector: GraphConnector,
    metric: Metric[_ <: AtomicFormula],
    supervisionBuilder: EvidenceBuilder,
    adjacencyMatrix: AdjacencyMatrix,
    memory: Int,
    nodeCache: NodeCache) extends LazyLogging {

  private var W = adjacencyMatrix

  def completeSupervisionGraphCut: (Set[EvidenceAtom], Evidence) =
    if (currentUnlabeledNodes.isEmpty) { // TODO: There should also be a case where unlabeled is not empty but labeled are.
      logger.warn("No new unlabeled nodes found")
      (Set.empty[EvidenceAtom], supervisionBuilder.result())
    } else (graphCut, supervisionBuilder.result())

  private def graphCut: Set[EvidenceAtom] = {

    logger.info(
      s"Supervision graph has ${storedLabeled.length} labeled nodes and ${storedUnlabeled.length} unlabeled.\n" +
        s"\t\t- Stored labeled Nodes: ${storedLabeled.length}\n" +
        s"\t\t- New unlabeled Nodes: ${currentUnlabeledNodes.length}\n" +
        s"\t\t- Query Signature: $querySignature"
    )

    //storedLabeled.map(_.clause.get.toText()).foreach(println)

    //currentUnlabeledNodes.map(_.body.get).foreach(c => println(c.toText()))

    val allLabeled = storedLabeled
    val unlabeledNodes = currentUnlabeledNodes
    //val allUnlabeled = storedUnlabeled ++ unlabeledNodes

    if (unlabeledNodes.nonEmpty) {
      W = DenseMatrix.horzcat(
        DenseMatrix.vertcat(W, DenseMatrix.zeros[Double](unlabeledNodes.length, W.rows)),
        DenseMatrix.zeros[Double](W.cols + unlabeledNodes.length, unlabeledNodes.length)
      )
    }
    //println("After extending the matrix for new unlabeled:\n" + W + "\n")

    // Second, add rows and cols for the current unlabeled nodes
    for (i <- unlabeledNodes.indices) {
      val x = unlabeledNodes(i)
      val idx = i + (2 + storedUnlabeled.length)
      val neighborCosts = DenseVector.zeros[Double](allLabeled.length)

      // connect to labeled nodes cluster (0 and 1)
      for (j <- allLabeled.indices) {
        val y = allLabeled(j)
        val s = 1 - {
          metric match {
            case m: EvidenceMetric        => m.distance(x.evidence, y.evidence)
            case m: Metric[AtomicFormula] => m.distance(x.atoms, y.atoms)
          }
        }

        neighborCosts(j) = s
      }

      // select the best labelled neighbors before adding the parallel edges
      val sparse = connector.sparse(neighborCosts)

      allLabeled.zip(sparse.toArray).foreach {
        case (n, s) =>
          if (n.isNegative) {
            W(idx, 0) += s
            W(0, idx) += s
          } else {
            W(idx, 1) += s
            W(1, idx) += s
          }
      }

      // connect to previously stored unlabeled
      for (j <- storedUnlabeled.indices) {
        val y = storedUnlabeled(j)

        val timeAdjacent = math.abs(x.query.terms.last.symbol.toLong - y.query.terms.last.symbol.toLong) == 1

        if (timeAdjacent) {
          val s = 1 - {
            metric match {
              case m: EvidenceMetric        => m.distance(x.evidence, y.evidence)
              case m: Metric[AtomicFormula] => m.distance(x.atoms, y.atoms)
            }
          }
          W(idx, j + 2) = s
          W(j + 2, idx) = s
        }
      }

      for (j <- unlabeledNodes.indices) {
        val y = unlabeledNodes(j)
        val idx1 = j + (2 + storedUnlabeled.length)

        val timeAdjacent = math.abs(x.query.terms.last.symbol.toLong - y.query.terms.last.symbol.toLong) == 1

        if (idx != idx1 && timeAdjacent) {
          val s = 1 - {
            metric match {
              case m: EvidenceMetric        => m.distance(x.evidence, y.evidence)
              case m: Metric[AtomicFormula] => m.distance(x.atoms, y.atoms)
            }
          }
          W(idx, idx1) = s
          W(idx1, idx) = s
        }
      }
    }

    //println("After adding new unlabeled nodes:\n" + W + "\n")

    // Third, delete old nodes that do not fit into memory (apply S. complement)
    if (W.cols - 2 > memory) {
      //println(s"${W.cols - 2} is greater than $memory. Delete the oldest ${W.cols - 2 - memory} unlabeled")

      (1 to W.cols - 2 - memory).foreach { _ =>
        val degree = W(2, ::).inner.toArray.sum

        for (n <- 0 until W.rows if n != 2) {
          for (m <- 0 until W.cols if m != 2) {
            if (n != m && ((n > 2 && m < 2) || (n < 2 && m > 2))) {
              W(n, m) = W(n, m) + (W(n, 2) * W(2, m) / degree)
              W(m, n) = W(m, n) + (W(n, 2) * W(2, m) / degree)
            }
          }
        }

        W = W.delete(2, Axis._0).delete(2, Axis._1)
        // Move to next
      }
    }

    //println("After deleting old unlabeled nodes:\n" + W + "\n")

    val D = DenseMatrix.zeros[Double](W.rows, W.cols)
    for (i <- 0 until W.rows) {
      D(i, i) = sum(W(i, ::))
    }

    //println("Degree matrix:\n" + D + "\n")

    val startSolution = System.currentTimeMillis

    // Vector holding the labeled values
    val fl = DenseVector(-1d, 1d)

    val solution = GraphOps.HFc(W, D, fl).toArray
    val truthValues = solution.map(value => if (value <= UNCONNECTED) FALSE else TRUE)

    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    //println(solution.deep)

    logger.whenDebugEnabled {
      logger.debug {
        (unlabeledNodes.map(_.query) zip solution.takeRight(unlabeledNodes.length))
          .map { case (atom, state) => s"$atom = $state" }.mkString("\n")
      }
    }

    val labeledEvidenceAtoms = unlabeledNodes.zip(truthValues.takeRight(unlabeledNodes.length))
      .flatMap {
        case (node, value) => node.labelUsingValue(value)
      }

    supervisionBuilder.evidence ++= labeledEvidenceAtoms
    labeledEvidenceAtoms.toSet
  }

  private def HoeffdingFilter(x: Double, y: Double): Boolean = {
    val N = x + y
    val fx = x / N
    val fy = y / N
    HoeffdingBound(fx, fy, N.toLong) && x < y
  }

  def ++(mln: MLN, annotationDB: EvidenceDB, modes: ModeDeclarations): StreamingGraph = {

    // Group the given data into nodes, using the domains of the existing graph
    val currentNodes = SupervisionGraph.partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = false)

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
      if (storedUnlabeled.length + currentUnlabeledNodes.length > memory)
        storedUnlabeled.drop(storedUnlabeled.length + currentUnlabeledNodes.length - memory) ++ currentUnlabeledNodes
      else storedUnlabeled ++ currentUnlabeledNodes

    /*
     * In case no labeled nodes exist in the given data, then reuse the old ones. In any other
     * case try to separate old labeled nodes that are dissimilar to the ones in the current batch
     * of data (the current supervision graph). Moreover remove noisy nodes using the Hoeffding bound.
     */
    if (labeled.isEmpty)
      new StreamingGraph(
        nonEmptyUnlabeled,
        storedLabeled,
        updatedStoredUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence ++ currentNodes.map(_.atoms),
        annotationBuilder,
        W.copy, // copy avoids memory leak
        memory,
        nodeCache)
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
      logger.info(s"${cleanedUniqueLabeled.length}/${storedLabeled.length + labeled.length} unique labeled nodes kept.")
      logger.debug(updatedNodeCache.toString)

      // Labeled nodes MUST appear before unlabeled!
      new StreamingGraph(
        nonEmptyUnlabeled,
        cleanedUniqueLabeled,
        updatedStoredUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence ++ currentNodes.map(_.atoms),
        annotationBuilder,
        W.copy,
        memory,
        updatedNodeCache)
    }
  }
}

object StreamingGraph extends LazyLogging {

  def apply(
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature,
      connector: GraphConnector,
      metric: Metric[_ <: AtomicFormula],
      memory: Int): StreamingGraph = {

    // Group the given data into nodes
    val nodes = SupervisionGraph.partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = false)

    logger.info("Constructing supervision graph.")

    // Partition nodes into labeled and unlabeled. Then find empty unlabeled nodes.
    val (labeledNodes, unlabeledNodes) = nodes.partition(_.isLabeled)
    val (nonEmptyUnlabeled, emptyUnlabeled) = unlabeledNodes.partition(_.nonEmpty)

    /*
     * Create a cache using only non empty labeled nodes, i.e., nodes having at least
     * one evidence predicate in their body.
     *
     * Cache stores only unique nodes (patterns) along their counts.
     */
    val startCacheConstruction = System.currentTimeMillis

    val nodeCache = FastNodeCache(querySignature) ++ labeledNodes.filter(_.nonEmpty)
    val uniqueLabeled = nodeCache.collectNodes

    logger.info(msecTimeToTextUntilNow(s"Cache constructed in: ", startCacheConstruction))
    logger.info(s"${uniqueLabeled.length} / ${labeledNodes.length} unique labeled nodes kept.")
    logger.debug(nodeCache.toString)

    // Labeled query atoms and empty unlabeled query atoms as FALSE.
    val labeledEntries =
      labeledNodes.map(_.query) ++ emptyUnlabeled.flatMap(_.labelUsingValue(FALSE))

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

    new StreamingGraph(
      nonEmptyUnlabeled,
      uniqueLabeled,
      IndexedSeq.empty,
      querySignature,
      connector,
      metric ++ mln.evidence ++ nodes.map(_.atoms),
      annotationBuilder,
      DenseMatrix.zeros[Double](2, 2), // One entry for each labeled/unlabeled cluster of nodes
      memory,
      nodeCache
    )
  }
}
