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

import lomrf.logic.AtomicFormula
import breeze.linalg.{ Axis, DenseMatrix, DenseVector, argtopk, sum }
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.metric.{ EvidenceMetric, Metric }
import com.typesafe.scalalogging.LazyLogging
import spire.syntax.cfor._

/**
  * A graph connector constructs a graph and changes the number of connected
  * neighbors (edges) to each vertex according to a given strategy.
  */
trait GraphConnector extends LazyLogging {

  /**
    * @param neighbors a vector containing the edge values of neighboring nodes
    * @param L number of labeled neighbors
    * @return a sparser vector containing the retained neighbor edges
    */
  def makeSparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double]

  /**
    * @param neighbors a vector containing the edge values of neighboring nodes
    * @return a sparser vector containing the retained neighbor edges
    */
  def makeSparse(neighbors: Array[Double]): DenseVector[Double] = makeSparse(DenseVector(neighbors))

  /**
    * Compute the edge value for a pair of nodes.
    *
    * @param x a node
    * @param y another node
    * @param metric a metric for atomic formula
    * @return the edge value for the given nodes
    */
  def connect(x: Node, y: Node)(metric: Metric[_ <: AtomicFormula]): Double = 1 - {
    metric match {
      case m: EvidenceMetric        => m.distance(x.evidence, y.evidence)
      case m: Metric[AtomicFormula] => m.distance(x.atoms, y.atoms)
    }
  }

  /**
    * Connect graph faster by specifying the unlabeled nodes.
    *
    * @note The graph may become sparser by using the appropriate connector type.
    *
    * @param nodes a sequence of nodes
    * @param unlabeled a sequence of unlabeled nodes
    * @param metric a metric for atomic formula
    * @return the adjacency and degree matrix of the resulted graph
    */
  def smartConnect(
      nodes: IndexedSeq[Node],
      unlabeled: IndexedSeq[Node],
      nodeCache: Option[NodeCache] = None)(metric: Metric[_ <: AtomicFormula]): EncodedGraph = {

    val numberOfNodes = nodes.length
    val parallelIndices = nodes.indices.par
    val L = nodes.count(_.isLabeled)

    val W = DenseMatrix.fill[Double](numberOfNodes, numberOfNodes)(UNCONNECTED)
    val D = DenseMatrix.zeros[Double](numberOfNodes, numberOfNodes)

    cfor(0)(_ < unlabeled.length, _ + 1) { ii =>
      val i = ii + L

      logger.debug {
        s"""
          |===== Current Unlabeled =====
          |${unlabeled(ii).toText} -> ${unlabeled(ii).query.terms.last.symbol}
          |=============================
          |""".stripMargin
      }

      for (j <- parallelIndices if i != j) { // A node cannot be connected to itself

        // W is symmetric and therefore avoid computing both upper and lower triangular
        if (j < L) {
          W(i, j) = connect(nodes(i), nodes(j))(metric)
          W(j, i) = W(i, j)
        } else {
          if (i > j) W(i, j) = W(j, i)
          else W(i, j) = connect(nodes(i), nodes(j))(metric)
        }

        logger.whenDebugEnabled {
          if (W(i, j) > 0) {
            val counts = nodeCache.map(_.getOrElse(nodes(j), 0)).getOrElse(1)
            logger.debug {
              s"""
                |${nodes(j).toText} [$counts] -> ${nodes(j).query.terms.last.symbol}
                |Similarity: ${W(i, j)}
                |""".stripMargin
            }
          }
        }

      }
    }

    val countsPerNode = nodeCache.map { cache =>
      DenseVector(nodes.map(cache.getOrElse(_, 1).toDouble).toArray) ->
        nodes.map(cache.getOrElse(_, 0).toDouble).toArray
    }

    for (i <- parallelIndices) {
      W(i, ::).t := makeSparse(W(i, ::).t, L)

      countsPerNode.map {
        case (counts, labeledCounts) =>
          val div = W(i, ::).t.toArray.zip(labeledCounts)
            .withFilter { case (w, _) => w > 0 }
            .map { case (_, count) => count }.sum

          W(i, ::).t := W(i, ::).t *:* counts
          if (div != 0) W(i, ::).t := W(i, ::).t / div
      }

      D(i, i) = sum(W(i, ::))
    }

    W -> D // return the final encoded graph
  }

  /**
    * Fully connect a graph.
    *
    * @note The graph may become sparser by using the appropriate connector type.
    *
    * @param nodes a sequence of nodes
    * @param metric a metric for atomic formula
    * @return the adjacency and degree matrix of the resulted graph
    */
  def fullyConnect(
      nodes: IndexedSeq[Node],
      nodeCache: Option[NodeCache] = None)(metric: Metric[_ <: AtomicFormula]): EncodedGraph = {

    val numberOfNodes = nodes.length
    val parallelIndices = nodes.indices.par
    val L = nodes.count(_.isLabeled)

    val W = DenseMatrix.fill[Double](numberOfNodes, numberOfNodes)(UNCONNECTED)
    val D = DenseMatrix.zeros[Double](numberOfNodes, numberOfNodes)

    cfor(0)(_ < numberOfNodes, _ + 1) { i =>
      for (j <- parallelIndices if i != j) { // A node cannot be connected to itself

        // W is symmetric and therefore avoid computing both upper and lower triangular
        if (i > j) W(i, j) = W(j, i)
        else W(i, j) = connect(nodes(i), nodes(j))(metric)
      }
    }

    val countsPerNode = nodeCache.map { cache =>
      DenseVector(nodes.map(cache.getOrElse(_, 1).toDouble).toArray) ->
        nodes.map(cache.getOrElse(_, 0).toDouble).toArray
    }

    for (i <- parallelIndices) {
      W(i, ::).t := makeSparse(W(i, ::).t, L)

      countsPerNode.map {
        case (counts, labeledCounts) =>
          val div = W(i, ::).t.toArray.zip(labeledCounts)
            .withFilter { case (w, _) => w > 0 }
            .map { case (_, count) => count }.sum

          W(i, ::).t := W(i, ::).t *:* counts
          if (div != 0) W(i, ::).t := W(i, ::).t / div
      }

      D(i, i) = sum(W(i, ::))
    }

    W -> D // return the final encoded graph
  }

  /**
    * Connect a bi-graph using the given sequences of nodes.
    *
    * @note The graph may become sparser by using the appropriate connector type.
    *
    * @param leftNodes a sequence of nodes
    * @param rightNodes another sequence of nodes
    * @param metric a metric for atomic formula
    * @return the adjacency matrix of the resulted graph
    */
  def biConnect(leftNodes: IndexedSeq[Node], rightNodes: IndexedSeq[Node])(metric: Metric[_ <: AtomicFormula]): GraphMatrix = {

    val numberOfCols = rightNodes.length
    val parallelIndices = rightNodes.indices.par
    val W = DenseMatrix.fill[Double](leftNodes.length, rightNodes.length)(UNCONNECTED)

    cfor(0)(_ < leftNodes.length, _ + 1) { i =>
      val neighborCosts = DenseVector.zeros[Double](numberOfCols)
      for (j <- parallelIndices if i != j) // A node cannot be connected to itself
        neighborCosts(j) = connect(leftNodes(i), rightNodes(j))(metric)

      W(i, ::).t := makeSparse(neighborCosts, numberOfCols)
    }
    W // return the final (fully connected) adjacency matrix
  }

  /**
    * Creates a synopsis of the given graph matrix by keeping only
    * the first start nodes plus the latest end nodes.
    *
    * @param W a weighted graph matrix
    * @param start retain the first 'start' nodes
    * @param end retain the latest 'end' nodes
    * @return a synopsis graph containing only start + end nodes
    */
  def synopsisOf(W: GraphMatrix, start: Int, end: Int): GraphMatrix = {
    var reducedGraph = W
    for (_ <- 1 to W.cols - (start + end)) { // for all nodes out of memory

      // compute degree of the oldest node (that is start, since arrays begin at 0)
      val degree = sum(reducedGraph(start, ::))
      logger.debug(s"Degree of current node [$start] is $degree.")

      if (degree != 0) for {
        i <- 0 until reducedGraph.cols if i != start
        j <- 0 until reducedGraph.cols if j != start
        if i < j && (i > start || j > start)
      } {
        reducedGraph(i, j) += (reducedGraph(i, start) * reducedGraph(start, j)) / degree
        reducedGraph(j, i) += (reducedGraph(start, i) * reducedGraph(j, start)) / degree

        logger.debug {
          s"""
             |($i, $start) * ($start, $j) / degree :=
             |${reducedGraph(i, start)} * ${reducedGraph(start, j)} / $degree := ${reducedGraph(i, j)}
             |
             |($start, $i) * ($j, $start) / degree :=
             |${reducedGraph(start, i)} * ${reducedGraph(j, start)} / $degree := ${reducedGraph(j, i)}
          """.stripMargin
        }
      }

      // delete oldest node, denoted by the start pointer
      reducedGraph = reducedGraph.delete(start, Axis._0).delete(start, Axis._1)
    }
    reducedGraph
  }
}

/**
  * A full connector is used to construct a fully connected graph.
  */
object FullConnector extends GraphConnector {

  /**
    * Retains all neighbors.
    *
    * @param neighbors a vector containing the edge values of neighboring nodes
    * @param L number of labeled neighbors
    * @return the vector itself (retains all neighbor edges(
    */
  override def makeSparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = neighbors

  override def toString: String = "full"
}

/**
  * A aNN connector is used to construct an aNN graph. For each vertex it
  * only keeps the nearest neighbors representing the 1/3 of the total mass
  * by setting everything else to zero.
  */
object aNNConnector extends GraphConnector {

  /**
    * Retain nearest neighbors adaptively.
    *
    * @param neighbors a vector containing the edge values of neighboring nodes
    * @param L number of labeled neighbors
    * @return a vector holding costs only for the nearest neighbors representing the
    *         1/3 of the total mass, everything else is unconnected (zero)
    */
  override def makeSparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = {

    val distinctCosts = neighbors.toArray.distinct
    val distinctVector = DenseVector(distinctCosts)
    val sortedCosts = distinctCosts.sortWith(_ > _)
    val summed = sortedCosts.sum
    val normalizedCosts = sortedCosts.map(_ / summed)

    var k = 1
    while (k < sortedCosts.length && normalizedCosts.take(k).sum < 0.33)
      k += 1

    if (distinctVector.length > k) {
      val topK = argtopk(distinctVector, k).map(distinctVector.apply)
      neighbors.map {
        cost => if (topK.contains(cost)) cost else UNCONNECTED
      }
    } else neighbors
  }

  override def toString: String = "ann"
}

/**
  * A aNNL connector is used to construct an aNN graph on the labeled vertices.
  * For each vertex it only keeps the labeled nearest neighbors representing the
  * 1/3 of the total mass by setting everything else to zero.
  */
class aNNLConnector extends GraphConnector {

  /**
    * Retains the k labeled nearest neighbors. All unlabeled neighbors remain connected.
    *
    * @param neighbors a vector containing the edge values of neighboring nodes
    * @param L number of labeled neighbors
    * @return a sparser vector containing only for the k labeled nearest neighbors (top k costs),
    *         everything else is unconnected (zero). Unlabeled neighbors remain connected
    */
  override def makeSparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = {

    val distinctCosts = neighbors.toArray.take(L).distinct
    val distinctVector = DenseVector(distinctCosts)
    val sortedCosts = distinctCosts.sortWith(_ > _)
    val summed = sortedCosts.sum
    val normalizedCosts = sortedCosts.map(_ / summed)

    var k = 1
    while (k < sortedCosts.length && normalizedCosts.take(k).sum < 0.33)
      k += 1

    if (distinctVector.length > k) {
      val topK = argtopk(distinctVector, k).map(distinctVector.apply)
      DenseVector.vertcat(
        neighbors.slice(0, L).map(cost => if (topK.contains(cost)) cost else UNCONNECTED),
        neighbors.slice(L, neighbors.length)
      )

    } else neighbors
  }

  override def toString: String = s"ann.lb"
}

/**
  * A temporal aNN connector is used to construct an aNN graph on the labeled vertices,
  * while unlabeled vertices are connected temporally (as a sequence). For each vertex
  * it only keeps the top k labeled nearest neighbors by setting everything else to zero.
  * The unlabeled vertices are connected as a chain.
  */
class aNNTemporalConnector extends aNNLConnector {

  /**
    * Compute the edge value for a pair of nodes. Pairs of unlabeled
    * nodes are only connected if they are time-adjacent.
    *
    * @note Override the method to implement another connection strategy.
    *
    * @param x a node
    * @param y another node
    * @param metric a metric for atomic formula
    * @return the edge value for the given nodes
    */
  override def connect(x: Node, y: Node)(metric: Metric[_ <: AtomicFormula]): Double = {

    val timeAdjacent = math.abs(x.query.terms.last.symbol.toLong - y.query.terms.last.symbol.toLong) == 1

    if (x.isUnlabeled && y.isUnlabeled && !timeAdjacent) UNCONNECTED
    else 1 - {
      metric match {
        case m: EvidenceMetric        => m.distance(x.evidence, y.evidence)
        case m: Metric[AtomicFormula] => m.distance(x.atoms, y.atoms)
      }
    }
  }

  override def toString: String = s"ann.temp"
}

/**
  * A kNN connector is used to construct a kNN graph. For each vertex it
  * only keeps the top k nearest neighbors by setting everything else to zero.
  *
  * @note kNN graphs having small k tends to perform better.
  *
  * @param k the number of nearest neighbors to be retained
  */
case class kNNConnector(k: Int) extends GraphConnector {

  /**
    * Retains the k nearest neighbors.
    *
    * @param neighbors a vector containing the edge values of neighboring nodes
    * @param L number of labeled neighbors
    * @return a sparser vector containing only for the k nearest neighbors (top k costs),
    *         everything else is unconnected (zero)
    */
  override def makeSparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = {

    // Find distinct costs in the neighbor vector
    val distinctCosts = DenseVector(neighbors.toArray.distinct)

    if (distinctCosts.length > k) {
      val topK = argtopk(distinctCosts, k).map(distinctCosts.apply)
      neighbors.map {
        cost => if (topK.contains(cost)) cost else UNCONNECTED
      }
    } else neighbors
  }

  override def toString: String = s"knn[$k]"
}

/**
  * A kNNL connector is used to construct a kNN graph on the labeled vertices.
  * For each vertex it only keeps the top k labeled nearest neighbors by setting
  * everything else to zero. The unlabeled vertices remain fully connected.
  *
  * @note kNN graphs having small k tends to perform better.
  *
  * @param k the number of nearest neighbors to be retained
  */
case class kNNLConnector(k: Int) extends GraphConnector {

  /**
    * Retains the k labeled nearest neighbors. All unlabeled neighbors remain connected.
    *
    * @param neighbors a vector containing the edge values of neighboring nodes
    * @param L number of labeled neighbors
    * @return a sparser vector containing only for the k labeled nearest neighbors (top k costs),
    *         everything else is unconnected (zero). Unlabeled neighbors remain connected
    */
  override def makeSparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = {

    // Find distinct costs in the labeled neighbor vector
    val distinctLabeledCosts = DenseVector(neighbors.toArray.take(L).distinct)

    if (distinctLabeledCosts.length > k) {
      val topK = argtopk(distinctLabeledCosts, k).map(distinctLabeledCosts.apply)

      DenseVector.vertcat(
        neighbors.slice(0, L).map(cost => if (topK.contains(cost)) cost else UNCONNECTED),
        neighbors.slice(L, neighbors.length)
      )
    } else neighbors
  }

  override def toString: String = s"knn.lb[$k]"
}

/**
  * A temporal kNN connector is used to construct a kNN graph on the labeled vertices,
  * while unlabeled vertices are connected temporally (as a sequence). For each vertex
  * it only keeps the top k labeled nearest neighbors by setting everything else to zero.
  * The unlabeled vertices are connected as a chain.
  *
  * @note kNN graphs having small k tends to perform better.
  *
  * @param k the number of nearest neighbors to be retained
  */
class kNNTemporalConnector(k: Int) extends kNNLConnector(k) {

  /**
    * Compute the edge value for a pair of nodes. Pairs of unlabeled
    * nodes are only connected if they are time-adjacent.
    *
    * @note Override the method to implement another connection strategy.
    *
    * @param x a node
    * @param y another node
    * @param metric a metric for atomic formula
    * @return the edge value for the given nodes
    */
  override def connect(x: Node, y: Node)(metric: Metric[_ <: AtomicFormula]): Double = {

    val timeAdjacent = math.abs(x.query.terms.last.symbol.toLong - y.query.terms.last.symbol.toLong) == 1

    if (x.isUnlabeled && y.isUnlabeled && !timeAdjacent) UNCONNECTED
    else 1 - {
      metric match {
        case m: EvidenceMetric        => m.distance(x.evidence, y.evidence)
        case m: Metric[AtomicFormula] => m.distance(x.atoms, y.atoms)
      }
    }
  }

  override def toString: String = s"knn.temp[$k]"
}

/**
  * A eNN connector is used to construct an eNN graph. For each vertex it
  * only keeps the neighbors having cost greater than a given epsilon value.
  *
  * @param epsilon the threshold epsilon
  */
final case class eNNConnector(epsilon: Double) extends GraphConnector {

  /**
    * Retains all epsilon nearest neighbors. All unlabeled neighbors remain connected.
    *
    * @param neighbors a vector containing the edge values of neighboring nodes
    * @param L number of labeled neighbors
    * @return a sparser vector containing only for the epsilon labeled nearest neighbors,
    *         everything else is unconnected (zero). Unlabeled neighbors remain connected
    */
  override def makeSparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] =
    neighbors.map(cost => if (cost < epsilon) UNCONNECTED else cost)

  override def toString: String = s"enn[$epsilon]"
}

/**
  * A eNN connector is used to construct an eNN graph on the labeled vertices.
  * For each vertex it only keeps the neighbors having cost greater than
  * a given epsilon value. The unlabeled neighbors remain fully connected.
  *
  * @param epsilon the threshold epsilon
  */
case class eNNLConnector(epsilon: Double) extends GraphConnector {

  /**
    * Retains all epsilon labeled nearest neighbors. All unlabeled neighbors remain connected.
    *
    * @param neighbors a vector containing the edge values of neighboring nodes
    * @param L number of labeled neighbors
    * @return a sparser vector containing only for the epsilon labeled nearest neighbors,
    *         everything else is unconnected (zero). Unlabeled neighbors remain connected
    */
  override def makeSparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = {
    DenseVector.vertcat(
      neighbors.slice(0, L).map(cost => if (cost < epsilon) UNCONNECTED else cost),
      neighbors.slice(L, neighbors.length)
    )
  }

  override def toString: String = s"enn.lb[$epsilon]"
}

class eNNTemporalConnector(epsilon: Double) extends eNNLConnector(epsilon) {

  override def connect(x: Node, y: Node)(metric: Metric[_ <: AtomicFormula]): Double = {

    val timeAdjacent = math.abs(x.query.terms.last.symbol.toLong - y.query.terms.last.symbol.toLong) == 1

    if (x.isUnlabeled && y.isUnlabeled && !timeAdjacent) UNCONNECTED
    else 1 - {
      metric match {
        case m: EvidenceMetric        => m.distance(x.evidence, y.evidence)
        case m: Metric[AtomicFormula] => m.distance(x.atoms, y.atoms)
      }
    }
  }

  override def toString: String = s"enn.temp[$epsilon]"
}
