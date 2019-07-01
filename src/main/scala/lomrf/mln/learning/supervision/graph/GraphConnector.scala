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
import breeze.linalg.{ DenseMatrix, DenseVector, argtopk, sum }
import lomrf.mln.learning.supervision.metric.{ Metric, EvidenceMetric }

/**
  * A graph connector constructs a graph and changes the number of connected
  * neighbors (edges) to each vertex according to a strategy.
  */
trait GraphConnector {

  /**
    * @param neighbors a vector containing the edge values of neighboring nodes
    * @param L number of labeled neighbors
    * @return a sparser vector containing the retained neighbor edges
    */
  def sparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double]

  /**
    * Compute the edge value for a pair of nodes. Pair of labeled
    * nodes are never connected.
    *
    * @note Override the method to implement another connection strategy.
    *
    * @param x a node
    * @param y another node
    * @param metric a metric for atomic formula
    * @return the edge value for the given nodes
    */
  def connect(x: Node, y: Node)(metric: Metric[_ <: AtomicFormula]): Double =
    if (!x.isLabeled || !y.isLabeled) 1 - { // connect nodes only if they are not both labeled
      metric match {
        case m: EvidenceMetric        => m.distance(x.evidence, y.evidence)
        case m: Metric[AtomicFormula] => m.distance(x.atoms, y.atoms)
      }
    }
    else UNCONNECTED

  /**
    * Connect a graph.
    *
    * @note The graph may become sparser by using the appropriate connector type.
    *
    * @param nodes a sequence of nodes
    * @param metric a metric for atomic formula
    * @return the adjacency and degree matrix of the resulted graph
    */
  def connect(nodes: IndexedSeq[Node], labeled: IndexedSeq[Node], unlabeled: IndexedSeq[Node])(metric: Metric[_ <: AtomicFormula]): EncodedGraph = {
    val numberOfNodes = nodes.length
    val parallelIndices = nodes.indices.par
    val L = labeled.length

    val W = DenseMatrix.fill[Double](numberOfNodes, numberOfNodes)(UNCONNECTED)
    val D = DenseMatrix.zeros[Double](numberOfNodes, numberOfNodes)

    for (ii <- unlabeled.indices) {
      val i = ii + L
      for (j <- parallelIndices if i != j) { // A node cannot be connected to itself

        // W is symmetric and therefore avoid computing both upper and lower triangular
        if (j < L) {
          W(i, j) = connect(nodes(i), nodes(j))(metric)
          W(j, i) = W(i, j)
        } else {
          if (i > j) W(i, j) = W(j, i)
          else W(i, j) = connect(nodes(i), nodes(j))(metric)
        }
      }
    }

    for (i <- parallelIndices) {
      W(i, ::).inner := sparse(W(i, ::).inner, L)
      D(i, i) = sum(W(i, ::))
    }

    W -> D // return the final encoded graph
  }
  /*def connect(nodes: IndexedSeq[Node])(metric: Metric[_ <: AtomicFormula]): EncodedGraph = {
    val numberOfNodes = nodes.length
    val parallelIndices = nodes.indices.par
    val L = nodes.count(_.isLabeled)

    val W = DenseMatrix.fill[Double](numberOfNodes, numberOfNodes)(UNCONNECTED)
    val D = DenseMatrix.zeros[Double](numberOfNodes, numberOfNodes)

    for (i <- nodes.indices) {
      val neighborCosts = DenseVector.zeros[Double](numberOfNodes)
      for (j <- parallelIndices if i != j) { // A node cannot be connected to itself

        // W is symmetric and therefore avoid computing both upper and lower triangular
        if (i > j) neighborCosts(j) = W(j, i)
        else neighborCosts(j) = connect(nodes(i), nodes(j))(metric)
      }
    }

    for (i <- parallelIndices) {
      W(i, ::).inner := sparse(W(i, ::).inner, L)
      D(i, i) = sum(W(i, ::))
    }

    W -> D // return the final encoded graph
  }*/

  /**
    * Connect a bi-graph using the given sequences of nodes.
    *
    * @note The graph may become sparser by using the appropriate connector type.
    *
    * @param nodesR a sequence of nodes
    * @param nodesC another sequence of nodes
    * @param metric a metric for atomic formula
    * @return the adjacency matrix of the resulted graph
    */
  def connect(nodesR: IndexedSeq[Node], nodesC: IndexedSeq[Node])(metric: Metric[_ <: AtomicFormula]): AdjacencyMatrix = {
    val numberOfCols = nodesC.length
    val parallelIndices = nodesC.indices.par
    val W = DenseMatrix.fill[Double](nodesR.length, numberOfCols)(UNCONNECTED)

    for (i <- nodesR.indices) {
      val neighborCosts = DenseVector.zeros[Double](numberOfCols)
      for (j <- parallelIndices if i != j) // A node cannot be connected to itself
        neighborCosts(j) = connect(nodesR(i), nodesC(j))(metric)

      W(i, ::).inner := sparse(neighborCosts, numberOfCols)
    }
    W // return the final (fully connected) adjacency matrix
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
  override def sparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = neighbors

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
  override def sparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = {

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

  override def toString: String = "aNN"
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
  override def sparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = {
    // find distinct costs in the neighbor vector
    val distinctCosts = DenseVector(neighbors.toArray.distinct)

    if (distinctCosts.length > k) {
      val topK = argtopk(distinctCosts, k).map(distinctCosts.apply)
      neighbors.map {
        cost => if (topK.contains(cost)) cost else UNCONNECTED
      }
    } else neighbors
  }

  override def toString: String = s"kNN.$k"
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
  override def sparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = {
    // find distinct costs in the labeled neighbor vector
    val distinctLabeledCosts = DenseVector(neighbors.toArray.take(L).distinct)

    if (distinctLabeledCosts.length > k) {
      val topK = argtopk(distinctLabeledCosts, k).map(distinctLabeledCosts.apply)

      DenseVector.vertcat(
        neighbors.slice(0, L).map(cost => if (topK.contains(cost)) cost else UNCONNECTED),
        neighbors.slice(L, neighbors.length)
      )
    } else neighbors
  }

  override def toString: String = s"kNN.$k.labeled"
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
    * Compute the edge value for a pair of nodes. Pair of labeled
    * nodes are never connected. Moreover unlabeled nodes are only
    * connected if they are time-adjacent.
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

    if ((x.isLabeled && y.isLabeled) || (x.isUnlabeled && y.isUnlabeled && !timeAdjacent)) UNCONNECTED
    else 1 - {
      metric match {
        case m: EvidenceMetric        => m.distance(x.evidence, y.evidence)
        case m: Metric[AtomicFormula] => m.distance(x.atoms, y.atoms)
      }
    }
  }

  override def toString: String = s"kNN.$k.temporal"
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
  override def sparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] =
    neighbors.map(cost => if (cost < epsilon) UNCONNECTED else cost)

  override def toString: String = s"eNN.$epsilon"
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
  override def sparse(neighbors: DenseVector[Double], L: Int = 0): DenseVector[Double] = {
    DenseVector.vertcat(
      neighbors.slice(0, L).map(cost => if (cost < epsilon) UNCONNECTED else cost),
      neighbors.slice(L, neighbors.length)
    )
  }

  override def toString: String = s"eNN.$epsilon.labeled"
}

class eNNTemporalConnector(epsilon: Double) extends eNNLConnector(epsilon) {

  override def connect(x: Node, y: Node)(metric: Metric[_ <: AtomicFormula]): Double = {

    val timeAdjacent = math.abs(x.query.terms.last.symbol.toLong - y.query.terms.last.symbol.toLong) == 1

    if ((x.isLabeled && y.isLabeled) || (x.isUnlabeled && y.isUnlabeled && !timeAdjacent)) UNCONNECTED
    else 1 - {
      metric match {
        case m: EvidenceMetric        => m.distance(x.evidence, y.evidence)
        case m: Metric[AtomicFormula] => m.distance(x.atoms, y.atoms)
      }
    }
  }

  override def toString: String = s"eNN.$epsilon.temporal"
}
