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

package lomrf.mln.learning.supervision.graphs

import breeze.linalg.{ DenseVector, argtopk }

/**
  * A GraphConnector is a higher order function that changes the number of connected
  * neighbors (edges) to the vertex according to a strategy. The edges are represented by a
  * vector containing their costs.
  */
trait GraphConnector extends (DenseVector[Double] => DenseVector[Double])

/**
  * A FullConnector is used to essentially construct a fully connected graph. Therefore,
  * for a given cost vector of a vertex it changes nothing.
  */
object FullConnector extends GraphConnector {

  /**
    * @param neighbors a vector containing the costs for each neighbor of the vertex
    *
    * @return the vector itself
    */
  override def apply(neighbors: DenseVector[Double]): DenseVector[Double] = neighbors

  override def toString(): String = s"full"
}

/**
  * A kNNConnector is used to essentially construct a kNN graph. For the neighbors of
  * the vertex it only keeps the top k nearest neighbors by setting everything else to zero.
  *
  * @note kNN graphs having small k tends to perform better
  *
  * @param k the number of nearest neighbors to be retained
  */
final case class kNNConnector(k: Int) extends GraphConnector {

  /**
    * @param neighbors a vector containing the costs for each neighbor of the vertex
    *
    * @return a vector having costs only for the k nearest neighbors (top k costs),
    *         everything else is unconnected (zero)
    */
  override def apply(neighbors: DenseVector[Double]): DenseVector[Double] = {
    // find distinct costs in the neighbor vector
    val distinctCosts = DenseVector(neighbors.toArray.distinct)

    if (distinctCosts.length > k) {
      val topK = argtopk(distinctCosts, k).map(distinctCosts.apply)
      neighbors.map {
        cost => if (topK.contains(cost)) cost else UNCONNECTED
      }
    } else neighbors
  }

  override def toString(): String = s"kNN.$k"
}

/**
  * A eNNConnector is used to essentially construct an eNN graph. For the neighbors of
  * the vertex it only keeps the ones having cost greater than a given epsilon value.
  *
  * @param epsilon the threshold epsilon
  */
final case class eNNConnector(epsilon: Double) extends GraphConnector {

  /**
    * @param neighbors a vector containing the costs for each neighbor of a vertex
    *
    * @return a vector having retained only costs that are greater than epsilon,
    *         everything else unconnected (zero)
    */
  override def apply(neighbors: DenseVector[Double]): DenseVector[Double] =
    neighbors.map(cost => if (cost < epsilon) UNCONNECTED else cost)

  override def toString(): String = s"eNN.$epsilon"
}
