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

package lomrf.mln.learning.supervision.metric

import breeze.optimize.linear.KuhnMunkres

/**
  * A Matcher is any object that solves the assignment problem. The problem consists of finding
  * a maximum cost matching (or a minimum cost perfect matching) in a bipartite graph. The input
  * graph is usually represented as a cost matrix. Zero values define the absence of edges.
  *
  * === General Formulation ===
  *
  * Each problem instance has a number of agents and a number of tasks. Any agent can be assigned to
  * any task, incurring a cost that may vary depending on the agent-task assignment. It is required
  * that all tasks are assigned to exactly one agent in such a way that the total cost is minimized.
  * In case the numbers of agents and tasks are equal and the total cost of the assignment for all tasks
  * is equal to the sum of the costs for each agent then the problem is called the linear assignment problem.
  *
  * @see https://en.wikipedia.org/wiki/Assignment_problem
  */
trait Matcher[T] extends (CostMatrix[T] => T)

/**
  * The Hungarian matcher is a combinatorial optimization algorithm that solves the assignment problem in
  * polynomial time O(n&#94;3).
  *
  * @see https://en.wikipedia.org/wiki/Hungarian_algorithm
  */
object HungarianMatcher extends Matcher[Double] {

  /**
    * It solves the assignment problem for the given cost matrix. The cost
    * matrix represents the costs for each edge in the graph.
    *
    * @param costMatrix the bipartite graph cost matrix
    * @return the cost of the optimal assignment
    */
  override def apply(costMatrix: CostMatrix[Double]): Double = {
    val unmatched = math.abs(costMatrix.length - costMatrix.head.length)
    val maxDimension = math.max(costMatrix.length, costMatrix.head.length)

    KuhnMunkres.extractMatching(costMatrix) match {
      case (_, cost) => (cost + unmatched) / maxDimension
    }
  }
}

/**
  * The Hausdorff matcher is based on the Hausdorff distance. The Hausdorff distance is the longest distance
  * you can be forced to travel by an adversary that chooses a point in one set, from where you then must travel
  * to the other set. In other words, it is the greatest of all the distances from a point in one set to the
  * closest point in another set.
  *
  * @note The Hausdorff matcher can be used for solving the assignment problem, but the solution is not
  *       guaranteed to be the optimal one. Moreover, the matching is not guaranteed to be one to one.
  * @see https://en.wikipedia.org/wiki/Hausdorff_distance
  *      Distance Between Herbrand Interpretations: A Measure for Approximations
  *      to a Target Concept (1997)
  */
object HausdorffMatcher extends Matcher[Double] {

  /**
    * It solves the assignment problem for a given cost matrix. The cost
    * matrix represents the costs for each edge in the graph.
    *
    * @param costMatrix the bipartite graph cost matrix
    * @return the cost of the assignment
    */
  override def apply(costMatrix: CostMatrix[Double]): Double =
    math.max(costMatrix.map(_.min).max, costMatrix.transpose.map(_.min).max)
}
