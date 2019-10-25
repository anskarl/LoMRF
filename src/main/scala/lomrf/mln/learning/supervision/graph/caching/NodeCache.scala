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

package lomrf.mln.learning.supervision.graph.caching

import com.typesafe.scalalogging.LazyLogging
import lomrf.logic.AtomSignature
import lomrf.mln.learning.supervision.graph.Node

trait NodeCache extends LazyLogging {

  val useHoeffdingBound: Boolean
  val querySignature: AtomSignature

  /**
    * @return the number of unique nodes in the cache
    */
  def size: Int

  /**
    * @return the number of positive unique nodes in the cache
    */
  def numberOfPositive: Int

  /**
    * @return the number of negative unique nodes in the cache
    */
  def numberOfNegative: Int

  /**
    * @param node a node
    * @return an Option value containing the counts of the given node, or None
    *         if the node does not exist in the cache.
    */
  def get(node: Node): Option[Long]

  /**
    * @param node a node
    * @return the counts of the given node or the result of the default computation
    *         if the node does not exist in the cache.
    */
  def getOrElse(node: Node, default: => Long): Long

  /**
    * @param node a node
    * @return true if the node exists in the cache
    */
  def contains(node: Node): Boolean

  /**
    * Add a node to the cache.
    *
    * @param node a node to be added
    * @return a node cache that contains all nodes of the current cache
    *         along the given node.
    */
  def +(node: Node): NodeCache

  /**
    * Remove a node from the cache.
    *
    * @param node a node to be removed
    * @return a node cache that contains all nodes of the current cache
    *         except the given node.
    */
  def -(node: Node): NodeCache

  /**
    * Add a sequence of nodes to the cache.
    *
    * @param nodes a sequence of nodes
    * @return a node cache containing all nodes of the current cache
    *         along the given sequence of nodes.
    */
  def ++(nodes: Seq[Node]): NodeCache

  /**
    * Remove a sequence of nodes to the cache.
    *
    * @param nodes a sequence of nodes
    * @return a node cache containing all nodes of the current cache
    *         except the given sequence of nodes.
    */
  def --(nodes: Seq[Node]): NodeCache

  /**
    * Collects all unique nodes.
    *
    * @return all unique nodes in the cache
    */
  def collectNodes: IndexedSeq[Node]
}
