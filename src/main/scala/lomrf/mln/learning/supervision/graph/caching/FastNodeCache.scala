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

import gnu.trove.map.hash.TCustomHashMap
import gnu.trove.set.hash.TCustomHashSet
import lomrf.logic.AtomSignature
import lomrf.mln.learning.supervision.graph.HoeffdingBound
import lomrf.mln.learning.supervision.graph.Node
import scala.collection.convert.ImplicitConversionsToScala._

final class FastNodeCache private (
    override val querySignature: AtomSignature,
    data: TCustomHashMap[Node, Long],
    marked: TCustomHashSet[Node],
    noisy: TCustomHashSet[Node]) extends NodeCache {

  private var _uniqueNodes = IndexedSeq.empty[Node]
  private var _reComputeUnique = false

  /**
    * @return the number of unique nodes in the cache
    */
  def size: Int = data.size

  /**
    * @return the number of positive unique nodes in the cache
    */
  def numberOfPositive: Int = data.count { case (node, _) => node.isPositive }

  /**
    * @return the number of negative unique nodes in the cache
    */
  def numberOfNegative: Int = data.count { case (node, _) => node.isNegative }

  /**
    * @param node a node
    * @return an Option value containing the counts of the given node, or None
    *         if the node does not exist in the cache.
    */
  def get(node: Node): Option[Long] = if (node.isLabeled) Option(data.get(node)) else None

  /**
    * @param node a node
    * @return the counts of the given node or the result of the default computation
    *         if the node does not exist in the cache.
    */
  def getOrElse(node: Node, default: => Long): Long = get(node).getOrElse(default)

  /**
    * @param node a node
    * @return true if the node exists in the cache
    */
  def contains(node: Node): Boolean = if (node.isLabeled) data.contains(node) else false

  /**
    * @return true if the cache has changed, false otherwise
    */
  def hasChanged: Boolean = _reComputeUnique

  /**
    * Add a node to the cache.
    *
    * @param node a node to be added
    * @return a node cache that contains all nodes of the current cache
    *         along the given node.
    */
  def +(node: Node): FastNodeCache = {
    Option(data.get(node)) match {

      case Some(count) if node.isPositive =>
        data.put(node, count + 1)
        if (data.contains(node.opposite)) marked.add(node)

      case Some(count) if node.isNegative =>
        data.put(node, count + 1)
        if (data.contains(node.opposite)) marked.add(node)

      case None if node.isPositive =>
        data.put(node, 1)
        if (data.contains(node.opposite)) marked.add(node)
        _uniqueNodes :+= node

      case None if node.isNegative =>
        data.put(node, 1)
        if (data.contains(node.opposite)) marked.add(node)
        _uniqueNodes :+= node

      case _ => logger.warn("Node is unlabelled. Ignoring node.")
    }

    this
  }

  /**
    * Remove a node from the cache.
    *
    * @param node a node to be removed
    * @return a node cache that contains all nodes of the current cache
    *         except the given node.
    */
  def -(node: Node): FastNodeCache = {
    data.remove(node)
    noisy.remove(node)
    noisy.remove(node.opposite)
    marked.remove(node)
    marked.remove(node.opposite)
    _reComputeUnique = true
    this
  }

  /**
    * Add a sequence of nodes to the cache.
    *
    * @param nodes a sequence of nodes
    * @return a node cache containing all nodes of the current cache
    *         along the given sequence of nodes.
    */
  def ++(nodes: Seq[Node]): FastNodeCache = {
    for (node <- nodes) this + node
    this
  }

  /**
    * Remove a sequence of nodes to the cache.
    *
    * @param nodes a sequence of nodes
    * @return a node cache containing all nodes of the current cache
    *         except the given sequence of nodes.
    */
  def --(nodes: Seq[Node]): FastNodeCache = {
    for (node <- nodes) this - node
    this
  }

  /**
    * Collects all unique nodes.
    *
    * @return all unique nodes in the cache
    */
  def collectNodes: IndexedSeq[Node] = {

      @inline def isNoisy(node: Node): Boolean = {
        val x = data(node)
        val y = data(node.opposite)
        val N = x + y
        HoeffdingBound(x.toDouble / N, y.toDouble / N, N) && x < y
      }

    if (marked.nonEmpty || _reComputeUnique) {

      _reComputeUnique = _reComputeUnique || marked.exists { node =>
        !noisy.contains(node) && {
          val x = data(node)
          val y = data(node.opposite)
          val N = x + y
          HoeffdingBound(x.toDouble / N, y.toDouble / N, N)
        }
      }

      if (_reComputeUnique) {
        _reComputeUnique = false
        noisy.clear()
        _uniqueNodes = data.flatMap {
          case (node, _) =>
            if (marked.contains(node) && isNoisy(node)) {
              noisy.add(node)
              None
            } else Some(node)
        }.toIndexedSeq
      }
    }

    _uniqueNodes
  }

  override def toString: String =
    data.map { case (node, freq) => s"${node.clause.get.toText()} -> $freq" }.mkString("\n")
}

object FastNodeCache {

  def apply(querySignature: AtomSignature) = new FastNodeCache(
    querySignature,
    new TCustomHashMap(new ClauseStrategy),
    new TCustomHashSet(new BodyStrategy),
    new TCustomHashSet(new BodyStrategy)
  )
}
