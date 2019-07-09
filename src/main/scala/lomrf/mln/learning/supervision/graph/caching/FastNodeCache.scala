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
  def get(node: Node): Option[Long] = Option(data.get(node))

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
  def contains(node: Node): Boolean = data.contains(node)

  /**
    * Add a node to the cache.
    *
    * @param node a node to be added
    * @return a new node cache that contains all nodes of the current cache
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
    * Add a sequence of nodes to the cache.
    *
    * @param nodes a sequence of nodes
    * @return a new node cache containing all nodes of the current cache
    *         along the given sequence of nodes.
    */
  def ++(nodes: Seq[Node]): FastNodeCache = {
    for (node <- nodes) this + node
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

    if (marked.nonEmpty) {

      val reComputeUnique = marked.exists { node =>
        !noisy.contains(node) && {
          val x = data(node)
          val y = data(node.opposite)
          val N = x + y
          HoeffdingBound(x.toDouble / N, y.toDouble / N, N)
        }
      }

      if (reComputeUnique) {
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
