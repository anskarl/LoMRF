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

package lomrf.mln.learning.supervision.graph.clustering

import lomrf.mln.learning.supervision.graph.Node
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.metric.features.Feature

case class NodeCluster(prototype: Set[Feature], nodes: Set[Node], density: Long) {

  def isPositive: Boolean = nodes.forall(_.isPositive)

  def hasPositive: Boolean = nodes.exists(_.isPositive)

  def isNegative: Boolean = nodes.forall(_.isNegative)

  def hasNegative: Boolean = nodes.exists(_.isNegative)

  def isEmpty: Boolean = nodes.isEmpty

  def nonEmpty: Boolean = nodes.nonEmpty

  def contains(node: Node): Boolean = nodes.contains(node)

  def majorityPrototype(nodeCache: NodeCache): NodeCluster = {
    val reduced = prototype.flatMap { f =>
      val (contains, notContains) = nodes.partition(_.features.contains(f))
      if (contains.foldLeft(0L)((sum, x) => sum + nodeCache.getOrElse(x, 1L)) > notContains.foldLeft(0L)((sum, x) => sum + nodeCache.getOrElse(x, 1L)))
        Some(f)
      else None
    }
    new NodeCluster(reduced, nodes, density)
  }

  def +(node: Node, nodeCache: Option[NodeCache]): NodeCluster =
    new NodeCluster(prototype ++ node.features, nodes + node, density + nodeCache.map(_.getOrElse(node, 1L)).getOrElse(1L))

  def ++(otherNodes: Seq[Node], nodeCache: Option[NodeCache]): NodeCluster = {
    new NodeCluster(
      prototype ++ otherNodes.flatMap(_.features),
      nodes ++ otherNodes,
      density + otherNodes.foldLeft(0L) { case (sum, node) => sum + nodeCache.map(_.getOrElse(node, 1L)).getOrElse(1L) })
  }

  def toText(nodeCache: NodeCache, totalMass: Double = 0): String = {
    s"""
       |Prototype: ${prototype.mkString(", ")}
       |Density: $density ${if (totalMass != 0) "(" + f"${density / totalMass}%1.4f" + "%)" else ""}
       |Nodes:
       |${nodes.map(n => "* " + n.toText + " : " + nodeCache.getOrElse(n, 1L)).mkString("\n")}
       |""".stripMargin
  }

  override def toString: String = {
    s"""
      |Prototype: ${prototype.mkString(", ")}
      |Density: $density
      |Nodes:
      |${nodes.map(n => s"* ${n.toText}").mkString("\n")}
      |""".stripMargin
  }
}

object NodeCluster {

  def emptyCluster: NodeCluster = new NodeCluster(Set.empty, Set.empty, 0)

  def fromNodes(nodes: Iterable[Node], nodeCache: Option[NodeCache]): NodeCluster = {
    new NodeCluster(
      nodes.flatMap(_.features).toSet,
      nodes.toSet,
      nodes.foldLeft(0L) { case (sum, node) => sum + nodeCache.map(_.getOrElse(node, 1L)).getOrElse(1L) }
    )
  }
}
