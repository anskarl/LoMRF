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

import lomrf.logic.{ AtomSignature, Clause }
import lomrf.mln.learning.supervision.graph.HoeffdingBound
import lomrf.mln.learning.supervision.graph.Node
import lomrf.util.logging.Implicits._

final case class SimpleNodeCache(
    querySignature: AtomSignature,
    data: Set[(Clause, Long)] = Set.empty,
    uniqueNodes: IndexedSeq[Node] = IndexedSeq.empty,
    useHoeffdingBound: Boolean = false) extends NodeCache {

  /**
    * @return the number of unique nodes in the cache
    */
  def size: Int = data.size

  /**
    * @return the number of positive unique nodes in the cache
    */
  def numberOfPositive: Int =
    data.count { case (clause, _) => clause.literals.find(_.sentence.signature == querySignature).get.isPositive }

  /**
    * @return the number of negative unique nodes in the cache
    */
  def numberOfNegative: Int =
    data.count { case (clause, _) => clause.literals.find(_.sentence.signature == querySignature).get.isNegative }

  /**
    * @param node a node
    * @return an Option value containing the counts of the given node, or None
    *         if the node does not exist in the cache.
    */
  def get(node: Node): Option[Long] =
    if (node.clause.isEmpty) None
    else data.find { case (clause, _) => clause =~= node.clause.get }.map { case (_, count) => count }

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
  def contains(node: Node): Boolean =
    if (node.isLabeled) data.exists { case (clause, _) => clause =~= node.clause.get } else false

  /**
    * Add a node to the cache.
    *
    * @param node a node to be added
    * @return a new node cache that contains all nodes of the current cache
    *         along the given node.
    */
  def +(node: Node): SimpleNodeCache = this ++ Seq(node)

  /**
    * Remove a node from the cache.
    *
    * @param node a node to be removed
    * @return a node cache that contains all nodes of the current cache
    *         except the given node.
    */
  def -(node: Node): SimpleNodeCache = this -- Seq(node)

  /**
    * Add a sequence of nodes to the cache.
    *
    * @param nodes a sequence of nodes
    * @return a node cache containing all nodes of the current cache
    *         along the given sequence of nodes.
    */
  def ++(nodes: Seq[Node]): SimpleNodeCache = {

    val (updatedUniqueNodes, updatedNodeSet) = nodes.foldLeft(uniqueNodes -> data) {
      case ((unique, cache), node) =>

        val pattern = node.clause.getOrElse(logger.fatal("Cannot construct a pattern."))

        if (!unique.flatMap(_.clause).exists(_ =~= pattern))
          cache.find { case (c, _) => c =~= pattern } match {
            case Some(entry @ (_, counts)) => (unique :+ node, (cache - entry) + (pattern -> (counts + 1)))
            case None                      => (unique :+ node, cache + (pattern -> 1))
          }
        else cache.find { case (c, _) => c =~= pattern } match {
          case Some(entry @ (_, counts)) => (unique, (cache - entry) + (pattern -> (counts + 1)))
          case None =>
            logger.fatal(s"Pattern '${node.toText}' is not unique, but it does not exist in the cache.")
        }
    }

    SimpleNodeCache(querySignature, updatedNodeSet, updatedUniqueNodes, useHoeffdingBound)
  }

  /**
    * Remove a sequence of nodes to the cache.
    *
    * @param nodes a sequence of nodes
    * @return a node cache containing all nodes of the current cache
    *         except the given sequence of nodes.
    */
  def --(nodes: Seq[Node]): SimpleNodeCache = {

    val (updatedUniqueNodes, updatedNodeSet) = nodes.foldLeft(uniqueNodes -> data) {
      case ((unique, cache), node) =>

        val pattern = node.clause.getOrElse(logger.fatal("Cannot construct a pattern."))

        cache.find { case (c, _) => c =~= pattern } match {
          case Some(entry) => (unique.filterNot(_.clause.get =~= pattern), cache - entry)
          case None        => (unique, cache)
        }
    }

    SimpleNodeCache(querySignature, updatedNodeSet, updatedUniqueNodes, useHoeffdingBound)
  }

  /**
    * Collects all unique nodes.
    *
    * @return all unique nodes in the cache
    */
  def collectNodes: IndexedSeq[Node] = uniqueNodes.foldLeft(IndexedSeq.empty[Node]) {
    case (result, node) =>

      val nodeCounts = getOrElse(node, logger.fatal(s"Pattern '${node.toText}' does not exist in the cache."))
      val oppNodeClause = node.opposite.clause.getOrElse(logger.fatal("Cannot construct a pattern."))

      data.find { case (c, _) => c =~= oppNodeClause } match {

        case Some((_, oppNodeCounts)) =>
          val N = nodeCounts + oppNodeCounts
          val nodeFreq = nodeCounts.toDouble / N
          val oppositeNodeFreq = oppNodeCounts.toDouble / N
          val isSatisfied = if (useHoeffdingBound) HoeffdingBound(nodeFreq, oppositeNodeFreq, N) else true

          if (isSatisfied && nodeFreq < oppositeNodeFreq) {
            logger.debug(s"Remove pattern ${node.toText}")
            result
          } else result :+ node

        case None => result :+ node
      }
  }

  override def toString: String =
    data.map { case (clause, counts) => s"${clause.toText()} -> $counts" }.mkString("\n")
}
