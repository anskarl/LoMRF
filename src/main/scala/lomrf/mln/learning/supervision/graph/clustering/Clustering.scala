package lomrf.mln.learning.supervision.graph.clustering

import com.typesafe.scalalogging.LazyLogging
import lomrf.mln.learning.supervision.graph.Node
import lomrf.mln.learning.supervision.graph.caching.NodeCache

/**
  *
  * @param maxDensity maximum density
  */
case class Clustering(maxDensity: Double) extends LazyLogging {

  def cluster(nodes: Seq[Node], cache: NodeCache): Set[NodeCluster] = {

    val (positiveNodes, negativeNodes) = nodes.sortWith(_.size > _.size).partition(_.isPositive)

    if (positiveNodes.nonEmpty && negativeNodes.nonEmpty) {

      var pClusters = Set(NodeCluster.fromNodes(Set(positiveNodes.maxBy(_.size)), Some(cache)))
      var nClusters = Set(NodeCluster.fromNodes(Set(negativeNodes.maxBy(_.size)), Some(cache)))

      pClusters = positiveNodes.filterNot(pClusters.head.contains).foldLeft(pClusters) {
        case (clusters, node) =>
          clusters.filter(_.nodes.exists(node.subsumes)) match {
            case set if set.isEmpty => clusters + NodeCluster.fromNodes(Seq(node), Some(cache))
            case set if set.nonEmpty =>
              val c = set.maxBy(_.density)
              (clusters - c) + (c + (node, Some(cache)))
          }
      }

      logger.info {
        s"""
          |Positive clusters:
          |${pClusters.map(c => c.majorityPrototype(cache).toText(cache, nodes.flatMap(cache.get).sum)).mkString("\n")}
          |""".stripMargin
      }

      nClusters = negativeNodes.filterNot(nClusters.head.contains).foldLeft(nClusters) {
        case (clusters, node) =>
          clusters.filter(_.nodes.exists(node.subsumes)) match {
            case set if set.isEmpty => clusters + NodeCluster.fromNodes(Seq(node), Some(cache))
            case set if set.nonEmpty =>
              val c = set.maxBy(_.density)
              (clusters - c) + (c + (node, Some(cache)))
          }
      }

      logger.info {
        s"""
           |Negative clusters:
           |${nClusters.map(c => c.majorityPrototype(cache).toText(cache, nodes.flatMap(cache.get).sum)).mkString("\n")}
           |""".stripMargin
      }

      pClusters ++ nClusters

    } else Set(
      NodeCluster.fromNodes(positiveNodes, Some(cache)),
      NodeCluster.fromNodes(negativeNodes, Some(cache))
    )
  }
}