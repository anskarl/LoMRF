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

package lomrf.mln.learning.supervision.graph.selection

import com.typesafe.scalalogging.LazyLogging
import lomrf.mln.learning.supervision.graph.Node
import lomrf.mln.learning.supervision.graph.caching.NodeCache

/**
  * @param maxDensity clusters maximum density
  */
case class Clustering(maxDensity: Double, retainNoise: Boolean = true) extends LazyLogging {

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

      if (!retainNoise) pClusters = pClusters.filter(_.nodes.exists(n => cache.getOrElse(n, 0) > 1))

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

      if (!retainNoise) nClusters = nClusters.filter(_.nodes.exists(n => cache.getOrElse(n, 0) > 1))

      logger.info {
        s"""
           |Negative clusters:
           |${nClusters.map(c => c.majorityPrototype(cache).toText(cache, nodes.flatMap(cache.get).sum)).mkString("\n")}
           |""".stripMargin
      }

      if (maxDensity >= 1) pClusters ++ nClusters
      else {
        logger.info(s"Keeping only $maxDensity of the total density:")
        val totalMass = nodes.flatMap(cache.get).sum.toDouble
        val maxP = pClusters.maxBy(_.density)
        val maxN = nClusters.maxBy(_.density)
        var rest = (pClusters - maxP) ++ (nClusters - maxN)
        var clusters = Set(maxP, maxN)
        while (clusters.map(_.density).sum / totalMass < maxDensity && rest.nonEmpty) {
          val next = rest.maxBy(_.density)
          clusters += next
          rest -= next
        }
        logger.info(s"${
          clusters.map(c => c.majorityPrototype(cache).toText(cache, nodes.flatMap(cache.get).sum))
            .mkString("\n")
        }")

        clusters
      }

    } else Set(
      NodeCluster.fromNodes(positiveNodes, Some(cache)),
      NodeCluster.fromNodes(negativeNodes, Some(cache))
    )
  }
}
