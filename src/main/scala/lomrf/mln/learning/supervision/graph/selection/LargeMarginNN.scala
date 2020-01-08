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
import lomrf.logic.AtomicFormula
import lomrf.mln.learning.supervision.graph.{ FullConnector, Node }
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.metric.Metric
import lomrf.mln.learning.supervision.metric.features.Feature
import optimus.algebra.Expression
import optimus.optimization.MPModel
import optimus.optimization.enums.{ PreSolve, SolverLib }
import optimus.optimization.model.{ MPBinaryVar, MPFloatVar }
import optimus.algebra.AlgebraOps.sum

case class LargeMarginNN(k: Int, mu: Double) extends LazyLogging {

  def optimize(classes: Set[NodeCluster], cache: NodeCache)
    (metric: Metric[_ <: AtomicFormula]): (Map[Feature, Double], IndexedSeq[Node]) = {

    val nodes = classes.foldLeft(IndexedSeq.empty[Node])((coll, set) => coll ++ set.nodes)
    val features = nodes.flatMap(_.features)
    var globalWeights = features.map(f => f -> 0.0).toMap
    val indexedNodes = nodes.zipWithIndex
    val (neighbors, _) = FullConnector.fullyConnect(nodes)(metric)

    if (classes.exists(_.hasPositive) && classes.exists(_.hasNegative)) classes.foreach { cs =>

      val model = MPModel(SolverLib.LpSolve)
      var slackVariables = List.empty[MPFloatVar]
      var expressions = List.empty[Expression]
      val localWeights = features.map(f => f -> MPBinaryVar(f.toString)(model)).toMap

      indexedNodes.withFilter { case (node, _) => cs.contains(node) }.foreach {
        case (node, idx) =>

          val indices = classes.find(_.contains(node)).get.nodes.map(n => indexedNodes.find(_._1 == n).get._2)

          val (hits, misses) = neighbors(idx, ::).t.toArray
            .zipWithIndex.partition { case (_, jdx) => indices.contains(jdx) }

          val nearestHist = {
            val topK = hits.map(_._1).distinct.sorted.reverse.take(k)
            hits.withFilter(x => topK.contains(x._1)).map(_._2)
          }

          val nodeFeatures = node.atoms.map(Feature.atom2Feature)

          nearestHist.map(nodes(_)).foreach { hitNode =>
            val hitNodeFeatures = hitNode.atoms.map(Feature.atom2Feature)
            val commonFeatures = nodeFeatures intersect hitNodeFeatures
            val nodeDiff = nodeFeatures diff commonFeatures
            val hitNodeDiff = hitNodeFeatures diff commonFeatures
            expressions ::= cache.get(node).get * cache.get(hitNode).get * {
              sum((nodeDiff ++ hitNodeDiff).map(localWeights(_))) + sum(commonFeatures.map(localWeights(_) * 1e-10))
            }

            // foreach hit node we should add constraints for the misses
            misses.map(_._2).map(nodes(_)).foreach { missNode =>
              val missNodeFeatures = missNode.atoms.map(Feature.atom2Feature)
              val commFeatures = nodeFeatures intersect missNodeFeatures
              val nodeDiffComm = nodeFeatures diff commFeatures
              val missNodeDiff = missNodeFeatures diff commFeatures

              val expr = sum((nodeDiffComm ++ missNodeDiff).map(localWeights(_))) + sum(commFeatures.map(localWeights(_) * 1e-10))
              -sum((nodeDiff ++ hitNodeDiff).map(localWeights(_))) - sum(commonFeatures.map(localWeights(_) * 1e-10))

              slackVariables ::= MPFloatVar(0)(model)
              model.add(expr >:= 1 - slackVariables.head)
            }
          }

          cs.nodes.foreach(n => model.add(sum(n.atoms.map(a => localWeights(Feature.atom2Feature(a)))) >:= 1))
      }

      model.minimize((1 - mu) * sum(expressions) + mu * sum(slackVariables))
      model.start(preSolve = PreSolve.AGGRESSIVE)

      logger.debug {
        s"""
          |Slack variables:
          |${slackVariables.map(v => s"${v.toText} := ${v.value}").mkString("\n")}
          |Binary weights:
          |${localWeights.mapValues(_.value.getOrElse(Double.NaN)).mkString("\n")}
          |""".stripMargin
      }

      localWeights.foreach {
        case (f, w) =>
          globalWeights += f -> (globalWeights(f).toInt | w.value.getOrElse(0.0).toInt).toDouble
      }
    }

    logger.info {
      s"""
        |Global binary weights:
        |${globalWeights.mkString("\n")}
        |""".stripMargin
    }
    globalWeights -> nodes
  }
}
