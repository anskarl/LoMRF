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

package lomrf.mln.learning.supervision.graph.optimize

import lomrf.logic.AtomicFormula
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.graph.{ FullConnector, Node }
import lomrf.mln.learning.supervision.metric.Metric
import lomrf.mln.learning.supervision.metric.features.Feature
import optimus.algebra._
import optimus.optimization._
import optimus.algebra.AlgebraOps._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.{ MPFloatVar, ModelSpec }

case class LMFWsim(k: Int, CE: Double, CF: Double, features: Set[Feature], metric: Metric[_ <: AtomicFormula]) extends ModelSpec(SolverLib.LpSolve) {

  def optimize(nodes: IndexedSeq[Node], cache: NodeCache): Map[Feature, Double] = {

    val weights = features.map(f => f -> MPFloatVar(0, 1)).toMap

    var slackVariables = List.empty[MPFloatVar]
    var expressions = List.empty[Expression]

    val numberOfPositives = nodes.count(_.isPositive)
    val numberOfNegatives = nodes.length - numberOfPositives
    val sortedNodes = nodes.sortBy(_.isNegative)

    sortedNodes.map(_.toText).foreach(println)

    val (neighbors, _) = FullConnector.fullyConnect(sortedNodes)(metric)

    import lomrf.mln.learning.supervision.graph._
    println("POSITIVES: " + numberOfPositives)
    println("NEGATIVES: " + numberOfNegatives)
    println(neighbors.mkString())

    if (nodes.exists(_.isPositive) && nodes.exists(_.isNegative)) sortedNodes.zipWithIndex.foreach {
      case (node, idx) =>
        val (p, n) = neighbors(idx, ::).t.toArray.zipWithIndex.splitAt(numberOfPositives)
        val (hits, misses) = if (node.isPositive) (p, n) else (n, p)

        val closestHits = {
          //val maxSim = hits.maxBy(_._1)._1
          //hits.withFilter(_._1 == maxSim).map(_._2)
          val maxS = hits.map(_._1).distinct.sorted.reverse.take(k)
          hits.withFilter(x => maxS.contains(x._1)).map(_._2)
        }
        val closestMisses = {
          //val maxSim = misses.maxBy(_._1)._1
          //misses.withFilter(_._1 == maxSim).map(_._2)
          val maxS = misses.map(_._1).distinct.sorted.reverse.take(k)
          misses.withFilter(x => maxS.contains(x._1)).map(_._2)
        }

        println(node.toText)
        println(s"HITS: ${closestHits.mkString(", ")}")
        println(s"MISSES: ${closestMisses.mkString(", ")}")

        val nodeFeatures = node.atoms.map(Feature.atom2Feature)

        closestHits.map(sortedNodes(_)).foreach { hitNode =>
          val hitNodeFeatures = hitNode.atoms.map(Feature.atom2Feature)
          val commonFeatures = nodeFeatures intersect hitNodeFeatures
          val nodeDiff = nodeFeatures diff commonFeatures
          val hitNodeDiff = hitNodeFeatures diff commonFeatures
          expressions ::= sum((nodeDiff ++ hitNodeDiff).map(weights(_))) + sum(commonFeatures.map(-weights(_)))

          // foreach hit node we should add constraints for the misses
          misses.map(_._2).map(sortedNodes(_)).foreach { missNode =>
            val missNodeFeatures = missNode.atoms.map(Feature.atom2Feature)
            val commFeatures = nodeFeatures intersect missNodeFeatures
            val nodeDiffComm = nodeFeatures diff commFeatures
            val missNodeDiff = missNodeFeatures diff commFeatures

            val a = (nodeDiff ++ hitNodeDiff)
            val b = (nodeDiffComm ++ missNodeDiff)
            val ab = a intersect b
            val aDiff = a diff ab
            val bDiff = b diff ab
            val expr = sum((nodeDiff ++ hitNodeDiff).map(weights(_))) + sum(commonFeatures.map(-weights(_))) -
              (sum((nodeDiffComm ++ missNodeDiff).map(weights(_))) + sum(commFeatures.map(-weights(_))))

            slackVariables ::= MPFloatVar(0)
            add(expr >:= 5 - slackVariables.head)
            println(expr + " >= 1 - ξ")
          }
        }
    }

    println(weights)
    println(sum(expressions))
    minimize(0.5 * sum(expressions) + 0.5 * sum(slackVariables) + CF * sum(weights.values))
    start()
    println("OBJ: " + objectiveValue)
    weights.map { case (f, v) => f -> v.value.getOrElse(Double.NaN) }
  }
}
