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
import lomrf.mln.learning.supervision.graph.{FullConnector, Node}
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.graph.clustering.NodeCluster
import lomrf.mln.learning.supervision.metric.Metric
import lomrf.mln.learning.supervision.metric.features.Feature
import optimus.algebra.AlgebraOps.sum
import optimus.algebra.Expression
import optimus.optimization.enums.{PreSolve, SolverLib}
import optimus.optimization.{MPModel, add, minimize, start}
import optimus.optimization.model.{MPBinaryVar, MPFloatVar, ModelSpec}

case class LMNN(k: Int, mu: Double) {

  def optimize(classes: Set[NodeCluster], cache: NodeCache)(metric: Metric[_ <: AtomicFormula]): (Map[Feature, Double], IndexedSeq[Node]) = {

    var weights1 = classes.flatMap(_.nodes.flatMap(_.features)).map(f => f -> 0.0).toMap

    val sortedNodes = classes.foldLeft(IndexedSeq.empty[Node])((coll, set) => coll ++ set.nodes)
    val snidx = sortedNodes.zipWithIndex

    val (neighbors, _) = FullConnector.fullyConnect(sortedNodes)(metric)

    if (classes.exists(_.hasPositive) && classes.exists(_.hasNegative)) classes.foreach {
      c =>

        implicit val model = MPModel(SolverLib.LpSolve)
        val weights = classes.flatMap(_.nodes.flatMap(_.features)).map(f => f -> MPBinaryVar()).toMap

        var slackVariables = List.empty[MPFloatVar]
        var expressions = List.empty[Expression]

        snidx.filter(x => c.contains(x._1)).foreach {
          case (node, idx) =>

            val indices = classes.find(_.contains(node)).get.nodes.map(n => snidx.find(_._1 == n).get._2)
            val (hits, misses) = neighbors(idx, ::).t.toArray.zipWithIndex.partition { case (_x, jdx) => indices.contains(jdx) }
            //val (hits, misses) = if (node.isPositive) (p, n) else (n, p)

            val closestHits = {
              val maxS = hits.map(_._1).distinct.sorted.reverse.take(k)
              hits.withFilter(x => maxS.contains(x._1)).map(_._2)
            }

            println(node.toText)
            println(indices)
            println(s"HITS: ${closestHits.mkString(", ")}")

            val nodeFeatures = node.atoms.map(Feature.atom2Feature)

            closestHits.map(sortedNodes(_)).foreach { hitNode =>
              val hitNodeFeatures = hitNode.atoms.map(Feature.atom2Feature)
              val commonFeatures = nodeFeatures intersect hitNodeFeatures
              val nodeDiff = nodeFeatures diff commonFeatures
              val hitNodeDiff = hitNodeFeatures diff commonFeatures
              //expressions ::= (sum((nodeDiff ++ hitNodeDiff).map(weights(_))) + sum(commonFeatures.map(-weights(_))))
              expressions ::= cache.get(node).get * cache.get(hitNode).get * (
                sum((nodeDiff ++ hitNodeDiff).map(weights(_))) + sum(commonFeatures.map(weights(_) * 1e-10))
              )

              // foreach hit node we should add constraints for the misses
              misses.map(_._2).map(sortedNodes(_)).foreach { missNode =>
                //println(missNode.toText)
                val missNodeFeatures = missNode.atoms.map(Feature.atom2Feature)
                val commFeatures = nodeFeatures intersect missNodeFeatures
                val nodeDiffComm = nodeFeatures diff commFeatures
                val missNodeDiff = missNodeFeatures diff commFeatures

                //println(commFeatures)
                //println(nodeDiffComm ++ missNodeDiff)

                val a = (nodeDiff ++ hitNodeDiff)
                val b = (nodeDiffComm ++ missNodeDiff)
                val ab = a intersect b
                val aDiff = a diff ab
                val bDiff = b diff ab
                //val expr = sum(aDiff.map(-weights(_))) + sum(bDiff.map(weights(_)))
                val expr = sum((nodeDiffComm ++ missNodeDiff).map(weights(_))) + sum(commFeatures.map(weights(_) * 1e-10))
                -sum((nodeDiff ++ hitNodeDiff).map(weights(_))) - sum(commonFeatures.map(weights(_) * 1e-10))

                //val expr = sum(aDiff.map(-weights(_))) + sum(bDiff.map(weights(_)))
                //val expr = sum((nodeDiff ++ hitNodeDiff).map(weights(_))) + sum(commonFeatures.map(-weights(_))) -
                //(sum((nodeDiffComm ++ missNodeDiff).map(weights(_))) + sum(commFeatures.map(-weights(_))))

                slackVariables ::= MPFloatVar(0)
                add(expr >:= 1 - slackVariables.head)
              }
            }

            if (c.nodes.exists(_.isPositive)) {
              c.nodes.foreach(n => add(sum(n.atoms.map(a => weights(Feature.atom2Feature(a)))) >:= 1))
            }
        }

        minimize((1 - mu) * sum(expressions) + mu * sum(slackVariables) /*+ 2 * sum(weights.values.map(-_))*/ )
        start(PreSolve.AGGRESSIVE)

        slackVariables.foreach(s => println(s.toText + " := " + s.value))
        weights.map { case (f, v) => f -> v.value.getOrElse(Double.NaN) }.foreach(println)
        weights.foreach(x => weights1 += x._1 -> (weights1(x._1) + x._2.value.get))
    }

    val nn = classes.flatMap(_.nodes)
    val r = nn.flatMap(_.augment).foldLeft(nn) {
      case (set, n) =>
        if (set.exists(_.clause.get =~= n.clause.get)) set
        else set + n
    }.toIndexedSeq

    /*val r = (classes.flatMap(_.nodes) ++ classes.flatMap(_.nodes).flatMap(_.createSubNodes.map(_.toNegative)).foldLeft(classes.flatMap(_.nodes)) {
      case (set, n) =>
        if (set.exists(_.clause.get =~= n.clause.get)) set
        else set + n
    }).toIndexedSeq*/

    println {
      s"""
         |Labelled nodes selection and augmentation:
         |${r.map(n => n.toText + " -> " + cache.getOrElse(n, 0L)).mkString("\n")}
         |""".stripMargin
    }

    weights1 = weights1.mapValues(x => if (x >= 1) 1 else 0)
    println(weights1)

    /*weights.map { case (f, v) => f -> v.value.getOrElse(Double.NaN) }*/ weights1 -> r
  }
}
