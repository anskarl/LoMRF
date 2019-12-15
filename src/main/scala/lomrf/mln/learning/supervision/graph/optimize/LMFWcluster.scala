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
import optimus.optimization.model.{ MPBinaryVar, MPFloatVar, ModelSpec }

case class LMFWcluster(k: Int, CE: Double, CF: Double, features: Set[Feature], metric: Metric[_ <: AtomicFormula]) extends ModelSpec(SolverLib.LpSolve) {

  def clusters(nodes: IndexedSeq[Node], cache: NodeCache) = {

    val (positiveNodes, negativeNodes) = nodes
      //.filter(n => n.head.variables.forall(v => n.atoms.exists(_.variables.contains(v))))
      .sortBy(_.size).reverse.partition(_.isPositive)

    var dp = NodeCluster.emptyCluster
    var dn = NodeCluster.emptyCluster

    if (positiveNodes.nonEmpty && negativeNodes.nonEmpty) {
      var positiveClusters = Set(NodeCluster.fromNodes(Set(positiveNodes.maxBy(_.size)), Some(cache)))
      var negativeClusters = Set(NodeCluster.fromNodes(Set(negativeNodes.maxBy(_.size)), Some(cache)))

      positiveClusters = positiveNodes.filterNot(positiveClusters.head.contains).foldLeft(positiveClusters) {
        case (clusters, node) =>
          println(node.toText)
          clusters.filter(c => c.nodes.exists(n => node.softSubsumes(n))) match {
            case x if x.isEmpty => clusters + NodeCluster.fromNodes(Seq(node), Some(cache))
            case x if x.nonEmpty =>
              val xx = x.maxBy(_.density)
              (clusters - xx) + (xx + (node, Some(cache)))
            case x =>
              clusters
          }
      }

      println
      positiveClusters.foreach { c =>
        println(c.majorityPrototype(cache).toText(cache))
        println("##################")
      }

      negativeClusters = negativeNodes.filterNot(negativeClusters.head.contains).foldLeft(negativeClusters) {
        case (clusters, node) =>
          clusters.filter(c => c.nodes.exists(n => node.softSubsumes(n))) match {
            case x if x.isEmpty => clusters + NodeCluster.fromNodes(Seq(node), Some(cache))
            case x if x.nonEmpty =>
              val xx = x.maxBy(_.density)
              (clusters - xx) + (xx + (node, Some(cache)))
            case _ => clusters
          }
      }

      negativeClusters.foreach { c =>
        println(c.majorityPrototype(cache).toText(cache))
        println("##################")
      }

      //positiveClusters ++ negativeClusters

      var densePositive = positiveClusters.maxBy(_.density)
      var denseNegative = negativeClusters.maxBy(_.density)
      dp = densePositive
      dn = denseNegative

      // Extend clusters while there is an overlap
      var clusters = Set(densePositive, denseNegative)
      var stop = false
      while (densePositive.nodes.flatMap(n => n.signatures) == denseNegative.nodes.flatMap(_.signatures) && !stop) {
        val remainingPositives = positiveClusters diff clusters
        val remainingNegatives = negativeClusters diff clusters
        if (remainingPositives.nonEmpty && remainingNegatives.nonEmpty) {
          densePositive = remainingPositives.maxBy(_.density)
          denseNegative = remainingNegatives.maxBy(_.density)
          clusters ++= Set(densePositive, denseNegative)
        } else stop = true
      }

      println("EXTENDED")
      clusters.foreach { c =>
        println(c.majorityPrototype(cache).toText(cache))
        println("##################")
      }

      // Enhance using remaining clusters
      var remainingPositives = positiveClusters diff clusters
      var remainingNegatives = negativeClusters diff clusters
      stop = false
      while ((remainingPositives.nonEmpty || remainingNegatives.nonEmpty) && !stop) {
        val a = remainingPositives.find(c => clusters.exists(cc => cc.nodes.head.isNegative && c.nodes.flatMap(_.signatures) == cc.nodes.flatMap(_.signatures)))
        val b = remainingNegatives.find(c => clusters.exists(cc => cc.nodes.head.isPositive && c.nodes.flatMap(_.signatures) == cc.nodes.flatMap(_.signatures)))
        if (a.isEmpty && b.isEmpty) stop = true
        else {
          clusters ++= Set(a.getOrElse(NodeCluster.emptyCluster), b.getOrElse(NodeCluster.emptyCluster)).filter(_.nonEmpty)
          remainingPositives -= a.getOrElse(NodeCluster.emptyCluster)
          remainingNegatives -= b.getOrElse(NodeCluster.emptyCluster)
        }
      }

      println("ENHANCED")
      clusters.foreach { c =>
        println(c.majorityPrototype(cache).toText(cache))
        println("##################")
      }

      clusters
    } else Set(NodeCluster.fromNodes(positiveNodes, Some(cache)), NodeCluster.fromNodes(negativeNodes, Some(cache)))
  }

  def optimize(nodes: IndexedSeq[Node], cache: NodeCache): Map[Feature, Double] = {

    println("CLUSTERS")
    val classes = clusters(nodes, cache)
    val weights = classes.flatMap(_.nodes.flatMap(_.features)).map(f => f -> MPFloatVar(0.1)).toMap

    var slackVariables = List.empty[MPFloatVar]
    var expressions = List.empty[Expression]

    val sortedNodes = classes.foldLeft(IndexedSeq.empty[Node])((coll, set) => coll ++ set.nodes)
    val (neighbors, _) = FullConnector.fullyConnect(sortedNodes)(metric)

    import lomrf.mln.learning.supervision.graph._
    println(neighbors.mkString())

    if (nodes.exists(_.isPositive) && nodes.exists(_.isNegative)) sortedNodes.zipWithIndex.foreach {
      case (node, idx) =>

        val indices = classes.find(_.contains(node)).get.nodes.map(n => sortedNodes.zipWithIndex.find(_._1 == n).get._2)
        val (hits, misses) = neighbors(idx, ::).t.toArray.zipWithIndex.partition { case (_x, jdx) => indices.contains(jdx) }
        //val (hits, misses) = if (node.isPositive) (p, n) else (n, p)

        val closestHits = {
          val maxS = hits.map(_._1).distinct.sorted.reverse.take(k)
          hits.withFilter(x => maxS.contains(x._1)).map(_._2)
        }

        println(node.toText)
        println(s"HITS: ${closestHits.mkString(", ")}")

        val nodeFeatures = node.atoms.map(Feature.atom2Feature)

        closestHits.map(sortedNodes(_)).foreach { hitNode =>
          val hitNodeFeatures = hitNode.atoms.map(Feature.atom2Feature)
          val commonFeatures = nodeFeatures intersect hitNodeFeatures
          val nodeDiff = nodeFeatures diff commonFeatures
          val hitNodeDiff = hitNodeFeatures diff commonFeatures
          //expressions ::= (sum((nodeDiff ++ hitNodeDiff).map(weights(_))) + sum(commonFeatures.map(-weights(_))))
          expressions ::= cache.get(node).get * cache.get(hitNode).get * (
            sum((nodeDiff ++ hitNodeDiff).map(weights(_))) + sum(commonFeatures.map(weights(_) * 0.001))
          )

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
            val expr = sum((nodeDiffComm ++ missNodeDiff).map(weights(_))) + sum(commFeatures.map(weights(_) * 0.001))
            -sum((nodeDiff ++ hitNodeDiff).map(weights(_))) - sum(commonFeatures.map(weights(_) * 0.001))

            //val expr = sum(aDiff.map(-weights(_))) + sum(bDiff.map(weights(_)))
            //val expr = sum((nodeDiff ++ hitNodeDiff).map(weights(_))) + sum(commonFeatures.map(-weights(_))) -
            //(sum((nodeDiffComm ++ missNodeDiff).map(weights(_))) + sum(commFeatures.map(-weights(_))))

            slackVariables ::= MPFloatVar(0.1)
            add(expr >:= 1 - slackVariables.head)
          }
        }
    }

    /*if (nodes.exists(_.isPositive) && nodes.exists(_.isNegative)) {
      add(sum(sortedNodes.filter(_.isPositive).maxBy(cache.get).atoms.map(a => weights(Feature.atom2Feature(a)))) >:= 0.00001)
      add(sum(sortedNodes.filter(_.isNegative).maxBy(cache.get).atoms.map(a => weights(Feature.atom2Feature(a)))) >:= 0.00001)
    }*/

    /*sortedNodes.foreach { n =>
      add(sum(n.atoms.map(a => weights(Feature.atom2Feature(a)))) >:= 0.00001)
    }*/

    /*add(sum(weights.values) >:= 1)*/

    /*if (classes.forall(_.nonEmpty)) classes.foreach { cl =>
     val k = cl.nodes.map(n => sum(n.atoms.map(a => weights(Feature.atom2Feature(a)))))
     val k1 = k.reduceLeft(_ + _)
     add(k1 >:= 1)
     //k.foreach(x => add(x >:= 1))
   }*/

    if (classes.forall(_.nonEmpty)) classes.foreach { cl =>
      val k = cl.nodes.map(n => sum(n.atoms.map(a => weights(Feature.atom2Feature(a)))))
      //val k1 = k.reduceLeft(_ + _)
      k.foreach(x => add(x >:= 1))
    }

    minimize(0.5 * sum(expressions) + 0.5 * sum(slackVariables) /*+ 0.5 * sum(weights.values.map(1 - _))*/ )
    start()
    weights.map { case (f, v) => f -> v.value.getOrElse(Double.NaN) }
  }
}
