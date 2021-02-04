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
import lomrf.mln.learning.structure.ModeDeclaration
import lomrf.mln.learning.supervision.graph.{ FullConnector, Node }
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.metric.{ Feature, Metric }
import lomrf.mln.model.ModeDeclarations
import optimus.algebra.Expression
import optimus.optimization.MPModel
import optimus.optimization.enums.{ PreSolve, SolverLib }
import optimus.optimization.model.{ MPBinaryVar, MPFloatVar }
import optimus.algebra.AlgebraOps.sum

case class LargeMarginNN(k: Int, mu: Double) extends LazyLogging {

  def optimizeTogether(classes: Set[NodeCluster], modes: ModeDeclarations, cache: NodeCache)
    (metric: Metric[_ <: AtomicFormula]): (Map[Feature, Double], IndexedSeq[Node]) = {

    val nodes = classes.foldLeft(IndexedSeq.empty[Node])((coll, set) => coll ++ set.nodes)
    val indexedNodes = nodes.zipWithIndex
    val (neighbors, _) = FullConnector.fullyConnect(nodes)(metric)

    val model = MPModel(SolverLib.LpSolve)
    var slackVariables = List.empty[MPFloatVar]
    var expressions = List.empty[Expression]

    val features = nodes.flatMap(_.features).toSet
    val globalWeights = features.map(f => f -> MPBinaryVar(f.toString)(model)).toMap

    val maxRecall = modes
      .withFilter { case (_, mode) => mode.recall < Int.MaxValue }
      .map { case (_, mode) => mode.recall }.max

    val md = modes.mapValues { m =>
      ModeDeclaration(if (m.recall < Int.MaxValue) m.recall else maxRecall, m.placeMarkers, m.incompatibleSignatures)
    }

    if (classes.exists(_.hasPositive) && classes.exists(_.hasNegative)) {
      classes.foreach { cs =>

        // For each node of the class add terms to the optimisation
        indexedNodes.withFilter { case (node, _) => cs.contains(node) }.foreach {
          case (node, idx) =>

            // Find indices of all nodes in the current class
            val indices = classes.find(_.contains(node)).get.nodes.map(n => indexedNodes.find(_._1 == n).get._2)

            val (hits, misses) = neighbors(idx, ::).t.toArray
              .zipWithIndex.partition { case (_, jdx) => indices.contains(jdx) }

            val nearestHits = {
              val topK =
                if (hits.length == 1) hits.map(_._1)
                else hits.map(_._1).filterNot(_ == idx).distinct.sorted.reverse.take(k)

              hits.withFilter(x => topK.contains(x._1)).map(_._2)
            }

            val nodeFeatures1 = node.atoms.map(Feature.fromAtomicFormula)

            val nodeFeatureCounts = nodeFeatures1.groupBy(Predef.identity).mapValues(_.length)
            val nodeFeatures = nodeFeatureCounts.foldLeft(IndexedSeq.empty[Feature]) {
              case (res, (f, counts)) =>
                if (counts <= md(f.signature).recall) res ++ IndexedSeq.fill(counts)(f)
                else res ++ IndexedSeq.fill(md(f.signature).recall)(f)
            }

            nearestHits.map(nodes(_)).foreach { hitNode =>
              val hitNodeFeatures1 = hitNode.atoms.map(Feature.fromAtomicFormula)
              val hitNodeFeatureCounts = hitNodeFeatures1.groupBy(Predef.identity).mapValues(_.length)
              val hitNodeFeatures = hitNodeFeatureCounts.foldLeft(IndexedSeq.empty[Feature]) {
                case (res, (f, counts)) =>
                  if (counts <= md(f.signature).recall) res ++ IndexedSeq.fill(counts)(f)
                  else res ++ IndexedSeq.fill(md(f.signature).recall)(f)
              }

              val commonFeatures = nodeFeatures intersect hitNodeFeatures
              val nodeDiff = nodeFeatures diff commonFeatures
              val hitNodeDiff = hitNodeFeatures diff commonFeatures
              expressions ::= cache.get(node).get * cache.get(hitNode).get * {
                sum((nodeDiff ++ hitNodeDiff).map(globalWeights(_)))
                //+ sum(commonFeatures.map(globalWeights(_) * 1e-10))
              }

              // foreach hit node we should add constraints for the misses
              misses.map(_._2).map(nodes(_)).foreach { missNode =>
                val missNodeFeatures1 = missNode.atoms.map(Feature.fromAtomicFormula)
                val missNodeFeatureCounts = missNodeFeatures1.groupBy(Predef.identity).mapValues(_.length)
                val missNodeFeatures = missNodeFeatureCounts.foldLeft(IndexedSeq.empty[Feature]) {
                  case (res, (f, counts)) =>
                    if (counts <= md(f.signature).recall) res ++ IndexedSeq.fill(counts)(f)
                    else res ++ IndexedSeq.fill(md(f.signature).recall)(f)
                }

                val commFeatures = nodeFeatures intersect missNodeFeatures
                val nodeDiffComm = nodeFeatures diff commFeatures
                val missNodeDiff = missNodeFeatures diff commFeatures

                val expr =
                  sum((nodeDiffComm ++ missNodeDiff).map(globalWeights(_))) //+
                //sum(commFeatures.map(globalWeights(_) * 1e-10)) -
                -sum((nodeDiff ++ hitNodeDiff).map(globalWeights(_))) //-
                //sum(commonFeatures.map(globalWeights(_) * 1e-10))

                slackVariables ::= MPBinaryVar()(model)
                model.add(expr >:= 1 /*- slackVariables.head*/ )
              }
            }

            cs.nodes.foreach { n =>
              val a = n.atoms.map(Feature.fromAtomicFormula)

              val nodeFeatureCounts = a.groupBy(Predef.identity).mapValues(_.length)
              val a1 = nodeFeatureCounts.foldLeft(IndexedSeq.empty[Feature]) {
                case (res, (f, counts)) =>
                  if (counts <= md(f.signature).recall) res ++ IndexedSeq.fill(counts)(f)
                  else res ++ IndexedSeq.fill(md(f.signature).recall)(f)
              }

              model.add(sum(a1.map(a => globalWeights(a))) >:= 1)
            }
        }
      }

      model.minimize((1 - mu) * sum(expressions) + mu * sum(slackVariables))
      model.start(preSolve = PreSolve.AGGRESSIVE)
    }

    logger.debug {
      s"""
         |Slack variables:
         |${slackVariables.map(v => s"${v.toText} := ${v.value}").mkString("\n")}
         |Global binary weights:
         |${globalWeights.mapValues(_.value.getOrElse(Double.NaN)).mkString("\n")}
         |""".stripMargin
    }

    globalWeights.mapValues(_.value.getOrElse(0.0)) -> nodes
  }

  def optimizeAloneAndMerge(classes: Set[NodeCluster], cache: NodeCache)
    (metric: Metric[_ <: AtomicFormula]): (Map[Feature, Double], IndexedSeq[Node]) = {

    val nodes = classes.foldLeft(IndexedSeq.empty[Node])((coll, set) => coll ++ set.nodes)
    val indexedNodes = nodes.zipWithIndex
    val (neighbors, _) = FullConnector.fullyConnect(nodes)(metric)

    val features = nodes.flatMap(_.features).toSet
    var globalWeights = features.map(f => f -> 0.0).toMap

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
            val topK =
              if (hits.length == 1) hits.map(_._1)
              else hits.map(_._1).filterNot(_ == idx).distinct.sorted.reverse.take(k)

            hits.withFilter(x => topK.contains(x._1)).map(_._2)
          }

          val nodeFeatures = node.atoms.map(Feature.fromAtomicFormula)

          nearestHist.map(nodes(_)).foreach { hitNode =>
            val hitNodeFeatures = hitNode.atoms.map(Feature.fromAtomicFormula)
            val commonFeatures = nodeFeatures intersect hitNodeFeatures
            val nodeDiff = nodeFeatures diff commonFeatures
            val hitNodeDiff = hitNodeFeatures diff commonFeatures
            expressions ::= cache.get(node).get * cache.get(hitNode).get * {
              sum((nodeDiff ++ hitNodeDiff).map(localWeights(_))) + sum(commonFeatures.map(localWeights(_) * 1e-10))
            }

            // foreach hit node we should add constraints for the misses
            misses.map(_._2).map(nodes(_)).foreach { missNode =>
              val missNodeFeatures = missNode.atoms.map(Feature.fromAtomicFormula)
              val commFeatures = nodeFeatures intersect missNodeFeatures
              val nodeDiffComm = nodeFeatures diff commFeatures
              val missNodeDiff = missNodeFeatures diff commFeatures

              val expr = sum((nodeDiffComm ++ missNodeDiff).map(localWeights(_))) +
                sum(commFeatures.map(localWeights(_) * 1e-10)) -
                sum((nodeDiff ++ hitNodeDiff).map(localWeights(_))) -
                sum(commonFeatures.map(localWeights(_) * 1e-10))

              slackVariables ::= MPFloatVar(0)(model)
              model.add(expr >:= 1 - slackVariables.head)
            }
          }

          cs.nodes.foreach(n => model.add(sum(n.atoms.map(a => localWeights(Feature.fromAtomicFormula(a)))) >:= 1))
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
