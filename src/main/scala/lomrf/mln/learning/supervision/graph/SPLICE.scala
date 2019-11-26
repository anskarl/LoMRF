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

package lomrf.mln.learning.supervision.graph

import lomrf.logic._
import breeze.linalg.DenseVector
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.metric.features.FeatureStats
import lomrf.mln.learning.supervision.metric._
import lomrf.mln.model._
import lomrf.util.time._
import lomrf.mln.model.builders.EvidenceBuilder
import scala.language.existentials
import lomrf.logic.LogicOps._

/**
  * SPLICE supervision graph represents a graph having nodes for a given query signature. These
  * nodes contain a single ground query atom and a sequence of evidence atoms sharing
  * constants to the corresponding query atom. Nodes can be either labeled (the ground query
  * atom is TRUE or FALSE) or unlabeled. The graph is connected using a specified connector
  * strategy and can be solved in order to label the unlabeled ground query atoms.
  *
  * @param nodes an indexed sequence of nodes. Labeled nodes appear before unlabelled
  * @param querySignature the query signature of interest
  * @param connector a graph connector
  * @param metric a metric for atomic formula
  * @param supervisionBuilder a supervision evidence builder that contains the completed annotation
  * @param nodeCache a node cache for storing labelled nodes
  * @param solver a graph solver for supervision completion
  * @param enableClusters enables clustering of unlabeled examples
  * @param minNodeSize minimum node process size
  */
final class SPLICE private[graph] (
    nodes: IndexedSeq[Node],
    querySignature: AtomSignature,
    connector: GraphConnector,
    metric: Metric[_ <: AtomicFormula],
    supervisionBuilder: EvidenceBuilder,
    nodeCache: NodeCache,
    featureStats: FeatureStats,
    solver: GraphSolver,
    enableClusters: Boolean,
    minNodeSize: Int)
  extends SupervisionGraph(nodes, querySignature, connector, metric, supervisionBuilder, nodeCache, featureStats) {

  protected def optimize(potentials: Map[EvidenceAtom, Double]): Set[EvidenceAtom] = {

    logger.info {
      s"""
        |Supervision graph has $numberOfNodes nodes:
        |\t- Labeled Nodes: $numberOfLabeled
        |\t- Unlabeled Nodes: $numberOfUnlabeled
        |\t- Query Signature: $querySignature
      """.stripMargin
    }

    val startGraphConnection = System.currentTimeMillis
    val encodedGraph = solver match {
      case _: HFc => connector.smartConnect(nodes, unlabeledNodes, Some(nodeCache))(metric) // TODO
      case _      => connector.fullyConnect(nodes, Some(nodeCache))(metric)
    }

    val W = encodedGraph._1
    val D = encodedGraph._2
    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis

    // Vector holding the labeled values
    val fl = DenseVector(labeledNodes.map(_.value).toArray)

    val fullSolution = solver.solve(W, D, fl).toArray
    val solution = fullSolution.slice(numberOfLabeled, numberOfNodes)
    val truthValues = solution.map(value => if (value <= UNCONNECTED) FALSE else TRUE)

    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    logger.whenDebugEnabled {
      logger.debug {
        (unlabeledNodes.map(_.query) zip solution)
          .map { case (atom, state) => s"$atom = $state" }.mkString("\n")
      }
    }

    val labeledEvidenceAtoms = unlabeledNodes.zip(truthValues).flatMap {
      case (node, value) => node.labelUsingValue(value)
    }

    supervisionBuilder.evidence ++= labeledEvidenceAtoms
    labeledEvidenceAtoms.toSet
  }

  def ++(mln: MLN, annotationDB: EvidenceDB, modes: ModeDeclarations): SPLICE = {

    // Group the given data into nodes, using the domains of the existing graph
    val currentNodes = connector match {
      case _: kNNTemporalConnector | _: eNNTemporalConnector | _: aNNTemporalConnector =>
        SupervisionGraph.partition(mln, modes, annotationDB, querySignature)
      case _ =>
        SupervisionGraph.partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = enableClusters)
    }

    // Partition nodes into labeled and unlabeled. Then find empty unlabeled nodes.
    val (labeled, unlabeled) = currentNodes.partition(_.isLabeled)
    val (nonEmptyUnlabeled, emptyUnlabeled) = unlabeled.partition(_.size >= minNodeSize)

    // Remove empty labelled nodes or nodes subsumed by the background knowledge
    val pureLabeledNodes = labeled.filterNot { n =>
      n.isEmpty || mln.clauses.exists(_.subsumes(n.clause.get))
    }

    // Labeled query atoms and empty unlabeled query atoms as FALSE.
    val labeledEntries =
      labeled.map(_.query) ++ emptyUnlabeled.flatMap(_.labelUsingValue(FALSE))

    if (emptyUnlabeled.nonEmpty)
      logger.warn(s"Found ${emptyUnlabeled.length} empty unlabeled nodes. Set them to FALSE.")

    val pureNodes = pureLabeledNodes ++ nonEmptyUnlabeled
    logger.info(s"Found ${pureLabeledNodes.length} pure labelled and unlabeled nodes.")

    /*
     * Create an annotation builder and append every query atom that is TRUE or FALSE,
     * or every UNKNOWN query atom that has no evidence atoms, everything else
     * should be labeled by the supervision graph.
     */
    val annotationBuilder =
      EvidenceBuilder(
        mln.schema.predicates.filter { case (sig, _) => sig == querySignature },
        Set(querySignature),
        Set.empty,
        mln.evidence.constants
      ).withCWAForAll().evidence ++= labeledEntries

    /*
     * In case no labeled nodes exist in the given data, then reuse the old ones. In any other
     * case try to separate old labeled nodes that are dissimilar to the ones in the current batch
     * of data (the current supervision graph). Moreover remove noisy nodes using the Hoeffding bound.
     */
    if (pureLabeledNodes.isEmpty)
      new SPLICE(
        labeledNodes ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence ++ pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms)),
        annotationBuilder,
        nodeCache,
        featureStats,
        solver,
        enableClusters,
        minNodeSize)
    else {
      /*
       * Update the cache using only non empty labeled nodes, i.e., nodes having at least
       * one evidence predicate in their body
       *
       * Cache stores only unique nodes (patterns) along their counts.
       */
      val startCacheUpdate = System.currentTimeMillis

      var updatedNodeCache = nodeCache
      updatedNodeCache ++= pureLabeledNodes
      val cleanedUniqueLabeled = updatedNodeCache.collectNodes.filter(_.size >= minNodeSize)
        .filter(n => updatedNodeCache.getOrElse(n, 0) > 1) // TODO maybe this should be a parameter (e.g., minOcc)

      logger.info(msecTimeToTextUntilNow(s"Cache updated in: ", startCacheUpdate))
      logger.info(s"${cleanedUniqueLabeled.length}/${numberOfLabeled + labeled.length} unique labeled nodes kept.")
      logger.debug(updatedNodeCache.toString)

      logger.info {
        s"""
          |Labelled nodes kept:
          |${cleanedUniqueLabeled.sortBy(_.isNegative).map(n => n.toText + " -> " + updatedNodeCache.get(n).get).mkString("\n")}
        |""".stripMargin
      }

      val (positiveNodes, negativeNodes) = cleanedUniqueLabeled.sortBy(_.size).reverse.partition(_.isPositive)

      val resulted = if (positiveNodes.nonEmpty && negativeNodes.nonEmpty) {
        var positiveClusters = Set(Set(positiveNodes.maxBy(_.size)))
        var negativeClusters = Set(Set(negativeNodes.maxBy(_.size)))

        /*positiveClusters.foreach { c =>
          println(c.map(n => n.toText + " -> " + updatedNodeCache.getOrElse(n, 0)).mkString("\n"))
          println("##################")
        }
        negativeClusters.foreach { c =>
          println(c.map(n => n.toText + " -> " + updatedNodeCache.getOrElse(n, 0)).mkString("\n"))
          println("##################")
        }*/

        positiveClusters = positiveNodes.filterNot(positiveClusters.head.contains).foldLeft(positiveClusters) {
          case (clusters, node) =>
            println(node.toText)
            clusters.filter(c => c.exists(n => node.relevantNode(n))) match {
              case x if x.isEmpty => clusters + Set(node)
              case x if x.size >= 1 =>
                val xx = x.maxBy(_.toList.map(n => updatedNodeCache.get(n).get).sum)
                (clusters - xx) + (xx + node)
              case x =>
                /*println("XAXAXa")
                x.foreach { c =>
                  println(c.map(n => n.toText + " -> " + updatedNodeCache.getOrElse(n, 0)).mkString("\n"))
                  println("##################")
                }
                println*/
                clusters
            }
        }

        println
        positiveClusters.foreach { c =>
          println(c.map(n => n.toText + " -> " + updatedNodeCache.getOrElse(n, 0)).mkString("\n"))
          println("##################")
        }

        negativeClusters = negativeNodes.filterNot(negativeClusters.head.contains).foldLeft(negativeClusters) {
          case (clusters, node) =>
            clusters.filter(c => c.exists(n => node.relevantNode(n))) match {
              case x if x.isEmpty => clusters + Set(node)
              case x if x.size >= 1 =>
                val xx = x.maxBy(_.toList.map(n => updatedNodeCache.get(n).get).sum)
                (clusters - xx) + (xx + node)
              case _ => clusters
            }
        }

        negativeClusters.foreach { c =>
          println(c.map(n => n.toText + " -> " + updatedNodeCache.getOrElse(n, 0)).mkString("\n"))
          println("##################")
        }

        var densePositive = positiveClusters.maxBy(_.toList.map(n => updatedNodeCache.get(n).get).sum)
        var denseNegative = negativeClusters.maxBy(_.toList.map(n => updatedNodeCache.get(n).get).sum)

        // Extend clusters while there is an overlap
        var clusters = Set(densePositive, denseNegative)
        var stop = false
        while (densePositive.flatMap(n => n.signatures) == denseNegative.flatMap(_.signatures) && !stop) {
          val remainingPositives = positiveClusters diff clusters
          val remainingNegatives = negativeClusters diff clusters
          if (remainingPositives.nonEmpty && remainingNegatives.nonEmpty) {
            densePositive = remainingPositives.maxBy(_.toList.map(n => updatedNodeCache.get(n).get).sum)
            denseNegative = remainingNegatives.maxBy(_.toList.map(n => updatedNodeCache.get(n).get).sum)
            clusters ++= Set(densePositive, denseNegative)
          } else stop = true
        }

        println("EXTENDED")
        clusters.foreach { c =>
          println(c.map(n => n.toText + " -> " + updatedNodeCache.getOrElse(n, 0)).mkString("\n"))
          println("##################")
        }

        // Enhance using remaining clusters
        var remainingPositives = positiveClusters diff clusters
        var remainingNegatives = negativeClusters diff clusters
        stop = false
        while ((remainingPositives.nonEmpty || remainingNegatives.nonEmpty) && !stop) {
          val a = remainingPositives.find(c => clusters.exists(cc => cc.head.isNegative && c.flatMap(_.signatures) == cc.flatMap(_.signatures)))
          val b = remainingNegatives.find(c => clusters.exists(cc => cc.head.isPositive && c.flatMap(_.signatures) == cc.flatMap(_.signatures)))
          if (a.isEmpty && b.isEmpty) stop = true
          else {
            clusters ++= Set(a.getOrElse(Set.empty), b.getOrElse(Set.empty)).filter(_.nonEmpty)
            remainingPositives -= a.getOrElse(Set.empty)
            remainingNegatives -= b.getOrElse(Set.empty)
          }
        }

        println("ENHANCED")
        clusters.foreach { c =>
          println(c.map(n => n.toText + " -> " + updatedNodeCache.getOrElse(n, 0)).mkString("\n"))
          println("##################")
        }

        (clusters ++ clusters.filter(_.head.isPositive).flatten.map(_.createSubNodes.map(_.toNegative))).flatten.toIndexedSeq
      } else cleanedUniqueLabeled

      logger.info {
        s"""
          |Labelled nodes selection and augmentation:
          |${resulted.map(n => n.toText + " -> " + updatedNodeCache.getOrElse(n, 0L)).mkString("\n")}
        |""".stripMargin
      }

      println(cleanedUniqueLabeled.size)
      println(resulted.size)

      // Labeled nodes MUST appear before unlabeled!
      new SPLICE(
        resulted ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence ++ pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms)),
        annotationBuilder,
        updatedNodeCache,
        featureStats,
        solver,
        enableClusters,
        minNodeSize)
    }
  }
}
