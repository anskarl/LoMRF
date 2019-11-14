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
import lomrf.mln.learning.supervision.metric.features.{ Feature, FeatureStats }
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
      case _: HFc => connector.smartConnect(nodes, unlabeledNodes, Some(nodeCache))(metric)
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
      //.filter(n => updatedNodeCache.getOrElse(n, 0) > 2)

      logger.info(msecTimeToTextUntilNow(s"Cache updated in: ", startCacheUpdate))
      logger.info(s"${cleanedUniqueLabeled.length}/${numberOfLabeled + labeled.length} unique labeled nodes kept.")
      logger.info(updatedNodeCache.toString)

      var weights = featureStats.computeIG_F(cleanedUniqueLabeled, Some(updatedNodeCache))
      weights =
        if (weights.forall(_._2 == 0) || weights.forall(_._2 == Double.PositiveInfinity)) Map.empty[Feature, Double]
        else weights

      println(weights.toList.sortBy(_._2).reverse.mkString("\n"))

      println("SIGNIFICANCE")
      weights.keySet.foreach { f =>
        val s = featureStats.wComputeBetaDependencyDegree_F(0, weights.keySet - f, cleanedUniqueLabeled, weights, None /*Some(updatedNodeCache)*/ )
        println(s"$f -> $s")
      }

      println("INCONSISTENCY")
      weights.keySet.foreach { f =>
        val s = featureStats.inconsistency_F(Set(f), cleanedUniqueLabeled, Some(updatedNodeCache))
        println(s"$f -> $s")
      }

      //println("Keeping (weighted):")
      //println(cleanedUniqueLabeled.map(_.toText).mkString("\n"))
      //println(cleanedUniqueLabeled.map(n => n.toText -> n.atoms.map(weights.getOrElse(_, 1d)).sum / n.size).sortBy(_._2).reverse.map(x => x._1 + " -> " + x._2).mkString("\n"))
      //val best = featureStats.test(0, cleanedUniqueLabeled, None/*Some(updatedNodeCache)*/)
      //println(best)

      /*val core = featureStats.core(cleanedNodes)
      val gamma = featureStats.wComputeBetaDependencyDegree_F(0.0, core, cleanedUniqueLabeled, weights, Some(updatedNodeCache))
      val gamma_C = featureStats.wComputeBetaDependencyDegree_F(0.0, weights.keySet, cleanedUniqueLabeled, weights, Some(updatedNodeCache))
      println("CORE: " + core + " CORE GAMMA: " + gamma + " C GAMMA: " + gamma_C)

      println("SIGNIFICANCE")
      println(s"TOTAL ${featureStats.wComputeBetaDependencyDegree_F(0, weights.keySet, cleanedUniqueLabeled, weights, None/*Some(updatedNodeCache)*/)}")
      weights.keySet.foreach { f =>
        val s = featureStats.strictComputeBetaDependencyDegree_F(Set(f), cleanedUniqueLabeled, weights, None/*Some(updatedNodeCache)*/)
        println(s"$f -> $s")
      }

      println("FORWARD SELECTION GAMMA")
      val fs = featureStats.roughSetFS(0, cleanedUniqueLabeled, Some(updatedNodeCache))
      println("FS: " + fs)*/

      //val kept = featureStats.roughSetFS(0.0, cleanedUniqueLabeled, Some(updatedNodeCache))
      //println(kept)
      //val general = generalise(cl.reduce(_ ++ _), weights.keySet -- fs2/*, Some(updatedNodeCache)*/)
      //val general = generalise(cleanedUniqueLabeled, weights.keySet.filterNot(f => f.signature.symbol == "Walking" || f.signature.symbol == "Exit")/*, Some(updatedNodeCache)*/)
      //println(general.map(_.toText).mkString("\n"))
      //weights = if (weights.nonEmpty) weights.map { case (f, d) => if (kept.contains(f)) f -> d else f -> 0d } else weights
      //println(weights.mkString("\n"))

      //println
      //println
      cleanedUniqueLabeled.map(n => n.toText + " -> " + updatedNodeCache.get(n).get).foreach(println)
      val f1 = featureStats.optimization(cleanedUniqueLabeled, Some(nodeCache))
      println(f1)

      val general1 = generalise(cleanedUniqueLabeled, weights.keySet -- f1)
      println(general1.map(_.toText).mkString("\n"))

      /*
       * RendezVous
       * k: 1
       * Work (better or the same): 2, 3, 4, 5
       *
       * PilotOps
       * k: 1
       * Work (better or the same): 1, 2, 4,
       * k: 2
       * Work (better or the same): 1, 2, 4, 5, 6
       *
       * TODO Mutual information NOT so good!
       * CHECK SIGNIFICANCE AGAIN
       */

      // Labeled nodes MUST appear before unlabeled!
      new SPLICE(
        cleanedUniqueLabeled ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        metric /*.normalizeWith(weights)*/ ++ mln.evidence ++ pureNodes.flatMap(n => IndexedSeq.fill(n.clusterSize)(n.atoms)),
        annotationBuilder,
        updatedNodeCache,
        featureStats,
        solver,
        enableClusters,
        minNodeSize)
    }
  }
}
