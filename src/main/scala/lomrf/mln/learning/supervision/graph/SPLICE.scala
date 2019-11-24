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
import breeze.linalg.{DenseMatrix, DenseVector, diag}
import lomrf.mln.learning.supervision.graph.caching.{FastNodeCache, NodeCache}
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
      case _: HFc => connector.smartConnect(nodes, unlabeledNodes/*, Some(nodeCache)*/)(metric)
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
      logger.info(updatedNodeCache.toString)

      var weights = featureStats.computeIG_F(cleanedUniqueLabeled, Some(updatedNodeCache))
      weights =
        if (weights.forall(_._2 == 0) || weights.forall(_._2 == Double.PositiveInfinity)) Map.empty[Feature, Double]
        else weights

      println(weights.toList.sortBy(_._2).reverse.mkString("\n"))

      println("SIGNIFICANCE")
      weights.keySet.foreach { f =>
        val s = 1 - featureStats.wComputeBetaDependencyDegree_F(0, weights.keySet - f, cleanedUniqueLabeled, weights, None /*Some(updatedNodeCache)*/ )
        println(s"$f -> $s")
      }

      println("INCONSISTENCY")
      weights.keySet.foreach { f =>
        val s = featureStats.inconsistency_F(Set(f), cleanedUniqueLabeled, Some(updatedNodeCache))
        println(s"$f -> $s")
      }

      cleanedUniqueLabeled.sortBy(_.isPositive).map(n => n.toText + " -> " + updatedNodeCache.get(n).get).foreach(println)

    /* cleanedUniqueLabeled.sortBy(_.isPositive).map(n => n.toText + " -> " + updatedNodeCache.get(n).get).foreach(println)
      val (w, d) = kNNConnector(2).fullyConnect(cleanedUniqueLabeled.sortBy(_.isPositive)/*, Some(updatedNodeCache)*/)(
        metric.asInstanceOf[HybridMetric].metrics.head)
      val ww = (0 until w.rows).map(w(_, ::).t.toArray).toArray
      //val res = smile.mds.mds(ww, 2).getCoordinates
      if (count.counter > 5) {
        val res1 = smile.manifold.lle(ww, 2, 2).getCoordinates
        val xx = res1.map(a => a.head -> a.last).toSeq

        println(xx.length)

        val labels = smile.clustering.kmeans(res1, k = 3).getClusterLabel

        cleanedUniqueLabeled.sortBy(_.isPositive).zip(labels).map(x => x._1.toText + " " + x._2).foreach(println)

        println(labels.length)
        println

        xx.zip(cleanedUniqueLabeled.sortBy(_.isPositive)).map(x => x._2.toText + " " + x._1).foreach(println)

        val (pxx, nxx) = xx.zip(cleanedUniqueLabeled.sortBy(_.isPositive)).zip(labels).partition(_._1._2.isPositive)

        val pxx1 = pxx.filter(_._2 == 0).map(_._1._1)
        val pxx2 = pxx.filter(_._2 == 1).map(_._1._1)
        val pxx3 = pxx.filter(_._2 == 2).map(_._1._1)

        val nxx1 = nxx.filter(_._2 == 0).map(_._1._1)
        val nxx2 = nxx.filter(_._2 == 1).map(_._1._1)
        val nxx3 = nxx.filter(_._2 == 2).map(_._1._1)

        scalatikz.pgf.plots.Figure(s"mds_${count.counter}")
          .scatter(markStrokeColor = scalatikz.pgf.enums.Color.GREEN, markFillColor = scalatikz.pgf.enums.Color.WHITE, markSize = 3)(pxx.map(_._1._1))
          .scatter(markStrokeColor = scalatikz.pgf.enums.Color.RED, markFillColor = scalatikz.pgf.enums.Color.WHITE, markSize = 2)(nxx.map(_._1._1))
          .saveAsPDF("/home/vagmcs/Desktop")
      }*/
     /* scalatikz.pgf.plots.Figure(s"cl_${count.counter}")
        .scatter(markStrokeColor = scalatikz.pgf.enums.Color.GREEN,
          markFillColor = scalatikz.pgf.enums.Color.WHITE,
          marker = scalatikz.pgf.plots.enums.Mark.CIRCLE,
          markSize = 2)(pxx1)
        .scatter(markStrokeColor = scalatikz.pgf.enums.Color.GREEN,
          markFillColor = scalatikz.pgf.enums.Color.WHITE,
          marker = scalatikz.pgf.plots.enums.Mark.DIAMOND,
          markSize = 2)(pxx2)
        .scatter(markStrokeColor = scalatikz.pgf.enums.Color.GREEN,
          markFillColor = scalatikz.pgf.enums.Color.WHITE,
          marker = scalatikz.pgf.plots.enums.Mark.SQUARE,
          markSize = 2)(pxx3)
        .scatter(markStrokeColor = scalatikz.pgf.enums.Color.RED,
          markFillColor = scalatikz.pgf.enums.Color.WHITE,
          marker = scalatikz.pgf.plots.enums.Mark.CIRCLE,
          markSize = 2)(nxx1)
        .scatter(markStrokeColor = scalatikz.pgf.enums.Color.RED,
          markFillColor = scalatikz.pgf.enums.Color.WHITE,
          marker = scalatikz.pgf.plots.enums.Mark.DIAMOND,
          markSize = 2)(nxx2)
        .scatter(markStrokeColor = scalatikz.pgf.enums.Color.RED,
          markFillColor = scalatikz.pgf.enums.Color.WHITE,
          marker = scalatikz.pgf.plots.enums.Mark.SQUARE,
          markSize = 2)(nxx3)
        .saveAsPDF("/home/vagmcs/Desktop")*/

      /*val l = d - w
     val eg = breeze.linalg.eig(l)

     //assert(l * eg.eigenvectors == eg.eigenvectors * diag(eg.eigenvalues), "WRONG")
     println((l * eg.eigenvectors ).mkString())
     println
     println((eg.eigenvectors * diag(eg.eigenvalues)).mkString())
     println
     println(eg.eigenvectors.mkString())
     println
     println(eg.eigenvalues)
     println(eg.eigenvalues.toArray.sorted.deep)

     //val data = (0 until eg.eigenvectors.rows).map(eg.eigenvectors(_, ::).t.toArray).toArray
     val ww = (0 until w.rows).map(w(_, ::).t.toArray).toArray
     val km = smile.clustering.specc(ww, 2).getClusterLabel //.kmeans(data, 2)
     cleanedUniqueLabeled.zip(km).foreach { case (n, v) =>
       println(n.toText -> v)
     }

     val (c0, c1) = cleanedUniqueLabeled.zip(km).partition(_._2 == 0)
     val maxP = cleanedUniqueLabeled.filter(_.isPositive).maxBy(n => updatedNodeCache.get(n).get)
     val cc = if (c0.exists(_._1 == maxP)) {
       c0.filter(_._1.isPositive).map(_._1) ++ c1.filter(_._1.isNegative).map(_._1)
     } else {
       c1.filter(_._1.isPositive).map(_._1) ++ c0.filter(_._1.isNegative).map(_._1)
     }

     cc.sortBy(_.isPositive).map(n => n.toText + " -> " + updatedNodeCache.get(n).get).foreach(println)*/

      /*val sorted = cleanedUniqueLabeled.sortBy(_.isPositive).zip(eg.eigenvectors(1, ::).t.toArray).sortBy(_._2)
      val n = cleanedUniqueLabeled.filter(_.isPositive).maxBy(n => updatedNodeCache.get(n).get)
      val median = sorted.find(_._1 == n).get._2
      val (c1, c2) = sorted.partition(_._2 <= median)
      c1.map(x => x._1.toText -> x._2).foreach(println)
      c2.map(x => x._1.toText -> x._2).foreach(println)

      val c = if (c1.nonEmpty && c2.nonEmpty) {
        val maxP = cleanedUniqueLabeled.filter(_.isPositive).maxBy(n => updatedNodeCache.get(n).get)
        if (c1.exists(_._1 == maxP)) {
          c1.withFilter(_._1.isPositive).map(_._1) ++ c2.withFilter(_._1.isNegative).map(_._1)
        } else {
          c1.withFilter(_._1.isNegative).map(_._1) ++ c2.withFilter(_._1.isPositive).map(_._1)
        }
      }
      else cleanedUniqueLabeled

      println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
      c.map(_.toText).foreach(println)*/

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
      //cleanedUniqueLabeled.map(n => n.toText + " -> " + updatedNodeCache.get(n).get).foreach(println)
      //val f1 = featureStats.optimization(cleanedUniqueLabeled, Some(nodeCache))
      //println(f1)
//
//      val general1 = generalise(cleanedUniqueLabeled, weights.keySet -- f1)
//      println(general1.map(_.toText).mkString("\n"))


      /**
        * Meet and Move works fine
        */
      /*val (pos, neg) = cleanedUniqueLabeled.partition(_.isPositive)
      val kept =
        if (pos.nonEmpty && neg.nonEmpty)
        IndexedSeq(
          pos.map(n => n -> updatedNodeCache.get(n).get).maxBy(_._2)._1,
          neg.map(n => n -> updatedNodeCache.get(n).get).maxBy(_._2)._1
        )
      else cleanedUniqueLabeled

      kept.map(_.toText).foreach(println)*/

      // CLUSTERING // TODO IT SEEMS TO WORK

      val (pp, nn) = cleanedUniqueLabeled.partition(_.isPositive)
      var clustersP = Set.empty[Set[Node]]
      var clustersN = Set.empty[Set[Node]]
      var best = Set.empty[Set[Node]]

      if (pp.nonEmpty && nn.nonEmpty) {
        val a = pp.maxBy(_.size)
        val b = nn.maxBy(_.size)
        clustersP += Set(a)
        clustersN += Set(b)
        pp.filterNot(_ == a).sortBy(_.size).reverse.foreach { n =>
          val cls = clustersP.filter(c => c.exists(n1 => n.relevantNode(n1)))
          if (cls.size == 1) clustersP = (clustersP - cls.head) + (cls.head + n)
          else if (cls.size == 0) clustersP += Set(n)
        }
        nn.filterNot(_ == b).sortBy(_.size).reverse.foreach { n =>
          val cls = clustersN.filter(c => c.exists(n1 => n.relevantNode(n1)))
          if (cls.size == 1) clustersN = (clustersN - cls.head) + (cls.head + n)
          else if (cls.size == 0) clustersN += Set(n)
        }

        println

        // TODO check clustering, it seems wrong
        (clustersP ++ clustersN).foreach { c =>
          println("#################")
          c.map(n => n.toText + " -> " + updatedNodeCache.get(n).get).foreach(println)
        }

        val bestP = clustersP.maxBy(_.toList.map(n => updatedNodeCache.get(n).get).sum)
        val restP = clustersP - bestP
        val bestN = clustersN.maxBy(_.toList.map(n => updatedNodeCache.get(n).get).sum)
        val restN = clustersN - bestN

        var rest = Set.empty[Set[Node]]
        restN.foreach { c =>
          clustersP.find(c1 => c1.exists(n => c.exists(n1 => connector.connect(n, n1)(metric) > 0.5))) match {
            case Some(x) =>
              rest += c
              rest += x
            case None =>
          }
        }

        //best = rest + bestP + bestN
        println
        /*best.foreach { c =>
          println("#################")
          c.map(n => n.toText + " -> " + updatedNodeCache.get(n).get).foreach(println)
        }*/
        //println(featureStats.core2((bestP ++ bestN).toIndexedSeq))

        Set(bestP, bestN).foreach { c =>
          println("#################")
          c.map(n => n.toText + " -> " + updatedNodeCache.get(n).get).foreach(println)
        }
        println("EXTRA")
        bestP.flatMap(_.createSubNodes).map(_.toNegative).map(_.toText).foreach(println)
        best = Set(bestP, bestN) ++ bestP.map(_.createSubNodes.map(_.toNegative))
      }

      val cc = (clustersP ++ clustersN).flatten.toIndexedSeq
      val bb = best.flatten.toIndexedSeq

      println
      bb.map(_.toText).foreach(println)

      //val res = featureStats.optimization_clusters(clustersP ++ clustersN, Some(updatedNodeCache))
      //println(res)

      /*println
      (clustersP ++ clustersN).flatten.toIndexedSeq.sortBy(_.isPositive).map(_.toText).foreach(println)
      println
      compressed.flatten.toIndexedSeq.sortBy(_.isPositive).map(_.toText).foreach(println)
      println*/


      /////

      //count.reducedCache = FastNodeCache(querySignature)
      val (pos, neg) = cleanedUniqueLabeled.partition(_.isPositive)
      val k1 = pos.filterNot { n =>
        pos.filterNot(_ == n).exists(n1 => n.body.get.subsumes(n1.body.get))
      }

      val k2 = neg.filterNot { n =>
        //println(n.toText)
        //println("%%%%%")
        val e = neg.filterNot(_ == n).exists { n1 =>
          //println(n1.toText)
          val tr = n.clause.get.subsumes(n1.clause.get) /*&& n.atoms.map(_.signature).diff(n1.atoms.map(_.signature)).isEmpty*/
          //println(tr)
          tr
        }
        //println("***********")
        e
      }

      /*val ccg = generalise(cc, weights.keySet -- res, None)
      ccg.map(_.toText).foreach(println)*/

     /* println
      println("TWO BEST")
      (k1 ++ k2).map(_.toText).foreach(println)*/

      // Labeled nodes MUST appear before unlabeled!
      new SPLICE(
        bb ++ nonEmptyUnlabeled,
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
