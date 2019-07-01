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
import lomrf.util.logging.Implicits._
import breeze.linalg.DenseVector
import com.typesafe.scalalogging.LazyLogging
import lomrf.mln.learning.supervision.graph.caching.{ FastNodeCache, NodeCache, NodeHashSet }
import lomrf.mln.learning.supervision.metric._
import lomrf.mln.model._
import lomrf.mln.model.builders.EvidenceBuilder
import lomrf.util.time._
import lomrf.{ AUX_PRED_PREFIX => PREFIX }
import scala.util.{ Failure, Success }
import scala.language.existentials

/**
  * Supervision graph represents a graph having nodes for a given query signature. These
  * nodes contain a single ground query atom and a sequence of evidence atoms sharing
  * constants to the corresponding query atom. Nodes can be either labeled (the ground query
  * atom is TRUE or FALSE) or unlabeled. The graph is connected using a specified connector
  * strategy and can be solved in order to label the unlabeled ground query atoms.
  *
  * @see [[lomrf.mln.learning.supervision.graph.Node]]
  *      [[lomrf.mln.learning.supervision.metric.Matcher]]
  * @param nodes an indexed sequence of nodes. Labeled nodes appear before unlabelled
  * @param querySignature the query signature of interest
  * @param connector a graph connector
  * @param metric a metric for atomic formula
  * @param supervisionBuilder a supervision evidence builder that contains the completed annotation
  * @param nodeCache a cache for frequent patterns
  */
final class SupervisionGraph private (
    nodes: IndexedSeq[Node],
    querySignature: AtomSignature,
    connector: GraphConnector,
    metric: Metric[_ <: AtomicFormula],
    supervisionBuilder: EvidenceBuilder,
    nodeCache: NodeCache,
    cluster: Boolean) extends LazyLogging {

  private val (labeledNodes, unlabeledNodes) = nodes.partition(_.isLabeled)

  val numberOfNodes: Int = nodes.length
  val numberOfLabeled: Int = labeledNodes.length
  val numberOfUnlabeled: Int = unlabeledNodes.length

  /**
    * @return a set of labeled query atoms along the fully labeled
    *         annotation database.
    */
  def completeSupervisionGraphCut: (Set[EvidenceAtom], Evidence) = {
    if (unlabeledNodes.isEmpty) {
      logger.warn("No unlabeled query atoms found!")
      (Set.empty[EvidenceAtom], supervisionBuilder.result())
    } else if (labeledNodes.isEmpty) {
      logger.warn("No labeled query atoms found. Set all unlabeled to FALSE due to close world assumption!")
      val unlabeledAtoms = unlabeledNodes.map { node =>
        EvidenceAtom.asFalse(node.query.symbol, node.query.terms)
      }.toSet
      supervisionBuilder.evidence ++= unlabeledAtoms
      (unlabeledAtoms, supervisionBuilder.result())
    } else (graphCut, supervisionBuilder.result())
  }

  /**
    * @return a set of labeled query atoms along the fully labeled
    *         annotation database.
    */
  def completeSupervisionNN: (Set[EvidenceAtom], Evidence) = {
    if (unlabeledNodes.isEmpty) {
      logger.warn("No unlabeled query atoms found!")
      (Set.empty[EvidenceAtom], supervisionBuilder.result())
    } else if (labeledNodes.isEmpty) {
      logger.warn("No labeled query atoms found. Set all unlabeled to FALSE due to close world assumption!")
      val unlabeledAtoms = unlabeledNodes.map { node =>
        EvidenceAtom.asFalse(node.query.symbol, node.query.terms)
      }.toSet
      supervisionBuilder.evidence ++= unlabeledAtoms
      (unlabeledAtoms, supervisionBuilder.result())
    } else (nearestNeighbor, supervisionBuilder.result())
  }

  private def nearestNeighbor: Set[EvidenceAtom] = {

    logger.info(
      s"Supervision graph has $numberOfNodes nodes. Nodes have varying size sequences " +
        s"of evidence atoms [${nodes.map(_.size).distinct.mkString(", ")}].\n" +
        s"\t\t- Labeled Nodes: $numberOfLabeled\n" +
        s"\t\t- Unlabeled Nodes: $numberOfUnlabeled\n" +
        s"\t\t- Query Signature: $querySignature"
    )

    val startGraphConnection = System.currentTimeMillis
    val W = connector.connect(unlabeledNodes, labeledNodes)(metric)
    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis

    val labeledEvidenceAtoms = unlabeledNodes.zipWithIndex.flatMap {
      case (node, i) =>

        val nearest = W(i, ::).inner.toArray.zipWithIndex
          .withFilter { case (v, _) => v != UNCONNECTED }
          .map {
            case (v, j) =>
              val freq = nodeCache.getOrElse(labeledNodes(j), logger.fatal(s"Pattern ${labeledNodes(j).clause.get.toText()} not found."))
              v -> (labeledNodes(j).isPositive, freq)
          }

        val (positive, negative) = nearest.partition { case (_, (tv, _)) => tv }

        val value =
          if (nearest.length == 0) false
          else if (positive.map(_._2._2).sum > negative.map(_._2._2).sum) true
          else if (negative.map(_._2._2).sum > positive.map(_._2._2).sum) false
          else nearest.maxBy { case (v, _) => v }._2._1

        node.labelUsingValue(value)
    }

    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    supervisionBuilder.evidence ++= labeledEvidenceAtoms
    labeledEvidenceAtoms.toSet
  }

  private def graphCut: Set[EvidenceAtom] = {

    logger.info(
      s"Supervision graph has $numberOfNodes nodes. Nodes have varying size sequences " +
        s"of evidence atoms [${nodes.map(_.size).distinct.mkString(", ")}].\n" +
        s"\t\t- Labeled Nodes: $numberOfLabeled\n" +
        s"\t\t- Unlabeled Nodes: $numberOfUnlabeled\n" +
        s"\t\t- Query Signature: $querySignature"
    )

    val startGraphConnection = System.currentTimeMillis
    val encodedGraph = connector.connect(nodes, labeledNodes, unlabeledNodes)(metric)
    val W = encodedGraph._1
    val D = encodedGraph._2
    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis

    // Vector holding the labeled values
    val fl = DenseVector(labeledNodes.map(_.value).toArray)

    val solution = GraphOps.HFc(W, D, fl).toArray
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

  /**
    * Extends this supervision graph to include nodes produced by a given MLN and
    * an annotation database. Only the labeled nodes are retained from this graph
    * as they are useful for completing the unlabelled query atoms of the given
    * annotation database.
    *
    * @note The resulted supervision graph would have identical connection type as the
    *       old one and also identical matcher function and metric type. Moreover the
    *       introduced evidence is going to be partitioned based on the old graph
    *       group by domain. Thus, you cannot partition the introduced evidence based
    *       on another domain.
    * @param mln an MLN
    * @param annotationDB an annotation database
    * @param modes mode declarations
    * @return a supervision graph having only the labeled nodes of this one
    *         and all nodes produced by the given MLN and annotation database
    */
  def ++(mln: MLN, annotationDB: EvidenceDB, modes: ModeDeclarations): SupervisionGraph = {

    // Group the given data into nodes, using the domains of the existing graph
    val currentNodes = connector match {
      case _: kNNTemporalConnector | _: eNNTemporalConnector =>
        SupervisionGraph.partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = false)
      case _ =>
        if (cluster) SupervisionGraph.partition(mln, modes, annotationDB, querySignature)
        else SupervisionGraph.partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = false)
    }

    // Partition nodes into labeled and unlabeled. Then find empty unlabeled nodes.
    val (labeled, unlabeled) = currentNodes.partition(_.isLabeled)
    val (nonEmptyUnlabeled, emptyUnlabeled) = unlabeled.partition(_.nonEmpty)

    // Labeled query atoms and empty unlabeled query atoms as FALSE.
    val labeledEntries =
      labeled.map(_.query) ++ emptyUnlabeled.flatMap(_.labelUsingValue(FALSE))

    if (emptyUnlabeled.nonEmpty)
      logger.warn(s"Found ${emptyUnlabeled.length} empty unlabeled nodes. Set them to FALSE.")

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
    if (labeled.isEmpty)
      new SupervisionGraph(
        nodes.takeWhile(_.isLabeled) ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence ++ nodes.map(_.atoms),
        annotationBuilder,
        nodeCache,
        cluster)
    else {
      /*
       * Update the cache using only non empty labeled nodes, i.e., nodes having at least
       * one evidence predicate in their body
       *
       * Cache stores only unique nodes (patterns) along their counts.
       */
      val startCacheUpdate = System.currentTimeMillis

      var updatedNodeCache = nodeCache
      updatedNodeCache ++= labeled.filter(_.nonEmpty)
      val cleanedUniqueLabeled = updatedNodeCache.collectNodes

      logger.info(msecTimeToTextUntilNow(s"Cache updated in: ", startCacheUpdate))
      logger.info(s"${cleanedUniqueLabeled.length}/${labeledNodes.length + labeled.length} unique labeled nodes kept.")
      logger.debug(updatedNodeCache.toString)

      // Labeled nodes MUST appear before unlabeled!
      new SupervisionGraph(
        cleanedUniqueLabeled ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence ++ nodes.map(_.atoms),
        annotationBuilder,
        updatedNodeCache,
        cluster)
    }
  }
}

/**
  * Supervision graph object enables the construction of various types of supervision
  * graphs given an MLN, an annotation database, a query atom signature of interest and
  * a list of domains to group the data.
  */
object SupervisionGraph extends LazyLogging {

  /**
    * Partitions the given evidence database into nodes according to a given list of domains. The
    * domains must exist in the predicate schema of the query. Each node contains all evidence atoms
    * relevant to each domain constant and corresponds to a single ground query atom.
    *
    * @note In case no domains are given, all domains are used instead.
    * @see [[lomrf.mln.learning.structure.ModeDeclaration]]
    *
    * @param mln an MLN
    * @param modes mode declarations
    * @param annotationDB an annotation database
    * @param querySignature the query signature of interest
    * @return an indexed sequence of nodes. Labeled nodes appear before unlabelled
    */
  private[graph] def partition(
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature,
      clusterUnlabeled: Boolean = true): IndexedSeq[Node] = {

    val predicateSchema = mln.schema.predicates
    val auxPredicateSchema = predicateSchema.filter { case (signature, _) => signature.symbol.contains(PREFIX) }

    // Check if the given annotation database contains the query signature of interest.
    val queryAnnotationDB =
      annotationDB.getOrElse(
        querySignature,
        logger.fatal(s"Query signature '$querySignature' does not exist in the given annotation database.")
      )

    // Collect all auxiliary predicates
    val auxiliary = for {
      (signature, db) <- mln.evidence.db.filter { case (signature, _) => signature.symbol.contains(PREFIX) }
      id <- db.identity.indices.filter(db(_) == TRUE)
      constants <- db.identity.decode(id).toOption
    } yield Constant(constants.head) -> (signature, constants.tail.map(Constant))

    if (auxiliary.isEmpty) logger.warn(s"No auxiliary predicates found in the evidence database.")

      /**
        * Recursively constructs a map of domains to constants by flattening all given pairs
        * of domain-constant corresponding to function return types (auxiliary predicates).
        *
        * @param constants an sequence of constants
        * @param domains a sequence of domains for each constant
        * @return a map of flattened domains to constants
        */
      def domain2Constants(constants: IndexedSeq[Constant], domains: Seq[String]): Map[String, IndexedSeq[Constant]] =
        (constants zip domains).foldLeft(Map.empty[String, IndexedSeq[Constant]]) {
          case (result, (constant, domain)) => auxiliary.get(constant) match {
            case None => result.updated(domain, result.getOrElse(domain, Vector.empty) :+ constant)
            case Some((signature, indexedSeq)) =>
              combine(result, domain2Constants(indexedSeq, predicateSchema(signature).tail))
          }
        }

      /**
        * Recursively find all possible flattened domain sequences that
        * can be produced by valid function replacements.
        *
        * @param domains a sequence of domains
        * @return a sequence of all possible flattened domain sequences
        */
      def flattenDomains(domains: Seq[String]): Seq[Seq[String]] = {
        domains.foldLeft(Seq(Seq.empty[String])) { (result, domain) =>

          val flatSeq = auxPredicateSchema
            .filter { case (_, domainSeq) => domainSeq.head == domain }
            .map { case (_, domainSeq) => domainSeq.tail }
            .flatMap(flattenDomains).toSeq

          if (flatSeq.nonEmpty) result.map(seq => flatSeq.flatMap(_ ++ seq))
          else result.map(_ ++ Seq(domain))
        }
      }

    // Clean the given evidence database by removing auxiliary predicates and query atoms.
    val evidenceDB = mln.evidence.db.filterNot {
      case (signature, _) =>
        modes(signature).recall == 0 || signature.symbol.contains(PREFIX) || mln.space.queryAtoms.contains(signature)
    }

    // Convert all TRUE ground atoms in the cleaned evidence database into evidence atoms
    val evidenceAtoms = evidenceDB.flatMap {
      case (signature, db) =>
        for {
          id <- db.identity.indices
          if db(id) == TRUE
          constants <- db.identity.decode(id).toOption
          terms = constants.map(Constant).toVector
          if !terms.exists { c =>
            auxiliary.get(c) match {
              case Some((auxSignature, _)) => modes(auxSignature).recall == 0
              case None                    => false
            }
          }
        } yield EvidenceAtom(signature.symbol, terms, db(id)) -> domain2Constants(terms, predicateSchema(signature))
    }

    // Convert all query atoms in the annotation database into evidence atoms
    val queryAtoms = for {
      id <- queryAnnotationDB.identity.indices
      constants <- queryAnnotationDB.identity.decode(id).toOption
      terms = constants.map(Constant).toVector
    } yield EvidenceAtom(querySignature.symbol, terms, queryAnnotationDB(id))

    /*
     * Partition the query atoms into labeled and unlabeled in order to produce an
     * ordered node sequence.
     */
    val (unlabeled, labeled) = queryAtoms.partition(_.state == UNKNOWN)
    val orderIndex = modes(querySignature).placeMarkers.indexWhere(_.isOrdered)
    val partitionIndices = modes(querySignature).placeMarkers.zipWithIndex.withFilter(_._1.isPartition).map(_._2)
    if (labeled.isEmpty) logger.warn("There are no labeled query atoms in the annotation database!")

    val start = System.currentTimeMillis

    // A set of cluster nodes used for grouping identical (under unification) unlabeled nodes.
    val clusterSet = new NodeHashSet()

    val nodes = (labeled ++ unlabeled).groupBy(_.constants).flatMap {
      case (_, queryAtomGroup) =>
        val queryAtom = queryAtomGroup.head
        val queryDomain2Constants =
          domain2Constants(queryAtom.terms, predicateSchema(querySignature))

        // TODO: Check if a predicate has no common domain at all with the query atom
        val evidence = evidenceAtoms.flatMap {
          case (atom, domain2Constants) =>
            if (domain2Constants.forall {
              case (domain, constants) =>
                queryDomain2Constants.get(domain) match {
                  case None                 => true
                  case Some(otherConstants) => constants.forall(otherConstants.contains)
                }
            } && domain2Constants.keys.exists(queryDomain2Constants.keySet.contains)) Some(atom)
            else None
        }

        val evidenceSeq = evidence.toIndexedSeq

        asPattern(querySignature, evidenceSeq :+ queryAtom, mln, modes) match {
          case Success(clause) =>
            val (headLiterals, bodyLiterals) = clause.literals.partition(_.sentence.signature == querySignature)
            val body = Clause(bodyLiterals)

            if (queryAtom.state == UNKNOWN) {
              val unlabeledNode = Node(
                queryAtom,
                evidenceSeq,
                None,
                Some(body),
                headLiterals.head.sentence,
                orderIndex,
                partitionIndices)

              // append identical query atoms, e.g., Q(A,B,1) is the same as Q(B,A,1)
              unlabeledNode.similarNodeQueryAtoms ++= queryAtomGroup.tail

              if (clusterUnlabeled) {
                clusterSet += unlabeledNode
                None
              } else Some(unlabeledNode)
            } else {
              val labeledNode = Node(
                queryAtom,
                evidenceSeq,
                Some(clause),
                Some(body),
                headLiterals.head.sentence,
                orderIndex,
                partitionIndices)

              // append identical query atoms, e.g., Q(A,B,1) is the same as Q(B,A,1)
              labeledNode.similarNodeQueryAtoms ++= queryAtomGroup.tail
              Some(labeledNode)
            }

          case Failure(error) => throw error
        }
    }.toIndexedSeq

    logger.info(s"Nodes constructed in ${msecTimeToTextUntilNow(start)}")
    nodes ++ clusterSet.collectNodes
  }

  /**
    * Constructs a supervision graph. The nodes are constructed given an MLN, an annotation database,
    * a query signature and optionally a list of domains to group by. Moreover, a connector and a matcher
    * are required in order to be able to label the unlabeled ground query atoms.
    *
    * @see [[lomrf.mln.learning.structure.ModeDeclaration]]
    *
    * @param mln an MLN
    * @param modes mode declarations
    * @param annotationDB an annotation database
    * @param querySignature the query signature of interest
    * @param connector a graph connector
    * @param metric a metric for atomic formula
    * @return a supervision graph instance
    */
  def apply(
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature,
      connector: GraphConnector,
      metric: Metric[_ <: AtomicFormula],
      cluster: Boolean): SupervisionGraph = {

    // Group the given data into nodes
    val nodes = connector match {
      case _: kNNTemporalConnector | _: eNNTemporalConnector =>
        partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = false)
      case _ =>
        if (cluster) partition(mln, modes, annotationDB, querySignature)
        else partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = false)
    }

    // Partition nodes into labeled and unlabeled. Then find empty unlabeled nodes.
    val (labeledNodes, unlabeledNodes) = nodes.partition(_.isLabeled)
    val (nonEmptyUnlabeled, emptyUnlabeled) = unlabeledNodes.partition(_.nonEmpty)

    /*
     * Create a cache using only non empty labeled nodes, i.e., nodes having at least
     * one evidence predicate in their body.
     *
     * Cache stores only unique nodes (patterns) along their counts.
     */
    val startCacheConstruction = System.currentTimeMillis

    val nodeCache = FastNodeCache(querySignature) ++ labeledNodes.filter(_.nonEmpty)
    val uniqueLabeled = nodeCache.collectNodes

    logger.info(msecTimeToTextUntilNow(s"Cache constructed in: ", startCacheConstruction))
    logger.info(s"${uniqueLabeled.length} / ${labeledNodes.length} unique labeled nodes kept.")
    logger.debug(nodeCache.toString)

    // Labeled query atoms and empty unlabeled query atoms as FALSE.
    val labeledEntries =
      labeledNodes.map(_.query) ++ emptyUnlabeled.flatMap(_.labelUsingValue(FALSE))

    if (emptyUnlabeled.nonEmpty)
      logger.warn(s"Found ${emptyUnlabeled.length} empty unlabeled nodes. Set them to FALSE.")

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

    new SupervisionGraph(
      uniqueLabeled ++ nonEmptyUnlabeled,
      querySignature,
      connector,
      metric ++ mln.evidence ++ nodes.map(_.atoms),
      annotationBuilder,
      nodeCache,
      cluster
    )
  }
}
