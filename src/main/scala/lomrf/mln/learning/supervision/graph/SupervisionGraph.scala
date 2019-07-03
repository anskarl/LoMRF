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

import breeze.linalg.{ DenseMatrix, DenseVector }
import lomrf.logic._
import lomrf.mln.learning.supervision.graph.caching.{ FastNodeCache, NodeCache, NodeHashSet }
import lomrf.mln.model.builders.EvidenceBuilder
import lomrf.util.time._
import lomrf.mln.learning.supervision.metric.Metric
import lomrf.mln.model.{ Evidence, EvidenceDB, MLN, ModeDeclarations }
import lomrf.util.logging.Implicits._
import lomrf.{ AUX_PRED_PREFIX => PREFIX }
import scala.util.{ Failure, Success }
import com.typesafe.scalalogging.LazyLogging

abstract class SupervisionGraph(
    nodes: IndexedSeq[Node],
    querySignature: AtomSignature,
    connector: GraphConnector,
    metric: Metric[_ <: AtomicFormula],
    supervisionBuilder: EvidenceBuilder,
    nodeCache: NodeCache) extends LazyLogging {

  protected val (labeledNodes, unlabeledNodes) = nodes.partition(_.isLabeled)

  lazy val numberOfNodes: Int = nodes.length
  lazy val numberOfLabeled: Int = labeledNodes.length
  lazy val numberOfUnlabeled: Int = unlabeledNodes.length

  /**
    * @param potentials potentials for nodes
    * @return a set of labeled query atoms along the fully labeled annotation database.
    */
  def completeSupervision(potentials: Map[EvidenceAtom, Double] = Map.empty): (Set[EvidenceAtom], Evidence) = {
    if (unlabeledNodes.isEmpty) {
      logger.warn("No unlabeled nodes found!")
      (Set.empty[EvidenceAtom], supervisionBuilder.result())
    } else if (labeledNodes.isEmpty) {
      logger.warn("No labeled nodes found. Set all unlabeled nodes to FALSE due to close world assumption!")
      val unlabeledAtoms = unlabeledNodes.head.labelUsingValue(FALSE).toSet
      supervisionBuilder.evidence ++= unlabeledAtoms
      (unlabeledAtoms, supervisionBuilder.result())
    } else {
      val completedAtoms = optimize(potentials)
      supervisionBuilder.evidence ++= completedAtoms
      (completedAtoms, supervisionBuilder.result())
    }
  }

  /**
    * @param potentials potentials for nodes
    * @return a set of complete evidence atoms.
    */
  protected def optimize(potentials: Map[EvidenceAtom, Double]): Set[EvidenceAtom]

  /**
    * Extends the graph to include nodes produced by a given MLN and an annotation database.
    * The labeled nodes are cached as they are useful for labeling the unlabeled query
    * atoms of the given annotation database.
    *
    * @note The resulted graph would have identical connector and metric types as the old one.
    *
    * @param mln an MLN
    * @param annotationDB an annotation database
    * @param modes a map from atom signature to mode declarations
    * @return a graph having only the labeled nodes of this one and all nodes
    *         produced by the given MLN and annotation database
    */
  def ++(mln: MLN, annotationDB: EvidenceDB, modes: ModeDeclarations): SupervisionGraph
}

object SupervisionGraph extends LazyLogging {

  /**
    * Partitions a given evidence database into nodes according to a given list of domains. The
    * domains must exist in the predicate schema of the query. Each node contains all evidence atoms
    * relevant to each domain constant and corresponds to a single ground query atom.
    *
    * @note In case no domains are given, all domains are used instead.
    * @see [[lomrf.mln.learning.structure.ModeDeclaration]]
    *
    * @param mln an MLN
    * @param modes a map from atom signature to mode declarations
    * @param annotationDB an annotation database
    * @param querySignature the query signature of interest
    * @param clusterUnlabeled cluster unlabeled examples according to unification (default is false)
    * @return an indexed sequence of nodes. Labeled nodes appear before unlabelled
    */
  def partition(
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature,
      clusterUnlabeled: Boolean = false): IndexedSeq[Node] = {

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
    * Constructs a batch supervision graph (SPLICE). The nodes are constructed given an MLN, an annotation
    * database and a query signature. Moreover, a graph connector and a metric is required in order to be
    * able to label the unlabeled ground query atoms.
    *
    * @see [[lomrf.mln.learning.structure.ModeDeclaration]]
    *
    * @param mln an MLN
    * @param modes a map from atom signature to mode declarations
    * @param annotationDB an annotation database
    * @param querySignature the query signature of interest
    * @param connector a graph connector
    * @param metric a metric for atomic formula
    * @param enableClusters enables clustering of unlabeled examples
    * @return a SPLICE supervision graph instance
    */
  def SPLICE(
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature,
      connector: GraphConnector,
      metric: Metric[_ <: AtomicFormula],
      solver: (GraphMatrix, GraphMatrix, DenseVector[Double]) => DenseVector[Double],
      enableClusters: Boolean): SPLICE = {

    // Group the given data into nodes
    val nodes = connector match {
      case _: kNNTemporalConnector | _: eNNTemporalConnector | _: aNNTemporalConnector =>
        partition(mln, modes, annotationDB, querySignature)
      case _ => partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = enableClusters)
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

    new SPLICE(
      uniqueLabeled ++ nonEmptyUnlabeled,
      querySignature,
      connector,
      metric ++ mln.evidence ++ nodes.map(_.atoms),
      annotationBuilder,
      nodeCache,
      solver,
      enableClusters
    )
  }

  /**
    * Constructs a classic nearest neighbor graph. The nodes are constructed given an MLN, an annotation
    * database and a query signature. Moreover, a graph connector and a metric is required in order to be
    * able to label the unlabeled ground query atoms.
    *
    * @see [[lomrf.mln.learning.structure.ModeDeclaration]]
    *
    * @param mln an MLN
    * @param modes a map from atom signature to mode declarations
    * @param annotationDB an annotation database
    * @param querySignature the query signature of interest
    * @param connector a graph connector
    * @param metric a metric for atomic formula
    * @param enableClusters enables clustering of unlabeled examples
    * @return a nearest neighbor graph instance
    */
  def nearestNeighbor(
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature,
      connector: GraphConnector,
      metric: Metric[_ <: AtomicFormula],
      enableClusters: Boolean): NNGraph = {

    // Group the given data into nodes
    val nodes = connector match {
      case _: kNNTemporalConnector | _: eNNTemporalConnector | _: aNNTemporalConnector =>
        partition(mln, modes, annotationDB, querySignature)
      case _ => partition(mln, modes, annotationDB, querySignature, clusterUnlabeled = enableClusters)
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

    new NNGraph(
      uniqueLabeled ++ nonEmptyUnlabeled,
      querySignature,
      connector,
      metric ++ mln.evidence ++ nodes.map(_.atoms),
      annotationBuilder,
      nodeCache,
      enableClusters
    )
  }

  /**
    * Constructs a TLP supervision graph. The nodes are constructed given an MLN, an annotation
    * database and a query signature. Moreover, a graph connector and a metric is required in order to be
    * able to label the unlabeled ground query atoms.
    *
    * @see [[lomrf.mln.learning.structure.ModeDeclaration]]
    *
    * @param mln an MLN
    * @param modes a map from atom signature to mode declarations
    * @param annotationDB an annotation database
    * @param querySignature the query signature of interest
    * @param connector a graph connector
    * @param metric a metric for atomic formula
    * @param memory the graph memory (number of unlabeled nodes)
    * @return a temporal label propagation graph instance
    */
  def TLP(
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature,
      connector: GraphConnector,
      metric: Metric[_ <: AtomicFormula],
      solver: (GraphMatrix, GraphMatrix, DenseVector[Double]) => DenseVector[Double],
      memory: Int): StreamingGraph = {

    // Group the given data into nodes
    val nodes = connector match {
      case _: kNNTemporalConnector | _: eNNTemporalConnector | _: aNNTemporalConnector =>
        partition(mln, modes, annotationDB, querySignature)
      case _ => logger.fatal("Temporal label propagation required a temporal connection strategy!")
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

    new StreamingGraph(
      uniqueLabeled ++ nonEmptyUnlabeled,
      querySignature,
      connector,
      metric ++ mln.evidence ++ nodes.map(_.atoms),
      annotationBuilder,
      nodeCache,
      solver,
      IndexedSeq.empty,
      DenseMatrix.zeros[Double](2, 2),
      memory
    )
  }
}
