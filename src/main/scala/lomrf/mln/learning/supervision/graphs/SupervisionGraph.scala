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

package lomrf.mln.learning.supervision.graphs

import lomrf.logic._
import lomrf.util.logging.Implicits._
import breeze.linalg.{ DenseMatrix, DenseVector, sum }
import com.typesafe.scalalogging.LazyLogging
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
  * @see [[lomrf.mln.learning.supervision.graphs.Node]]
  *      [[lomrf.mln.learning.supervision.graphs.GraphConnector]]
  *      [[lomrf.mln.learning.supervision.metric.Matcher]]
  *      [[lomrf.mln.learning.supervision.metric.StructureMetric]]
  *
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
    nodeCache: Set[(Clause, Long)] = Set.empty) extends LazyLogging {

  private val (labeledNodes, unlabeledNodes) = nodes.partition(_.isLabeled)

  private lazy val parallelIndices = nodes.indices.par
  private lazy val parallelLabeledIndices = labeledNodes.indices.par

  val numberOfNodes: Int = nodes.length
  val numberOfLabeled: Int = labeledNodes.length
  val numberOfUnlabeled: Int = unlabeledNodes.length

  /**
    * @return a set of labeled query atoms along the fully labeled annotation database
    */
  def completeSupervision: (Set[EvidenceAtom], Evidence) = {
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

  private def nearestNeighbor: Set[EvidenceAtom] = {

    // Cost symmetric matrix U x L
    val W = DenseMatrix.zeros[Double](numberOfUnlabeled, numberOfLabeled)

    logger.info(
      s"Supervision graph has $numberOfNodes nodes. Nodes have varying size sequences " +
        s"of evidence atoms [${nodes.map(_.size).distinct.mkString(", ")}].\n" +
        s"\t\t- Labeled Nodes: $numberOfLabeled\n" +
        s"\t\t- Unlabeled Nodes: $numberOfUnlabeled\n" +
        s"\t\t- Query Signature: $querySignature")

    val startGraphConnection = System.currentTimeMillis()

    for (i <- unlabeledNodes.indices) {
      val neighborCosts = DenseVector.zeros[Double](numberOfLabeled)

      for (j <- parallelLabeledIndices) {
        neighborCosts(j) = 1 - {
          metric match {
            case x: StructureMetric => x.distance(unlabeledNodes(i).evidence, labeledNodes(j).evidence)
            case x: AtomMetric      => x.distance(unlabeledNodes(i).atoms, labeledNodes(j).atoms)
            case x: BinaryMetric    => x.distance(unlabeledNodes(i).atoms, labeledNodes(j).atoms)
          }
        }

        logger.whenDebugEnabled {
          logger.debug(s"${unlabeledNodes(i).query.toText} - ${labeledNodes(j).query.toText} = ${neighborCosts(j)}")
        }
      }

      W(i, ::).inner := connector(neighborCosts)
    }

    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis()

    val labeledEvidenceAtoms = for {
      (node, idx) <- unlabeledNodes.zipWithIndex
    } yield {
      val nearest = W(idx, ::).inner.toArray.zipWithIndex
        .withFilter { case (v, _) => v != UNCONNECTED }
        .map {
          case (v, i) =>
            val freq = nodeCache.find { case (c, _) => c =~= labeledNodes(i).clause.get } match {
              case Some((_, count)) => count
              case None => logger.fatal(
                s"Pattern ${labeledNodes(i).clause.get.toText()} is not unique, but it does not exist in the frequency set.")
            }

            v -> (labeledNodes(i).isPositive, freq)
        }

      val (positive, negative) = nearest.partition { case (_, (tv, _)) => tv }

      val value =
        if (positive.map(_._2._2).sum > negative.map(_._2._2).sum) true
        else if (negative.map(_._2._2).sum > positive.map(_._2._2).sum) false
        else nearest.maxBy { case (v, _) => v }._2._1

      println(node)
      println(positive.map(_._2._2).sum)
      println(negative.map(_._2._2).sum)

      EvidenceAtom(node.query.symbol, node.query.terms, value)
    }

    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    supervisionBuilder.evidence ++= labeledEvidenceAtoms
    labeledEvidenceAtoms.toSet
  }

  private def graphCut: Set[EvidenceAtom] = {

    // Cost symmetric matrix
    val W = DenseMatrix.zeros[Double](numberOfNodes, numberOfNodes)

    // Cost degree diagonal matrix
    val D = DenseMatrix.zeros[Double](numberOfNodes, numberOfNodes)

    logger.info(
      s"Supervision graph has $numberOfNodes nodes. Nodes have varying size sequences " +
        s"of evidence atoms [${nodes.map(_.size).distinct.mkString(", ")}].\n" +
        s"\t\t- Labeled Nodes: $numberOfLabeled\n" +
        s"\t\t- Unlabeled Nodes: $numberOfUnlabeled\n" +
        s"\t\t- Query Signature: $querySignature")

    val startGraphConnection = System.currentTimeMillis()

    for (i <- nodes.indices) {
      val neighborCosts = DenseVector.zeros[Double](numberOfNodes)

      for (j <- parallelIndices if i != j) { // A node cannot be connected to itself

        // W is symmetric and therefore there is no need to compute both upper and lower triangular parts
        if (i > j) neighborCosts(j) = W(j, i)
        else if (nodes(i).isLabeled && nodes(j).isLabeled) neighborCosts(j) = UNCONNECTED
        else neighborCosts(j) = 1 - {
          metric match {
            case x: StructureMetric => x.distance(unlabeledNodes(i).evidence, labeledNodes(j).evidence)
            case x: AtomMetric      => x.distance(unlabeledNodes(i).atoms, labeledNodes(j).atoms)
            case x: BinaryMetric    => x.distance(unlabeledNodes(i).atoms, labeledNodes(j).atoms)
          }
        }

        logger.whenDebugEnabled {
          if (i <= j) logger.debug(s"${nodes(i).query.toText} - ${nodes(j).query.toText} = ${neighborCosts(j)}")
        }
      }

      W(i, ::).inner := connector(neighborCosts)
      D(i, i) = sum(W(i, ::))
    }

    logger.info(msecTimeToTextUntilNow(s"Graph connected in: ", startGraphConnection))

    val startSolution = System.currentTimeMillis

    // Vector holding the labeled values
    val fl = DenseVector(labeledNodes.map(_.value).toArray)

    val solution = GraphCut.HFc(W, D, fl).toArray
    val truthValues = solution.map(value => if (value <= UNCONNECTED) FALSE else TRUE)

    logger.info(msecTimeToTextUntilNow(s"Labeling solution found in: ", startSolution))

    logger.whenDebugEnabled {
      (unlabeledNodes.map(_.query) zip solution)
        .map { case (atom, state) => s"$atom = $state" }.mkString("\n")
    }

    val labeledEvidenceAtoms = for {
      (query, value) <- unlabeledNodes.map(_.query) zip truthValues
    } yield EvidenceAtom(query.symbol, query.terms, value)

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
    val currentNodes = SupervisionGraph.partition(mln, modes, annotationDB, querySignature)

    // Partition nodes into labeled and unlabeled. Then find empty unlabeled nodes.
    val (labeled, unlabeled) = currentNodes.partition(_.isLabeled)
    val (nonEmptyUnlabeled, emptyUnlabeled) = unlabeled.partition(_.nonEmpty)

    // Labeled query atoms and empty unlabeled query atoms as FALSE.
    val labeledEntries =
      labeled.map(_.query) ++ emptyUnlabeled.map(x => EvidenceAtom.asFalse(x.query.symbol, x.query.terms))

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
        metric ++ mln.evidence,
        annotationBuilder,
        nodeCache)
    else {

      val startCacheUpdate = System.currentTimeMillis

      /*
       * Update the cache using only non empty labeled nodes, i.e., nodes having at least one
       * evidence predicate. Keep only unique pattern. Moreover, update the pattern frequencies
       * present in the cache accordingly.
       */
      val (uniqueLabeled, updatedNodeCache) =
        labeled.filter(_.nonEmpty).foldLeft(labeledNodes -> nodeCache) {
          case ((unique, cache), node) =>

            val pattern = node.clause.getOrElse(logger.fatal("Cannot construct a pattern!"))

            if (!unique.flatMap(_.clause).exists(_ =~= pattern))
              cache.find { case (c, _) => c =~= pattern } match {
                case Some(entry @ (_, frequency)) => (unique :+ node, (cache - entry) + (pattern -> (frequency + 1)))
                case None                         => (unique :+ node, cache + (pattern -> 1))
              }
            else cache.find { case (c, _) => c =~= pattern } match {
              case Some(entry @ (_, frequency)) => (unique, (cache - entry) + (pattern -> (frequency + 1)))
              case None                         => logger.fatal(s"Pattern ${pattern.toText()} is not unique, but it does not exist in the frequency set.")
            }
        }

      logger.info(s"${uniqueLabeled.length}/${labeledNodes.length + labeled.length} unique labeled nodes kept.")

      logger.whenDebugEnabled {
        updatedNodeCache.foreach { case (clause, freq) => logger.debug(s"${clause.toText()} -> $freq") }
      }

      /*
       * For each unique labeled node, search for an inverse pattern. Inverse patterns,
       * are patterns having identical body but inverse sense in the head. For the inverse
       * pattern and the current node test the Hoeffding bound in order to remove the noisy node.
       */
      val cleanedUniqueLabeled = uniqueLabeled.foldLeft(IndexedSeq.empty[Node]) {
        case (result, node) =>

          val nodeBody = node.body.getOrElse(logger.fatal("Cannot construct a pattern!"))
          val nodeClause = node.clause.getOrElse(logger.fatal("Cannot construct a pattern!"))

          val nodeFrequency = updatedNodeCache.find { case (c, _) => c =~= nodeClause } match {
            case Some((_, freq)) => freq
            case None            => logger.fatal(s"Pattern ${nodeClause.toText()} does not exist in the frequency set.")
          }

          updatedNodeCache.find {
            case (c, _) =>
              val (headLiteral, bodyLiterals) = c.literals.partition(_.sentence.signature == querySignature)
              headLiteral.head.positive != node.isPositive && Clause(bodyLiterals) =~= nodeBody
          } match {
            case Some((_, inversePatternFreq)) if !(HoeffdingBound(
              nodeFrequency.toDouble / (nodeFrequency + inversePatternFreq).toDouble,
              inversePatternFreq.toDouble / (nodeFrequency + inversePatternFreq).toDouble,
              nodeFrequency + inversePatternFreq) && nodeFrequency < inversePatternFreq) => result :+ node
            case None => result :+ node
            case _    => result
          }
      }

      logger.info(msecTimeToTextUntilNow(s"Cache updated in: ", startCacheUpdate))

      // Labeled nodes MUST appear before unlabeled!
      new SupervisionGraph(
        cleanedUniqueLabeled ++ nonEmptyUnlabeled,
        querySignature,
        connector,
        metric ++ mln.evidence,
        annotationBuilder,
        updatedNodeCache)
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
    * @see [[lomrf.mln.learning.supervision.graphs.Node]]
    *      [[lomrf.mln.learning.structure.ModeDeclaration]]
    * @param mln an MLN
    * @param modes mode declarations
    * @param annotationDB an annotation database
    * @param querySignature the query signature of interest
    * @return an indexed sequence of nodes. Labeled nodes appear before unlabelled
    */
  private def partition(
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature): IndexedSeq[Node] = {

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

    if (labeled.isEmpty) logger.warn("There are no labeled query atoms in the annotation database!")

    val start = System.currentTimeMillis

    val nodes = (labeled ++ unlabeled).par.map { queryAtom =>
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

      if (queryAtom.state == UNKNOWN) Node(queryAtom, evidenceSeq, None, None)
      else asPattern(querySignature, evidenceSeq :+ queryAtom, mln, modes) match {
        case Success(clause) =>
          val body = Clause(clause.literals.filterNot(_.sentence.signature == querySignature))
          Node(queryAtom, evidenceSeq, Some(clause), Some(body))
        case Failure(error) => throw error
      }
    }

    logger.info(s"Nodes constructed in ${msecTimeToTextUntilNow(start)}")
    nodes.toIndexedSeq
  }

  /**
    * Constructs a supervision graph. The nodes are constructed given an MLN, an annotation database,
    * a query signature and optionally a list of domains to group by. Moreover, a connector and a matcher
    * are required in order to be able to label the unlabeled ground query atoms.
    *
    * @see
    *     [[lomrf.mln.learning.supervision.graphs.GraphConnector]]
    *     [[lomrf.mln.learning.structure.ModeDeclaration]]
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
      metric: Metric[_ <: AtomicFormula]): SupervisionGraph = {

    // Group the given data into nodes
    val nodes = partition(mln, modes, annotationDB, querySignature)

    logger.info("Constructing supervision graph.")

    // Partition nodes into labeled and unlabeled. Then find empty unlabeled nodes.
    val (labeledNodes, unlabeledNodes) = nodes.partition(_.isLabeled)
    val (nonEmptyUnlabeled, emptyUnlabeled) = unlabeledNodes.partition(_.nonEmpty)

    val startCacheConstruction = System.currentTimeMillis

    /*
     * Create a cache using only non empty labeled nodes, i.e., nodes having at least
     * one evidence predicate. Keep only unique patterns in the cache along their frequencies.
     */
    val (uniqueLabeled, nodeCache) = labeledNodes.filter(_.nonEmpty)
      .foldLeft(IndexedSeq.empty[Node] -> Set.empty[(Clause, Long)]) {
        case ((unique, cache), node) =>
          val pattern = node.clause.getOrElse(logger.fatal("Cannot construct a pattern!"))

          if (!unique.flatMap(_.clause).exists(_ =~= pattern))
            cache.find { case (c, _) => c =~= pattern } match {
              case Some(entry @ (_, frequency)) => (unique :+ node, (cache - entry) + (pattern -> (frequency + 1)))
              case None                         => (unique :+ node, cache + (pattern -> 1))
            }
          else cache.find { case (c, _) => c =~= pattern } match {
            case Some(entry @ (_, frequency)) => (unique, (cache - entry) + (pattern -> (frequency + 1)))
            case None                         => logger.fatal(s"Pattern ${pattern.toText()} is not unique, but it does not exist in the frequency set.")
          }
      }

    logger.info(msecTimeToTextUntilNow(s"Cache constructed in: ", startCacheConstruction))
    logger.info(s"${uniqueLabeled.length} / ${labeledNodes.length} unique labeled nodes kept.")

    logger.whenDebugEnabled {
      nodeCache.foreach { case (clause, freq) => logger.debug(s"${clause.toText()} -> $freq") }
    }

    // Labeled query atoms and empty unlabeled query atoms as FALSE.
    val labeledEntries =
      labeledNodes.map(_.query) ++ emptyUnlabeled.map(x => EvidenceAtom.asFalse(x.query.symbol, x.query.terms))

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
      metric ++ mln.evidence,
      annotationBuilder,
      nodeCache
    )
  }

  /**
    * Constructs a kNN connected graph. Essentially the kNN graph connects each node only to the
    * k nearest neighbors, the ones having the top k distances. The nodes are constructed given an
    * MLN, an annotation database, a query signature and optionally a list of domains to group by.
    * Moreover, a matcher is required in order to be able to label the unlabeled ground query atoms.
    *
    * @see
    *      [[lomrf.mln.learning.supervision.graphs.kNNConnector]]
    *      [[lomrf.mln.learning.structure.ModeDeclaration]]
    * @param k the number of nearest neighbors to be retained
    * @param mln an MLN
    * @param modes mode declarations
    * @param annotationDB an annotation database
    * @param querySignature the query signature of interest
    * @param metric a metric for atomic formula
    * @return a kNN supervision graph instance
    */
  def kNNGraph(
      k: Int,
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature,
      metric: Metric[_ <: AtomicFormula]): SupervisionGraph =
    apply(mln, modes, annotationDB, querySignature, kNNConnector(k), metric)

  /**
    * Constructs a eNN connected graph. Essentially the eNN graph connects nodes that have distance
    * greater than a given epsilon value. The nodes are constructed given an MLN, an annotation database,
    * a query signature and optionally a list of domains to group by. Moreover, a matcher is required in
    * order to be able to label the unlabeled ground query atoms.
    *
    * @see
    *      [[lomrf.mln.learning.supervision.graphs.eNNConnector]]
    *      [[lomrf.mln.learning.structure.ModeDeclaration]]
    * @param epsilon the threshold epsilon
    * @param mln an MLN
    * @param modes mode declarations
    * @param annotationDB an annotation database
    * @param querySignature the query signature of interest
    * @param metric a metric for atomic formula
    * @return a eNN supervision graph instance
    */
  def eNNGraph(
      epsilon: Double,
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature,
      metric: Metric[_ <: AtomicFormula]): SupervisionGraph =
    apply(mln, modes, annotationDB, querySignature, eNNConnector(epsilon), metric)

  /**
    * Constructs a fully connected graph. The nodes are constructed given an MLN, an annotation
    * database, a query signature and optionally a list of domains to group by. Moreover, a matcher
    * is required in order to be able to label the unlabeled ground query atoms.
    *
    * @see
    *      [[lomrf.mln.learning.supervision.graphs.FullConnector]]
    *      [[lomrf.mln.learning.structure.ModeDeclaration]]
    * @param mln an MLN
    * @param modes mode declarations
    * @param annotationDB an annotation database
    * @param querySignature the query signature of interest
    * @param metric a metric for atomic formula
    * @return a fully connected supervision graph
    */
  def fullyConnectedGraph(
      mln: MLN,
      modes: ModeDeclarations,
      annotationDB: EvidenceDB,
      querySignature: AtomSignature,
      metric: Metric[_ <: AtomicFormula]): SupervisionGraph =
    apply(mln, modes, annotationDB, querySignature, FullConnector, metric)
}
