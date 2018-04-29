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

package lomrf.mln.learning.structure.hypergraph

import com.typesafe.scalalogging.Logger
import lomrf.logic.AtomSignatureOps._
import lomrf.logic.{ AtomSignature, TRUE }
import lomrf.mln.learning.structure.ModeDeclaration
import lomrf.mln.model.AtomIdentityFunctionOps._
import lomrf.mln.model._
import lomrf.util.logging.Implicits._

import scala.language.implicitConversions
import lomrf.{ AUX_PRED_PREFIX => PREFIX }

/**
  * HyperGraph is a generalized graph in which an edge can connect any number of vertices. Given a relational
  * example it constructs such a HyperGraph using constants as nodes and true ground atoms as hyper edges connecting
  * the nodes via their arguments. Moreover mode declarations are required (as a form of language bias) to constrain
  * the HyperGraph creation and the operations over the resulted network.
  * <p>
  * Example:
  * {{{
  * Constants = {Anna, Bob}
  * Ground atoms = {Smokes(Anna), Friends(Anna, Bob)}
  * HyperGraph = {Anna -> [Smokes(Anna), Friends(Anna, Bob)], Bob -> [Friends(Anna, Bob)]}
  * }}}
  *
  * @param mln Markov logic network of the current problem
  * @param annotationDB annotation database holding the ground truth values for non evidence atoms
  * @param modes mode declarations for all atom signatures
  * @param network map from nodes (constants) to ground atoms (ground atoms ids)
  * @param auxiliary map from function return constants to auxiliary ground predicate ids
  * @param pathTemplates a set of path templates
  */
final class HyperGraph private (annotationDB: EvidenceDB, modes: ModeDeclarations,
    network: Map[String, Vector[Int]], auxiliary: Map[String, Int],
    pathTemplates: Set[PathTemplate])(implicit mln: MLN) {

  /**
    * Get all edges (atom ids) connected to given node (constant).
    *
    * @param node the node (constant)
    * @return a vector of edges (atom ids)
    */
  def apply(node: String): Vector[Int] = network(node)

  /**
    * Get all edges (atom ids) connected to given node (constant).
    *
    * @param node the node (constant)
    * @return a vector of edges (atom ids) if any exists
    */
  def get(node: String): Option[Vector[Int]] = network.get(node)

  /**
    * @return number of nodes in the HyperGraph
    */
  def numberOfNodes: Int = network.size

  /**
    * @return number of edges in the HyperGraph
    */
  def numberOfEdges: Int = network.map { case (_, edges) => edges.length }.sum

  /**
    * Starting from a set of ground atom ids, for each one search the HyperGraph for paths
    * containing true ground atoms connected via their arguments.
    *
    * @param searchSet the initial set of atom ids
    * @param maxLength the maximum length allowed for each path
    * @param allowFreeVariables enables free variables in the paths
    *
    * @return a set of paths
    */
  def findPaths(searchSet: Vector[Int], maxLength: Int, allowFreeVariables: Boolean = false): Set[HPath] = {
    implicit val modes: ModeDeclarations = this.modes
    implicit val identities: Identities = this.mln.space.identities
    implicit val auxiliary: Map[String, Int] = this.auxiliary

    var paths = Set[HPath]()

    if (pathTemplates.nonEmpty) for { // In case path templates are defined
      id <- searchSet
      template <- pathTemplates
      templateIds = template.extractValidConstants(id, annotationDB)
      tid <- templateIds
      signature = tid.signature
      mode = modes(signature)
      identity = identities(signature)
      constants = identity.decode(tid).get
    } {

      /*
       * Check if this search id can be added to an empty path:
       *
       * For a ground atom to be able to added in an empty path the only constraint
       * is the function return values appearing as its arguments (if any) to satisfy
       * their place marker dependencies.
       */
      val currentPath =
        if (canAdd(HPath.empty, tid, signature, mode, identity))
          HPath.head(addAuxPredicates(HPath.empty + (tid, signature), constants))
        else
          sys.error(s"Ground atom ${AtomIdentityFunction.decodeAtom(tid).get} cannot be added to an empty path." +
            s" Please check mode declarations for this ground atom and its corresponding functions!")

      /*
       * Pass as an initial constant set only the constants appearing in the path
       * that are not function return values.
       */
      paths = findPaths(
        currentPath,
        currentPath.constants.filterNot(auxiliary.contains),
        maxLength,
        allowFreeVariables,
        paths)
    }
    else for { // In case there are no path templates
      id <- searchSet
      signature = id.signature
      identity = identities(signature)
      mode = modes(signature)
      constants = identity.decode(id).get
    } {

      /*
       * Check if this search id can be added to an empty path:
       *
       * For a ground atom to be able to added in an empty path the only constraint
       * is the function return values appearing as its arguments (if any) to satisfy
       * their place marker dependencies.
       */
      val currentPath =
        if (canAdd(HPath.empty, id, signature, mode, identity))
          HPath.head(addAuxPredicates(HPath.empty + (id, signature), constants))
        else
          sys.error(s"Ground atom ${AtomIdentityFunction.decodeAtom(id).get} cannot be added to an empty path." +
            s" Please check mode declarations for this ground atom and its corresponding functions!")

      /*
       * Pass as an initial constant set only the constants appearing in the path
       * that are not function return values.
       */
      paths = findPaths(
        currentPath,
        currentPath.constants.filterNot(auxiliary.contains),
        maxLength,
        allowFreeVariables,
        paths)
    }

    paths
  }

  /**
    * Recursively append auxiliary predicates to arbitrary depth given a path
    * and a set of constants.
    *
    * @param path a starting path
    * @param constants the constant set of interest
    *
    * @return a path having auxiliary predicates appended
    */
  private def addAuxPredicates(path: HPath, constants: IndexedSeq[String])(implicit identities: Identities): HPath =
    constants.flatMap(auxiliary.get).foldLeft(path) {
      case (p, auxID) =>
        val auxSignature = auxID.signature
        addAuxPredicates(p + (auxID, auxSignature), identities(auxSignature).decode(auxID).get.tail)
    }

  /**
    * Starting from a path search HyperGraph for paths containing true ground atoms
    * connected via their arguments. Store everything found
    *
    * @param currentPath the current path
    * @param constantSet the constant set holding all constants appearing in the current path
    * @param maxLength the maximum desired length of a path
    * @param allowFreeVariables flag indicating whether free variables are allowed in a path
    * @param paths all stored paths found so far
    * @return a set of stored paths
    */
  private def findPaths(currentPath: HPath, constantSet: Set[String],
      maxLength: Int, allowFreeVariables: Boolean,
      paths: Set[HPath]): Set[HPath] = {

    implicit val identities: Identities = this.mln.space.identities

    // don't go deeper when the current path has reached the maximum length (functions does not count)
    if (currentPath.length - currentPath.auxiliary >= maxLength) return paths

    var result = paths

    for {
      constant <- constantSet
      storedIDs <- network.get(constant)
      id <- storedIDs
      signature = id.signature
      identity = identities(signature)
      mode = modes(signature)
      if canAdd(currentPath, id, signature, mode, identity)
    } {

      // extend the current path and recursively append auxiliary predicates (functions)
      val newPath = addAuxPredicates(currentPath + (id, signature), identity.decode(id).get)

      if (!result.contains(newPath)) { // if the new path is not already exists proceed

        if (allowFreeVariables || (!allowFreeVariables && newPath.hasNoFreeVariables))
          result += newPath // add this path to the result set

        // make recursive call to extend the current path
        result = findPaths(
          newPath,
          newPath.constants.filterNot(auxiliary.contains),
          maxLength,
          allowFreeVariables,
          result)
      }
    }

    result
  }

  /**
    * Check if a given id can be added to the specified path. Specifically it can be added to the path if it is
    * not a duplicate entry, if the number of appearances for this predicate is less than the recall number
    * of the corresponding mode declaration and if the constants appearing as input variables in the mode
    * declaration has been previously appeared in the path. Moreover for the given id it checks recursively if
    * the function return values appearing as its arguments (if any exists) can be added.
    *
    * @param path the path to check
    * @param atomID the id of the ground atom
    * @param signature the atom signature
    * @param mode the mode declaration for the given id
    * @param identityFunction the atom identity function for the given id
    *
    * @return true if the id was successfully added and false otherwise
    */
  private def canAdd(path: HPath, atomID: Int, signature: AtomSignature,
      mode: ModeDeclaration, identityFunction: AtomIdentityFunction): Boolean = {

    // If id already exists do not add it to the path
    if (path.contains(atomID)) return false

    // If signature is incompatible to this path do not add it to the path
    if (path.incompatibleSignatures.contains(signature)) return false

    // Check if number of appearances for this predicate is greater than the recall number
    path.predicateAppearance.get(signature) match {
      case Some(appearance) if appearance == mode.recall => return false
      case _ => ; // do nothing!
    }

    // Check input/output connectivity
    if (path.length > 0) for ((constant, placeMarker) <- identityFunction.decode(atomID).get zip mode.placeMarkers) {
      // If this is an input constant and has not been previously appeared in the path do not add it
      if (placeMarker.isInputOnly && !path.contains(constant)) return false
    }

    /*
     * Check if the ground atom (id) has auxiliary predicate (function) return constants:
     *
     * For each constant present in the ground atom check if there is any auxiliary ID containing that constant
     * as its left most argument (using auxiliary mapping). If none exists, then its safe to add.
     *
     * Otherwise, for each existing auxiliary ID check if it can be added (recursively). That way search can support
     * nested function addition at one step of the HyperGraph search.
     *
     * NOTE: Because the procedure is recursive, each canAdd call collects the constants belonging to an auxiliary
     * predicate by removing the left most one (call tail). That is because the left most argument is always the
     * return value of the function (auxiliary predicate) and if we do not remove it then the recursion will be infinite.
     */
    val constants =
      if (signature.symbol.contains(PREFIX))
        identityFunction.decode(atomID).get.tail
      else identityFunction.decode(atomID).get

    constants.flatMap(auxiliary.get).forall { auxiliaryID =>
      val auxSignature = auxiliaryID.signature
      val auxIdentity = mln.evidence.db(auxSignature).identity
      canAdd(path + (atomID, signature), auxiliaryID, auxSignature, modes(auxSignature), auxIdentity)
    }
  }

  def toText: String =
    if (network.nonEmpty) network.map {
      case (node, edges) =>
        node + " -> " + edges.map { atomID =>
          atomID.decodeAtom(mln).getOrElse(sys.error(s"Failed to decode atom id: '$atomID'"))
        }.mkString(", ")
    }.mkString("\n")
    else "HyperGraph is empty!"

  override def toString: String = network.map { case (node, edges) => node + " -> " + edges.mkString("[", ", ", "]") }.mkString("\n")
}

/**
  * HyperGraph object that enables construction of HyperGraphs given the evidence and annotation database.
  */
object HyperGraph {

  private lazy val logger = Logger(this.getClass)

  def apply(mln: MLN, evidenceDB: EvidenceDB, annotationDB: EvidenceDB, modes: ModeDeclarations, pathTemplates: Option[Set[PathTemplate]] = None): HyperGraph = {

    // Network map from constants to ground atom ids
    var network = Map[String, Vector[Int]]()

    // Map from auxiliary predicate (function) return constant to ground auxiliary atom ids
    var auxiliary = Map[String, Int]()

      /**
        * Process the given database and add nodes and hyperEdges to the existing
        * network. Specifically for all ground atom in the database being TRUE, append
        * one node for each constant belonging to the current atom if the placeMarker
        * in the mode declaration is input or output (ignore constants having an ignore placeMarker).
        *
        * @param evidenceDB the evidence database
        */
      def processDBFor(evidenceDB: EvidenceDB): Unit = {

        for {
          (signature, db) <- evidenceDB.filter { case (signature, _) => signature.symbol.contains(PREFIX) }
          mode = modes.getOrElse(
            signature,
            logger.fatal(s"Mode declaration for function signature '${signature.symbol.replaceFirst(PREFIX, "")}/${signature.arity - 1}' does not exist!"))
          if mode.recall > 0 // consider only auxiliary predicates having positive recall
          id <- db.identity.indices
          if db(id) == TRUE // consider only auxiliary atoms being TRUE
          constants <- db.identity.decode(id)
        } {
          auxiliary += constants.head -> id
        }

        for {
          (signature, db) <- evidenceDB.filterNot { case (signature, _) => signature.symbol.contains(PREFIX) }
          mode = modes.getOrElse(signature, logger.fatal(s"Mode declaration for signature '$signature' does not exist!"))
          if mode.recall > 0 // consider only predicates having positive recall in their mode declaration
          placeMarkers = mode.placeMarkers
          id <- db.identity.indices.filter(db(_) == TRUE) // consider only ground atoms being TRUE
          constants <- db.identity.decode(id)
          (constant, placeMarker) <- constants zip placeMarkers
          // consider only constant having input or output placeMarker AND not considered function return values
          if placeMarker.isInputOrOutput && !auxiliary.contains(constant)
        } network.get(constant) match {

          case Some(ids) =>
            network += constant -> (ids :+ id)

          case None =>
            network += constant -> Vector(id)
        }
      }

    // Processing evidence database
    processDBFor(evidenceDB)

    // Processing annotation database
    processDBFor(annotationDB)

    // Check if network is empty
    if (network.isEmpty) logger.warn("HyperGraph is empty (i.e., evidence and annotation sets is empty).")

    // Check if auxiliary predicates (functions) exist.
    if (auxiliary.isEmpty) logger.warn("There are no given functions (as auxiliary predicates) in the evidence database.")

    // Check if path templates exist and constructs the hyperGraph in proportion
    pathTemplates match {

      case None            => new HyperGraph(annotationDB, modes, network, auxiliary, Set.empty)(mln)

      case Some(templates) => new HyperGraph(annotationDB, modes, network, auxiliary, templates)(mln)
    }
  }

}

/**
  * HyperGraph path is a set of ground atoms which are connected via their arguments. Path keeps
  * track of predicate and constant appearances as well as the ordering of ground atom ids.
  *
  * @param groundAtomIDs the set of ground atom ids belonging to the path
  * @param predicateAppearance the number of appearances for each predicate
  * @param constantAppearance the number of appearances of a constant as variable in the path
  * @param incompatibleSignatures atom signatures incompatible to this path (cannot be added)
  * @param ordering the ordering of appended ground atom ids
  * @param auxiliary the number of auxiliary predicates in this path
  * @param modes mode declarations (implicit)
  * @param identities identity functions (implicit)
  */
class HPath private (
    val groundAtomIDs: Set[Int],
    val predicateAppearance: Map[AtomSignature, Int],
    val constantAppearance: Map[String, Int],
    val incompatibleSignatures: Set[AtomSignature],
    val ordering: List[(Int, AtomSignature)],
    val auxiliary: Int)(implicit modes: ModeDeclarations, identities: Identities) {

  /*
   * Check if this path has constants appearing only once and therefore it will
   * produce clauses having at least one free variable. Value is false if a constant
   * exist appearing only once, true otherwise.
   */
  lazy val hasNoFreeVariables: Boolean =
    constantAppearance.forall { case (_, appearance) => appearance != 1 }

  /**
    * Add the given id to the path if it is not a duplicate entry. Update the number of appearances for
    * this predicate and the number of appearances for its constants.
    *
    * @param atomID the id of the ground atom
    * @param signature the atom signature
    *
    * @return a new path having the given atom id appended
    */
  def +(atomID: Int, signature: AtomSignature): HPath = {

    // If id already exists do not add it to the path (do nothing)
    if (groundAtomIDs.contains(atomID)) return this

    // Proceed, update constant and predicate appearances and add the id to the path.
    val identityFunction = identities(signature)
    val mode = modes(signature)

    var _constantAppearance = constantAppearance
    var _predicateAppearance = predicateAppearance

    // Update constant appearances
    for ((constant, placeMarker) <- identityFunction.decode(atomID).get zip mode.placeMarkers; if placeMarker.isInputOrOutput) {
      _constantAppearance.get(constant) match {
        // Keep track of constant appearances in the path along with the number of times that will remain as a variable
        case Some(times) if !placeMarker.constant => _constantAppearance += constant -> (times + 1)
        case Some(_) if placeMarker.constant      => // do nothing
        case None                                 => _constantAppearance += constant -> !placeMarker.constant
      }
    }

    // Update predicate appearance count
    _predicateAppearance.get(signature) match {
      case Some(times) => _predicateAppearance += signature -> (times + 1)
      case None        => _predicateAppearance += signature -> 1
    }

    val _auxiliary = if (signature.symbol.contains(PREFIX)) auxiliary + 1 else auxiliary

    // Everything is fine, return a newly created path having the id appended
    new HPath(groundAtomIDs + atomID, _predicateAppearance, _constantAppearance, incompatibleSignatures, (atomID, signature) :: ordering, _auxiliary)
  }

  /**
    * Checks if a ground atom id exists in the path.
    *
    * @param atomID the atom id to search for in the path
    * @return true if the given ground atom id exists in the path, false otherwise
    */
  def contains(atomID: Int): Boolean = groundAtomIDs.contains(atomID)

  /**
    * Checks if a ground atom exists in the path having the given signature.
    *
    * @param signature the signature of the predicate to search for in the path
    * @return true if a ground atom exists having the given signature, false otherwise
    */
  def contains(signature: AtomSignature): Boolean = predicateAppearance.contains(signature)

  /**
    * Checks if a constant exists in the path.
    *
    * @param constant the constant to search for in the path
    * @return true if the constant exists, false otherwise
    */
  def contains(constant: String): Boolean = constantAppearance.contains(constant)

  /**
    * @return a set of constants appearing in the path
    */
  def constants: Set[String] = constantAppearance.keySet

  /**
    * @return a set of atom signatures (including auxiliary) appearing in the path
    */
  def predicates: Set[AtomSignature] = predicateAppearance.keySet

  /**
    * @return the number of ground atoms (including auxiliary) in the path
    */
  def length: Int = groundAtomIDs.size

  /**
    * Equality checking method for paths.
    *
    * @param other another object
    * @return true if paths contain exactly identical ids, false otherwise
    */
  override def equals(other: Any): Boolean = other match {
    case obj: HPath => groundAtomIDs == obj.groundAtomIDs
    case _          => false
  }

  /**
    * @return the hash code of ground atom ids set
    */
  override def hashCode(): Int = groundAtomIDs.##

  /**
    * @param mln a Markov logic network
    * @return the HyperGraph path into text format
    */
  def toText(mln: MLN) = s"{ ${groundAtomIDs.map(_.decodeAtom(mln).get).mkString(", ")} }"

  override def toString = s"{ ${groundAtomIDs.mkString(", ")} }"

  private implicit def boolean2Int(b: Boolean): Int = if (b) 1 else 0
}

/**
  * HyperGraph path object. Supporting constructors for empty path creation or
  * a path initially having a single ground atom.
  */
object HPath {

  /**
    * @return an empty path
    */
  def empty(implicit modes: ModeDeclarations, identities: Identities): HPath =
    new HPath(Set.empty, Map.empty, Map.empty, Set.empty, Nil, 0)

  /**
    * Constructs a singleton path containing only a ground atom id and its signature.
    *
    * @param atomID the ground atom id
    * @param signature the atom signature
    *
    * @return a singleton path containing only the given ground atom id
    */
  def apply(atomID: Int, signature: AtomSignature)(implicit modes: ModeDeclarations, identities: Identities): HPath = empty + (atomID, signature)

  /**
    * Constraints the given path to incorporate incompatible signatures that cannot
    * be added into the resulting path. Therefore the path resembles the head of a clause
    * and the incompatible signatures define constrains in the body (following ground atoms).
    *
    * @param path the given path
    *
    * @return a copy of the given path containing incompatible signatures
    */
  def head(path: HPath)(implicit modes: ModeDeclarations, identities: Identities): HPath = {

    val incompatibleSignatures =
      (modes.keySet intersect path.predicates) flatMap modes.get flatMap (_.incompatibleSignatures)

    new HPath(
      path.groundAtomIDs,
      path.predicateAppearance,
      path.constantAppearance,
      incompatibleSignatures,
      path.ordering,
      path.auxiliary)
  }
}
