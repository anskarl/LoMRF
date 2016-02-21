package lomrf.mln.learning.structure.hypergraph

import auxlib.log.Logger
import lomrf.logic.AtomSignatureOps._
import lomrf.logic.{AtomSignature, TRUE}
import lomrf.mln.learning.structure.ModeDeclaration
import lomrf.mln.model.AtomIdentityFunctionOps._
import lomrf.mln.model._
import scala.language.implicitConversions

/**
 * Hypergraph is a generalized graph in which an edge can connect any number of vertices. Given a relational
 * example it constructs such a hypergraph using constants as nodes and true ground atoms as hyperedges connecting
 * the nodes via their arguments. Moreover mode declarations are required (as a form of language bias) to constrain
 * the hypergraph creation and the operations over the resulted network.
 *
 * Example:
 * Constants = {Anna, Bob}
 * Ground atoms = {Smokes(Anna), Friends(Anna, Bob)}
 * Hypergraph = {Anna -> [Smokes(Anna), Friends(Anna, Bob)], Bob -> [Friends(Anna, Bob)]}
 *
 * @param mln Markov logic network of the current problem
 * @param annotationDB Annotation database holding the ground truth values for non evidence atoms
 * @param modes Mode declarations for all atom signatures
 */
final class HyperGraph private(annotationDB: EvidenceDB, modes: ModeDeclarations,
                               network: Map[String, Vector[Int]], pathTemplates: Set[PathTemplate])(implicit mln: MLN) {

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
   * @return number of nodes in the hypergraph
   */
  def numberOfNodes: Int = network.size

  /**
   * @return number of edges in the hypergraph
   */
  def numberOfEdges: Int = network.map(tuple => tuple._2.length).sum

  /**
   * Starting from a set of ground atom ids, for each one search the hypergraph for paths
   * containing true ground atoms connected via their arguments.
   *
   * @param searchSet the initial set of atom ids
   * @return a set of paths
   */
  def findPaths(searchSet: Vector[Int], maxLength: Int, allowFreeVariables: Boolean): Set[HPath] = {
    implicit val modes = this.modes
    implicit val identities = this.mln.space.identities

    var paths = Set[HPath]()

    if (pathTemplates.nonEmpty) for { // In case path templates are defined
        id <- searchSet
        template <- pathTemplates
        templateIds = template.extractValidConstants(id, annotationDB)
        tid <- templateIds } {

        val signature = tid.signature
        val constants = identities(signature).decode(tid).get

        val currentPath = HPath.template(tid, signature, constants)
        paths = findPaths(currentPath, constants.toSet, maxLength, allowFreeVariables, paths)
    }
    else for { // In case there are no path templates
      id <- searchSet
      signature = id.signature
      db = mln.evidence.db(signature)
      mode = modes(signature) } {

      var constantSet = Set[String]()

      for {
        constants <- db.identity.decode(id)
        (constant, placemarker) <- constants zip mode.placemarkers
        if placemarker.isInputOrOutput } {
        constantSet += constant // unique elements only, so check is not required
      }

      val currentPath = HPath(id, signature)
      paths = findPaths(currentPath, constantSet, maxLength, allowFreeVariables, paths)
    }

    paths
  }

  /**
   * Starting from a path search hypergraph for paths containing true ground atoms
   * connected via their arguments. Store everything found
   *
   * @param currentPath the current path
   * @param constantSet the constant set holding all constants appearing in the current path
   * @param maxLength the maximum desired length of a path
   * @param allowFreeVariables flag indicating whether free variables are allowed in a path
   * @param paths all stored paths found so far
   *
   * @return a set of stored paths
   */
  private def findPaths(currentPath: HPath, constantSet: Set[String],
                        maxLength: Int, allowFreeVariables: Boolean,
                        paths: Set[HPath]): Set[HPath] = {

    // don't go deeper when the current path has reached the maximum length
    if (currentPath.length >= maxLength) return paths

    var result = paths

    for {
      constant <- constantSet
      storedIDs <- network.get(constant)
      id <- storedIDs
      signature = id.signature
      db = mln.evidence.db(signature)
      mode = modes(signature)
      if canAdd(currentPath, id, signature, mode, db.identity) } {

      // extend the current path
      val newPath = currentPath + (id, signature)

      if (!result.contains(newPath)) { // if the new path is not already exists proceed

        if (allowFreeVariables || (!allowFreeVariables && newPath.hasNoFreeVariables))
          result += newPath // add this path to the result set

        var newConstantSet = constantSet

        // For each constant belonging to the newly added ground atom, appended it to the new constant set
        for {
          decoded <- db.identity.decode(id)
          (constant, placemarker) <- decoded zip mode.placemarkers
          if placemarker.isInputOrOutput && !constantSet.contains(constant) } {
          newConstantSet += constant
        }

        // make recursive call to extend the current path
        result = findPaths(newPath, newConstantSet, maxLength, allowFreeVariables, result)
      }
    }

    result
  }

  /**
   * Check if a given id can be added to the specified path. Specifically it can be added to the path if it is
   * not a duplicate entry, if the number of appearances for this predicate is less than the recall number
   * of the corresponding mode declaration and if the constants appearing as input variables in the mode
   * declaration has been previously appeared in the path.
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

    // Check if number of appearances for this predicate is greater than the recall number
    path.predicateAppearance.get(signature) match {
      case Some(appearance) if appearance == mode.recall => return false
      case _ => ; // do nothing!
    }

    // Check input/output connectivity
    if (path.length > 0) for( (constant, placemarker) <- identityFunction.decode(atomID).get zip mode.placemarkers) {
      // If this is an input constant and has not been previously appeared in the path do not add it
      if(placemarker.isInputOnly && !path.contains(constant)) return false
    }

    true
  }

  def toText =
    if(network.nonEmpty) network.map {
      case (node, edges) => node + " -> " + edges.map(atomID => atomID.decodeAtom(mln).getOrElse(sys.error(s"failed to decode atom id: '$atomID'"))).mkString(", ")
    }.mkString("\n")
    else "Hypergraph is empty!"

  override def toString = network.map(tuple => tuple._1 + " -> [" + tuple._2.mkString(", ") + "]").mkString("\n")
}

/**
 * Hypergraph object
 */
object HyperGraph {

  private lazy val log = Logger(this.getClass)

  def apply(mln: MLN, evidenceDB: EvidenceDB, annotationDB: EvidenceDB, modes: ModeDeclarations, pathTemplates: Option[Set[PathTemplate]] = None) = {

    import log._

    // Network map from constants to atom ids
    var network = Map[String, Vector[Int]]()

    /**
     * Process the given database and add nodes and hyperedges to the existing
     * network. Specifically for all ground atom in the database being TRUE, append
     * one node for each constant belonging to the current atom if the placemarker
     * in the mode declaration is input or output (ignore constants having an ignore placemarker).
     *
     * @param evidenceDB the evidence database
     */
    def processDBFor(evidenceDB: EvidenceDB) = {

      for {
        (signature, db) <- evidenceDB
        if !pathTemplates.getOrElse(Set.empty).exists(_.contains(signature)) // if a signature is template then mode declaration is not required
        mode = modes.getOrElse(signature, fatal(s"Mode declaration for signature '$signature' does not exist!"))
        if mode.recall > 0 // consider only predicates having positive recall
        placemarkers = mode.placemarkers
        id <- db.identity.indices
        if db(id) == TRUE // consider only atoms being TRUE
        constants <- db.identity.decode(id)
        (constant, placemarker) <- constants zip placemarkers
        if placemarker.isInputOrOutput // consider only constant having input or output placemarker
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
    if(network.isEmpty) warn("Hypergraph is empty (i.e., evidence and annotation sets is empty).")

    // Check if path templates exist and constructs the hypergraph in proportion
    pathTemplates match {

      case None => new HyperGraph(annotationDB, modes, network, Set.empty)(mln)

      case Some(templates) => new HyperGraph(annotationDB, modes, network, templates)(mln)
    }
  }

}

/**
 * Hypergraph path is a set of ground atoms which are connected via their arguments. Path keeps
 * track of predicate and constant appearances as well as the ordering of ground atom ids.
 *
 * @param groundAtomIDs the set of ground atom ids belonging to the path
 * @param predicateAppearance the number of appearances for each predicate
 * @param constantAppearance the number of appearances of a constant as variable in the path
 * @param ordering the ordering of appended ground atom ids
 * @param modes mode declarations (implicit)
 * @param identities identity functions (implicit)
 */
class HPath private (val groundAtomIDs: Set[Int],
                     val predicateAppearance: Map[AtomSignature, Int],
                     val constantAppearance: Map[String, Int],
                     val ordering: List[(Int, AtomSignature)])
                    (implicit modes: ModeDeclarations, identities: Identities) {

  /*
   * Check if this path has constants appearing only once as varabilized and therefore
   * it will produce clauses having at least one free variable. Value is false if a
   * varabilized constant exist appearing only once, true otherwise.
   */
  lazy val hasNoFreeVariables: Boolean = constantAppearance.forall(_._2 != 1)

  /**
   * Add the given id to the path if it is not a duplicate entry. Update the number of appearances for
   * this predicate and the number
   *
   * @param atomID the id of the ground atom
   * @param signature the atom signature
   * @return a new path with the atom id appended
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
    for( (constant, placemarker) <- identityFunction.decode(atomID).get zip mode.placemarkers; if placemarker.isInputOrOutput) {
      _constantAppearance.get(constant) match {
        // Keep track of constant appearances in the path along with the number of times that will remain as a variable
        case Some(times) if !placemarker.constant => _constantAppearance += constant -> (times + 1)
        case Some(times) if placemarker.constant => // do nothing
        case None => _constantAppearance += constant -> !placemarker.constant
      }
    }

    // Update predicate appearance count
    _predicateAppearance.get(signature) match {
      case Some(times) => _predicateAppearance += signature -> (times + 1)
      case None => _predicateAppearance += signature -> 1
    }

    // Everything is fine, return a newly created path having the id appended
    new HPath(groundAtomIDs + atomID, _predicateAppearance, _constantAppearance, (atomID, signature) :: ordering)
  }

  /**
   * Checks if a ground atom id exists in the path.
   *
   * @param atomID the atom id to search for in the path
   * @return true if the given ground atom id exists in the path, false otherwise
   */
  def contains(atomID: Int) = groundAtomIDs.contains(atomID)

  /**
   * Checks if a ground atom exists in the path having the given signature.
   *
   * @param signature the signature of the predicate to search for in the path
   * @return true if a ground atom exists having the given signature, false otherwise
   */
  def contains(signature: AtomSignature) = predicateAppearance.contains(signature)

  /**
   * Checks if a constant exists in the path.
   *
   * @param constant the constant to search for in the path
   * @return true if the constant exists, false otherwise
   */
  def contains(constant: String) = constantAppearance.contains(constant)

  /**
   * @return the number of ground atoms in the path
   */
  def length = groundAtomIDs.size

  /**
   * Equality checking method for paths.
   *
   * @param other other object
   * @return true if paths contain exactly identical ids, false otherwise
   */
  override def equals(other: Any) = other match {
    case obj: HPath => groundAtomIDs == obj.groundAtomIDs
    case _ => false
  }

  /**
   * @return the hash code of ground atom ids set
   */
  override def hashCode(): Int = groundAtomIDs.##

  /**
   * @param mln Markov logic network of the current problem
   * @return the hypergraph path into text format
   */
  def toText(mln: MLN) = s"{ ${groundAtomIDs.map(_.decodeAtom(mln).get).mkString(", ")} }"

  override def toString = s"{ ${groundAtomIDs.mkString(", ")} }"

  private implicit def boolean2Int(b: Boolean): Int = if (b) 1 else 0
}

/**
 * Hypergraph path object. Supporting constructors for empty path creation or
 * a path initially having a single ground atom.
 */
object HPath {

  def empty(implicit modes: ModeDeclarations, identities: Identities): HPath = new HPath(Set.empty, Map.empty, Map.empty, Nil)

  def apply(atomID: Int, signature: AtomSignature)(implicit modes: ModeDeclarations, identities: Identities): HPath = {
    empty + (atomID, signature)
  }

  private[hypergraph] def template(atomID: Int, signature: AtomSignature, constants: IndexedSeq[String])
                                  (implicit modes: ModeDeclarations, identities: Identities): HPath = {

    var _constantAppearance = Map[String, Int]()

    // Update constant appearances
    for (constant <- identities(signature).decode(atomID).get) {
      _constantAppearance.get(constant) match {
        // Keep track of constant appearances in the path along with the number of times that will remain as a variable
        case Some(times) => _constantAppearance += constant -> (times + 1)
        case None => _constantAppearance += constant -> 1
      }
    }

    new HPath(Set(atomID), Map(signature -> 1), _constantAppearance, (atomID, signature) :: List())
  }

}