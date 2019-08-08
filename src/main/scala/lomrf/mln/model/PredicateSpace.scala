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

package lomrf.mln.model

import com.typesafe.scalalogging.Logger
import lomrf.logic.AtomSignature

/**
  * Predicate space encodes the space of ground predicates using integers.
  *
  * @see [[lomrf.mln.model.AtomIdentityFunction]]
  *
  * @param identities a map that associates atom signatures to their corresponding identity functions
  * @param orderedAtomSignatures an array of ordered atom signatures according to the domain of groundings
  * @param orderedStartIDs an array of ordered start ids for each atom signature
  * @param queryStartID the start id of the ground query atoms
  * @param queryEndID the last id of the ground query atoms
  * @param queryAtoms a set of query atom signatures
  * @param cwa a set of atom signatures subject to close world assumption
  * @param owa a set of atom signatures subject to open world assumption
  * @param hiddenAtoms a set of hidden atom signatures
  */
final class PredicateSpace private (
    val identities: Identities,
    val orderedAtomSignatures: Array[AtomSignature],
    val orderedStartIDs: Array[Int],
    val queryStartID: Int,
    val queryEndID: Int,
    val queryAtoms: Set[AtomSignature],
    val cwa: Set[AtomSignature],
    val owa: Set[AtomSignature],
    val hiddenAtoms: Set[AtomSignature]) {

  /** Total number of ground query atoms. */
  val numberOfQueryIDs: Int = queryStartID - queryEndID

  /**
    * @param signature an atom signature
    * @return true if the atom is subject to open world assumption, false otherwise
    */
  def isOWA(signature: AtomSignature): Boolean = owa.contains(signature)

  /**
    * @param signature an atom signature
    * @return true if the atom is subject to close world assumption, false otherwise
    */
  def isCWA(signature: AtomSignature): Boolean = cwa.contains(signature)

  /**
    * @param signature an atom signature
    * @return true if the atom is hidden, false otherwise
    */
  def isHidden(signature: AtomSignature): Boolean = hiddenAtoms.contains(signature)

  /**
    * @param signature an atom signature
    * @return true if the atom is a query atom, false otherwise
    */
  def isQuery(signature: AtomSignature): Boolean = queryAtoms.contains(signature)

  /**
    * @note If the literal number is positive then it encodes a positive
    *       literal, otherwise it encodes a negative one.
    *
    * @param literal a literal number
    * @return the atom signature for the given literal number
    */
  def signatureOf(literal: Int): AtomSignature = {
    val atomID = math.abs(literal)
    val result = java.util.Arrays.binarySearch(orderedStartIDs, atomID)
    val position = if (result < 0) (-result) - 2 else result

    orderedAtomSignatures(position)
  }
}

object PredicateSpace {

  private val logger = Logger(this.getClass)

  /**
    * @see [[lomrf.mln.model.ConstantsSet]]
    *
    * @param schema an MLN schema
    * @param nonEvidence a set of non evidence atom signatures
    * @param constants a map from domain names to constant sets
    * @return a PredicateSpace instance
    */
  def apply(
      schema: MLNSchema,
      nonEvidence: Set[AtomSignature],
      constants: ConstantsDomain): PredicateSpace = {
    PredicateSpace(schema.predicates, nonEvidence, Set.empty[AtomSignature], constants)
  }

  /**
    * @see [[lomrf.mln.model.ConstantsSet]]
    *
    * @param schema an MLN schema
    * @param queryPredicates a set of query atom signatures
    * @param hiddenPredicates a set of hidden atom signatures
    * @param constants a map from domain names to constant sets
    * @return a PredicateSpace instance
    */
  def apply(
      schema: MLNSchema,
      queryPredicates: Set[AtomSignature],
      hiddenPredicates: Set[AtomSignature],
      constants: ConstantsDomain): PredicateSpace = {
    PredicateSpace(schema.predicates, queryPredicates, hiddenPredicates, constants)
  }

  /**
    * @see [[lomrf.mln.model.ConstantsSet]]
    *
    * @param predicateSchema a map from atom signatures to their argument domain names
    * @param queryPredicates a set of query atom signatures
    * @param hiddenPredicates a set of hidden atom signatures
    * @param constants a map from domain names to constant sets
    * @return a PredicateSpace instance
    */
  def apply(
      predicateSchema: PredicateSchema,
      queryPredicates: Set[AtomSignature],
      hiddenPredicates: Set[AtomSignature],
      constants: ConstantsDomain): PredicateSpace = {

    val owa = queryPredicates ++ hiddenPredicates
    val cwa = predicateSchema.keySet -- owa

    var identities = Map.empty[AtomSignature, AtomIdentityFunction]

    var currentID = 1
    val orderedStartIDs = new Array[Int](predicateSchema.size)
    val orderedAtomSignatures = new Array[AtomSignature](predicateSchema.size)
    var index = 0

    // Query predicates
    for ((signature, atomSchema) <- predicateSchema; if queryPredicates.contains(signature)) {
      orderedStartIDs(index) = currentID
      orderedAtomSignatures(index) = signature
      index += 1
      val idFunction = AtomIdentityFunction(signature, atomSchema, constants, currentID)
      currentID += idFunction.length + 1
      identities += (signature -> idFunction)
      logger.debug(s"$signature {range: [${idFunction.startID}, ${idFunction.endID}], length: ${idFunction.length}}")
    }

    val queryStartID = 1
    val queryEndID = currentID - 1

    // Other OWA predicates
    for ((signature, atomSchema) <- predicateSchema; if hiddenPredicates.contains(signature)) {
      orderedStartIDs(index) = currentID
      orderedAtomSignatures(index) = signature
      index += 1
      val idFunction = AtomIdentityFunction(signature, atomSchema, constants, currentID)
      currentID += idFunction.length + 1
      identities += (signature -> idFunction)
      logger.debug(s"$signature {range: [${idFunction.startID}, ${idFunction.endID}], length: ${idFunction.length}}")
    }

    // CWA predicates (Evidence predicates)
    for ((signature, atomSchema) <- predicateSchema; if cwa.contains(signature)) {
      orderedStartIDs(index) = currentID
      orderedAtomSignatures(index) = signature
      index += 1
      val idFunction = AtomIdentityFunction(signature, atomSchema, constants, currentID)
      currentID += idFunction.length + 1
      identities += (signature -> idFunction)
      logger.debug(s"$signature {range: [${idFunction.startID}, ${idFunction.endID}], length: ${idFunction.length}}")
    }

    logger.whenDebugEnabled {
      orderedAtomSignatures.zip(orderedStartIDs).foreach { case (sig, startID) => logger.debug(s"$sig -> $startID") }
    }

    new PredicateSpace(
      identities,
      orderedAtomSignatures,
      orderedStartIDs,
      queryStartID,
      queryEndID,
      queryPredicates,
      cwa,
      owa,
      hiddenPredicates
    )
  }
}
