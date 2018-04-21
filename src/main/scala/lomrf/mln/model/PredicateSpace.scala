/*
 * o                        o     o   o         o
 * |             o          |     |\ /|         | /
 * |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 * |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 * O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *             |
 *          o--o
 * o--o              o               o--o       o    o
 * |   |             |               |    o     |    |
 * O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 * |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 * o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.mln.model

import com.typesafe.scalalogging.Logger
import lomrf.util.logging.Implicits._
import lomrf.logic.AtomSignature
import lomrf.mln.model.AtomIdentityFunction

/**
 * @param identities a map that associates atom signatures to their corresponding identity functions
 * @param orderedAtomSignatures the order of atom signatures according to the complete domain of groundings
 * @param orderedStartIDs the fist index for each atom in the complete domain of groundings
 * @param queryStartID the first index of ground query atom in the MRF
 * @param queryEndID the last index of ground query atom in the MRF
 */
final class PredicateSpace private(
                                    val identities: Identities,
                                    val orderedAtomSignatures: Array[AtomSignature],
                                    val orderedStartIDs: Array[Int],
                                    val queryStartID: Int,
                                    val queryEndID: Int,
                                    val queryAtoms: Set[AtomSignature],
                                    val cwa: Set[AtomSignature],
                                    val owa: Set[AtomSignature],
                                    val hiddenAtoms: Set[AtomSignature]) {
  
  /**
   * Total number of ground query atoms
   */
  val numberOfQueryIDs: Int = queryStartID - queryEndID

  def isOWA(signature: AtomSignature): Boolean = owa.contains(signature)

  def isCWA(signature: AtomSignature): Boolean = cwa.contains(signature)

  def isHidden(signature: AtomSignature): Boolean = hiddenAtoms.contains(signature)

  def isQuery(signature: AtomSignature): Boolean = queryAtoms.contains(signature)

  def signatureOf(literal: Int): AtomSignature = {

    val atomID = math.abs(literal)
    val result = java.util.Arrays.binarySearch(orderedStartIDs, atomID)
    val position = if(result < 0) (-result) - 2 else result

   orderedAtomSignatures(position)
  }

}

object PredicateSpace {

  private val logger = Logger(this.getClass)

  def apply(schema: MLNSchema,
            nonEvidence: Set[AtomSignature],
            constants: ConstantsDomain): PredicateSpace = {
    PredicateSpace(schema.predicates, nonEvidence, Set.empty[AtomSignature], constants)
  }


  def apply(schema: MLNSchema,
            queryPredicates: Set[AtomSignature],
            hiddenPredicates: Set[AtomSignature],
            constants: ConstantsDomain): PredicateSpace = {
    PredicateSpace(schema.predicates, queryPredicates, hiddenPredicates, constants)
  }

  /**
   *
   * @param predicateSchema
   * @param queryPredicates
   * @param hiddenPredicates
   * @param constants
   *
   * @return a new instance of DomainSpace
   */
  def apply(predicateSchema: PredicateSchema,
            queryPredicates: Set[AtomSignature],
            hiddenPredicates: Set[AtomSignature],
            constants: ConstantsDomain): PredicateSpace = {

    val owa = queryPredicates ++ hiddenPredicates
    val cwa = predicateSchema.keySet -- owa

    var identities: Map[AtomSignature, AtomIdentityFunction] = Map[AtomSignature, AtomIdentityFunction]()

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
      logger.debug(s"$signature {[${idFunction.startID}, ${idFunction.length + idFunction.startID}], length: ${idFunction.length}}")
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
      logger.debug(s"$signature {[${idFunction.startID}, ${idFunction.length + idFunction.startID}], length: ${idFunction.length}}")
    }

    // CWA predicates (Evidence predicates)
    for ((signature, atomSchema) <- predicateSchema; if cwa.contains(signature)) {
      orderedStartIDs(index) = currentID
      orderedAtomSignatures(index) = signature
      index += 1
      val idFunction = AtomIdentityFunction(signature, atomSchema, constants, currentID)
      currentID += idFunction.length + 1
      identities += (signature -> idFunction)
      logger.debug(s"$signature {[${idFunction.startID}, ${idFunction.length + idFunction.startID}], length: ${idFunction.length}}")
    }

    logger.whenDebugEnabled {
      orderedAtomSignatures.zip(orderedStartIDs).foreach{case (sig, startid) => logger.debug(s"$sig -> $startid") }
    }


    new PredicateSpace(identities, orderedAtomSignatures, orderedStartIDs, queryStartID, queryEndID, queryPredicates, cwa, owa, hiddenPredicates)
  }
}
