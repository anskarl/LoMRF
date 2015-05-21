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
 * Logical Markov Random Fields.
 *
 * Copyright (C) 2012  Anastasios Skarlatidis.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package lomrf.mln.model

import auxlib.log.Logger
import lomrf.logic.AtomSignature
import lomrf.util.AtomIdentityFunction

/**
 * @param identities a map that associates atom signatures to their corresponding identity functions
 * @param orderedAtomSignatures the order of atom signatures according to the complete domain of groundings
 * @param orderedStartIDs the fist index for each atom in the complete domain of groundings
 * @param queryStartID the first index of ground query atom in the MRF
 * @param queryEndID the last index of ground query atom in the MRF
 */
final class PredicateSpace private(val identities: Identities,
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
  val numberOfQueryIDs = queryStartID - queryEndID

  def isOWA(signature: AtomSignature): Boolean = owa.contains(signature)

  def isCWA(signature: AtomSignature): Boolean = cwa.contains(signature)

  def isHidden(signature: AtomSignature): Boolean = hiddenAtoms.contains(signature)

  def isQuery(signature: AtomSignature): Boolean = queryAtoms.contains(signature)

}

object PredicateSpace {

  private val logger = Logger(this.getClass)


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

    logger.whenDebug {
      orderedAtomSignatures.zip(orderedStartIDs).foreach{case (sig, startid) => logger.debug(s"$sig -> $startid") }
    }


    new PredicateSpace(identities, orderedAtomSignatures, orderedStartIDs, queryStartID, queryEndID, queryPredicates, cwa, owa, hiddenPredicates)
  }
}
