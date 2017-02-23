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

package lomrf.mln.learning.supervision

import lomrf.logic._
import lomrf.{AUX_PRED_PREFIX => PREFIX}
import lomrf.mln.model.EvidenceDB

package object metric {

  type CostMatrix[T] = Seq[Seq[T]]

  /**
    * An AuxConstruct represents an auxiliary ground predicate structure.
    *
    * @note The constant sequence must not include the corresponding function
    *       return constant.
    *
    * @param signature the signature of the predicate
    * @param constants a sequence of constants
    */
  private[metric] case class AuxConstruct(signature: AtomSignature, constants: IndexedSeq[Constant])

  /**
    * Collect all auxiliary predicates given an evidence database and construct a
    * mapping from function return constants to auxiliary construct objects.
    *
    * @see [[lomrf.mln.learning.supervision.metric.AuxConstruct]]
    *
    * @param evidenceDB an evidence database
    * @return a map from function return constants to auxiliary constructs
    */
  private[metric] def collectAuxConstructs(evidenceDB: EvidenceDB) =
    for {
      (signature, db) <- evidenceDB.filter { case (signature, _) => signature.symbol.contains(PREFIX) }
      id <- db.identity.indices.filter(db(_) == TRUE)
      constants <- db.identity.decode(id).toOption
    } yield Constant(constants.head) -> AuxConstruct(signature, constants.tail.map(Constant))
}