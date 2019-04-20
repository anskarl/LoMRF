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

package lomrf.mln.learning.supervision.metric

import lomrf.logic.{ Constant, EvidenceAtom }
import lomrf.mln.learning.structure.PlaceMarker
import lomrf.mln.model.{ Evidence, ModeDeclarations }

/**
  * An evidence metric is a measure of distance for logical interpretations.
  * Such a measure enables calculates of structural distance of ground atoms.
  *
  * === Example ===
  * {{{
  *   d(A, A) = 0
  *
  *   d(P(A, B), P(A, B)) = 0
  *
  *   d(P(A, B), Q(A, B)) = 1
  *
  *   d(P(B, A), P(A, C)) =
  *   ( 1 / (2 * arity) ) * [ distance(B, A) + distance(A, C) ]
  *
  *   d(Z(foo(A),B), Z(foo(B),B)) =
  *   (1 / 2) * [ distance(foo(A), foo(B)) + distance(B, B) ] =
  *   (1 / 4) * [ (1 / 2) * distance(A, B) + distance(B, B) ]
  * }}}
  *
  * @see Distance Between Herbrand Interpretations: A Measure for Approximations
  *      to a Target Concept (1997)
  *
  * @param modes mode declarations
  * @param auxConstructs a map from return constants to auxiliary constructs
  * @param matcher a matcher function
  */
final class EvidenceMetric private(
    modes: ModeDeclarations,
    auxConstructs: Map[Constant, AuxConstruct],
    override protected val matcher: Matcher) extends StructureMetric[EvidenceAtom] {

  /**
    * Distance for ground evidence atoms. The function must obey to the following properties:
    *
    * {{{
    * 1. d(x, y) >= 0 for all x, y and d(x, y) = 0 if and only if x = y
    * 2. d(x, y) = d(y, x) for all x, y
    * 3. d(x, y) + d(y, z) >= d(x, z) for all x, y, z (triangle inequality)
    * }}}
    *
    * @param xAtom an evidence atom
    * @param yAtom another evidence atom
    * @return a distance for the given evidence atoms
    */
  override def distance(xAtom: EvidenceAtom, yAtom: EvidenceAtom): Double =
    if (xAtom.state != yAtom.state || xAtom.signature != yAtom.signature) 1
    else modes.get(xAtom.signature) match {
      case Some(mode) =>
        constantSeqDistance(xAtom.terms, yAtom.terms, Some(mode.placeMarkers))
      case None =>
        constantSeqDistance(xAtom.terms, yAtom.terms, None)
    }

  /**
    * Distance for auxiliary predicates, e.g., functions.
    *
    * @param xConstruct an auxiliary predicate
    * @param yConstruct another auxiliary predicate
    * @return a distance in the interval [0, 1] for the given auxiliary predicates.
    */
  @inline private def functionDistance(xConstruct: AuxConstruct, yConstruct: AuxConstruct): Double =
    if (xConstruct.signature != yConstruct.signature) 1
    else modes.get(xConstruct.signature) match {
      case Some(mode) => constantSeqDistance(xConstruct.constants, yConstruct.constants, Some(mode.placeMarkers.tail))
      case None       => constantSeqDistance(xConstruct.constants, yConstruct.constants, None)
    }

  /**
    * Distance for constant sequences.
    *
    * @param constantSeqA a constant sequence
    * @param constantSeqB another constant sequence
    * @return a distance in the interval [0, 1] for the given constant sequences
    */
  @inline private def constantSeqDistance(
      constantSeqA: IndexedSeq[Constant],
      constantSeqB: IndexedSeq[Constant],
      markers: Option[Vector[PlaceMarker]]): Double = markers match {
    case None => (constantSeqA zip constantSeqB)
      .map { case (a, b) => constantDistance(a, b) }.sum / (2d * constantSeqA.length)
    case Some(x) => (constantSeqA zip constantSeqB zip x.map(_.numeric))
      .map { case ((a, b), isNumeric) => constantDistance(a, b, isNumeric) }.sum / (2d * constantSeqA.length)
  }

  /**
    * Distance for individual constants.
    *
    * @note If the given constants belong to some function return type, then the
    *       distance for their corresponding auxiliary constructs is measured.
    * @param xConstant a constant
    * @param yConstant another constant
    * @return a distance in the interval [0, 1] for the given constants. If constants are identical
    *         the distance is 0, else is 1.
    */
  @inline private def constantDistance(xConstant: Constant, yConstant: Constant, isNumeric: Boolean = false): Double =
    (auxConstructs.get(xConstant), auxConstructs.get(yConstant)) match {
      case (Some(functionA), Some(functionB)) => functionDistance(functionA, functionB)
      case _ if isNumeric                     => distance(xConstant.symbol.toDouble, yConstant.symbol.toDouble)
      case _                                  => if (xConstant == yConstant) 0 else 1
    }

  /**
    * Returns an evidence metric extended by the auxiliary predicates
    * of the given evidence database.
    *
    * @param evidence an evidence database
    * @return an extended evidence metric space
    */
  override def ++(evidence: Evidence): EvidenceMetric =
    new EvidenceMetric(modes, auxConstructs ++ collectAuxConstructs(evidence.db), matcher)
}

object EvidenceMetric {

  /**
    * @param matcher a matcher function
    * @return a evidence metric agnostic of any domain
    */
  def apply(matcher: Matcher): EvidenceMetric =
    new EvidenceMetric(Map.empty, Map.empty, matcher)

  /**
    * @param modes mode declarations
    * @param matcher a matcher function
    * @return an evidence metric based on the given MLN
    */
  def apply(modes: ModeDeclarations, matcher: Matcher): EvidenceMetric =
    new EvidenceMetric(modes, Map.empty, matcher)

  /**
    * @param evidence an evidence database
    * @param matcher a matcher function
    * @return an evidence metric based on the given evidence database
    */
  def apply(evidence: Evidence, matcher: Matcher): EvidenceMetric =
    new EvidenceMetric(Map.empty, collectAuxConstructs(evidence.db), matcher)

  /**
    * @param modes mode declarations
    * @param evidence an evidence database
    * @param matcher a matcher function
    * @return an evidence metric based on the given evidence database
    */
  def apply(modes: ModeDeclarations, evidence: Evidence, matcher: Matcher): EvidenceMetric =
    new EvidenceMetric(modes, collectAuxConstructs(evidence.db), matcher)
}
