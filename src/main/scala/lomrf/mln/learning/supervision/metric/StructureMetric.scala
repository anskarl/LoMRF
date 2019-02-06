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

import lomrf.logic.{Constant, EvidenceAtom}
import lomrf.mln.learning.structure.PlaceMarker
import lomrf.mln.model.{Evidence, ModeDeclarations}

/**
  * A structural metric space is a measure of distance for logical interpretations.
  * Such a measure enables the calculation of distances based on the structural similarity
  * of atoms. Moreover, it can compute numerical distances over specific domains.
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
final class StructureMetric private (
    modes: ModeDeclarations,
    auxConstructs: Map[Constant, AuxConstruct],
    override protected val matcher: Matcher) extends Metric[EvidenceAtom] {

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
    * @see [[lomrf.logic.Constant]]
    * @param xConstant a constant
    * @param yConstant another constant
    * @return a distance in the interval [0, 1] for the given constants. If constants are identical
    *         the distance is 0, else is 1.
    */
  @inline private def constantDistance(xConstant: Constant, yConstant: Constant, isNumeric: Boolean = false): Double =
    (auxConstructs.get(xConstant), auxConstructs.get(yConstant)) match {
      case (Some(functionA), Some(functionB)) => functionDistance(functionA, functionB)
      case _ if isNumeric => distance(xConstant.symbol.toDouble, yConstant.symbol.toDouble)
      case _ => if (xConstant == yConstant) 0 else 1
    }

  /**
    * Returns a structure metric space stemming from the concatenation of this one and a given one.
    * that contains the auxiliary predicates for both metric spaces
    *
    * @param evidence an evidence database
    * @return an extended structure metric space
    */
  override def ++(evidence: Evidence): StructureMetric =
    new StructureMetric(modes, auxConstructs ++ collectAuxConstructs(evidence.db), matcher)
}

/**
  * Structure metric space object that enables the construction of metric spaces
  * either agnostic, based on a given an MLN or a predicate schema and an evidence database.
  */
object StructureMetric {

  /**
    * @return a structure metric agnostic of any domain
    */
  def apply(matcher: Matcher): StructureMetric =
    new StructureMetric(Map.empty, Map.empty, matcher)

  /**
    * @param modes a given MLN
    * @return a structure metric based on the given MLN
    */
  def apply(modes: ModeDeclarations, matcher: Matcher): StructureMetric =
    new StructureMetric(modes, Map.empty, matcher)

  /**
    *
    * @param evidence an evidence database
    * @return a structure metric based on the given evidence database
    */
  def apply(evidence: Evidence, matcher: Matcher): StructureMetric =
    new StructureMetric(Map.empty, collectAuxConstructs(evidence.db), matcher)

  def apply(modes: ModeDeclarations, evidence: Evidence, matcher: Matcher): StructureMetric =
    new StructureMetric(modes, collectAuxConstructs(evidence.db), matcher)
}
