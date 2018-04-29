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
import lomrf.mln.model.{ EvidenceDB, MLN, PredicateSchema }

/**
  * A structural metric space is a measure of distance for herbrand interpretations. Such a measure
  * enables the calculation of distances based on the structural similarity of atoms. Moreover, it can
  * be extended given a numerical function for calculating numerical distances over specific domains.
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
  * @param predicateSchema predicate schema
  * @param auxConstructs a map from return constants to auxiliary constructs
  * @param numericDistance a numerical distance (optional)
  * @param numericDomains a set of numerical domains (optional)
  */
final class StructureMetric private (
    predicateSchema: PredicateSchema,
    auxConstructs: Map[Constant, AuxConstruct],
    numericDistance: Option[(Double, Double) => Double] = None,
    numericDomains: Option[Set[String]] = None) extends Metric {

  /**
    * Distance for ground evidence atoms. The function must obey to the following properties:
    *
    * {{{
    * 1. d(x, y) >= 0 for all x, y and d(x, y) = 0 if and only if x = y
    * 2. d(x, y) = d(y, x) for all x, y
    * 3. d(x, y) + d(y, z) >= d(x, z) for all x, y, z (triangle inequality)
    * }}}
    *
    * @see [[lomrf.logic.EvidenceAtom]]
    * @param xAtom an evidence atom
    * @param yAtom another evidence atom
    * @return a distance for the given evidence atoms
    */
  override def distance(xAtom: EvidenceAtom, yAtom: EvidenceAtom): Double =
    if (xAtom.state != yAtom.state || xAtom.symbol != yAtom.symbol) 1D
    else distance(xAtom.terms, yAtom.terms, predicateSchema.get(xAtom.signature))

  /**
    * Distance for auxiliary predicates.
    *
    * @see [[lomrf.mln.learning.supervision.metric.AuxConstruct]]
    * @param xConstruct an auxiliary predicate
    * @param yConstruct another auxiliary predicate
    * @return a distance in the interval [0, 1] for the given auxiliary predicates.
    */
  @inline private def distance(xConstruct: AuxConstruct, yConstruct: AuxConstruct): Double =
    if (xConstruct.signature != yConstruct.signature) 1D
    else predicateSchema.get(xConstruct.signature) match {
      case Some(domains) =>
        distance(xConstruct.constants, yConstruct.constants, Some(domains.tail))
      case None =>
        distance(xConstruct.constants, yConstruct.constants, None)
    }

  /**
    * Distance for constant sequences.
    *
    * @param constantSeqA a constant sequence
    * @param constantSeqB another constant sequence
    * @return a distance in the interval [0, 1] for the given constant sequences
    */
  @inline private def distance(
      constantSeqA: IndexedSeq[Constant],
      constantSeqB: IndexedSeq[Constant],
      domains: Option[Seq[String]]): Double = domains match {
    case None => (constantSeqA zip constantSeqB)
      .map { case (a, b) => distance(a, b) }.sum / (2d * constantSeqA.length)

    case Some(domainSeq) =>
      (constantSeqA zip constantSeqB zip domainSeq.map(numericDomains.getOrElse(Set.empty).contains))
        .map { case ((a, b), isNumeric) => distance(a, b, isNumeric) }.sum / (2d * constantSeqA.length)
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
  @inline private def distance(xConstant: Constant, yConstant: Constant, isNumeric: Boolean = false): Double =
    (auxConstructs.get(xConstant), auxConstructs.get(yConstant)) match {
      case (Some(functionA), Some(functionB)) => distance(functionA, functionB)
      case _ if numericDistance.isDefined && isNumeric && xConstant.symbol.matches("-?\\d+") =>
        numericDistance.get(xConstant.symbol.toDouble, yConstant.symbol.toDouble)
      case _ => if (xConstant == yConstant) 0.0 else 1.0
    }

  /**
    * Returns a structure metric space stemming from the concatenation of this one and a given one.
    *
    * @param mln an MLN
    * @return an extended metric space
    */
  def ++(mln: MLN): StructureMetric = ++(mln.evidence.db)

  /**
    * that contains the auxiliary predicates for both metric spaces
    *
    * @param evidenceDB an evidence database
    * @return an extended structure metric space
    */
  def ++(evidenceDB: EvidenceDB): StructureMetric =
    new StructureMetric(
      predicateSchema,
      auxConstructs ++ collectAuxConstructs(evidenceDB),
      numericDistance,
      numericDomains)

  /**
    *
    * @param distance a numerical distance function
    * @param domains a set of numerical domains
    * @return an extended structure metric that
    */
  def makeNumeric(distance: (Double, Double) => Double, domains: Set[String]): StructureMetric =
    new StructureMetric(predicateSchema, auxConstructs, Some(distance), Some(domains))
}

/**
  * Structure metric space object that enables the construction of metric spaces
  * either agnostic, based on a given an MLN or a predicate schema and an evidence database.
  */
object StructureMetric {

  /**
    * @return a structure metric agnostic of any domain
    */
  def apply(): StructureMetric =
    new StructureMetric(Map.empty, Map.empty)

  /**
    * @param mln a given MLN
    * @return a structure metric based on the given MLN
    */
  def apply(mln: MLN): StructureMetric =
    apply(mln.schema.predicates, mln.evidence.db)

  /**
    *
    * @param evidenceDB an evidence database
    * @return a structure metric based on the given evidence database
    */
  def apply(evidenceDB: EvidenceDB): StructureMetric =
    apply(Map.empty, evidenceDB)

  /**
    * @param predicateSchema a predicate schema
    * @param evidenceDB an evidence database
    * @return a structure metric based on the given predicate schema and evidence database
    */
  def apply(predicateSchema: PredicateSchema, evidenceDB: EvidenceDB): StructureMetric =
    new StructureMetric(predicateSchema, collectAuxConstructs(evidenceDB))
}
