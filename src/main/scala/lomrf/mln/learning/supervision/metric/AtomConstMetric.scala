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

import lomrf.logic._

/**
  * A atomic constant metric is a distance for atomic formulas that measures the
  * structural distance of atoms by ignoring the variables.
  *
  * @param matcher a matcher function
  */
case class AtomConstMetric(matcher: Matcher) extends StructureMetric[AtomicFormula] {

  /**
    * Distance for atoms. The function must obey to the following properties:
    *
    * {{{
    * 1. d(x, y) >= 0 for all x, y and d(x, y) = 0 if and only if x = y
    * 2. d(x, y) = d(y, x) for all x, y
    * 3. d(x, y) + d(y, z) >= d(x, z) for all x, y, z (triangle inequality)
    * }}}
    *
    * @param xAtom an atom
    * @param yAtom another atom
    * @return a distance for the given atoms
    */
  override def distance(xAtom: AtomicFormula, yAtom: AtomicFormula): Double =
    if (xAtom.signature != yAtom.signature) 1
    else if (xAtom.constants.isEmpty) 0
    else termSeqDistance(xAtom.terms.filter(_.isConstant), yAtom.terms.filter(_.isConstant))

  /**
    * Distance for term sequences.
    *
    * @param termSeqA a term sequence
    * @param termSeqB another term sequence
    * @return a distance in the interval [0, 1] for the given term sequences
    */
  @inline private def termSeqDistance(termSeqA: IndexedSeq[Term], termSeqB: IndexedSeq[Term]): Double =
    (termSeqA zip termSeqB).map { case (a, b) => termDistance(a, b) }.sum / (2d * termSeqA.count(_.isConstant))

  /**
    * Distance for individual terms.
    *
    * @note If the given term is a term function, then the distance for their
    *       corresponding term functions are measured.
    *
    * @param xTerm a term
    * @param yTerm another term
    * @return a distance in the interval [0, 1] for the given terms.
    */
  @inline private def termDistance(xTerm: Term, yTerm: Term): Double = (xTerm, yTerm) match {
    case (x: Constant, y: Constant) if x.symbol == y.symbol => 0
    case (_: Variable, _: Variable) => 0
    case (x: TermFunction, y: TermFunction) if x.signature == y.signature => termSeqDistance(x.terms, y.terms)
    case _ => 1
  }
}
