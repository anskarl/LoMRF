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

package lomrf.logic

import annotation.tailrec
import LogicOps._

/**
  * An object for applying the logical Unification operator to logical formulas. The Unification
  * operator searches for a mapping of terms (theta-substitution) in order to transform the
  * former expression into latter one.
  *
  * @example
  * {{{
  *   Unify Happens(x,t) with Happens(Event,t) should give Map((x->Event))
  *   Unify Happens(x,10) with Happens(Event,t) should give Map((x->Event), (t->10))
  * }}}
  *
  * @see Wikipedia article:
  *      [[http://en.wikipedia.org/wiki/Unification_(computing)#Definition_of_unification_for_first-order_logic]]
  * @see Russell, S.J., Norvig, P., Canny, J.F., Malik, J. and Edwards, D.D.
  *      Artificial Intelligence: A Modern Approach, Chapter 9.2.2 Unification
  *      [[http://aima.cs.berkeley.edu/]]
  */
object Unify {

  type ThetaOpt = Option[Theta]

  def apply[T](x: T, y: T): ThetaOpt = apply(x, y, Some(Map[Term, Term]()))

  def apply[T](x: T, y: T, theta: ThetaOpt): ThetaOpt = (x, y) match {
    case (a: Term, b: Term) => unifyTerm(a, b, theta)
    case (a: AtomicFormula, b: AtomicFormula) => unifyAtomicFormula(a, b, theta)
    case (va: Vector[Term @unchecked], vb: Vector[Term @unchecked]) => unifyTerms(va, vb, theta)
    case _ => None
  }

  def apply(x: AtomicFormula, f: FormulaConstruct): ThetaOpt = unifyFormula(x, f, Some(Map[Term, Term]()))

  private def unifyTerm(x: Term, y: Term, theta: ThetaOpt): ThetaOpt = theta match {
    case None => None // failure
    case _ =>
      if (x == y) theta
      else (x, y) match {
        case (v: Variable, _) => unifyVar(v, y, theta)
        case (_, v: Variable) => unifyVar(v, x, theta)
        case (a: TermFunction, b: TermFunction) =>
          if (a.symbol == b.symbol) unifyTerms(a.terms, b.terms, theta)
          else None
        case _ => None
      }
  }

  @tailrec
  private def unifyTerms(x: Vector[Term], y: Vector[Term], theta: ThetaOpt): ThetaOpt = theta match {
    case None => None // failure
    case Some(_) =>
      (x, y) match {
        case (aX +: restX, aY +: restY)   => unifyTerms(restX, restY, unifyTerm(aX, aY, theta))
        case (IndexedSeq(), IndexedSeq()) => theta
        case _                            => None
      }
  }

  @inline
  private def unifyAtomicFormula(x: AtomicFormula, y: AtomicFormula, theta: ThetaOpt): ThetaOpt = {
    if (x.signature == y.signature) unifyTerms(x.terms, y.terms, theta)
    else None
  }

  @inline
  private def unifyVar(v: Variable, x: Term, theta: ThetaOpt): ThetaOpt = theta match {
    case None                     => None // failure
    case Some(m) if m.contains(v) => apply(m(v), x, theta)
    case Some(m) => x match {
      case a: Variable if m.contains(a) => apply(v, m(a), theta)
      case f: TermFunction =>
        val groundFunction = f.substitute(m)
        if (groundFunction.variables.contains(v)) None // failure
        else Some(m + (v -> groundFunction))
      case _ => Some(m + (v -> x))
    }
  }

  @inline
  private def unifyFormula(srcAtom: AtomicFormula, src: FormulaConstruct, theta: ThetaOpt): ThetaOpt = src match {
    case atom: AtomicFormula => unifyAtomicFormula(srcAtom, atom, theta)
    case _ => src.first(srcAtom.signature) match {
      case Some(targetAtom) => apply(srcAtom, targetAtom, theta)
      case _                => None
    }
  }
}
