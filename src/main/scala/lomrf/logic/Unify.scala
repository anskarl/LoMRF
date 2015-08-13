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
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */

package lomrf.logic

import annotation.tailrec

/**
 * A utility object for applying the Unification operator between MLN expressions.
 * {{{
 *   Unify Happens(x,t) with Happens(Event,t) = Map((x->Event))
 *   Unify Happens(x,10) with Happens(Event,t) =  Map((x->Event), (t->10))
 * }}}
 *
 * @see Wikipedia article [[http://en.wikipedia.org/wiki/Unification_(computing)#Definition_of_unification_for_first-order_logic]]
 * @see Russell, S.J. and Norvig, P. and Canny, J.F. and Malik, J. and Edwards, D.D. Artificial Intelligence: A Modern Approach, chapter 9.2.2 Unification [[http://aima.cs.berkeley.edu/]]
 *
 *
 */
object Unify {

  type ThetaOpt = Option[Theta]

  def apply[T](x: T, y: T): ThetaOpt = apply(x, y, Some(Map[Term, Term]()))

  def apply[T](x: T, y: T, theta: ThetaOpt)(implicit m: Manifest[Term]): ThetaOpt = x match{
      case p: Term => unifyTerm(p, y.asInstanceOf[Term], theta)
      case f: AtomicFormula => unifyAtomicFormula(f, y.asInstanceOf[AtomicFormula], theta)
      case l: Vector[Term] => unifyTerms(l, y.asInstanceOf[Vector[Term]], theta)
  }

  def apply(x: AtomicFormula, f: FormulaConstruct): ThetaOpt = unifyFormula(x,f, Some(Map[Term, Term]()))

  //@inline
  private def unifyTerm(x: Term, y: Term, theta: ThetaOpt): ThetaOpt = theta match {
      case None => None //failure
      case _ =>
        if(x == y) theta
        else (x,y) match{
          case (v: Variable, _) => unifyVar(v,y,theta)
          case (_, v: Variable) => unifyVar(v,x,theta)
          case (a: TermFunction, b:TermFunction) =>
            if(a.symbol == b.symbol) unifyTerms(a.terms, b.terms, theta)
            else None
          case _ => None
        }
  }

  @tailrec
  private def unifyTerms(x: Vector[Term], y: Vector[Term], theta: ThetaOpt): ThetaOpt = theta match {
    case None => None //failure
    case Some(m) =>
      (x,y) match {
        case (aX +: restX, aY +: restY) => unifyTerms(restX, restY, unifyTerm(aX, aY, theta))
        case (IndexedSeq(), IndexedSeq()) => theta
        case _ => None
      }
  }

  @inline
  private def unifyAtomicFormula(x: AtomicFormula, y: AtomicFormula, theta: ThetaOpt): ThetaOpt ={
    if(x.signature == y.signature) unifyTerms(x.terms, y.terms, theta)
    else None
  }

  @inline
  private def unifyVar(v: Variable, x:Term, theta: ThetaOpt): ThetaOpt = theta match {
    case None => None // failure
    case Some(m) if m.contains(v) => apply(m(v),x, theta)
    case Some(m) => x match {
      case a: Variable if m.contains(a) => apply(v, m(a), theta)
      case f: TermFunction =>
        val groundFunction = Substitute(m, f)
        if(groundFunction.variables.contains(v)) None //failure
        else Some(m + (v -> groundFunction))
      case _ => Some(m + (v -> x))
    }
  }

  @inline
  private def unifyFormula(srcAtom: AtomicFormula, src: FormulaConstruct, theta: ThetaOpt): ThetaOpt = src match {
    case atom: AtomicFormula => unifyAtomicFormula(srcAtom, atom, theta)
    case _ => fetchAtom(srcAtom.signature, src) match {
      case Some(targetAtom) => apply(srcAtom, targetAtom, theta)
      case _ => None
    }
  }

}
