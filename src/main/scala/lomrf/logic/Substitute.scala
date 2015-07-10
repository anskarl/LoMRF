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

/**
 * A utility object for performing theta-substitution in MLN expressions.
 *
 * @example
 * {{{
 *  Happens(e, t) / {e = "Event_A"} = Happens(Event_A, t)
 * }}}
 *
 * @see Wikipedia article [[http://en.wikipedia.org/wiki/First-order_logic]]
 * @see Russell, S.J. and Norvig, P. and Canny, J.F. and Malik, J. and Edwards, D.D. Artificial Intelligence: A Modern Approach, chapter 8.3 Using First-Order Logic [[http://aima.cs.berkeley.edu/]]
 *
 *
 */
object Substitute {


  def apply[T](theta: Theta, alpha: T): T = {
    alpha match {
      case x: Term => substTerm(theta, x).asInstanceOf[T]
      case x: AtomicFormula => substAtomicFormula(theta, x).asInstanceOf[T]
      case x: Formula => substFormula(theta, x).asInstanceOf[T]
      case x: Clause => substClause(theta, x).asInstanceOf[T]
      case x: Literal => substLiteral(theta, x).asInstanceOf[T]
      case x: Set[_] => x.map(apply(theta, _)).asInstanceOf[T]
    }
  }

  /**
   * Theta substitution for a Term
   */
  @inline
  private def substTerm(theta: Theta, alpha: Term): Term = {
    if(theta.contains(alpha))
      theta(alpha)
    else alpha match {
      case x: TermFunction => TermFunction(x.symbol, x.terms.map(substTerm(theta, _)), x.domain)
      case _ => alpha
    }
  }

  /**
   * Theta substitution for an Atomic Formula
   */
  @inline
  private def substAtomicFormula(theta: Theta, alpha: AtomicFormula) = AtomicFormula(alpha.symbol, alpha.terms.map(substTerm(theta, _)))

  //@inline
  private def substFormula(theta: Theta, alpha: Formula): Formula = {
    alpha match {
      case x: WeightedFormula => WeightedFormula(x.weight, substFormula(theta, x.subFormulas.head))
      case x: AtomicFormula => substAtomicFormula(theta, x)
      case x: Not => Not(substFormula(theta, x.arg))
      case x: And => And(substFormula(theta, x.left), substFormula(theta, x.right))
      case x: Or => Or(substFormula(theta, x.left), substFormula(theta, x.right))
      case x: Implies => Implies(substFormula(theta, x.left), substFormula(theta, x.right))
      case x: Equivalence => Equivalence(substFormula(theta, x.left), substFormula(theta, x.right))
      case x: Quantifier => substQuantifier(theta, x)
      case _ => throw new IllegalStateException("Illegal formula type.")
    }
  }

  @inline
  private def substQuantifier(theta: Theta, alpha: Quantifier): Quantifier = {

    val newVar = theta.get(alpha.variable) match {
      case Some(x) if x.isInstanceOf[Variable] => x.asInstanceOf[Variable]
      case _ => alpha.variable
    }

    alpha match {
      case q: ExistentialQuantifier => ExistentialQuantifier(newVar, substFormula(theta, q.formula))
      case q: UniversalQuantifier => UniversalQuantifier(newVar, substFormula(theta, q.formula))
    }
  }

  /**
   * Theta substitution for a literal
   */
  private def substLiteral(theta: Theta, alpha: Literal): Literal = {
    alpha match {
      case x: PositiveLiteral => PositiveLiteral(substAtomicFormula(theta, x.sentence))
      case x: NegativeLiteral => NegativeLiteral(substAtomicFormula(theta, x.sentence))
    }
  }

  /**
   * Theta substitution for a clause
   */
  private def substClause(theta: Theta, alpha: Clause): Clause = new Clause(alpha.weight, alpha.literals.map(substLiteral(theta, _)).toSet)

}
