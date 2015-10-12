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

import java.text.DecimalFormat

/**
 * This class represents a weighted clause, which contains a disjunction of literal. A literal is an atom of its negation.
 *
 * @param weight the weight of this clause
 * @param literals a set of literals, representing a disjunction of atoms of their negations.
 */
final class Clause(val weight: Double, val literals: Set[Literal]) extends Substitutable[Clause]{

  /**
   * The set of variables that appear inside this clause
   */
  lazy val variables: Set[Variable] = literals.foldRight(Set[Variable]())((a:Literal, b) => a.sentence.variables ++ b)

  /**
   * The set of constants that appear inside this clause
   */
  lazy val constants: Set[Constant] = literals.foldRight(Set[Constant]())((a:Literal, b) => a.sentence.constants ++ b)

  /**
   * The set of functions that appear inside this clause
   */
  lazy val functions: Set[TermFunction] = literals.foldRight(Set[TermFunction]())((a:Literal, b) => a.sentence.functions ++ b)

  /**
   * @return true if this clause is hard-constrained (i.e. weight = Infinity), otherwise false.
   */
  def isHard: Boolean = weight.isInfinity

  /**
   * @return true if this clause does not contain any variable, otherwise false.
   */
  def isGround: Boolean = variables.isEmpty

  /**
   * @return true if the clause is empty, otherwise false.
   */
  def isEmpty = literals.isEmpty

  /**
   * @return true if the clause contains only one literal, otherwise false.
   */
  def isUnit = literals.size == 1

  /**
   * @return true if contains only one positive literal.
   */
  def isDefiniteClause = literals.count(_.isPositive) == 1

  /**
   * @return the number of literals
   */
  def size = literals.size

  def toDefiniteClause = {
    literals.filter(_.isPositive).toList match {
      case p1 :: Nil =>
        val negativeLiterals = literals.filter(_.isNegative)
        if (negativeLiterals.nonEmpty)
          new ImplicationDefiniteClause(negativeLiterals.map(_.sentence), p1.sentence)
        else p1.sentence
      case _ => throw new IllegalStateException("Not a definite clause.")
    }
  }

  def toText(weighted: Boolean = true)(implicit numFormat: DecimalFormat = Clause.defaultNumFormat): String = {
    if(weighted) {
      if(isHard) literals.map(_.toText).mkString(" v ")+"."
      else numFormat.format(weight)+" "+literals.map(_.toText).mkString(" v ")
    }
    else literals.map(_.toText).mkString(" v ")
  }

  override def substitute(theta: Theta): Clause = {
    Clause(literals.map(_.substitute(theta)), weight)
  }

  override def equals(that: Any) = {
    that match {
      case x: Clause => x.weight == this.weight && this.literals.size == x.literals.size  && x.literals == this.literals
      case _ => false
    }
  }

  /**
   * A pair of clauses are similar, when:
   * <ul>
   * <li> both have the same number of literals, and </li>
   * <li> for each literal in this clause, another literal exists in the other clause having the
   * same sense (positive or negated) and similar atomic formulas.
   * For example the clause !HoldsAt(f,t1) v HoldsAt(f,t2) is similar to HoldsAt(f,t1) v !HoldsAt(f,t2) but
   * is not similar to !HoldsAt(f,t1) v !HoldsAt(f,t2)
   * </li>
   * </ul>
   *
   * @param that the other clause for comparison
   *
   * @return true if this clause is similar to that one, otherwise false
   */
  def =~= (that: Clause): Boolean = {
    if (this.literals.size == that.literals.size) {
      var otherLiterals = that.literals
      this.literals.forall { lit1 =>
        otherLiterals.find(lit2 => lit1.positive == lit2.positive && lit1.sentence =~= lit2.sentence) match {
          case Some(matchedLiteral) =>
            otherLiterals -= matchedLiteral
            true
          case _ => false
        }
      }
    }
    else false
  }

  override def hashCode(): Int = hash

  override def toString: String = weight+" {" + literals.map(_.toString()).mkString(" v ") + "}"

  private lazy val hash: Int = {
    var code = weight.hashCode()
    for (literal <- literals) code ^= literal.hashCode()
    code
  }
}

object Clause {

  private val defaultNumFormat = new DecimalFormat("0.############")

  def apply(literals: Set[Literal], weight: Double = Double.PositiveInfinity): Clause = new Clause(weight, literals)

  def from(atom: AtomicFormula, weight: Double = Double.PositiveInfinity, negated: Boolean = false): Clause = {
    new Clause(weight, if(negated) Set(NegativeLiteral(atom)) else Set(PositiveLiteral(atom)))
  }

  def unit(literal: Literal, weight: Double = Double.PositiveInfinity): Clause = new Clause(weight, Set(literal))

}


//FOL Definite Clause
trait FOLDefiniteClause

class ImplicationDefiniteClause(val premise: Set[AtomicFormula],
                                val conclusion: AtomicFormula) extends FOLDefiniteClause {
  override def toString = s"((${premise.mkString(" ^ ")}) => ${conclusion.toString})"
}


/**
 * A literal is either an atomic sentence or a negation of an atomic sentence
 * @example
 * {{{
 *   Atom(x,y,z) or its negation !Atom(x,y,z)
 * }}}
 * @param sentence an atomic formula (optionally ground)
 */
sealed abstract class Literal(val sentence: AtomicFormula) extends Substitutable[Literal] with TermIterable{
  /**
   * The number of sentence arguments.
   */
  lazy val arity = sentence.arity

  /**
   * Is true if the sentence is ''not negated''
   */
  lazy val positive = isPositive

  /**
   * @return true if the sentence is ''not negated'', otherwise false.
   */
  def isPositive: Boolean

  /**
   * @return true if the sentence is ''negated'', otherwise false.
   */
  def isNegative: Boolean = !isPositive

  /**
   * @return the negations of this literal
   */
  def negate: Literal = if(isPositive) NegativeLiteral(sentence) else PositiveLiteral(sentence)

  override def equals(obj: Any): Boolean = obj match {
    case other: Literal => other.positive == this.positive && other.sentence == this.sentence
    case _ =>  false
  }


  def =~= (other: Literal): Boolean = {
    this.positive == other.positive && this.sentence =~= other.sentence
  }

  def toText: String

  override def iterator: Iterator[Term] = sentence.terms.iterator
}

object Literal {

  def apply(a: AtomicFormula, negative: Boolean): Literal = {
    if(negative) NegativeLiteral(a) else PositiveLiteral(a)
  }

  def asNegative(a: AtomicFormula): Literal = NegativeLiteral(a)

  def asPositive(a: AtomicFormula): Literal = PositiveLiteral(a)

}

/**
 * Represents a positive literal (i.e., not negated).
 *
 * @param s atomic formula
 */
case class PositiveLiteral(s: AtomicFormula) extends Literal(s){

  def isPositive = true

  def toText = s.toText

  override def toString() = s.toString

  override def substitute(theta: Theta): PositiveLiteral = PositiveLiteral(s.substitute(theta))

}

/**
 * Represents a negative literal (i.e., negated).
 *
 * @param s atomic formula
 */
case class NegativeLiteral(s: AtomicFormula) extends Literal(s) {

  def isPositive = false

  def toText = "!" + s.toText

  override def toString = "!" + s.toString

  override def substitute(theta: Theta): NegativeLiteral = NegativeLiteral(s.substitute(theta))
}

final class LiteralArityOrdering extends Ordering[Literal]{

  override def compare(lit0: Literal, lit1: Literal):Int = {
    if(lit0 == lit1) 0
    else if(lit0.arity <= lit1.arity) -1
    else 1
  }

}

final class LiteralTextOrdering extends Ordering[Literal]{

  override def compare(lit0: Literal, lit1: Literal): Int = {
    val (txt0, txt1) = (lit0.toText, lit1.toText)
    txt0.compare(txt1)
  }

}

final class LiteralSentenceOrdering extends Ordering[Literal]{

  override def compare(lit0: Literal, lit1: Literal): Int = {
    val (txt0, txt1) = (lit0.sentence.toText, lit1.sentence.toText)
    txt0.compare(txt1)
  }

}
