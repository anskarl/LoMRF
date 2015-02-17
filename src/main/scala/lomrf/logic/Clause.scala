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

package lomrf.logic

/**
 * This class represents a weighted clause, which contains a disjunction of literal. A literal is an atom of its negation.
 *
 * @param weight the weight of this clause
 * @param literals a set of literals, representing a disjunction of atoms of their negations.
 *
 * @author Anastasios Skarlatidis
 */
final class Clause(val weight: Double, val literals: Set[Literal]){

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
  def isHard: Boolean = weight == Double.PositiveInfinity

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
   *
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


  override def equals(that: Any) = {
    that match {
      case x: Clause => x.weight == this.weight && this.literals.size == x.literals.size  && x.literals == this.literals
      case _ => false
    }
  }

  override def hashCode(): Int = hash

  override def toString: String = weight+" {" + literals.map(_.toString).reduceLeft(_ + " v " + _) + "}"

  private lazy val hash: Int = {
    var code = weight.hashCode()
    for (literal <- literals) code ^= literal.hashCode()
    code
  }
}

object Clause {
  def apply(weight: Double, literals: Set[Literal]):Clause = new Clause(weight,literals)
  def apply(weight: Double, atom: AtomicFormula, negated: Boolean = false) = new Clause(weight, if(negated) Set(NegativeLiteral(atom)) else Set(PositiveLiteral(atom)))
}


//FOL Definite Clause
trait FOLDefiniteClause

//AtomicSentence is non-implication FOL Definite clause and here is the
//implication one
class ImplicationDefiniteClause(val premise: Set[AtomicFormula],
                                val conclusion: AtomicFormula) extends FOLDefiniteClause {
  override def toString =
    "((" + premise.map(_.toString).reduceLeft(_ + " ^ " + _) + ") => " + conclusion.toString + ")"
}


/**
 * A literal is either an atomic sentence or a negation of an atomic sentence
 * @example
 * {{{
 *   Atom(x,y,z) or its negation !Atom(x,y,z)
 * }}}
 * @param sentence an atomic formula (optionally ground)
 */
sealed abstract class Literal(val sentence: AtomicFormula) {
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

  override def equals(obj: Any) = {
    obj match{
      case other:Literal => other.positive == this.positive && other.sentence == this.sentence
      case _ =>  false
    }
  }

  def toText: String
}

/**
 * Represents a positive literal (i.e. not negated).
 *
 * @param s atomic formula
 */
case class PositiveLiteral(s: AtomicFormula) extends Literal(s){
  def isPositive = true


  def toText = s.toText

  override def toString = s.toString



}

/**
 * Represents a negative literal (i.e. negated).
 *
 * @param s atomic formula
 */
case class NegativeLiteral(s: AtomicFormula) extends Literal(s){
  def isPositive = false


  def toText = "!"+s.toText

  override def toString = "!" + s.toString
}

final class LiteralArityOrdering extends Ordering[Literal]{
  override def compare(lit0: Literal, lit1: Literal):Int = {
    if(lit0 == lit1) 0
    else if(lit0.arity <= lit1.arity) -1
    else 1
  }

}