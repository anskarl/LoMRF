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

import java.text.DecimalFormat

/**
  * Represents a weighted CNF clause, that is a disjunction of literals.
  *
  * @param weight the weight of the clause
  * @param literals a set of literals, representing a disjunction of atoms or their negations.
  */
final class Clause(val weight: Double, val literals: Set[Literal]) extends Substitutable[Clause] {

  /**
    * The set of variables appearing inside the clause
    */
  lazy val variables: Set[Variable] = literals.foldRight(Set[Variable]())((a: Literal, b) => a.sentence.variables ++ b)

  /**
    * The set of constants appearing inside the clause
    */
  lazy val constants: Set[Constant] = literals.foldRight(Set[Constant]())((a: Literal, b) => a.sentence.constants ++ b)

  /**
    * The set of functions appearing inside the clause
    */
  lazy val functions: Set[TermFunction] = literals.foldRight(Set[TermFunction]())((a: Literal, b) => a.sentence.functions ++ b)

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
  def isEmpty: Boolean = literals.isEmpty

  /**
    * @return true if the clause contains only one literal, otherwise false.
    */
  def isUnit: Boolean = literals.size == 1

  /**
    * @return true if contains only one positive literal.
    */
  def isDefiniteClause: Boolean = literals.count(_.isPositive) == 1

  /**
    * @return the number of literals
    */
  def size: Int = literals.size

  /**
    * @param weighted set to false in order to omit the weight (default is true)
    * @return the textual representation of this clause
    */
  def toText(weighted: Boolean = true)(implicit numFormat: DecimalFormat = Clause.defaultNumFormat): String = {
    if (weighted && !weight.isNaN) {
      if (isHard) literals.map(_.toText).mkString(" v ") + "."
      else numFormat.format(weight) + " " + literals.map(_.toText).mkString(" v ")
    } else literals.map(_.toText).mkString(" v ")
  }

  /**
    * Substitutes all terms present in the literals of this clause
    * with other terms in the given mapping, if any mapping exists for these terms.
    *
    * @param theta a given mapping of logical terms
    * @return a substitution instance of the original expression type
    */
  override def substitute(theta: Theta): Clause = {
    Clause(literals.map(_.substitute(theta)), weight)
  }

  /**
    * Compares this clause to another object.
    *
    * @param that the object to compare
    * @return true if `that` is a clause that has the same weight,
    *         and the same set of literals, false otherwise
    */
  override def equals(that: Any): Boolean = that match {
    case x: Clause if x.weight.isNaN && weight.isNaN =>
      literals.size == x.literals.size && literals == x.literals
    case x: Clause =>
      weight == x.weight && literals.size == x.literals.size && literals == x.literals
    case _ => false
  }

  /**
    * A pair of clauses are similar, when:
    *
    * <ul>
    * <li> both have the same number of literals, and </li>
    * <li> for each literal in this clause, another literal exists in the other clause
    * having the same sense (positive or negated) and similar atomic formulas.
    * </li>
    * </ul>
    *
    * @example {{{
    *     Clause !HoldsAt(f,t1) v HoldsAt(f,t2) is similar to HoldsAt(f,t1) v !HoldsAt(f,t2),
    *     but not similar to !HoldsAt(f,t1) v !HoldsAt(f,t2)
    * }}}
    *
    * @param that the other clause for comparison
    *
    * @return true if this clause is similar to the given one, otherwise false
    */
  def =~=(that: Clause): Boolean = {
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
    } else false
  }

  override def hashCode: Int = hash

  override def toString: String = weight + " {" + literals.map(_.toString).mkString(" v ") + "}"

  private lazy val hash: Int = {
    var code = weight.hashCode
    for (literal <- literals) code ^= literal.hashCode
    code
  }
}

object Clause {

  private val defaultNumFormat = new DecimalFormat("0.############")

  /**
    * Constructs a weighted clause given a set of literals.
    *
    * @see [[lomrf.logic.Literal]]
    *
    * @param literals a set of literals
    * @param weight a weight for the clause (default is infinite)
    * @return a clause representing a disjunction over the given literals
    */
  def apply(literals: Set[Literal], weight: Double = Double.PositiveInfinity): Clause = new Clause(weight, literals)

  /**
    * Constructs a weighted unit clause given an atomic formula.
    *
    * @see [[lomrf.logic.AtomicFormula]]
    *
    * @param atom an atomic formula
    * @param weight a weight for the unit clause (default is infinite)
    * @param negated true for a positive unit clause, false otherwise
    * @return a unit clause having a single literal
    */
  def from(atom: AtomicFormula, weight: Double = Double.PositiveInfinity, negated: Boolean = false): Clause = {
    new Clause(weight, if (negated) Set(NegativeLiteral(atom)) else Set(PositiveLiteral(atom)))
  }

  /**
    * Constructs a weighted unit clause given a single literal.
    *
    * @see [[lomrf.logic.Literal]]
    *
    * @param literal a literal
    * @param weight a weight for the given unit clause (default is infinite)
    * @return a unit clause having a single literal
    */
  def unit(literal: Literal, weight: Double = Double.PositiveInfinity): Clause = new Clause(weight, Set(literal))
}

/**
  * A literal is either an atomic sentence or a negation of an atomic sentence.
  *
  * @example {{{
  *         Atom(x,y,z) or its negation !Atom(x,y,z)
  * }}}
  *
  * @param sentence an atomic formula (optionally ground)
  */
sealed abstract class Literal(val sentence: AtomicFormula) extends Substitutable[Literal] with TermIterable {

  /**
    * The number of sentence arguments.
    */
  lazy val arity: Int = sentence.arity

  /**
    * Is true if the sentence is ''not negated''
    */
  lazy val positive: Boolean = isPositive

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
  def negate: Literal = if (isPositive) NegativeLiteral(sentence) else PositiveLiteral(sentence)

  /**
    * Compares this literal to another object.
    *
    * @param obj the object to compare
    * @return true if `obj` is a literal that has the same atomic formula,
    *         and sense, false otherwise
    */
  override def equals(obj: Any): Boolean = obj match {
    case other: Literal => other.positive == this.positive && other.sentence == this.sentence
    case _              => false
  }

  /**
    * A pair of literals are similar, when:
    *
    * <ul>
    * <li> both have the same sense (positive or negative), and </li>
    * <li> both have similar underlying atomic formulas </li>
    * </ul>
    *
    * @example {{{
    *     !Friends(a,b) is similar to !Friends(b,c),
    *     but not similar to Friends(a,10)
    * }}}
    *
    * @param other another literal
    * @return true if the literals are similar, false otherwise
    */
  def =~=(other: Literal): Boolean = {
    this.positive == other.positive && this.sentence =~= other.sentence
  }

  /**
    * @return the textual representation of this literal.
    */
  def toText: String

  /**
    * @return an iterator over the terms inside the
    *         underlying atomic formula
    */
  override def iterator: Iterator[Term] = sentence.terms.iterator
}

object Literal {

  /**
    * Constructs a `Literal` given an atomic formula.
    *
    * @see [[lomrf.logic.AtomicFormula]]
    *
    * @param a an atomic formula
    * @param negative true if the literal is negated, false otherwise
    * @return a literal over the given atomic formula
    */
  def apply(a: AtomicFormula, negative: Boolean): Literal = {
    if (negative) NegativeLiteral(a) else PositiveLiteral(a)
  }

  /**
    * Constructs a `Negative Literal` given an atomic formula.
    *
    * @see [[lomrf.logic.AtomicFormula]]
    * @see [[lomrf.logic.NegativeLiteral]]
    *
    * @param a an atomic formula
    * @return a negative literal over the given atomic formula
    */
  def asNegative(a: AtomicFormula): Literal = NegativeLiteral(a)

  /**
    * Constructs a `Positive Literal` given an atomic formula.
    *
    * @see [[lomrf.logic.AtomicFormula]]
    * @see [[lomrf.logic.PositiveLiteral]]
    *
    * @param a an atomic formula
    * @return a positive literal over the given atomic formula
    */
  def asPositive(a: AtomicFormula): Literal = PositiveLiteral(a)
}

/**
  * Represents a positive literal (i.e., not negated).
  *
  * @param s an atomic formula
  */
case class PositiveLiteral(s: AtomicFormula) extends Literal(s) {

  /**
    * @inheritdoc
    * @return true if the sentence is ''not negated'', otherwise false.
    */
  def isPositive = true

  /**
    * @return the textual representation of this literal.
    */
  def toText: String = s.toText

  /**
    * @return a string representation for this positive literal,
    *         that essentially is the string representation of its
    *         underlying atomic formula
    */
  override def toString: String = s.toString()

  /**
    * Substitutes all terms present in the underlying atomic formula of
    * this positive literal with other terms in the given mapping, if any
    * mapping exists for these terms.
    *
    * @param theta a given mapping of logical terms
    * @return a substitution instance of the original expression type
    */
  override def substitute(theta: Theta): PositiveLiteral = PositiveLiteral(s.substitute(theta))
}

/**
  * Represents a negative literal (i.e., negated).
  *
  * @param s an atomic formula
  */
case class NegativeLiteral(s: AtomicFormula) extends Literal(s) {

  /**
    * @inheritdoc
    * @return true if the sentence is ''not negated'', otherwise false.
    */
  def isPositive = false

  /**
    * @return the textual representation of this literal.
    */
  def toText: String = "!" + s.toText

  /**
    * @return a string representation for this negative literal,
    *         that essentially is the string representation of its
    *         underlying atomic formula prefixed by '!'.
    */
  override def toString: String = "!" + s.toString

  /**
    * Substitutes all terms present in the underlying atomic formula of
    * this negative literal with other terms in the given mapping, if any
    * mapping exists for these terms.
    *
    * @param theta a given mapping of logical terms
    * @return a substitution instance of the original expression type
    */
  override def substitute(theta: Theta): NegativeLiteral = NegativeLiteral(s.substitute(theta))
}

/**
  * Literal ordering based on arity.
  */
final class LiteralArityOrdering extends Ordering[Literal] {

  override def compare(lit0: Literal, lit1: Literal): Int = {
    if (lit0 == lit1) 0
    else if (lit0.arity <= lit1.arity) -1
    else 1
  }

}

/**
  * Literal ordering based on the textual representation.
  */
final class LiteralTextOrdering extends Ordering[Literal] {

  override def compare(x: Literal, y: Literal): Int = x.toText.compare(y.toText)
}

/**
  * Literal ordering based on the underlying atomic formula.
  *
  * @see [[lomrf.logic.AtomicFormula]]
  */
final class LiteralSentenceOrdering extends Ordering[Literal] {

  override def compare(x: Literal, y: Literal): Int = x.sentence.toText.compare(y.sentence.toText)
}
