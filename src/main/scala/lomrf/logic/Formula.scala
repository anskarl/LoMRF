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

import lomrf.mln.model.ConstantsSet


sealed trait Formula extends MLNExpression {

  // The collection of variables that appear inside this formula
  lazy val variables: Set[Variable] = subFormulas.foldRight(Set[Variable]())((f: Formula, rest) => f.variables ++ rest)

  // The collection of constants that appear inside this formula
  lazy val constants: Set[Constant] = subFormulas.foldRight(Set[Constant]())((f: Formula, rest) => f.constants ++ rest)

  // The collection of functions that appear inside this formula
  lazy val functions: Set[TermFunction] = subFormulas.foldRight(Set[TermFunction]())((f: Formula, rest) => f.functions ++ rest)

  /**
   * Gives the sub-formulas that this formula contains
   */
  def subFormulas: Seq[Formula] = Seq.empty[Formula]

  /**
   * Gives the quantifiers that appear inside this formula
   */
  def getQuantifiers: List[Quantifier] =
    subFormulas.foldRight(List[Quantifier]())((a, b) => a.getQuantifiers ::: b)

  /**
   * Gives the existential quantifiers that appear inside this formula
   */
  def getExistentialQuantifiers: List[ExistentialQuantifier] =
    subFormulas.foldRight(List[ExistentialQuantifier]())((a, b) => a.getExistentialQuantifiers ::: b)

  /**
   * Gives that collection of existential quantified variables that appear inside this formula
   */
  def getExistentialQuantifiedVariables: Set[Variable] = {
    getExistentialQuantifiers.foldLeft(Set[Variable]())((rest, quantifier) => rest + quantifier.v)
  }

  /**
   * Gives the universal quantifiers that appear inside this formula
   */
  def getUniversalQuantifiedVariables: Set[Variable] = {
    variables -- getExistentialQuantifiedVariables
  }

  /**
   * @return true if this formula contains at least one existential quantifier
   */
  def containsExistentialQuantifier: Boolean = {
    getQuantifiers.find(_.isInstanceOf[ExistentialQuantifier]) match {
      case Some(_) => true
      case _ => false
    }
  }

  /**
   * Gives the CNF clauses of this formula
   *
   * @param constants the domain of constants, required for existentially quantified variables
   * @return a set of clauses
   */
  def toCNF(implicit constants: Map[String, ConstantsSet]): Set[Clause] = NormalForm.toCNF(this)(constants)

  /**
   * The textual representation of this formula
   */
  def toText: String

  /**
   * @return the number of AtomicFormulas
   */
  def countAtoms: Int = subFormulas.foldRight(1)((current, rest) => current.countAtoms + rest)

}

trait DefiniteClauseConstruct extends Formula


case class DefiniteClause(head: AtomicFormula, body: DefiniteClauseConstruct) extends Formula {

  override lazy val variables: Set[Variable] = body.subFormulas.foldRight(head.variables)((a: Formula, b) => a.variables ++ b)

  override lazy val constants: Set[Constant] = body.subFormulas.foldRight(head.constants)((a: Formula, b) => a.constants ++ b)

  override lazy val functions: Set[TermFunction] = body.subFormulas.foldRight(head.functions)((a: Formula, b) => a.functions ++ b)

  override def subFormulas: Seq[Formula] = Seq(head, body)

  override def getQuantifiers = List[Quantifier]()

  override def getExistentialQuantifiers = List[ExistentialQuantifier]()

  override def toCNF(implicit constants: Map[String, ConstantsSet]): Set[Clause] = NormalForm.toCNF(Or(head, Not(body)))(constants)

  def toText = head.toText + " :- " + body.toText
}

/**
 * This class represents a weighted FOL formula, for example:
 * {{{
 * 1.386 A v B ^ C => D
 * }}}
 * A,B,F and D are FOL atoms and 1.386 is the corresponding weight.
 */
final class WeightedFormula private(val weight: Double, val formula: Formula) extends Formula {

  override def subFormulas: Seq[Formula] = Seq(formula)

  override def toText: String = weight match {
    case Double.PositiveInfinity => formula.toText + "."
    case x if x.isNaN => formula.toText
    case _ => weight.toString + " " + formula.toText
  }

  override def toString: String = weight.toString + " " + formula.toString

  override def hashCode() = weight.hashCode() ^ formula.hashCode()

  override def equals(obj: Any) = obj match {
    case WeightedFormula(w, f) if w == weight && f == formula => true
    case _ => false
  }
}

object WeightedFormula {

  def asUnit(formula: Formula) = WeightedFormula(1.0, formula)

  def apply(weight: Double, formula: Formula): WeightedFormula = new WeightedFormula(weight, formula)


  def unapply(obj: WeightedFormula): Option[(Double, Formula)] = {
    if (obj ne null) Some(obj.weight, obj.formula) else None
  }
}


final class WeightedDefiniteClause private(val weight: Double, val clause: DefiniteClause) extends Formula {

  override def subFormulas: Seq[Formula] = clause.subFormulas

  override def toText: String = weight match {
    case Double.PositiveInfinity => clause.toText + "."
    case x if x.isNaN => clause.toText
    case _ => weight + " " + clause.toText
  }

  override def toString: String = weight.toString + " " + clause.toString

  override def hashCode() = weight.hashCode() ^ clause.hashCode()

  override def equals(obj: Any) = obj match {
    case WeightedDefiniteClause(w, c) if w == weight && c == clause => true
    case _ => false
  }
}

object WeightedDefiniteClause {

  def apply(weight: Double, clause: DefiniteClause): WeightedDefiniteClause = {
    new WeightedDefiniteClause(weight, clause)
  }

  def unapply(obj: WeightedDefiniteClause): Option[(Double, DefiniteClause)] = {
    if (obj ne null) Some(obj.weight, obj.clause) else None
  }
}

/**
 * This class represents an atomic-formula (or atom) in FOL, for example:
 * {{{
 * Friends(x, Anna)
 * Friends(x, y)
 * Friends(Bob, Anna)
 * }}}
 * The symbol ''Friends'' is the name of the atomic formula.
 * x,y are variables and ''Bob'' and ''Anna'' are constants.
 *
 * An atomic formula in its arguments may have either constants or variables.
 */
case class AtomicFormula(symbol: String, terms: Vector[Term]) extends DefiniteClauseConstruct {

  val isDynamic = false

  val arity = terms.size

  lazy val signature = AtomSignature(symbol, terms.size)

  override def countAtoms = 1

  /**
   * All variables of this atom
   */
  override lazy val variables: Set[Variable] = uniqueVariablesIn(terms)

  /**
   * All constants of this atom
   */
  override lazy val constants: Set[Constant] = uniqueConstantsIn(terms)

  /**
   * All functions of this atom
   */
  override lazy val functions: Set[TermFunction] = uniqueFunctionsIn(terms)

  def isGround = variables.isEmpty

  override def toText: String = s"$symbol(${terms.map(_.toText).mkString(",")})"

  override def toString: String = s"$symbol(${terms.map(_.toString).mkString(",")})"

  /**
   * Two atoms are similar, when:
   * <ul>
   * <li> both have the same signature, i.e. Name/Arity, and </li>
   * <li> the most general unifier (MGU) gives a result (i.e. unification is possible), in which all mapped entries are variables.
   * For example, the predicates Happens(x,t) and Happens(y,t) are similar, as the MGU gives Map(x->y)
   * and thus the only difference is the name of the variable in the first argument.
   * However, the predicates Happens(x,t) and Happens(A,t) are not similar, as the MGU gives Map(x->A).
   * Additionally, the predicates Happens(A,t) and Happens(B,t) are not similar, as the MGU cannot give any results, i.e. the constants A and B cannot unified.
   * </li>
   * </ul>
   *
   * @param other the atom to compare
   *
   * @return true if this atom is similar to the given atom, otherwise false
   */
  def =~= (other: AtomicFormula): Boolean = {
    if (signature == other.signature) {
      Unify(this, other) match {
        case Some(x) if x.forall(_._2.isInstanceOf[Variable]) => true
        case _ => false
      }
    } else false
  }
}


/**
 * This class represents a ground atom (all its terms are constants) and its truth value may be known (given by the input evidence).
 * Consider, for example, the following atoms:
 *
 * <ul>
 * <li> '!Friends(A,B)' indicates that A and B are not friends, that is Friends(A,B) = False </li>
 * <li> 'Friends(D,F)'  indicates that D and F are friends, that is Friends(D,F) = True </li>
 * <li> 'Friends(D,F) 0.76' indicates that D and F are friends with probability 0.76, that is P(Friends(D,F) = True) = 0.76  </li>
 * <li> '!Friends(D,F) 0.76' indicates that D and F are friends with probability 0.76, that is P(Friends(D,F) = True) = 0.24  </li>
 * </ul>
 *
 * An evidence atom is an atomic formula with a known truth value.
 */
class EvidenceAtom(override val symbol: String, override val terms: Vector[Constant],
                   val state: TriState, val probability: Double = Double.NaN) extends AtomicFormula(symbol, terms) with EvidenceExpression {


  override lazy val variables: Set[Variable] = Set.empty[Variable]

  override lazy val constants: Set[Constant] = terms.toSet[Constant]

  override lazy val functions: Set[TermFunction] = Set.empty[TermFunction]

  override def isGround = true


  override def toText: String = {
    lazy val sentence = s"$symbol(${terms.map(_.toText).mkString(",")})"

    state match {
      case TRUE => sentence
      case FALSE => s"!$sentence"
      case UNKNOWN => s"$sentence $probability"
    }
  }

}

object EvidenceAtom {

  def asTrue(predicate: String, args: Vector[Constant]): EvidenceAtom = new EvidenceAtom(predicate, args, TRUE)

  def asFalse(predicate: String, args: Vector[Constant]): EvidenceAtom = new EvidenceAtom(predicate, args, FALSE)

  def asUnknown(predicate: String, args: Vector[Constant]): EvidenceAtom = new EvidenceAtom(predicate, args, UNKNOWN)

  def apply(predicate: String, args: Vector[Constant], state: TriState): EvidenceAtom = new EvidenceAtom(predicate, args, state)

  def apply(predicate: String, args: Vector[Constant], isPositive: Boolean): EvidenceAtom = apply(predicate, args, if (isPositive) TRUE else FALSE)

  def apply(predicate: String, args: Vector[Constant], probability: Double): EvidenceAtom = {
    require(probability <= 1.0 && probability >= 0, "The specified probability value is not in [0, 1].")

    if (probability == 0.0) apply(predicate, args, FALSE)
    else if (probability == 1.0) apply(predicate, args, TRUE)
    else new EvidenceAtom(predicate, args, UNKNOWN, probability)
  }

  def unapply(obj: EvidenceAtom): Option[(String, Vector[Constant], TriState, Double)] = {
    if (obj != null) Some((obj.symbol, obj.terms, obj.state, obj.probability)) else None
  }
}

class FunctionMapping(val retValue: String, val functionSymbol: String, val values: Vector[String]) extends EvidenceExpression {

  lazy val signature = AtomSignature(functionSymbol, values.size)

  override def toString = s"$retValue = $functionSymbol(${values.mkString(",")})"

}

object FunctionMapping {
  def apply(retValue: String, functionSymbol: String, values: Vector[Constant]) = new FunctionMapping(retValue, functionSymbol, values.map(_.toString))
}

trait LogicalConnective extends Formula {
  def isUnit: Boolean
}


/**
 * Negation of a formula (! { Formula } )
 */
case class Not(arg: Formula) extends LogicalConnective with DefiniteClauseConstruct {

  private lazy val _isUnit: Boolean = arg.isInstanceOf[AtomicFormula]

  override def subFormulas: Seq[Formula] = Seq(arg)

  override def toText = if (_isUnit) s"!${arg.toText}" else s"!(${arg.toText})"

  def isUnit: Boolean = _isUnit
}

/**
 * Logical AND of two formulas ( { Formula1 } &#094; { Formula2 } ).
 */
case class And(left: Formula, right: Formula) extends LogicalConnective with DefiniteClauseConstruct {

  override def subFormulas: Seq[Formula] = Seq(left, right)

  override def toText: String = {
    subFormulas map {
      case currFormula @ (f: Or) => "(" + currFormula.toText + ")"
      case f: ConditionalStatement => "(" + right.toText + ")"
      case currFormula => currFormula.toText
    } mkString " ^ "
  }

  def isUnit = false
}

/**
 * Logical OR of two formulas ( { Formula1 } v { Formula2 } ).
 */
case class Or(left: Formula, right: Formula) extends LogicalConnective {

  override def subFormulas: Seq[Formula] = Seq(left, right)

  override def toText: String = {
    subFormulas map {
      case currFormula @ (f: And) => "(" + currFormula.toText + ")"
      case f: ConditionalStatement => "(" + right.toText + ")"
      case currFormula => currFormula.toText
    } mkString " v "
  }

  def isUnit = false
}

trait ConditionalStatement extends LogicalConnective {
  def isUnit = false
}


/**
 * An implication between two formulas ( { Formula1 } => { Formula2 } ).
 */
case class Implies(left: Formula, right: Formula) extends ConditionalStatement {
  override def subFormulas: Seq[Formula] = Seq(left, right)

  override def toText = left.toText + " => " + right.toText

  override def toString = "Implies(" + left.toString + "," + right.toString + ")"
}

/**
 * An equivalence between two formulas ( { Formula1 } <=> { Formula2 } ).
 */
case class Equivalence(left: Formula, right: Formula) extends ConditionalStatement {

  override def subFormulas: Seq[Formula] = Seq(left, right)

  override def toText = left.toText + " <=> " + right.toText

}

sealed abstract class Quantifier(v: Variable, f: Formula) extends Formula {

  val variable = v

  val formula = f

  override def subFormulas: Seq[Formula] = Seq(formula)

  override def getQuantifiers = this :: formula.getQuantifiers

}

case class UniversalQuantifier(v: Variable, f: Formula) extends Quantifier(v, f) {

  override def toText: String = "(Forall " + v.toText + " " + formula.toText + ")"

}

case class ExistentialQuantifier(v: Variable, f: Formula) extends Quantifier(v, f) {

  override def toText: String = "(Exist " + v.toText + " " + formula.toText + ")"

  override def getExistentialQuantifiers = this :: formula.getExistentialQuantifiers
}
