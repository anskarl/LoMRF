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

import lomrf.logic.LogicOps._
import lomrf.mln.model.ConstantsSet

/**
  * Represents any ''First-Order Logic'' formula.
  *
  * @see [[lomrf.logic.WeightedFormula]]
  *
  * @example {{{
  *           // All Pompeians are Roman (hard-constrained)
  *           Pompeian(x) => Roman(x).
  *
  *           // All Romans were either loyal to Caesar or hated him or both (hard-constrained)
  *           Roman(x) => Loyal(x, Caesar) v Hate(x, Caesar).
  *
  *           // Usually, everyone is loyal to someone (soft-constrained)
  *           1 Exist y Loyal(x,y)
  *
  *           // People may try to assassinate rulers to whom they are not loyal (soft-constrained)
  *           2 People(x) ^ Ruler(y) ^ Assassinate(x,y) => !Loyal(x, y)
  *
  *           // Usually, nobody hates himself (soft-constrained)
  *           1 !Hate(x, x)
  *          }}}
  */
sealed trait Formula extends MLNExpression with Substitutable[Formula] with Serializable {

  /**
    * The collection of variables that appear inside this formula
    */
  def variables: Set[Variable] = subFormulas.foldRight(Set[Variable]())((f: FormulaConstruct, rest) => f.variables ++ rest)

  /**
    * The collection of constants that appear inside this formula
    */
  @transient lazy val constants: Set[Constant] = subFormulas.foldRight(Set[Constant]())((f: FormulaConstruct, rest) => f.constants ++ rest)

  /**
    * The collection of functions that appear inside this formula
    */
  @transient lazy val functions: Set[TermFunction] = subFormulas.foldRight(Set[TermFunction]())((f: FormulaConstruct, rest) => f.functions ++ rest)

  /**
    * @return the sub-formulas that this formula contains
    */
  def subFormulas: Seq[FormulaConstruct] = Seq.empty[FormulaConstruct]

  /**
    * @return the quantifiers that appear inside this formula
    */
  def getQuantifiers: List[Quantifier] =
    subFormulas.foldRight(List[Quantifier]())((a, b) => a.getQuantifiers ::: b)

  /**
    * @return the existential quantifiers that appear inside this formula
    */
  def getExistentialQuantifiers: List[ExistentialQuantifier] =
    subFormulas.foldRight(List[ExistentialQuantifier]())((a, b) => a.getExistentialQuantifiers ::: b)

  /**
    * @return the collection of existential quantified variables that appear inside this formula
    */
  def getExistentialQuantifiedVariables: Set[Variable] = {
    getExistentialQuantifiers.foldLeft(Set[Variable]())((rest, quantifier) => rest + quantifier.v)
  }

  /**
    * @return the universal quantifiers that appear inside this formula
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
      case _       => false
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
    * @return the textual representation of this formula
    */
  def toText: String

  /**
    * @return the number of AtomicFormulas
    */
  def countAtoms: Int = subFormulas.foldRight(1)((current, rest) => current.countAtoms + rest)
}

sealed trait FormulaConstruct extends Formula {

  /**
    * A formula is unit if and only if it consists of a single atom.
    *
    * @return true if the construct is a unit formula, false otherwise
    */
  def isUnit: Boolean

  /**
    * @inheritdoc
    * @param theta a given mapping of logical terms
    * @return a substitution instance of the original expression type
    */
  override def substitute(theta: Theta): FormulaConstruct

  /**
    * All free variables in the FormulaConstruct that are not appearing in the
    * given target atom, will be existentially quantified.
    *
    * @param target the target atom
    * @return the resulting FormulaConstruct, which may be existentially quantified over some variables
    */
  def boundVarsNotIn(target: AtomicFormula): FormulaConstruct = {

    //A set of existentially quantified variables in the body
    val exQVars = this.getQuantifiers.filter(_.isInstanceOf[ExistentialQuantifier]).map(_.variable).toSet

    // Find which free variables appear in the body, but not in the head of the clause
    val diff = this.variables -- exQVars -- target.variables

    // If the variables that appear in the body are the same with the variables in the head,
    // then keep the body as it is. Otherwise, define them as existentially quantified.
    if (diff.isEmpty) this
    else diff.foldRight(this)((v, f) => ExistentialQuantifier(v, f))

  }
}

/**
  * A definite clause construct represents is any construct allowed
  * in the body of a definite clause formula.
  *
  * @see [[lomrf.logic.DefiniteClause]]
  */
sealed trait DefiniteClauseConstruct extends FormulaConstruct {
  override def substitute(theta: Theta): DefiniteClauseConstruct
}

/**
  * A conditional statement construct represents any construct that
  * has a condition and a result for the condition.
  *
  * @see [[lomrf.logic.Implies]]
  *      [[lomrf.logic.Equivalence]]
  */
sealed trait ConditionalStatement extends FormulaConstruct {
  override def isUnit = false
  override def substitute(theta: Theta): ConditionalStatement
}

/**
  * ''Not'' represents the negation of a formula (! { Formula } ).
  *
  * @example {{{
  *           !(Friends(x, Bob) ^ Friends(x, Alice))
  * }}}
  *
  * @param arg the formula construct to be negated
  */
final case class Not(arg: FormulaConstruct) extends DefiniteClauseConstruct with FormulaConstruct {

  private val _isUnit: Boolean = arg.isInstanceOf[AtomicFormula]

  override def subFormulas: Seq[FormulaConstruct] = Seq(arg)

  override def toText: String = if (_isUnit) s"!${arg.toText}" else s"!(${arg.toText})"

  def isUnit: Boolean = _isUnit

  override def substitute(theta: Theta): Not = Not(arg.substitute(theta))
}

/**
  * Represents the logical ''AND'' of two formulas ( { Formula1 } &#094; { Formula2 } ).
  *
  * @example {{{
  *         Happens(x, t) ^ Happens(y, t+1)
  * }}}
  *
  * @param left the left formula participating in the logical connective
  * @param right the right formula participating in the logical connective
  */
final case class And(left: FormulaConstruct, right: FormulaConstruct) extends DefiniteClauseConstruct with FormulaConstruct {

  override def subFormulas: Seq[FormulaConstruct] = Seq(left, right)

  override def toText: String = {
    subFormulas map {
      case currFormula: Or         => "(" + currFormula.toText + ")"
      case _: ConditionalStatement => "(" + right.toText + ")"
      case currFormula             => currFormula.toText
    } mkString " ^ "
  }

  def isUnit = false

  override def substitute(theta: Theta): And = {
    And(left.substitute(theta), right.substitute(theta))
  }
}

/**
  * Represents the logical ''OR'' of two formulas ( { Formula1 } v { Formula2 } ).
  *
  * @example {{{
  *          Happens(x, t) v Happens(y, t)
  * }}}
  *
  * @param left the left formula participating in the logical connective
  * @param right the right formula participating in the logical connective
  */
final case class Or(left: FormulaConstruct, right: FormulaConstruct) extends FormulaConstruct {

  override def subFormulas: Seq[FormulaConstruct] = Seq(left, right)

  override def toText: String = {
    subFormulas map {
      case currFormula: And        => "(" + currFormula.toText + ")"
      case _: ConditionalStatement => "(" + right.toText + ")"
      case currFormula             => currFormula.toText
    } mkString " v "
  }

  def isUnit = false

  override def substitute(theta: Theta): Or = {
    Or(left.substitute(theta), right.substitute(theta))
  }
}

/**
  * Represents the logical implication between two formulas ( { Formula1 } => { Formula2 } ).
  *
  * @example {{{
  *           King(x) ^ Greedy(x) => Evil(x)
  * }}}
  *
  * @param left the left formula participating in the conditional statement
  * @param right the right formula participating in the conditional statement
  */
final case class Implies(left: FormulaConstruct, right: FormulaConstruct) extends ConditionalStatement {

  override def subFormulas: Seq[FormulaConstruct] = Seq(left, right)

  override def toText: String = left.toText + " => " + right.toText

  override def toString: String = "Implies(" + left.toString + "," + right.toString + ")"

  override def substitute(theta: Theta): Implies = {
    Implies(left.substitute(theta), right.substitute(theta))
  }
}

/**
  * Represents the logical equivalence between two formulas ( { Formula1 } <=> { Formula2 } ).
  *
  * @example {{{
  *         isNumber(x) <=> hasDigits(x) ^ !hasLetters(x)
  * }}}
  *
  * @param left the left formula participating in the conditional statement
  * @param right the right formula participating in the conditional statement
  */
final case class Equivalence(left: FormulaConstruct, right: FormulaConstruct) extends ConditionalStatement {

  override def subFormulas: Seq[FormulaConstruct] = Seq(left, right)

  override def toText: String = left.toText + " <=> " + right.toText

  override def substitute(theta: Theta): Equivalence = {
    Equivalence(left.substitute(theta), right.substitute(theta))
  }
}

/**
  * Represents a logical quantifier over a variable.
  *
  * @see [[lomrf.logic.UniversalQuantifier]]
  *      [[lomrf.logic.ExistentialQuantifier]]
  *
  * @param variable the quantified variable
  * @param formula the formula in which the quantification applies
  */
sealed abstract class Quantifier(val variable: Variable, val formula: FormulaConstruct) extends FormulaConstruct {

  override def subFormulas: Seq[FormulaConstruct] = Seq(formula)

  override def getQuantifiers: List[Quantifier] = this :: formula.getQuantifiers

  override def isUnit: Boolean = false
}

/**
  * Represents the universal quantifier over a variable.
  *
  * @example {{{
  *     ForAll x hasFriend(x, y)
  * }}}
  *
  * @param v the universally quantified variable
  * @param f the formula in which the quantification applies
  */
final case class UniversalQuantifier(v: Variable, f: FormulaConstruct) extends Quantifier(v, f) {

  override def toText: String = "(Forall " + v.toText + " " + formula.toText + ")"

  override def substitute(theta: Theta): UniversalQuantifier = {

    val newVar = theta.get(v) match {
      case Some(x) if x.isInstanceOf[Variable] => x.asInstanceOf[Variable]
      case _                                   => v
    }

    UniversalQuantifier(newVar, f.substitute(theta))
  }
}

/**
  * Represents the universal quantifier over a variable.
  *
  * @example {{{
  *     Exist x hasFriend(x, Bob)
  * }}}
  *
  * @param v the universally quantified variable
  * @param f the formula in which the quantification applies
  */
final case class ExistentialQuantifier(v: Variable, f: FormulaConstruct) extends Quantifier(v, f) {

  override def toText: String = "(Exist " + v.toText + " " + formula.toText + ")"

  override def getExistentialQuantifiers: List[ExistentialQuantifier] = this :: formula.getExistentialQuantifiers

  override def substitute(theta: Theta): ExistentialQuantifier = {

    val newVar = theta.get(v) match {
      case Some(x) if x.isInstanceOf[Variable] => x.asInstanceOf[Variable]
      case _                                   => v
    }

    ExistentialQuantifier(newVar, f.substitute(theta))
  }
}

/**
  * Represents an atomic-formula (or atom for short) in FOL.
  *
  * @example {{{
  *         Friends(x, Anna)
  *         Friends(x, y)
  *         Friends(Bob, Anna)
  * }}}
  * @note An atomic formula may have either constants or variables as its
  *       arguments.The symbol ''Friends'' is the name of the atomic formula,
  *       x, y are variables and ''Bob'' and ''Anna'' are constants.
  *
  * @param symbol the symbol of the atom
  * @param terms the terms (arguments) of the atom
  */
case class AtomicFormula(symbol: String, terms: Vector[Term])
  extends DefiniteClauseConstruct with FormulaConstruct with TermIterable {

  val isDynamic = false

  val arity: Int = terms.size

  override def variables: Set[Variable] = uniqueVariablesIn(this)

  @transient override lazy val constants: Set[Constant] = uniqueConstantsIn(this)

  @transient override lazy val functions: Set[TermFunction] = uniqueFunctionsIn(this)

  @transient lazy val signature = AtomSignature(symbol, terms.size)

  override def countAtoms = 1

  override def iterator: Iterator[_ <: Term] = terms.iterator

  def isGround: Boolean = variables.isEmpty

  override def isUnit: Boolean = true

  override def toText: String = s"$symbol(${terms.map(_.toText).mkString(",")})"

  override def toString(): String = s"$symbol(${terms.map(_.toString).mkString(",")})"

  /**
    * Two atoms are similar, when:
    * <ul>
    * <li> both have the same signature, i.e. Name/Arity, and </li>
    * <li> the most general unifier (MGU) gives a result (i.e. unification is possible), in which all mapped entries are variables.
    * For example, the predicates Happens(x,t) and Happens(y,t) are similar, as the MGU gives Map(x->y)
    * and thus the only difference is the name of the variable in the first argument.
    * However, the predicates Happens(x,t) and Happens(A,t) are not similar, as the MGU gives Map(x->A).
    * Additionally, the predicates Happens(A,t) and Happens(B,t) are not similar, as the MGU cannot give any results, i.e.
    * the constants A and B cannot unified.
    * </li>
    * </ul>
    *
    * @param other an atom to compare against
    * @return true if this atom is similar to the given atom, otherwise false
    */
  def =~=(other: AtomicFormula): Boolean = {
    if (signature == other.signature && variables.size == other.variables.size) {
      Unify(this, other) match {
        case Some(x) if x.forall(_._2.isInstanceOf[Variable]) => true
        case _ => false
      }
    } else false
  }

  override def substitute(theta: Theta): AtomicFormula = AtomicFormula(symbol, terms.map(_.substitute(theta)))
}

/**
  * Represents a generic weighted FOL formula.
  *
  * @example {{{
  *         1.386 A v B ^ F => D
  * }}}
  *
  * @note A,B,F and D are FOL atoms and 1.386 is the corresponding weight.
  *
  * @param weight a weight expressing the formula's importance
  * @param formula a formula construct
  */
final case class WeightedFormula(weight: Double, formula: FormulaConstruct) extends Formula {

  override def subFormulas: Seq[FormulaConstruct] = Seq(formula)

  override def toText: String = weight match {
    case Double.PositiveInfinity => formula.toText + "."
    case x if x.isNaN            => formula.toText
    case _                       => weight.toString + " " + formula.toText
  }

  override def toString: String = weight.toString + " " + formula.toString

  override def hashCode(): Int = weight.hashCode() ^ formula.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case WeightedFormula(w, f) if w.isNaN && weight.isNaN && f == formula => true
    case WeightedFormula(w, f) if w == weight && f == formula => true
    case _ => false
  }

  override def substitute(theta: Theta): WeightedFormula = {
    WeightedFormula(weight, formula.substitute(theta))
  }
}

object WeightedFormula {
  def asUnit(formula: FormulaConstruct) = WeightedFormula(1.0, formula)
  def asHard(formula: FormulaConstruct) = WeightedFormula(Double.PositiveInfinity, formula)
}

/**
  * Represents a definite clause. Definite clauses are constructs mostly used in logic programming
  * and they express equivalences, that is, the if and only if conditional statement.
  *
  * @example {{{
  *           Smokes(y) :- Smokes(x) ^ Friends(x, y)
  * }}}
  *
  * @note The head cannot be negated and the body of the clause only consists of
  *       atoms or their negations connected using logical ''AND''.
  *
  * @param head an atomic formula that represents the head of the clause
  * @param body the body of the definite clause
  */
final case class DefiniteClause(head: AtomicFormula, body: DefiniteClauseConstruct) extends Formula {

  override def variables: Set[Variable] = body.subFormulas.foldRight(head.variables)((a: FormulaConstruct, b) => a.variables ++ b)

  @transient override lazy val constants: Set[Constant] = body.subFormulas.foldRight(head.constants)((a: FormulaConstruct, b) => a.constants ++ b)

  @transient override lazy val functions: Set[TermFunction] = body.subFormulas.foldRight(head.functions)((a: FormulaConstruct, b) => a.functions ++ b)

  override def subFormulas: Seq[FormulaConstruct] = Seq(head, body)

  override def getQuantifiers: List[Quantifier] = List[Quantifier]()

  override def getExistentialQuantifiers: List[ExistentialQuantifier] = List[ExistentialQuantifier]()

  override def toCNF(implicit constants: Map[String, ConstantsSet]): Set[Clause] = NormalForm.toCNF(Or(head, Not(body)))(constants)

  def toText: String = head.toText + " :- " + body.toText

  def toWeightedFormula: WeightedFormula = WeightedFormula(Double.PositiveInfinity, Implies(body, head))

  override def substitute(theta: Theta): DefiniteClause = {
    val sHead = head.substitute(theta)
    val sBody = body.substitute(theta)
    DefiniteClause(sHead, sBody)
  }

  /**
    * A pair of definite clauses are similar, when:
    * <ul>
    * <li> both have the same head predicate, and </li>
    * <li> both have the same number of literals in the body, and </li>
    * <li> for each literal in the body of this definite clause, another literal exists in the other definite
    * clause having the same sense (positive or negated) and similar atomic formulas.
    * For example the clause InitiatedAt(f,t1) :- HoldsAt(f,t2) is similar to InitiatedAt(f,t3) :- HoldsAt(f,t4) but
    * is not similar to InitiatedAt(f,t1) :- !HoldsAt(f,t4)
    * </li>
    * </ul>
    *
    * @param that the other definite clause for comparison
    *
    * @return true if this definite clause is similar to that one, otherwise false
    */
  def =~=(that: DefiniteClause): Boolean = {
    if (head =~= that.head && body.countAtoms == that.body.countAtoms) {
      var otherLiterals = that.bodyLiterals

      this.bodyLiterals.forall { lit1 =>
        otherLiterals.find(lit2 => lit1 =~= lit2) match {
          case Some(matchedLiteral) =>
            otherLiterals -= matchedLiteral
            true
          case _ => false
        }
      }
    } else false
  }
}

/**
  * Represents a weighted definite clause.
  *
  * @param weight a weight expressing the clause's importance
  * @param clause a definite clause
  */
final case class WeightedDefiniteClause(weight: Double, clause: DefiniteClause) extends Formula {

  override def subFormulas: Seq[FormulaConstruct] = clause.subFormulas

  override def toText: String = weight match {
    case Double.PositiveInfinity => clause.toText + "."
    case x if x.isNaN            => clause.toText
    case _                       => weight + " " + clause.toText
  }

  override def toString: String = weight.toString + " " + clause.toString

  override def hashCode(): Int = weight.hashCode() ^ clause.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case WeightedDefiniteClause(w, c) if w.isNaN && weight.isNaN && c == clause => true
    case WeightedDefiniteClause(w, c) if w == weight && c == clause => true
    case _ => false
  }

  /**
    * A pair of definite clauses are similar, when:
    * <ul>
    * <li> both have the same head predicate, and </li>
    * <li> both have the same number of literals in the body, and </li>
    * <li> for each literal in the body of this definite clause, another literal exists in the other definite
    * clause having the same sense (positive or negated) and similar atomic formulas.
    * For example the clause InitiatedAt(f,t1) :- HoldsAt(f,t2) is similar to InitiatedAt(f,t3) :- HoldsAt(f,t4) but
    * is not similar to InitiatedAt(f,t1) :- !HoldsAt(f,t4)
    * </li>
    * </ul>
    *
    * @param that the other definite clause for comparison
    *
    * @return true if this definite clause is similar to that one, otherwise false
    */
  def =~=(that: WeightedDefiniteClause): Boolean = this.clause =~= that.clause

  def toWeightedFormula: WeightedFormula = WeightedFormula(weight, Implies(clause.body, clause.head))

  override def substitute(theta: Theta): WeightedDefiniteClause = {
    WeightedDefiniteClause(weight, clause.substitute(theta))
  }
}

/**
  * Represents a ground atom (i.e., all its terms are constants) and its truth value
  * may be known (given by the input evidence).
  *
  * @example
  * <ul>
  * <li> '!Friends(A,B)' indicates that A and B are not friends, that is Friends(A,B) = False </li>
  * <li> 'Friends(D,F)'  indicates that D and F are friends, that is Friends(D,F) = True </li>
  * <li> 'Friends(D,F) 0.76' indicates that D and F are friends with probability 0.76, that is P(Friends(D,F) = True) = 0.76  </li>
  * <li> '!Friends(D,F) 0.76' indicates that D and F are friends with probability 0.76, that is P(Friends(D,F) = True) = 0.24  </li>
  * </ul>
  *
  * @note An evidence atom is an atomic formula with a known truth value.
  *
  * @param symbol the symbol of the atom
  * @param terms the terms (arguments) of the atom
  * @param state the state of the evidence atom (True, False or Unknown)
  * @param probability the probability of the evidence atom to be True.
  */
final class EvidenceAtom(
    override val symbol: String,
    override val terms: Vector[Constant],
    val state: TriState,
    val probability: Double = Double.NaN) extends AtomicFormula(symbol, terms) with EvidenceExpression {

  override def variables: Set[Variable] = Set.empty[Variable]

  @transient override lazy val constants: Set[Constant] = terms.toSet[Constant]

  @transient override lazy val functions: Set[TermFunction] = Set.empty[TermFunction]

  override def isGround = true

  override def toText: String = {
    lazy val sentence = s"$symbol(${terms.map(_.toText).mkString(",")})"

    state match {
      case TRUE    => sentence
      case FALSE   => s"!$sentence"
      case UNKNOWN => s"$sentence $probability"
    }
  }

  override def substitute(theta: Theta): EvidenceAtom = this
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

/**
  * Represents a function mapping from specific arguments to a return value.
  *
  * @example {{{
  *           5 = next(4)
  * }}}
  *
  * @note The above mapping states that given the argument '4'
  *       the next function produces the result value '5'.
  *
  * @param retValue a return value for the given arguments
  * @param functionSymbol the function symbol
  * @param values the arguments of the mapping
  */
final class FunctionMapping(
    val retValue: String,
    val functionSymbol: String,
    val values: Vector[String]) extends EvidenceExpression with Serializable {

  @transient lazy val signature = AtomSignature(functionSymbol, values.size)

  override def toString = s"$retValue = $functionSymbol(${values.mkString(",")})"

  /**
    * @return an auxiliary EvidenceAtom representing the function mapping
    */
  def toEvidenceAtom: EvidenceAtom = {
    val symbol = lomrf.AUX_PRED_PREFIX + functionSymbol
    val terms = values.+:(retValue).map(Constant)
    EvidenceAtom.asTrue(symbol, terms)
  }

  override def equals(that: Any): Boolean = that match {
    case fp: FunctionMapping =>
      this.functionSymbol == fp.functionSymbol &&
        this.retValue == fp.retValue &&
        this.values == fp.values
    case _ => false
  }
}

object FunctionMapping {
  def apply(retValue: String, functionSymbol: String, values: Vector[Constant]) =
    new FunctionMapping(retValue, functionSymbol, values.map(_.toString))
}
