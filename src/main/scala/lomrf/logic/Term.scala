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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.logic


/**
 * In First-Order Logic, a Term is any expression representing an object
 * in the domain. It can be a constant, a variable or a function applied
 * to a tuple of objects.
 *
 *
 */
sealed trait Term extends MLNExpression with Substitutable[Term]{

  val symbol: String

  /**
   * Determine whether this term is ground (i.e. does not contain any variable) or not.
   *
   * @return true if it is ground, false otherwise.
   */
  def isGround = false

  /**
   * Determine whether this term is a variable
   *
   * @return true if it is a variable, false otherwise.
   */
  def isVariable = false

  /**
   * Determine whether this term is a constant
   *
   * @return true if it is a constant, false otherwise.
   */
  def isConstant = false

  /**
   * Determine whether this term is a function
   *
   * @return true if it is a function, false otherwise.
   */
  def isFunction = false

  /**
   * Gives the textual representation of this term.
   */
  def toText: String

}

/**
 * This class represents a ''variable'' of a First-order Logic expression.
 *
 * @param symbol the variable name
 * @param domainName the variable domain (e.g. time, objects, persons, etc.)
 * @param index after variable standardization the index may be greater than 0 (see [[lomrf.logic.NormalForm]])
 */
sealed case class Variable(override val symbol: String,
                           private[logic] var domainName: String = Variable.UNDEFINED_DOMAIN,
                           index: Int = Variable.DEFAULT_INDEX,
                           groundPerConstant: Boolean = false) extends Term {


  override def isGround = false

  override def isVariable = true

  def domain: String = domainName

  def toText = {
    val txtSymbol = if (index > 0) symbol + "_" + index else symbol

    if(groundPerConstant) "+"+txtSymbol else txtSymbol
  }

  override def toString = symbol + (if (index > 0) "$" + index + ":" else ":") + domain

  override lazy val hashCode = symbol.## ^ index

  override def equals(obj: scala.Any) = obj match {
    case other: Variable =>
      this.symbol == other.symbol &&
        this.domain == other.domain &&
        this.index == other.index

    case _ => false
  }

  override def substitute(theta: Theta): Term = theta.getOrElse(this, this)
}

object Variable {

  /**
   * If the domain of a variable is not specified, then by default is '0' (i.e., unassigned domain).
   */
  val UNDEFINED_DOMAIN = "0"

  /**
   * In first-oder logic a variable name is unique inside the scope of a quantifier. For example:
   *
   * {{{A(x) ^ Exist x B(x)}}}
   *
   * The scope of variable 'x' after the existential quantifier is different from the one in A(x). Therefore,
   * the instance of the variable is different and in LoMRF this situation is represented by the index of the
   * variable. In the example, the index of the first variable is 0, while the index of the second one is 1
   * (i.e., incremented by one). As a result, the .toString() of the first one is 'x', while the .toString() of the
   * second one is 'x$1'.
   *
   * By default, all variables are assumed to have the default index value, which is zero.
   *
   * Please note that in LoMRF, unquantified variables are implicitly assumed that are universally quantified, for example:
   *
   * {{{Foo(x) => Bar(y)}}} is the same with {{{Forall x, y Foo(x) => Bar(y)}}}
   */
  val DEFAULT_INDEX = 0
}

/**
 * This class represents a constant symbol.
 *
 * @param symbol constant value
 */
sealed case class Constant(override val symbol: String) extends Term{

  override def isGround = true

  override def isConstant = true

  def toText = symbol

  override def toString = symbol

  override def substitute(theta: Theta): Constant = this
}

/**
 * This class represents a function term. A function in First-order Logic accepts some input
 * arguments (through constant instantiation of its variables) and produces a result (constant).
 *
 * @param symbol function name
 * @param terms function's arguments (Terms, i.e. constants, variables or other functions)
 * @param domain the domain of resulting constant (e.g. persons, object, numbers, etc.)
 */
sealed case class TermFunction(override val symbol: String,
                               terms: Vector[_ <: Term],
                               domain: String) extends Term with TermIterable{

  override def iterator: Iterator[_ <: Term] = terms.iterator

  def this(symbol: String, terms: Vector[Term]) = this(symbol, terms, "_?")

  lazy val signature = AtomSignature(symbol, terms.size)

  def variables: Set[Variable] = uniqueVariablesIn(this)

  lazy val constants: Set[Constant] = uniqueConstantsIn(this)

  lazy val functions: Set[TermFunction] = uniqueFunctionsIn(this)

  /**
   * A function is ground, only when it does not contain any variable
   */
  override def isGround = variables.isEmpty

  override def isFunction = true

  def arity: Int = terms.size

  def isDomainDefined: Boolean = domain != "_?"

  def toText = {
    if (terms.isEmpty) symbol + "()"
    else s"$symbol(${terms.map(_.toText).mkString(", ")})"
  }

  override def toString() = {
    if (terms.isEmpty) symbol + "():" + domain
    else s"$symbol(${terms.map(_.toString).mkString(", ")}):$domain"
  }

  override lazy val hashCode = {
    var code = symbol.## ^ domain.##
    for (term <- terms) code ^= term.##
    code
  }

  override def equals(obj: scala.Any) = obj match {
    case other: TermFunction =>
      other.## == this.## &&
        other.arity == this.arity &&
        other.symbol == this.symbol &&
        other.domain == this.domain &&
        other.terms == this.terms
    case _ => false
  }

  override def substitute(theta: Theta): TermFunction =
    TermFunction(symbol, terms.map(_.substitute(theta)), domain)

}

object TermFunction {

  /**
   * Default value for undefined domain
   */
  val UNDEFINED_RETURN_TYPE = "_?"

  /**
   * Create a new function with undefined domain
   *
   * @param symbol the name of the function
   * @param args the terms of the function
   * @return a new instance of function
   */
  def apply(symbol: String, args: Vector[Term]) = new TermFunction(symbol, args, UNDEFINED_RETURN_TYPE)
}
