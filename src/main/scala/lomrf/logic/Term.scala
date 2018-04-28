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


/**
  * In ''First-Order Logic'' a `Term` is any expression representing an object
  * in the domain. It can be a constant ([[lomrf.logic.Constant Constant]]) or
  * a variable ([[lomrf.logic.Variable]]) or a term ([[lomrf.logic.TermFunction]])
  * applied to a tuple of objects, i.e. a tuple of `Term` instances.
  *
  * @see [[lomrf.logic.Substitutable]] <br> [[lomrf.logic.MLNExpression]]
  */
sealed trait Term extends MLNExpression with Substitutable[Term]{

  val symbol: String

  /**
    * Determine whether this term is ''ground'' (i.e. does not contain any variable) or not.
    *
    * @return true if it is ground, false otherwise.
    */
  def isGround = false

  /**
    * Determine whether this term is a variable.
    *
    * @see [[lomrf.logic.Variable]]
    *
    * @return true if it is a variable, false otherwise.
    */
  def isVariable = false

  /**
    * Determine whether this term is a constant.
    *
    * @see [[lomrf.logic.Constant]]
    *
    * @return true if it is a constant, false otherwise.
    */
  def isConstant = false

  /**
    * Determine whether this term is a function.
    *
    * @see [[lomrf.logic.TermFunction]]
    *
    * @return true if it is a function, false otherwise.
    */
  def isFunction = false

  /**
    * @return the textual representation of this term.
    */
  def toText: String
}

/**
  * Represents a variable of a ''First-Order Logic'' expression. Variables can be replaced
  * by constants belonging to their domain type.
  *
  * @example A variable `p` having domain type {{{ person = {Bob, Anna} }}} can only be replaced
  *          by the constants `Bob` and `Anna`.
  *
  * @note Due to variable standardization the index may be greater than '0'
  *       (see [[lomrf.logic.NormalForm]])
  *
  * @param symbol the variable symbol
  * @param domainName the variable domain type (e.g., time, objects, persons, etc.)
  * @param index the variable index in a ''First Order Logic'' expression (default is '0')
  * @param groundPerConstant grounds the variable appearances by replacing them
  */
sealed case class Variable(override val symbol: String,
                           private[logic] var domainName: String = Variable.UNDEFINED_DOMAIN,
                           index: Int = Variable.DEFAULT_INDEX,
                           groundPerConstant: Boolean = false) extends Term {


  /**
    * @inheritdoc
    * @return true if it is ground, false otherwise.
    */
  override def isGround = false

  /**
    * @inheritdoc
    * @return true if it is a variable, false otherwise.
    */
  override def isVariable = true

  /**
    * @return the variable domain name
    */
  def domain: String = domainName

  /**
    * @return the textual representation of this term.
    */
  def toText = {
    val txtSymbol = if (index > 0) symbol + "_" + index else symbol

    if(groundPerConstant) "+"+txtSymbol else txtSymbol
  }

  /**
    * @return a string representation for this variable
    *         containing its `symbol`, its `index` and its `domain`.
    */
  override def toString: String = symbol + (if (index > 0) "$" + index + ":" else ":") + domain

  /**
    * Hash code of a variable produces a value using the hash code of
    * its `symbol` and its `index`.
    */
  override lazy val hashCode: Int = symbol.## ^ index

  /**
    * Compares this variable to another object.
    *
    * @param obj the object to compare
    * @return true if `obj` is a variable that has the same `symbol`,
    *         `domain` and `index`, false otherwise
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Variable =>
      this.symbol == other.symbol &&
        this.domain == other.domain &&
        this.index == other.index

    case _ => false
  }

  /**
    * Substitutes this variable instance with another term in the given
    * mapping, if any mapping exists.
    *
    * @see [[lomrf.logic.Term]]
    *
    * @param theta a mapping from terms to terms
    * @return the substituted term according to the given mapping
    */
  override def substitute(theta: Theta): Term = theta.getOrElse(this, this)
}

object Variable {

  /**
   * If the domain of a variable is not specified, then by default is '0' (i.e., unassigned domain).
   */
  val UNDEFINED_DOMAIN = "0"

  /**
    * In '''First-Order Logic''' a variable name is unique inside the scope of a quantifier.
    *
    * @example {{{A(x) ^ Exist x B(x)}}}
    *
    * The scope of variable `x` after the existential quantifier is different from the one in `A(x)`. Therefore,
    * the instance of the variable is different and in '''LoMRF''' this situation is represented by the index of the
    * variable. In the example, the index of the first variable is 0, while the index of the second one is 1
    * (i.e., incremented by one). As a result, the .toString() of the first one is `x`, while the .toString() of the
    * second one is 'x$1'.By default, all variables are assumed to have the default index value, which is zero.
    *
    * @note In LoMRF, unquantified variables are implicitly assumed that are universally quantified.
    *
    * @example {{{Foo(x) => Bar(y)}}} is the same with {{{Forall x, y Foo(x) => Bar(y)}}}
    */
  val DEFAULT_INDEX = 0
}

/**
 * Represents a constant symbol.
 *
 * @param symbol constant value
 */
sealed case class Constant(override val symbol: String) extends Term{

  /**
    * @inheritdoc
    * @return true if it is ground, false otherwise.
    */
  override def isGround = true

  /**
    * @inheritdoc
    * @return true if it is a constant, false otherwise.
    */
  override def isConstant = true

  /**
    * @return the textual representation of this term.
    */
  def toText: String = symbol

  /**
    * @return the `symbol` of this constant
    */
  override def toString: String = symbol

  /**
    * A constant cannot be substituted.
    *
    * @see [[lomrf.logic.Term]]
    *
    * @param theta a mapping from term to term
    * @return this constant
    */
  override def substitute(theta: Theta): Constant = this
}

/**
  * Represents a term function. A function in ''First-Order Logic'' accepts a tuple of input arguments
  * (through constant instantiation of the variables) and produces a result (constant).
  *
  * @param symbol function symbol
  * @param terms function's arguments (Terms, i.e. constants, variables or other functions)
  * @param domain the domain of resulting constant (e.g. persons, object, numbers, etc.)
  */
sealed case class TermFunction(override val symbol: String,
                               terms: Vector[_ <: Term],
                               domain: String) extends Term with TermIterable{
  
  def this(symbol: String, terms: Vector[Term]) = this(symbol, terms, "_?")

  lazy val signature = AtomSignature(symbol, terms.size)

  lazy val constants: Set[Constant] = uniqueConstantsIn(this)

  lazy val functions: Set[TermFunction] = uniqueFunctionsIn(this)

  override def iterator: Iterator[_ <: Term] = terms.iterator

  def variables: Set[Variable] = uniqueVariablesIn(this)

  /**
    * @inheritdoc
    * @note A function is ground, only when it does not contain any variable.
    *
    * @return true if it is ground, false otherwise.
    */
  override def isGround: Boolean = variables.isEmpty

  /**
    * @inheritdoc
    * @return true if it is a function, false otherwise.
    */
  override def isFunction = true

  /**
    * @return the arity of the function, i.e., the number of terms
    */
  def arity: Int = terms.size

  /**
    * Determine whether the function has a defined domain.
    *
    * @return true if the domain of the function is not undefined, false otherwise.
    */
  def isDomainDefined: Boolean = domain != "_?"

  /**
    * @return the textual representation of this term.
    */
  def toText = {
    if (terms.isEmpty) symbol + "()"
    else s"$symbol(${terms.map(_.toText).mkString(", ")})"
  }

  /**
    * @return a string representation for this term function
    *         containing its `symbol`, its `domain` and its
    *         corresponding `terms`.
    */
  override def toString(): String = {
    if (terms.isEmpty) symbol + "():" + domain
    else s"$symbol(${terms.map(_.toString).mkString(", ")}):$domain"
  }

  /**
    * Hash code of a term function produces a value using the hash code
    * of its `symbol`, `domain`, as well as its `terms` hash codes.
    */
  override lazy val hashCode: Int = {
    var code = symbol.## ^ domain.##
    for (term <- terms) code ^= term.##
    code
  }

  /**
    * Compares this term function to another object.
    *
    * @param obj the object to compare
    * @return true if `obj` is a term function that has the same `symbol`,
    *         `domain` and `terms`, false otherwise
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: TermFunction =>
      other.## == this.## &&
        other.arity == this.arity &&
        other.symbol == this.symbol &&
        other.domain == this.domain &&
        other.terms == this.terms
    case _ => false
  }

  /**
    * Substitutes all terms of this function with other terms in the given mapping,
    * if any mapping exists for these terms.
    *
    * @see [[lomrf.logic.Term]]
    *
    * @param theta a mapping from terms to terms
    * @return the substituted function according to the given mapping
    */
  override def substitute(theta: Theta): TermFunction =
    TermFunction(symbol, terms.map(_.substitute(theta)), domain)
}

object TermFunction {

  /**
    * Default value for undefined domain.
    */
  val UNDEFINED_RETURN_TYPE = "_?"

  /**
    * Creates a new `TermFunction` with undefined domain
    *
    * @param symbol the symbol of the function
    * @param args the terms of the function
    * @return a new instance of function
    */
  def apply(symbol: String, args: Vector[Term]) = new TermFunction(symbol, args, UNDEFINED_RETURN_TYPE)
}
