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
 * In First-Order Logic, a Term is any expression representing an object
 * in the domain. It can be a constant, a variable or a function applied
 * to a tuple of objects.
 *
 * @author Anastasios Skarlatidis
 */
sealed abstract class Term(symbol: String) extends MLNExpression {

  /**
   * The name of this term
   */
  def getSymbol: String = symbol

  /**
   * Determine whether this term is ground (i.e. does not contain any variable) or not.
   *
   * @return true if it is ground, false otherwise.
   */
  def isGround: Boolean

  /**
   * If the symbol is numeric, it will give its ''int'' numeric representation.
   */
  def toInt = symbol.toInt
  /**
   * If the symbol is numeric, it will give its ''double'' numeric representation.
   */
  def toDouble = symbol.toDouble

  /**
   * If the symbol is numeric, it will give its ''long'' numeric representation.
   */
  def toLong = symbol.toLong

  /**
   * If the symbol is numeric, it will give its ''float'' numeric representation.
   */
  def toFloat = symbol.toFloat

  /**
   * Gives the textual representation of this term.
   */
  def toText: String

  override def toString = symbol

}

/**
 * This class represents a ''variable'' of a First-order Logic expression.
 *
 * @param symbol the variable name
 * @param domainName the variable domain (e.g. time, objects, persons, etc.)
 * @param index after variable standardization the index may be greater than 0 (see [[lomrf.logic.NormalForm]])
 */
case class Variable(symbol: String, private[logic] var domainName: String = Variable.UNDEFINED_DOMAIN, index:Int = Variable.DEFAULT_INDEX) extends Term(symbol) {

  /**
   * Always false
   */
  def isGround = false

  def domain: String = domainName

  def toText = if(index>0) symbol+"-"+index else symbol //symbol.replaceAll("\\$","-")

  override def toString = symbol+(if(index>0) "$"+index+":" else ":")+domain
}

object Variable{
  val UNDEFINED_DOMAIN = "0"
  val DEFAULT_INDEX = 0
}

/**
 * This class represents a constant symbol.
 *
 * @param symbol constant value
 */
case class Constant(symbol: String) extends Term(symbol) {

  def isGround: Boolean = true

  def toText = symbol

  override def toString = symbol

}

/**
 * This class represents a function term. A function in First-order Logic accepts some input
 * arguments (through constant instantiation of its variables) and produces a result (constant).
 *
 * @param symbol function name
 * @param args function's arguments (Terms, i.e. constants, variables or other functions)
 * @param domain the domain of resulting constant (e.g. persons, object, numbers, etc.)
 */
case class Function(symbol: String, args: List[Term], domain: String) extends Term(symbol){

  def this(symbol: String, args: List[Term]) = this(symbol, args, "_?")

  lazy val signature = AtomSignature(symbol, args.size)

  lazy val variables:Set[Variable] = args.foldRight(Set[Variable]()){
    (a: Term, b) => a match {
      case v: Variable => Set(v) ++ b
      case f: Function => f.variables ++ b
      case _ => b
    }
  }

  /**
   * A function is computeGroundings, if all its arguments are computeGroundings.
   */
  def isGround: Boolean = !args.exists(_.isGround != true)

  def arity: Int = args.size

  def isDomainDefined: Boolean = domain != "_?"

  def toText = {
    if(args == Nil) symbol + "()"
    else symbol + "(" + args.map((t: Term) => t.toText).reduceLeft( _ + ", " + _ ) + ")"
  }
  
  override def toString = {
    if(args == Nil) symbol+"():"+domain
    else symbol + "(" + args.map((t: Term) => t.toString).reduceLeft( _ + ", " + _ ) + "):"+domain
  }

}

object Function{

  val UNDEFINED_RETURN_TYPE = "_?"

  def apply(symbol: String, args: List[Term]) = new Function(symbol, args, UNDEFINED_RETURN_TYPE)
}