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

package lomrf.logic.parser

import auxlib.log.Logging
import lomrf.logic._

/**
  * Parser for MLN domain definitions. Definition can be of constant type,
  * integer type, atomic type or function type.
  */
final class DomainParser extends CommonsMLNParser with Logging {

  /**
    * Parses MLN domain definitions.
    *
    * @return a list of MLN domain expressions
    */
  def definitions: Parser[List[MLNDomainExpression]] = rep(definition)

  /**
    * Parses an MLN definition. A definition may be an atomic type, a function type,
    * an integer type or a constant type definition.
    *
    * @return an MLN domain expression
    */
  def definition: Parser[MLNDomainExpression] =
    atomicTypeDefinition | functionTypeDefinition | integerTypeDefinition | constantTypeDefinition

  /**
    * Parses constant type definitions, for instance:
    *
    * {{{
    *   fluent = { Meeting, Browsing, Fighting }
    * }}}
    *
    * In this example '''fluent''' is the constant type name, and '''Meeting''',
    * '''Browsing''' and '''Fighting''' are the allowed constant literals.
    */
  def constantTypeDefinition: Parser[ConstantTypeDefinition] =
    (lowerCaseID ~ "=" ~ "{" ~ repsep(upperCaseID, ",") ~ "}") ^^ {
      case name ~ "=" ~ "{" ~ constants ~ "}" => ConstantTypeDefinition(name, constants)
    }

  /**
    * Parses integer interval type definitions, for instance:
    *
    * {{{
    *   time =  { 0, ..., 10 }
    * }}}
    *
    * In this example '''time''' is the constant type name and is takes
    * integer literals in the range from 0 to 10.
    */
  def integerTypeDefinition: Parser[IntegerTypeDefinition] =
    (lowerCaseID ~ "=" ~ "{" ~ numPosInt ~ "," ~ "..." ~ "," ~ numPosInt ~ "}") ^^ {
      case name ~ "=" ~ "{" ~ fromNum ~ "," ~ "..." ~ "," ~ toNum ~ "}" =>
        IntegerTypeDefinition(name, fromNum.toInt, toNum.toInt)
    }

  /**
    * Parses atomic definitions having arguments or not (unary predicates).
    */
  def atomicTypeDefinition: Parser[AtomicType] = atomicTypeArgs | atomicTypeNoArgs

  /**
    * Parses atomic definitions having arguments, for instance:
    *
    * {{{
    *   Parent(person, person)
    * }}}
    *
    * In this example '''Parent''' is the predicate symbol and '''person'''
    * is the type definition of its arguments. The atomic type definition
    * '''Parent(person, person)''' defines a predicate having the symbol name
    * '''Parent''' of arity 2 (signature: Parent/2) and its 2 arguments accept
    * variables or constants of type '''person'''.
    */
  def atomicTypeArgs: Parser[AtomicType] = (upperCaseID ~ "(" ~ repsep(lowerCaseID, ",") ~ ")") ^^ {
    case name ~ "(" ~ termArgs ~ ")" => AtomicType(name, termArgs.toVector)
  }

  /**
    * Parses atomic definitions having NO arguments, for instance:
    *
    * {{{
    *   UnaryPredicate
    * }}}
    *
    * In this example '''UnaryPredicate''' is the predicate symbol. The atomic type definition
    * '''UnaryPredicate''' defines a predicate having the symbol name '''UnaryPredicate''' of
    * arity 0 (signature: UnaryPredicate/0) having no arguments.
    */
  def atomicTypeNoArgs: Parser[AtomicType] = upperCaseID ^^
    (name => AtomicType(name.trim, Vector.empty[String]))

  /**
    * Parses function definitions, for instance:
    *
    * {{{
    *   fluent walking(id, id)
    * }}}
    *
    * In this example '''walking''' is the function name, '''fluent''' is the return type
    * of the function and '''id''' is the type of function input arguments.
    */
  def functionTypeDefinition: Parser[FunctionType] =
    (lowerCaseID ~ lowerCaseID ~ "(" ~ repsep(lowerCaseID, ",") ~ ")") ^^ {
      case retType ~ funcName ~ "(" ~ funcArgs ~ ")" => FunctionType(retType, funcName, funcArgs.toVector)
    }


  //--------------------------------------------------------------------------------------------------------------------
  //--- Public API defining utility functions for parsing
  //--------------------------------------------------------------------------------------------------------------------

  def parseConstantType(src: String): ConstantTypeDefinition = parse(constantTypeDefinition, src) match {
    case Success(result, _) => result
    case x => fatal(s"Can't parse the following expression: $x")
  }

  def parseIntegerType(src: String): IntegerTypeDefinition = parse(integerTypeDefinition, src) match {
    case Success(result, _) => result
    case x => fatal(s"Can't parse the following expression: $x")
  }

  def parseAtomicType(src: String): AtomicType = parse(atomicTypeDefinition, src) match {
    case Success(result, _) => result
    case x => fatal(s"Can't parse the following expression: $x")
  }

  def parseFunctionType(src: String): FunctionType = parse(functionTypeDefinition, src) match {
    case Success(result, _) => result
    case x => fatal(s"Can't parse the following expression: $x")
  }

  def parseSingleDefinition(src: String): MLNDomainExpression = parse(definition, src) match {
    case Success(result, _) => result
    case x => fatal(s"Can't parse the following expression: $x")
  }

  def parseDefinitions(src: String): List[MLNDomainExpression] = parse(definitions, src) match {
    case Success(result, _) => result
    case x => fatal(s"Can't parse the given definitions:\n$x")
  }
}
