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
 * @author Anastasios Skarlatidis
 */

class DomainParser extends CommonsMLNParser {

  val atomTypeNoArgs = """[A-Z0-9]([a-zA-Z0-9]|_[a-zA-Z0-9])*\s*\n""".r
  val atomType = """[A-Z0-9]([a-zA-Z0-9]|_[a-zA-Z0-9])*\(.*\)\n""".r
  val newLine = """\n""".r


  def definitions: Parser[List[MLNDomainExpression]] = rep(definition)

  def definition: Parser[MLNDomainExpression] =
    atomicType | functionType | integerTypeDefinition | constantTypeDefinition

  /**
   * Parses constant type definitions, for example:
   *
   * fluent = { Meeting, Browsing, Fighting }
   * where 'fluent' is the constant type name, 'Meeting', 'Browsing' and 'Fighting' are the
   * allowed constant literals
   */
  def constantTypeDefinition: Parser[ConstantTypeDefinition] = {
    (lowerCaseID ~ "=" ~ "{" ~ repsep(upperCaseID, ",") ~ "}") ^^ {
      case name ~ "=" ~ "{" ~ constants ~ "}" => ConstantTypeDefinition(name, constants)
    }
  }

  /**
   * Parses integer interval type definitions, for example:
   *
   * time =  { 0, ..., 10 }
   * where 'time' is the constant type name and is takes integer literals
   * that range from 0 to 10.
   */
  def integerTypeDefinition: Parser[IntegerTypeDefinition] =
    (lowerCaseID ~ "=" ~ "{" ~ numPosInt ~ "," ~ "..." ~ "," ~ numPosInt ~ "}") ^^ {
      case name ~ "=" ~ "{" ~ fromNum ~ "," ~ "..." ~ "," ~ toNum ~ "}" =>
        IntegerTypeDefinition(name, fromNum.toInt, toNum.toInt)
    }

  /**
   * Parses atomic definitions, for example:
   *
   * Parent(person, person)
   * where 'Parent' is the predicate symbol and 'person' is the type definition of its two arguments.
   *
   * In this example, the atomic type definition 'Parent(person, person)' defines a predicate with symbol name
   * 'Parent', with arity 2 (Parent/2) and its 2 arguments accept variables or constants of 'person' type.
   */

  def atomicTypeWithArgs: Parser[AtomicType] = (upperCaseID ~ "(" ~ repsep(lowerCaseID, ",") ~ ")") ^^ {
    case name ~ "(" ~ termArgs ~ ")" => AtomicType(name, termArgs)
  }

  def atomicTypeWithoutArgs: Parser[AtomicType] = atomTypeNoArgs ^^ {
    case name => AtomicType(name.trim, List[String]())
  }

  def atomicType = atomicTypeWithArgs | atomicTypeWithoutArgs


  /**
   * Parses function definitions, for example:
   *
   * fluent walking(id, id)
   * where 'walking' id the function name, 'fluent' is the return type of the function and 'id' is the type of
   * function input arguments.
   *
   */
  def functionType: Parser[FunctionType] =
    (lowerCaseID ~ lowerCaseID ~ "(" ~ repsep(lowerCaseID, ",") ~ ")") ^^ {
      case retType ~ funcName ~ "(" ~ funcArgs ~ ")" => FunctionType(retType, funcName, funcArgs)
    }


}