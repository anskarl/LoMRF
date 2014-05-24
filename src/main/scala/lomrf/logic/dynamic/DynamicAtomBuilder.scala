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

package lomrf.logic.dynamic

import lomrf.logic._

/**
 * @author Anastasios Skarlatidis
 */

trait DynamicAtomBuilder {

  def signature: AtomSignature

  def stateFunction: (List[String] => Boolean)

  def apply(terms: List[Term]): DynamicAtom

}

private[dynamic] final class DynInfix(prefixSymbol: String, infixSymbol:String, terms:List[Term]) extends DynamicAtom(prefixSymbol, terms){
  require(terms.size == 2)

  override def toText: String = terms.head.toText+" "+ infixSymbol+" "+terms.last.toText
}


final class DynEqualsBuilder extends DynamicAtomBuilder {

  def signature = AtomSignature("equals", 2)

  def stateFunction = (constants: List[String]) => constants.head == constants.last

  def apply(terms: List[Term]) = new DynInfix("equals","=", terms)

}

object DynEqualsBuilder{
  def apply() = new DynEqualsBuilder
}


final class DynLessThanBuilder extends DynamicAtomBuilder {

  def signature = AtomSignature("lessThan", 2)

  def stateFunction = (constants: List[String]) => constants.head.toInt < constants.last.toInt

  def apply(terms: List[Term]) = new DynInfix("lessThan","<",terms)

}

object DynLessThanBuilder{
  def apply() = new DynLessThanBuilder
}

final class DynLessThanEqBuilder extends DynamicAtomBuilder {

  def signature = AtomSignature("lessThanEq", 2)

  def stateFunction = (constants: List[String]) => constants.head.toInt <= constants.last.toInt

  def apply(terms: List[Term]) = new DynInfix("lessThanEq","<=", terms)

}

object DynLessThanEqBuilder{
  def apply() = new DynLessThanEqBuilder
}


final class DynGreaterThanBuilder extends DynamicAtomBuilder {

  def signature = AtomSignature("greaterThan", 2)

  def stateFunction = (constants: List[String]) => constants.head.toInt > constants.last.toInt

  def apply(terms: List[Term]) = new DynInfix("greaterThan",">", terms)

}

object DynGreaterThanBuilder{
  def apply() = new DynGreaterThanBuilder
}


final class DynGreaterThanEqBuilder extends DynamicAtomBuilder {

  def signature = AtomSignature("greaterThanEq", 2)

  def stateFunction = (constants: List[String]) => constants.head.toInt >= constants.last.toInt

  def apply(terms: List[Term]) = new DynInfix("greaterThanEq",">=", terms)

}

object DynGreaterThanEqBuilder{
  def apply() = new DynGreaterThanEqBuilder
}


final class DynSubstringBuilder extends DynamicAtomBuilder{
  def signature = AtomSignature("substr", 2)

  def stateFunction = (constants: List[String]) => constants.last.contains(constants.head)

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new DynamicAtom("substr", terms)
  }
}

object DynSubstringBuilder{
  def apply() = new DynSubstringBuilder
}
