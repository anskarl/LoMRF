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

import lomrf.logic.{Term, AtomSignature, Function}

/**
 * @author Anastasios Skarlatidis
 */

trait DynamicFunctionBuilder {
  def signature: AtomSignature
  def resultFunction: (List[String] => String)
  def apply(terms: List[Term], resultDomain: String): Function
  def apply(terms: List[Term]): Function
}

final class DynSuccFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("succ", 1)

  def resultFunction = (constants: List[String]) => (constants.head.toInt + 1).toString //successive (n+1)

  def apply(terms: List[Term], resultDomain: String) = {
    require(terms.size == 1)
    new Function("succ", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 1)
    new Function("succ", terms)
  }
}

object DynSuccFunctionBuilder{
  def apply() = new DynSuccFunctionBuilder
}

final class DynPrecFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("prec", 1)

  def resultFunction = (constants: List[String]) => (constants.head.toInt - 1).toString  //preceding  (n-1)

  def apply(terms: List[Term], resultDomain: String) = {
    require(terms.size == 1)
    new Function("prec", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 1)
    new Function("prec", terms)
  }
}

object DynPrecFunctionBuilder {

  def apply() = new DynPrecFunctionBuilder
}

final class DynPlusFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("plus", 2)

  def resultFunction = (constants: List[String]) => (constants.head.toInt + constants.last.toInt).toString

  def apply(terms: List[Term], resultDomain: String) = {
    require(terms.size == 2)
    new Function("plus", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new Function("plus", terms)
  }
}

object DynPlusFunctionBuilder{
  def apply() = new DynPlusFunctionBuilder
}


final class DynMinusFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("minus", 2)

  def resultFunction = (constants: List[String]) => (constants.head.toInt - constants.last.toInt).toString

  def apply(terms: List[Term], resultDomain: String) = {
    require(terms.size == 2)
    new Function("minus", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new Function("minus", terms)
  }
}

object DynMinusFunctionBuilder{
  def apply() = new DynMinusFunctionBuilder
}


final class DynTimesFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("times", 2)

  def resultFunction = (constants: List[String]) => (constants.head.toInt * constants.last.toInt).toString

  def apply(terms: List[Term], resultDomain: String) = {
    require(terms.size == 2)
    new Function("times", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new Function("times", terms)
  }
}

object DynTimesFunctionBuilder{
  def apply() = new DynTimesFunctionBuilder
}

final class DynDividedByFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("dividedBy", 2)

  def resultFunction = (constants: List[String]) => (constants.head.toInt / constants.last.toInt).toString

  def apply(terms: List[Term], resultDomain: String) = {
    require(terms.size == 2)
    new Function("dividedBy", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new Function("dividedBy", terms)
  }
}

object DynDividedByFunctionBuilder{
  def apply() = new DynDividedByFunctionBuilder
}

final class DynModFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("mod", 2)

  def resultFunction = (constants: List[String]) => (constants.head.toInt % constants.last.toInt).toString

  def apply(terms: List[Term], resultDomain: String) = {
    require(terms.size == 2)
    new Function("mod", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new Function("mod", terms)
  }
}

object DynModFunctionBuilder{
  def apply() = new DynModFunctionBuilder
}


final class DynConcatFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("concat", 2)

  def resultFunction = (constants: List[String]) => constants.head.concat(constants.last)

  def apply(terms: List[Term], resultDomain: String) = {
    require(terms.size == 2)
    new Function("concat", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new Function("concat", terms)
  }
}

object DynConcatFunctionBuilder{
  def apply() = new DynConcatFunctionBuilder
}
