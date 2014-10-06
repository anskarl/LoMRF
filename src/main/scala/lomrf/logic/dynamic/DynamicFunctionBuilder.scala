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

import lomrf.logic.{Term, AtomSignature, TermFunction}

/**
 * @author Anastasios Skarlatidis
 */

trait DynamicFunctionBuilder {
  def signature: AtomSignature
  def resultFunction: (List[String] => String)
  def apply(terms: List[Term], resultDomain: String): TermFunction
  def apply(terms: List[Term]): TermFunction
}

final class DynSuccFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("succ", 1)

  def resultFunction = (constants: List[String]) => (constants.head.toInt + 1).toString //successive (n+1)

  def apply(terms: List[Term], resultDomain: String) = {
    require(terms.size == 1)
    new TermFunction("succ", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 1)
    new TermFunction("succ", terms)
  }


  def apply(term: Term, resultDomain: String) = {
    new TermFunction("succ", term :: Nil, resultDomain)
  }

  def apply(term: Term) = {
    new TermFunction("succ", term :: Nil)
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
    new TermFunction("prec", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 1)
    new TermFunction("prec", terms)
  }

  def apply(term: Term, resultDomain: String) = {
    new TermFunction("prec", term :: Nil, resultDomain)
  }

  def apply(term: Term) = {
    new TermFunction("prec", term :: Nil)
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
    new TermFunction("plus", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new TermFunction("plus", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("plus", term :: term2 :: Nil, resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("plus", term :: term2 :: Nil)
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
    new TermFunction("minus", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new TermFunction("minus", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("minus", term :: term2 :: Nil, resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("minus", term :: term2 :: Nil)
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
    new TermFunction("times", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new TermFunction("times", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("times", term :: term2 :: Nil, resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("times", term :: term2 :: Nil)
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
    new TermFunction("dividedBy", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new TermFunction("dividedBy", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("dividedBy", term :: term2 :: Nil, resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("dividedBy", term :: term2 :: Nil)
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
    new TermFunction("mod", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new TermFunction("mod", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("mod", term :: term2 :: Nil, resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("mod", term :: term2 :: Nil)
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
    new TermFunction("concat", terms, resultDomain)
  }

  def apply(terms: List[Term]) = {
    require(terms.size == 2)
    new TermFunction("concat", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("concat", term :: term2 :: Nil, resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("concat", term :: term2 :: Nil)
  }
}

object DynConcatFunctionBuilder{
  def apply() = new DynConcatFunctionBuilder
}
