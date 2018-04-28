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

package lomrf.logic.dynamic

import lomrf.logic.{Term, AtomSignature, TermFunction}

trait DynamicFunctionBuilder {
  def signature: AtomSignature
  def resultFunction: (Vector[String] => String)
  def apply(terms: Vector[Term], resultDomain: String): TermFunction
  def apply(terms: Vector[Term]): TermFunction
}

final class DynSuccFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("succ", 1)

  def resultFunction = (constants: Vector[String]) => (constants.head.toInt + 1).toString //successive (n+1)

  def apply(terms: Vector[Term], resultDomain: String) = {
    require(terms.size == 1)
    new TermFunction("succ", terms, resultDomain)
  }

  def apply(terms: Vector[Term]) = {
    require(terms.size == 1)
    new TermFunction("succ", terms)
  }


  def apply(term: Term, resultDomain: String) = {
    new TermFunction("succ", Vector(term), resultDomain)
  }

  def apply(term: Term) = {
    new TermFunction("succ", Vector(term))
  }

}

object DynSuccFunctionBuilder{
  def apply() = new DynSuccFunctionBuilder
}

final class DynPrecFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("prec", 1)

  def resultFunction = (constants: Vector[String]) => (constants.head.toInt - 1).toString  //preceding  (n-1)

  def apply(terms: Vector[Term], resultDomain: String) = {
    require(terms.size == 1)
    new TermFunction("prec", terms, resultDomain)
  }

  def apply(terms: Vector[Term]) = {
    require(terms.size == 1)
    new TermFunction("prec", terms)
  }

  def apply(term: Term, resultDomain: String) = {
    new TermFunction("prec", Vector(term), resultDomain)
  }

  def apply(term: Term) = {
    new TermFunction("prec", Vector(term))
  }
}

object DynPrecFunctionBuilder {

  def apply() = new DynPrecFunctionBuilder
}

final class DynPlusFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("plus", 2)

  def resultFunction = (constants: Vector[String]) => (constants.head.toInt + constants.last.toInt).toString

  def apply(terms: Vector[Term], resultDomain: String) = {
    require(terms.size == 2)
    new TermFunction("plus", terms, resultDomain)
  }

  def apply(terms: Vector[Term]) = {
    require(terms.size == 2)
    new TermFunction("plus", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("plus", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("plus", Vector(term, term2))
  }
}

object DynPlusFunctionBuilder{
  def apply() = new DynPlusFunctionBuilder
}


final class DynMinusFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("minus", 2)

  def resultFunction = (constants: Vector[String]) => (constants.head.toInt - constants.last.toInt).toString

  def apply(terms: Vector[Term], resultDomain: String) = {
    require(terms.size == 2)
    new TermFunction("minus", terms, resultDomain)
  }

  def apply(terms: Vector[Term]) = {
    require(terms.size == 2)
    new TermFunction("minus", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("minus", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("minus", Vector(term, term2))
  }
}

object DynMinusFunctionBuilder{
  def apply() = new DynMinusFunctionBuilder
}


final class DynTimesFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("times", 2)

  def resultFunction = (constants: Vector[String]) => (constants.head.toInt * constants.last.toInt).toString

  def apply(terms: Vector[Term], resultDomain: String) = {
    require(terms.size == 2)
    new TermFunction("times", terms, resultDomain)
  }

  def apply(terms: Vector[Term]) = {
    require(terms.size == 2)
    new TermFunction("times", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("times", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("times", Vector(term, term2))
  }
}

object DynTimesFunctionBuilder{
  def apply() = new DynTimesFunctionBuilder
}

final class DynDividedByFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("dividedBy", 2)

  def resultFunction = (constants: Vector[String]) => (constants.head.toInt / constants.last.toInt).toString

  def apply(terms: Vector[Term], resultDomain: String) = {
    require(terms.size == 2)
    new TermFunction("dividedBy", terms, resultDomain)
  }

  def apply(terms: Vector[Term]) = {
    require(terms.size == 2)
    new TermFunction("dividedBy", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("dividedBy", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("dividedBy", Vector(term, term2))
  }
}

object DynDividedByFunctionBuilder{
  def apply() = new DynDividedByFunctionBuilder
}

final class DynModFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("mod", 2)

  def resultFunction = (constants: Vector[String]) => (constants.head.toInt % constants.last.toInt).toString

  def apply(terms: Vector[Term], resultDomain: String) = {
    require(terms.size == 2)
    new TermFunction("mod", terms, resultDomain)
  }

  def apply(terms: Vector[Term]) = {
    require(terms.size == 2)
    new TermFunction("mod", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("mod", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("mod", Vector(term, term2))
  }
}

object DynModFunctionBuilder{
  def apply() = new DynModFunctionBuilder
}


final class DynConcatFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("concat", 2)

  def resultFunction = (constants: Vector[String]) => constants.head.concat(constants.last)

  def apply(terms: Vector[Term], resultDomain: String) = {
    require(terms.size == 2)
    new TermFunction("concat", terms, resultDomain)
  }

  def apply(terms: Vector[Term]) = {
    require(terms.size == 2)
    new TermFunction("concat", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String) = {
    new TermFunction("concat", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term) = {
    new TermFunction("concat", Vector(term, term2))
  }
}

object DynConcatFunctionBuilder{
  def apply() = new DynConcatFunctionBuilder
}
