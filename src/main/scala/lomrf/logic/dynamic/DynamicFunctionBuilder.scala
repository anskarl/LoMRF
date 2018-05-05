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

import lomrf.logic.{ Term, AtomSignature, TermFunction }

/**
  * Dynamic function builder should be extended by any dynamic function
  * type in order to define its functionality.
  */
trait DynamicFunctionBuilder {
  def signature: AtomSignature

  def resultFunction: Vector[String] => String

  def apply(terms: Vector[Term], resultDomain: String): TermFunction

  def apply(terms: Vector[Term]): TermFunction
}

// -- Various dynamic function builders:

final class DynSuccFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("succ", 1)

  def resultFunction: Vector[String] => String =
    (constants: Vector[String]) => (constants.head.toInt + 1).toString // successive (n+1)

  def apply(terms: Vector[Term], resultDomain: String): TermFunction = {
    require(terms.size == 1)
    new TermFunction("succ", terms, resultDomain)
  }

  def apply(terms: Vector[Term]): TermFunction = {
    require(terms.size == 1)
    new TermFunction("succ", terms)
  }

  def apply(term: Term, resultDomain: String): TermFunction = {
    new TermFunction("succ", Vector(term), resultDomain)
  }

  def apply(term: Term): TermFunction = {
    new TermFunction("succ", Vector(term))
  }
}

final class DynPrecFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("prec", 1)

  def resultFunction: Vector[String] => String =
    (constants: Vector[String]) => (constants.head.toInt - 1).toString // preceding  (n-1)

  def apply(terms: Vector[Term], resultDomain: String): TermFunction = {
    require(terms.size == 1)
    new TermFunction("prec", terms, resultDomain)
  }

  def apply(terms: Vector[Term]): TermFunction = {
    require(terms.size == 1)
    new TermFunction("prec", terms)
  }

  def apply(term: Term, resultDomain: String): TermFunction = {
    new TermFunction("prec", Vector(term), resultDomain)
  }

  def apply(term: Term): TermFunction = {
    new TermFunction("prec", Vector(term))
  }
}

final class DynPlusFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("plus", 2)

  def resultFunction: Vector[String] => String =
    (constants: Vector[String]) => (constants.head.toInt + constants.last.toInt).toString

  def apply(terms: Vector[Term], resultDomain: String): TermFunction = {
    require(terms.size == 2)
    new TermFunction("plus", terms, resultDomain)
  }

  def apply(terms: Vector[Term]): TermFunction = {
    require(terms.size == 2)
    new TermFunction("plus", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String): TermFunction = {
    new TermFunction("plus", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term): TermFunction = {
    new TermFunction("plus", Vector(term, term2))
  }
}

final class DynMinusFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("minus", 2)

  def resultFunction: Vector[String] => String =
    (constants: Vector[String]) => (constants.head.toInt - constants.last.toInt).toString

  def apply(terms: Vector[Term], resultDomain: String): TermFunction = {
    require(terms.size == 2)
    new TermFunction("minus", terms, resultDomain)
  }

  def apply(terms: Vector[Term]): TermFunction = {
    require(terms.size == 2)
    new TermFunction("minus", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String): TermFunction = {
    new TermFunction("minus", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term): TermFunction = {
    new TermFunction("minus", Vector(term, term2))
  }
}

final class DynTimesFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("times", 2)

  def resultFunction: Vector[String] => String =
    (constants: Vector[String]) => (constants.head.toInt * constants.last.toInt).toString

  def apply(terms: Vector[Term], resultDomain: String): TermFunction = {
    require(terms.size == 2)
    new TermFunction("times", terms, resultDomain)
  }

  def apply(terms: Vector[Term]): TermFunction = {
    require(terms.size == 2)
    new TermFunction("times", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String): TermFunction = {
    new TermFunction("times", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term): TermFunction = {
    new TermFunction("times", Vector(term, term2))
  }
}

final class DynDividedByFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("dividedBy", 2)

  def resultFunction: Vector[String] => String =
    (constants: Vector[String]) => (constants.head.toInt / constants.last.toInt).toString

  def apply(terms: Vector[Term], resultDomain: String): TermFunction = {
    require(terms.size == 2)
    new TermFunction("dividedBy", terms, resultDomain)
  }

  def apply(terms: Vector[Term]): TermFunction = {
    require(terms.size == 2)
    new TermFunction("dividedBy", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String): TermFunction = {
    new TermFunction("dividedBy", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term): TermFunction = {
    new TermFunction("dividedBy", Vector(term, term2))
  }
}

final class DynModFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("mod", 2)

  def resultFunction: Vector[String] => String =
    (constants: Vector[String]) => (constants.head.toInt % constants.last.toInt).toString

  def apply(terms: Vector[Term], resultDomain: String): TermFunction = {
    require(terms.size == 2)
    new TermFunction("mod", terms, resultDomain)
  }

  def apply(terms: Vector[Term]): TermFunction = {
    require(terms.size == 2)
    new TermFunction("mod", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String): TermFunction = {
    new TermFunction("mod", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term): TermFunction = {
    new TermFunction("mod", Vector(term, term2))
  }
}

final class DynConcatFunctionBuilder extends DynamicFunctionBuilder {

  def signature = AtomSignature("concat", 2)

  def resultFunction: Vector[String] => String =
    (constants: Vector[String]) => constants.head.concat(constants.last)

  def apply(terms: Vector[Term], resultDomain: String): TermFunction = {
    require(terms.size == 2)
    new TermFunction("concat", terms, resultDomain)
  }

  def apply(terms: Vector[Term]): TermFunction = {
    require(terms.size == 2)
    new TermFunction("concat", terms)
  }

  def apply(term: Term, term2: Term, resultDomain: String): TermFunction = {
    new TermFunction("concat", Vector(term, term2), resultDomain)
  }

  def apply(term: Term, term2: Term): TermFunction = {
    new TermFunction("concat", Vector(term, term2))
  }
}
