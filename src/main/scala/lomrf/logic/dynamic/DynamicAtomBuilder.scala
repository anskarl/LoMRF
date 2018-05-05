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

import lomrf.logic._

/**
  * Dynamic atom builder should be extended by any dynamic atom
  * type in order to define its functionality.
  */
trait DynamicAtomBuilder {
  def signature: AtomSignature

  def stateFunction: Vector[String] => Boolean

  def apply(terms: Vector[Term]): DynamicAtom

  def apply(terms: Term*): DynamicAtom = apply(terms.toVector)
}

/**
  * An infix dynamic atom.
  *
  * @param prefixSymbol atom symbol
  * @param infixSymbol the infix symbol used for syntactic sugar
  * @param terms dynamic atom arguments, i.e., constants, variables or functions
  */
private[dynamic] final class DynInfix(
    prefixSymbol: String,
    infixSymbol: String,
    terms: Vector[Term]) extends DynamicAtom(prefixSymbol, terms) {

  require(terms.size == 2)

  override def toText: String = terms.head.toText + " " + infixSymbol + " " + terms.last.toText
}

// -- Various dynamic atom builders:

final class DynEqualsBuilder extends DynamicAtomBuilder {

  override def signature = AtomSignature("equals", 2)

  override def stateFunction: Vector[String] => Boolean =
    (constants: Vector[String]) => constants.head == constants.last

  override def apply(terms: Vector[Term]) = new DynInfix("equals", "=", terms)
}

final class DynNotEqualsBuilder extends DynamicAtomBuilder {

  override def signature = AtomSignature("notEquals", 2)

  override def stateFunction: Vector[String] => Boolean =
    (constants: Vector[String]) => constants.head == constants.last

  override def apply(terms: Vector[Term]) = new DynInfix("notEquals", "=", terms)
}

final class DynLessThanBuilder extends DynamicAtomBuilder {

  override def signature = AtomSignature("lessThan", 2)

  override def stateFunction: Vector[String] => Boolean =
    (constants: Vector[String]) => constants.head.toInt < constants.last.toInt

  override def apply(terms: Vector[Term]) = new DynInfix("lessThan", "<", terms)
}

final class DynLessThanEqBuilder extends DynamicAtomBuilder {

  override def signature = AtomSignature("lessThanEq", 2)

  override def stateFunction: Vector[String] => Boolean =
    (constants: Vector[String]) => constants.head.toInt <= constants.last.toInt

  override def apply(terms: Vector[Term]) = new DynInfix("lessThanEq", "<=", terms)
}

final class DynGreaterThanBuilder extends DynamicAtomBuilder {

  override def signature = AtomSignature("greaterThan", 2)

  override def stateFunction: Vector[String] => Boolean =
    (constants: Vector[String]) => constants.head.toInt > constants.last.toInt

  override def apply(terms: Vector[Term]) = new DynInfix("greaterThan", ">", terms)
}

final class DynGreaterThanEqBuilder extends DynamicAtomBuilder {

  override def signature = AtomSignature("greaterThanEq", 2)

  override def stateFunction: Vector[String] => Boolean =
    (constants: Vector[String]) => constants.head.toInt >= constants.last.toInt

  override def apply(terms: Vector[Term]) = new DynInfix("greaterThanEq", ">=", terms)
}

final class DynSubstringBuilder extends DynamicAtomBuilder {
  override def signature = AtomSignature("substr", 2)

  override def stateFunction: Vector[String] => Boolean =
    (constants: Vector[String]) => constants.last.contains(constants.head)

  override def apply(terms: Vector[Term]): DynamicAtom = {
    require(terms.size == 2)
    new DynamicAtom("substr", terms)
  }
}
