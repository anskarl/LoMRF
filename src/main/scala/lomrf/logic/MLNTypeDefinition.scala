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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.logic

/**
  * Types and constants can be declared in a .kb file having the following syntax:
  * {{{
  *  <typename> =  { <constant1>, <constant2>, ... },
  * }}}
  * @example {{{
  *           person =  { Alice, Bob }
  *           time = {1, ..., 100} // for quickly defining a range of integers
  *          }}}
  * According to the above example definitions, the domain '''person''' is composed of
  * 2 constant symbols (i.e. Alice and Bob). Similarly the domain  '''time''' is composed
  * of the symbols that belong into the range [1, 100].
  *
  * @param name a name for the MLN definition
  */
sealed abstract class MLNTypeDefinition(name: String) extends MLNDomainExpression {

  /**
    * @return the name of the MLN definition
    */
  def getName: String = name
}

/**
  * Integer type definition is defined by a name and an interval of integer literals.
  *
  * @param name a name for the MLN definition
  * @param from the beginning of the interval
  * @param to the end of the interval
  */
case class IntegerTypeDefinition(name: String, from: Int, to: Int) extends MLNTypeDefinition(name)

/**
  * Constant type definition is defined by a name and a sequence of constants.
  *
  * @param name a name for the MLN definition
  * @param constants a sequence of constants
  */
case class ConstantTypeDefinition(name: String, constants: Seq[String]) extends MLNTypeDefinition(name)


