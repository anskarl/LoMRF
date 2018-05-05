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

import lomrf.logic.{ Term, AtomicFormula }

/**
  * Represents a dynamic atom. Dynamic atoms are atomic formulas whose
  * state is evaluated dynamically according to a state function.
  *
  * @see [[lomrf.logic.Term]]
  *
  * @param symbol dynamic atom symbol
  * @param terms dynamic atom arguments, i.e., constants, variables or functions
  */
class DynamicAtom(override val symbol: String, override val terms: Vector[Term]) extends AtomicFormula(symbol, terms) {
  override val isDynamic = true
}

object DynamicAtom {
  def apply(symbol: String, terms: Vector[Term]) = new DynamicAtom(symbol, terms)

  def unapply(obj: DynamicAtom): Option[(String, List[Term])] = obj match {
    case DynamicAtom(symbol, terms) => Some((symbol, terms))
    case _                          => None
  }
}
