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

package lomrf.logic

/**
  * Enables substitution operation on an expression type. Substitution is a
  * fundamental logical operation, a syntactic transformation on formal expressions
  * that replaces variables, placeholders and symbols by other expressions.
  *
  * @tparam T an expression type
  */
trait Substitutable[T] {

  /**
    * Apply a theta substitution to the expression. Theta is a mapping of
    * logical terms to be substituted. The resulting expression is a substitution
    * instance of the original expression type.
    *
    * @param theta a given mapping of logical terms
    * @return a substitution instance of the original expression type
    */
  def substitute(theta: Theta): T
}
