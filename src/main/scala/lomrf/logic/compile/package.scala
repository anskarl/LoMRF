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

import enumeratum._
import scala.collection.immutable

package object compile {

  sealed trait PredicateCompletionMode extends EnumEntry

  /**
    * Choose the type of predicate completion:
    * <ul>
    *   <li> Standard: standard predicate completion.</li>
    *   <li> Decomposed: computes predicate completion and decomposes the created equivalences into two implications.</li>
    *   <li> Simplification: computes predicate completion and simplifies the formulas based on the created equivalences.</li>
    * </ul>
    *
    * @see [[lomrf.logic.compile.PredicateCompletion]]
    */
  object PredicateCompletionMode extends Enum[PredicateCompletionMode] {

    val values: immutable.IndexedSeq[PredicateCompletionMode] = findValues

    case object Standard extends PredicateCompletionMode
    case object Decomposed extends PredicateCompletionMode
    case object Simplification extends PredicateCompletionMode
  }
}
