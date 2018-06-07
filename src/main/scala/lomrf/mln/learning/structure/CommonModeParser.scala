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

package lomrf.mln.learning.structure

import scala.util.matching.Regex
import scala.util.parsing.combinator.{ JavaTokenParsers, RegexParsers }

/**
  * Regular expressions for mode declaration parser
  */
trait CommonModeParser extends JavaTokenParsers with RegexParsers {

  protected val arity: Regex = """([1-9][0-9]*)""".r
  protected val recall: Regex = """([0]|[1-9][0-9]*|\*)""".r
  protected val predicate: Regex = """([A-Z0-9]([a-zA-Z0-9]|_[a-zA-Z0-9])*)""".r
  protected val function: Regex = """([a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*)""".r
  protected val placeMarker: Regex = """(#)?(n)?(\+|-|\.)""".r

  protected override val whiteSpace: Regex = """(\s|//.*\n|(/\*(?:.|[\n\r])*?\*/))+""".r
}
