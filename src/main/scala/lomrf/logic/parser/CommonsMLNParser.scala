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

package lomrf.logic.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.{ JavaTokenParsers, RegexParsers }

/**
  * Contains common regular expressions for parsing.
  */
trait CommonsMLNParser extends JavaTokenParsers with RegexParsers {

  protected val functionID: Regex = """[a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*\\([\w_\-,]+\\)""".r

  protected val lowerCaseID: Regex = """([a-z][_a-zA-Z0-9]*)""".r
  protected val upperCaseID: Regex = """-?([_A-Z0-9][_a-zA-Z0-9]*)""".r

  protected val variableArg: Regex = """(\+?([a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*))""".r

  protected val constOrVar: Regex = """[a-zA-Z0-9]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r

  protected val numDouble: Regex = "-?\\d+(\\.\\d+)?".r
  protected val numPosInt: Regex = "\\d+".r
  protected val exist: Regex = "Exist|EXIST".r
  protected val forall: Regex = "Forall|FORALL".r
  protected val quote: Regex = "\"".r
  protected val includeID: Regex = """.+\.mln""".r

  protected override val whiteSpace: Regex = """(\s|//.*\n|(/\*(?:.|[\n\r])*?\*/))+""".r
}
