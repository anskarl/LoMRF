package lomrf.mln.learning.structure

import scala.util.parsing.combinator.{RegexParsers, JavaTokenParsers}

/**
 * Regular expressions for mode declaration parser
 */
trait CommonModeParser extends JavaTokenParsers with RegexParsers {

  val recall = """([0]|[1-9][0-9]*|\*)""".r
  val predicate = """([A-Z0-9]([a-zA-Z0-9]|_[a-zA-Z0-9])*)""".r
  val function = """([a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*)""".r
  val placemarker = """(#)?(\+|-|\.)""".r

  override val whiteSpace = """(\s|//.*\n|(/\*(?:.|[\n\r])*?\*/))+""".r
}
