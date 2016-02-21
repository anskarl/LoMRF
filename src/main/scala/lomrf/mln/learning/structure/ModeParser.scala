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
 * Logical Markov Random Fields.
 *
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */

package lomrf.mln.learning.structure

import java.io.{FileReader, BufferedReader, File}
import auxlib.log.{Logger, Logging}
import lomrf.logic.AtomSignature
import lomrf.mln.model.ModeDeclarations

/**
 * Parser for mode declarations.
 *
 * For example mode(recall, PredicateName(#placemarkers, ...)) where
 * placemarkers are either + (input), - (output) or . (ignore). The symbol #
 * is optional and denotes that the specific placemarker is constant.
 */
class ModeParser extends CommonModeParser with Logging {

  def modeDeclarations: Parser[ModeDeclarations] = rep(mode) ^^ (Map() ++ _)

  private def mode: Parser[(AtomSignature, ModeDeclaration)] = ( "modeP" ~ "(" ~ recall ~ "," ~ predicate | "modeF" ~ "(" ~ recall ~ "," ~ function ) ~ "(" ~ repsep(placemarker, ",") ~ ")" ~ ")" ^^ {

    case "modeP" ~ "(" ~ "*" ~ "," ~ predName ~ "(" ~ values ~ ")" ~ ")" =>
      (AtomSignature(predName, values.length),
        ModeDeclaration(placemarkers = values.map(symbol => PlaceMarker(symbol.contains("+"), symbol.contains("-"), symbol.contains("#"))).toVector))

    case "modeP" ~ "(" ~ limit ~ "," ~ predName ~ "(" ~ values ~ ")" ~ ")" =>
      (AtomSignature(predName, values.length),
        ModeDeclaration(limit.toInt, values.map(symbol => PlaceMarker(symbol.contains("+"), symbol.contains("-"), symbol.contains("#"))).toVector))

    case "modeF" ~ "(" ~ limit ~ "," ~ funcName ~ "(" ~ values ~ ")" ~ ")" => // Function return value is always an input placemarker
      (AtomSignature(lomrf.AUX_PRED_PREFIX + funcName, values.length + 1),
        ModeDeclaration(limit.toInt, (PlaceMarker.input :: values.map(symbol => PlaceMarker(symbol.contains("+"), symbol.contains("-"), symbol.contains("#")))).toVector))
  }

  /**
   * Parses an individual mode declaration from text
   *
   * @param src string representation of the mode declaration
   * @return the resulting atom signature along the mode declaration
   */
  def parseMode(src: String): (AtomSignature, ModeDeclaration) = parse(mode, src) match {
    case Success(expr, _) if expr.isInstanceOf[(AtomSignature, ModeDeclaration)] => expr.asInstanceOf[(AtomSignature, ModeDeclaration)]
    case x => fatal("Can't parse the following expression: " + x)
  }
}

/**
 * Mode parser object used to parse mode declarations from source. Source could
 * be a file or a iterable of strings.
 */
object ModeParser {

  private lazy val parser = new ModeParser
  private lazy val log = Logger(this.getClass)
  import log._

  def parseFrom(sources: Iterable[String]): ModeDeclarations = sources.map(parser.parseMode).toMap

  def parseFrom(modesFileName: String): ModeDeclarations = {

    val modeFile = new File(modesFileName)
    val fileReader = new BufferedReader(new FileReader(modeFile))

    parser.parseAll(parser.modeDeclarations, fileReader) match {
      case parser.Success(expr, _) => expr.asInstanceOf[ModeDeclarations]
      case x => fatal("Can't parse the following expression: " + x +" in file: " + modeFile.getPath)
    }
  }

}

/**
 * Defines the mode declaration for a specific predicate schema (atom signature).
 *
 * @param recall limits the number of appearances in a clause
 * @param placemarkers vector of modes for each variable in the predicate
 */
final case class ModeDeclaration(recall: Int = Int.MaxValue, placemarkers: Vector[PlaceMarker]) {
  override def toString = "mode(" + (if(recall == Int.MaxValue) "*" else recall) + "," + placemarkers.mkString(",") + ")"
}

/**
 * Defines the mode of a specific variable.
 *
 * @param input is input variable
 * @param output is output variable
 * @param constant the variable would be constant (forbids varabilization)
 */
final case class PlaceMarker(input: Boolean, output: Boolean, constant: Boolean) {

  def isInputOrOutput: Boolean = input || output

  def isInputOnly: Boolean = input && !output

  def isOutputOnly: Boolean = !input && output

  override def toString =
    if(input && output && constant) "#+-"
    else if(!input && !output && constant) "#."
    else if(input && output) "+-"
    else if(input && constant) "#+"
    else if(output && constant) "#-"
    else if(input) "+"
    else if(output) "-"
    else "."
}

object PlaceMarker {

  /**
   * @return input placemarker
   */
  def input = new PlaceMarker(true, false, false)

  /**
   * @return output placemarker
   */
  def output = new PlaceMarker(false, true, false)

  /**
   * @return ignore placemarker
   */
  def ignore = new PlaceMarker(false, false, false)
}
