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

package lomrf.logic

import util.parsing.combinator.{RegexParsers, JavaTokenParsers}

/**
 * Contains common regular expressions for parsing
 */

trait CommonsMLNParser extends JavaTokenParsers with RegexParsers {
  val functionID = """[a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*\\([\w_\-,]+\\)""".r

  val lowerCaseID = """([a-z][_a-zA-Z0-9]*)""".r

  val variableArg = """(\+?([a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*))""".r


  val upperCaseID = """-?([_A-Z0-9][_a-zA-Z0-9]*)""".r
  val constOrVar = """[a-zA-Z0-9]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r

  val numDouble = "-?\\d+(\\.\\d+)?".r
  val numPosInt = "\\d+".r
  val exist = "Exist|EXIST".r
  val forall = "Forall|FORALL".r
  val quote = "\"".r
  val includeID = """.+\.mln""".r
  val dot = ".".r

  override val whiteSpace = """(\s|//.*\n|(/\*(?:.|[\n\r])*?\*/))+""".r


}
