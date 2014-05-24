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
 * Copyright (C) 2012  Anastasios Skarlatidis.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package lomrf.logic

import util.parsing.combinator.{RegexParsers, JavaTokenParsers}

/**
 * @author Anastasios Skarlatidis
 */

trait CommonsMLNParser extends JavaTokenParsers with RegexParsers {
  val functionID = """[a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*\\([\w_\-,]+\\)""".r

  val lowerCaseID = """([a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*)""".r
  val upperCaseID = """([A-Z0-9]([a-zA-Z0-9]|_[a-zA-Z0-9])*)""".r
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