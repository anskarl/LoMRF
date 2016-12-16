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

import scala.util.parsing.combinator.{RegexParsers, JavaTokenParsers}

/**
 * Regular expressions for mode declaration parser
 */
trait CommonModeParser extends JavaTokenParsers with RegexParsers {

  protected val arity =  """([1-9][0-9]*)""".r
  protected val recall = """([0]|[1-9][0-9]*|\*)""".r
  protected val predicate = """([A-Z0-9]([a-zA-Z0-9]|_[a-zA-Z0-9])*)""".r
  protected val function = """([a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*)""".r
  protected val placeMarker = """(#)?(\+|-|\.)""".r

  protected override val whiteSpace = """(\s|//.*\n|(/\*(?:.|[\n\r])*?\*/))+""".r
}
