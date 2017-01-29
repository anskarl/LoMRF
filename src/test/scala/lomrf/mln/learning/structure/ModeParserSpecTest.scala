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

import lomrf.logic.AtomSignature
import org.scalatest.{Matchers, FunSpec}

/**
 * Specification test for mode declaration parser.
 */
final class ModeParserSpecTest extends FunSpec with Matchers {

  val modeParser = new ModeParser

  // List of mode declarations to be parsed along with an annotation of the results
  val modeList = List (
    ("modeP(2, A(#-,+,.))", AtomSignature("A", 3), 2, Vector( (false, true, true), (true, false, false), (false, false, false) )),
    ("modeP(4, B(.,+,+))", AtomSignature("B", 3), 4, Vector( (false, false, false), (true, false, false), (true, false, false) )),
    ("modeP(1, C(-,-))", AtomSignature("C", 2), 1, Vector( (false, true, false), (false, true, false) )),
    ("modeP(91, D(-,+))", AtomSignature("D", 2), 91, Vector( (false, true, false), (true, false, false) )),
    ("modeP(7, E(#+))", AtomSignature("E", 1), 7, Vector( (true, false, true) )),
    ("modeP(0, F(+))", AtomSignature("F", 1), 0, Vector( (true, false, false) )),
    ("modeP(21, G(-))", AtomSignature("G", 1), 21, Vector( (false, true, false) )),
    ("modeP(67, H(.,+))", AtomSignature("H", 2), 67, Vector( (false, false, false), (true, false, false) )),
    ("modeP(9, I(.,-))", AtomSignature("I", 2), 9, Vector( (false, false, false), (false, true, false) )),
    ("modeP(*, J(#.,+))", AtomSignature("J", 2), Int.MaxValue, Vector( (false, false, true), (true, false, false) )),
    ("modeP(5, K(.,+))", AtomSignature("K", 2), 5, Vector( (false, false, false), (true, false, false) )),
    ("modeP(1, L(.,.))", AtomSignature("L", 2), 1, Vector( (false, false, false), (false, false, false) )),
    ("modeP(*, M(+,+))", AtomSignature("M", 2), Int.MaxValue, Vector( (true, false, false), (true, false, false) )),
    ("modeP(2, N(#-,#.,#+))", AtomSignature("N", 3), 2, Vector( (false, true, true), (false, false, true), (true, false, true) )),
    ("modeP(102, O(+,#-,#+))", AtomSignature("O", 3), 102, Vector( (true, false, false), (false, true, true), (true, false, true) )),
    ("modeF(7, foo(-,.))", AtomSignature(lomrf.AUX_PRED_PREFIX + "foo", 3), 7, Vector( (true, false, false), (false, true, false), (false, false, false) )),
    ("modeF(0, bar(-))", AtomSignature(lomrf.AUX_PRED_PREFIX + "bar", 2), 0, Vector( (true, false, false), (false, true, false) ))
  )

  // For each mode declaration string, parse it and then check if everything is OK
  for( (source, signature, recall, values) <- modeList) describe("Mode declaration '" + source + "'") {

    val (parsedSignature, mode) = modeParser.parseMode(source)

    it(s"Signature should be equal to $signature") {
      parsedSignature shouldEqual signature
    }

    it(s"Recall should be equal to $recall") {
      mode.recall shouldEqual recall
    }

    it("All placemarkers should be valid") {
      val placemarkers = mode.placemarkers
      for(i <- placemarkers.indices) {
        val (isInput, isOutput, isConstant) = values(i)
        placemarkers(i).input shouldBe isInput
        placemarkers(i).output shouldBe isOutput
        placemarkers(i).constant shouldBe isConstant
      }
    }

  }
}
