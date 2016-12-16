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
import lomrf.mln.learning.structure.{PlaceMarker => PM}

/**
 * Specification test for the mode declaration parser.
 */
final class ModeParserSpecTest extends FunSpec with Matchers {

  val noIncompatible = Set.empty[AtomSignature]

  // List of mode declarations to be parsed along with an annotation of the results
  val modeList = List (
    ("modeP(2, A(#-, +, .))", AtomSignature("A", 3), 2, Vector(PM.outputConstant, PM.input, PM.ignore), noIncompatible),
    ("modeP(4, B(., +, +))", AtomSignature("B", 3), 4, Vector(PM.ignore, PM.input, PM.input), noIncompatible),
    ("modeP(1, C(-, -))", AtomSignature("C", 2), 1, Vector(PM.output, PM.output), noIncompatible),
    ("modeP(91, D(-, +))", AtomSignature("D", 2), 91, Vector(PM.output, PM.input), noIncompatible),
    ("modeP(7, E(#+) body~/> A/3, D/2)", AtomSignature("E", 1), 7, Vector(PM.inputConstant), Set(AtomSignature("A", 3), AtomSignature("D", 2)) ),
    ("modeP(0, F(+) body~/> foo/2)", AtomSignature("F", 1), 0, Vector(PM.input), Set(AtomSignature(lomrf.AUX_PRED_PREFIX + "foo", 3)) ),
    ("modeP(21, G(-))", AtomSignature("G", 1), 21, Vector(PM.output), noIncompatible),
    ("modeP(67, H(., +))", AtomSignature("H", 2), 67, Vector(PM.ignore, PM.input), noIncompatible),
    ("modeP(9, I(., -))", AtomSignature("I", 2), 9, Vector(PM.ignore, PM.output), noIncompatible),
    ("modeP(*, J(#., +))", AtomSignature("J", 2), Int.MaxValue, Vector(PM.ignoreConstant, PM.input), noIncompatible),
    ("modeP(5, K(., +))", AtomSignature("K", 2), 5, Vector(PM.ignore, PM.input), noIncompatible),
    ("modeP(1, L(., .))", AtomSignature("L", 2), 1, Vector(PM.ignore, PM.ignore), noIncompatible),
    ("modeP(*, M(+, +))", AtomSignature("M", 2), Int.MaxValue, Vector(PM.input, PM.input), noIncompatible),
    ("modeP(2, N(#-, #., #+))", AtomSignature("N", 3), 2, Vector(PM.outputConstant, PM.ignoreConstant, PM.inputConstant), noIncompatible),
    ("modeP(102, O(+, #-, #+))", AtomSignature("O", 3), 102, Vector(PM.input, PM.outputConstant, PM.inputConstant), noIncompatible),
    ("modeF(7, foo(-, .))", AtomSignature(lomrf.AUX_PRED_PREFIX + "foo", 3), 7, Vector(PM.input, PM.output, PM.ignore), noIncompatible),
    ("modeF(*, bar(-))", AtomSignature(lomrf.AUX_PRED_PREFIX + "bar", 2), Int.MaxValue, Vector(PM.input, PM.output), noIncompatible)
  )

  // For each mode declaration string, parse it and then check if everything is OK
  for( (source, signature, recall, values, incompatible) <- modeList) describe(s"Mode declaration '$source'") {

    val (parsedSignature, mode) = ModeParser.parseFrom(source)

    it(s"Signature should be equal to $signature") {
      parsedSignature shouldEqual signature
    }

    it(s"Recall should be equal to $recall") {
      mode.recall shouldEqual recall
    }

    it("All place markers should be valid") {
      for {
        (a, b) <- mode.placeMarkers zip values
      } a shouldEqual b
    }

    it(s"Incompatible atom signatures should be [${incompatible.mkString(" ")}]") {
       incompatible shouldEqual mode.incompatibleSignatures
    }
  }

}
