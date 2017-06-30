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

package lomrf.logic.parser

import lomrf.logic.{Constant, EvidenceAtom, FunctionMapping}
import org.scalatest.{FunSpec, Matchers}

final class EvidenceParserSpecTest extends FunSpec with Matchers {

  val evidenceParser = new EvidenceParser

  describe("Function mappings") {

    val meet = "Meet_ID1_ID2 = meet(ID1, ID2)"

    it(s"$meet should be a valid function mapping definition") {
      evidenceParser.parseFunctionMapping(meet) shouldEqual
        new FunctionMapping("Meet_ID1_ID2", "meet", Vector("ID1", "ID2"))
    }
  }

  describe("Evidence atoms") {

    val positiveHappens = "Happens(Active_ID1, 5)"

    it(s"$positiveHappens should be a valid evidence atom definition") {
      evidenceParser.parseEvidenceAtom(positiveHappens) shouldEqual
        EvidenceAtom.asTrue("Happens", Vector("Active_ID1", "5").map(Constant))
    }

    val probabilisticPositiveHappens = "Happens(Active_ID1, 5) 1.0"

    it(s"$probabilisticPositiveHappens should be a valid evidence atom definition") {
      evidenceParser.parseEvidenceAtom(probabilisticPositiveHappens) shouldEqual
        EvidenceAtom.asTrue("Happens", Vector("Active_ID1", "5").map(Constant))
    }

    val positiveUnary = "UnaryPredicate"

    it(s"$positiveUnary should be a valid evidence atom definition") {
      evidenceParser.parseEvidenceAtom(positiveUnary) shouldEqual
        EvidenceAtom.asTrue("UnaryPredicate", Vector.empty[Constant])
    }

    val negatedHappens = "!Happens(Inactive_ID0, 10)"

    it(s"$negatedHappens should be a valid evidence atom definition") {
      evidenceParser.parseEvidenceAtom(negatedHappens) shouldEqual
        EvidenceAtom.asFalse("Happens", Vector("Inactive_ID0", "10").map(Constant))
    }

    val probabilisticNegativeHappens = "Happens(Inactive_ID0, 10) 0.0"

    it(s"$probabilisticNegativeHappens should be a valid evidence atom definition") {
      evidenceParser.parseEvidenceAtom(probabilisticNegativeHappens) shouldEqual
        EvidenceAtom.asFalse("Happens", Vector("Inactive_ID0", "10").map(Constant))
    }

    val negatedUnary = "!UnaryPredicate"

    it(s"$negatedUnary should be a valid evidence atom definition") {
      evidenceParser.parseEvidenceAtom(negatedUnary) shouldEqual
        EvidenceAtom.asFalse("UnaryPredicate", Vector.empty[Constant])
    }

    val unknownHappens = "?Happens(Exit_ID2, 25)"

    it(s"$unknownHappens should be a valid evidence atom definition") {
      evidenceParser.parseEvidenceAtom(unknownHappens) shouldEqual
        EvidenceAtom.asUnknown("Happens", Vector("Exit_ID2", "25").map(Constant))
    }

    val unknownUnary = "?UnaryPredicate"

    it(s"$unknownUnary should be a valid evidence atom definition") {
      evidenceParser.parseEvidenceAtom(unknownUnary) shouldEqual
        EvidenceAtom.asUnknown("UnaryPredicate", Vector.empty[Constant])
    }

    val probabilisticAtom = "Parent(George, Peter) 0.8"

    it(s"$probabilisticAtom is not supported yet and should throw an exception") {
      intercept[UnsupportedOperationException] {
        evidenceParser.parseEvidenceAtom(probabilisticAtom)
      }
    }
  }
}
