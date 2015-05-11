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

package lomrf.mln.model

import lomrf.logic.{TRUE, Constant, EvidenceAtom, AtomSignature}
import lomrf.util.ConstantsSet
import org.scalatest.{Matchers, FunSpec}

class EvidenceBuilderSpecTest extends FunSpec with Matchers {

  private val samplePredicateSchemas = Seq(
    ("HoldsAt", Vector("fluent", "time")),
    ("HappensAt", Vector("event", "time")),
    ("InitiatedAt", Vector("fluent", "time")),
    ("TerminatedAt", Vector("fluent", "time"))
  ).map(schema => AtomSignature(schema._1, schema._2.size) -> schema._2).toMap

  private val queryPredicates = Set(AtomSignature("HoldsAt", 2))
  private val hiddenPredicates = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt",2))

  private val constantsDomain = Seq("time", "fluent", "event").map(s=> s -> ConstantsSet()).toMap



  // --------------------------------------------------------
  // --- Evidence Builder (empty)
  // --------------------------------------------------------
  describe("An empty EvidenceBuilder should produce an empty evidence") {
    val builder = EvidenceBuilder(samplePredicateSchemas, queryPredicates, hiddenPredicates, constantsDomain)

    val result = builder.result()

    it("should contain an empty database of evidence atoms"){
      assert(result.constants.isEmpty)
    }

    it("should contain an empty collection of function mappings"){
      assert(result.functionMappers.isEmpty)
    }

  }

  // --------------------------------------------------------
  // --- Evidence Builder (incremental addition of evidence)
  // --------------------------------------------------------
  describe("Incremental addition of evidence atoms using EvidenceBuilder"){
    val SIZE = 10
    val PRED_NAME = "HappensAt"
    val SIGNATURE = AtomSignature(PRED_NAME, 2)

    val builder = EvidenceBuilder(samplePredicateSchemas, queryPredicates, hiddenPredicates, constantsDomain)

    var inserted = List.empty[EvidenceAtom]

    for(timepoint <- 1 to SIZE) {
      val atom = EvidenceAtom.asTrue(PRED_NAME, Vector[Constant](Constant("walking"), Constant(timepoint.toString)))
      inserted = atom :: inserted
      builder.evidence += atom
    }

    val resultDB = builder.result().db

    it(s"should contain $SIZE evidence predicates of $PRED_NAME"){
      assert(resultDB(SIGNATURE).numberOfTrue == SIZE)
    }

    it(s"should contain all the inserted evidence predicates with the correct truth state"){

      assert{
        inserted.forall{atom =>
          val args = atom.terms.map(_.symbol)
          resultDB(SIGNATURE).contains(args) && resultDB(SIGNATURE)(args) == TRUE
        }
      }
    }

  }


}
