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

import lomrf.logic.AtomSignature
import org.scalatest.{Matchers, FunSpec}

class KBBuilderSpecTest extends FunSpec with Matchers {

  // ----------------------------------------------
  // --- EMPTY KB
  // ----------------------------------------------

  describe("The KB from an empty KBBuilder") {
    val builder = KBBuilder()

    val kb = builder.result()

    it("should be composed of an empty Map of constant builders") {
      assert(kb.constants.isEmpty)
    }

    it("should be composed of an empty Set of formulas") {
      assert(kb.formulas.isEmpty)
    }

    it("should be composed of an empty Map of predicate schema") {
      assert(kb.predicateSchema.isEmpty)
    }

    it("should be composed of an empty Map of function schema") {
      assert(kb.functionSchema.isEmpty)
    }

    it("should be composed of an empty Map of dynamic predicates") {
      assert(kb.dynamicPredicates.isEmpty)
    }

    it("should be composed of an empty Map of dynamic functions") {
      assert(kb.dynamicFunctions.isEmpty)
    }

  }


  // ----------------------------------------------
  // --- KB Builder (insertion of constant symbols)
  // ----------------------------------------------

  describe("KBBuilder insertion of constant symbols") {

    describe("Incremental addition of domain 'time' with constants from 1 to 10") {
      val builder = KBBuilder()

      for (timePoint <- 1 to 10)
        builder.constants += ("time", timePoint.toString)


      it("contain 10 constant symbols for domain 'time'"){
        builder.constants("time").size shouldEqual 10
      }

      it("contain 10 constant symbols for domain 'time', after re-adding the same constant symbols") {
        for (timePoint <- 1 to 10)
          builder.constants += ("time", timePoint.toString)
      }

      it("contains all the incrementally inserted constants [1, 10]") {
        val kb = builder.result()

        val constants = kb.constants.mapValues(_.result())

        assert((1 to 10).forall(t => constants("time").contains(t.toString)))
      }
    }

    describe("Batch addition of domain 'time' with constants from 1 to 10") {
      val builder = KBBuilder()

      builder.constants ++= ("time", (1 to 10).map(_.toString))

      it("contains 10 constant symbols for domain 'time'") {
        builder.constants("time").size shouldEqual 10
      }

      it("contain 10 constant symbols for domain 'time', after re-adding the same constant symbols") {
        builder.constants ++= ("time", (1 to 10).map(_.toString))
      }

      it("contains all the batched inserted constants [1, 10]") {

        val kb = builder.result()

        val constants = kb.constants.mapValues(_.result())

        assert((1 to 10).forall(t => constants("time").contains(t.toString)))
      }
    }

    describe("Incremental addition of domains 'time' and 'person'") {
      val builder = KBBuilder()

      for (timePoint <- 1 to 10)
        builder.constants += ("time", timePoint.toString)

      for(person <- List("Agamemnon", "Odysseus", "Achilles", "Menelaus"))
        builder.constants += ("person", person)


      it("contain 10 constant symbols for domain 'time'") {
        builder.constants("time").size shouldEqual 10
      }

      it("contains 4 constant symbols for domain 'person'") {
        builder.constants("person").size shouldEqual 4
      }

      it("contain 10 constant symbols for domain 'time', after re-adding the same constant symbols") {
        for (timePoint <- 1 to 10)
          builder.constants += ("time", timePoint.toString)
      }

      it("contain 4 constant symbols for domain 'person', after re-adding the same constant symbols") {
        builder.constants ++= ("person", List("Agamemnon", "Odysseus", "Achilles", "Menelaus"))
      }

      it("contains all the incrementally inserted constants for both 'time' and 'person' domains") {
        val kb = builder.result()

        val constants = kb.constants.mapValues(_.result())

        assert((1 to 10).forall(t => constants("time").contains(t.toString)))
        assert(List("Agamemnon", "Odysseus", "Achilles", "Menelaus").forall(p => constants("person").contains(p)))
      }
    }

    describe("Batch addition of domains 'time' and 'person'") {
      val builder = KBBuilder()

      builder.constants ++= ("time", (1 to 10).map(_.toString))
      builder.constants ++= ("person", List("Agamemnon", "Odysseus", "Achilles", "Menelaus"))

      it("contains 10 constant symbols for domain 'time'") {
        builder.constants("time").size shouldEqual 10
      }

      it("contains 4 constant symbols for domain 'person'") {
        builder.constants("person").size shouldEqual 4
      }

      it("contain 10 constant symbols for domain 'time', after re-adding the same constant symbols") {
        builder.constants ++= ("time", (1 to 10).map(_.toString))
      }

      it("contain 4 constant symbols for domain 'person', after re-adding the same constant symbols") {
        builder.constants ++= ("person", List("Agamemnon", "Odysseus", "Achilles", "Menelaus"))
      }

      it("contains all the batched inserted constants for both 'time' and 'person' domains") {

        val kb = builder.result()

        val constants = kb.constants.mapValues(_.result())

        assert((1 to 10).forall(t => constants("time").contains(t.toString)))
        assert(List("Agamemnon", "Odysseus", "Achilles", "Menelaus").forall(p => constants("person").contains(p)))
      }
    }

    describe("Creation two knowledge bases (KB1 and KB2), using the same KB builder") {
      val builder = KBBuilder()

      // Create the first KB, having time domain with symbols 1 to 10
      builder.constants ++= ("time", (1 to 10).map(_.toString))
      val kb1 = builder.result()
      val kb1TimeSize = kb1.constants("time").size
      val kb1ConstantsSet = kb1.constants.map{case (k, v) => k -> v.result()}

      // Thereafter, create the second KB by adding the new constant symbols (100 to 1000)
      builder.constants ++= ("time", (100 to 1000).map(_.toString))
      val kb2 = builder.result()
      val kb2TimeSize = kb2.constants("time").size
      val kb2ConstantsSet = kb2.constants.map{case (k, v) => k -> v.result()}

      // As a result, the KB1 should contain only the first batch of symbols (total 10)
      val kb1TimeTotal = 10
      // While the KB2, should contain the same and the additional symbols (total 10 + 901)
      val kb2TimeTotal = 911

      it(s"KB1 contains $kb1TimeTotal symbols in domain 'time'") {
        kb1TimeSize shouldEqual kb1TimeTotal
        kb1TimeSize shouldEqual kb1ConstantsSet("time").size
      }

      it(s"KB2 contains $kb2TimeTotal symbols in domain 'time'") {
        kb2TimeSize shouldEqual kb2TimeTotal
        kb2TimeSize shouldEqual kb2ConstantsSet("time").size
      }

      it(s"KB2 contains all KB1 symbols in domain 'time'") {
        assert(kb1ConstantsSet("time").forall(kb2ConstantsSet("time").contains))
      }
      it(s"KB1 does not contain all KB2 symbols in domain 'time'") {
        assert(!kb2ConstantsSet("time").forall(kb1ConstantsSet("time").contains))
      }
    }

  }

  // ----------------------------------------------
  // --- KB Builder (insertion of predicate schema)
  // ----------------------------------------------

  describe("KBBuilder insertion of predicate schemas"){

    val samplePredicateSchemas = List(
      ("HoldsAt", Vector("fluent", "time")),
      ("HappensAt", Vector("event", "time")),
      ("InitiatedAt", Vector("fluent", "time")),
      ("TerminatedAt", Vector("fluent", "time")),
      ("Initiates", Vector("event","fluent", "time")),
      ("Terminates", Vector("event","fluent", "time"))
    ).map(schema => AtomSignature(schema._1, schema._2.size) -> schema._2)

    describe("Incremental addition of predicate schemas") {
      val builder = KBBuilder()

      for (schema <- samplePredicateSchemas)
        builder.predicateSchema += schema


      it(s"contains ${samplePredicateSchemas.size} predicate schema definitions"){
        builder.predicateSchema().size shouldEqual samplePredicateSchemas.size
      }

      it(s"contains ${samplePredicateSchemas.size} predicate schema definitions, after re-adding the same schema definitions") {
        for (schema <- samplePredicateSchemas)
          builder.predicateSchema += schema
      }

      it("contains all the incrementally inserted schema definitions") {
        val kb = builder.result()

        val predicateSchema = kb.predicateSchema

        assert(samplePredicateSchemas.forall(schema => predicateSchema(schema._1) == schema._2))
      }
    }

    describe("Batch addition of predicate schemas") {
      val builder = KBBuilder()

      builder.predicateSchema ++= samplePredicateSchemas


      it(s"contains ${samplePredicateSchemas.size} predicate schema definitions"){
        builder.predicateSchema().size shouldEqual samplePredicateSchemas.size
      }

      it(s"contains ${samplePredicateSchemas.size} predicate schema definitions, after re-adding the same schema definitions") {
        builder.predicateSchema ++= samplePredicateSchemas
      }

      it("contains all the batched inserted schema definitions") {
        val kb = builder.result()

        val predicateSchema = kb.predicateSchema

        assert(samplePredicateSchemas.forall(schema => predicateSchema(schema._1) == schema._2))
      }
    }
  }
}
