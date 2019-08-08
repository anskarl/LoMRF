/*
 *
 *  o                        o     o   o         o
 *  |             o          |     |\ /|         | /
 *  |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 *  |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 *  O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *              |
 *           o--o
 *  o--o              o               o--o       o    o
 *  |   |             |               |    o     |    |
 *  O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 *  |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 *  o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 *  Logical Markov Random Fields (LoMRF).
 *
 *
 */

package lomrf.mln.learning.structure.hypergraph

import lomrf.logic.{ AtomSignature, Constant, EvidenceAtom, TRUE }
import lomrf.mln.learning.structure.ModeParser
import lomrf.mln.model._
import lomrf.mln.model.builder.EvidenceBuilder
import org.scalatest.{ FunSpec, Matchers }

/**
  * Specification test for HyperGraph paths.
  */
final class HPathSpecTest extends FunSpec with Matchers {

  private val DOMAIN_SIZE = 5

  // Predicate schema
  private val predicateSchema = Map(
    AtomSignature("PredicateA", 2) -> Vector("Integer", "Integer"),
    AtomSignature("PredicateB", 2) -> Vector("Integer", "Character"))

  // Constants domain
  private val constantsDomain = Map(
    "Integer" -> ConstantsSet((0 to DOMAIN_SIZE).map(_.toString)),
    "Character" -> ConstantsSet("A", "B", "C", "D", "E"))

  // Evidence Builder
  val builder = EvidenceBuilder(predicateSchema, Set.empty, Set.empty, constantsDomain)

  // Append evidence atoms for 'PredicateA'
  val evidenceForPredicateA = (0 until DOMAIN_SIZE).map { integer =>
    EvidenceAtom.asTrue("PredicateA", Vector[Constant](Constant((integer + 1).toString), Constant(integer.toString)))
  }
  builder.evidence ++= evidenceForPredicateA

  // Append evidence atoms for 'PredicateB'
  builder.evidence += EvidenceAtom.asTrue("PredicateB", Vector[Constant](Constant(0.toString), Constant("A")))
  builder.evidence += EvidenceAtom.asTrue("PredicateB", Vector[Constant](Constant(1.toString), Constant("B")))
  builder.evidence += EvidenceAtom.asTrue("PredicateB", Vector[Constant](Constant(2.toString), Constant("C")))
  builder.evidence += EvidenceAtom.asTrue("PredicateB", Vector[Constant](Constant(3.toString), Constant("D")))
  builder.evidence += EvidenceAtom.asTrue("PredicateB", Vector[Constant](Constant(4.toString), Constant("E")))

  val evidence = builder.result()

  // Create MLN
  val mlnSchema = MLNSchema(predicateSchema, Map.empty, Map.empty, Map.empty)
  val predicateSpace = PredicateSpace(mlnSchema, Set.empty, constantsDomain)

  // Parse mode declarations
  val modes = List("modeP(1, PredicateA(-, +))", "modeP(2, PredicateB(-, #-))")
    .map(ModeParser.parseFrom).toMap

  // Identity functions for each atom signature
  val identities = predicateSpace.identities

  // Collect all atom ids in the evidence database
  val atomIDs = predicateSchema.keys.flatMap { signature =>
    val db = evidence.db(signature)
    val idf = db.identity
    idf.indices filter (id => db.get(id) == TRUE) map (id => id -> signature)
  }.toMap

  it("should exist only 10 true ground atoms") {
    atomIDs.size shouldBe 10
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST CASE: HyperGraph Path (begin from empty path).
  // ------------------------------------------------------------------------------------------------------------------

  describe("HyperGraph Path starting from an empty path.") {

    var path = HPath.empty(modes, identities)

    // Add all ids
    atomIDs.foreach { case (id, signature) => path += (id, signature) }

    it("path should have length 10") {
      path.length shouldBe 10
    }

    it("path should contain all true ground atoms, 2 atom signatures and 10 constants") {
      atomIDs.keys.forall(path.contains) shouldBe true
      predicateSchema.keys.forall(path.contains) shouldBe true
      constantsDomain.values.foreach { set =>
        val iterator = set.valuesIterator
        while (iterator.hasNext) {
          iterator.advance()
          path.contains(iterator.key) shouldBe true
        }
      }
    }

    it("predicate appearance should contain 2 entries each having 5 appearances") {
      path.predicates.size shouldBe 2
      path.predicateAppearance.values.forall(count => count == 5) shouldBe true
    }

    it("constant appearance should contain the correct counts for all constants") {

      constantsDomain("Integer").foreach { value =>
        if (value == "0") path.constantAppearance(value) shouldBe 2
        else if (value == "5") path.constantAppearance(value) shouldBe 1
        else path.constantAppearance(value) shouldBe 3
      }

      constantsDomain("Character").foreach(path.constantAppearance(_) shouldBe 0)
    }

    it("checking path ordering") {
      path.ordering.reverse.map { case (id, signature) => id }.toSet shouldEqual atomIDs.keys
    }
  }
}
