package lomrf.mln.learning.structure.hypergraph

import lomrf.logic.{AtomSignature, Constant, EvidenceAtom, TRUE}
import lomrf.mln.learning.structure.ModeParser
import lomrf.mln.model._
import org.scalatest.{FunSpec, Matchers}

/**
 * Specification test for hypergraph paths.
 */
final class HPathSpecTest extends FunSpec with Matchers {

  private val DOMAIN_SIZE = 5

  // Predicate schema
  private val predicateSchema = Map(
    AtomSignature("PredA", 2) -> Vector("domA", "domA"),
    AtomSignature("PredB", 2) -> Vector("domA", "domB"))

  // Constants domain
  private val constantsDomain = Map(
    "domA" -> ConstantsSet((0 to DOMAIN_SIZE).map(_.toString)),
    "domB" -> ConstantsSet("A", "B", "C", "D", "E"))


  val builder = EvidenceBuilder(predicateSchema, Set.empty, Set.empty, constantsDomain)

  // Append evidence atoms for predicate A
  val evidenceForPredA = (0 until DOMAIN_SIZE).map { timepoint =>
    EvidenceAtom.asTrue("PredA", Vector[Constant](Constant((timepoint + 1).toString), Constant(timepoint.toString)))
  }.toSeq

  builder.evidence ++= evidenceForPredA

  // Append evidence atoms for predicate B
  builder.evidence += EvidenceAtom.asTrue("PredB", Vector[Constant](Constant(0.toString), Constant("A")))
  builder.evidence += EvidenceAtom.asTrue("PredB", Vector[Constant](Constant(1.toString), Constant("B")))
  builder.evidence += EvidenceAtom.asTrue("PredB", Vector[Constant](Constant(2.toString), Constant("C")))
  builder.evidence += EvidenceAtom.asTrue("PredB", Vector[Constant](Constant(3.toString), Constant("D")))
  builder.evidence += EvidenceAtom.asTrue("PredB", Vector[Constant](Constant(4.toString), Constant("E")))

  val evidence = builder.result()

  // Create MLN
  val mlnSchema = MLNSchema(predicateSchema, Map.empty, Map.empty, Map.empty)
  val predicateSpace = PredicateSpace(mlnSchema, Set.empty, constantsDomain)

  // Parse mode declarations
  val modeParser = new ModeParser
  val modes = List("modeP(1, PredA(-,+))", "modeP(2, PredB(-,#-))").map(entry => modeParser.parseMode(entry)).toMap

  // Identity functions for each atom signature
  val identities = predicateSpace.identities

  // Collect all atom ids in the evidence database
  val atomIDs = predicateSchema.keys.flatMap { signature =>
    val db = evidence.db(signature)
    val idf = db.identity
    idf.indices filter (id => db.get(id) == TRUE) map ( id => id -> signature)
  }.toMap

  it("should exist only 10 true ground atoms") {
    atomIDs.size shouldBe 10
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Hypergraph Path (begin from empty path).
  // ------------------------------------------------------------------------------------------------------------------

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
      while(iterator.hasNext) {
        iterator.advance()
        path.contains(iterator.key) shouldBe true
      }
    }
  }

  it("predicate appearance should contain 2 entries each having 5 appearances") {
    path.predicateAppearance.size shouldBe 2
    path.predicateAppearance.values.forall( count => count == 5) shouldBe true
  }

  it("constant appearance should contain the correct counts for all constants") {

    constantsDomain("domA").foreach { value =>
      if (value == "0") path.constantAppearance(value) shouldBe 2
      else if (value == "5") path.constantAppearance(value) shouldBe 1
      else path.constantAppearance(value) shouldBe 3
    }

    constantsDomain("domB").foreach(path.constantAppearance(_) shouldBe 0)
  }

  it("checking path ordering") {
    path.ordering.reverse.map(_._1).toSet shouldEqual atomIDs.keys
  }
}
