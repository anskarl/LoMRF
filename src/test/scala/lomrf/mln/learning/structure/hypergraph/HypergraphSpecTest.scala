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

import lomrf.logic.{ AtomSignature, Constant, EvidenceAtom, FunctionMapping }
import lomrf.mln.learning.structure.ClauseConstructor.ClauseType
import lomrf.mln.learning.structure._
import lomrf.mln.model._
import lomrf.mln.model.builders.EvidenceBuilder
import lomrf.{ AUX_PRED_PREFIX => PREFIX }
import org.scalatest.{ FunSpec, Matchers }
import scala.util.{ Failure, Success }

/**
  * Specification test for HyperGraph creation and search.
  */
final class HypergraphSpecTest extends FunSpec with Matchers {

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST I: HyperGraph using the simple Yale Shooting Scenario (NO FUNCTIONS)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Yale Shooting Scenario HyperGraph") {

    info("==== Yale Shooting Scenario HyperGraph ====")

    val TIME_DOMAIN_SIZE = 14
    val MAX_PATH_LENGTH = 5

    // Predicate schema
    val predicateSchema = Map(
      AtomSignature("Happens", 2) -> Vector("event", "time"),
      AtomSignature("Next", 2) -> Vector("time", "time"),
      AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"),
      AtomSignature("Ignore", 1) -> Vector("ignore"),
      AtomSignature("ZeroRecall", 1) -> Vector("ignore"))

    // Query predicates
    val queryPredicates = Set[AtomSignature](AtomSignature("HoldsAt", 2))

    // Constants domain
    val constantsDomain = Map(
      "time" -> ConstantsSet((0 to TIME_DOMAIN_SIZE).map(_.toString)),
      "fluent" -> ConstantsSet("Dead", "Alive", "Loaded"),
      "event" -> ConstantsSet("Shoot", "Load"),
      "ignore" -> ConstantsSet("Ignore1", "Ignore2", "Ignore3", "Ignore4", "Ignore5", "Ignore6", "Ignore7", "Ignore8"))

    val builder = EvidenceBuilder(predicateSchema, queryPredicates, Set.empty, constantsDomain)

    // Append evidence atoms for Next predicate
    val evidenceForNext = (0 until TIME_DOMAIN_SIZE).map { timePoint =>
      EvidenceAtom.asTrue("Next", Vector[Constant](Constant((timePoint + 1).toString), Constant(timePoint.toString)))
    }
    builder.evidence ++= evidenceForNext

    // Append evidence atoms for Happens predicate
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Shoot"), Constant(2.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Shoot"), Constant(5.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Shoot"), Constant(8.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Shoot"), Constant(11.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Load"), Constant(3.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Load"), Constant(9.toString)))

    // Append evidence atoms for Ignore predicate
    builder.evidence += EvidenceAtom.asTrue("Ignore", Vector[Constant](Constant("Ignore1")))
    builder.evidence += EvidenceAtom.asTrue("Ignore", Vector[Constant](Constant("Ignore2")))
    builder.evidence += EvidenceAtom.asTrue("Ignore", Vector[Constant](Constant("Ignore3")))
    builder.evidence += EvidenceAtom.asTrue("Ignore", Vector[Constant](Constant("Ignore4")))

    // Append evidence atoms for ZeroRecall predicate
    builder.evidence += EvidenceAtom.asTrue("ZeroRecall", Vector[Constant](Constant("Ignore5")))
    builder.evidence += EvidenceAtom.asTrue("ZeroRecall", Vector[Constant](Constant("Ignore6")))
    builder.evidence += EvidenceAtom.asTrue("ZeroRecall", Vector[Constant](Constant("Ignore7")))
    builder.evidence += EvidenceAtom.asTrue("ZeroRecall", Vector[Constant](Constant("Ignore8")))

    val evidence = builder.result()

    // Create MLN
    val mlnSchema = new MLNSchema(predicateSchema, Map.empty, Map.empty, Map.empty)
    val predicateSpace = PredicateSpace(mlnSchema, queryPredicates, constantsDomain)
    val mln = MLN(mlnSchema, evidence, predicateSpace, Vector.empty)

    // Create annotation
    val annotationSchema = Map(AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"))
    val annotationBuilder = EvidenceBuilder(annotationSchema, queryPredicates, Set.empty, constantsDomain)

    // Fluent Alive annotation
    annotationBuilder.evidence ++= (6 to 14).map { timePoint =>
      EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Alive"), Constant(timePoint.toString)))
    }
    annotationBuilder.evidence ++= (0 to 5).map { timePoint =>
      EvidenceAtom.asTrue("HoldsAt", Vector[Constant](Constant("Alive"), Constant(timePoint.toString)))
    }

    // Fluent Dead annotation
    annotationBuilder.evidence ++= (6 to 14).map { timePoint =>
      EvidenceAtom.asTrue("HoldsAt", Vector[Constant](Constant("Dead"), Constant(timePoint.toString)))
    }
    annotationBuilder.evidence ++= (0 to 5).map { timePoint =>
      EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Dead"), Constant(timePoint.toString)))
    }

    // Fluent Loaded annotation
    annotationBuilder.evidence ++= (12 to 14).map { timePoint =>
      EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Loaded"), Constant(timePoint.toString)))
    }
    annotationBuilder.evidence ++= (10 to 11).map { timePoint =>
      EvidenceAtom.asTrue("HoldsAt", Vector[Constant](Constant("Loaded"), Constant(timePoint.toString)))
    }
    annotationBuilder.evidence ++= (6 to 9).map { timePoint =>
      EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Loaded"), Constant(timePoint.toString)))
    }
    annotationBuilder.evidence ++= (4 to 5).map { timePoint =>
      EvidenceAtom.asTrue("HoldsAt", Vector[Constant](Constant("Loaded"), Constant(timePoint.toString)))
    }
    annotationBuilder.evidence ++= (0 to 3).map { timePoint =>
      EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Loaded"), Constant(timePoint.toString)))
    }

    val annotationDB = annotationBuilder.result().db

    // Parse mode declarations
    val modeList = List(
      "modeP(1, Happens(#-,-))",
      "modeP(2, HoldsAt(+,+))",
      "modeP(1, Next(-,+))",
      "modeP(*, Ignore(.))",
      "modeP(0, ZeroRecall(+))")

    val modes = ModeParser.parseFrom(modeList)

    val HG = HyperGraph(mln, evidence.db, annotationDB, modes)

    info(HG.toText)

    it("should contain 20 nodes and 78 edges") {
      HG.numberOfNodes shouldBe 20
      HG.numberOfEdges shouldBe 78
    }

    // Use as initial search set all the query ground atom ids
    val searchSet = annotationDB.values.flatMap(_.identity.indices).toVector

    // ------------------------------------------------------------------------------------------------------------------
    // --- TEST CASE: HyperGraph find paths (without free variables).
    // ------------------------------------------------------------------------------------------------------------------
    describe("HyperGraph path finding, when free variables are NOT allowed") {

      val pathsCase1 = HG.findPaths(searchSet, MAX_PATH_LENGTH)
      info(pathsCase1.map(_.toText(mln)).mkString("\n"))

      it("paths should be 34 when free variables are not allowed") {
        pathsCase1.size shouldBe 34
      }

      val clausesCaseA = ClauseConstructor.clauses(pathsCase1, predicateSchema, modes, evidence, ClauseType.BOTH) match {
        case Success(result) =>
          info(result.map(_.toText(weighted = false)).mkString("\n"))
          result
        case Failure(exception) => throw exception
      }

      it("Generated clauses when free variables are not allowed should be 10") {
        clausesCaseA.size shouldBe 10
      }
    }

    // ------------------------------------------------------------------------------------------------------------------
    // --- TEST CASE: HyperGraph find paths (allowing free variables).
    // ------------------------------------------------------------------------------------------------------------------
    describe("HyperGraph path finding, when free variables are allowed") {

      val pathsCaseB = HG.findPaths(searchSet, MAX_PATH_LENGTH, allowFreeVariables = true)
      info(pathsCaseB.map(_.toText(mln)).mkString("\n"))

      it("paths should be 130 when free variables are allowed") {
        pathsCaseB.size shouldBe 130
      }

      val clausesCaseB = ClauseConstructor.clauses(pathsCaseB, predicateSchema, modes, evidence, ClauseType.BOTH) match {
        case Success(result) =>
          info(result.map(_.toText(weighted = false)).mkString("\n"))
          result
        case Failure(exception) => throw exception
      }

      it("Generated clauses when free variables are allowed should be 24") {
        clausesCaseB.size shouldBe 24
      }
    }

  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST II: HyperGraph using the CAVIAR problem (1 LEVEL FUNCTIONS)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Caviar HyperGraph") {

    info("==== Caviar HyperGraph ====")

    val TIME_DOMAIN_SIZE = 14
    val MAX_PATH_LENGTH = 4

    // Predicate schema
    val predicateSchema = Map(
      AtomSignature("Happens", 2) -> Vector("event", "time"),
      AtomSignature("Next", 2) -> Vector("time", "time"),
      AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"))

    // Function schema
    val functionSchema = Map(
      AtomSignature("meet", 2) -> ("fluent", Vector("id", "id")),
      AtomSignature("inactive", 1) -> ("event", Vector("id")),
      AtomSignature("walking", 1) -> ("event", Vector("id")))

    // Query predicates
    val queryPredicates = Set[AtomSignature](AtomSignature("HoldsAt", 2))

    // Constants domain
    val constantsDomain = Map(
      "id" -> ConstantsSet("ID1", "ID2"),
      "time" -> ConstantsSet((0 to TIME_DOMAIN_SIZE).map(_.toString)),
      "fluent" -> ConstantsSet("Meet_ID1_ID2", "Meet_ID2_ID1", "Meet_ID1_ID1", "Meet_ID2_ID2"),
      "event" -> ConstantsSet("Walking_ID1", "Walking_ID2", "Inactive_ID1", "Inactive_ID2"))

    // Evidence Builder
    val builder = EvidenceBuilder(predicateSchema, functionSchema, queryPredicates, Set.empty, constantsDomain, convertFunctions = true)

    // Append evidence atoms for Next predicate
    builder.evidence ++= (0 until TIME_DOMAIN_SIZE).map { timePoint =>
      EvidenceAtom.asTrue("Next", Vector[Constant](Constant((timePoint + 1).toString), Constant(timePoint.toString)))
    }

    // Append evidence atoms for Happens predicate for ID1
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID1"), Constant(0.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID1"), Constant(1.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID1"), Constant(2.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID1"), Constant(3.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID1"), Constant(4.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID1"), Constant(5.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID1"), Constant(6.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID1"), Constant(7.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID1"), Constant(8.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID1"), Constant(9.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID1"), Constant(10.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID1"), Constant(11.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID1"), Constant(12.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID1"), Constant(13.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID1"), Constant(14.toString)))

    // Append evidence atoms for Happens predicate for ID2
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID2"), Constant(0.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID2"), Constant(1.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID2"), Constant(2.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID2"), Constant(3.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID2"), Constant(4.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID2"), Constant(5.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Inactive_ID2"), Constant(6.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID2"), Constant(7.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID2"), Constant(8.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID2"), Constant(9.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID2"), Constant(10.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID2"), Constant(11.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID2"), Constant(12.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID2"), Constant(13.toString)))
    builder.evidence += EvidenceAtom.asTrue("Happens", Vector[Constant](Constant("Walking_ID2"), Constant(14.toString)))

    // Append function mappings for walking/1, inactive/1 and meet/2
    builder.functions += FunctionMapping("Inactive_ID1", "inactive", Vector(Constant("ID1")))
    builder.functions += FunctionMapping("Inactive_ID2", "inactive", Vector(Constant("ID2")))
    builder.functions += FunctionMapping("Walking_ID1", "walking", Vector(Constant("ID1")))
    builder.functions += FunctionMapping("Walking_ID2", "walking", Vector(Constant("ID2")))
    builder.functions += FunctionMapping("Meet_ID1_ID2", "meet", Vector(Constant("ID1"), Constant("ID2")))
    builder.functions += FunctionMapping("Meet_ID2_ID1", "meet", Vector(Constant("ID2"), Constant("ID1")))
    builder.functions += FunctionMapping("Meet_ID1_ID1", "meet", Vector(Constant("ID1"), Constant("ID1")))
    builder.functions += FunctionMapping("Meet_ID2_ID2", "meet", Vector(Constant("ID2"), Constant("ID2")))

    val evidence = builder.result()

    // Predicate schema having the auxiliary predicates (VERY IMPORTANT)
    val AuxPredicateSchema = Map(
      AtomSignature("Happens", 2) -> Vector("event", "time"),
      AtomSignature("Next", 2) -> Vector("time", "time"),
      AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"),
      AtomSignature(s"${PREFIX}meet", 3) -> Vector("fluent", "id", "id"),
      AtomSignature(s"${PREFIX}inactive", 2) -> Vector("event", "id"),
      AtomSignature(s"${PREFIX}walking", 2) -> Vector("event", "id"))

    // Create MLN
    val mlnSchema = new MLNSchema(AuxPredicateSchema, Map.empty, Map.empty, Map.empty)
    val predicateSpace = PredicateSpace(mlnSchema, queryPredicates, constantsDomain)
    val mln = MLN(mlnSchema, evidence, predicateSpace, Vector.empty)

    // Create annotation
    val annotationSchema = Map(AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"))
    val annotationBuilder = EvidenceBuilder(annotationSchema, queryPredicates, Set.empty, constantsDomain)

    // Fluent Meet_ID1_ID2 and Meet_ID2_ID1 annotation
    annotationBuilder.evidence ++= (4 to 6).map { timePoint =>
      EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Meet_ID1_ID2"), Constant(timePoint.toString)))
    }
    annotationBuilder.evidence ++= (4 to 6).map { timePoint =>
      EvidenceAtom.asTrue("HoldsAt", Vector[Constant](Constant("Meet_ID2_ID1"), Constant(timePoint.toString)))
    }

    val annotationDB = annotationBuilder.result().db

    // Parse mode declarations
    val modeList = List(
      "modeP(2, Happens(-,+))",
      "modeP(2, HoldsAt(-,-))",
      "modeP(1, Next(-,+))",
      "modeF(2, walking(+))",
      "modeF(2, inactive(+))",
      "modeF(1, meet(-,-))")

    val modes = ModeParser.parseFrom(modeList)

    val HG = HyperGraph(mln, evidence.db, annotationDB, modes)

    info(HG.toText)

    it("should contain 15 nodes and 61 edges") {
      HG.numberOfNodes shouldBe 15
      HG.numberOfEdges shouldBe 61
    }

    // Use as initial search set all the query ground atom ids
    val searchSet = annotationDB.values.flatMap(_.identity.indices).toVector

    // ------------------------------------------------------------------------------------------------------------------
    // --- TEST CASE: HyperGraph find paths (without free variables).
    // ------------------------------------------------------------------------------------------------------------------
    describe("HyperGraph path finding, when free variables are NOT allowed") {

      val pathsCase1 = HG.findPaths(searchSet, MAX_PATH_LENGTH)
      info(pathsCase1.map(_.toText(mln)).mkString("\n"))

      it("paths should be 175 when free variables are not allowed") {
        pathsCase1.size shouldBe 175
      }

      val clausesCaseA = ClauseConstructor.clauses(pathsCase1, AuxPredicateSchema, modes, evidence, ClauseType.BOTH) match {
        case Success(result) =>
          info(result.map(_.toText(weighted = false)).mkString("\n"))
          result
        case Failure(exception) => throw exception
      }

      it("Generated clauses when free variables are not allowed should be 44") {
        clausesCaseA.size shouldBe 44
      }
    }

    // ------------------------------------------------------------------------------------------------------------------
    // --- TEST CASE: HyperGraph find paths (allowing free variables).
    // ------------------------------------------------------------------------------------------------------------------
    describe("HyperGraph path finding, when free variables are allowed") {

      val pathsCaseB = HG.findPaths(searchSet, MAX_PATH_LENGTH, allowFreeVariables = true)
      info(pathsCaseB.map(_.toText(mln)).mkString("\n"))

      it("paths should be 465 when free variables are allowed") {
        pathsCaseB.size shouldBe 465
      }

      val clausesCaseB = ClauseConstructor.clauses(pathsCaseB, AuxPredicateSchema, modes, evidence, ClauseType.BOTH) match {
        case Success(result) =>
          info(result.map(_.toText(weighted = false)).mkString("\n"))
          result
        case Failure(exception) => throw exception
      }

      it("Generated clauses when free variables are allowed should be 92") {
        clausesCaseB.size shouldBe 92
      }
    }

  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST III: HyperGraph having nested functions (3 LEVEL FUNCTIONS)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Nested Functions HyperGraph") {

    info("==== Nested Function HyperGraph ====")

    val NUMBER_DOMAIN_SIZE = 1
    val MAX_PATH_LENGTH = 2

    // Predicate schema
    val predicateSchema = Map(
      AtomSignature("Evidence", 2) -> Vector("fooDomain", "id"),
      AtomSignature("Query", 2) -> Vector("quxDomain", "id"))

    // Function schema
    val functionSchema = Map(
      AtomSignature("foo", 2) -> ("fooDomain", Vector("barDomain", "number")),
      AtomSignature("bar", 2) -> ("barDomain", Vector("bazDomain", "character")),
      AtomSignature("baz", 1) -> ("bazDomain", Vector("symbol")),
      AtomSignature("qux", 2) -> ("quxDomain", Vector("qaxDomain", "number")),
      AtomSignature("qax", 2) -> ("qaxDomain", Vector("quzDomain", "character")),
      AtomSignature("quz", 1) -> ("quzDomain", Vector("symbol")))

    // Query predicates
    val queryPredicates = Set[AtomSignature](AtomSignature("Query", 2))

    // Constants domain
    val constantsDomain = Map(
      "id" -> ConstantsSet("ID1"),
      "character" -> ConstantsSet("A", "B"),
      "number" -> ConstantsSet((0 to NUMBER_DOMAIN_SIZE).map(_.toString)),
      "symbol" -> ConstantsSet("@", "*"),
      "fooDomain" -> ConstantsSet("Foo_AT_A_0", "Foo_AT_B_0", "Foo_AT_A_1", "Foo_AT_B_1",
        "Foo_STAR_A_0", "Foo_STAR_B_0", "Foo_STAR_A_1", "Foo_STAR_B_1"),
      "barDomain" -> ConstantsSet("Bar_AT_A", "Bar_AT_B", "Bar_STAR_A", "Bar_STAR_B"),
      "bazDomain" -> ConstantsSet("Baz_AT", "Baz_STAR"),
      "quxDomain" -> ConstantsSet("Qux_AT_A_0", "Qux_AT_B_0", "Qux_AT_A_1", "Qux_AT_B_1",
        "Qux_STAR_A_0", "Qux_STAR_B_0", "Qux_STAR_A_1", "Qux_STAR_B_1"),
      "qaxDomain" -> ConstantsSet("Qax_AT_A", "Qax_AT_B", "Qax_STAR_A", "Qax_STAR_B"),
      "quzDomain" -> ConstantsSet("Quz_AT", "Quz_STAR"))

    // Evidence Builder
    val builder = EvidenceBuilder(predicateSchema, functionSchema, queryPredicates, Set.empty, constantsDomain, convertFunctions = true)

    // Append evidence atoms for Evidence predicate
    builder.evidence += EvidenceAtom.asTrue("Evidence", Vector[Constant](Constant("Foo_AT_A_0"), Constant("ID1")))
    builder.evidence += EvidenceAtom.asTrue("Evidence", Vector[Constant](Constant("Foo_AT_A_1"), Constant("ID1")))
    builder.evidence += EvidenceAtom.asTrue("Evidence", Vector[Constant](Constant("Foo_AT_B_1"), Constant("ID1")))
    builder.evidence += EvidenceAtom.asTrue("Evidence", Vector[Constant](Constant("Foo_STAR_B_0"), Constant("ID1")))
    builder.evidence += EvidenceAtom.asTrue("Evidence", Vector[Constant](Constant("Foo_STAR_B_1"), Constant("ID1")))

    // Append function mappings for qux/2, qax/2, quz/1, foo/2, bar/2 and baz/1
    builder.functions += FunctionMapping("Foo_AT_A_0", "foo", Vector(Constant("Bar_AT_A"), Constant("0")))
    builder.functions += FunctionMapping("Foo_AT_A_1", "foo", Vector(Constant("Bar_AT_A"), Constant("1")))
    builder.functions += FunctionMapping("Foo_AT_B_0", "foo", Vector(Constant("Bar_AT_B"), Constant("0")))
    builder.functions += FunctionMapping("Foo_AT_B_1", "foo", Vector(Constant("Bar_AT_B"), Constant("1")))
    builder.functions += FunctionMapping("Foo_STAR_A_0", "foo", Vector(Constant("Bar_STAR_A"), Constant("0")))
    builder.functions += FunctionMapping("Foo_STAR_A_1", "foo", Vector(Constant("Bar_STAR_A"), Constant("1")))
    builder.functions += FunctionMapping("Foo_STAR_B_0", "foo", Vector(Constant("Bar_STAR_B"), Constant("0")))
    builder.functions += FunctionMapping("Foo_STAR_B_1", "foo", Vector(Constant("Bar_STAR_B"), Constant("1")))

    builder.functions += FunctionMapping("Bar_AT_A", "bar", Vector(Constant("Baz_AT"), Constant("A")))
    builder.functions += FunctionMapping("Bar_AT_B", "bar", Vector(Constant("Baz_AT"), Constant("B")))
    builder.functions += FunctionMapping("Bar_STAR_A", "bar", Vector(Constant("Baz_STAR"), Constant("A")))
    builder.functions += FunctionMapping("Bar_STAR_B", "bar", Vector(Constant("Baz_STAR"), Constant("B")))

    builder.functions += FunctionMapping("Baz_AT", "baz", Vector(Constant("@")))
    builder.functions += FunctionMapping("Baz_STAR", "baz", Vector(Constant("*")))

    builder.functions += FunctionMapping("Qux_AT_A_0", "qux", Vector(Constant("Qax_AT_A"), Constant("0")))
    builder.functions += FunctionMapping("Qux_AT_A_1", "qux", Vector(Constant("Qax_AT_A"), Constant("1")))
    builder.functions += FunctionMapping("Qux_AT_B_0", "qux", Vector(Constant("Qax_AT_B"), Constant("0")))
    builder.functions += FunctionMapping("Qux_AT_B_1", "qux", Vector(Constant("Qax_AT_B"), Constant("1")))
    builder.functions += FunctionMapping("Qux_STAR_A_0", "qux", Vector(Constant("Qax_STAR_A"), Constant("0")))
    builder.functions += FunctionMapping("Qux_STAR_A_1", "qux", Vector(Constant("Qax_STAR_A"), Constant("1")))
    builder.functions += FunctionMapping("Qux_STAR_B_0", "qux", Vector(Constant("Qax_STAR_B"), Constant("0")))
    builder.functions += FunctionMapping("Qux_STAR_B_1", "qux", Vector(Constant("Qax_STAR_B"), Constant("1")))

    builder.functions += FunctionMapping("Qax_AT_A", "qax", Vector(Constant("Quz_AT"), Constant("A")))
    builder.functions += FunctionMapping("Qax_AT_B", "qax", Vector(Constant("Quz_AT"), Constant("B")))
    builder.functions += FunctionMapping("Qax_STAR_A", "qax", Vector(Constant("Quz_STAR"), Constant("A")))
    builder.functions += FunctionMapping("Qax_STAR_B", "qax", Vector(Constant("Quz_STAR"), Constant("B")))

    builder.functions += FunctionMapping("Quz_AT", "quz", Vector(Constant("@")))
    builder.functions += FunctionMapping("Quz_STAR", "quz", Vector(Constant("*")))

    val evidence = builder.result()

    // Predicate schema having the auxiliary predicates (VERY IMPORTANT)
    val AuxPredicateSchema = Map(
      AtomSignature("Evidence", 2) -> Vector("fooDomain", "id"),
      AtomSignature("Query", 2) -> Vector("quxDomain", "id"),
      AtomSignature(s"${PREFIX}foo", 3) -> Vector("fooDomain", "barDomain", "number"),
      AtomSignature(s"${PREFIX}bar", 3) -> Vector("barDomain", "bazDomain", "character"),
      AtomSignature(s"${PREFIX}baz", 2) -> Vector("bazDomain", "symbol"),
      AtomSignature(s"${PREFIX}qux", 3) -> Vector("quxDomain", "qaxDomain", "number"),
      AtomSignature(s"${PREFIX}qax", 3) -> Vector("qaxDomain", "quzDomain", "character"),
      AtomSignature(s"${PREFIX}quz", 2) -> Vector("quzDomain", "symbol"))

    // Create MLN
    val mlnSchema = new MLNSchema(AuxPredicateSchema, Map.empty, Map.empty, Map.empty)
    val predicateSpace = PredicateSpace(mlnSchema, queryPredicates, constantsDomain)
    val mln = MLN(mlnSchema, evidence, predicateSpace, Vector.empty)

    // Create annotation
    val annotationSchema = Map(AtomSignature("Query", 2) -> Vector("quxDomain", "id"))
    val annotationBuilder = EvidenceBuilder(annotationSchema, queryPredicates, Set.empty, constantsDomain)

    // Query annotation
    annotationBuilder.evidence += EvidenceAtom.asTrue("Query", Vector[Constant](Constant("Qux_AT_A_0"), Constant("ID1")))
    annotationBuilder.evidence += EvidenceAtom.asTrue("Query", Vector[Constant](Constant("Qux_AT_A_1"), Constant("ID1")))
    annotationBuilder.evidence += EvidenceAtom.asTrue("Query", Vector[Constant](Constant("Qux_STAR_B_0"), Constant("ID1")))
    annotationBuilder.evidence += EvidenceAtom.asTrue("Query", Vector[Constant](Constant("Qux_STAR_B_1"), Constant("ID1")))

    val annotationDB = annotationBuilder.result().db

    // Parse mode declarations
    val modeList = List(
      "modeP(1, Evidence(-, +))",
      "modeP(0, Query(-, -))",
      "modeF(1, qux(-, -))",
      "modeF(1, qax(-, -))",
      "modeF(1, quz(-))",
      "modeF(1, foo(-, +))",
      "modeF(1, bar(-, +))",
      "modeF(1, baz(-))")

    val modes = ModeParser.parseFrom(modeList)

    val HG = HyperGraph(mln, evidence.db, annotationDB, modes)

    info(HG.toText)

    it("should contain 1 node and 5 edges") {
      HG.numberOfNodes shouldBe 1
      HG.numberOfEdges shouldBe 5
    }

    // Use as initial search set all the query ground atom ids
    val searchSet = annotationDB.values.flatMap(_.identity.indices).toVector

    // ------------------------------------------------------------------------------------------------------------------
    // --- TEST CASE: HyperGraph find paths (without free variables).
    // ------------------------------------------------------------------------------------------------------------------
    describe("HyperGraph path finding, when free variables are NOT allowed") {

      val pathsCase1 = HG.findPaths(searchSet, MAX_PATH_LENGTH)
      info(pathsCase1.map(_.toText(mln)).mkString("\n"))

      it("paths should be 5 when free variables are not allowed") {
        pathsCase1.size shouldBe 5
      }

      val clausesCaseA = ClauseConstructor.clauses(pathsCase1, AuxPredicateSchema, modes, evidence, ClauseType.BOTH) match {
        case Success(result) =>
          info(result.map(_.toText(weighted = false)).mkString("\n"))
          result
        case Failure(exception) => throw exception
      }

      it("Generated clauses when free variables are not allowed should be 2") {
        clausesCaseA.size shouldBe 2
      }
    }

    // ------------------------------------------------------------------------------------------------------------------
    // --- TEST CASE: HyperGraph find paths (allowing free variables).
    // ------------------------------------------------------------------------------------------------------------------
    describe("HyperGraph path finding, when free variables are allowed") {

      val pathsCaseB = HG.findPaths(searchSet, MAX_PATH_LENGTH, allowFreeVariables = true)
      info(pathsCaseB.map(_.toText(mln)).mkString("\n"))

      it("paths should be 10 when free variables are allowed") {
        pathsCaseB.size shouldBe 10
      }

      val clausesCaseB = ClauseConstructor.clauses(pathsCaseB, AuxPredicateSchema, modes, evidence, ClauseType.BOTH) match {
        case Success(result) =>
          info(result.map(_.toText(weighted = false)).mkString("\n"))
          result
        case Failure(exception) => throw exception
      }

      it("Generated clauses when free variables are allowed should be 4") {
        clausesCaseB.size shouldBe 4
      }
    }

  }
}
