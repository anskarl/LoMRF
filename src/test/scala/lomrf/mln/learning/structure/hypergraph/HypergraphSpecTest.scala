package lomrf.mln.learning.structure.hypergraph

import lomrf.logic.{AtomSignature, Constant, EvidenceAtom}
import lomrf.mln.learning.structure.ClauseConstructor.ClauseType
import lomrf.mln.learning.structure._
import lomrf.mln.model._
import org.scalatest.{FunSpec, Matchers}

import scala.util.{Success, Failure}

/**
 * Specification test for hypergraph creation and search.
 */
final class HypergraphSpecTest extends FunSpec with Matchers {

  private val TIME_DOMAIN_SIZE = 14
  private val MAX_PATH_LENGTH = 5

  // Predicate schema
  private val predicateSchema = Map(
    AtomSignature("Happens", 2) -> Vector("event", "time"),
    AtomSignature("Next", 2) -> Vector("time", "time"),
    AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"),
    AtomSignature("Ignore", 1) -> Vector("ignore"),
    AtomSignature("ZeroRecall", 1) -> Vector("ignore"))

  // Query predicates
  private val queryPredicates = Set[AtomSignature](AtomSignature("HoldsAt", 2))

  // Constants domain
  private val constantsDomain = Map(
    "time" -> ConstantsSet((0 to TIME_DOMAIN_SIZE).map(_.toString)),
    "fluent" -> ConstantsSet("Dead", "Alive", "Loaded"),
    "event" -> ConstantsSet("Shoot", "Load"),
    "ignore" -> ConstantsSet("Ignore1", "Ignore2", "Ignore3", "Ignore4", "Ignore5", "Ignore6", "Ignore7", "Ignore8"))

  val builder = EvidenceBuilder(predicateSchema, queryPredicates, Set.empty, constantsDomain)

  // Append evidence atoms for Next predicate
  val evidenceForNext = (0 to TIME_DOMAIN_SIZE - 1).map { timepoint =>
    EvidenceAtom.asTrue("Next", Vector[Constant](Constant((timepoint + 1).toString), Constant(timepoint.toString)))
  }.toSeq

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
  private val annotationSchema = Map(AtomSignature("HoldsAt", 2) -> Vector("fluent", "time"))
  val annotationBuilder = EvidenceBuilder(annotationSchema, queryPredicates, Set.empty, constantsDomain)

  // Fluent Alive annotation
  annotationBuilder.evidence ++= (6 to 14).map { timepoint =>
    EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Alive"), Constant(timepoint.toString)))
  }
  annotationBuilder.evidence ++= (0 to 5).map { timepoint =>
    EvidenceAtom.asTrue("HoldsAt", Vector[Constant](Constant("Alive"), Constant(timepoint.toString)))
  }

  // Fluent Dead annotation
  annotationBuilder.evidence ++= (6 to 14).map { timepoint =>
    EvidenceAtom.asTrue("HoldsAt", Vector[Constant](Constant("Dead"), Constant(timepoint.toString)))
  }
  annotationBuilder.evidence ++= (0 to 5).map { timepoint =>
    EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Dead"), Constant(timepoint.toString)))
  }

  // Fluent Loaded annotation
  annotationBuilder.evidence ++= (12 to 14).map { timepoint =>
    EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Loaded"), Constant(timepoint.toString)))
  }
  annotationBuilder.evidence ++= (10 to 11).map { timepoint =>
    EvidenceAtom.asTrue("HoldsAt", Vector[Constant](Constant("Loaded"), Constant(timepoint.toString)))
  }
  annotationBuilder.evidence ++= (6 to 9).map { timepoint =>
    EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Loaded"), Constant(timepoint.toString)))
  }
  annotationBuilder.evidence ++= (4 to 5).map { timepoint =>
    EvidenceAtom.asTrue("HoldsAt", Vector[Constant](Constant("Loaded"), Constant(timepoint.toString)))
  }
  annotationBuilder.evidence ++= (0 to 3).map { timepoint =>
    EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("Loaded"), Constant(timepoint.toString)))
  }

  val annotationDB = annotationBuilder.result().db

  // Parse mode declarations
  val modeList = List(
    "modeP(1, Happens(#-,-))",
    "modeP(2, HoldsAt(+,+))",
    "modeP(1, Next(-,+))",
    "modeP(*, Ignore(.))",
    "modeP(0, ZeroRecall(+))")

  val modeParser = new ModeParser

  val modes = modeList.map { source =>
    modeParser.parseMode(source)
  }.toMap

  val HG = HyperGraph(mln, evidence.db, annotationDB, modes)

  info(HG.toText)

  it("should contain 20 nodes and 78 edges") {
    HG.numberOfNodes shouldBe 20
    HG.numberOfEdges shouldBe 78
  }

  // Use as initial search set all the query ground atom ids
  val searchSet = annotationDB.values.flatMap(_.identity.indices).toVector

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Hypergraph find paths (without free variables).
  // ------------------------------------------------------------------------------------------------------------------
  describe("Hypergraph path finding, when free variales are NOT allowed") {

    val pathsCase1 = HG.findPaths(searchSet, MAX_PATH_LENGTH, allowFreeVariables = false)
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

    it("Generated clauses when free variables are not allowed should be 6") {
      clausesCaseA.size shouldBe 6
    }

  }
  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Hypergraph find paths (allowing free variables).
  // ------------------------------------------------------------------------------------------------------------------
  describe("Hypergraph path finding, when free variales are allowed") {

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
      clausesCaseB.size shouldBe 16
    }

  }
}
