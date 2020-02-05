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

package lomrf.mln.model.builders

import lomrf.logic._
import lomrf.logic.AtomSignatureOps._
import lomrf.mln.model.{ AtomIdentityFunction, ConstantsSet }
import org.scalatest.{ FunSpec, Matchers }
import scala.language.implicitConversions

/**
  * A series of specification test for the evidence builder.
  *
  * @see [[lomrf.mln.model.builders.EvidenceBuilder]]
  */
final class EvidenceBuilderSpecTest extends FunSpec with Matchers {

  implicit def str2Constant(symbol: String): Constant = Constant(symbol)

  implicit def const2String(constant: Constant): String = constant.symbol

  // ------------------------------------------------------------------------------------------------------------------
  // --- CONSTANTS
  // ------------------------------------------------------------------------------------------------------------------
  private val TIME_DOMAIN_SIZE = 10

  private val samplePredicateSchemas = Seq(
    ("HoldsAt", Vector("fluent", "time")),
    ("HappensAt", Vector("event", "time")),
    ("Next", Vector("time", "time")),
    ("InitiatedAt", Vector("fluent", "time")),
    ("TerminatedAt", Vector("fluent", "time"))).map(schema => AtomSignature(schema._1, schema._2.size) -> schema._2).toMap

  private val sampleFunctionSchema = Map(
    AtomSignature("walking", 1) -> ("event", Vector("person")),
    AtomSignature("running", 1) -> ("event", Vector("person")))

  private val sampleFunctionMappings = Seq(
    ("Walking_Kalypso", "walking", Vector("Kalypso")),
    ("Walking_Phaidra", "walking", Vector("Phaidra")),
    ("Walking_Ligeia", "walking", Vector("Ligeia")),
    ("Walking_Ismene", "walking", Vector("Ismene")),
    ("Walking_Irene", "walking", Vector("Irene")),
    ("Running_Kalypso", "running", Vector("Kalypso")),
    ("Running_Phaidra", "running", Vector("Phaidra")),
    ("Running_Ligeia", "running", Vector("Ligeia")),
    ("Running_Ismene", "running", Vector("Ismene")),
    ("Running_Irene", "running", Vector("Irene")))

  private val sampleFunctionMappers = sampleFunctionMappings map {
    case (retValue, symbol, args) => new FunctionMapping(retValue, symbol, args)
  }

  private val queryPredicates = Set[AtomSignature](("HoldsAt", 2))
  private val hiddenPredicates = Set[AtomSignature](("InitiatedAt", 2), ("TerminatedAt", 2))

  // Predicates that we are not plan to give any evidence in our unit test scenarios
  private val predicatesMissingEvidence = Set[AtomSignature](("InitiatedAt", 2), ("TerminatedAt", 2), ("Next", 2))

  private val constantsDomain = Map(
    "time" -> ConstantsSet((1 to TIME_DOMAIN_SIZE).map(_.toString)),
    "fluent" -> ConstantsSet("moving", "meeting", "leaving_object"),
    "event" -> ConstantsSet(
      "Walking_Kalypso", "Walking_Phaidra", "Walking_Ligeia", "Walking_Ismene", "Walking_Irene",
      "Running_Kalypso", "Running_Phaidra", "Running_Ligeia", "Running_Ismene", "Running_Irene"),
    "person" -> ConstantsSet("Kalypso", "Phaidra", "Ligeia", "Ismene", "Irene"))

  // ------------------------------------------------------------------------------------------------------------------
  // --- utility constants (automatically produced)
  // ------------------------------------------------------------------------------------------------------------------
  private val owaPredicates = queryPredicates ++ hiddenPredicates
  private val cwaPredicates = samplePredicateSchemas.keySet -- owaPredicates

  /**
    * Number of known truth values of all ground predicates:
    * - For CWA predicates = all their possible groundings (Computed by the product of their term domains)
    * - Otherwise, for OWA predicates = 0 (since, all possible groundings have unknown truth state)
    */
  private val initialKnown = samplePredicateSchemas map {
    case (signature, schema) if cwaPredicates.contains(signature) =>
      signature -> schema.map(d => constantsDomain(d).size).product
    case (signature, _) => signature -> 0
  }

  private val space = samplePredicateSchemas map {
    case (signature, schema) => signature -> schema.map(d => constantsDomain(d).size).product
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Evidence Builder (empty)
  // ------------------------------------------------------------------------------------------------------------------
  describe("EvidenceBuilder with empty evidence") {
    val builder = EvidenceBuilder(samplePredicateSchemas, queryPredicates, hiddenPredicates, constantsDomain)

    val result = builder.result()

    it("should give correct number of known groundings for all predicates") {
      assert {
        result.db.forall {
          case (signature, atomDB) => atomDB.numberOfKnown == initialKnown(signature)
        }
      }
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Evidence Builder (incremental and batch addition of evidence)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Evidence construction (groundings of a single predicate)") {

    // ----- Data generation:
    val PRED_NAME = "HappensAt"
    val SIGNATURE = AtomSignature(PRED_NAME, 2)

    val builder = EvidenceBuilder(samplePredicateSchemas, queryPredicates, hiddenPredicates, constantsDomain)

    val builderBatch = EvidenceBuilder(samplePredicateSchemas, queryPredicates, hiddenPredicates, constantsDomain)

    // (STEP 1) Insert the first half (incremental)
    var insertedFirstHalf = List.empty[EvidenceAtom]

    for (timepoint <- 1 to 5) {
      val atom = EvidenceAtom.asTrue(PRED_NAME, Vector[Constant](Constant("Walking_Kalypso"), Constant(timepoint.toString)))
      insertedFirstHalf = atom :: insertedFirstHalf
      builder.evidence += atom
    }

    // (STEP 2) Insert the first half (batch)
    builderBatch.evidence ++= insertedFirstHalf

    // and produce the resulting atom evidence DBs (contains 5 insertions)
    val resultIncrementalDB1 = builder.result().db
    val resultBatchDB1 = builderBatch.result().db

    // (STEP 3) Insert the second half (incrementally)
    var insertedSecondHalf = List.empty[EvidenceAtom]
    for (timepoint <- 6 to TIME_DOMAIN_SIZE) {
      val atom = EvidenceAtom.asTrue(PRED_NAME, Vector[Constant](Constant("Walking_Kalypso"), Constant(timepoint.toString)))
      insertedSecondHalf = atom :: insertedSecondHalf
      builder.evidence += atom
    }

    // (STEP 4) Insert the second half (batch)
    builderBatch.evidence ++= insertedSecondHalf

    // and produce the resulting atom evidence DB (contains 10 insertions)
    val resultIncrementalDB2 = builder.result().db
    val resultBatchDB2 = builderBatch.result().db

    // All insertions:
    val insertedAll = insertedFirstHalf ::: insertedSecondHalf

    // ----- Perform checks for DB1 (half incremental insertions):

    describe("DB1 (half incremental insertions)") {
      it("should give correct number of known groundings for all predicates") {
        assert {
          resultIncrementalDB1.forall {
            case (signature, atomDB) => atomDB.numberOfKnown == initialKnown(signature)
          }
        }

      }

      it(s"should contain 5 evidence predicates of $PRED_NAME") {
        assert(resultIncrementalDB1(SIGNATURE).numberOfTrue == 5)
      }

      it(s"should contain all the inserted evidence predicates with the correct truth state") {

        assert {
          insertedFirstHalf.forall { atom =>
            val args = atom.terms.map(_.symbol)
            resultIncrementalDB1(SIGNATURE).contains(args) && resultIncrementalDB1(SIGNATURE)(args) == TRUE
          }
        }
      }
    }

    // ----- Perform checks for DB1 (half batch insertions):

    describe("DB1 (half batch insertions)") {
      it("should give correct number of known groundings for all predicates") {
        assert {
          resultBatchDB1.forall {
            case (signature, atomDB) => atomDB.numberOfKnown == initialKnown(signature)
          }
        }

      }

      it(s"should contain 5 evidence predicates of $PRED_NAME") {
        assert(resultBatchDB1(SIGNATURE).numberOfTrue == 5)
      }

      it(s"should contain all the inserted evidence predicates with the correct truth state") {

        assert {
          insertedFirstHalf.forall { atom =>
            val args = atom.terms.map(_.symbol)
            resultBatchDB1(SIGNATURE).contains(args) && resultBatchDB1(SIGNATURE)(args) == TRUE
          }
        }
      }
    }

    // ----- Perform checks for DB2 (all incremental insertions):

    describe("DB2 (all incremental insertions)") {
      it("should give correct number of known groundings for all predicates") {
        assert {
          resultIncrementalDB2.forall {
            case (signature, atomDB) => atomDB.numberOfKnown == initialKnown(signature)
          }
        }
      }

      it(s"should contain $TIME_DOMAIN_SIZE evidence predicates of $PRED_NAME") {
        assert(resultIncrementalDB2(SIGNATURE).numberOfTrue == TIME_DOMAIN_SIZE)
      }

      it(s"should contain all the inserted evidence predicates with the correct truth state") {

        assert {
          insertedAll.forall { atom =>
            val args = atom.terms.map(_.symbol)
            resultIncrementalDB2(SIGNATURE).contains(args) && resultIncrementalDB2(SIGNATURE)(args) == TRUE
          }
        }
      }
    }

    // ----- Perform checks for DB2 (all batch insertions):
    describe("DB2 (all batch insertions)") {
      it("should give correct number of known groundings for all predicates") {
        assert {
          resultBatchDB2.forall {
            case (signature, atomDB) => atomDB.numberOfKnown == initialKnown(signature)
          }
        }
      }

      it(s"should contain $TIME_DOMAIN_SIZE evidence predicates of $PRED_NAME") {
        assert(resultBatchDB2(SIGNATURE).numberOfTrue == TIME_DOMAIN_SIZE)
      }

      it(s"should contain all the inserted evidence predicates with the correct truth state") {

        assert {
          insertedAll.forall { atom =>
            val args = atom.terms.map(_.symbol)
            resultBatchDB2(SIGNATURE).contains(args) && resultBatchDB2(SIGNATURE)(args) == TRUE
          }
        }
      }
    }

  }

  describe("Evidence construction (groundings of a multiple predicates)") {

    val builder = EvidenceBuilder(samplePredicateSchemas, queryPredicates, hiddenPredicates, constantsDomain)

    var insertedCWATrue = List.empty[EvidenceAtom]
    var insertedCWAFalse = List.empty[EvidenceAtom]

    var insertedOWAFalse = List.empty[EvidenceAtom]
    var insertedOWAUnknown = List.empty[EvidenceAtom]

    // insert some evidence
    for (timepoint <- 1 to TIME_DOMAIN_SIZE) {

      // closed-world assumption predicate (as TRUE)
      val atomCWATrue = EvidenceAtom.asTrue("HappensAt", Vector[Constant](Constant("Walking_Kalypso"), Constant(timepoint.toString)))
      builder.evidence += atomCWATrue
      insertedCWATrue = atomCWATrue :: insertedCWATrue

      // closed-world assumption predicate (as FALSE) -> thus no effect
      val atomCWAFalse = EvidenceAtom.asFalse("HappensAt", Vector[Constant](Constant("Running_Ismene"), Constant(timepoint.toString)))
      builder.evidence += atomCWAFalse
      insertedCWAFalse = atomCWAFalse :: insertedCWAFalse

      // open-world assumption predicate (as FALSE)
      val atomOWAFalse = EvidenceAtom.asFalse("HoldsAt", Vector[Constant](Constant("meeting"), Constant(timepoint.toString)))
      builder.evidence += atomOWAFalse
      insertedOWAFalse = atomOWAFalse :: insertedOWAFalse

      // open-world assumption predicate (as UNKNOWN) -> thus no effect
      val atomOWAUnknown = EvidenceAtom.asUnknown("HoldsAt", Vector[Constant](Constant("moving"), Constant(timepoint.toString)))
      builder.evidence += atomOWAUnknown
      insertedOWAUnknown = atomOWAUnknown :: insertedOWAUnknown
    }

    // get result DB
    val resultDB = builder.result().db

    describe("CWA predicates as evidence") {
      val signature = AtomSignature("HappensAt", 2)
      val atomDB = resultDB(signature)

      val negatives = initialKnown(signature) - insertedCWATrue.size
      val positives = insertedCWATrue.size

      they(s"have ${initialKnown(signature)} known groundings") {
        assert(atomDB.numberOfKnown == initialKnown(signature))
      }

      they("have 0 unknown groundings") {
        assert(atomDB.numberOfUnknown == 0)
      }

      they(s"have ${insertedCWATrue.size} true groundings") {
        assert(atomDB.numberOfTrue == positives)
      }

      they(s"have $negatives false groundings") {
        assert(atomDB.numberOfFalse == negatives)
      }

      they(s"have the correct collection of true groundings") {
        assert(insertedCWATrue.forall(a => atomDB(a.terms.map(_.toText)) == TRUE))
      }

      they(s"have the correct collection of false groundings") {
        assert(insertedCWAFalse.forall(a => atomDB(a.terms.map(_.toText)) == FALSE))
      }

    }

    describe("OWA predicates with evidence") {
      val signature = AtomSignature("HoldsAt", 2)
      val atomDB = resultDB(signature)

      val positives = 0
      val negatives = insertedOWAFalse.size
      val known = positives + negatives
      val unknown = space(signature) - known

      they(s"have $known known groundings") {
        assert(atomDB.numberOfKnown == known)
      }

      they(s"have $unknown unknown groundings") {
        assert(atomDB.numberOfUnknown == unknown)
      }

      they(s"have $positives true groundings") {
        assert(atomDB.numberOfTrue == positives)
      }

      they(s"have $negatives false groundings") {
        assert(atomDB.numberOfFalse == negatives)
      }

      they(s"have the correct collection of unknown groundings") {
        assert(insertedOWAUnknown.forall(a => atomDB(a.terms.map(_.toText)) == UNKNOWN))
      }

      they(s"have the correct collection of false groundings") {
        assert(insertedOWAFalse.forall(a => atomDB(a.terms.map(_.toText)) == FALSE))
      }
    }

    describe("Predicates without evidence") {

      val (missingOWA, missingCWA) = predicatesMissingEvidence.partition(owaPredicates.contains)

      they("should have state DB") {
        assert(predicatesMissingEvidence.forall(resultDB.contains))
      }

      for {
        signature <- missingOWA
        if resultDB.contains(signature) // execute the following tests only when 'they("should have state DB")' passes
        atomDB = resultDB(signature)

        positives = 0
        negatives = 0
        known = positives + negatives
        unknown = space(signature) - known

      } describe(s"OWA groundings of $signature") {

        they(s"should have $known known groundings") {
          assert(atomDB.numberOfKnown == known)
        }

        they(s"should have $unknown unknown groundings") {
          assert(atomDB.numberOfUnknown == unknown)
        }

        they(s"should have $positives true groundings") {
          assert(atomDB.numberOfTrue == positives)
        }

        they(s"should have $negatives false groundings") {
          assert(atomDB.numberOfFalse == negatives)
        }

      }

      for {
        signature <- missingCWA
        if resultDB.contains(signature) // execute the following tests only when 'they("should have state DB")' passes
        atomDB = resultDB(signature)

        positives = 0
        negatives = space(signature)
        known = positives + negatives
        unknown = 0

      } describe(s"CWA groundings of $signature") {

        they(s"should have $known known groundings") {
          assert(atomDB.numberOfKnown == known)
        }

        they(s"should have $unknown unknown groundings") {
          assert(atomDB.numberOfUnknown == unknown)
        }

        they(s"should have $positives true groundings") {
          assert(atomDB.numberOfTrue == positives)
        }

        they(s"should have $negatives false groundings") {
          assert(atomDB.numberOfFalse == negatives)
        }
      }
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Evidence Builder (incremental addition of function mappings)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Incremental addition of function mappings using EvidenceBuilder") {
    val builder = EvidenceBuilder(samplePredicateSchemas, sampleFunctionSchema, queryPredicates, hiddenPredicates, constantsDomain)

    for ((retValue, symbol, args) <- sampleFunctionMappings)
      builder.functions += new FunctionMapping(retValue, symbol, args)

    val resultFM = builder.result().functionMappers

    it("should contain all inserted function mappings") {
      assert {
        sampleFunctionMappings.forall {
          case (retValue, symbol, args) =>
            val mappingOpt = resultFM(AtomSignature(symbol, args.size)).get(args)
            if (mappingOpt.isEmpty) false
            else mappingOpt.get == retValue
        }
      }
    }

    it("should not contain undefined function mappings") {
      assert {
        val mappingOpt = resultFM(AtomSignature("running", 1)).get(Vector("other"))
        if (mappingOpt.isEmpty) true
        else false
      }
    }
  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Evidence Builder (incremental addition of function mappings as auxiliary predicates)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Incremental addition of function mappings, converted to auxiliary predicates, using EvidenceBuilder") {
    val builder = EvidenceBuilder(
      samplePredicateSchemas, sampleFunctionSchema, queryPredicates,
      hiddenPredicates, constantsDomain, convertFunctions = true)

    // construct the schema of all auxiliary predicates
    val auxPredicateSchema = sampleFunctionSchema.map {
      case (originalSignature, (retType, argTypes)) =>
        val convertedSignature = AtomSignature(lomrf.AUX_PRED_PREFIX + originalSignature.symbol, originalSignature.arity + 1)
        val convertedTermTypes = argTypes.+:(retType)
        (convertedSignature, convertedTermTypes)
    }

    // compute the space for each auxiliary predicate (i.e., the number of possible groundings)
    val auxSpace = auxPredicateSchema map {
      case (signature, schema) =>
        signature -> schema.map(d => constantsDomain(d).size).product
    }

    // utility mapping for associating function symbol to function signature,
    // please note that in this test we assume that function symbols are unique
    val symbol2Signature = sampleFunctionSchema.keys.map { originalSignature =>
      val convertedSignature = AtomSignature(lomrf.AUX_PRED_PREFIX + originalSignature.symbol, originalSignature.arity + 1)
      originalSignature.symbol -> convertedSignature
    }.toMap

    // compute the number of positives for each function, i.e., the true groundings of the function.
    val numOfPositives = sampleFunctionMappings
      .groupBy(_._2)
      .map {
        case (symbol, entries) =>
          symbol2Signature(symbol) -> entries.size
      }

    for ((retValue, symbol, args) <- sampleFunctionMappings)
      builder.functions += new FunctionMapping(retValue, symbol, args)

    val evidence = builder.result()

    it("has empty functionMappers") {
      evidence.functionMappers.isEmpty should be(true)
    }

    it("contains all function schemas as auxiliary predicate schemas") {
      val edb = evidence.db
      val signatures = edb.keys.filterNot(samplePredicateSchemas.contains)
      assert(signatures.size == sampleFunctionSchema.size)

      assert {
        auxPredicateSchema.forall {
          case (convertedSignature, convertedTermTypes) =>
            edb.get(convertedSignature).exists(adb => adb.identity.schema == convertedTermTypes)
        }
      }

    }

    for (signature <- auxPredicateSchema.keys) describe(s"Groundings of the auxiliary predicate '$signature'") {
      val known = auxSpace(signature)
      val positives = numOfPositives(signature)
      val negatives = known - positives
      val unknown = 0
      val atomDB = evidence.db(signature)

      they(s"should have $known known groundings") {
        assert(atomDB.numberOfKnown == known)
      }

      they(s"should have $unknown unknown groundings") {
        assert(atomDB.numberOfUnknown == unknown)
      }

      they(s"should have $positives true groundings") {
        assert(atomDB.numberOfTrue == positives)
      }

      they(s"should have $negatives false groundings") {
        assert(atomDB.numberOfFalse == negatives)
      }
    }

  }

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: Evidence Builder (error handling)
  // ------------------------------------------------------------------------------------------------------------------
  describe("Evidence construction (error handling)") {

    val builder = EvidenceBuilder(samplePredicateSchemas, queryPredicates, hiddenPredicates, constantsDomain)

    val dummySchema = Map(
      AtomSignature("Foo", 1) -> Vector("bar"),
      AtomSignature("Bar", 1) -> Vector("foo"))

    val dummyConstantsDomain = Map(
      "foo" -> ConstantsSet((1 to 5).map(v => "F" + v)),
      "bar" -> ConstantsSet((1 to 5).map(v => "B" + v)))

    val dummyIDF = dummySchema.map {
      case (signature, schema) => signature -> AtomIdentityFunction(signature, schema, dummyConstantsDomain, 1)
    }

    val dummyBuilders = dummySchema.map {
      case (s, _) => s -> AtomEvidenceDBBuilder.CWA(dummyIDF(s))
    }

    val dummyDB = dummyBuilders.map(e => e._1 -> e._2.result())

    it("should throw IllegalArgumentException when inserting evidence atom with unknown signature") {
      intercept[IllegalArgumentException] {
        builder.evidence += EvidenceAtom.asTrue("Foo", Vector(Constant("bar")))
      }
    }

    it("should throw IllegalArgumentException when setting builders that are related to unknown signatures (using 'builder.withEvidenceBuilders(dummyBuilders)')") {
      intercept[IllegalArgumentException](builder.withEvidenceBuilders(dummyBuilders))
    }

    it("should throw IllegalArgumentException when setting builders that are related to unknown signatures (using 'builder.evidence() = dummyDB')") {
      intercept[IllegalArgumentException] {
        builder.evidence() = dummyDB
      }
    }

    it("should throw Exception when adding evidence with unknown constants") {
      intercept[NoSuchElementException] {
        builder.evidence += EvidenceAtom.asTrue("Next", Vector(Constant("100"), Constant("99")))
      }

    }

  }
}
