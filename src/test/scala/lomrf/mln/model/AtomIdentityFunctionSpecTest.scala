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

package lomrf.mln.model

import gnu.trove.set.hash.TIntHashSet
import lomrf.logic.AtomSignature
import lomrf.util.Cartesian
import lomrf.util.collection.GlobalIndexPartitioned
import lomrf.util.time._
import org.scalatest.{ FunSpec, Matchers }
import lomrf.tests.TestData
import lomrf.util.io._

import scala.language.implicitConversions

final class AtomIdentityFunctionSpecTest extends FunSpec with Matchers {

  private val testFilesPath = TestData.TestFilesPath
  private val naiveMLN = testFilesPath / "naive.mln"
  private val emptyEvidence = testFilesPath / "Empty.db"

  // Naive example: We have two predicates: Alpha/3 and Beta/2
  // 1. query atoms:
  val queryAtoms = Set[AtomSignature](AtomSignature("Beta", 2))
  // 2. evidence atoms:
  val cwa = Set[AtomSignature](AtomSignature("Alpha", 3))

  // Construct a MLN (given an empty evidence db):
  // fluent={F1,F2}
  // event={E1,E2,E3}
  // time={0,...,7}
  // Alpha(event, fluent, time)
  // Beta(fluent,time)
  // Alpha(e,f,t) => Beta(f,t).
  info("Constructing MLN from '" + naiveMLN + "' with evidence '" + emptyEvidence + "'")
  val mln = MLN.fromFile(naiveMLN, queryAtoms, emptyEvidence, cwa)

  // --------------------------------------------
  // --- Utility functions:
  // --------------------------------------------
  private implicit def toStr(iterable: Iterable[String]): String = s"(${iterable.mkString(",")})"

  private def mkAtomIdentityFunction(signature: AtomSignature, mln: MLN, startID: Int): AtomIdentityFunction = {
    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find the schema of predicate: " + signature))
    AtomIdentityFunction(signature, schema, mln.evidence.constants, startID)
  }

  // --------------------------------------------
  // --- Testing cases:
  // --------------------------------------------

  /**
    * CASE 1 --- Test if the atom identity function is 1-1:
    *
    * The identity function should create a unique id from a
    * ground atom and by decoding that id we must get the same
    * ground atom.
    */
  describe("An atom identity function is 1-1.") {
    import System._

    val signature = AtomSignature("Alpha", 3)

    // Create an atom identity function
    val initStartTime = currentTimeMillis
    val identityFunction = mkAtomIdentityFunction(signature, mln, 1)
    info(msecTimeToTextUntilNow("Identity function initialisation", initStartTime))

    val schema = mln.getSchemaOf(signature).getOrElse(sys.error(s"Cannot find signature: '$signature' in the produced MLN."))

    val domain = for (s <- schema) yield mln.evidence.constants(s)
    val expectedNumOfGroundings = domain.map(_.size).product

    info("Initialising Cartesian iterator")
    val cartesianIterator = Cartesian.CartesianIterator(domain)
    val idSet = new TIntHashSet()

    var totalEnc = 0L
    var totalDec = 0L

    val symbol = identityFunction.signature.symbol

    var allProductsStr = Set[String]()

    while (cartesianIterator.hasNext) {
      val product = cartesianIterator.next() // get the next collection of Constants
      allProductsStr += product.toSeq

      describe("Atom '" + symbol + "(" + product + ")") {

        val startEncTime = nanoTime
        val encodedID = identityFunction.encode(product)
        totalEnc += nanoTime - startEncTime
        val isUnique = idSet.add(encodedID)

        val startDecTime = nanoTime
        val decoded = identityFunction.decode(encodedID).getOrElse(sys.error("Cannot decode id: " + encodedID))
        totalDec += nanoTime - startDecTime

        it("can be encoded to a unique id") {
          isUnique shouldEqual true
        }

        it("the id can be decoded to the constants '" + product + "'") {
          decoded shouldEqual product
        }

      }
    }

    describe("The total number of Cartesian products") {
      it(s"should be equal to the number of expected groundings ( = $expectedNumOfGroundings)") {
        idSet.size shouldEqual expectedNumOfGroundings
      }
    }

    info(nsecTimeToText("Total time producing IDs ", totalEnc))
    info(nsecTimeToText("Average time of producing an ID ", totalEnc / expectedNumOfGroundings))
    info(nsecTimeToText("Total time decoding IDs ", totalDec))
    info(nsecTimeToText("Average time of decoding an ID ", totalDec / expectedNumOfGroundings))

    describe("Filter all instances of Alpha/3") {

      val query = Map[String, String]()
      info("FILTER: " + query)
      val fIter = identityFunction.matchesIterator(query)
      var counter = 0

      it("returns valid ids") {
        while (fIter.hasNext) {
          val filteredID = fIter.next()
          assert(idSet.contains(filteredID))

          val decElements = identityFunction.decode(filteredID).getOrElse(sys.error("Cannot decode: " + filteredID))
          assert(allProductsStr.contains(decElements))

          counter += 1
        }
      }

      it("returns all instances") {
        counter shouldEqual expectedNumOfGroundings
      }
    }

    describe("Filter all instances of Alpha/3 where fluent is equal to 'F1'") {
      import scala.collection.mutable

      val cartesianIterator = Cartesian.CartesianIterator(domain)

      val collected = mutable.HashMap[Int, Iterable[String]]()

      while (cartesianIterator.hasNext) {
        val elements = cartesianIterator.next()
        val id = identityFunction.encode(elements)
        if (elements(1) == "F1") collected.put(id, elements)
      }

      val query = Map("fluent" -> "F1")
      info("\nFILTER: " + query)

      val fIter = identityFunction.matchesIterator(query)

      var counter = 0
      it("returns valid ids") {
        while (fIter.hasNext) {
          val filteredID = fIter.next()
          assert(idSet.contains(filteredID))

          val decElements = identityFunction.decode(filteredID).getOrElse(sys.error("Cannot decode: " + filteredID))
          assert(allProductsStr.contains(decElements))

          val collectedElements = collected(filteredID)
          decElements.zip(collectedElements).foreach(entries => entries._1 shouldEqual entries._2)

          counter += 1
        }
      }

      it("returns " + collected.size + " instances") {
        counter shouldEqual collected.size
      }
    }

    describe("Filter all instances of Alpha/3 where fluent and time are equal to 'F1' and '0' respectively") {
      import scala.collection.mutable

      val cartesianIterator = Cartesian.CartesianIterator(domain)

      val collected = mutable.HashMap[Int, Iterable[String]]()

      while (cartesianIterator.hasNext) {
        val elements = cartesianIterator.next()
        val id = identityFunction.encode(elements)

        if (elements(1) == "F1" && elements(2) == "0") {
          collected.put(id, elements)
        }
      }

      val query = Map[String, String](("fluent", "F1"), ("time", "0"))
      info("\nFILTER: " + query)

      val fIter = identityFunction.matchesIterator(query)
      var counter = 0

      it("returns valid ids") {
        while (fIter.hasNext) {
          val filteredID = fIter.next()
          assert(idSet.contains(filteredID))

          val decElements = identityFunction.decode(filteredID).getOrElse(sys.error("Cannot decode: " + filteredID))
          assert(allProductsStr.contains(decElements))

          val collectedElements = collected(filteredID)
          decElements.zip(collectedElements).foreach(entries => entries._1 shouldEqual entries._2)

          counter += 1
        }
      }

      it("returns " + collected.size + " instances") {
        counter shouldEqual collected.size
      }
    }

    describe("Filter all instances of Alpha/3 where fluent, time and event are equal to 'F1', '0' and 'E3' respectively") {
      import scala.collection.mutable

      val cartesianIterator = Cartesian.CartesianIterator(domain)

      val collected = mutable.HashMap[Int, Iterable[String]]()

      while (cartesianIterator.hasNext) {
        val elements = cartesianIterator.next()
        val id = identityFunction.encode(elements)
        if (elements(0) == "E3" && elements(1) == "F1" && elements(2) == "0") {
          collected.put(id, elements)
        }
      }

      val query = Map[String, String]("fluent" -> "F1", "time" -> "0", "event" -> "E3")

      info("\nFILTERED: " + query)
      val fIter = identityFunction.matchesIterator(query)
      var counter = 0
      it("returns valid ids") {
        while (fIter.hasNext) {
          val filteredID = fIter.next()
          assert(idSet.contains(filteredID))

          val decElements = identityFunction.decode(filteredID).getOrElse(sys.error("Cannot decode: " + filteredID))
          assert(allProductsStr.contains(decElements))

          val collectedElements = collected(filteredID)
          decElements.zip(collectedElements).foreach(entries => entries._1 shouldEqual entries._2)

          counter += 1
        }
      }

      it("returns " + collected.size + " instances") {
        counter shouldEqual collected.size
      }
    }

  }

  describe("Benchmark 1 --- Encoding/Decoding a Discrete Event Calculus Knowledge Base.") {
    import System._

    val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
    val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2))
    val owa = Set[AtomSignature](AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))
    val mln = MLN.fromFile(testFilesPath / "DEC_TEST.mln", queryAtoms, testFilesPath / "Empty.db", cwa, owa)

    val signature = AtomSignature("Next", 2)

    val initStartTime = currentTimeMillis
    val identityFunction = mkAtomIdentityFunction(signature, mln, 1)
    info(msecTimeToTextUntilNow("Identity function initialisation", initStartTime))

    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find " + signature))

    val domain = for (s <- schema) yield mln.evidence.constants(s)
    val expectedNumOfGroundings = domain.map(_.size).product.toLong

    val cartesianIterator = Cartesian.CartesianIterator(domain)

    var counter = 0

    it("returns valid ids") {
      var totalEnc = 0L
      var totalDec = 0L

      while (cartesianIterator.hasNext) {
        val product = cartesianIterator.next()

        val startEncTime = System.nanoTime()
        val id = identityFunction.encode(product)
        val endEncTime = System.nanoTime()

        totalEnc = totalEnc + (endEncTime - startEncTime)

        val startDecTime = System.nanoTime()
        val decElements = identityFunction.decode(id).getOrElse(sys.error("Cannot decode: " + id))
        val endDecTime = System.nanoTime()
        totalDec += (endDecTime - startDecTime)

        product shouldEqual decElements

        counter += 1
      }

      info("totalEnc= " + totalEnc)

      info(nsecTimeToText("Total time encoding IDs ", totalEnc))
      info(nsecTimeToText("Average time encoding ID ", totalEnc / expectedNumOfGroundings))

      info(nsecTimeToText("Total time decoding IDs ", totalDec))
      info(nsecTimeToText("Average time decoding ID ", totalDec / expectedNumOfGroundings))

    }

    describe("The total number of Cartesian products") {
      it("should be equal to the number of expected groundings ( = " + expectedNumOfGroundings + ")") {
        counter shouldEqual expectedNumOfGroundings
      }
    }
  }

  describe("Benchmark 2 --- Encoding/Decoding a KB with a large domain.") {
    import System._

    val queryAtoms = Set[AtomSignature](AtomSignature("AdvisedBy", 2))
    val cwa = Set[AtomSignature](AtomSignature("GradStudent", 1), AtomSignature("Prof", 1), AtomSignature("TA", 2), AtomSignature("SameGroup", 2), AtomSignature("SameGroup", 2))
    val owa = Set[AtomSignature]()
    val mln = MLN.fromFile(testFilesPath / "TestUniversityBIG.mln", queryAtoms, testFilesPath / "Empty.db", cwa, owa)

    val signature = AtomSignature("AdvisedBy", 2)

    val initStartTime = currentTimeMillis
    val identityFunction = mkAtomIdentityFunction(signature, mln, 1)
    info(msecTimeToTextUntilNow("Identity function initialisation", initStartTime))

    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find " + signature))

    val domain = for (s <- schema) yield mln.evidence.constants(s)
    val expectedNumOfGroundings = domain.map(_.size).product

    val cartesianIterator = Cartesian.CartesianIterator(domain)

    var counter = 0

    it("returns valid ids") {
      var totalEnc = 0L
      var totalDec = 0L

      while (cartesianIterator.hasNext) {
        val product = cartesianIterator.next()

        val startEncTime = nanoTime
        val id = identityFunction.encode(product)
        totalEnc += nanoTime - startEncTime

        val startDecTime = nanoTime
        val decElements = identityFunction.decode(id).getOrElse(sys.error("Cannot decode: " + id))
        totalDec += nanoTime - startDecTime

        product shouldEqual decElements

        counter += 1
      }

      info(nsecTimeToText("Total time encoding IDs ", totalEnc))
      info(nsecTimeToText("Average time encoding ID ", totalEnc / expectedNumOfGroundings))

      info(nsecTimeToText("Total time decoding IDs ", totalDec))
      info(nsecTimeToText("Average time decoding ID ", totalDec / expectedNumOfGroundings))
    }

    describe("The total number of Cartesian products") {
      it("should be equal to the number of expected groundings ( = " + expectedNumOfGroundings + ")") {
        counter shouldEqual expectedNumOfGroundings
      }
    }
  }

  describe("Decoding using global constant IDs") {

    // Constants:
    val builder = ConstantsDomainBuilder()

    builder.of("domainA") ++= (1 to 10).map(n => "A" + n)
    builder.of("domainB") ++= (1 to 7).map(n => "B" + n)
    builder.of("domainC") ++= (1 to 10000).map(n => "C" + n)
    builder.of("domainD") ++= (1 to 2).map(n => "D" + n)

    val constants = builder.result()

    val domainsValues = new Array[ConstantsSet](constants.size)

    for (((k, v), idx) <- constants.zipWithIndex) {
      domainsValues(idx) = v
    }

    // globally indexed constants
    val gConstants = GlobalIndexPartitioned[ConstantsSet, String](domainsValues)

    // Foo(domainA, domainC, domainD)
    val schema = Vector("domainA", "domainB", "domainC", "domainD")
    val signature = AtomSignature("Foo", schema.length)

    val expectedNumOfGroundings = domainsValues.map(_.size).product.toLong
    val cartesianIterator = Cartesian.CartesianIterator(domainsValues)

    val identityFunction = AtomIdentityFunction(signature, schema, constants, 1)

    var allProductsStr = Set[String]()

    it("extracts valid ids") {
      var totalEnc = 0L
      var totalExt = 0L

      while (cartesianIterator.hasNext) {
        val product = cartesianIterator.next() // get the next collection of Constants
        allProductsStr += product.toSeq

        val startEncTime = System.nanoTime
        val encodedID = identityFunction.encode(product)
        totalEnc += System.nanoTime - startEncTime

        if (encodedID == AtomIdentityFunction.IDENTITY_NOT_EXIST) fail(s"Failed to encode Foo(${product.mkString(",")})")

        val startExtTime = System.nanoTime
        val extracted = identityFunction.extract(encodedID).getOrElse(fail("Cannot extract id: " + encodedID))
        totalExt += System.nanoTime - startExtTime

        val decoded = extracted.map(id => gConstants(id))

        decoded shouldEqual product
      }

      info(nsecTimeToText("Total time encoding IDs ", totalEnc))
      info(nsecTimeToText("Average time encoding ID ", totalEnc / expectedNumOfGroundings))

      info(nsecTimeToText("Total time extracting IDs ", totalExt))
      info(nsecTimeToText("Average time extracting ID ", totalExt / expectedNumOfGroundings))
    }
  }
}
