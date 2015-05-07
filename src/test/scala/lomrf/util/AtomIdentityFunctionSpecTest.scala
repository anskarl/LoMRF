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

package lomrf.util


import lomrf.logic.AtomSignature
import lomrf.mln.model.MLN
import gnu.trove.set.hash.TIntHashSet
import org.scalatest.{FunSpec, Matchers}

final class AtomIdentityFunctionSpecTest extends FunSpec with Matchers {


  private val sep = System.getProperty("file.separator")
  private val testFilesPath = System.getProperty("user.dir") + sep + "Examples" + sep + "data" + sep + "tests" + sep
  private val naiveMLN = testFilesPath + "naive.mln"
  private val emptyEvidence = testFilesPath + "Empty.db"


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
  private implicit def toStr(iterable: Iterable[String]): String = "(" + iterable.map(_.toString).reduceLeft(_ + "," + _) + ")"

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
    info(Utilities.msecTimeToTextUntilNow("Identity function initialisation", initStartTime))

    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find signature: " + signature + " in the produced MLN."))

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
      allProductsStr += product

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
      it("should be equal to the number of expected groundings ( = " + expectedNumOfGroundings + ")") {
        idSet.size shouldEqual expectedNumOfGroundings
      }
    }

    info(Utilities.nsecTimeToText("Total time producing IDs ", totalEnc))
    info(Utilities.nsecTimeToText("Average time of producing an ID ", totalEnc / expectedNumOfGroundings))
    info(Utilities.nsecTimeToText("Total time decoding IDs ", totalDec))
    info(Utilities.nsecTimeToText("Average time of decoding an ID ", totalDec / expectedNumOfGroundings))

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
    val mln = MLN.fromFile(testFilesPath + "DEC_TEST.mln", queryAtoms, testFilesPath + "Empty.db", cwa, owa)

    val signature = AtomSignature("Next", 2)

    val initStartTime = currentTimeMillis
    val identityFunction = mkAtomIdentityFunction(signature, mln, 1)
    info(Utilities.msecTimeToTextUntilNow("Identity function initialisation", initStartTime))

    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find " + signature))

    val domain = for (s <- schema) yield mln.evidence.constants(s)
    val expectedNumOfGroundings = domain.map(_.size).product

    val cartesianIterator = Cartesian.CartesianIterator(domain)

    var totalEnc: Long = 0
    var counter = 0

    it("returns valid ids") {
      while (cartesianIterator.hasNext) {
        val product = cartesianIterator.next()
        val startEncTime = nanoTime
        val id = identityFunction.encode(product)
        val decElements = identityFunction.decode(id).getOrElse(sys.error("Cannot decode: " + id))
        product shouldEqual decElements

        totalEnc += nanoTime - startEncTime
        counter += 1
      }
    }

    info(Utilities.nsecTimeToText("Total time producing IDs ", totalEnc))
    info(Utilities.nsecTimeToText("Avg time producing ID ", totalEnc / expectedNumOfGroundings))
    info("Expected number of groundings: " + expectedNumOfGroundings)
    info("Produced  products: " + counter)

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
    val mln = MLN.fromFile(testFilesPath + "TestUniversityBIG.mln", queryAtoms, testFilesPath + "Empty.db", cwa, owa)

    val signature = AtomSignature("AdvisedBy", 2)

    val initStartTime = currentTimeMillis
    val identityFunction = mkAtomIdentityFunction(signature, mln, 1)
    println(Utilities.msecTimeToTextUntilNow("Identity function initialisation", initStartTime))

    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find " + signature))

    val domain = for (s <- schema) yield mln.evidence.constants(s)
    val expectedNumOfGroundings = domain.map(_.size).product

    val cartesianIterator = Cartesian.CartesianIterator(domain)
    //val symbol = identityFunction.signature.symbol

    var totalEnc = 0L
    var counter = 0

    it("returns valid ids") {
      while (cartesianIterator.hasNext) {
        val product = cartesianIterator.next()
        val startEncTime = nanoTime
        val id = identityFunction.encode(product)
        val decElements = identityFunction.decode(id).getOrElse(sys.error("Cannot decode: " + id))
        product shouldEqual decElements

        totalEnc += nanoTime - startEncTime
        counter += 1
      }
    }

    info(Utilities.nsecTimeToText("Total time producing IDs ", totalEnc))
    info(Utilities.nsecTimeToText("Average time producing ID ", totalEnc / expectedNumOfGroundings))
    info("Expected number of groundings: " + expectedNumOfGroundings)
    info("Produced  products: " + counter)



    describe("The total number of Cartesian products") {
      it("should be equal to the number of expected groundings ( = " + expectedNumOfGroundings + ")") {
        counter shouldEqual expectedNumOfGroundings
      }
    }
  }


}
