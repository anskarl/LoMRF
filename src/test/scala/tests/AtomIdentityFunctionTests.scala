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

package tests


import lomrf.logic.AtomSignature
import lomrf.mln.model.MLN
import gnu.trove.set.hash.TIntHashSet
import lomrf.util.{Utilities, Cartesian, AtomIdentityFunction}
import org.scalatest.{Matchers, FunSuite}

/**
 * @author Anastasios Skarlatidis
 */

class AtomIdentityFunctionTests extends FunSuite with Matchers {

  val verboseMsg = true

  val sep = System.getProperty("file.separator")
  val projectPath = System.getProperty("user.dir") + sep
  private val testFilesPath = System.getProperty("user.dir") + sep + "data" + sep + "tests" + sep

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
  val mln = MLN(
    testFilesPath + "naive.mln",
    testFilesPath + "Empty.db",
    queryAtoms,
    cwa)


  // --------------------------------------------
  // --- Utility functions:
  // --------------------------------------------
  private implicit def toStr(iterable: Iterable[String]): String = "(" + iterable.map(_.toString).reduceLeft(_ + "," + _) + ")"

  private def createAIF(signature: AtomSignature, mln: MLN, startID: Int): AtomIdentityFunction = {
    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find the schema of predicate: " + signature))
    AtomIdentityFunction(signature, schema, mln.constants, startID)
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
  test("An atom identity function is 1-1.") {
    import System._


    val signature = AtomSignature("Alpha", 3)

    // Create an atom identity function
    val initStartTime = currentTimeMillis
    val identityFunction = createAIF(signature, mln, 1)
    println(Utilities.msecTimeToTextUntilNow("Identity function initialisation", initStartTime))

    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find signature: " + signature + " in the produced MLN."))

    val domain = for (s <- schema) yield mln.constants(s)
    val expectedNumOfGroundings = domain.map(_.size).product

    val cartesianIterator = Cartesian.CartesianIterator(domain)
    val idSet = new TIntHashSet()

    var totalEnc = 0L
    var totalDec = 0L

    val symbol = identityFunction.signature.symbol

    while (cartesianIterator.hasNext) {
      val product = cartesianIterator.next() // Seq[String]
      if (verboseMsg) print("ENC[ " + symbol + "(" + product.map(_.toString).reduceLeft(_ + "," + _) + ") ")
      val startEncTime = nanoTime
      val id = identityFunction.encode(product)
      totalEnc += nanoTime - startEncTime
      if (verboseMsg) print(" -> id:=" + id)
      val isUnique = idSet.add(id)

      if (!isUnique && verboseMsg) println("] FAILURE id:= " + id + " is not unique, identity function is not 1-1.")

      isUnique should be true

      if (verboseMsg) {
        println("]")
        print("DEC[ id:=" + id)
      }

      val startDecTime = nanoTime
      val dec = identityFunction.decode(id).getOrElse(sys.error("Cannot decode id: " + id))

      totalDec += nanoTime - startDecTime
      if (verboseMsg) print(" -> " + symbol + "(" + dec.map(_.toString).reduceLeft(_ + "," + _) + ")")
      val nid = identityFunction.encode(dec)

      if (verboseMsg) {
        if (nid != id) println("] FAILURE! " + nid + " != " + id)
        else println("] OK")
      }


      id shouldEqual nid


    }
    println(Utilities.nsecTimeToText("Total time producing IDs ", totalEnc))
    println(Utilities.nsecTimeToText("Average time of producing an ID ", totalEnc / expectedNumOfGroundings))
    println(Utilities.nsecTimeToText("Total time decoding IDs ", totalDec))
    println(Utilities.nsecTimeToText("Average time of decoding an ID ", totalDec / expectedNumOfGroundings))
    println("Expected number of groundings: " + expectedNumOfGroundings)
    println("Stored products: " + idSet.size)

    idSet.size shouldEqual expectedNumOfGroundings

  }

  test("Encoding benchmark 1 --- Discrete Event Calculus Knowledge Base.") {
    import System._

    val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
    val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Close", 4), AtomSignature("Next", 2))
    val owa = Set[AtomSignature](AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))
    val mln = MLN(
      testFilesPath + "DEC_TEST.mln",
      testFilesPath + "Empty.db",
      queryAtoms,
      cwa, owa)

    val signature = AtomSignature("Next", 2)

    val initStartTime = currentTimeMillis
    val identityFunction = createAIF(signature, mln, 1)
    println(Utilities.msecTimeToTextUntilNow("Identity function initialisation", initStartTime))

    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find " + signature))

    val domain = for (s <- schema) yield mln.constants(s)
    val expectedNumOfGroundings = domain.map(_.size).product

    val cartesianIterator = Cartesian.CartesianIterator(domain)
    //val symbol = identityFunction.signature.symbol

    var totalEnc = 0L
    var counter = 0

    while (cartesianIterator.hasNext) {
      val product = cartesianIterator.next()
      val startEncTime = nanoTime
      val id = identityFunction.encode(product)
      totalEnc += nanoTime - startEncTime
      counter += 1
    }

    println(Utilities.nsecTimeToText("Total time producing IDs ", totalEnc))
    println(Utilities.nsecTimeToText("Avg time producing ID ", totalEnc / expectedNumOfGroundings))
    println("Expected number of groundings: " + expectedNumOfGroundings)
    println("Produced  products: " + counter)
    counter shouldEqual expectedNumOfGroundings
  }

  test("Encoding benchmark 2 --- KB with a large domain.") {
    import System._

    val queryAtoms = Set[AtomSignature](AtomSignature("AdvisedBy", 2))
    val cwa = Set[AtomSignature](AtomSignature("GradStudent", 1), AtomSignature("Prof", 1), AtomSignature("TA", 2), AtomSignature("SameGroup", 2), AtomSignature("SameGroup", 2))
    val owa = Set[AtomSignature]()
    val mln = MLN(
      testFilesPath + "TestUniversityBIG.mln",
      testFilesPath + "Empty.db",
      queryAtoms,
      cwa, owa)

    val signature = AtomSignature("AdvisedBy", 2)

    val initStartTime = currentTimeMillis
    val identityFunction = createAIF(signature, mln, 1)
    println(Utilities.msecTimeToTextUntilNow("Identity function initialisation", initStartTime))

    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find " + signature))

    val domain = for (s <- schema) yield mln.constants(s)
    val expectedNumOfGroundings = domain.map(_.size).product

    val cartesianIterator = Cartesian.CartesianIterator(domain)
    //val symbol = identityFunction.signature.symbol

    var totalEnc = 0L
    var counter = 0

    while (cartesianIterator.hasNext) {
      val product = cartesianIterator.next()
      val startEncTime = nanoTime
      val id = identityFunction.encode(product)
      totalEnc += nanoTime - startEncTime
      counter += 1
    }

    println(Utilities.nsecTimeToText("Total time producing IDs ", totalEnc))
    println(Utilities.nsecTimeToText("Average time producing ID ", totalEnc / expectedNumOfGroundings))
    println("Expected number of groundings: " + expectedNumOfGroundings)
    println("Produced  products: " + counter)
    counter shouldEqual  expectedNumOfGroundings
  }

  test("Filter all instances of Alpha/3 --- returns all instances") {
      val signature = AtomSignature("Alpha", 3)
      val identityFunction = createAIF(signature, mln, 1)
      val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find " + signature))
      if (verboseMsg) {
        println("predicate: " + signature)
        println("schema: " + schema)
      }

      val domain = for (s <- schema) yield mln.constants(s)
      val expectedNumOfGroundings = domain.map(_.size).product
      val cartesianIterator = Cartesian.CartesianIterator(domain)

      if (verboseMsg) println("ALL:")
      while (cartesianIterator.hasNext) {
        val elements = cartesianIterator.next()
        val id = identityFunction.encode(elements)
        if (verboseMsg) println(id + "\t" + elements)
      }

      val query = Map[String, String]()
      if (verboseMsg) println("\nFILTERED: " + query)
      val fIter = identityFunction.matchesIterator(query)
      var counter = 0
      while (fIter.hasNext) {
        val id = fIter.next()
        val decElements = identityFunction.decode(id).getOrElse(sys.error("Cannot decode: " + id))
        if (verboseMsg) println(id + "\t" + toStr(decElements))

        counter += 1
      }
      if (verboseMsg) println("counted: " + counter)
      counter shouldEqual expectedNumOfGroundings
    }

  test("Filter all instances of Alpha/3 where fluent is equal to 'F1'") {
    import scala.collection.mutable

    val signature = AtomSignature("Alpha", 3)
    val identityFunction = createAIF(signature, mln, 1)
    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find: " + signature))

    if (verboseMsg) {
      println("predicate: " + signature)
      println("schema: " + schema)
    }


    val domain = for (s <- schema) yield mln.constants(s)
    //val expectedNumOfGroundings = domain.map(_.size).product
    val cartesianIterator = Cartesian.CartesianIterator(domain)

    val collected = mutable.HashMap[Int, Iterable[String]]()

    if (verboseMsg) println("ALL:")

    while (cartesianIterator.hasNext) {
      val elements = cartesianIterator.next()
      val id = identityFunction.encode(elements)
      if (verboseMsg) println(id + "\t" + elements)

      if (elements(1) == "F1") collected.put(id, elements)
    }

    //val query = Map[String, String](("fluent", "F1"))
    val query = Map("fluent" -> "F1")

    if (verboseMsg) println("\nFILTERED: " + query)

    val fIter = identityFunction.matchesIterator(query)
    var counter = 0

    while (fIter.hasNext) {
      val id = fIter.next()
      val decElements = identityFunction.decode(id).getOrElse(sys.error("Cannot decode: " + id))
      val collectedElements = collected(id)
      decElements.zip(collectedElements).foreach(entries => entries._1 shouldEqual entries._2)

      if (verboseMsg) println(id + "\t" + toStr(decElements) + " == " + toStr(collectedElements))

      counter += 1
    }

    if (verboseMsg) {
      println("counted: " + counter)
      println("collected: " + collected.size)
    }

    counter shouldEqual collected.size
  }



  test("Filter all instances of Alpha/3 where fluent and time are equal to 'F1' and '0' repsectively") {
    import scala.collection.mutable

    val signature = AtomSignature("Alpha", 3)
    val identityFunction = createAIF(signature, mln, 1)
    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find " + signature))
    if (verboseMsg) {
      println("predicate: " + signature)
      println("schema: " + schema)
    }

    val domain = for (s <- schema) yield mln.constants(s)
    val cartesianIterator = Cartesian.CartesianIterator(domain)

    val collected = mutable.HashMap[Int, Iterable[String]]()

    if (verboseMsg) println("ALL:")
    while (cartesianIterator.hasNext) {
      val elements = cartesianIterator.next()
      val id = identityFunction.encode(elements)
      if (verboseMsg) println(id + "\t" + elements)
      if (elements(1) == "F1" && elements(2) == "0") {
        collected.put(id, elements)
      }
    }

    val query = Map[String, String](("fluent", "F1"), ("time", "0"))
    if (verboseMsg) println("\nFILTERED: " + query)
    val fIter = identityFunction.matchesIterator(query)
    var counter = 0
    while (fIter.hasNext) {
      val id = fIter.next()
      val decElements = identityFunction.decode(id) match {
        case Some(elements) => elements
        case None => sys.error("Cannot decode: " + id)
      }
      val collectedElements = collected(id)
      decElements.zip(collectedElements).foreach(entries => entries._1 shouldEqual entries._2)
      if (verboseMsg) println(id + "\t" + toStr(decElements) + " == " + toStr(collectedElements))

      counter += 1
    }
    if (verboseMsg) {
      println("counted: " + counter)
      println("collected: " + collected.size)
    }

    counter shouldEqual collected.size
  }

  test("Filter all instances of Alpha/3 where fluent, time and event are equal to 'F1', '0' and 'E3' repsectively") {
    import scala.collection.mutable

    val signature = AtomSignature("Alpha", 3)
    val identityFunction = createAIF(signature, mln, 1)
    val schema = mln.getSchemaOf(signature).getOrElse(sys.error("Cannot find " + signature))
    if (verboseMsg) {
      println("predicate: " + signature)
      println("schema: " + schema)
    }


    val domain = for (s <- schema) yield mln.constants(s)
    val cartesianIterator = Cartesian.CartesianIterator(domain)

    val collected = mutable.HashMap[Int, Iterable[String]]()

    if (verboseMsg) println("ALL:")
    while (cartesianIterator.hasNext) {
      val elements = cartesianIterator.next()
      val id = identityFunction.encode(elements)
      if (verboseMsg) println(id + "\t" + elements)
      if (elements(0) == "E3" && elements(1) == "F1" && elements(2) == "0") {
        collected.put(id, elements)
      }
    }

    val query = Map[String, String]("fluent" -> "F1", "time" -> "0", "event" -> "E3")
    if (verboseMsg) println("\nFILTERED: " + query)
    val fIter = identityFunction.matchesIterator(query)
    var counter = 0
    while (fIter.hasNext) {
      val id = fIter.next()
      val decElements = identityFunction.decode(id).getOrElse(sys.error("Cannot decode: " + id))
      val collectedElements = collected(id)
      decElements.zip(collectedElements).foreach(entries => entries._1 shouldEqual entries._2)
      if (verboseMsg) println(id + "\t" + toStr(decElements) + " == " + toStr(collectedElements))

      counter += 1
    }
    if (verboseMsg) {
      println("counted: " + counter)
      println("collected: " + collected.size)
    }
    counter shouldEqual collected.size
  }


}