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
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */

package lomrf.util.collection.mutable

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.{Before, Test}


class TuplesDBSpecTest extends AssertionsForJUnit {
  private val verbose = true

  val db = new MyTuplesDB

  @Before def initialize() {
    println("INITIALIZATION (1)")
    db *= ("x", Set("x0", "x1", "x2"))
    db.printAll
    println("INITIALIZATION (2)")
    db *= ("y", Set("y0", "y1", "y2"))
    db.printAll
    println("INITIALIZATION (3)")
    db *= ("z", Set("z0", "z1", "z2"))
    db.printAll
    assertEquals(27, db.size)
  }


  @Test def validateBasic() {
    //deleting all "x", the number of tuples should remain at 27
    db ~= "x"
    db.printAll
    assertEquals(27, db.size)

    //deleting all "z", the number of tuples should remain at 27
    db ~= "z"
    db.printAll
    assertEquals(27, db.size)

    //However, when deleting all "y" (the last variable),
    //the number of tuples should be 0. Thus, tuples and
    //index should be empty!
    db ~= "y"
    db.printAll
    assertEquals(0, db.size)

    //insertions
    db *= ("z", Set("z0", "z1", "z2"))
    db.printAll
    assertEquals(3, db.size)
    db *= ("y", Set("y0", "y1", "y2"))
    assertEquals(9, db.size)
    db *= ("x", Set("x0", "x1", "x2"))
    assertEquals(27, db.size)

    //should ignore the following insertions,
    //thus the size remains the same
    db *= ("x", Set("x0", "x1", "x2"))
    assertEquals(27, db.size)
    db *= ("x", "B")
    assertEquals(27, db.size)
    db *= ("y", "y0")
    assertEquals(27, db.size)
    db *= ("z", Set("a", "b", "c", "d"))
    assertEquals(27, db.size)

    // new insertions
    db *= ("w", Set("W1", "W2"))
    db *= ("a", Set("a1", "a2"))
    assertEquals(108, db.size)
    db.printAll

    //delete specific constants
    db -= ("a", Set("a1"))
    db.printAll
    assertEquals(54, db.size)

    db -= ("a", Set("a1"))
    db.printAll
    assertEquals(54, db.size)

    db -= ("x", Set("a1"))
    db.printAll
    assertEquals(54, db.size)

    db -= ("x", Set("x0"))
    db.printAll
    assertEquals(36, db.size)

    db -= ("x", Set("x0"))
    assertEquals(36, db.size)
    db -= ("x", Set("x1"))
    db -= ("x", Set("x2"))
    db.printAll
    assertEquals(0, db.size)

  }

  @Test def validateValuesIterator() {
    val size = db.size
    val iter = db.valuesIterator
    var count = 0
    while (iter.hasNext) {
      iter.next()
      count += 1
    }
    assertEquals(size, count)

    val iter2 = db.valuesIterator(List("x", "z"))
    count = 0
    while (iter2.hasNext) {
      iter2.next()
      count += 1
    }
    assertEquals(size, count)
  }

  //stupid conversion for easy testing
  private def convert(source: collection.Map[String, String]): (collection.Map[String, Int], List[String]) = {
    var idx: Int = source.size
    var values = List[String]()
    var mapping = Map[String, Int]()
    for ((k, v) <- source) {
      mapping = Map[String, Int](k -> idx) ++ mapping
      values = v :: values
      idx -= 1
    }
    return (mapping, values)
  }

  class MyTuplesDB extends TuplesDB[String, String]() {
    def printTuples() {
      println("tuples{")
      print("  Variables: < ")
      this.variables.foreach(v => print(v + " "))
      println(">")
      val iter = this.valuesIterator
      while (iter.hasNext) {
        println("  " + iter.next)
      }
      println("}")
    }

    def printIndexes() {
      println("index{")
      this.getIndex.foreach {
        entry =>
          println("  " + entry._1 + "{")
          entry._2.foreach(p => println("\t" + p.toString))
          println("   }")
      }
      println("}\n\n")
    }

    def printAll {
      if (verbose) {
        printTuples()
        printIndexes()
      }
    }
  }
}