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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert._
import lomrf.util.Cartesian.CartesianIteratorMap

/**
 * @author Anastasios Skarlatidis
 */

class CartesianTests extends AssertionsForJUnit{

  @Test def cartesianIteratorForGrounding(){

    val m = Map(
      "v1" -> List("One", "Two", "Three"),
      "v2" -> List("Alpha", "Beta", "Gamma"),
      "v3" ->  List("X", "Y", "Z")
    )

    val cartesian = CartesianIteratorMap[String,String](m)
    var count = 0
    while(cartesian.hasNext){
      val tuple = cartesian.next()
      val line = tuple.map(x => x._1.toString+" -> "+x._2.toString).reduceLeft(_ + ", " + _)
      println(line)
      count+=1
    }
    println("counted = "+count)
    assertEquals(27,count)

  }

}
