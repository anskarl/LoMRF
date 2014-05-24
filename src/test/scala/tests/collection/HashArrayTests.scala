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

package tests.collection

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import lomrf.util.collection.HashArray


/**
 * @author Anastasios Skarlatidis
 */

class HashArrayTests extends AssertionsForJUnit{

  @Test def test1() {
    val ha = new HashArray[String]()
    for(i<- 0 to 10) ha += i.toString
    println("size = "+ha.size)
    println("elements: ")
    ha.elements.zipWithIndex.foreach(tuple => println(tuple._2+" "+tuple._1))

    println("index:")
    println("index.no_value := "+ha.indices.getNoEntryValue)

    val iterator = ha.indices.iterator()
    while(iterator.hasNext){
      iterator.advance()
      println(iterator.key()+" "+iterator.value())
    }

  }

}
