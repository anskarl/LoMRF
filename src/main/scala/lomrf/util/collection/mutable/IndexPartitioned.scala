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

package lomrf.util.collection.mutable

import scala.reflect.ClassTag
import scalaxy.streams.optimize


trait IndexPartitioned[T] extends lomrf.util.collection.IndexPartitioned[T] {

  def update(idx: Int, elem: T)
}


object IndexPartitioned{
  def apply[T](data: Array[T]): IndexPartitioned[T] = new IndexPartitioned[T] {

    private val positionOf = (idx: Int) => math.abs(idx % data.length)

    override def apply(idx: Int): T = data(positionOf(idx))

    override def partitions = data.toIterable

    override def size = data.length

    override def update(idx: Int, elem: T): Unit = data(idx) = elem

    override def partition(partitionIndex: Int) = data(partitionIndex)
  }

  def apply[T: ClassTag](size: Int, initializer:(Int => T)): IndexPartitioned[T] = {
    val data = new Array[T](size)

    optimize(for(i <- 0 until size) data(i) = initializer(i))

    apply(data)
  }

  def apply[T: ClassTag](size: Int): IndexPartitioned[T] = apply( new Array[T](size))
}