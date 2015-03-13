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


trait PartitionedData[T] extends lomrf.util.collection.PartitionedData[T] {

  def update(idx: Int, elem: T)
}


object PartitionedData{
  def apply[T](data: Array[T]): PartitionedData[T] = new PartitionedData[T] {

    override def apply(idx: Int) = data(math.abs(idx % data.length))

    override def partitions = data.toIterable

    override def length = data.length

    override def update(idx: Int, elem: T): Unit = data(idx) = elem

  }

  def apply[T: ClassTag](size: Int, initializer:(Int => T)): PartitionedData[T] = {
    val data = new Array[T](size)

    optimize(for(i <- 0 until size) data(i) = initializer(i))

    apply(data)
  }

  def apply[T: ClassTag](size: Int): PartitionedData[T] = apply( new Array[T](size))
}