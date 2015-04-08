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

package lomrf.util.collection

import scala.reflect.ClassTag
import scalaxy.streams.optimize

trait PartitionedData[T] extends (Int => T) {

  /**
   * Gives the corresponding object that is associated to the specified index value,
   * possibly by using an hash partitioning function.
   *
   * @param idx the index value
   *
   * @return the corresponding object
   */
  def apply(idx: Int): T

  /**
   * @return the number of partitions
   */
  def length: Int

  /**
   * @return an iterable collection that contains all partitions
   */
  def partitions: Iterable[T]

  /**
   * Direct access to corresponding object at the specified partition index.
   *
   * @param partitionIndex partition index
   * @return corresponding object
   */
  def indexOf(partitionIndex: Int): T

}

object PartitionedData {


  object hash {

    def apply[T](data: Array[T]): PartitionedData[T] =
      new PartitionedDataImpl(data, (idx: Int) => math.abs(data.length % idx))

    def apply[T: ClassTag](size: Int, initializer:(Int => T)): PartitionedData[T] = {
      val data = new Array[T](size)

      optimize(for(i <- 0 until size) data(i) = initializer(i))

      apply(data)
    }

    def apply[T: ClassTag](size: Int): PartitionedData[T] = apply( new Array[T](size))
  }
/*

  object indexed {

    def apply[T](indices: Array[Int], data: Array[(Int) => T]) = new PartitionedData[T]{

      private val cumulativeIndices = indices.takeRight(indices.length -1).scanLeft(0)(_+_)

      override def apply(idx: Int): T = {
        val result = java.util.Arrays.binarySearch(indices, math.abs(idx))
        val partitionIndex = if(result < 0) ( -result ) - 2 else result
        val entryIndex = idx - cumulativeIndices(partitionIndex)
        data(partitionIndex)(entryIndex)
      }

      override def partitions: Iterable[T] = ???

      override def indexOf(partitionIndex: Int): (Int) => T = data(partitionIndex)

      override def length: Int = data.length
    }
  }
*/


  @deprecated(message = "use PartitionedData.hash.apply()")
  def apply[T](data: Array[T]): PartitionedData[T] = new PartitionedData[T] {

    override def apply(idx: Int) = data(math.abs(idx % data.length))

    override def partitions = data.toIterable

    override def length = data.length

    override def indexOf(partitionIndex: Int) = data(partitionIndex)
  }

  @deprecated(message = "use PartitionedData.hash.apply()")
  def apply[T: ClassTag](size: Int, initializer:(Int => T)): PartitionedData[T] = {
    val data = new Array[T](size)
    
    optimize(for(i <- 0 until size) data(i) = initializer(i))

    apply(data)
  }

  @deprecated(message = "use PartitionedData.hash.apply()")
  def apply[T: ClassTag](size: Int): PartitionedData[T] = apply( new Array[T](size))



  private class PartitionedDataImpl[T](data: Array[T], fPosition: Int => Int) extends PartitionedData[T]{

    override def apply(idx: Int) = data(fPosition(idx))

    override def partitions = data.toIterable

    override def length = data.length

    override def indexOf(partitionIndex: Int) = data(partitionIndex)
  }

}
