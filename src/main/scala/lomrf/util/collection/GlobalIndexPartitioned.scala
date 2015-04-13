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

import scala.{specialized => sp}


trait GlobalIndexPartitioned[C, @sp(Byte, Short, Int, Long, Float, Double, Boolean) V] {

  def apply(key: Int): V

  def size: Int

  def partitions: Iterable[C]

  def indexOf(partitionIndex: Int): C

}

private abstract class AbstractGlobalIndexPartitioned[C, @sp(Byte, Short, Int, Long, Float, Double, Boolean) V]
(data: Array[C], indices: Array[Int], partitioner: Partitioner[Int]) extends GlobalIndexPartitioned[C, V] {

  protected val cumulativeIndices = indices.takeRight(indices.length - 1).scanLeft(0)(_ + _)

  override def partitions: Iterable[C] = data.toIterable

  override def indexOf(partitionIndex: Int): C = data(partitionIndex)

  override def size: Int = data.length
}

object GlobalIndexPartitioned {

  def apply[C <: IndexedSeq[V], @sp(Byte, Short, Int, Long, Float, Double, Boolean) V]
  (data: Array[C], indices: Array[Int]): GlobalIndexPartitioned[C, V] = {

    val partitioner = Partitioner.indexed(indices)

    new AbstractGlobalIndexPartitioned[C, V](data, indices, partitioner) {

      override def apply(idx: Int): V = {
        val partitionIndex = partitioner(idx)
        val entryIndex = idx - cumulativeIndices(partitionIndex)

        data(partitionIndex)(entryIndex)
      }

    }
  }

  def apply[C, @sp(Byte, Short, Int, Long, Float, Double, Boolean) V]
  (data: Array[C], indices: Array[Int], partitionFetcher: PartitionFetcher[Int, C, V]): GlobalIndexPartitioned[C, V] = {

    val partitioner = Partitioner.indexed(indices)

    new AbstractGlobalIndexPartitioned[C, V](data, indices, partitioner) {

      override def apply(idx: Int): V = {
        val partitionIndex = partitioner(idx)
        val entryIndex = idx - cumulativeIndices(partitionIndex)
        partitionFetcher(entryIndex, data(partitionIndex))
      }

    }
  }


}
