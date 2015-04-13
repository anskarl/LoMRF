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

trait KeyPartitioned[C, @sp(Byte, Short, Int, Long) K, @sp(Byte, Short, Int, Long) V] extends (K => V) {

  def apply(key: K): V

  def size: Int

  def partitions: Iterable[C]

  def indexOf(partitionIndex: Int): C

}

object KeyPartitioned {

  def apply[C, @sp(Byte, Short, Int, Long) K, @sp(Byte, Short, Int, Long) V]
  (data: Array[C], partitioner: Partitioner[K], partitionFetcher: PartitionFetcher[K, C, V]): KeyPartitioned[C, K, V] = {
    new KeyPartitionedImpl[C, K, V](data, partitioner, partitionFetcher)
  }

  private class KeyPartitionedImpl[C, @sp(Byte, Short, Int, Long) K, @sp(Byte, Short, Int, Long) V]
  (data: Array[C], partitioner: Partitioner[K], partitionFetcher: PartitionFetcher[K, C, V]) extends KeyPartitioned[C, K, V] {

    override def apply(key: K): V = partitionFetcher(key, data(partitioner(key)))

    override def partitions: Iterable[C] = data.toIterable

    override def indexOf(partitionIndex: Int): C = data(partitionIndex)

    override def size: Int = data.length
  }

}
