/*
 *
 *  o                        o     o   o         o
 *  |             o          |     |\ /|         | /
 *  |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 *  |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 *  O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *              |
 *           o--o
 *  o--o              o               o--o       o    o
 *  |   |             |               |    o     |    |
 *  O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 *  |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 *  o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 *  Logical Markov Random Fields (LoMRF).
 *     
 *
 */

package lomrf.util.collection

import scala.{specialized => sp}


trait GlobalIndexPartitioned[C, @sp(Byte, Short, Int, Long, Float, Double, Boolean) V] {

  def apply(key: Int): V

  def fetch(key: Int): V

  def get(key: Int): Option[V]

  def size: Int

  def numberOfPartitions: Int

  def partitions: Iterable[C]

  def partition(partitionIndex: Int): C

  def getPartition(partitionIndex: Int): Option[C]

  val firstKey: Int

  val lastKey: Int

}

abstract class AbstractGlobalIndexPartitioned[C, @sp(Byte, Short, Int, Long, Float, Double, Boolean) V]
(data: Array[C], partitionSizes: Array[Int]) extends GlobalIndexPartitioned[C, V] {

  protected val cumulativeIndices: Array[Int] = partitionSizes.scanLeft(0)(_ + _)
  protected val partitioner: Partitioner[Int] = Partitioner.indexed(cumulativeIndices)
  protected val numberOfElements = partitionSizes.sum

  override def apply(key: Int): V = {
    if(key < 0 || key >= cumulativeIndices.last)
      throw new IndexOutOfBoundsException(s"Invalid index value.")

    fetch(key)
  }

  override def get(key: Int): Option[V] = {
    if(key < 0 || key >= cumulativeIndices.last) None
    else Some(fetch(key))
  }

  override def partitions: Iterable[C] = data.toIterable

  override def partition(partitionIndex: Int): C = data(partitionIndex)

  override def getPartition(partitionIndex: Int): Option[C] = {
    if(partitionIndex < 0 || partitionIndex >= data.length) None
    else Some(data(partitionIndex))
  }


  override def size: Int = numberOfElements

  override def numberOfPartitions: Int = data.length

  override val firstKey: Int = cumulativeIndices.head

  override val lastKey: Int = cumulativeIndices.last

  override def toString: String = s"GlobalIndexPartitioned{cumulativeIndices = (${cumulativeIndices.mkString(",")})}"

}

object GlobalIndexPartitioned {

  def apply[C <: IndexedSeq[V], @sp(Byte, Short, Int, Long, Float, Double, Boolean) V]
  (data: Array[C]): GlobalIndexPartitioned[C, V] = {

    new AbstractGlobalIndexPartitioned[C, V](data, data.map(_.size)) {

      override def fetch(key: Int): V = {
        val partitionIndex = partitioner(key)
        val entryIndex = key - cumulativeIndices(partitionIndex)
        data(partitionIndex)(entryIndex)
      }

    }
  }

  def apply[C, @sp(Byte, Short, Int, Long, Float, Double, Boolean) V]
  (data: Array[C], partitionSizes: Array[Int], partitionFetcher: PartitionFetcher[Int, C, V]): GlobalIndexPartitioned[C, V] = {

    new AbstractGlobalIndexPartitioned[C, V](data, partitionSizes) {

      override def fetch(key: Int): V = {
        val partitionIndex = partitioner(key)
        val entryIndex = key - cumulativeIndices(partitionIndex)
        partitionFetcher(entryIndex, data(partitionIndex))
      }
    }
  }


}
