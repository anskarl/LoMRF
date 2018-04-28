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

trait Partitioner[@sp(Byte, Short, Int, Long) K] {

  def apply(key: K): Int

  def numPartitions: Int

}


object Partitioner {

  object hash {

    import scala.reflect.runtime.universe._

    def apply[K: TypeTag](size: Int): Partitioner[K] = {

      (typeOf[K] match {

        case TypeTag.Int => new FixedSizePartitionerInt(size)

        case TypeTag.Long => new FixedSizePartitionerLong(size)

        case TypeTag.Short => new FixedSizePartitionerShort(size)

        case TypeTag.Byte => new FixedSizePartitionerByte(size)

        case t => new FixedSizePartitioner[K](size) {
          override def apply(key: K): Int = {
            if (key == null) 0
            else (key.## & Int.MaxValue) % size // always return non-negative integer
          }
        }
      }).asInstanceOf[Partitioner[K]]
    }

  }


  object indexed {

    def apply(indices: Array[Int]): Partitioner[Int] = new FixedSizePartitioner[Int](indices.length) {

      override def apply(key: Int): Int = {

        val searchResult = java.util.Arrays.binarySearch(indices, math.abs(key))
        val partitionIndex = if (searchResult < 0) (-searchResult) - 2 else searchResult

        //assert(partitionIndex >= 0)

        partitionIndex
      }

      override def numPartitions: Int = indices.length
    }

    def apply[@sp(Byte, Short, Int, Long) K](indices: Array[Int], f: K => Int): Partitioner[K] = new FixedSizePartitioner[K](indices.length) {
      override def apply(key: K): Int = {

        val searchResult = java.util.Arrays.binarySearch(indices, math.abs(f(key)))
        val partitionIndex = if (searchResult < 0) (-searchResult) - 2 else searchResult

        //assert(partitionIndex >= 0)

        partitionIndex
      }
    }


    def fromRanges(ranges: Iterable[Range]): Partitioner[Int] = {
      require(ranges.forall(_.step == 1),
        "Only ranges with successive values supported, i.e., ranges having step size equal to one.")

      apply(ranges.map(_.size).scanLeft(0)(_ + _).toArray)
    }

    def fromRanges[@sp(Byte, Short, Int, Long) K](ranges: Iterable[Range], f: K => Int): Partitioner[K] = {
      require(ranges.forall(_.step == 1),
        "Only ranges with successive values supported, i.e., ranges having step size equal to one.")

      apply(ranges.map(_.size).scanLeft(0)(_ + _).toArray, f)
    }

    def fromSizes(sizes: Iterable[Int]): Partitioner[Int] = apply(sizes.scanLeft(0)(_ + _).toArray)

    def fromSizes[@sp(Byte, Short, Int, Long) K](sizes: Iterable[Int], f: K => Int): Partitioner[K] = apply(sizes.scanLeft(0)(_ + _).toArray, f)


  }

  private abstract class FixedSizePartitioner[@sp(Byte, Short, Int, Long) K](size: Int) extends Partitioner[K] {
    override def numPartitions: Int = size
  }

  private class FixedSizePartitionerByte(size: Int) extends FixedSizePartitioner[Byte](size){
      override def apply(key: Byte): Int = (key & Int.MaxValue) % size
  }

  private class FixedSizePartitionerShort(size: Int) extends FixedSizePartitioner[Short](size){
    override def apply(key: Short): Int = (key & Int.MaxValue) % size
  }

  private class FixedSizePartitionerInt(size: Int) extends FixedSizePartitioner[Int](size){
    override def apply(key: Int): Int = (key & Int.MaxValue) % size
  }

  private class FixedSizePartitionerLong(size: Int) extends FixedSizePartitioner[Long](size){
    override def apply(key: Long): Int = (key.toInt & Int.MaxValue) % size
  }


}