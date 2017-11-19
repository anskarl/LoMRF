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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.util.collection

trait PartitionFetcher[Key, Collection, Value] {

  def apply(key: Key, collection: Collection): Value

  def contains(key: Key, collection: Collection): Boolean

  def valuesIterator(key: Key, collection: Collection): Iterator[Value]

}

object PartitionFetcher {

  def create[C <: IndexedSeq[V], V]: PartitionFetcher[Int, C, V] = new  PartitionFetcher[Int, C, V](){
    override def apply(key: Int, collection: C): V = collection(key)

    override def contains(key: Int, collection: C): Boolean = collection.contains(key)

    override def valuesIterator(key: Int, collection: C): Iterator[V] = collection.iterator
  }
}
