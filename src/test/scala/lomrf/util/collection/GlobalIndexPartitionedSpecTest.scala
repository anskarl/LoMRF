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

import lomrf.mln.model.{ConstantsSet, ConstantsDomainBuilder}
import org.scalatest.{Matchers, FunSpec}


class GlobalIndexPartitionedSpecTest extends FunSpec with Matchers {

  describe("Array-based tests") {

    val arrayPartitionFetcher = new PartitionFetcher[Int, Array[Int], Int] {

      override def apply(key: Int, collection: Array[Int]): Int = collection(key)

      override def contains(key: Int, collection: Array[Int]): Boolean = collection.contains(key)

      override def valuesIterator(key: Int, collection: Array[Int]): Iterator[Int] = collection.iterator
    }

    val scenarios = List(
      ("A globally indexed partitioned collection of integers (1 to 400), using four equal sized partitions:", Array.fill(4)(100), None),

      ("A globally indexed partitioned collection of integers (1 to 400), using five unequal sized partitions:", Array(11, 102, 167, 101, 19), None),

      ("A globally indexed partitioned collection of integers (1 to 400), using four equal sized partitions and a PartitionFetcher function:",
        Array.fill(4)(100),
        Some(arrayPartitionFetcher)),

      ("A globally indexed partitioned collection of integers (1 to 400), using five unequal sized partitions and a PartitionFetcher function:",
        Array(11, 102, 167, 101, 19),
        Some(arrayPartitionFetcher))
    )

    for ((message, partitionSizes, fetcherOpt) <- scenarios) describe(message) {

      // the un-partitioned collection of integers
      val gIndices = (1 to partitionSizes.sum).toArray

      // Create the globally indexed partitioned collection
      val ipart = fetcherOpt match {
        case Some(fetcher) =>
          val data: Array[Array[Int]] = partitionSizes.scanLeft(0)(_ + _).sliding(2).map(x => (x(0) + 1 to x(1)).toArray).toArray
          GlobalIndexPartitioned[Array[Int], Int](data, partitionSizes, fetcher)

        case _ =>
          val data: Array[IndexedSeq[Int]] = partitionSizes.scanLeft(0)(_ + _).sliding(2).map(x => (x(0) + 1 to x(1)).toIndexedSeq).toArray
          GlobalIndexPartitioned[IndexedSeq[Int], Int](data)
      }


      it(s"should contain ${partitionSizes.length} partitions") {
        ipart.numberOfPartitions shouldEqual partitionSizes.length
      }

      it(s"should contain ${partitionSizes.sum} elements") {
        ipart.lastKey shouldEqual partitionSizes.sum
        ipart.size shouldEqual partitionSizes.sum
      }

      it(s"has 0 and ${partitionSizes.sum - 1} as first and last key values, respectively") {
        ipart.firstKey shouldEqual 0
        ipart.lastKey shouldEqual partitionSizes.sum
      }

      it("should directly give all indexed collection of integers (with bounds checking, using the 'apply(key: Int): V' function)") {
        gIndices.indices.forall(idx => ipart(idx) == gIndices(idx)) shouldBe true
      }

      it("should directly give all indexed collection of integers (with bounds checking, using the 'get(key: Int): Option[V]' function)") {
        gIndices.indices.forall(idx => ipart.get(idx).getOrElse(fail(s"cannot find element using key '$idx'")) == gIndices(idx)) shouldBe true
      }

      it("should directly give all indexed collection of integers (without bounds checking, using the 'fetch(key: Int): V' function)") {
        gIndices.indices.forall(idx => ipart.fetch(idx) == gIndices(idx)) shouldBe true
      }

      it("should throw IndexOutOfBoundsException when the key is out of bounds (apply(key: Int) function)") {
        assert(intercept[IndexOutOfBoundsException](ipart(ipart.firstKey - 1)).isInstanceOf[IndexOutOfBoundsException])
        assert(intercept[IndexOutOfBoundsException](ipart(ipart.lastKey + 1)).isInstanceOf[IndexOutOfBoundsException])
      }

      it("should give None when the key is out of bounds (get(key: Int) function)") {
        assert(ipart.get(ipart.firstKey - 1).isEmpty)
        assert(ipart.get(ipart.lastKey + 1).isEmpty)
      }
    }

  }

  describe("ConstantsDomain-based tests"){

    val domainA = (1 to 100).map(n => "A"+n)
    val domainB = (1 to 10).map(n => "B"+n)
    val domainC = (1 to 299).map(n => "C"+n)
    val domainD = (1 to 541).map(n => "D"+n)


    val builder = ConstantsDomainBuilder()

    builder.of("domainA") ++= domainA
    builder.of("domainB") ++= domainB
    builder.of("domainC") ++= domainC
    builder.of("domainD") ++= domainD

    val constants = builder.result()

    val domainNames = new Array[String](constants.size)
    val domainsValues = new Array[ConstantsSet](constants.size)

    for( ((k, v), idx) <- constants.zipWithIndex ){
      domainNames(idx) = k
      domainsValues(idx) = v
    }

    val partitionSizes = domainsValues.map(_.size)
    val numberOfElements = partitionSizes.sum


    val scenarios =
      List[(String,GlobalIndexPartitioned[ConstantsSet, String])](

        ("With partition fetcher",
          GlobalIndexPartitioned[ConstantsSet, String](domainsValues, partitionSizes, PartitionFetcher.create[ConstantsSet, String])),

        ("Without partition fetcher",
          GlobalIndexPartitioned[ConstantsSet, String](domainsValues))
      )

    for ((message, collection) <- scenarios ) describe(message) {


      it(s"contains total '$numberOfElements' elements") {
        collection.size shouldEqual numberOfElements
      }

      it("provides global access to elements") {

        var offset = 0
        for {
          partNumber <- 0 until collection.numberOfPartitions
          domain = domainsValues(partNumber) } {

          for((element, localIndex) <- domain.zipWithIndex; globalIndex = offset + localIndex) {
            collection(globalIndex) shouldEqual element
          }

          offset += domain.size
        }

      }

    }

  }


}
