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

package lomrf.mln.model

import gnu.trove.iterator.TObjectIntIterator
import gnu.trove.map.TObjectIntMap
import lomrf.mln.model.builders.ConstantsSetBuilder
import scala.collection.mutable

/**
  * A ConstantsSet is an immutable collection of unique and indexed constant symbols.
  * The symbols are represented by strings and their associated index numbers are represented
  * by positive integers. The association between constant symbols and their indexes
  * (also referred as identities or IDs) is a unique one-to-one mapping.
  */
sealed trait ConstantsSet extends Iterable[String] with IndexedSeq[String] with Serializable {

  /**
    * Gives the constant symbol that is associated to the specified identity, if one exists.
    *
    * @param id an integer number that represents the identity of a constant
    * @return the associated constant symbol, if one exists, otherwise null
    */
  def apply(id: Int): String

  /**
    * Gives the identity associated to the specified constant symbol, if one exists.
    *
    * @param constant a string that represents a constant symbol
    * @return the associated identity number, if one exists, otherwise null
    */
  def apply(constant: String): Int

  /**
    * Gives the constant symbol associated to the specified identity, if one exists.
    *
    * @param id an integer number that represents the identity of a constant
    * @return the associated constant symbol as an Option
    */
  def get(id: Int): Option[String]

  /**
    * Gives the identity associated to the specified constant symbol, if one exists.
    *
    * @param constant a string that represents a constant symbol
    * @return the associated identity number as an Option
    */
  def get(constant: String): Option[Int]

  /**
    * Checks if the given constant symbol exists in the constant set.
    *
    * @param constant the constant symbol to check
    * @return true if the given constant symbol exists in the set, false otherwise.
    */
  def contains(constant: String): Boolean

  /**
    * Checks if the given identity exists in the constants set.
    *
    * @param id the identity to check
    * @return true if the given identity exists in the set, false otherwise
    */
  def contains(id: Int): Boolean

  /**
    * Gives an iterator over the constant symbols that exist in the constant set.
    *
    * @return an iterator over the constant symbols that exist in the set
    */
  def valuesIterator: TObjectIntIterator[String]

  /**
    * Gives an iterator over the identities that are associated to the constant
    * symbols that exist in the constants set.
    *
    * @return an iterator instance of the identities that are associated to the
    *         constant symbols that exist in the set
    */
  def idsIterator: Iterator[Int]

  /**
    * Gives the range of identities for all constant symbols that exist
    * in the constants set.
    *
    * @return the range of identities
    */
  def idsRange: Range

}

/**
  * Default ConstantsSet implementation, which is backend by a HashMap and an ArrayBuffer. The HashMap gives constant
  * time near O(1) access to the ID of each constant symbol. On the other hand, the ArrayBuffer gives O(1) access to
  * the constant symbol given its ID. Internally, the ID is the position to the ArrayBuffer. Since the construction of
  * this class is private, the only way to construct a ConstantsSet is through a ConstantsSetBuilder or by using the
  * companion object ConstantsSet.
  *
  * @see [[lomrf.mln.model.builders.ConstantsSetBuilder]]
  * @param constants2Id a hash map that associates constant symbols (String) to integers (IDs)
  * @param id2Constants an array buffer that contains all constant symbols (String). The position of each symbol
  *                     in the array buffer is identical to its ID.
  */
final class ConstantsSetImpl(
    private[model] val constants2Id: TObjectIntMap[String],
    private[model] val id2Constants: mutable.ArrayBuffer[String]) extends ConstantsSet {

  import ConstantsSet.NO_ENTRY

  override def head: String = id2Constants.head

  override def last: String = id2Constants.last

  override def apply(id: Int): String = id2Constants(id)

  override def apply(constant: String): Int = constants2Id.get(constant)

  override def get(id: Int): Option[String] = if (id <= id2Constants.size) Some(id2Constants(id)) else None

  override def get(constant: String): Option[Int] = {
    val id = constants2Id.get(constant)
    if (id == NO_ENTRY) None else Some(id)
  }

  override def contains(constant: String): Boolean = constants2Id.containsKey(constant)

  override def contains(id: Int): Boolean = id >= 0 && id < id2Constants.length

  override def iterator: Iterator[String] = id2Constants.iterator

  override def valuesIterator: TObjectIntIterator[String] = constants2Id.iterator

  override def idsIterator: Iterator[Int] = id2Constants.indices.iterator

  override def idsRange: Range = id2Constants.indices

  override def isEmpty: Boolean = id2Constants.isEmpty

  override def length: Int = constants2Id.size

  override def toString: String =
    s"ConstantsSet(const2id->{${constants2Id.size} elements}, id2const->{${id2Constants.size} elements})"
}

/**
  * Default ConstantsSet implementation for a single constant symbol. Since the construction of this class
  * is private, the only way to construct a ConstantsSet is through a ConstantsSetBuilder or by using the
  * companion object ConstantsSet.
  *
  * @see [[lomrf.mln.model.builders.ConstantsSetBuilder]]
  * @param element a single constant symbol to include the constants set
  */
final class ConstantsSetUnaryImpl(val element: String) extends ConstantsSet {
  import ConstantsSet.NO_ENTRY

  override def head: String = element

  override def last: String = element

  override def apply(id: Int): String = if (id == 0) element else null

  override def apply(constant: String): Int = if (constant == element) 0 else NO_ENTRY

  override def get(id: Int): Option[String] = if (id == 0) Some(element) else None

  override def get(constant: String): Option[Int] = if (constant == element) Some(0) else None

  override def contains(constant: String): Boolean = constant == element

  override def contains(id: Int): Boolean = id == 0

  override def iterator: Iterator[String] = new Iterator[String] {
    private var hasNextFlag = true

    override def hasNext: Boolean = hasNextFlag

    override def next(): String = {
      hasNextFlag = false
      element
    }
  }

  override def valuesIterator: TObjectIntIterator[String] = new TObjectIntIterator[String]() {
    private var hasNextFlag = true

    override def key: String = element

    override def setValue(i: Int): Int =
      throw new UnsupportedOperationException("UnaryConstantsSet is immutable.")

    override def value: Int = 0

    override def advance(): Unit = hasNextFlag = false

    override def remove(): Unit =
      throw new UnsupportedOperationException("UnaryConstantsSet is immutable.")

    override def hasNext: Boolean = hasNextFlag
  }

  override def idsIterator: Iterator[Int] = (0 until 1).iterator

  override def idsRange: Range = 0 to 0

  override def isEmpty: Boolean = false

  override def length: Int = 1

  override def toString: String = s"UnaryConstantsSet($element)"
}

object ConstantsSet {

  // Identity that represents no-entry key (should be a negative integer).
  val NO_ENTRY: Int = -1000

  /**
    * Creates a unary constants set from a single constant symbol (string).
    * @see [[lomrf.mln.model.ConstantsSetUnaryImpl]]
    *
    * @param symbol a constant symbol
    * @return a unary ConstantsSet instance
    */
  def apply(symbol: String): ConstantsSet = new ConstantsSetUnaryImpl(symbol)

  /**
    * Creates a constants set from a sequence of constant symbols (strings).
    * @see [[lomrf.mln.model.ConstantsSetUnaryImpl]]
    * @see [[lomrf.mln.model.ConstantsSetImpl]]
    *
    * @param symbols the constant symbols to include in the constants set
    * @return a ConstantsSet instance
    */
  def apply(symbols: String*): ConstantsSet = {
    if (symbols.size == 1)
      new ConstantsSetUnaryImpl(symbols.head)
    else {
      val builder = new ConstantsSetBuilder()
      symbols.foreach(builder += _)
      builder.result()
    }
  }

  /**
    * Creates a constants set from an iterable of constant symbols (strings).
    * @see [[lomrf.mln.model.ConstantsSetUnaryImpl]]
    * @see [[lomrf.mln.model.ConstantsSetImpl]]
    *
    * @param symbols an iterable of constant symbols to include in the constants set
    * @return a ConstantsSet instance
    */
  def apply(symbols: Iterable[String]): ConstantsSet = {
    if (symbols.size == 1)
      new ConstantsSetUnaryImpl(symbols.head)
    else {
      val builder = new ConstantsSetBuilder()
      symbols.foreach(builder += _)
      builder.result()
    }
  }
}
