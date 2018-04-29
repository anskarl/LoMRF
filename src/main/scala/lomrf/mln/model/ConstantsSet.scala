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

import gnu.trove.TCollections
import gnu.trove.impl.{ Constants => TC }
import gnu.trove.iterator.TObjectIntIterator
import gnu.trove.map.TObjectIntMap
import gnu.trove.map.hash.TObjectIntHashMap
import lomrf.logic.Constant

import scala.collection.mutable

/**
  * A ConstantsSet is an immutable collection of unique and indexed constant symbols. The symbols are represented by
  * strings and their associated index numbers are represented by positive integers. The association between constant
  * symbols and their indexes (also referred as identities or IDs) is a bijection, that is, an one-to-one mapping.
  */
sealed trait ConstantsSet extends Iterable[String] with IndexedSeq[String] with Serializable {

  /**
    * Gives the constant symbol that is associated to the specified identity, if exists.
    *
    * @param id an integer number that represents the identity of a constant.
    * @return The associated constant symbol, if exists, otherwise null.
    */
  def apply(id: Int): String

  /**
    * Gives the identity that is associated to the specified constant symbol, if exists.
    *
    * @param constant a string that represents a constant symbol.
    * @return The associated identity number, if exists, otherwise null.
    */
  def apply(constant: String): Int

  /**
    * Gives the constant symbol that is associated to the specified identity, if exists.
    *
    * @param id an integer number that represents the identity of a constant.
    * @return The associated constant symbol wrapped as Option, if exists, otherwise None.
    */
  def get(id: Int): Option[String]

  /**
    * Gives the identity that is associated to the specified constant symbol, if exists.
    *
    * @param constant a string that represents a constant symbol.
    * @return The associated identity number wrapped as Option, if exists, otherwise None.
    */
  def get(constant: String): Option[Int]

  /**
    * Checks if the specified constant symbol exists in this ConstantsSet
    *
    * @param constant the constant symbol to check
    * @return true if the specified constant symbol exists in this ConstantsSet, otherwise false.
    */
  def contains(constant: String): Boolean

  /**
    * Checks if the specified identity exists in this ConstantsSet
    *
    * @param id the identity to check
    * @return true if the specified identity exists in this ConstantsSet, otherwise false.
    */
  def contains(id: Int): Boolean

  /**
    * Gives an iterator over the space of constant symbols that exist in this ConstantSet
    *
    * @return an iterator instance over the constant symbols that exist in this ConstantSet
    */
  def valuesIterator: TObjectIntIterator[String]

  /**
    * Gives an iterator over the space of identities that are associated to the constant symbols that exist in
    * this ConstantSet
    *
    * @return an iterator instance of the identities that are associated to the constant symbols
    *         that exist in this ConstantSet
    */
  def idsIterator: Iterator[Int]

  /**
    * Gives the range of identities of all constant symbols that exist in this ConstantSet
    *
    * @return the range of identities
    */
  def idsRange: Range

  def size: Int

  def length: Int = size

}

/**
  * Default ConstantsSet implementation, which is backend by a HashMap and an ArrayBuffer. The HashMap gives constant
  * time near O(1) access to the ID of each constant symbol. On the other hand, the ArrayBuffer gives O(1) access to
  * the constant symbol given its ID. Internally, the ID is the position to the ArrayBuffer. Since the construction of
  * this class is private, the only way to construct a ConstantsSet is through a ConstantsSetBuilder or by using the
  * companion object ConstantsSet.
  *
  * @param constants2Id Hashmap that associates constant symbols (String) to integers (IDs)
  * @param id2Constants Array buffer that contains all constant symbols (String). The position of each symbol in the
  *                     array buffer is the same with its ID.
  *
  * @see ConstantsSetBuilder
  * @see ConstantsSet
  */
final class ConstantsSetImpl(
    private[model] val constants2Id: TObjectIntMap[String],
    private[model] val id2Constants: mutable.ArrayBuffer[String]) extends ConstantsSet {

  import ConstantsSet.NO_ENTRY

  override def head = id2Constants.head

  override def last = id2Constants.last

  override def apply(id: Int): String = id2Constants(id)

  override def apply(constant: String): Int = constants2Id.get(constant)

  override def get(id: Int): Option[String] = if (id <= id2Constants.size) Some(id2Constants(id)) else None

  override def get(constant: String): Option[Int] = {
    val id = constants2Id.get(constant)
    if (id == NO_ENTRY) None else Some(id)
  }

  override def size: Int = constants2Id.size

  override def contains(constant: String): Boolean = constants2Id.containsKey(constant)

  override def contains(id: Int): Boolean = id >= 0 && id < id2Constants.length

  override def valuesIterator = constants2Id.iterator

  override def idsIterator: Iterator[Int] = id2Constants.indices.iterator

  override def idsRange: Range = id2Constants.indices

  override def iterator = id2Constants.iterator

  override def isEmpty: Boolean = id2Constants.isEmpty

  override def toString(): String =
    s"MultipleConstantsSet(const2id->{${constants2Id.size()} elements}, id2const->{${id2Constants.size} elements})"
}

/**
  * A default implementation of ConstantsSet, when the constant symbol is singular. Since the construction of
  * this class is private, the only way to construct a ConstantsSet is through a ConstantsSetBuilder or by using the
  * companion object ConstantsSet.
  *
  * @param element the single constant symbol to include this ConstantsSet implementation.
  *
  * @see ConstantsSetBuilder
  * @see ConstantsSet
  */
final class ConstantsSetUnaryImpl(val element: String) extends ConstantsSet {
  import ConstantsSet.NO_ENTRY

  override def head: String = element

  override def last: String = element

  override def apply(id: Int): String = if (id == 0) element else null

  override def apply(constant: String): Int = if (constant == element) 0 else NO_ENTRY

  override def get(id: Int): Option[String] = if (id == 0) Some(element) else None

  override def get(constant: String): Option[Int] = if (constant == element) Some(0) else None

  override def size: Int = 1

  override def contains(constant: String): Boolean = constant == element

  override def contains(id: Int): Boolean = id == 0

  override def valuesIterator = new TObjectIntIterator[String]() {
    private var hasNextFlag = true

    override def key(): String = element

    override def setValue(i: Int): Int =
      throw new UnsupportedOperationException("UnaryConstantsSet is immutable.")

    override def value(): Int = 0

    override def advance(): Unit = hasNextFlag = false

    override def remove(): Unit =
      throw new UnsupportedOperationException("UnaryConstantsSet is immutable.")

    override def hasNext: Boolean = hasNextFlag
  }

  override def idsIterator: Iterator[Int] = (0 until 1).iterator

  override def idsRange: Range = 0 to 0

  override def iterator: Iterator[String] = new Iterator[String] {
    private var hasNextFlag = true

    override def hasNext: Boolean = hasNextFlag

    override def next(): String = {
      hasNextFlag = false
      element
    }
  }

  override def isEmpty: Boolean = false

  override def toString(): String = s"UnaryConstantsSet($element)"

}

/**
  * Provides factory functions for building ConstantSets.
  */
object ConstantsSet {

  /**
    * Utility identity that represents no-entry key (should be negative integer)
    */
  val NO_ENTRY: Int = -1000

  /**
    * Creates a unary ConstantSet from the specified constant symbol.
    *
    * @param symbol the constant symbol
    * @return a unary ConstantSet
    */
  def apply(symbol: String): ConstantsSet = new ConstantsSetUnaryImpl(symbol)

  /**
    * Creates a ConstantSet from the specified constant symbols.
    *
    * @param symbols the constant symbols to include in the ConstantSet
    * @return an instance of ConstantSet
    */
  def apply(symbols: String*): ConstantsSet = {

    if (symbols.size == 1) new ConstantsSetUnaryImpl(symbols.head)
    else {
      val builder = new ConstantsSetBuilder()
      symbols.foreach(builder += _)
      builder.result()
    }

  }

  /**
    * Creates a ConstantSet from the specified collection constant symbols.
    *
    * @param symbols the iterable collection of constant symbols to include in the ConstantSet
    * @return an instance of ConstantSet
    */
  def apply(symbols: Iterable[String]): ConstantsSet = {
    if (symbols.size == 1) new ConstantsSetUnaryImpl(symbols.head)
    else {
      val builder = new ConstantsSetBuilder()
      symbols.foreach(builder += _)
      builder.result()
    }
  }
}

/**
  * A builder with which we can incrementally create a ConstantSet.
  */
final class ConstantsSetBuilder(
    private var constants2Id: TObjectIntHashMap[String],
    private var id2Constants: mutable.ArrayBuffer[String]) extends mutable.Builder[String, ConstantsSet] with Iterable[String] { self =>

  import ConstantsSet.NO_ENTRY
  import ConstantsSetBuilder.ConstantSymbol
  import gnu.trove.impl.{ Constants => TC }

  def this() =
    this(new TObjectIntHashMap[String](TC.DEFAULT_CAPACITY, TC.DEFAULT_LOAD_FACTOR, ConstantsSet.NO_ENTRY), new mutable.ArrayBuffer[String]())

  /**
    * flag to mark if [[result()]] function is being called
    */
  private var dirty = false

  private var _size = id2Constants.size

  override def iterator: Iterator[String] = id2Constants.iterator

  /**
    * Adds a constant symbol, if it has been never included.
    *
    * @param constant the specified constant symbol
    */
  override def +=(constant: String): self.type = {
    copyIfDirty()
    put(constant)
    this
  }

  def +=(constant: Number): self.type = {
    this += constant.toString
  }

  def ++=[T: ConstantSymbol](elements: Iterable[T]): self.type = {
    copyIfDirty()
    for (element <- elements) element match {
      case symbol: String     => put(symbol)
      case constant: Constant => put(constant.symbol)
      case _: Number          => put(element.toString)
    }
    this
  }

  def ++=(e: Any, elements: Any*): self.type = {
    copyIfDirty()

    putMatching(e)
    for (element <- elements) putMatching(element)

    this
  }

  private def putMatching(element: Any): Unit = {
    element match {
      case symbol: String     => put(symbol)
      case constant: Constant => put(constant.symbol)
      case _: Number          => put(element.toString)
      case _                  => throw new UnsupportedOperationException("")
    }
  }

  private def put(constant: String): Unit = {
    if (constants2Id.putIfAbsent(constant, _size) == NO_ENTRY) {
      id2Constants += constant
      _size += 1
    }
  }

  override def sizeHint(size: Int): Unit = {
    constants2Id.setUp(size)
    id2Constants.sizeHint(size)
  }

  /**
    * Gives the constructed ConstantsSet
    *
    * @return a ConstantsSet instance that contains all constant symbols that have been added so far.
    */
  override def result(): ConstantsSet = {
    // Mark builder as dirty, thus if we do another addition we will not have any side effects in the resulting
    // ConstantsSet, by performing copies.
    dirty = true
    new ConstantsSetImpl(TCollections.unmodifiableMap(constants2Id), id2Constants)
  }

  /**
    * Empties this builder.
    */
  override def clear() {
    constants2Id = new TObjectIntHashMap[String](TC.DEFAULT_CAPACITY, TC.DEFAULT_LOAD_FACTOR, NO_ENTRY)
    id2Constants = new mutable.ArrayBuffer[String]()
    dirty = false
  }

  override def toString(): String =
    s"ConstantsSetBuilder(constants2Id->{${constants2Id.size()} elements}, id2Constants->{${id2Constants.size} elements})"

  /**
    * @return the number of the unique collected constant symbols
    */
  override def size = _size

  def copy() = {
    val cp_constants2Id = new TObjectIntHashMap[String](_size + 23, TC.DEFAULT_LOAD_FACTOR, NO_ENTRY)
    cp_constants2Id.putAll(constants2Id)

    val cp_id2Constants = new mutable.ArrayBuffer[String]()
    id2Constants.foreach(cp_id2Constants += _)

    new ConstantsSetBuilder(cp_constants2Id, cp_id2Constants)
  }

  /**
    * When a ConstantSet has been created from this ConstantsSetBuilder by a previous call of the this.result function,
    * then before adding the specified symbol, we copy all constant symbols.
    * The reason behind that copy, is that the resulting ConstantsSets must behave like immutable collections.
    */
  private def copyIfDirty(): Unit = {
    if (dirty) {
      val cp_constants2Id = new TObjectIntHashMap[String](_size + 23, TC.DEFAULT_LOAD_FACTOR, NO_ENTRY)
      cp_constants2Id.putAll(constants2Id)
      val cp_id2Constants = new mutable.ArrayBuffer[String]()
      id2Constants.foreach(cp_id2Constants += _)
      constants2Id = cp_constants2Id
      id2Constants = cp_id2Constants
      dirty = false
    }
  }

}

object ConstantsSetBuilder {

  sealed class ConstantSymbol[T]

  object ConstantSymbol {
    implicit object ByteWitness extends ConstantSymbol[Byte]
    implicit object ShortWitness extends ConstantSymbol[Short]
    implicit object IntWitness extends ConstantSymbol[Int]
    implicit object LongWitness extends ConstantSymbol[Long]
    implicit object FloatWitness extends ConstantSymbol[Float]
    implicit object DoubleWitness extends ConstantSymbol[Double]
    implicit object StringWitness extends ConstantSymbol[String]
    implicit object ConstantWitness extends ConstantSymbol[Constant]
  }
  def apply: ConstantsSetBuilder = new ConstantsSetBuilder

  def apply(symbols: Iterable[String]): ConstantsSetBuilder = this.apply ++= symbols

  def apply(symbols: String*): ConstantsSetBuilder = this.apply ++= symbols

  def apply(constantsSet: ConstantsSet) = constantsSet match {
    case c: ConstantsSetImpl =>
      val _id2Constants = new mutable.ArrayBuffer[String](c.size)
      new ConstantsSetBuilder(new TObjectIntHashMap[String](c.constants2Id), _id2Constants ++= c.id2Constants)

    case c: ConstantsSetUnaryImpl =>
      new ConstantsSetBuilder += c.head
  }

}
