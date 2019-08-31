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

package lomrf.mln.model.builders

import gnu.trove.TCollections
import gnu.trove.map.hash.TObjectIntHashMap
import lomrf.logic.Constant
import lomrf.mln.model.{ ConstantsSet, ConstantsSetImpl, ConstantsSetUnaryImpl }
import scala.collection.mutable
import gnu.trove.impl.{ Constants => TC }

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
