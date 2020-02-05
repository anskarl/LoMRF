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
import gnu.trove.impl.Constants._
import lomrf.logic.Constant
import lomrf.mln.model.{ ConstantsSet, ConstantsSetImpl, ConstantsSetUnaryImpl }
import scala.collection.mutable

/**
  * Constants set builder.
  *
  * @param constants2Id a map from constants to ids
  * @param id2Constants an array buffer of constants
  */
final class ConstantsSetBuilder(
    private var constants2Id: TObjectIntHashMap[String],
    private var id2Constants: mutable.ArrayBuffer[String])
  extends mutable.Builder[String, ConstantsSet] with Iterable[String] { self =>

  private var dirty = false

  private var _size = id2Constants.size

  def this() = this(
    new TObjectIntHashMap[String](DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, ConstantsSet.NO_ENTRY),
    mutable.ArrayBuffer.empty[String]
  )

  /**
    * Puts a constant symbol into the constants set.
    *
    * @param constant a constant symbol
    */
  private def put(constant: String): Unit = {
    if (constants2Id.putIfAbsent(constant, _size) == ConstantsSet.NO_ENTRY) {
      id2Constants += constant
      _size += 1
    }
  }

  /**
    * Puts a given value into the constants set.
    *
    * @param element a value
    * @throws UnsupportedOperationException in case the given value is not supported.
    */
  private def putMatching(element: Any): Unit = element match {
    case symbol: String     => put(symbol)
    case constant: Constant => put(constant.symbol)
    case _: Number          => put(element.toString)
    case _                  => throw new UnsupportedOperationException("")
  }

  /**
    * Copies all constant symbols to new collections in case the result function has
    * been called. That way the builder avoids side-effects, that is the resulting constant
    * sets behave like immutable collections.
    */
  private def copyIfDirty(): Unit = if (dirty) {
    val cp_constants2Id = new TObjectIntHashMap[String](_size + 23, DEFAULT_LOAD_FACTOR, ConstantsSet.NO_ENTRY)
    cp_constants2Id.putAll(constants2Id)
    val cp_id2Constants = mutable.ArrayBuffer.empty[String]
    id2Constants.foreach(cp_id2Constants += _)
    constants2Id = cp_constants2Id
    id2Constants = cp_id2Constants
    dirty = false
  }

  /**
    * Adds a constant symbol to the constants set builder.
    *
    * @param constant a constant symbol
    * @return a ConstantsSetBuilder instance
    */
  override def +=(constant: String): self.type = {
    copyIfDirty()
    put(constant)
    this
  }

  /**
    * Adds a numeric constant to the constants set builder.
    *
    * @param constant a numeric constant
    * @return a ConstantsSetBuilder instance
    */
  def +=(constant: Number): self.type = {
    this += constant.toString
  }

  /**
    * Adds all given elements to the constants set builder.
    *
    * @see [[lomrf.mln.model.builders.ConstantsSetBuilder.ConstantSymbol]]
    *
    * @param elements an iterable of constant elements
    * @tparam T a constant type
    * @return a ConstantsSetBuilder instance
    */
  def ++=[T: ConstantsSetBuilder.ConstantSymbol](elements: Iterable[T]): self.type = {
    copyIfDirty()
    for (element <- elements) element match {
      case symbol: String     => put(symbol)
      case constant: Constant => put(constant.symbol)
      case _: Number          => put(element.toString)
    }
    this
  }

  /**
    * Adds all given values to the constants set builder.
    *
    * @param e a value
    * @param elements a sequence of values
    * @return a ConstantsSetBuilder instance
    */
  def ++=(e: Any, elements: Any*): self.type = {
    copyIfDirty()
    putMatching(e)
    elements.foreach(putMatching)
    this
  }

  /**
    * Creates a constant set from all the given constant symbols.
    *
    * @return a ConstantsSet instance
    */
  override def result(): ConstantsSet = {
    /*
     Mark builder as dirty. Then copy collections in order to avoid side effects
     in the resulting ConstantsSet if any more constants are added.
     */
    dirty = true
    new ConstantsSetImpl(TCollections.unmodifiableMap(constants2Id), id2Constants)
  }

  /**
    * Creates a copy of the builder. The copy contains all constant symbols.
    *
    * @return a ConstantsSetBuilder instance
    */
  def copy(): ConstantsSetBuilder = {
    val cp_constants2Id = new TObjectIntHashMap[String](_size + 23, DEFAULT_LOAD_FACTOR, ConstantsSet.NO_ENTRY)
    cp_constants2Id.putAll(constants2Id)

    val cp_id2Constants = new mutable.ArrayBuffer[String]()
    id2Constants.foreach(cp_id2Constants += _)

    new ConstantsSetBuilder(cp_constants2Id, cp_id2Constants)
  }

  /**
    * Clears the builder.
    */
  override def clear(): Unit = {
    constants2Id = new TObjectIntHashMap[String](DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, ConstantsSet.NO_ENTRY)
    id2Constants = new mutable.ArrayBuffer[String]()
    dirty = false
  }

  /**
    * @return an iterator over all constant symbols in the builder
    */
  override def iterator: Iterator[String] = id2Constants.iterator

  /**
    * @return the number of unique constant symbols in the builder
    */
  override def size: Int = _size

  /**
    * Gives a hint of the expected set size.
    *
    * @param size a hint of the expected size
    */
  override def sizeHint(size: Int): Unit = {
    constants2Id.setUp(size)
    id2Constants.sizeHint(size)
  }

  override def toString(): String =
    s"ConstantsSetBuilder(constants2Id->{${constants2Id.size} elements}, id2Constants->{${id2Constants.size} elements})"
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

  /**
    * Creates an empty constants set builder.
    *
    * @return an empty ConstantSetBuilder instance
    */
  def apply(): ConstantsSetBuilder = new ConstantsSetBuilder

  /**
    * Creates a constants set builder from a sequence of symbols.
    *
    * @param symbols a sequence of symbols
    * @return an ConstantSetBuilder instance
    */
  def apply(symbols: String*): ConstantsSetBuilder = apply ++= symbols

  /**
    * Creates a constants set builder from an iterable of symbols.
    *
    * @param symbols an iterable of symbols
    * @return an ConstantSetBuilder instance
    */
  def apply(symbols: Iterable[String]): ConstantsSetBuilder = apply ++= symbols

  /**
    * Creates a constants set builder from an existing constant set.
    *
    * @param constantsSet a constants set
    * @return an ConstantSetBuilder instance
    */
  def apply(constantsSet: ConstantsSet): ConstantsSetBuilder = constantsSet match {
    case c: ConstantsSetImpl =>
      val _id2Constants = new mutable.ArrayBuffer[String](c.size)
      new ConstantsSetBuilder(new TObjectIntHashMap[String](c.constants2Id), _id2Constants ++= c.id2Constants)
    case c: ConstantsSetUnaryImpl =>
      new ConstantsSetBuilder += c.head
  }
}
