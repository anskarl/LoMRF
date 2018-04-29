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

package lomrf.util.collection.trove

import gnu.trove.iterator._

import scala.language.implicitConversions

object TroveConversions {

  //===================== Byte =====================

  // Byte iterator
  implicit def asScalaIteratorByte(i: TByteIterator): Iterator[Byte] = i match {
    case TByteIteratorWrapper(wrapped) => wrapped
    case _                             => ScalaIteratorByteWrapper(i)
  }

  case class TByteIteratorWrapper(underlying: Iterator[Byte]) extends TByteIterator {
    def next(): Byte = underlying.next()

    def hasNext: Boolean = underlying.hasNext

    def remove(): Unit = throw new UnsupportedOperationException
  }

  case class ScalaIteratorByteWrapper(underlying: TByteIterator) extends Iterator[Byte] {
    def next(): Byte = underlying.next

    def hasNext: Boolean = underlying.hasNext
  }

  // Byte to Byte iterator
  implicit def asScalaIteratorByteByte(i: TByteByteIterator): Iterator[(Byte, Byte)] = i match {
    case TByteByteIteratorWrapper(wrapped) => wrapped
    case _                                 => ScalaIteratorByteByte(i)
  }

  case class TByteByteIteratorWrapper(underlying: Iterator[(Byte, Byte)]) extends TByteByteIterator {

    private var tuple: (Byte, Byte) = _

    def advance() {
      tuple = underlying.next()
    }

    def setValue(p1: Byte) = throw new UnsupportedOperationException

    def value(): Byte = tuple._2

    def key(): Byte = tuple._1

    def remove(): Unit = throw new UnsupportedOperationException

    def hasNext: Boolean = underlying.hasNext
  }

  case class ScalaIteratorByteByte(underlying: TByteByteIterator) extends Iterator[(Byte, Byte)] {

    def next(): (Byte, Byte) = {
      underlying.advance()
      (underlying.key(), underlying.value())
    }

    def hasNext: Boolean = underlying.hasNext
  }

  // Byte to Short iterator
  implicit def asScalaIteratorByteShort(i: TByteShortIterator): Iterator[(Byte, Short)] = i match {
    case TByteShortIteratorWrapper(wrapped) => wrapped
    case _                                  => ScalaIteratorByteShort(i)
  }

  case class TByteShortIteratorWrapper(underlying: Iterator[(Byte, Short)]) extends TByteShortIterator {

    private var tuple: (Byte, Short) = _

    def advance() {
      tuple = underlying.next()
    }

    def setValue(p1: Short) = throw new UnsupportedOperationException

    def value(): Short = tuple._2

    def key(): Byte = tuple._1

    def remove(): Unit = throw new UnsupportedOperationException

    def hasNext: Boolean = underlying.hasNext
  }

  case class ScalaIteratorByteShort(underlying: TByteShortIterator) extends Iterator[(Byte, Short)] {

    def next(): (Byte, Short) = {
      underlying.advance()
      (underlying.key(), underlying.value())
    }

    def hasNext: Boolean = underlying.hasNext
  }

  // Byte to Int iterator
  implicit def asScalaIteratorByteInt(i: TByteIntIterator): Iterator[(Byte, Int)] = i match {
    case TByteIntIteratorWrapper(wrapped) => wrapped
    case _                                => ScalaIteratorByteInt(i)
  }

  case class TByteIntIteratorWrapper(underlying: Iterator[(Byte, Int)]) extends TByteIntIterator {

    private var tuple: (Byte, Int) = _

    def advance() {
      tuple = underlying.next()
    }

    def setValue(p1: Int) = throw new UnsupportedOperationException

    def value(): Int = tuple._2

    def key(): Byte = tuple._1

    def remove(): Unit = throw new UnsupportedOperationException

    def hasNext: Boolean = underlying.hasNext
  }

  case class ScalaIteratorByteInt(underlying: TByteIntIterator) extends Iterator[(Byte, Int)] {

    def next(): (Byte, Int) = {
      underlying.advance()
      (underlying.key(), underlying.value())
    }

    def hasNext: Boolean = underlying.hasNext
  }

  // Byte to Long iterator
  implicit def asScalaIteratorByteLong(i: TByteLongIterator): Iterator[(Byte, Long)] = i match {
    case TByteLongIteratorWrapper(wrapped) => wrapped
    case _                                 => ScalaIteratorByteLong(i)
  }

  case class TByteLongIteratorWrapper(underlying: Iterator[(Byte, Long)]) extends TByteLongIterator {

    private var tuple: (Byte, Long) = _

    def advance() {
      tuple = underlying.next()
    }

    def setValue(p1: Long) = throw new UnsupportedOperationException

    def value(): Long = tuple._2

    def key(): Byte = tuple._1

    def remove(): Unit = throw new UnsupportedOperationException

    def hasNext: Boolean = underlying.hasNext
  }

  case class ScalaIteratorByteLong(underlying: TByteLongIterator) extends Iterator[(Byte, Long)] {

    def next(): (Byte, Long) = {
      underlying.advance()
      (underlying.key(), underlying.value())
    }

    def hasNext: Boolean = underlying.hasNext
  }

  // Byte to Float iterator
  implicit def asScalaIteratorByteFloat(i: TByteFloatIterator): Iterator[(Byte, Float)] = i match {
    case TByteFloatIteratorWrapper(wrapped) => wrapped
    case _                                  => ScalaIteratorByteFloat(i)
  }

  case class TByteFloatIteratorWrapper(underlying: Iterator[(Byte, Float)]) extends TByteFloatIterator {

    private var tuple: (Byte, Float) = _

    def advance() {
      tuple = underlying.next()
    }

    def setValue(p1: Float) = throw new UnsupportedOperationException

    def value(): Float = tuple._2

    def key(): Byte = tuple._1

    def remove(): Unit = throw new UnsupportedOperationException

    def hasNext: Boolean = underlying.hasNext
  }

  case class ScalaIteratorByteFloat(underlying: TByteFloatIterator) extends Iterator[(Byte, Float)] {

    def next(): (Byte, Float) = {
      underlying.advance()
      (underlying.key(), underlying.value())
    }

    def hasNext: Boolean = underlying.hasNext
  }

  // Byte to Double iterator
  implicit def asScalaIteratorByteDouble(i: TByteDoubleIterator): Iterator[(Byte, Double)] = i match {
    case TByteDoubleIteratorWrapper(wrapped) => wrapped
    case _                                   => ScalaIteratorByteDouble(i)
  }

  case class TByteDoubleIteratorWrapper(underlying: Iterator[(Byte, Double)]) extends TByteDoubleIterator {

    private var tuple: (Byte, Double) = _

    def advance() {
      tuple = underlying.next()
    }

    def setValue(p1: Double) = throw new UnsupportedOperationException

    def value(): Double = tuple._2

    def key(): Byte = tuple._1

    def remove(): Unit = throw new UnsupportedOperationException

    def hasNext: Boolean = underlying.hasNext
  }

  case class ScalaIteratorByteDouble(underlying: TByteDoubleIterator) extends Iterator[(Byte, Double)] {

    def next(): (Byte, Double) = {
      underlying.advance()
      (underlying.key(), underlying.value())
    }

    def hasNext: Boolean = underlying.hasNext
  }

  // Byte to Object iterator
  implicit def asScalaIteratorByteObject[T](i: TByteObjectIterator[T]): Iterator[(Byte, T)] = i match {
    case TByteObjectIteratorWrapper(wrapped) => wrapped
    case _                                   => ScalaIteratorByteObject[T](i)
  }

  case class TByteObjectIteratorWrapper[T](underlying: Iterator[(Byte, T)]) extends TByteObjectIterator[T] {

    private var tuple: (Byte, T) = _

    def advance() {
      tuple = underlying.next()
    }

    def setValue(p1: T) = throw new UnsupportedOperationException

    def value(): T = tuple._2

    def key(): Byte = tuple._1

    def remove(): Unit = throw new UnsupportedOperationException

    def hasNext: Boolean = underlying.hasNext
  }

  case class ScalaIteratorByteObject[T](underlying: TByteObjectIterator[T]) extends Iterator[(Byte, T)] {

    def next(): (Byte, T) = {
      underlying.advance()
      (underlying.key(), underlying.value())
    }

    def hasNext: Boolean = underlying.hasNext
  }

  // iterable
  /*
  implicit def asScalaIterableByte(i: TByteCollection): Iterable[Byte] = ScalaIterableByteWrapper(i)

  case class ScalaIterableByteWrapper(underlying: TByteCollection) extends Iterable[Byte]{
    def iterator = ScalaIteratorByteWrapper(underlying.iterator())
    override def size = underlying.size()
    override def isEmpty = underlying.isEmpty
  } */

  //===================== Short =====================

  implicit def asScalaIteratorShort(i: TShortIterator): Iterator[Short] = i match {
    case TShortIteratorWrapper(wrapped) => wrapped
    case _                              => ScalaIteratorShortWrapper(i)
  }

  case class TShortIteratorWrapper(underlying: Iterator[Short]) extends TShortIterator {
    def next(): Short = underlying.next()

    def hasNext: Boolean = underlying.hasNext

    def remove(): Unit = throw new UnsupportedOperationException
  }

  case class ScalaIteratorShortWrapper(underlying: TShortIterator) extends Iterator[Short] {
    def next(): Short = underlying.next

    def hasNext: Boolean = underlying.hasNext
  }

  //===================== Int =====================

  implicit def asScalaIteratorInt(i: gnu.trove.iterator.TIntIterator): Iterator[Int] = i match {
    case TIntIteratorWrapper(wrapped) => wrapped
    case _                            => ScalaIteratorIntWrapper(i)
  }

  case class TIntIteratorWrapper(underlying: Iterator[Int]) extends TIntIterator {
    def next(): Int = underlying.next()

    def hasNext: Boolean = underlying.hasNext

    def remove(): Unit = throw new UnsupportedOperationException
  }

  case class ScalaIteratorIntWrapper(underlying: gnu.trove.iterator.TIntIterator) extends Iterator[Int] {
    def next(): Int = underlying.next

    def hasNext: Boolean = underlying.hasNext
  }

  //===================== (Int, Object) =====================
  implicit def asScalaIteratorIntObj[T](i: gnu.trove.iterator.TIntObjectIterator[T]): Iterator[(Int, T)] = i match {
    case TIntObjectIteratorWrapper(wrapped) => wrapped
    case _                                  => ScalaIteratorIntObjectWrapper(i)
  }

  implicit def tIntObjectHashMapToScalaIterator[T](i: gnu.trove.map.TIntObjectMap[T]): Iterator[(Int, T)] = i.iterator()

  implicit def tIntObjectHashMapToScalaIterable[T](i: gnu.trove.map.TIntObjectMap[T]): Iterable[(Int, T)] = {
    new Iterable[(Int, T)] {
      override def iterator: Iterator[(Int, T)] = i.iterator()
    }
  }

  case class TIntObjectIteratorWrapper[T](underlying: Iterator[(Int, T)]) extends TIntObjectIterator[T] {
    private var _key: Int = _
    private var _value: T = _

    override def key(): Int = _key

    override def setValue(p1: T) = throw new UnsupportedOperationException

    override def value(): T = _value

    override def advance(): Unit = {
      val (fetchedKey, fetchedValue) = underlying.next()
      _key = fetchedKey
      _value = fetchedValue
    }

    override def hasNext: Boolean = underlying.hasNext
    def remove(): Unit = throw new UnsupportedOperationException
  }

  case class ScalaIteratorIntObjectWrapper[T](underlying: gnu.trove.iterator.TIntObjectIterator[T]) extends Iterator[(Int, T)] {
    def next(): (Int, T) = {
      underlying.advance()
      (underlying.key(), underlying.value())
    }

    def hasNext: Boolean = underlying.hasNext
  }

  //===================== Long =====================
  implicit def asScalaIteratorLong(i: gnu.trove.iterator.TLongIterator): Iterator[Long] = i match {
    case TLongIteratorWrapper(wrapped) => wrapped
    case _                             => ScalaIteratorLongWrapper(i)
  }

  case class TLongIteratorWrapper(underlying: Iterator[Long]) extends TLongIterator {
    def next(): Long = underlying.next()

    def hasNext: Boolean = underlying.hasNext

    def remove(): Unit = throw new UnsupportedOperationException
  }

  case class ScalaIteratorLongWrapper(underlying: gnu.trove.iterator.TLongIterator) extends Iterator[Long] {
    def next(): Long = underlying.next

    def hasNext: Boolean = underlying.hasNext
  }

  //===================== Float =====================
  implicit def asScalaIteratorFloat(i: gnu.trove.iterator.TFloatIterator): Iterator[Float] = i match {
    case TFloatIteratorWrapper(wrapped) => wrapped
    case _                              => ScalaIteratorFloatWrapper(i)
  }

  case class TFloatIteratorWrapper(underlying: Iterator[Float]) extends TFloatIterator {
    def next(): Float = underlying.next()

    def hasNext: Boolean = underlying.hasNext

    def remove(): Unit = throw new UnsupportedOperationException
  }

  case class ScalaIteratorFloatWrapper(underlying: gnu.trove.iterator.TFloatIterator) extends Iterator[Float] {
    def next(): Float = underlying.next

    def hasNext: Boolean = underlying.hasNext
  }

  //===================== Double =====================
  implicit def asScalaIteratorDouble(i: gnu.trove.iterator.TDoubleIterator): Iterator[Double] = i match {
    case TDoubleIteratorWrapper(wrapped) => wrapped
    case _                               => ScalaIteratorDoubleWrapper(i)
  }

  case class TDoubleIteratorWrapper(underlying: Iterator[Double]) extends TDoubleIterator {
    def next(): Double = underlying.next()

    def hasNext: Boolean = underlying.hasNext

    def remove(): Unit = throw new UnsupportedOperationException
  }

  case class ScalaIteratorDoubleWrapper(underlying: gnu.trove.iterator.TDoubleIterator) extends Iterator[Double] {
    def next(): Double = underlying.next

    def hasNext: Boolean = underlying.hasNext
  }
}
