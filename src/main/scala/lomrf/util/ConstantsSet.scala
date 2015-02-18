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

package lomrf.util

import gnu.trove.iterator.TObjectIntIterator
import gnu.trove.map.hash.TObjectIntHashMap
import gnu.trove.TCollections
import scala.collection.mutable
import gnu.trove.map.TObjectIntMap

sealed trait ConstantsSet extends Iterable[String]{

  def apply(id: Int): String

  def apply(constant: String): Int

  def get(id: Int): Option[String]

  def get(constant: String): Option[Int]

  def contains(constant: String): Boolean

  def valuesIterator: TObjectIntIterator[String]

  def idsIterator: Iterator[Int]

  def idsRange: Range

}

/**
 *
 * @author Anastasios Skarlatidis
 */
final class MultipleConstantsSet private[util](constants2Id: TObjectIntMap[String],
                                               id2Constants: mutable.ArrayBuffer[String]) extends ConstantsSet {

  import ConstantsSet.NO_ENTRY

  override def head = id2Constants.head

  override def last = id2Constants.last

  def apply(id: Int): String = id2Constants(id)

  def apply(constant: String): Int = constants2Id.get(constant)

  def get(id: Int): Option[String] = if (id <= id2Constants.size) Some(id2Constants(id)) else None

  def get(constant: String): Option[Int] = {
    val id = constants2Id.get(constant)
    if (id == NO_ENTRY) None else Some(id)
  }

  override def size: Int = constants2Id.size

  def contains(constant: String): Boolean = constants2Id.containsKey(constant)

  def valuesIterator = constants2Id.iterator

  def idsIterator: Iterator[Int] = (0 until id2Constants.length).iterator

  def idsRange: Range = 0 until id2Constants.length

  override def iterator = id2Constants.iterator

  override def isEmpty: Boolean = id2Constants.isEmpty

  override def toString(): String =
    s"MultipleConstantsSet(const2id->{${constants2Id.size()} elements}, id2const->{${id2Constants.size} elements})"
}

final class UnaryConstantsSet(val element: String) extends ConstantsSet{
  import ConstantsSet.NO_ENTRY


  override def head: String = element

  override def last: String = element

  def apply(id: Int): String = if(id == 0) element else null

  def apply(constant: String): Int = if(constant == element) 0 else NO_ENTRY

  def get(id: Int): Option[String] = if(id == 0) Some(element) else None

  def get(constant: String): Option[Int] = if(constant == element) Some(0) else None

  override def size: Int = 1

  def contains(constant: String): Boolean = constant == element

  def valuesIterator = new TObjectIntIterator[String](){
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

  def idsIterator: Iterator[Int] = (0 until 1).iterator

  def idsRange: Range = 0 to 0

  override def iterator: Iterator[String] = new Iterator[String]{
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

object ConstantsSet {
  val NO_ENTRY: Int = -1000

  //def apply: ConstantsSet = new MultipleConstantsSet(new TObjectIntHashMap[String](23, 0.5f, NO_ENTRY), new mutable.ArrayBuffer[String]())

  def apply(entry: String): ConstantsSet = new UnaryConstantsSet(entry)

  def apply(entries: String*): ConstantsSet = {

    if(entries.size == 1) new UnaryConstantsSet(entries.head)
    else {
      val builder = new ConstantsSetBuilder()
      entries.foreach(builder += _)
      builder.result
    }

  }
}

final class ConstantsSetBuilder {

  import ConstantsSet.NO_ENTRY

  private var dirty = false
  private var constants2Id = new TObjectIntHashMap[String](23, 0.5f, NO_ENTRY)
  private var id2Constants = new mutable.ArrayBuffer[String]()
  private var _size = 0

  def +=(constant: String) {

    if (dirty) {
      //copy
      val cp_constants2Id = new TObjectIntHashMap[String](_size + 23, 0.5f, NO_ENTRY)
      cp_constants2Id.putAll(constants2Id)
      val cp_id2Constants = new mutable.ArrayBuffer[String]()
      id2Constants.foreach(cp_id2Constants += _)
      constants2Id = cp_constants2Id
      id2Constants = cp_id2Constants
      dirty = false
    }

    if (constants2Id.putIfAbsent(constant, _size) == NO_ENTRY) {
      id2Constants += constant
      _size += 1
    }
  }


  def result: ConstantsSet = {
    dirty = true
    new MultipleConstantsSet(TCollections.unmodifiableMap(constants2Id), id2Constants)
  }

  def clear() {
    constants2Id = new TObjectIntHashMap[String](23, 0.5f, NO_ENTRY)
    id2Constants = new mutable.ArrayBuffer[String]()
    dirty = false
  }

  override def toString: String =
    s"ConstantsSetBuilder(constants2Id->{${constants2Id.size()} elements}, id2Constants->{${id2Constants.size} elements})"


  def size = _size

}

object ConstantsSetBuilder {
  def apply: ConstantsSetBuilder = new ConstantsSetBuilder
}