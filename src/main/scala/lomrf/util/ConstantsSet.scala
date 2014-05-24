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

import gnu.trove.map.hash.TObjectIntHashMap
import gnu.trove.TCollections
import scala.collection.mutable
import gnu.trove.map.TObjectIntMap

/**
 *
 * @author Anastasios Skarlatidis
 */
final class ConstantsSet private[util](val constants2Id: TObjectIntMap[String],
                                       id2Constants: mutable.ArrayBuffer[String]) extends Iterable[String] {

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

  override def toString(): String = {
    "ConstantsSet{\n constants2Id->" + constants2Id + "\n" +
      "id2Constants->" + id2Constants + "\n}"
  }
}

object ConstantsSet {
  val NO_ENTRY: Int = -1000

  def apply: ConstantsSet = new ConstantsSet(new TObjectIntHashMap[String](23, 0.5f, NO_ENTRY), new mutable.ArrayBuffer[String]())

  def apply(entries: String*): ConstantsSet = {
    val builder = new ConstantsSetBuilder()
    entries.foreach(builder += _)
    builder.result
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
    new ConstantsSet(TCollections.unmodifiableMap(constants2Id), id2Constants)
  }

  def clear() {
    constants2Id = new TObjectIntHashMap[String](23, 0.5f, NO_ENTRY)
    id2Constants = new mutable.ArrayBuffer[String]()
    dirty = false
  }

  override def toString: String = {
    "ConstantsSetBuilder{\n constants2Id->" + constants2Id + "\n" +
      "id2Constants->" + id2Constants + "\n}"
  }

  def size = _size

}

object ConstantsSetBuilder {
  def apply: ConstantsSetBuilder = new ConstantsSetBuilder
}