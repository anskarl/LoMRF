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

package lomrf.util.collection

import scala.collection.mutable
import gnu.trove.map.hash.TObjectIntHashMap

/**
 * @author Anastasios Skarlatidis
 */

class HashArray[@specialized T](initialCapacity: Int = 23, loadFactor: Float = 0.75f, noEntryValue: Int = -1) {

  private var _elements = new mutable.ArrayBuffer[T]()
  private var _indices = new TObjectIntHashMap[T](initialCapacity, loadFactor, noEntryValue)
  private var _size: Int = 0


  def apply(idx: Int): T = _elements(idx)

  def get(idx: Int): Option[T] = {
    if (idx >= _size) None
    else Some(_elements(idx))
  }

  def contains(element: T): Boolean = _indices.containsKey(element)

  def clear() {
    if (_size > 0) {
      _elements = new mutable.ArrayBuffer[T]()
      _indices = new TObjectIntHashMap[T](initialCapacity, loadFactor, noEntryValue)
      _size = 0
    }
  }

  def isEmpty: Boolean = _size == 0

  def +=(element: T) {
    if (_indices.putIfAbsent(element, _size) == noEntryValue) {
      _elements += element
      _size += 1
    }
  }

  def -=(element: T) {
    val idx = _indices.remove(element)
    if (idx != noEntryValue) {
      _size -= 1
      if (idx == _size) _elements.remove(idx)
      else {
        val last = _elements(_size)
        _indices.put(last, idx)
        _elements(idx) = last
        _elements.remove(_size)
      }
    }
  }

  def size = _size

  def elements = _elements

  def indices = _indices


}