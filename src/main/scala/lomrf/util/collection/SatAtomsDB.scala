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

import gnu.trove.set.hash.TIntHashSet

/**
 * @author Anastasios Skarlatidis
 */

object SatAtomsDB{
  def apply(length: Int): SatAtomsDB ={
    if(length == 1) new SatAtomDBUnit
    else if(length >1 && length <= 10) new SatAtomDBArrayImpl(length)
    else new SatAtomDBHashImpl()
  }
}

trait SatAtomsDB {
  def add(atomId: Int): Boolean
  def remove(atomId:Int): Boolean
  def contains(atomId: Int): Boolean
  def size: Int
  def isEmpty: Boolean
  def capacity: Int
  def clear()
}

private class SatAtomDBUnit extends SatAtomsDB{
  private var _atomId = 0

  def add(atomId: Int): Boolean = {
    if (_atomId != atomId) {
      _atomId = atomId
      true
    } else false
  }

  def remove(atomId: Int): Boolean = {
    if (atomId == _atomId) {
      _atomId = 0
      true
    } else false
  }

  def contains(atomId: Int): Boolean = _atomId == atomId

  def size: Int = if(_atomId != 0) 1 else 0

  def isEmpty = _atomId == 0

  def capacity = 1

  def clear() { _atomId = 0 }
}

private class SatAtomDBArrayImpl(length: Int) extends SatAtomsDB{
  import java.util.Arrays._

  private val array = new Array[Int](length)
  fill(array, Int.MaxValue)

  private var _size = 0

  def add(atomId: Int): Boolean = {
    if(_size == 0){
      array(_size) = atomId
      _size += 1
      true
    } else if(_size == array.length){
      false
    } else if(binarySearch(array, 0, _size, atomId) < 0) {
      array(_size) = atomId
      sort(array)
      _size +=1
      true
    } else false
  }

  def remove(atomId: Int): Boolean ={
    if(_size == 0 ) false
    else{
      val idx = binarySearch(array, atomId)
      if (idx >= 0 ){
        array(idx) = Int.MaxValue
        sort(array)
        _size -= 1
        true
      } else false
    }
  }

  def contains(atomId: Int): Boolean = {
    if(_size == 0) false
    else binarySearch(array, atomId) >=0
  }

  def size: Int = _size

  def isEmpty = _size == 0

  def capacity = array.length

  def clear(){
    fill(array, Int.MaxValue)
    _size = 0
  }
}

private class SatAtomDBHashImpl() extends SatAtomsDB{
  private val set = new TIntHashSet()

  def add(atomId: Int): Boolean = set.add(atomId)

  def remove(atomId: Int): Boolean = {
    set.remove(atomId)
  }

  def contains(atomId: Int): Boolean = set.contains(atomId)

  def size: Int = set.size()

  def isEmpty = set.isEmpty

  def capacity = set.capacity()

  def clear(){
    set.clear()
  }
}