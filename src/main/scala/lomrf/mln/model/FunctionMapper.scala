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

package lomrf.mln.model

import gnu.trove.TCollections
import gnu.trove.map.TIntObjectMap
import gnu.trove.map.hash.TIntObjectHashMap
import lomrf.util.AtomIdentityFunction

trait FunctionMapper {

  def apply(args: Vector[String]): String

  def get(args: Vector[String]): Option[String]
}

object FunctionMapper {
  def apply(fb: FunctionMapperBuilder): FunctionMapper = fb.result()

  def apply(idf: AtomIdentityFunction, args2Value: TIntObjectMap[String]): FunctionMapper = new FunctionMapperDefaultImpl(idf, args2Value)

  def apply(func: Vector[String] => String): FunctionMapper = new FunctionMapperSpecialImpl(func)
}

final class FunctionMapperDefaultImpl(identityFunction: AtomIdentityFunction, args2Value: TIntObjectMap[String]) extends FunctionMapper {

  def apply(args: Vector[String]): String = {
    val id = identityFunction.encode(args)
    args2Value.get(id)
  }

  def get(args: Vector[String]): Option[String] = {
    val id = identityFunction.encode(args)
    val result = args2Value.get(id)
    if (result eq null) None else Some(result)
  }

  override def toString = s"FunctionMapperDefaultImpl{signature:= ${identityFunction.signature}, size:=${args2Value.size()}"

}

final class FunctionMapperBuilder(identityFunction: AtomIdentityFunction) {

  private var dirty = false
  private var args2Value = new TIntObjectHashMap[String]()

  def +=(args: Vector[String], value: String) {
    if (dirty) {
      //copy
      val cp_args2Value = new TIntObjectHashMap[String](args2Value.size)
      cp_args2Value.putAll(args2Value)
      args2Value = cp_args2Value
      dirty = false
    }
    val id = identityFunction.encode(args)
    args2Value.putIfAbsent(id, value)
  }

  def result(): FunctionMapper = {
    dirty = true
    new FunctionMapperDefaultImpl(identityFunction, TCollections.unmodifiableMap(args2Value))
  }

  def clear() {
    args2Value = new TIntObjectHashMap[String]()
    dirty = false
  }
}

final class FunctionMapperSpecialImpl(func: Vector[String] => String) extends FunctionMapper {


  def apply(args: Vector[String]) = func(args)

  def get(args: Vector[String]): Option[String] = Some(func(args))

  override def toString = s"FunctionMapperSpecialImpl{f:= $func}"

}
