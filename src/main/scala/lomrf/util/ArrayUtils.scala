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
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */

package lomrf.util

import java.util

/**
 * Array utilities.
 */
object ArrayUtils {

  /**
   * Specialized hash code calculation for arbitrary array type.
   *
   * @return the hash code for any array type
   */
  def hashCodeOf(array: Array[_]): Int = array match {
    case x: Array[Char] => util.Arrays.hashCode(x)
    case x: Array[Byte] => util.Arrays.hashCode(x)
    case x: Array[Short] => util.Arrays.hashCode(x)
    case x: Array[Int] => util.Arrays.hashCode(x)
    case x: Array[Boolean] => util.Arrays.hashCode(x)
    case x: Array[Float] => util.Arrays.hashCode(x)
    case x: Array[Long] => util.Arrays.hashCode(x)
    case x: Array[Double] => util.Arrays.hashCode(x)
    case x: Array[_] => util.Arrays.hashCode(x.asInstanceOf[Array[AnyRef]])
    case _ => throw new RuntimeException("possible bug?")
  }

  /**
   * Specialized equality between arrays for arbitrary types. It checks
   * if the arrays have the same length and contain elements from the
   * same class type.
   *
   * @param array1 one array
   * @param array2 another array
   *
   * @return true if arrays are equal, false otherwise
   */
  def equals(array1: Array[_], array2: Array[_]): Boolean = {

    // length checking
    if(array1.length != array2.length) return false

    val classOfArray1 = array1.getClass
    val classOfArray2 = array2.getClass

    // class type checking
    if(classOfArray1 == classOfArray2) array1 match {
      case x: Array[Char] => util.Arrays.equals(array1.asInstanceOf[Array[Char]], array2.asInstanceOf[Array[Char]])
      case x: Array[Byte] => util.Arrays.equals(array1.asInstanceOf[Array[Byte]], array2.asInstanceOf[Array[Byte]])
      case x: Array[Short] => util.Arrays.equals(array1.asInstanceOf[Array[Short]], array2.asInstanceOf[Array[Short]])
      case x: Array[Int] => util.Arrays.equals(array1.asInstanceOf[Array[Int]], array2.asInstanceOf[Array[Int]])
      case x: Array[Boolean] => util.Arrays.equals(array1.asInstanceOf[Array[Boolean]], array2.asInstanceOf[Array[Boolean]])
      case x: Array[Float] => util.Arrays.equals(array1.asInstanceOf[Array[Float]], array2.asInstanceOf[Array[Float]])
      case x: Array[Long] => util.Arrays.equals(array1.asInstanceOf[Array[Long]], array2.asInstanceOf[Array[Long]])
      case x: Array[Double] => util.Arrays.equals(array1.asInstanceOf[Array[Double]], array2.asInstanceOf[Array[Double]])
      case x: Array[_] => util.Arrays.equals(array1.asInstanceOf[Array[AnyRef]], array2.asInstanceOf[Array[AnyRef]])
      case _ => throw new RuntimeException("possible bug?")
    }
    else false
  }

}