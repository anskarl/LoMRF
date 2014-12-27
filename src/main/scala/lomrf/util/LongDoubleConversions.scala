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

import com.vividsolutions.jts.math.DD

/**
 * Conversions for DD numbers operation of the JTS library,
 * in order to be more Scala friendly.
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
object LongDoubleConversions {

  type LongDouble = DD

  final val ZERO = DD.valueOf(0.0)
  final val ONE = DD.valueOf(1.0)
  final val MAXVALUE =  DD.valueOf(9.9999999999999999E300) // Do not used DD.valueOf(Double.MaxValue)

  /**
   * According to Scala specification, value classes are not able to
   * define a equals or hashCode method.
   *
   * @see http://docs.scala-lang.org/overviews/core/value-classes.html
   *
   * @param number LongDouble value
   */
  implicit class LongDoubleConversions(val number: LongDouble) extends AnyVal {

    def +(other: LongDouble) = number.add(other)
    def -(other: LongDouble) = number.subtract(other)
    def *(other: LongDouble) = number.multiply(other)
    def /(other: LongDouble) = number.divide(other)
    def unary_- = number.negate()

    def ===(other: LongDouble) = number.equals(other)
    def >(other: LongDouble) = number.gt(other)
    def >=(other: LongDouble) = number.ge(other)
    def <(other: LongDouble) = number.lt(other)
    def <=(other: LongDouble) = number.le(other)
  }
}
