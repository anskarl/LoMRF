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

package lomrf.util

import com.vividsolutions.jts.math.DD

/**
  * Conversions for DD numbers operation of the JTS library,
  * in order to be more Scala friendly.
  */
object LongDoubleConversions {

  type LongDouble = DD

  final val ZERO = DD.valueOf(0.0)
  final val ONE = DD.valueOf(1.0)
  final val MAXVALUE = DD.valueOf(9.9999999999999999E300) // Do not used DD.valueOf(Double.MaxValue)

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
