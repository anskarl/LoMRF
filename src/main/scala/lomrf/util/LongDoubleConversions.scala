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
  final val MAXVALUE =  DD.valueOf(Double.MaxValue)

  implicit class LongDoubleConversions(val number: LongDouble) extends AnyVal {

    def +(other: LongDouble) = number.add(other)
    def -(other: LongDouble) = number.subtract(other)
    def *(other: LongDouble) = number.multiply(other)
    def /(other: LongDouble) = number.divide(other)
    def unary_- = number.negate()

    def ==(other: LongDouble) = number.equals(other)
    def >(other: LongDouble) = number.gt(other)
    def >=(other: LongDouble) = number.ge(other)
    def <(other: LongDouble) = number.lt(other)
    def <=(other: LongDouble) = number.le(other)
  }
}
