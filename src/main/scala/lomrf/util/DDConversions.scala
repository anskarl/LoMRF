package lomrf.util

import com.vividsolutions.jts.math.DD

/**
 * Conversions for DD numbers operation of the JTS library,
 * in order to be more Scala friendly.
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
object DDConversions {

  implicit class DDConversions(number: DD) {

    final val ZERO = new DD // CHECK IT!

    def +(other: DD) = number.add(other)
    def -(other: DD) = number.subtract(other)
    def *(other: DD) = number.multiply(other)
    def /(other: DD) = number.divide(other)
    def unary_- = number.negate()

    def ==(other: DD) = number.equals(other)
    def >(other: DD) = number.gt(other)
    def >=(other: DD) = number.ge(other)
    def <(other: DD) = number.lt(other)
    def <=(other: DD) = number.le(other)
  }
}
