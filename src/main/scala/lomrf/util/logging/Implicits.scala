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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.util.logging

import com.typesafe.scalalogging.Logger
import org.slf4j.MarkerFactory

object Implicits {

  final val FATAL_ERROR_MARKER = MarkerFactory.getMarker("FATAL")

  implicit class RichLogger(val instance: Logger) extends AnyVal {

    def fatal(message: => String): Nothing ={
      instance.whenErrorEnabled{
        instance.error(Implicits.FATAL_ERROR_MARKER, message)
      }
      sys.exit(1)
    }

    final def fatal(message: => String, ex: => Throwable, exitCode: Int = 1): Nothing = {
      instance.whenErrorEnabled{
        instance.error(Implicits.FATAL_ERROR_MARKER, message)
      }
      sys.exit(exitCode)
    }
  }
}


