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

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.pattern.color.ForegroundCompositeConverterBase

import scala.annotation.switch

class LogbackColouredHighlighter extends ForegroundCompositeConverterBase[ILoggingEvent] {

  import LogbackColouredHighlighter._

  override def getForegroundColorCode(event: ILoggingEvent): String =
    (event.getLevel.levelInt: @switch) match {
      case Level.ERROR_INT => STYLE_ERROR
      case Level.WARN_INT => STYLE_WARN
      case Level.INFO_INT => STYLE_INFO
      case Level.DEBUG_INT => STYLE_DEBUG
      case Level.TRACE_INT => STYLE_TRACE
      case _ => DEFAULT_FG
    }
}

object LogbackColouredHighlighter {
  final val DEFAULT_FG: String = "39"
  final val STYLE_ERROR = "31" // Red
  final val STYLE_WARN = "33" // Orange
  final val STYLE_INFO = "32" // Green
  final val STYLE_DEBUG = "34" // Blue
  final val STYLE_TRACE = "35" // Magenta
}
