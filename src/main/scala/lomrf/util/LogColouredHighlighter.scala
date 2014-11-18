package lomrf.util

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.pattern.color.ForegroundCompositeConverterBase

import scala.annotation.switch

/**
 * @author Anastasios Skarlatidis
 */
final class LogColouredHighlighter extends ForegroundCompositeConverterBase[ILoggingEvent]{
  private val DEFAULT_FG: String = "39"
  private val STYLE_ERROR = "31" // Red
  private val STYLE_WARN = "33" // Orange
  private val STYLE_INFO = "32" // Green
  private val STYLE_DEBUG = "35" // Magenta
  private val STYLE_TRACE = "35" // Magenta

  override def getForegroundColorCode(event: ILoggingEvent): String =
    ( event.getLevel.toInt: @switch) match {
      case Level.ERROR_INT => STYLE_ERROR
      case Level.WARN_INT => STYLE_WARN
      case Level.INFO_INT => STYLE_INFO
      case Level.DEBUG_INT => STYLE_DEBUG
      case Level.TRACE_INT => STYLE_TRACE
      case _ => DEFAULT_FG
    }



}
