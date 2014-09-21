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

import org.slf4j.{LoggerFactory, MarkerFactory}

import scala.reflect._

/**
 * Companion object for logging functionality
 *
 * @author Anastasios Skarlatidis
 */
object Logger {

  protected val FATAL_ERROR_MARKER = MarkerFactory.getMarker("FATAL")

  /**
   * Get a new logger where its name is given by: org.slf4j.Logger.ROOT_LOGGER_NAME
   *
   * @return logger instance
   * @see org.slf4j.Logger.ROOT_LOGGER_NAME
   */
  def apply(): Logger = apply(org.slf4j.Logger.ROOT_LOGGER_NAME)

  /**
   * Get a new logger with the specified name.
   *
   * @param name: the name of the logger
   * @return logger instance
   */
  def apply(name: String): Logger = new Logger(LoggerFactory.getLogger(name))

  /**
   * Get a new logger where its name is given by the name of the specified class
   *
   * @param clazz class type
   * @return logger instance
   */
  def apply(clazz: Class[_]): Logger = this.apply(clazz.getName)

  /**
   * Get a new logger where its name is given by the name of the specified class tag type
   *
   * @tparam T: class tag type
   * @return logger instance
   */
  def apply[T: ClassTag](): Logger = this.apply(classTag[T].runtimeClass.getName)

}

/**
 *
 * Provides standard logging functionality, using the SLF4j library.
 *
 * @author Anastasios Skarlatidis
 */
class Logger protected(val instance: org.slf4j.Logger) {

  /**
   * Log debug messages only when debug is enabled, otherwise the message is ignored.
   *
   * @param message: the message to log
   */
  @inline
  final def debug(message: => String): Unit =
    if (instance.isDebugEnabled) instance.debug(message)

  /**
   * Log debug messages only when debug is enabled, otherwise the message is ignored.
   *
   * @param message: the message to log
   * @param ex: the thrown exception
   */
  @inline
  final def debug(message: => String, ex: => Throwable): Unit =
    if (instance.isDebugEnabled) instance.debug(message, ex)

  /**
   * Log the returned value of a particular instance (it is strongly suggested to use real debug,
   * instead of this utility function).
   *
   * @param valueName: the name of a particular instance
   * @param value: the instance's value
   * @return the instance's value
   */
  @inline
  final def debugValue[T](valueName: => String, value: => T): T = {
    debug("Value: '" + valueName + "' is '" + value.toString + "'")
    value
  }

  /**
   * Log informational messages (only when info is enabled, otherwise the message is ignored).
   *
   * @param message: the informational message to log
   */
  @inline
  final def info(message: => String): Unit =
    if (instance.isInfoEnabled) instance.info(message)

  /**
   * Log informational messages (only when info messages are enabled, otherwise the message is ignored) accompanied
   * with the thrown exception.
   *
   * @param message: the informational message to log
   * @param ex: the thrown exception
   */
  @inline
  final def info(message: => String, ex: => Throwable): Unit =
    if (instance.isInfoEnabled) instance.info(message, ex)

  /**
   * Log warning messages (only when info is enabled, otherwise the message is ignored).
   *
   * @param message: the informational message to log
   */
  @inline
  final def warn(message: => String): Unit =
    if (instance.isWarnEnabled) instance.warn(message)


  /**
   * Log warning messages (only when warning messages are enabled, otherwise the message is ignored) accompanied with
   * the thrown exception.
   *
   * @param message: the warning message to log
   * @param ex: the thrown exception to log
   */
  @inline
  final def warn(message: => String, ex: => Throwable) =
    if (instance.isWarnEnabled) instance.warn(message, ex)


  /**
   * Log error messages (only when error messages are enabled, otherwise the message is ignored) accompanied with
   * the thrown exception.
   *
   * @param message: the error message to log
   */
  @inline
  final def error(message: => String): Unit =
    if (instance.isErrorEnabled) instance.error(message)


  /**
   * Log error messages (only when error messages are enabled, otherwise the message is ignored) accompanied with
   * the thrown exception.
   *
   * @param message: the error message to log
   * @param ex: the thrown exception to log
   */
  @inline
  final def error(message: => String, ex: => Throwable): Unit =
    if (instance.isErrorEnabled) instance.error(message, ex)


  /**
   * Logs the given message and exits with exit code '1'
   *
   * @param message: the message to log before exiting
   */
  @inline
  final def fatal(message: => String) = {
    if (instance.isErrorEnabled) instance.error(Logger.FATAL_ERROR_MARKER, message)
    sys.exit(1)
  }

  /**
   * Logs the given message and exception and exits with the specified exit code (default is '1')
   *
   * @param message: the message to log before exiting
   * @param ex: the thrown exception to log
   * @param exitCode exit code (default is '1')
   */
  @inline
  final def fatal(message: => String, ex: => Throwable, exitCode: Int = 1) = {
    if (instance.isErrorEnabled) instance.error(Logger.FATAL_ERROR_MARKER, message, ex)
    sys.exit(exitCode)
  }


  /**
   * Log a trace message
   *
   * @param message: the message to log
   *
   * @see <a href="http://www.slf4j.org/faq.html#trace">www.slf4j.org/faq.html#trace</a>
   */
  @inline
  final def trace(message: => String): Unit =
    if (instance.isTraceEnabled) instance.trace(message)


  /**
   * Log trace message and exception
   *
   * @param message:the message to log
   * @param ex: the thrown exception to log
   *
   * @see <a href="http://www.slf4j.org/faq.html#trace">www.slf4j.org/faq.html#trace</a>
   */
  @inline
  final def trace(message: => String, ex: => Throwable): Unit =
    if (instance.isTraceEnabled) instance.trace(message, ex)


  /**
   * Determine whether INFO log messages are enabled in logger's configuration/runtime.
   *
   * @return true when INFO log messages are enabled, otherwise false.
   */
  def isInfoEnabled = instance.isInfoEnabled

  /**
   * Determine whether WARN log messages are enabled in logger's configuration/runtime.
   *
   * @return true when WARN log messages are enabled, otherwise false.
   */
  def isWarnEnabled = instance.isWarnEnabled

  /**
   * Determine whether ERROR log messages are enabled in logger's configuration/runtime.
   *
   * @return true when ERROR log messages are enabled, otherwise false.
   */
  def isErrorEnabled = instance.isErrorEnabled

  /**
   * Determine whether FATAL log messages are enabled in logger's configuration/runtime.
   *
   * @return true when ERROR log messages are also enabled, otherwise false.
   */
  def isFatalEnabled = instance.isErrorEnabled

  /**
   * Determine whether TRACE log messages are enabled in logger's configuration/runtime.
   *
   * @return true when TRACE log messages are enabled, otherwise false.
   */
  def isTraceEnabled = instance.isTraceEnabled

  /**
   * Determine whether DEBUG log messages are enabled in logger's configuration/runtime.
   *
   * @return true when DEBUG log messages are enabled, otherwise false.
   */
  def isDebugEnabled = instance.isDebugEnabled

}