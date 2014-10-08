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

/**
 *
 * Provides standard logging functionality, using scala-friendly syntax. The logging is managed by the SLF4j library.
 * Each class that implements this trait will be associated with a logger and the logging identity is defined by the
 * class name.
 *
 * @author Anastasios Skarlatidis
 */
trait Logging {

  // Lazy initialization of logger instance, using the current class name as logger name
  protected lazy val loggerInstance: Logger = Logger(getClass.getName)


  /**
   * Log debug messages only when debug is enabled, otherwise the message is ignored.
   *
   * @param message: the message to log
   */
  protected def debug(message: => String) = loggerInstance.debug(message)

  /**
   * Log debug messages only when debug is enabled, otherwise the message is ignored.
   *
   * @param message: the message to log
   * @param ex: the thrown exception
   */
  protected def debug(message: => String, ex: => Throwable) = loggerInstance.debug(message, ex)

  /**
   * Log the returned value of a particular instance (it is strongly suggested to use real debug,
   * instead of this utility function).
   *
   * @param valueName: the name of a particular instance
   * @param value: the instance's value
   * @return the instance's value
   */
  protected def debugValue[T](valueName: => String, value: => T) = loggerInstance.debugValue(valueName, value)

  /**
   * Log informational messages (only when info is enabled, otherwise the message is ignored).
   *
   * @param message: the informational message to log
   */
  protected def info(message: => String) = loggerInstance.info(message)

  /**
   * Log informational messages (only when info messages are enabled, otherwise the message is ignored) accompanied
   * with the thrown exception.
   *
   * @param message: the informational message to log
   * @param ex: the thrown exception
   */
  protected def info(message: => String, ex: => Throwable) = loggerInstance.info(message, ex)

  /**
   * Log warning messages (only when info is enabled, otherwise the message is ignored).
   *
   * @param message: the informational message to log
   */
  protected def warn(message: => String) = loggerInstance.warn(message)


  /**
   * Log warning messages (only when warning messages are enabled, otherwise the message is ignored) accompanied with
   * the thrown exception.
   *
   * @param message: the warning message to log
   * @param ex: the thrown exception to log
   */
  protected def warn(message: => String, ex: => Throwable) = loggerInstance.warn(message, ex)


  /**
   * Log error messages (only when error messages are enabled, otherwise the message is ignored) accompanied with
   * the thrown exception.
   *
   * @param message: the error message to log
   */
  protected def error(message: => String) = loggerInstance.error(message)


  /**
   * Log error messages (only when error messages are enabled, otherwise the message is ignored) accompanied with
   * the thrown exception.
   *
   * @param message: the error message to log
   * @param ex: the thrown exception to log
   */
  protected def error(message: => String, ex: => Throwable) = loggerInstance.error(message, ex)


  /**
   * Logs the given message and exits with exit code '1'
   *
   * @param message: the message to log before exiting
   */
  protected def fatal(message: => String) = loggerInstance.fatal(message)

  /**
   * Logs the given message and exception and exits with the specified exit code (default is '1')
   *
   * @param message: the message to log before exiting
   * @param ex: the thrown exception to log
   * @param exitCode exit code (default is '1')
   */
  protected def fatal(message: => String, ex: => Throwable, exitCode: Int = 1) = loggerInstance.fatal(message, ex, exitCode)


  /**
   * Log a trace message
   *
   * @param message: the message to log
   *
   * @see <a href="http://www.slf4j.org/faq.html#trace">www.slf4j.org/faq.html#trace</a>
   */
  protected def trace(message: => String) = loggerInstance.trace(message)


  /**
   * Log trace message and exception
   *
   * @param message:the message to log
   * @param ex: the thrown exception to log
   *
   * @see <a href="http://www.slf4j.org/faq.html#trace">www.slf4j.org/faq.html#trace</a>
   */
  protected def trace(message: => String, ex: => Throwable) = loggerInstance.trace(message, ex)


  /**
   * Determine whether INFO log messages are enabled in logger's configuration/runtime.
   *
   * @return true when INFO log messages are enabled, otherwise false.
   */
  protected def isInfoEnabled = loggerInstance.isInfoEnabled

  /**
   * Determine whether WARN log messages are enabled in logger's configuration/runtime.
   *
   * @return true when WARN log messages are enabled, otherwise false.
   */
  protected def isWarnEnabled = loggerInstance.isWarnEnabled

  /**
   * Determine whether ERROR log messages are enabled in logger's configuration/runtime.
   *
   * @return true when ERROR log messages are enabled, otherwise false.
   */
  protected def isErrorEnabled = loggerInstance.isErrorEnabled

  /**
   * Determine whether FATAL log messages are enabled in logger's configuration/runtime.
   *
   * @return true when ERROR log messages are also enabled, otherwise false.
   */
  protected def isFatalEnabled = loggerInstance.isErrorEnabled

  /**
   * Determine whether TRACE log messages are enabled in logger's configuration/runtime.
   *
   * @return true when TRACE log messages are enabled, otherwise false.
   */
  protected def isTraceEnabled = loggerInstance.isTraceEnabled

  /**
   * Determine whether DEBUG log messages are enabled in logger's configuration/runtime.
   *
   * @return true when DEBUG log messages are enabled, otherwise false.
   */
  protected def isDebugEnabled = loggerInstance.isDebugEnabled


  def whenDebug(body: => Unit): Unit = if (loggerInstance.isDebugEnabled) body
}