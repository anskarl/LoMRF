/*
 * .    _           _____    ____ _____
 *   ___| | ___   __|_   _|  / _  |____ |
 *  |__ \ |/ _ \ / _ \| |   | (_| | |_  |
 *  / __/ | (_) | (_) | |    > _  |___| |
 *  \___|_|\___/ \___/|_|   /_/ |_|_____|
 *
 * Event Recognition Tools.
 *
 * Copyright (c) 2012 Anastasios Skarlatidis.
 * All rights reserved.
 */

package lomrf.util

import org.slf4j.{Logger, LoggerFactory}

/**
 * @author Anastasios Skarlatidis
 * Date: Jul 22, 2010
 */

trait Logging {

  implicit val logger: Logging = this

  private lazy val _logger: Logger = LoggerFactory.getLogger(getClass.getName)

  lazy val isDebugEnabled = _logger.isDebugEnabled

  def debug(message: => String) = {if (_logger.isDebugEnabled) _logger.debug(message)}
  def debug(message: => String, ex: => Throwable) = {if (_logger.isDebugEnabled) _logger.debug(message,ex)}
  
  def debugValue[T](valueName: String, value: => T):T = {
    val result:T = value
    debug(valueName + " == " + result.toString)
    result
  }

  def info(message: => String) = {if (_logger.isInfoEnabled) _logger.info(message)}
  def info(message: => String, ex: => Throwable) = { if (_logger.isInfoEnabled) _logger.info(message,ex)}

 /* def info(messages: (Unit => String)*) = if (_logger.isInfoEnabled) {
    _logger.info(messages.reduceLeft( _ + "\n" + _.apply()))
  }*/

  def warn(message: => String) {if (_logger.isWarnEnabled) _logger.warn(message)}
  def warn(message: => String, ex: => Throwable) = {if (_logger.isWarnEnabled) _logger.warn(message,ex)}

  /*def error(ex: => Throwable) = {
    if (_logger.isErrorEnabled) {
      _logger.error(ex.toString,ex)
      sys.error(ex.toString)
    }
  }*/


  def error(message: => String) = {
    if (_logger.isErrorEnabled){
      _logger.error(message)
      sys.error(message)
    }
  }

  def error(message: => String, ex: => Throwable) =
      if(_logger.isErrorEnabled) {
      _logger.error(message, ex)
      sys.error(message)
    }


  //def fatal(ex: => Throwable): Nothing = fatal(ex, 1)

  def fatal(message: => String, ex: => Throwable, exitValue: => Int = 1): Nothing = {
      _logger.error(message, ex)
      sys.exit(exitValue)
    }

  def fatal(message: => String): Nothing ={
    _logger.error(message)
    sys.exit(1)
  }

  def fatal(message: => String, ex: => Throwable): Nothing = {
    _logger.error(message, ex)
    sys.exit(1)
  }


  def whenDebug(body: => Unit): Unit = if(_logger.isDebugEnabled) body
}