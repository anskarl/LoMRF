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

/**
 * The source code below is an extended/modified version of the code in https://gist.github.com/246481
 */
package lomrf.util

import scala.collection.mutable.ListBuffer


/**
 * This is the base class for options.
 */
case class OptionDefinition( canBeInvoked: Boolean, shortopt: String, longopt: String, keyName: String, valueName: String,
                             description: String, action: String => Unit, gobbleNextArgument: Boolean, keyValueArgument: Boolean) {
  def shortDescription = "option " + longopt
}

// ----- Some standard option types ---------
class SeparatorDefinition(description: String)
  extends OptionDefinition(false, null, null, null, null, description, {a: String => {}}, false, false)

class Argument(name: String, description: String, val allowMultiple: Boolean, action: String => Unit )
  extends OptionDefinition(false, null, name, null, name, description, action, false, false) {
  override def shortDescription = "argument " + name
}

class ArgOptionDefinition( shortopt: String, longopt: String, valueName: String, description: String, action: String => Unit)
  extends OptionDefinition(true, shortopt, longopt, null, valueName, description, action, true, false)

class IntArgOptionDefinition( shortopt: String, longopt: String, valueName: String, description: String, action: Int => Unit)
  extends OptionDefinition(true, shortopt, longopt, null, valueName, description, {a: String => action(a.toInt) }, true, false)

class DoubleArgOptionDefinition( shortopt: String, longopt: String, valueName: String, description: String, action: Double => Unit )
  extends OptionDefinition(true, shortopt, longopt, null, valueName, description, { a: String => action(a.toDouble) }, true, false)

class BooleanArgOptionDefinition( shortopt: String, longopt: String, valueName: String, description: String, action: Boolean => Unit)
  extends OptionDefinition(true, shortopt, longopt, null, valueName, description, {
  a: String =>
    val boolValue = a.toLowerCase match {
      case "true" => true
      case "false" => false
      case "yes" => true
      case "no" => false
      case "1" => true
      case "0" => false
      case _ =>
        throw new IllegalArgumentException("Expected a string I can interpret as a boolean")
    }
    action(boolValue) }, true, false)

class KeyValueArgOptionDefinition( shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, String) => Unit)
  extends OptionDefinition(true, shortopt, longopt, keyName, valueName, description, {
        a: String =>
          a.indexOf('=') match {
            case -1 => throw new IllegalArgumentException("Expected a key=value pair")
            case n: Int => action(a.dropRight(a.length - n), a.drop(n + 1))
      }
  },false, true)

class KeyIntValueArgOptionDefinition( shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, Int) => Unit)
  extends OptionDefinition(true, shortopt, longopt, keyName, valueName, description, {
      a: String =>
        a.indexOf('=') match {
          case -1 => throw new IllegalArgumentException("Expected a key=value pair")
          case n: Int => action(a.dropRight(a.length - n), a.drop(n + 1).toInt)
        }
    },false, true)

class KeyDoubleValueArgOptionDefinition( shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, Double) => Unit)
  extends OptionDefinition(true, shortopt, longopt, keyName, valueName,
    description, {
      a: String =>
        a.indexOf('=') match {
          case -1 => throw new IllegalArgumentException("Expected a key=value pair")
          case n: Int => action(a.dropRight(a.length - n), a.drop(n + 1).toDouble)
        }
    }, false, true)

class KeyBooleanValueArgOptionDefinition( shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, Boolean) => Unit)
  extends OptionDefinition(true, shortopt, longopt, null, valueName, description, {
      a: String =>
        if (!a.contains("="))
          throw new IllegalArgumentException("Expected a key=value pair")

        val key = a.dropRight(a.length - a.indexOf('='))
        val boolValue = a.drop(a.indexOf('=') + 1).toLowerCase match {
          case "true" => true
          case "false" => false
          case "yes" => true
          case "no" => false
          case "1" => true
          case "0" => false
          case _ =>
            throw new IllegalArgumentException("Expected a string I can interpret as a boolean")
        }
        action(key, boolValue)
    },false, true)

class FlagOptionDefinition( shortopt: String, longopt: String, description: String, action: => Unit)
  extends OptionDefinition(true, shortopt, longopt, null, null, description, { a: String => action}, false, false)

/**
 * OptionParser is instantiated within your object,
 * set up by an (ordered) sequence of invocations of 
 * the various builder methods such as #opt or #arg
 */
trait OptionParser {

  protected val programName: Option[String] = None
  protected val errorOnUnknownArgument: Boolean = true

  private val options = new ListBuffer[OptionDefinition]
  private val arguments = new ListBuffer[Argument]
  private val NL = System.getProperty("line.separator")
  private val TB = "        " //8 whitespace chars
  private val NLTB = NL + TB
  private val NLNL = NL + NL
  private val MAX_TXT_LENGTH = 72 //chars
  private val defaultKeyName = "<key>"
  private val defaultValueName = "<value>"
  private var argList: Option[Argument] = None

  // -------- Defining options ---------------
  def add(option: OptionDefinition) {
    option match {
      case a: Argument =>
        if (a.allowMultiple)
          argList = Some(a)
        else
          arguments += a
      case _ => options += option
    }
  }

  // setup options (which require -char or --string to invoke
  def opt(shortopt: String, longopt: String, description: String, action: String => Unit) {
    add(new ArgOptionDefinition(shortopt, longopt, defaultValueName, description, action))
  }

  def opt(shortopt: String, longopt: String, valueName: String, description: String, action: String => Unit) {
    add(new ArgOptionDefinition(shortopt, longopt, valueName, description, action))
  }

  def flagOpt(shortopt: String, longopt: String, description: String, action: => Unit) {
    add(new FlagOptionDefinition(shortopt, longopt, description, action))
  }

  def intOpt(shortopt: String, longopt: String, description: String, action: Int => Unit) {
    add(new IntArgOptionDefinition(shortopt, longopt, defaultValueName, description, action))
  }

  def intOpt(shortopt: String, longopt: String, valueName: String, description: String, action: Int => Unit) {
    add(new IntArgOptionDefinition(shortopt, longopt, valueName, description, action))
  }

  def doubleOpt(shortopt: String, longopt: String, description: String, action: Double => Unit) {
    add(new DoubleArgOptionDefinition(shortopt, longopt, defaultValueName, description, action))
  }

  def doubleOpt(shortopt: String, longopt: String, valueName: String, description: String, action: Double => Unit) {
    add(new DoubleArgOptionDefinition(shortopt, longopt, valueName, description, action))
  }

  def booleanOpt(shortopt: String, longopt: String, description: String, action: Boolean => Unit) {
    add(new BooleanArgOptionDefinition(shortopt, longopt, defaultValueName, description, action))
  }

  def booleanOpt(shortopt: String, longopt: String, valueName: String, description: String, action: Boolean => Unit) {
    add(new BooleanArgOptionDefinition(shortopt, longopt, valueName, description, action))
  }

  def keyValueOpt(shortopt: String, longopt: String, description: String, action: (String, String) => Unit) {
    add(new KeyValueArgOptionDefinition(shortopt, longopt, defaultKeyName, defaultValueName, description, action))
  }

  def keyValueOpt(shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, String) => Unit) {
    add(new KeyValueArgOptionDefinition(shortopt, longopt, keyName, valueName, description, action))
  }

  def keyIntValueOpt(shortopt: String, longopt: String, description: String, action: (String, Int) => Unit) {
    add(new KeyIntValueArgOptionDefinition(shortopt, longopt, defaultKeyName, defaultValueName, description, action))
  }

  def keyIntValueOpt(shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, Int) => Unit) {
    add(new KeyIntValueArgOptionDefinition(shortopt, longopt, keyName, valueName, description, action))
  }

  def keyDoubleValueOpt(shortopt: String, longopt: String, description: String, action: (String, Double) => Unit) {
    add(new KeyDoubleValueArgOptionDefinition(shortopt, longopt, defaultKeyName, defaultValueName, description, action))
  }

  def keyDoubleValueOpt(shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, Double) => Unit) {
    add(new KeyDoubleValueArgOptionDefinition(shortopt, longopt, keyName, valueName, description, action))
  }

  def keyBooleanValueOpt(shortopt: String, longopt: String, description: String, action: (String, Boolean) => Unit) {
    add(new KeyBooleanValueArgOptionDefinition(shortopt, longopt, defaultKeyName, defaultValueName, description, action))
  }

  def keyBooleanValueOpt(shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, Boolean) => Unit) {
    add(new KeyBooleanValueArgOptionDefinition(shortopt, longopt, keyName, valueName, description, action))
  }

  def help(shortopt: String, longopt: String, description: String = "Show this help message.") {
    add(new FlagOptionDefinition(shortopt, longopt, description, { showUsage(); sys.exit() }))
  }

  def separator(description: String) {
    add(new SeparatorDefinition(description))
  }

  // regular arguments without the - or -- which have a name purely for help
  def arg(name: String, description: String, action: String => Unit) {
    add(new Argument(name, description, false, action))
  }

  // arglist allows multiple arguments
  def arglist(name: String, description: String, action: String => Unit) {
    add(new Argument(name, description, true, action))
  }


  def wrapText(description: String): String = {
    if(description.length < MAX_TXT_LENGTH) description
    else if(description.substring(0,MAX_TXT_LENGTH).contains(NL)){
      val idxNL = description.indexOf(NL)
      description.substring(0, idxNL).trim() +NLTB+wrapText(description.substring(idxNL+1).trim())
    }
    else{
      val idx = math.max(math.max(description.lastIndexOf(" ", MAX_TXT_LENGTH), description.lastIndexOf(TB,MAX_TXT_LENGTH)), description.lastIndexOf("-",MAX_TXT_LENGTH))
      description.substring(0, idx).trim()+NLTB+wrapText(description.substring(idx).trim())
    }

  }

  // -------- Getting usage information ---------------
  def descriptions: Seq[String] = options.map {
    case x if !x.canBeInvoked => wrapText(x.description)
    case x if x.keyValueArgument => "[-" + x.shortopt + ", --" + x.longopt + ":" + x.keyName + "=" + x.valueName + "]" + NLTB + wrapText(x.description)
    case x if x.gobbleNextArgument => "[-" + x.shortopt + ", --" + x.longopt + " " + x.valueName + "]" + NLTB + wrapText(x.description)
    case opt => "[-" + opt.shortopt + ", " + "--" + opt.longopt + "]" + NLTB + wrapText(opt.description)
  } ++= (argList match {
    case Some(x: Argument) => List(x.valueName + NLTB + wrapText(x.description))
    case None => arguments.map(a => a.valueName + NLTB + wrapText(a.description))
  })

  def usage: String = {
    val prorgamText = programName match {
      case Some(x) => x + " "
      case None => ""
    }
    val optionText = if (options.isEmpty) {
      ""
    } else {
      "[options] "
    }
    val argumentList = argumentNames.mkString(" ")
    NL + "Usage: " + prorgamText + optionText + argumentList + NLNL +
      "  " + descriptions.mkString(NL + "  ") + NL
  }

  def showUsage() {
    Console.err.println(usage)
  }


  def argumentNames: Seq[String] = argList match {
    case Some(x: Argument) => List(x.valueName)
    case None => arguments.map(_.valueName)
  }

  def applyArgument(option: OptionDefinition, arg: String): Boolean = {
    try {
      option.action.apply(arg)
      true
    } catch {
      case e: NumberFormatException => System.err.println("Error: " +
        option.shortDescription + " expects a number but was given '" + arg + "'")
      false
      case e: Throwable => System.err.println("Error: " +
        option.shortDescription + " failed when given '" + arg + "'. " + e.getMessage)
      false
    }
  }

  // -------- Parsing ---------------
  def parse(argz: Seq[String]): Boolean = {
    import scala.collection.mutable

    var i = 0
    val requiredArgs = arguments.clone()
    var answer = true
    var argListCount = 0

    // Search for groups of parameters
    val args = new mutable.ArrayBuffer[String]()
    val currGrp = new mutable.StringBuilder()
    var grpMode = false
    for (arg <- argz) {
      (arg.startsWith("{"), arg.endsWith("}")) match {
        case (true, true) if arg.length() > 2 => args += arg.substring(1, arg.length() - 1).trim
        case (true, false)  =>
          grpMode = true
          currGrp.append(" " + arg.substring(1).trim)
        case (false, true) =>
          if (arg.length() > 1) currGrp.append(" " + arg.substring(0, arg.length() - 1))
          args += currGrp.result()
          currGrp.clear()
          grpMode = false
        case (false, false) =>
          if (grpMode) currGrp.append(" "+arg)
          else args += arg
        case _ => // ignore
      }

    }


    while (i < args.length) {
      val arg = args(i)
      val matchingOption = options.find(opt =>
        opt.canBeInvoked &&
          ((!opt.keyValueArgument &&
            (arg == "-" + opt.shortopt || arg == "--" + opt.longopt)) ||
            (opt.keyValueArgument &&
              (arg.startsWith("-" + opt.shortopt + ":") || arg.startsWith("--" + opt.longopt + ":"))))
      )

      matchingOption match {
        case None =>
          if (arg.startsWith("-")) {
            if (errorOnUnknownArgument) {
              System.err.println("Error: Unknown argument '" + arg + "'")
              answer = false
            } else
              System.err.println("Warning: Unknown argument '" + arg + "'")
          } else if (argList.isDefined) {
            argListCount += 1
            if (!applyArgument(argList.get, arg)) {
              answer = false
            }
          } else if (requiredArgs.isEmpty) {
            if (errorOnUnknownArgument) {
              System.err.println("Error: Unknown argument '" + arg + "'")
              answer = false
            } else
              System.err.println("Warning: Unknown argument '" + arg + "'")
          } else {
            val first = requiredArgs.remove(0)
            if (!applyArgument(first, arg)) {
              answer = false
            }
          }

        case Some(option) =>
          val argToPass = if (option.gobbleNextArgument) {
            i += 1
            args(i)
          } else if (option.keyValueArgument && arg.startsWith("-" + option.shortopt + ":")) {
            arg.drop(("-" + option.shortopt + ":").length)
          } else if (option.keyValueArgument && arg.startsWith("--" + option.longopt + ":")) {
            arg.drop(("--" + option.longopt + ":").length)
          } else
            ""

          if (!applyArgument(option, argToPass)) {
            answer = false
          }
      }
      i += 1
    }

    if (!requiredArgs.isEmpty ||
      (argListCount == 0 && argList.isDefined)) {
      System.err.println("Error: missing arguments: " + argumentNames.mkString(", "))
      answer = false
    }
    if (!answer)
      showUsage()
    answer
  }

  def exitMissingArgument(shortArg: String, longArg: String): scala.Nothing = {
    sys.error("Missing argument: -" + shortArg + " | --" + longArg)
    println(usage)
    sys.exit(10)
  }
}
