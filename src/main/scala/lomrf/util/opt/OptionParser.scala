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

/**
  * The source code below is an extended/modified version of the code in https://gist.github.com/246481
  */
package lomrf.util.opt

import scala.collection.mutable.ListBuffer
import Console.err

import scala.util.control.NonFatal
import scala.util.{ Failure, Try }

/**
  * This is the base class for options.
  */
case class OptionDefinition(canBeInvoked: Boolean, shortopt: String, longopt: String, keyName: String, valueName: String,
    description: String, action: String => Unit, gobbleNextArgument: Boolean, keyValueArgument: Boolean) {
  def shortDescription: String = "option " + longopt
}

// ----- Some standard option types ---------
class SeparatorDefinition(description: String)
  extends OptionDefinition(false, null, null, null, null, description, { a: String => {} }, false, false)

class Argument(name: String, description: String, val allowMultiple: Boolean, action: String => Unit)
  extends OptionDefinition(false, null, name, null, name, description, action, false, false) {
  override def shortDescription: String = "argument " + name
}

class ArgOptionDefinition(shortopt: String, longopt: String, valueName: String, description: String, action: String => Unit)
  extends OptionDefinition(true, shortopt, longopt, null, valueName, description, action, true, false)

class IntArgOptionDefinition(shortopt: String, longopt: String, valueName: String, description: String, action: Int => Unit)
  extends OptionDefinition(true, shortopt, longopt, null, valueName, description, { a: String => action(a.toInt) }, true, false)

class DoubleArgOptionDefinition(shortopt: String, longopt: String, valueName: String, description: String, action: Double => Unit)
  extends OptionDefinition(true, shortopt, longopt, null, valueName, description, { a: String => action(a.toDouble) }, true, false)

class BooleanArgOptionDefinition(shortopt: String, longopt: String, valueName: String, description: String, action: Boolean => Unit)
  extends OptionDefinition(
    canBeInvoked       = true,
    shortopt           = shortopt,
    longopt            = longopt,
    keyName            = null,
    valueName          = valueName,
    description        = description, action = {
    a: String =>
      val boolValue = a.toLowerCase.trim match {
        case "true"  => true
        case "false" => false
        case "yes"   => true
        case "no"    => false
        case "1"     => true
        case "0"     => false
        case _ =>
          throw new IllegalArgumentException(
            "Expected a string that can be interpreted as a boolean, " +
              "e.g., (true/false or yes/no or 0/1)")
      }

      action(boolValue)
  },
    gobbleNextArgument = true,
    keyValueArgument   = false)

class KeyValueArgOptionDefinition(shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, String) => Unit)
  extends OptionDefinition(true, shortopt, longopt, keyName, valueName, description, {
    a: String =>
      a.indexOf('=') match {
        case -1     => throw new IllegalArgumentException("Expected a key=value pair")
        case n: Int => action(a.dropRight(a.length - n), a.drop(n + 1))
      }
  }, false, true)

class KeyIntValueArgOptionDefinition(shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, Int) => Unit)
  extends OptionDefinition(true, shortopt, longopt, keyName, valueName, description, {
    a: String =>
      a.indexOf('=') match {
        case -1     => throw new IllegalArgumentException("Expected a key=value pair")
        case n: Int => action(a.dropRight(a.length - n), a.drop(n + 1).toInt)
      }
  }, false, true)

class KeyDoubleValueArgOptionDefinition(shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, Double) => Unit)
  extends OptionDefinition(true, shortopt, longopt, keyName, valueName,
                                 description, {
    a: String =>
      a.indexOf('=') match {
        case -1     => throw new IllegalArgumentException("Expected a key=value pair")
        case n: Int => action(a.dropRight(a.length - n), a.drop(n + 1).toDouble)
      }
  }, false, true)

class KeyBooleanValueArgOptionDefinition(shortopt: String, longopt: String, keyName: String, valueName: String, description: String, action: (String, Boolean) => Unit)
  extends OptionDefinition(true, shortopt, longopt, null, valueName, description, {
    a: String =>
      if (!a.contains("="))
        throw new IllegalArgumentException("Expected a key=value pair")

      val key = a.dropRight(a.length - a.indexOf('='))
      val boolValue = a.drop(a.indexOf('=') + 1).toLowerCase match {
        case "true"  => true
        case "false" => false
        case "yes"   => true
        case "no"    => false
        case "1"     => true
        case "0"     => false
        case _ =>
          throw new IllegalArgumentException("Expected a string I can interpret as a boolean")
      }
      action(key, boolValue)
  }, false, true)

class FlagOptionDefinition(shortopt: String, longopt: String, description: String, action: => Unit)
  extends OptionDefinition(true, shortopt, longopt, null, null, description, { _: String => action }, false, false)

/**
  * OptionParser is instantiated within your object,
  * set up by an (ordered) sequence of invocations of
  * the various builder methods such as #opt or #arg
  */
trait OptionParser {

  protected val programName: Option[String] = None
  protected val errorOnUnknownArgument: Boolean = true

  private final val MAX_TXT_LENGTH = 72 //chars

  private final val wrap = (txt: String) => wrapText(txt, MAX_TXT_LENGTH)

  private val options = new ListBuffer[OptionDefinition]
  private val arguments = new ListBuffer[Argument]
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

  // -------- Getting usage information ---------------
  def descriptions: Seq[String] = options.map {
    case x if !x.canBeInvoked      => wrap(x.description)
    case x if x.keyValueArgument   => "[-" + x.shortopt + ", --" + x.longopt + ":" + x.keyName + "=" + x.valueName + "]" + NLTB + wrap(x.description)
    case x if x.gobbleNextArgument => "[-" + x.shortopt + ", --" + x.longopt + " " + x.valueName + "]" + NLTB + wrap(x.description)
    case opt                       => "[-" + opt.shortopt + ", " + "--" + opt.longopt + "]" + NLTB + wrap(opt.description)
  } ++= (argList match {
    case Some(x: Argument) => List(x.valueName + NLTB + wrap(x.description))
    case None              => arguments.map(a => a.valueName + NLTB + wrap(a.description))
  })

  def usage: String = {
    val programText = programName match {
      case Some(x) => x + " "
      case None    => ""
    }
    val optionText = if (options.isEmpty) {
      ""
    } else {
      "[options] "
    }
    val argumentList = argumentNames.mkString(" ")
    NL + "Usage: " + programText + optionText + argumentList + NLNL +
      "  " + descriptions.mkString(NL + "  ") + NL
  }

  def showUsage() {
    Console.err.println(usage)
  }

  def argumentNames: Seq[String] = argList match {
    case Some(x: Argument) => List(x.valueName)
    case None              => arguments.map(_.valueName)
  }

  def applyArgument(option: OptionDefinition, arg: String): Boolean = {

    Try(option.action(arg)) match {
      case Failure(ex) =>
        ex match {
          case _: NumberFormatException =>
            err.println(s"Error: ${option.shortDescription} expects a number but was given '$arg'")

          case NonFatal(_) =>
            err.println(s"Error: ${option.shortDescription} failed when given '$arg'. Error message: ${ex.getMessage}")
        }
        false

      case _ => true
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

        case (true, false) =>
          grpMode = true
          currGrp.append(" " + arg.substring(1).trim)

        case (false, true) =>
          if (arg.length() > 1) currGrp.append(" " + arg.substring(0, arg.length() - 1))
          args += currGrp.result()
          currGrp.clear()
          grpMode = false

        case (false, false) =>
          if (grpMode) currGrp.append(" " + arg)
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
              (arg.startsWith("-" + opt.shortopt + ":") || arg.startsWith("--" + opt.longopt + ":")))))

      matchingOption match {
        case None =>
          if (arg.startsWith("-")) {
            if (errorOnUnknownArgument) {
              err.println("Error: Unknown argument '" + arg + "'")
              answer = false
            } else {
              err.println("Warning: Unknown argument '" + arg + "'")
            }
          } else if (argList.isDefined) {
            argListCount += 1
            if (!applyArgument(argList.get, arg)) {
              answer = false
            }
          } else if (requiredArgs.isEmpty) {
            if (errorOnUnknownArgument) {
              err.println("Error: Unknown argument '" + arg + "'")
              answer = false
            } else {
              err.println("Warning: Unknown argument '" + arg + "'")
            }
          } else {
            val first = requiredArgs.remove(0)
            if (!applyArgument(first, arg)) {
              answer = false
            }
          }

        case Some(option) =>
          val argToPass = {
            if (option.gobbleNextArgument) {
              i += 1
              args(i)
            } else if (option.keyValueArgument && arg.startsWith("-" + option.shortopt + ":")) {
              arg.drop(("-" + option.shortopt + ":").length)
            } else if (option.keyValueArgument && arg.startsWith("--" + option.longopt + ":")) {
              arg.drop(("--" + option.longopt + ":").length)
            } else {
              ""
            }
          }

          if (!applyArgument(option, argToPass)) {
            answer = false
          }
      }
      i += 1
    }

    if (requiredArgs.nonEmpty ||
      (argListCount == 0 && argList.isDefined)) {
      err.println("Error: missing arguments: " + argumentNames.mkString(", "))
      answer = false
    }

    if (!answer) showUsage()

    answer
  }

  def exitMissingArgument(shortArg: String, longArg: String): scala.Nothing = {
    err.println("Missing argument: -" + shortArg + " | --" + longArg)
    err.println(usage)
    sys.exit(10)
  }
}
