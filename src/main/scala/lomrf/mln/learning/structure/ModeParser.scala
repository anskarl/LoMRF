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

package lomrf.mln.learning.structure

import java.io.{ BufferedReader, File, FileReader }

import lomrf.util.logging.Implicits._
import com.typesafe.scalalogging.LazyLogging
import lomrf.logic.AtomSignature
import lomrf.mln.model.ModeDeclarations
import scala.language.postfixOps

/**
  * Parser for mode declarations. Mode declarations can be either function modes declared
  * using the prefix modeF or predicate modes declared using the prefix modeP. The parameters
  * of each mode are defined inside parentheses as follows:
  *
  * {{{
  *   modeP(recall, PredicateSymbol(#placeMarker, ...) body~/> AtomSignature/arity, ...)
  *   modeF(recall, functionSymbol(#placeMarker, ...) body~/> AtomSignature/arity, ...)
  * }}}
  *
  * The recall number limits the appearances of the predicate or function. For unlimited appearances
  * use the symbol * in the recall position. Place markers can be either + (input), - (output) or
  * . (ignore) and the prefix symbol # is optional and denotes that the specific place marker is constant.
  * Finally the body~/> notation following the predicate or function symbol definition is optional and
  * defines a set of atom signatures that cannot appear in the body of a clause that has this specific
  * predicate or function in the head.
  *
  * <p>
  *
  * Example:
  * {{{
  *   modeP(*, Q(+, +))
  *   modeF(2, foo(-, -, +))
  *   modeP(1, R(-, +) body~/> foo)
  * }}}
  */
class ModeParser extends CommonModeParser with LazyLogging {

  private def mode: Parser[(AtomSignature, ModeDeclaration)] = modePredicate | modeFunction

  private def modeDeclarations: Parser[ModeDeclarations] = rep(mode) ^^ (Map() ++ _)

  private def modePredicate =
    "modeP" ~ "(" ~ recall ~ "," ~ predicate ~ "(" ~ repsep(placeMarker, ",") ~ ")" ~
      (("body~/>" ~> repsep(predicateSignature | functionSignature, ","))?) ~ ")" ^^ {

        case "modeP" ~ "(" ~ "*" ~ "," ~ predicateSymbol ~ "(" ~ values ~ ")" ~ atoms ~ ")" => (
          AtomSignature(predicateSymbol, values.length),
          ModeDeclaration(
            placeMarkers           = values.map(symbol => PlaceMarker(symbol.contains("+"), symbol.contains("-"), symbol.contains("#"))).toVector,
            incompatibleSignatures = atoms.getOrElse(Set.empty[AtomSignature]).toSet))

        case "modeP" ~ "(" ~ limit ~ "," ~ predicateSymbol ~ "(" ~ values ~ ")" ~ atoms ~ ")" => (
          AtomSignature(predicateSymbol, values.length),
          ModeDeclaration(
            limit.toInt,
            values.map(symbol => PlaceMarker(symbol.contains("+"), symbol.contains("-"), symbol.contains("#"))).toVector,
            atoms.getOrElse(Set.empty[AtomSignature]).toSet))
      }

  private def modeFunction =
    "modeF" ~ "(" ~ recall ~ "," ~ function ~ "(" ~ repsep(placeMarker, ",") ~ ")" ~
      (("body~/>" ~> repsep(predicateSignature | functionSignature, ","))?) ~ ")" ^^ {

        // Function return value is always an input place marker

        case "modeF" ~ "(" ~ "*" ~ "," ~ functionSymbol ~ "(" ~ values ~ ")" ~ atoms ~ ")" => (
          AtomSignature(lomrf.AUX_PRED_PREFIX + functionSymbol.trim, values.length + 1),
          ModeDeclaration(
            placeMarkers           = (PlaceMarker.input :: values.map(symbol => PlaceMarker(symbol.contains("+"), symbol.contains("-"), symbol.contains("#")))).toVector,
            incompatibleSignatures = atoms.getOrElse(Set.empty[AtomSignature]).toSet))

        case "modeF" ~ "(" ~ limit ~ "," ~ functionSymbol ~ "(" ~ values ~ ")" ~ atoms ~ ")" => (
          AtomSignature(lomrf.AUX_PRED_PREFIX + functionSymbol.trim, values.length + 1),
          ModeDeclaration(
            limit.toInt,
            (PlaceMarker.input :: values.map(symbol => PlaceMarker(symbol.contains("+"), symbol.contains("-"), symbol.contains("#")))).toVector,
            atoms.getOrElse(Set.empty[AtomSignature]).toSet))
      }

  private def functionSignature = function ~ "/" ~ arity ^^ {
    case functionSymbol ~ "/" ~ functionArity =>
      AtomSignature(lomrf.AUX_PRED_PREFIX + functionSymbol.trim, functionArity.toInt + 1)
  }

  private def predicateSignature = predicate ~ "/" ~ arity ^^ {
    case predicateSymbol ~ "/" ~ predicateArity =>
      AtomSignature(predicateSymbol.trim, predicateArity.toInt)
  }

  /**
    * Parses an individual mode declaration from text
    *
    * @param src string representation of the mode declaration
    * @return the resulting atom signature along the mode declaration
    */
  private def parseMode(src: String): (AtomSignature, ModeDeclaration) = parse(mode, src) match {
    case Success(expr: (AtomSignature, ModeDeclaration), _) => expr
    case x => logger.fatal("Can't parse the following expression: " + x)
  }
}

/**
  * Mode parser object used to parse mode declarations from a given source. The source can
  * be a simple string, a file or an iterable of strings.
  */
object ModeParser extends LazyLogging {

  private lazy val parser = new ModeParser

  /**
    * Parses an individual mode declaration from a given string.
    *
    * @param source string representation of the mode declaration
    *
    * @return a tuple of an [[lomrf.logic.AtomSignature]] along a [[lomrf.mln.learning.structure.ModeDeclaration]]
    */
  def parseFrom(source: String): (AtomSignature, ModeDeclaration) = parser.parseMode(source)

  /**
    * Parses a set of mode declarations from a given iterable of strings.
    *
    * @param sources an iterable of mode declaration strings
    *
    * @return a map from [[lomrf.logic.AtomSignature]] to [[lomrf.mln.learning.structure.ModeDeclaration]]
    */
  def parseFrom(sources: Iterable[String]): ModeDeclarations = sources.map(parseFrom).toMap

  /**
    * Parses a set of mode declarations from a given file.
    *
    * @param modesFile a [[java.io.File]] object
    *
    * @return a map from [[lomrf.logic.AtomSignature]] to [[lomrf.mln.learning.structure.ModeDeclaration]]
    */
  def parseFrom(modesFile: File): ModeDeclarations = {

    val fileReader = new BufferedReader(new FileReader(modesFile))

    parser.parseAll(parser.modeDeclarations, fileReader) match {
      case parser.Success(expr: ModeDeclarations, _) => expr
      case x                                         => logger.fatal("Can't parse the following expression: " + x + " in file: " + modesFile.getPath)
    }
  }
}

/**
  * Definition of a mode declaration.
  *
  * @param recall limits the number of appearances in a clause
  * @param placeMarkers vector of modes for each variable in the predicate or function
  * @param incompatibleSignatures a set of incompatible atom signatures
  */
final case class ModeDeclaration(recall: Int = Int.MaxValue, placeMarkers: Vector[PlaceMarker], incompatibleSignatures: Set[AtomSignature]) {
  override def toString =
    "mode(" + (if (recall == Int.MaxValue) "*" else recall) + "," + placeMarkers.mkString(",") + ", " + incompatibleSignatures.toString + ")"
}

/**
  * Defines the place marker of a specific variable.
  *
  * @param input is input variable
  * @param output is output variable
  * @param constant the argument would remain constant
  */
final case class PlaceMarker(input: Boolean, output: Boolean, constant: Boolean) {

  def isInputOrOutput: Boolean = input || output

  def isInputOnly: Boolean = input && !output

  def isOutputOnly: Boolean = !input && output

  def isConstant: Boolean = constant

  override def toString =
    if (input && output && constant) "#+-"
    else if (!input && !output && constant) "#."
    else if (input && output) "+-"
    else if (input && constant) "#+"
    else if (output && constant) "#-"
    else if (input) "+"
    else if (output) "-"
    else "."
}

/**
  * Object for [[lomrf.mln.learning.structure.PlaceMarker]]
  */
object PlaceMarker {

  /**
    * An input place marker
    */
  val input = new PlaceMarker(true, false, false)

  /**
    * An input and constant place marker
    */
  val inputConstant = new PlaceMarker(true, false, true)

  /**
    * An output place marker
    */
  val output = new PlaceMarker(false, true, false)

  /**
    * An output and constant place marker
    */
  val outputConstant = new PlaceMarker(false, true, true)

  /**
    * An ignore place marker
    */
  val ignore = new PlaceMarker(false, false, false)

  /**
    * An ignore and constant place marker
    */
  val ignoreConstant = new PlaceMarker(false, false, true)
}
