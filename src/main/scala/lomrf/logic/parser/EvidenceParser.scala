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

package lomrf.logic.parser

import com.typesafe.scalalogging.LazyLogging
import lomrf.logic._
import lomrf.util.logging.Implicits._
import scala.language.implicitConversions

/**
  * Parser for evidence expressions.
  *
  * @see [[lomrf.logic.EvidenceExpression]]
  */
final class EvidenceParser extends CommonsMLNParser with LazyLogging {

  private implicit def list2Vector[T](src: List[T]): Vector[T] = src.toVector

  /**
    * Parses evidence expressions.
    *
    * @return a list of evidence expressions
    */
  def evidence: Parser[List[EvidenceExpression]] = rep(evidenceExpression)

  /**
    * An evidence expression can be a function mapping or a ground evidence
    * atom. The ground evidence atoms can be positive, negative, unknown or
    * probabilistic.
    *
    * @see [[lomrf.logic.FunctionMapping]]
    * @see [[lomrf.logic.EvidenceAtom]]
    *
    * @return an evidence expression
    */
  def evidenceExpression: Parser[EvidenceExpression] =
    functionMapping | positiveAtom | positiveAtom0 | negativeAtom | negativeAtom0 | unkAtom | unkAtom0

  /**
    * Parses a constant. A constant is always an upper case literal.
    *
    * @return a constant
    */
  def constant: Parser[Constant] = upperCaseID ^^ { c => Constant(c) }

  /**
    * Parses comma separated constants that represent function or atom arguments.
    *
    * @return a list of constants
    */
  def constantArguments: Parser[List[Constant]] = repsep(constant, ",") ^^ (cons => cons)

  /**
    * Parses positive evidence atoms having constant arguments.
    *
    * @example {{{
    *           Friends(Anna,Bob)
    *          }}}
    *
    * @note In case a probability is given the positive evidence atom
    *       becomes a probabilistic evidence atom.
    *
    * @return a positive evidence atom
    */
  def positiveAtom: Parser[EvidenceAtom] = upperCaseID ~ "(" ~ constantArguments ~ ")" ~ opt(numDouble) ^^ {
    case predicateName ~ "(" ~ args ~ ")" ~ None =>
      EvidenceAtom.asTrue(predicateName, args)

    case atomExpression @ (predicateName ~ "(" ~ args ~ ")" ~ Some(probability)) =>
      mkProbabilisticAtom(
        atomExpression.toString,
        predicateName,
        args,
        probability.toDouble,
        isPositive = true)
  }

  /**
    * Parses positive evidence atoms having no arguments.
    *
    * @example {{{
    *           UnaryPredicate
    *          }}}
    *
    * @note In case a probability is given the positive evidence atom
    *       becomes a probabilistic evidence atom.
    *
    * @return a positive evidence atom
    */
  def positiveAtom0: Parser[EvidenceAtom] = upperCaseID ~ opt(numDouble) ^^ {
    case predicateName ~ None =>
      EvidenceAtom.asTrue(predicateName, Vector.empty[Constant])

    case atomExpression @ (predicateName ~ Some(probability)) =>
      mkProbabilisticAtom(
        atomExpression.toString,
        predicateName,
        List.empty[Constant],
        probability.toDouble,
        isPositive = true)
  }

  /**
    * Parses negative evidence atoms having constant arguments.
    *
    * @example {{{
    *           !Friends(Anna,Bob)
    *          }}}
    *
    * @note In case a probability is given the negative evidence atom
    *       becomes a probabilistic evidence atom.
    *
    * @return a negative evidence atom
    */
  def negativeAtom: Parser[EvidenceAtom] = "!" ~ upperCaseID ~ "(" ~ constantArguments ~ ")" ~ opt(numDouble) ^^ {
    case "!" ~ predicateName ~ "(" ~ args ~ ")" ~ None =>
      EvidenceAtom.asFalse(predicateName, args)

    case atomExpression @ ("!" ~ predicateName ~ "(" ~ args ~ ")" ~ Some(probability)) =>
      mkProbabilisticAtom(
        atomExpression.toString,
        predicateName,
        args,
        probability.toDouble,
        isPositive = false)
  }

  /**
    * Parses negative evidence atoms having no arguments.
    *
    * @example {{{
    *           !UnaryPredicate
    *          }}}
    *
    * @note In case a probability is given the negative evidence atom
    *       becomes a probabilistic evidence atom.
    *
    * @return a negative evidence atom
    */
  def negativeAtom0: Parser[EvidenceAtom] = "!" ~ upperCaseID ~ opt(numDouble) ^^ {
    case "!" ~ predicateName ~ None =>
      EvidenceAtom.asFalse(predicateName, Vector.empty[Constant])

    case atomExpression @ ("!" ~ predicateName ~ Some(probability)) =>
      mkProbabilisticAtom(
        atomExpression.toString,
        predicateName,
        List.empty[Constant],
        probability.toDouble,
        isPositive = false)
  }

  /**
    * Parses unknown evidence atoms having constant arguments.
    *
    * @example {{{
    *           ? Friends(Anna,Bob)
    *          }}}
    *
    * @note Unknown evidence atoms cannot be probabilistic.
    *
    * @return an unknown evidence atom
    */
  def unkAtom: Parser[EvidenceAtom] = "?" ~ upperCaseID ~ "(" ~ constantArguments ~ ")" ^^ {
    case "?" ~ predicateName ~ "(" ~ args ~ ")" => EvidenceAtom.asUnknown(predicateName, args)
  }

  /**
    * Parses unknown evidence atoms having no arguments.
    *
    * @example {{{
    *           ? UnaryPredicate
    *          }}}
    *
    * @note Unknown evidence atoms cannot be probabilistic.
    *
    * @return an unknown evidence atom
    */
  def unkAtom0: Parser[EvidenceAtom] = "?" ~ upperCaseID ^^ {
    case "?" ~ predicateName => EvidenceAtom.asUnknown(predicateName, Vector.empty[Constant])
  }

  /**
    * Parses function mappings having constant arguments
    * and a constant return value for the constant arguments.
    *
    * @example {{{
    *           Active_ID0 = active(ID0)
    *          }}}
    *
    * @return a function mapping
    */
  def functionMapping: Parser[FunctionMapping] =
    upperCaseID ~ "=" ~ lowerCaseID ~ "(" ~ repsep(upperCaseID, ",") ~ ")" ^^ {
      case retValue ~ "=" ~ functionSymbol ~ "(" ~ values ~ ")" =>
        new FunctionMapping(retValue, functionSymbol, values)
    }

  /**
    * Creates a probabilistic atom.
    *
    * @note Probabilistic evidence atoms are not supported yet.
    *
    * @param atomString a string representation for the atom
    * @param predicateName the predicate name
    * @param args a list of constants
    * @param probability a probability for the atom
    * @param isPositive defines if the atom is positive
    *
    * @return a probabilistic evidence atom
    */
  private def mkProbabilisticAtom(
      atomString: String,
      predicateName: String,
      args: List[Constant],
      probability: Double,
      isPositive: Boolean): EvidenceAtom = {

    require(
      probability >= 0.0 && probability <= 1.0,
      s"The probability of evidence atom '$atomString' must be between 0.0 and 1.0.")

    probability match {
      case 1.0 => EvidenceAtom.asTrue(predicateName, args)
      case 0.0 => EvidenceAtom.asFalse(predicateName, args)
      case _   => throw new UnsupportedOperationException("Probabilistic atoms are not supported yet.")
    }
  }

  //--------------------------------------------------------------------------------------------------------------------
  //--- Public API defining utility functions for parsing
  //--------------------------------------------------------------------------------------------------------------------

  def parseFunctionMapping(src: String): FunctionMapping = parse(functionMapping, src) match {
    case Success(result, _) => result
    case x                  => logger.fatal(s"Can't parse the following expression: $x")
  }

  def parseEvidenceAtom(src: String): EvidenceAtom =
    parse(positiveAtom | positiveAtom0 | negativeAtom | negativeAtom0 | unkAtom | unkAtom0, src) match {
      case Success(result, _) => result
      case x                  => logger.fatal(s"Can't parse the following expression: $x")
    }

  def parseEvidenceExpression(src: String): EvidenceExpression = parse(evidenceExpression, src) match {
    case Success(result, _) => result
    case x                  => logger.fatal(s"Can't parse the following expression: $x")
  }

  def parseEvidence(src: String): List[EvidenceExpression] = parse(evidence, src) match {
    case Success(result, _) => result
    case x                  => logger.fatal(s"Can't parse the following expression: $x")
  }
}
