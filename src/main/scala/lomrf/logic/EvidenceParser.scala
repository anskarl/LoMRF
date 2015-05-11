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

package lomrf.logic

/**
 *
 */

class EvidenceParser extends CommonsMLNParser {

  private implicit def list2Vector[T](src: List[T]): Vector[T] = src.toVector

  /**
   * A grounded term is a constant
   */
  def groundedTerm: Parser[Term] = constant

  /**
   * A constant always starts with an upper case letter.
   */
  def constant: Parser[Constant] = upperCaseID ^^ {
    c => Constant(c)
  }

  //Evidence - KB database

  def evidenceExpression: Parser[EvidenceExpression] = functionMapping | positiveAtom | positiveAtom0 | negativeAtom | negativeAtom0 | unkAtom | unkAtom0

  def evidence: Parser[List[EvidenceExpression]] = rep(evidenceExpression)

  def constantArguments: Parser[List[Constant]] = repsep(constant, ",") ^^ {
    case cons => cons
  }

  def positiveAtom: Parser[EvidenceAtom] = upperCaseID ~ "(" ~ constantArguments ~ ")" ~ opt(numDouble) ^^ {
    case predName ~ "(" ~ args ~ ")" ~ None => EvidenceAtom(predName, args, isPositive = true)
    case atomExpression@(predName ~ "(" ~ args ~ ")" ~ Some(probability)) =>
      mkProbabilisticAtom(atomExpression.toString(), predName, args, probability.toDouble, isPositive = true)
  }

  def positiveAtom0: Parser[EvidenceAtom] = upperCaseID ~ opt(numDouble) ^^ {
    case predName ~ None => EvidenceAtom(predName, Vector.empty[Constant], isPositive = true)
    case atomExpression@(predName ~ Some(probability)) =>
      mkProbabilisticAtom(atomExpression.toString(), predName, List.empty[Constant], probability.toDouble, isPositive = true)
  }

  def negativeAtom: Parser[EvidenceAtom] = "!" ~ upperCaseID ~ "(" ~ constantArguments ~ ")" ~ opt(numDouble) ^^ {
    case "!" ~ predName ~ "(" ~ args ~ ")" ~ None => EvidenceAtom(predName, args, isPositive = false)
    case atomExpression@("!" ~ predName ~ "(" ~ args ~ ")" ~ Some(probability)) =>
      mkProbabilisticAtom(atomExpression.toString(), predName, args, probability.toDouble, isPositive = false)
  }

  def negativeAtom0: Parser[EvidenceAtom] = "!" ~ upperCaseID ~ opt(numDouble) ^^ {
    case "!" ~ predName ~ None => EvidenceAtom(predName, Vector.empty[Constant], isPositive = false)
    case atomExpression@("!" ~ predName ~ Some(probability)) =>
      mkProbabilisticAtom(atomExpression.toString(), predName, List.empty[Constant], probability.toDouble, isPositive = false)
  }

  def unkAtom: Parser[EvidenceAtom] = "?" ~ upperCaseID ~ "(" ~ constantArguments ~ ")" ^^ {
    case "?" ~ predName ~ "(" ~ args ~ ")" => EvidenceAtom(predName, args, UNKNOWN)
  }

  def unkAtom0: Parser[EvidenceAtom] = "?" ~ upperCaseID  ^^ {
    case "?" ~ predName  => EvidenceAtom(predName, Vector.empty[Constant], UNKNOWN)
  }

  /*def functionMapping: Parser[FunctionMapping] = upperCaseID ~ "=" ~ lowerCaseID ~ "(" ~ repsep(upperCaseID, ",") ~ ")" ^^ {
    case retValue ~ "=" ~ functionSymbol ~ "(" ~ values ~ ")" => new FunctionMapping(retValue, functionSymbol, values)
  }*/


  def functionMapping: Parser[FunctionMapping] = upperCaseID ~ "=" ~ lowerCaseID ~ "(" ~ constantArguments ~ ")" ^^ {
    case retValue ~ "=" ~ functionSymbol ~ "(" ~ values ~ ")" => new FunctionMapping(retValue, functionSymbol, values)
  }

  private def mkProbabilisticAtom(atomString: String, predName: String, args: List[Constant], probability: Double, isPositive: Boolean): EvidenceAtom = {
    require(probability >= 0.0 && probability <= 1.0, "The probability of evidence atom '" + atomString + "' must be between 0.0 and 1.0.")

    probability match {
      case 1.0 => EvidenceAtom(predName, args, isPositive = true)
      case 0.0 => EvidenceAtom(predName, args, isPositive = false)
      case _ => EvidenceAtom(predName, args, if (isPositive) probability else 1.0 - probability)
    }
  }


}
