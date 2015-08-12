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
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */

package lomrf.logic


/**
 * This class represents a ground atom (all its terms are constants) and its truth value may be known (given by the input evidence).
 * Consider, for example, the following atoms:
 *
 * <ul>
 * <li> '!Friends(A,B)' indicates that A and B are not friends, that is Friends(A,B) = False </li>
 * <li> 'Friends(D,F)'  indicates that D and F are friends, that is Friends(D,F) = True </li>
 * <li> 'Friends(D,F) 0.76' indicates that D and F are friends with probability 0.76, that is P(Friends(D,F) = True) = 0.76  </li>
 * <li> '!Friends(D,F) 0.76' indicates that D and F are friends with probability 0.76, that is P(Friends(D,F) = True) = 0.24  </li>
 * </ul>
 *
 * An evidence atom is an atomic formula with a known truth value.
 */
class EvidenceAtom(override val symbol: String, override val terms: Vector[Constant],
                   val state: TriState, val probability: Double = Double.NaN) extends AtomicFormula(symbol, terms) with EvidenceExpression {


  override lazy val variables: Set[Variable] = Set.empty[Variable]

  override lazy val constants: Set[Constant] = terms.toSet[Constant]

  override lazy val functions: Set[TermFunction] = Set.empty[TermFunction]

  override def isGround = true


  override def toText: String = {
    lazy val sentence = s"$symbol(${terms.map(_.toText).mkString(",")})"

    state match {
      case TRUE => sentence
      case FALSE => s"!$sentence"
      case UNKNOWN => s"$sentence $probability"
    }
  }

}

object EvidenceAtom {

  def asTrue(predicate: String, args: Vector[Constant]): EvidenceAtom = new EvidenceAtom(predicate, args, TRUE)

  def asFalse(predicate: String, args: Vector[Constant]): EvidenceAtom = new EvidenceAtom(predicate, args, FALSE)

  def asUnknown(predicate: String, args: Vector[Constant]): EvidenceAtom = new EvidenceAtom(predicate, args, UNKNOWN)

  def apply(predicate: String, args: Vector[Constant], state: TriState): EvidenceAtom = new EvidenceAtom(predicate, args, state)

  def apply(predicate: String, args: Vector[Constant], isPositive: Boolean): EvidenceAtom = apply(predicate, args, if (isPositive) TRUE else FALSE)

  def apply(predicate: String, args: Vector[Constant], probability: Double): EvidenceAtom = {
    require(probability <= 1.0 && probability >= 0, "The specified probability value is not in [0, 1].")

    if (probability == 0.0) apply(predicate, args, FALSE)
    else if (probability == 1.0) apply(predicate, args, TRUE)
    else new EvidenceAtom(predicate, args, UNKNOWN, probability)
  }

  def unapply(obj: EvidenceAtom): Option[(String, Vector[Constant], TriState, Double)] = {
    if (obj != null) Some((obj.symbol, obj.terms, obj.state, obj.probability)) else None
  }
}

class FunctionMapping(val retValue: String, val functionSymbol: String, val values: Vector[String]) extends EvidenceExpression {

  lazy val signature = AtomSignature(functionSymbol, values.size)

  override def toString = s"$retValue = $functionSymbol(${values.mkString(",")})"

  def toEvidenceAtom: EvidenceAtom ={
    val symbol = lomrf.AUX_PRED_PREFIX + functionSymbol
    val terms = values.+:(retValue).map(Constant)
    EvidenceAtom.asTrue(symbol, terms)
  }

}

object FunctionMapping {
  def apply(retValue: String, functionSymbol: String, values: Vector[Constant]) = new FunctionMapping(retValue, functionSymbol, values.map(_.toString))
}