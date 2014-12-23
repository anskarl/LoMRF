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

package lomrf.mln.grounding

import lomrf.logic.{AtomSignature, Literal}
import lomrf.mln.model.MLN
import lomrf.util.AtomEvidenceDB

/**
 *
 *
 * <p> To improve the grounding speed, we change the order of clause literals according to their type
 * (i.e. dynamic or regular predicates) and a score function.
 * </p>
 *
 * <ul>
 * <li> When both literals contain dynamic sentences (e.q. equals, lessThan, etc.), then
 * the literal with the lowest number of Variables is placed first</li>
 * <li> When only one literal contains a dynamic sentence, then there are two sub-cases:
 * (1) if the other literal contains a sentence with unknown groundings, then the dynamic one
 * is placed first. (2) Otherwise, the literal with the lowest number of Variables is placed first.</li>
 * <li>Finally, when both literals are regular (i.e. not dynamic), then the literal with the
 * lowest score is placed first:
 * <br/>
 * '''score = (number of unsatisfied - number of unknown)/(number of all groundings)'''
 * <br/>
 * In other words, this score value represents the fraction of tuples (i.e. constants replacing
 * variables in the clause)  that will remain after the literal is grounded. This heuristic score function
 * is based in the following paper:
 * <br/>
 * <br/>
 * ''Shavlik, J. and Natarajan, S. Speeding Up Inference in Markov Logic Networks by pre-processing to
 * Reduce the Size of the Resulting Grounded Network. In Proceedings of the 21th International
 * Joint Conference on Artificial Intelligence (IJCAI), 2009.''
 * </li>
 * </ul>
 *
 *
 * @author Anastasios Skarlatidis
 */
class ClauseLiteralsOrdering(atomStateDB: Map[AtomSignature, AtomEvidenceDB]) extends Ordering[Literal] {

  def compare(x: Literal, y: Literal) = {
    val xDB = atomStateDB.getOrElse(x.sentence.signature, null)
    val yDB = atomStateDB.getOrElse(y.sentence.signature, null)

    val scoreX =
      if (x.sentence.isDynamic) Double.NaN
      else {
        val satX = if (x.isNegative) xDB.numberOfFalse else xDB.numberOfTrue
        val unsatX = xDB.length - satX
        (unsatX + xDB.numberOfUnknown) / xDB.length.toDouble
      }

    val scoreY =
      if (y.sentence.isDynamic) Double.NaN
      else {
        val satY = if (y.isNegative) yDB.numberOfFalse else yDB.numberOfTrue
        val unsatY = yDB.length - satY
        (unsatY + yDB.numberOfUnknown) / yDB.length.toDouble
      }

    (scoreX, scoreY) match {
      case (Double.NaN, Double.NaN) =>
        val nVarX = x.sentence.variables.size
        val nVarY = y.sentence.variables.size
        nVarX.compare(nVarY)

      case (Double.NaN, _) =>
        if (yDB.numberOfUnknown > 0) -1
        else {
          val nVarX = x.sentence.variables.size
          val nVarY = y.sentence.variables.size
          nVarX.compare(nVarY)
        }

      case (_, Double.NaN) =>
        if (xDB.numberOfUnknown > 0) 1
        else {
          val nVarX = x.sentence.variables.size
          val nVarY = y.sentence.variables.size
          nVarX.compare(nVarY)
        }
        
      case _ =>
        // regular literals
        if (scoreX < scoreY) -1
        else if (scoreX > scoreY) 1
        else 0
    }
  }
}


object ClauseLiteralsOrdering {

  def apply(atomStateDB: Map[AtomSignature, AtomEvidenceDB]) = new ClauseLiteralsOrdering(atomStateDB)

  def apply(mln: MLN) = new ClauseLiteralsOrdering(mln.atomStateDB)
}
