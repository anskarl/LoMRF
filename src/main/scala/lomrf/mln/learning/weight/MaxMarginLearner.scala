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
 * Copyright (C) 2012 Anastasios Skarlatidis.
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

package lomrf.mln.learning.weight

import java.io.PrintStream
import auxlib.log.Logging
import lomrf.logic.AtomSignature
import lomrf.util.AtomEvidenceDB
import lomrf.mln.model.mrf._

/**
 * Under development
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
final class MaxMarginLearner(mrf: MRF, annotationDB: Map[AtomSignature, AtomEvidenceDB]) extends Logging {

  /*val state = MRFState(mrf)

  val annotation = annotationDB.get(AtomSignature("HoldsAt", 2)).get

  val iterator = mrf.atoms.iterator()
  while(iterator.hasNext) {
    iterator.advance()
    val atom = iterator.value()
    val value = annotation.get(atom.id)
    if(value == TRUE)
      atom.state = true
    else atom.state = false
  }

  state.evaluateState()*/

  def learn() = ???

  def writeResults(out: PrintStream = System.out) = ???
}
