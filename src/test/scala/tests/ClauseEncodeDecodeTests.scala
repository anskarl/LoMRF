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

package tests

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import lomrf.logic.AtomSignature
import lomrf.mln.model.MLN

/**
 * @author Anastasios Skarlatidis
 */

class ClauseEncodeDecodeTests extends FunSuite with ShouldMatchers {


  private val sep = System.getProperty("file.separator")
  private val testFilesPath = System.getProperty("user.dir") + sep + "data" + sep + "yale_shooting" + sep

  val queryAtoms = Set[AtomSignature](AtomSignature("HoldsAt", 2))
  val cwa = Set[AtomSignature](AtomSignature("Happens", 2), AtomSignature("Next", 2))
  val owa = Set[AtomSignature](AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2))
  val mln = MLN(
    testFilesPath + "theory.mln",
    testFilesPath + "narrative.db",
    queryAtoms, cwa, owa
  )





}