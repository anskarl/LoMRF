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

import auxlib.log.Logging
import gnu.trove.set.hash.TIntHashSet
import lomrf.logic.{AtomSignature, KBParser, _}
import lomrf.mln.model.{AtomIdentifier, MLN}
import lomrf.tests.ECExampleDomain1._
import lomrf.util.AtomEvidenceDB
import org.scalatest.{FunSpec, Matchers}

/**
 * @author Anastasios Skarlatidis
 */
object GroundingMLNExp extends App with Logging {

  val s = System.getProperty("file.separator")
  val prefix = System.getProperty("user.dir") +"/Examples/data/caviar/video25_complete/".replace("/", s)

  val mlnFileName = prefix + "dec7a_plus.mln"
  val evidenceFileName = prefix + "fra1gt_evidence_succ.db"
  val queryAtoms = Set(AtomSignature("HoldsAt", 2))
  val owa = Set(AtomSignature("InitiatedAt",2), AtomSignature("TerminatedAt", 2))
  val cwa = Set(
    AtomSignature("HappensAt", 2),
    AtomSignature("StartTime", 1),
    AtomSignature("Close", 4),
    AtomSignature("OrientationMove", 3))

  val mln = MLN(mlnFileName, evidenceFileName, queryAtoms, owa, cwa)

  println(mln.toString)
}
