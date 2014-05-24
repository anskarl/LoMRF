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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert._
import lomrf.mln.model._
import lomrf.logic.AtomSignature
import lomrf.mln.inference.MRFBuilder
import lomrf.util.{Utilities, Logging}

/**
 * @author Anastasios Skarlatidis
 */

class KBMCTest extends AssertionsForJUnit with Logging{
  private val sep = System.getProperty("file.separator")
  private val testFilesPath = System.getProperty("user.dir") + sep +"data"+ sep +"tests" +sep

  @Test def simpleKBMC_Test1(){
    val queryAtoms = Set[AtomSignature](AtomSignature("AdvisedBy",2))
    val cwa = Set[AtomSignature](AtomSignature("GradStudent",1), AtomSignature("Prof",1), AtomSignature("SameGroup",2), AtomSignature("TA",2))

    info("Loading an MLN instance from data.")
     val mln = MLN(
       testFilesPath+"TestUniversity.mln",
       testFilesPath+"TestUniversityEvidence_SMALL.db",
       queryAtoms,
       cwa)
    info("\n"+mln+"\n")

    

    info("Computing CNF clauses...")
    assertTrue(mln.clauses.size>0)
    info("No. of CNF clauses: "+mln.clauses.size)
    info("grounding...")
    val mrfBuilder = new MRFBuilder(mln)
    val startTime = System.currentTimeMillis()
    mrfBuilder.buildNetwork
    val endTime = System.currentTimeMillis()
    info("Total time: "+Utilities.msecTimeToText(endTime - startTime))
  }
  
}