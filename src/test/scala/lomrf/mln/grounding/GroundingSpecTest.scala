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

import lomrf.logic.{Literal, AtomSignature, KBParser}
import lomrf.mln.model.MLN
import lomrf.util.{AtomEvidenceDB, AtomIdentityFunction}
import org.scalatest.{FunSpec, Matchers}

import scala.collection.breakOut


/**
 * @author Anastasios Skarlatidis
 */
class GroundingSpecTest extends FunSpec with Matchers {
  import lomrf.tests.ECExampleDomain._


  private var currentID = 1
  private val orderedStartIDs = new Array[Int](predicateSchema.size)
  private val orderedAtomSignatures = new Array[AtomSignature](predicateSchema.size)
  private var index = 0



  private val parser = new KBParser(predicateSchema, functionsSchema)


  val formulaStr = "Next(t,tNext) ^ HoldsAt(f,t) ^ !TerminatedAt(f,t) => HoldsAt(f,tNext)."

  describe("Formula '" + formulaStr + "'") {
    val formula = parser.parseFormula(formulaStr)
    val clauses = formula.toCNF(constants)

    it("produces a single clause") {
      clauses.size should be(1)
    }

    val clause = clauses.head

    it("contains three variables") {
      clause.variables.size should be(3)
    }

    val mln = new MLN(
      formulas = Set(formula),
      predicateSchema,
      functionsSchema,
      dynamicAtoms,
      dynamicFunctions,
      constants,
      functionMappers,
      queryAtoms = Set(AtomSignature("HoldsAt", 2)),
      cwa = Set(AtomSignature("Next", 2), AtomSignature("Close", 2), AtomSignature("OrientationMove", 3)),
      owa = Set(AtomSignature("InitiatedAt", 2), AtomSignature("TerminatedAt", 2)),
      probabilisticAtoms = Set.empty[AtomSignature],
      tristateAtoms = Set.empty[AtomSignature],
      identityFunctions,

      atomStateDB = Map.empty[AtomSignature, AtomEvidenceDB],
      Array[Int](1,2,3),
      Array[AtomSignature](),
      1,
      1
    )


    /**
     * A utility Map that associates AtomSignatures with AtomIdentityFunction (= Bijection of ground atoms to integers).
     * This Map contains information only for ordinary atoms (not dynamic atoms).
     */
    val identities: Map[AtomSignature, AtomIdentityFunction] =
      (for (literal <- clause.literals if !dynamicAtoms.contains(literal.sentence.signature))
      yield literal.sentence.signature -> identityFunctions(literal.sentence.signature))(breakOut)


    val orderedLiterals: Array[(Literal, AtomIdentityFunction)] = clause
       .literals
       .view
       .map(lit => (lit, identities.getOrElse(lit.sentence.signature, null)))
       .toArray
       //.sortBy(entry => entry._1)(ClauseLiteralsOrdering(mln))


  }


}
