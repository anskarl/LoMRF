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

import gnu.trove.set.hash.TIntHashSet
import lomrf.logic._
import lomrf.logic.{Literal, AtomSignature, KBParser}
import lomrf.mln.model.{AtomIdentifier, MLN}
import lomrf.util.{AtomEvidenceDB, AtomIdentityFunction}
import org.scalatest.{FunSpec, Matchers}
import lomrf.tests.ECExampleDomain1._

/**
 * @author Anastasios Skarlatidis
 */
class GroundingSpecTest extends FunSpec with Matchers {


  private val parser = new KBParser(predicateSchema, functionsSchema)

  private val atomIdentifier = AtomIdentifier(predicateSchema, constants, queryAtoms, hiddenAtoms)

  // Manually create sample evidence
  private val atomStateDB: Map[AtomSignature, AtomEvidenceDB] = {

    var result = Map[AtomSignature, AtomEvidenceDB]()

    // All predicates with open-world assumption have unknown state values --- i.e., HoldsAt/2, InitiatedAt/2 and TerminatedAt/2
    for (signature <- owa)
      result += signature -> AtomEvidenceDB.OWA(atomIdentifier.identities(signature))

    // Add all positive instantiations of predicate Next/2 (for time points 1 to 100)
    val nextSignature = AtomSignature("Next", 2)
    val nextIDF = atomIdentifier.identities(nextSignature)
    val nextPositives = new TIntHashSet()
    (1 until 100).map(t => nextIDF.encode(Seq((t + 1).toString, t.toString))).foreach(nextPositives.add)
    result += (nextSignature -> AtomEvidenceDB.CWA(nextPositives, nextIDF))

    // Assume true All instantiations of predicate Happens/2 having its first argument equals with 'walking' and
    // the rest instantiations are false.
    val happensSignature = AtomSignature("Happens", 2)
    val happensIDF = atomIdentifier.identities(happensSignature)
    val happensPositives = new TIntHashSet()
    (1 to 100).map(t => happensIDF.encode(Seq("Walking", t.toString))).foreach(happensPositives.add)
    result += (happensSignature -> AtomEvidenceDB.CWA(happensPositives, happensIDF))

    result
  }


  val formulaStr = "HoldsAt(f,t) ^ !TerminatedAt(f,t) ^ Next(t,tNext) => HoldsAt(f,tNext)."

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
      queryAtoms,
      cwa,
      owa,
      probabilisticAtoms = Set.empty[AtomSignature],
      tristateAtoms = Set.empty[AtomSignature],
      atomStateDB,
      atomIdentifier
    )


    val orderedLiterals: Array[Literal] =
      clause
        .literals
        .toArray
        .sortBy(l => l)(ClauseLiteralsOrdering(mln))

    info("original sequence of literals : " + clause.literals.map(_.toString).mkString(" v ") + "\n" +
      "ordered sequence of literals  : " + orderedLiterals.map(_.toString).mkString(" v "))

    // extract the sequence of unique variables
    info(uniqueOrderedVariablesIn(orderedLiterals).mkString(","))

    // The total number of terms is the sum of all arities
    val numTerms = orderedLiterals.map(_.arity).sum

    // mask: 1 for the first time that a new variable appears, 0 otherwise.
    val mask = new Array[Int](numTerms)
    var uniqueVariables = Set[Variable]()
    var index = 0
    for (literal <- orderedLiterals; term <- literal.sentence.terms) {
      term match {
        case v: Variable if !uniqueVariables.contains(v) =>
          uniqueVariables += v
          mask(index) = 1

        case c: Constant => mask(index) = 0

        case _ => mask(index) = 0
      }
      index += 1

    }

    info("mask: " + mask.mkString(", "))

  }


}
