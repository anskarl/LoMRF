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
import lomrf.util.{AtomEvidenceDB, ConstantsSet}
import org.scalatest.{FunSpec, Matchers}

import scala.collection
import scala.collection.mutable
import scala.collection.parallel.mutable

/**
 * @author Anastasios Skarlatidis
 */
class GroundingSpecTest2 extends FunSpec with Matchers with Logging {


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
    (1 until LAST_TIME_POINT).map(t => nextIDF.encode(Seq((t + 1).toString, t.toString))).foreach(nextPositives.add)
    result += (nextSignature -> AtomEvidenceDB.CWA(nextPositives, nextIDF))

    // Assume true All instantiations of predicate Happens/2 having its first argument equals with 'walking' and
    // the rest instantiations are false.
    val happensSignature = AtomSignature("Happens", 2)
    val happensIDF = atomIdentifier.identities(happensSignature)
    val happensPositives = new TIntHashSet()
    (1 to LAST_TIME_POINT).map(t => happensIDF.encode(Seq("Walking", t.toString))).foreach(happensPositives.add)
    result += (happensSignature -> AtomEvidenceDB.CWA(happensPositives, happensIDF))

    result
  }


  val formulaStr = "HoldsAt(f,t) ^ !TerminatedAt(f,t) ^ Next(t,tNext) => HoldsAt(f,tNext)."
  //val formulaStr = "HoldsAt(f,t) ^ !TerminatedAt(f,t) ^ Next(t,tNext) => HoldsAt(f, t+1)."

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


    val orderedLiterals =
      clause
        .literals
        .toArray
        .sortBy(l => l)(ClauseLiteralsOrdering(mln))

    info("original sequence of literals : " + clause.literals.map(_.toString).mkString(" v ") + "\n" +
      "ordered sequence of literals  : " + orderedLiterals.map(_.toString).mkString(" v "))


    // --- Lets build the flat-indexes


    def mkFlatTermDomains(literals: Iterable[Literal],
                          predicateSchema: collection.Map[AtomSignature, collection.Seq[String]],
                          functionSchema: collection.Map[AtomSignature, (String, collection.Seq[String])]): Vector[(Term, String)] = {

      val queue = collection.mutable.Queue[(Term, String)]()

      var memory = Set[Term]()
      var result = Vector[(Term, String)]()

      for (literal <- literals) {

        queue ++= literal.sentence.terms.zip(predicateSchema(literal.sentence.signature))

        while (queue.nonEmpty) {
          val (candidate, domain) = queue.dequeue()

          if (!candidate.isFunction && !memory.contains(candidate)) {
            memory += candidate
            result :+= (candidate, domain)

          }
          else candidate match {
            case f: TermFunction => queue ++= f.terms.zip(functionSchema(f.signature)._2)
            case _ => //do nothing
          }
        }
      }

      result
    }

    // theta:
    //  - Assume that we want to perform a theta substitution to a clause, in order to produce a possible grounding.
    //  - The theta array will hold the current domain index for each variable in the clause. The domain index represents
    //  the current constant value that will substitute to the corresponding variable. Additionally, for efficiency and
    //  technical reasons, the theta will contain the corresponding domain index of a each constant in the clause. This
    //  feature is required in the grounding process (not discussed here, see function: ???).
    //
    // For example:
    //  - Lets say that we want to represent the theta of the clause: !Foo(x, y) v !Bar(x, A) v Q(y, f(z))
    //  - the theta will be: theta = <idx(x), idx(y), A, idx(z)>
    //
    // theta.size:
    //  - Equal with the number of distinct variables and constants
    //
    val theta = new Array[Int](clause.variables.size + clause.constants.size)

    val flatTerms = mkFlatTermDomains(orderedLiterals, mln.predicateSchema, mln.functionSchema)

    for( ((term, domain), index) <- flatTerms.zipWithIndex) theta(index) = term match {
      case c: Constant => mln.constants(domain).get(c.symbol).get
      case _ => mln.constants(domain).size - 1
    }

    println("TERMS: " + flatTerms.mkString(", "))

    println("THETA: " + theta.mkString(", "))


    def mkL2T(): Array[Array[Int]] = {
      val literal2Theta = new Array[Array[Int]](orderedLiterals.size + clause.functions.size)

      // Step 1: parse all functions
      var func2Pos = Map[TermFunction, Int]() // gives the position of a function in the literal2Theta
      val functionsQueue = collection.mutable.Queue[TermFunction]() // to collect the functions to parse

      // offset: to begin from the auxiliary position of functions
      val offset = orderedLiterals.size - 1

      // TODO: implement step 1



      // Step 2: parse all literals
      for( (literal, index) <- orderedLiterals.zipWithIndex; atom = literal.sentence ) {

      }


      literal2Theta
    }




  }


}
