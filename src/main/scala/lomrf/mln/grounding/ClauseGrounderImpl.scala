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

package lomrf.mln.grounding

import java.{util => jutil}

import akka.actor.ActorRef
import auxlib.log.Logging
import gnu.trove.set.TIntSet
import lomrf.logic._
import lomrf.mln.model.{AtomIdentityFunction, FunctionMapper, MLN}
import AtomIdentityFunction.IDENTITY_NOT_EXIST
import lomrf.util.collection.IndexPartitioned
import lomrf.util.Cartesian

import scala.collection._
import scala.language.postfixOps
import scalaxy.streams.optimize
import lomrf.mln.model.AtomIdentityFunctionOps._

class ClauseGrounderImpl(val clause: Clause,
                         clauseIndex: Int,
                         mln: MLN,
                         cliqueRegisters: IndexPartitioned[ActorRef],
                         atomSignatures: Set[AtomSignature],
                         atomsDB: IndexPartitioned[TIntSet],
                         noNegWeights: Boolean = false,
                         eliminateNegatedUnit: Boolean = false) extends ClauseGrounder with Logging{

  require(!clause.weight.isNaN, "Found a clause with not a valid weight value (NaN).")

  private val evidence = mln.evidence
  private val constants = evidence.constants

  private val variableDomains: Map[Variable, Iterable[String]] = {
    if (clause.isGround) Map.empty[Variable, Iterable[String]]
    else (for (v <- clause.variables) yield v -> constants(v.domain))(breakOut)
  }

  private val groundIterator =
    try {
      Cartesian.CartesianIterator(variableDomains)
    } catch {
      case ex: NoSuchElementException =>
        fatal(s"Failed to initialise CartesianIterator for clause '${clause.toText()}' with domain '$variableDomains'")
    }


  private val identities: Map[AtomSignature, AtomIdentityFunction] =
    (for (literal <- clause.literals if !mln.isDynamicAtom(literal.sentence.signature))
    yield literal.sentence.signature -> mln.space.identities(literal.sentence.signature))(breakOut)



  private val orderedLiterals =
    clause.literals.view.map(lit =>
      (lit, identities.getOrElse(lit.sentence.signature, null))).toArray.sortBy(entry => entry._1)(ClauseLiteralsOrdering(mln))

  private val owaLiterals = orderedLiterals.view.map(_._1).filter(literal => mln.isTriState(literal.sentence.signature))

  // Collect dynamic atoms
  private val dynamicAtoms: Map[Int, (Vector[String] => Boolean)] =
    (for (i <- orderedLiterals.indices; sentence = orderedLiterals(i)._1.sentence; if sentence.isDynamic)
    yield i -> mln.schema.dynamicPredicates(sentence.signature))(breakOut)


  private val length = clause.literals.count(l => mln.isTriState(l.sentence.signature))

  def collectedSignatures = clause.literals.map(_.sentence.signature) -- atomSignatures

  def getVariableDomains = variableDomains

  def computeGroundings() {

    debug("The ordering of literals in clause: " + clause + "\n\t" +
      "changed to: " + orderedLiterals.map(_.toString()).reduceLeft(_ + " v " + _))

    def performGrounding(theta: Map[Variable, String] = Map.empty[Variable, String]): Int = {

      var sat = 0
      var counter = 0

      // an array of integer literals, indicating the current ground clause's literals
      val currentVariables = new Array[Int](length)

      // partial function for substituting terms w.r.t the given theta
      val substitution = substituteTerm(theta) _
      var idx = 0 //literal position index in the currentVariables array
      var position = 0
      var flagDrop = false //utility flag to indicate whether to keep or not the current ground clause
      val literalsIterator = orderedLiterals.iterator // literals iterator, that gives first all evidence literals

      while (!flagDrop && literalsIterator.hasNext) {
        val (literal, idf) = literalsIterator.next()
        // When the literal is a dynamic atom, then invoke its truth state dynamically
        if (literal.sentence.isDynamic) {
          //if (literal.isPositive == dynamicAtoms(idx)(literal.sentence.terms.map(substitution))) flagDrop = true
          flagDrop = literal.isPositive == dynamicAtoms(position)(literal.sentence.terms.map(substitution))
        }
        else {
          // Otherwise, invoke its state from the evidence
          val atomID = idf.encode(literal.sentence, substitution)

          if (atomID == IDENTITY_NOT_EXIST) {
            // Due to closed-world assumption in the evidence atoms or in the function mappings,
            // the identity of the atom cannot be determined and in that case the current clause grounding
            // will be omitted from the MRF
            flagDrop = true
          } else {
            // Otherwise, the atomID has a valid id number and the following pattern matching procedure
            // investigates whether the current literal satisfies the ground clause. If it does, the clause
            // is omitted from the MRF, since it is always satisfied from that literal.
            val state = evidence.db(literal.sentence.signature).get(atomID).value
            if ((literal.isNegative && (state == FALSE.value)) || (literal.isPositive && (state == TRUE.value))) {
              // the clause is always satisfied from that literal
              sat += 1
              flagDrop = true //we don't need to keep that ground clause
            }
            else if (state == UNKNOWN.value) {
              // The state of the literal is unknown, thus the literal will be stored to the currentVariables
              currentVariables(idx) = atomID
              idx += 1
            }
          }
        }
        position += 1
      } //end:  while (literalsIterator.hasNext && !flagDrop)

      if (!flagDrop) {
        // So far the ground clause is produced, but we have to
        // examine whether we will keep it or not. If the
        // ground clause contains any literal that is included in the
        // atomsDB, then it will be stored (and later will be send to clique registers),
        // otherwise it will not be stored and omitted.

        var canSend = false //utility flag

        var owaIdx = 0
        val cliqueVariables = new Array[Int](idx)

        optimize {
          for (i <- 0 until idx) {
            //val currentLiteral = iterator.next()
            val currentAtomID = currentVariables(i)
            cliqueVariables(i) = if (owaLiterals(owaIdx).isPositive) currentAtomID else -currentAtomID

            // Examine whether the current literal is included to the atomsDB. If it isn't,
            // the current clause will be omitted from the MRF
            val atomsDBSegment = atomsDB(currentAtomID)
            if (!canSend && (atomsDBSegment ne null)) canSend = atomsDBSegment.contains(currentAtomID)
            else if (atomsDBSegment eq null) canSend = true // this case happens only for Query literals

            owaIdx += 1
          }
        }


        if (canSend) {
          // Finally, the current ground clause will be included in the MRF.
          // However, if the weight of the clause is a negative number, then
          // the ground clause will be negated and broke up into several
          // unit ground clauses with positive weight literals.

          if (noNegWeights && clause.weight < 0) {
            if (cliqueVariables.length == 1) {
              // If the clause is unit and its weight value is negative
              // negate this clause (e.g. the clause "-w A" will be converted into w !A)
              cliqueVariables(0) = -cliqueVariables(0)
              store(-clause.weight, cliqueVariables, -1)
              counter += 1
            }
            else {
              val posWeight = -clause.weight / cliqueVariables.length
              for (groundLiteral <- cliqueVariables) {
                store(posWeight, Array(-groundLiteral), -1)
                counter += 1
              }
            }
          }
          else {

            // When we have a typical ground clause, with at least two literals,
            // we simply sort its literals and thereafter we store the ground clause.
            if (cliqueVariables.length > 1) {
              jutil.Arrays.sort(cliqueVariables)
              store(clause.weight, cliqueVariables, 1)
            }
            // Otherwise, we have a unit ground clause
            else {
              // When the flag 'eliminateNegatedUnit=true' and its unique literal is negative
              // then we negate the literal and invert the sign of its weight value
              if(eliminateNegatedUnit && cliqueVariables(0) < 0){
                cliqueVariables(0) = -cliqueVariables(0)
                store(-clause.weight, cliqueVariables, -1)
              }
              // Otherwise, store the unit clause as it is.
              else store(clause.weight, cliqueVariables, 1)
            }
            counter += 1
          }
          counter = 1
        } // end: if (canSend)
      }

      counter
    }

    if (clause.isGround) performGrounding()
    else while (groundIterator.hasNext) performGrounding(theta = groundIterator.next())

  }

  private def substituteTerm(theta: collection.Map[Variable, String])(term: Term): String = term match {
    case c: Constant => c.symbol
    case v: Variable => theta(v)
    case f: TermFunction =>
      mln.functionMappers.get(f.signature) match {
        case Some(m) => m(f.terms.map(a => substituteTerm(theta)(a)))

        case None =>
          val thetaStr = theta.map {
              case (k, v) => s"${k.toText} -> $v"
            }
            .mkString(", ")

          fatal(s"Cannot apply substitution using theta '[$thetaStr]' in function '${f.signature.toString}'")
      }
  }

  /**
   *
   * @param weight the clause weight
   * @param variables the ground clause literals (where negative values indicate negated atoms)
   * @param freq: -1 when the clause weight is been inverted, +1 otherwise.
   */
  private def store(weight: Double, variables: Array[Int], freq: Int) {
    var hashKey = jutil.Arrays.hashCode(variables)
    if (hashKey == 0) hashKey += 1 //required for trove collections, since zero represents the key-not-found value

    cliqueRegisters(hashKey) ! messages.CliqueEntry(hashKey, weight, variables, clauseIndex, freq )
  }

}
