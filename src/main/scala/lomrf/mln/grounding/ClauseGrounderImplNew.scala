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

import java.{util => jutil}

import akka.actor.ActorRef
import gnu.trove.set.TIntSet
import lomrf.logic._
import lomrf.mln.model.MLN
import lomrf.util.AtomIdentityFunction.IDENTITY_NOT_EXIST
import lomrf.util.Cartesian.CartesianIterator
import lomrf.util.{Logging, AtomIdentityFunction, Cartesian}

import scala.collection._
import scala.language.postfixOps
import scalaxy.loops._

/**
 * EXPERIMENTAL
 *
 * @author Anastasios Skarlatidis
 */
class ClauseGrounderImplNew(
                          val clause: Clause,
                          mln: MLN,
                          cliqueRegisters: Array[ActorRef],
                          atomSignatures: Set[AtomSignature],
                          atomsDB: Array[TIntSet],
                          noNegWeights: Boolean = false,
                          eliminateNegatedUnit: Boolean = false) extends ClauseGrounder with Logging{

  require(!clause.weight.isNaN, "Found a clause with not a valid weight value (NaN).")

  private val cliqueBatches = cliqueRegisters.length
  private val atomsDBBatches = atomsDB.length

  private val variableDomains: Map[Variable, Iterable[String]] = {
    if (clause.isGround) Map.empty[Variable, Iterable[String]]
    else (for (v <- clause.variables) yield v -> mln.constants(v.domain))(breakOut)
  }

  private val groundIterator =
    try {
      Cartesian.CartesianIterator(variableDomains)
    } catch {
      case ex: NoSuchElementException =>
        fatal("Failed to initialise CartesianIterator for clause: " +
          clause.toString + " --- domain = " +
          variableDomains)
    }


  private val identities: Map[AtomSignature, AtomIdentityFunction] =
    (for (literal <- clause.literals if !mln.isDynamicAtom(literal.sentence.signature))
    yield literal.sentence.signature -> mln.identityFunctions(literal.sentence.signature))(breakOut)


  /**
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
   */
  private val orderedLiterals =
    clause.literals.view.map(lit =>
      (lit, identities.getOrElse(lit.sentence.signature, null))).toArray.sortBy(entry => entry._1)(new Ordering[Literal] {

      def compare(x: Literal, y: Literal) = {
        val xDB = mln.atomStateDB.getOrElse(x.sentence.signature, null)
        val yDB = mln.atomStateDB.getOrElse(y.sentence.signature, null)

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
    })

  private val owaLiterals = orderedLiterals.view.map(_._1).filter(literal => mln.isTriState(literal.sentence.signature))

  // Collect dynamic atoms
  private val dynamicAtoms: Map[Int, (List[String] => Boolean)] =
    (for (i <- 0 until orderedLiterals.length; sentence = orderedLiterals(i)._1.sentence; if sentence.isDynamic)
    yield i -> mln.dynamicAtoms(sentence.signature))(breakOut)


  private val length = clause.literals.count(l => mln.isTriState(l.sentence.signature))

  def collectedSignatures = clause.literals.map(_.sentence.signature) -- atomSignatures

  def getVariableDomains = variableDomains


  def computeGroundings() {

    debug("The ordering of literals in clause: " + clause + "\n\t" +
          "changed to: " + orderedLiterals.map(_.toString()).reduceLeft(_ + " v " + _))

    val orderedConstantSets =
      uniqueOrderedVariablesIn(orderedLiterals.map(_._1))
        .map(v => mln.constants(v.domain))


    val V2IDX: Map[Variable, Int] = (for((v, idx) <- uniqueOrderedVariablesIn(orderedLiterals.map(_._1)).zipWithIndex)
      yield v -> idx)(breakOut)

    val atom2TermIndexes = new Array[Array[Int]](orderedLiterals.size)

    var atomIdx =0
    for( (atom, _) <- orderedLiterals) {
      val atomVars = uniqueOrderedVariablesIn(atom.sentence).toArray
      val indexes = new Array[Int](atomVars.size)

      for(i <- (0 until atomVars.length).optimized){
        indexes(i) = V2IDX(atomVars(i))
      }

      atom2TermIndexes(atomIdx) = indexes
      atomIdx += 1
    }

    val ffIterator = CartesianIterator.apply2(orderedConstantSets)




    def performGrounding(substitution: Array[Int] ): Int = {


      var sat = 0
      var counter = 0

      // an array of integer literals, indicating the current ground clause's literals
      val currentVariables = new Array[Int](length)

      // partial function for substituting terms w.r.t the given theta
      //val substitution = substituteTerm(theta) _
      var idx = 0 //literal position index in the currentVariables array

      var literalIdx = 0 // current position in orderedLiterals
      val DROP = orderedLiterals.length + 1 //utility position to indicate whether to keep or not the current ground clause
      while (literalIdx < orderedLiterals.length) {
        val (literal, idf) = orderedLiterals(literalIdx)
        // When the literal is a dynamic atom, then invoke its truth state dynamically
        if (literal.sentence.isDynamic) {
          //TODO:
          //if (literal.isPositive == dynamicAtoms(idx)(literal.sentence.terms.map(substitution))) literalIdx = DROP
          sys.error("Dynamic atoms are not supported!")
        }
        else {
          // Otherwise, invoke its state from the evidence
          val atomID = idf.encode(atom2TermIndexes(literalIdx), substitution)

          if (atomID == IDENTITY_NOT_EXIST) {
            // Due to closed-world assumption in the evidence atoms or in the function mappings,
            // the identity of the atom cannot be determined and in that case the current clause grounding
            // will be omitted from the MRF
            literalIdx = DROP
          }
          else {
            // Otherwise, the atomID has a valid id number and the following pattern matching procedure
            // investigates whether the current literal satisfies the ground clause. If it does, the clause
            // is omitted from the MRF, since it is always satisfied from that literal.
            val state = mln.atomStateDB(literal.sentence.signature).get(atomID).value
            if ((literal.isNegative && (state == FALSE.value)) || (literal.isPositive && (state == TRUE.value))) {
              // the clause is always satisfied from that literal
              sat += 1
              literalIdx = DROP //we don't need to keep that ground clause
            }
            else if (state == UNKNOWN.value) {
              // The state of the literal is unknown, thus the literal will be stored to the currentVariables
              currentVariables(idx) = atomID
              idx += 1
            }
          }
        }
        literalIdx += 1
      } //end:  while (literalsIterator.hasNext && !flagDrop)

      if (literalIdx == DROP) {
        // So far the ground clause is produced, but we have to
        // examine whether we will keep it or not. If the
        // ground clause contains any literal that is included in the
        // atomsDB, then it will be stored (and later will be send to clique registers),
        // otherwise it will not be stored and omitted.

        var canSend = false //utility flag

        var owaIdx = 0
        val cliqueVariables = new Array[Int](idx)

        for (i <- (0 until idx).optimized) {
          //val currentLiteral = iterator.next()
          val currentAtomID = currentVariables(i)
          cliqueVariables(i) = if (owaLiterals(owaIdx).isPositive) currentAtomID else -currentAtomID

          // Examine whether the current literal is included to the atomsDB. If it isn't,
          // the current clause will be omitted from the MRF
          val atomsDBSegment = atomsDB(currentAtomID % atomsDBBatches)
          if (!canSend && (atomsDBSegment ne null)) canSend = atomsDBSegment.contains(currentAtomID)
          else if (atomsDBSegment eq null) canSend = true // this case happens only for Query literals

          owaIdx += 1
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
              store(-clause.weight, cliqueVariables)
              counter += 1
            }
            else {
              val posWeight = -clause.weight / cliqueVariables.length
              for(i <- (0 until cliqueVariables.length).optimized){
                store(posWeight, Array(-cliqueVariables(i)))
                counter += 1
              }
            }
          }
          else {

            // store as it is
            /* if (cliqueVariables.length > 1) jutil.Arrays.sort(cliqueVariables)
             store(clause.weight, cliqueVariables)*/

            var www = clause.weight
            if (cliqueVariables.length > 1) jutil.Arrays.sort(cliqueVariables)
            else if(eliminateNegatedUnit && cliqueVariables.length == 1 && cliqueVariables(0) < 0){
              cliqueVariables(0) = -cliqueVariables(0)
              www = -www
            }

            store(www, cliqueVariables)

            counter += 1
          }
          counter = 1
        } // end: if (canSend)
      }

      counter
    }

    //if(clause.isGround) performGrounding()
    //else
    while(ffIterator.hasNext) performGrounding(ffIterator.next())

    /*if (clause.isGround) performGrounding()
    else while (groundIterator.hasNext) performGrounding(theta = groundIterator.next())*/
  }

  private def substituteTerm(theta: collection.Map[Variable, String])(term: Term): String = term match {
    case c: Constant => c.symbol
    case v: Variable => theta(v)
    case f: TermFunction =>
      mln.functionMappers.get(f.signature) match {
        case Some(m) => m(f.terms.map(a => substituteTerm(theta)(a)))
        case None => fatal("Cannot apply substitution using theta: " + theta + " in function " + f.signature)
      }
  }

  private def store(weight: Double, variables: Array[Int]) {
    var hashKey = jutil.Arrays.hashCode(variables)
    if (hashKey == 0) hashKey += 1 //required for trove collections, since zero represents the key-not-found value

    cliqueRegisters(math.abs(hashKey % cliqueBatches)) ! CliqueEntry(hashKey, weight, variables)
  }

}