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
import lomrf.util.{AtomIdentityFunction, Logging}

import scala.collection._
import scala.language.postfixOps
import scalaxy.loops._

/**
 *
 * Experimental implementation of efficient grounding.
 *
 *
 * @author Anastasios Skarlatidis
 */
class ClauseGrounderImplNew private(
                          val clause: Clause,
                          mln: MLN,
                          cliqueRegisters: Array[ActorRef],
                          atomSignatures: Set[AtomSignature],
                          atomsDB: Array[TIntSet],
                          orderedLiterals: Array[(Literal, AtomIdentityFunction)],
                          owaLiterals: Array[Literal],
                          dynamicAtoms: Map[Int, (Vector[String] => Boolean)],
                          length: Int,
                          noNegWeights: Boolean,
                          eliminateNegatedUnit: Boolean) extends ClauseGrounder with Logging {

  require(!clause.weight.isNaN, "Found a clause with not a valid weight value (NaN).")

  // The number of clique batches is same with the number of clique registers, by default is the number of available
  // virtual processors
  private val cliqueBatches = cliqueRegisters.length

  // The number of atom DBs. Minimum is equal with the number of query atoms and maximum is the number of query and
  // hidden atoms.
  private val atomsDBBatches = atomsDB.length

  def collectedSignatures = clause.literals.map(_.sentence.signature) -- atomSignatures


  def computeGroundings() {

    debug("The ordering of literals in clause: " + clause + "\n\t" +
          "changed to: " + orderedLiterals.map(_.toString()).reduceLeft(_ + " v " + _))

    val orderedConstantSets =
      uniqueOrderedVariablesIn(orderedLiterals.map(_._1))
        .map(v => mln.constants(v.domain))


    val V2IDX: Map[Variable, Int] = (for((v, idx) <- uniqueOrderedVariablesIn(orderedLiterals.map(_._1)).zipWithIndex)
      yield v -> idx)(breakOut)

    val atom2TermIndexes = new Array[Array[Int]](orderedLiterals.size)

    var atomIdx = 0
    for( (atom, _ ) <- orderedLiterals) {
      val atomVars = uniqueOrderedVariablesIn(atom.sentence).toArray
      val indexes = new Array[Int](atomVars.size)

      for(i <- (0 until atomVars.length).optimized)
        indexes(i) = V2IDX(atomVars(i))


      atom2TermIndexes(atomIdx) = indexes
      atomIdx += 1
    }

    val ffIterator = CartesianIterator.mkArithmetic(orderedConstantSets)

    def performGrounding(substitution: Array[Int] ): Int = {

      var sat = 0
      var counter = 0

      // an array of integer literals, indicating the current ground clause's literals
      val currentVariables = new Array[Int](length)

      // partial function for substituting terms w.r.t the given theta
      //val substitution = substituteTerm(theta) _
      var idx = 0 //literal position index in the currentVariables array

      // current position in orderedLiterals
      var literalIdx = 0

      //utility position to indicate whether to keep or not the current ground clause
      val DROP = orderedLiterals.length + 1

      while (literalIdx < orderedLiterals.length) {
        val (literal, idf) = orderedLiterals(literalIdx)

        // When the literal is a dynamic atom, then invoke its truth state dynamically
        if (literal.sentence.isDynamic) {
          //TODO:
          //if (literal.isPositive == dynamicAtoms(idx)(literal.sentence.terms.map(substitution))) literalIdx = DROP
          sys.error("Dynamic atoms are not supported yet!")
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

            var weightToStore = clause.weight

            if (cliqueVariables.length > 1) jutil.Arrays.sort(cliqueVariables)
            else if(eliminateNegatedUnit && cliqueVariables.length == 1 && cliqueVariables(0) < 0){
              cliqueVariables(0) = -cliqueVariables(0)
              weightToStore = -weightToStore
            }

            store(weightToStore, cliqueVariables)

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

  /*private def substituteTerm(theta: collection.Map[Variable, String])(term: Term): String = term match {
    case c: Constant => c.symbol
    case v: Variable => theta(v)
    case f: TermFunction =>
      mln.functionMappers.get(f.signature) match {
        case Some(m) => m(f.terms.map(a => substituteTerm(theta)(a)))
        case None => fatal("Cannot apply substitution using theta: " + theta + " in function " + f.signature)
      }
  }*/

  private def store(weight: Double, variables: Array[Int]) {
    var hashKey = jutil.Arrays.hashCode(variables)
    if (hashKey == 0) hashKey += 1 //required for trove collections, since zero represents the key-not-found value

    cliqueRegisters(math.abs(hashKey % cliqueBatches)) ! CliqueEntry(hashKey, weight, variables)
  }

}

object ClauseGrounderImplNew {

  /**
   * @param clause the clause to ground
   * @param mln the MLN instance
   * @param cliqueRegisters the collection of available clique registers, to send groundings of the user specified clause.
   * @param atomSignatures the set of required atom signatures
   * @param atomsDB the Atoms DB, i.e., collection of integer sets that have been grounded
   * @param noNegWeights when it is true the negative weights are eliminated, otherwise the weights remain the same.
   * @param eliminateNegatedUnit when it is true the unit clauses with negative weights are eliminated.
   *
   * @return a new instance of ClauseGrounderImplNew
   */
  def apply(clause: Clause, mln: MLN, cliqueRegisters: Array[ActorRef], atomSignatures: Set[AtomSignature],
            atomsDB: Array[TIntSet], noNegWeights: Boolean = false, eliminateNegatedUnit: Boolean = false): ClauseGrounderImplNew = {

    /**
     * A utility Map that associates Variables with an iterable collection with its possible instantiations (according to
     * their given domain). For example, the predicate {{{HoldsAt(f, t)}}} has two variables, i.e., 'f' and 't'. Assume
     * that the possible instantiations of 't', according to some given evidence, is the discrete set 1 to 100. The Map
     * will contain as a key the {{{Variable("t")}}} and as a value the iterable collection of 1 to 100.
     */
    val variableDomains: Map[Variable, Iterable[String]] = {
      if (clause.isGround) Map.empty[Variable, Iterable[String]]
      else (for (v <- clause.variables) yield v -> mln.constants(v.domain))(breakOut)
    }

    /**
     * A utility Map that associates AtomSignatures with AtomIdentityFunction (= Bijection of ground atoms to integers).
     * This Map contains information only for ordinary atoms (not dynamic atoms).
     */
    val identities: Map[AtomSignature, AtomIdentityFunction] =
    (for (literal <- clause.literals if !mln.isDynamicAtom(literal.sentence.signature))
    yield literal.sentence.signature -> mln.identityFunctions(literal.sentence.signature))(breakOut)



    val orderedLiterals: Array[(Literal, AtomIdentityFunction)] = clause
      .literals
      .view
      .map(lit => (lit, identities.getOrElse(lit.sentence.signature, null)))
      .toArray
      .sortBy(entry => entry._1)(ClauseLiteralsOrdering(mln))


    // Collect literals with open-world assumption
    val owaLiterals: Array[Literal] = orderedLiterals
      .map(_._1) // get the literal instance
      .filter(literal => mln.isTriState(literal.sentence.signature))

    // Collect dynamic atoms
    val dynamicAtoms: Map[Int, (Vector[String] => Boolean)] =
      (for (i <- 0 until orderedLiterals.length; sentence = orderedLiterals(i)._1.sentence; if sentence.isDynamic)
      yield i -> mln.dynamicAtoms(sentence.signature))(breakOut)


    val length = clause.literals.count(l => mln.isTriState(l.sentence.signature))

    // Create the new instance 'ClauseGrounderImplNew':
    new ClauseGrounderImplNew(clause, mln, cliqueRegisters, atomSignatures, atomsDB, orderedLiterals,owaLiterals,
      dynamicAtoms,length, noNegWeights, eliminateNegatedUnit)
  }




}