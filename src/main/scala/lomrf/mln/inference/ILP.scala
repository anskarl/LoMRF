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

package lomrf.mln.inference

import lomrf.mln.model.mrf._
import lomrf.util._
import oscar.linprog.modeling._
import oscar.algebra._
import java.io.PrintStream
import gnu.trove.map.TIntObjectMap
import gnu.trove.map.hash.{TIntDoubleHashMap, TIntObjectHashMap}
import scala.annotation.switch
import scalaxy.loops._
import scala.language.postfixOps
import lomrf.util.TroveImplicits._
import lomrf.util.TroveConversions._

/**
 * This is an implementation of an approximate MAP inference algorithm for MLNs using Integer Linear Programming.
 * The original implementation of the algorithm can be found in: [[http://alchemy.cs.washington.edu/code/]].
 * Details about the ILP algorithm can be found in the following publications:
 *
 * <ul>
 * <li> Tuyen N. Huynh and Raymond J. Mooney. Max-Margin Weight Learning for Markov Logic Networks.
 * In Proceedings of the European Conference on Machine Learning and Principles and Practice of
 * Knowledge Discovery in Databases (ECML-PKDD 2011), Vol. 2, pp. 81-96, 2011.
 * </li>
 *
 * <li> Jan Noessner, Mathias Niepert and Heiner Stuckenschmidt.
 * RockIt: Exploiting Parallelism and Symmetry for MAP Inference in Statistical Relational Models.
 * Proceedings of the Twenty-Seventh (AAAI) Conference on Artificial Intelligence, July 14-18, 2013.
 * Bellevue, Washington: AAAI Press
 * </li>
 * </ul>
 *
 * @param mrf The ground Markov network
 * @param ilpRounding The rounding algorithm selection option
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
final class ILP(mrf: MRF, ilpRounding: Int) extends LPModel(LPSolverLib.lp_solve) with Logging {

  implicit val mln = mrf.mln

  def infer(out: PrintStream = System.out) {

    /* Hash maps containing pairs of unique literal keys to LD variables [y]
     * and unique clause ids to LD variables [z].
     */
    val literalLDVars = new TIntObjectHashMap[LPFloatVar]()
    val clauseLDVars = new TIntObjectHashMap[LPFloatVar]()

    /**
     * A collection of expressions of the equation that we aim to maximize.
     * Each expression has the following form:
     *
     * {{{ weight * LD variable}}}
     */
    var expressions = List[LinearExpression]()

    var constraintsIterator = mrf.constraints.iterator()

    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()

      var constraints: List[LinearExpression] = Nil

      // fetch the current constraint, i.e., current weighted ground clause or clique
      val constraint = constraintsIterator.value()

      debug(
        "Ground Clause: " + constraint.weight + " " +
          constraint
            .literals
            .map(l => decodeLiteral(l).getOrElse(sys.error("Cannot decode literal: " + l)))
            .reduceLeft(_ + " v " + _))

      // Step 1: Introduce variables for each ground atom and create possible constraints
      for (literal <- constraint.literals) {
        val atomID = math.abs(literal)
        literalLDVars.putIfAbsent(atomID, LPFloatVar("y" + atomID, 0, 1))
        val floatVar = literalLDVars.get(atomID)

        if ((constraint.weight > 0 || constraint.weight.isInfinite || constraint.weight.isNaN ||
          constraint.weight == mrf.weightHard) && literal > 0)
          constraints ::= floatVar
        else if ((constraint.weight > 0 || constraint.weight.isInfinite || constraint.weight.isNaN ||
          constraint.weight == mrf.weightHard) && literal < 0)
          constraints ::= (1 - floatVar)
        else if (constraint.weight < 0 && literal < 0)
          constraints ::= floatVar
        else
          constraints ::= (1 - floatVar)
      }

      debug("Possible Constraints: [" + constraints.mkString(", ") + "]")

      val cid = constraint.id

      // Step 2: Create expressions for objective function (only for soft constraints)
      if (!constraint.weight.isInfinite && !constraint.weight.isNaN && constraint.weight != mrf.weightHard) {

        if (constraint.isUnit) {
          expressions ::= {
            if (constraint.literals(0) > 0) constraint.weight * literalLDVars.get(math.abs(constraint.literals(0)))
            else -constraint.weight * literalLDVars.get(math.abs(constraint.literals(0)))
          }
        }
        else {
          clauseLDVars.putIfAbsent(cid, LPFloatVar("z" + cid, 0, 1))
          expressions ::= math.abs(constraint.weight) * clauseLDVars.get(cid)
        }

      }

      debug("Expressions: [" + expressions.mkString(", ") + "]")

      // Step 3: Add constraints to the solver
      if (constraint.isHardConstraint) {
        add(sum(constraints) >= 1)
        debug(constraints.mkString(" + ") + " >= 1")
      }
      else if (!constraint.isUnit) {
        val clauseVar = clauseLDVars.get(cid)
        if (constraint.weight > 0) {
          add(sum(constraints) >= clauseVar)
          debug(constraints.mkString(" + ") + " >= " + clauseVar.name)
        }
        else {
          for (c <- constraints) {
            add(c >= clauseVar)
            debug(c + " >= " + clauseVar.name)
          }
        }
      }
    }

    info(
        "\nGround Atoms: " + mrf.numberOfAtoms +
        "\nAtom Variables: " + literalLDVars.size + " + Clauses Variables: " + clauseLDVars.size +
        " = " + (literalLDVars.size + clauseLDVars.size))


    // Step 4: Optimize function subject to the constraints introduced
    maximize(sum(expressions))
    start()
    release()

    info(
        "\n=========================== Solution ===========================" +
        "\nAre constraints satisfied: " + checkConstraints() +
        "\nSolution status: " + status.toString +
        "\nObjective = " + objectiveValue.get)


    whenDebug {
      literalLDVars.iterator.foreach{ case (k: Int, v: LPFloatVar) => debug(v.name + " = " + v.value.get)}
      clauseLDVars.iterator.foreach{ case (k: Int, v: LPFloatVar) => debug(v.name + " = " + v.value.get)}
    }

    val solution = new TIntDoubleHashMap(literalLDVars.size())

    var nonIntegralSolutionsCounter = 0
    for ((k, v) <- literalLDVars.iterator()) {
      val p = v.value.get
      if (p != 0.0 && p != 1.0) nonIntegralSolutionsCounter += 1
      solution.put(k, p)
    }

    // TODO: At this point LDVar hash maps are useless, so should we destroy them?

    info("Number of non-integral solutions: " + nonIntegralSolutionsCounter)

    /*if (nonIntegralSolutionsCounter > 0) {
      val clauses = new TIntObjectHashMap[Constraint]()
      clauses.putAll(mrf.constraints)
      roundup(solution, clauses)
    }

    var w = 0.0
    var wNAll = 0
    constraintsIterator = mrf.constraints.iterator()
    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()
      if (constraint.weight.isInfinite || constraint.weight.isNaN || constraint.weight == mrf.weightHard) w += mrf.weightHard
      else if (constraint.weight > 0) w += constraint.weight
      else wNAll += 1
    }

    info("Number of clauses with negative weights: " + wNAll + "/" + mrf.constraints.size())
    info("Likelihood upper bound: e^" + w)*/

    val solutionIterator = solution.iterator()
    while(solutionIterator.hasNext){
      solutionIterator.advance()
      out.println(decodeLiteral(solutionIterator.key()).get + " " + solutionIterator.value())
    }

  }

  def roundup(solution: TIntDoubleHashMap, groundClauses: TIntObjectMap[Constraint]) {
    var fractionalSolutions = List[(Double, Int, Double)]()

    val unsatConstraints = new TIntObjectHashMap[Constraint]()

    val clausesIterator = groundClauses.iterator()

    while(clausesIterator.hasNext){
      clausesIterator.advance()

      val constraint = clausesIterator.value()
      var idx = 0
      var sat = false

      while(sat | idx < constraint.literals.length){
        val literal = constraint.literals(idx)
        val solutionForAtom = solution.get(math.abs(literal))
        if(solutionForAtom == 0.0 && literal < 0 || solutionForAtom == 1.0 && literal > 0) sat = true
        idx += 1
      }

      if(!sat) unsatConstraints.put(clausesIterator.key(), constraint)
    }

    info("Number of fractional solutions: " + fractionalSolutions.size)
    info("Number of unsatisfied clauses: " + unsatConstraints.size())

    for (i <- (0 until fractionalSolutions.size).optimized) {
      val k = fractionalSolutions(i)._2
      val v = fractionalSolutions(i)._3

      var delta = 0.0

      val constraintsIterator = unsatConstraints.iterator()

      while (constraintsIterator.hasNext) {
        constraintsIterator.advance()
        val constraint = constraintsIterator.value()

        if (constraint.literals.contains(k))
          delta += constraint.weight
        else if (constraint.literals.contains(-k))
          delta -= constraint.weight
      }

      val y = if (delta > 0) 1.0 else 0.0
      solution.put(k, y)

      (v: @switch) match {
        case 1.0 => unsatConstraints.retainEntries((a: Int, b: Constraint) => b.literals.contains(-k))
        case 0.0 => unsatConstraints.retainEntries((a: Int, b: Constraint) => b.literals.contains(k))
        case _ => fractionalSolutions ::= ((0.0, k, v))
      }

      info("\titeration: " + i + ", number of unsatisfied clauses: " + unsatConstraints.size() + ", solution: " + y)
    }

    var wNUnSat = 0
    var constraintsIterator = unsatConstraints.iterator()

    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()

      val key = constraintsIterator.key()
      val constraint = constraintsIterator.value()
      groundClauses.remove(key)

      if (constraint.weight < 0) wNUnSat += 1
    }

    var w = 0.0
    constraintsIterator = groundClauses.iterator()

    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()
      w += (if (constraint.isHardConstraint) mrf.weightHard else constraint.weight)
    }

    info(
      "\n#SAT Clauses: " + (groundClauses.size() - unsatConstraints.size()) +
      "\n#UnSAT clauses with negative weights: " + wNUnSat + "/" + unsatConstraints.size() +
      "\nLikelihood of the solution is: e^" + w)

  }

  /*def writeResults(out: PrintStream = System.out) {
    import lomrf.util.decodeAtom

    implicit val mln = mrf.mln

    val iterator = mrf.atoms.iterator()
    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key()
      if (atomID >= mln.queryStartID && atomID <= mln.queryEndID) {
        val groundAtom = iterator.value()
        val state = if(groundAtom.getState) 1 else 0
        if(showAll) {
          decodeAtom(iterator.key()) match {
            case Some(txtAtom) => out.println(txtAtom + " " + state)
            case _ => error("failed to decode id:" + atomID)
          }
        }
        else {
          if(state == 1) decodeAtom(iterator.key()) match {
            case Some(txtAtom) => out.println(txtAtom + " " + state)
            case _ => error("failed to decode id:" + atomID)
          }
        }
      }
    }
  }*/

}
