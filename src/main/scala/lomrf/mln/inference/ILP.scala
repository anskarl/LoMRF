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

import java.io.PrintStream

import auxlib.log.Logging
import lomrf.mln.inference.LossFunction.LossFunction
import lomrf.mln.inference.RoundingScheme.RoundingScheme
import lomrf.mln.inference.Solver.Solver
import lomrf.mln.model.mrf._
import lomrf.util._
import oscar.linprog.modeling._
import oscar.algebra._
import gnu.trove.map.hash.{TIntDoubleHashMap, TIntObjectHashMap}
import scalaxy.loops._
import scala.language.postfixOps
import auxlib.trove.TroveConversions._

/**
 * This is an implementation of an approximate MAP inference algorithm for MLNs using Integer Linear Programming.
 * The original implementation of the algorithm can be found in: [[http://alchemy.cs.washington.edu/code/]].
 * Details about the ILP algorithm can be found in: Tuyen N. Huynh and Raymond J. Mooney. Max-Margin Weight Learning for
 * Markov Logic Networks. In Proceedings of the European Conference on Machine Learning and Principles and Practice of
 * Knowledge Discovery in Databases (ECML-PKDD 2011), Vol. 2, pp. 81-96, 2011.
 *
 * @param mrf The ground Markov network
 * @param outputAll Show 0/1 results for all query atoms (default is true)
 * @param ilpRounding Rounding algorithm selection option (default is RoundUp)
 * @param ilpSolver Solver type selection option (default is LPSolve)
 * @param lossFunction Loss function type (default is hamming distance)
 * @param lossAugmented Perform loss augmented inference (default is false)
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
final case class ILP(mrf: MRF, outputAll: Boolean = true, ilpRounding: RoundingScheme = RoundingScheme.ROUNDUP,
                ilpSolver: Solver = Solver.GUROBI, lossFunction: LossFunction = LossFunction.HAMMING,
                lossAugmented: Boolean = false) extends Logging {

  // Select the appropriate linear programming solver
  implicit val lp = if(ilpSolver == Solver.GUROBI) LPSolver(LPSolverLib.gurobi) else LPSolver(LPSolverLib.lp_solve)

  implicit val mln = mrf.mln

  /**
   * Fetch atom given its id.
   *
   * @param atomID id of the atom
   * @return the ground atom which corresponds to the given id
   */
  @inline private def fetchAtom(atomID: Int) = mrf.atoms.get(atomID)

  def infer() {

    val sTranslation = System.currentTimeMillis()

    /* Hash maps containing pairs of unique literal keys to LP variables [y]
     * and unique clause ids to LP variables [z].
     */
    val literalLPVars = new TIntObjectHashMap[LPFloatVar]()
    val clauseLPVars = new TIntObjectHashMap[LPFloatVar]()

    /**
     * A collection of expressions of the equation that we aim to maximize.
     * Each expression has the following form:
     *
     * {{{ weight * LP variable}}}
     */
    var expressions = List[LinearExpression]()

    val constraintsIterator = mrf.constraints.iterator()

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

        literalLPVars.putIfAbsent(atomID, LPFloatVar("y" + atomID, 0, 1))
        val floatVar = literalLPVars.get(atomID)

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
            if (constraint.literals(0) > 0) constraint.weight * literalLPVars.get(math.abs(constraint.literals(0)))
            else -constraint.weight * literalLPVars.get(math.abs(constraint.literals(0)))
          }
        }
        else {
          clauseLPVars.putIfAbsent(cid, LPFloatVar("z" + cid, 0, 1))
          expressions ::= math.abs(constraint.weight) * clauseLPVars.get(cid)
        }

      }

      debug("Expressions: [" + expressions.mkString(", ") + "]")

      // Step 3: Add constraints to the solver
      if (constraint.isHardConstraint) {
        add(sum(constraints) >= 1)
        debug(constraints.mkString(" + ") + " >= 1")
      }
      else if (!constraint.isUnit) {
        val clauseVar = clauseLPVars.get(cid)
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

    val eTranslation = System.currentTimeMillis()
    info(Utilities.msecTimeToText("Total translation time: ", eTranslation - sTranslation))

    info(
        "\nGround Atoms: " + mrf.numberOfAtoms +
        "\nAtom Variables: " + literalLPVars.size + " + Clauses Variables: " + clauseLPVars.size +
        " = " + (literalLPVars.size + clauseLPVars.size))

    val sSolver = System.currentTimeMillis()

    // Step 4: Optimize function subject to the constraints introduced
    maximize(sum(expressions))
    start()
    release()

    val eSolver = System.currentTimeMillis()
    info(Utilities.msecTimeToText("Total solver time: ", eSolver - sSolver))

    info(
        "\n=========================== Solution ===========================" +
        "\nAre constraints satisfied: " + checkConstraints() +
        "\nSolution status: " + status.toString +
        "\nObjective = " + objectiveValue.get)


    whenDebug {
      literalLPVars.iterator.foreach{ case (k: Int, v: LPFloatVar) => debug(v.name + " = " + v.value.get)}
      clauseLPVars.iterator.foreach{ case (k: Int, v: LPFloatVar) => debug(v.name + " = " + v.value.get)}
    }

    val solution = new TIntDoubleHashMap(literalLPVars.size())
    var fractionalSolutions = List[(Int, Double)]()

    // Search for fractional solutions and fix atom values of non fractional solutions
    var nonIntegralSolutionsCounter = 0
    for ((id, lpVar) <- literalLPVars.iterator()) {
      val value = math.min(lpVar.value.getOrElse(fatal("The value of LPVar with id '"+id+"' cannot be empty.")), 1.0)
      if (value != 0.0 && value != 1.0) {
        nonIntegralSolutionsCounter += 1
        fractionalSolutions ::= ((id, value))
      }
      else {
        fetchAtom(id).fixedValue = if(value == 0.0) -1 else 1
        fetchAtom(id).state = if(value == 0.0) false else true
      }
      solution.put(id, value)
    }

    // create MRF state
    val state = MRFState(mrf)

    info("Number of non-integral solutions: " + nonIntegralSolutionsCounter)
    assert(state.countUnfixAtoms() == nonIntegralSolutionsCounter)

    val sRoundUp = System.currentTimeMillis()

    // Should be executed here!
    state.evaluateState()

    if(nonIntegralSolutionsCounter > 0) ilpRounding match {

      // 1. RoundUp algorithm (see Huynh and Mooney, 2011)
      case RoundingScheme.ROUNDUP =>
        whenDebug(state.printStatistics())
        for (i <- (0 until fractionalSolutions.size).optimized) {

          val id = fractionalSolutions(i)._1

          if(state.computeDelta(id) > 0) {
            fetchAtom(id).fixedValue = 1
            fetchAtom(id).state = true
          }
          else {
            fetchAtom(id).fixedValue = -1
            fetchAtom(id).state = false
          }
          state.evaluateState()
          whenDebug(state.printStatistics())
        }

      // 2. MaxWalkSAT algorithm
      case RoundingScheme.MWS => MaxWalkSAT(mrf).infer(state)
    }
    debug("Unfixed atoms: " + state.countUnfixAtoms())

    val eRoundUp = System.currentTimeMillis()
    info(Utilities.msecTimeToText("Total roundup time: ", eRoundUp - sRoundUp))

    state.printStatistics()
    info(Utilities.msecTimeToText("Total ILP time: ", (eTranslation - sTranslation) +
                                                      (eSolver - sSolver) +
                                                      (eRoundUp - sRoundUp)
    ))

  }


  /**
   * Write the results of inference into the selected output stream.
   *
   * @param out Selected output stream (default is console)
   */
  def writeResults(out: PrintStream = System.out) {
    import lomrf.util.decodeAtom

    implicit val mln = mrf.mln

    val iterator = mrf.atoms.iterator()
    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key()
      if (atomID >= mln.queryStartID && atomID <= mln.queryEndID) {
        val groundAtom = iterator.value()
        val state = if(groundAtom.getState) 1 else 0

        if(outputAll) decodeAtom(iterator.key()) match {
          case Some(txtAtom) => out.println(txtAtom + " " + state)
          case _ => error("failed to decode id:" + atomID)
        }
        else if(state == 1) decodeAtom(iterator.key()) match {
          case Some(txtAtom) => out.println(txtAtom + " " + state)
          case _ => error("failed to decode id:" + atomID)
        }
      }
    }
  }

}

/**
 * Object holding constants for rounding type.
 */
object RoundingScheme extends Enumeration {
  type RoundingScheme = Value
  val ROUNDUP, MWS = Value
}

/**
 * Object holding constants for solver type.
 */
object Solver extends Enumeration {
  type Solver = Value
  val GUROBI, LPSOLVE = Value
}

/**
 * Object holding constants for loss function type.
 */
object LossFunction extends Enumeration {
  type LossFunction = Value
  val HAMMING = Value
}