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

import lomrf.logic.{TRUE, TriState, AtomSignature}
import java.io.PrintStream
import auxlib.log.Logging
import lomrf.mln.inference.RoundingScheme.RoundingScheme
import lomrf.mln.inference.Solver.Solver
import lomrf.mln.model.{AtomIdentityFunctionOps, AtomEvidenceDB}
import lomrf.mln.model.mrf._
import lomrf.util.time._
import AtomIdentityFunctionOps._
import lomrf.logic.AtomSignatureOps._
import gnu.trove.map.hash.TIntObjectHashMap
import optimus.algebra._
import optimus.optimization._
import scala.util.{Failure, Success}
import scalaxy.streams.optimize
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
 * @param annotationDB Annotation database holding the ground truth values for non evidence
 *                     atoms. Required when performing loss augmented inference.
 * @param outputAll Show 0/1 results for all query atoms (default is true)
 * @param ilpRounding Rounding algorithm selection option (default is RoundUp)
 * @param ilpSolver Solver type selection option (default is LPSolve)
 * @param lossAugmented Perform loss augmented inference using hamming distance (default is false)
 *
 *
 *
 */
final class ILP(mrf: MRF, annotationDB: Map[AtomSignature, AtomEvidenceDB] = Map.empty[AtomSignature, AtomEvidenceDB],
                outputAll: Boolean = true, ilpRounding: RoundingScheme = RoundingScheme.ROUNDUP, ilpSolver: Solver = Solver.LPSOLVE,
                lossAugmented: Boolean = false) extends Logging {

  // Select the appropriate mathematical programming solver
  //implicit val problem = if(ilpSolver == Solver.GUROBI) LQProblem(SolverLib.gurobi) else LQProblem(SolverLib.lp_solve)

  implicit val problem = ilpSolver match {
    case Solver.GUROBI => LQProblem(SolverLib.gurobi)
    case Solver.LPSOLVE => LQProblem(SolverLib.lp_solve)
    case Solver.OJALGO => LQProblem(SolverLib.oJalgo)
  }

  implicit val mln = mrf.mln

  /**
   * Fetch atom given its id.
   *
   * @param atomID id of the atom
   * @return the ground atom which corresponds to the given id
   */
  @inline private def fetchAtom(atomID: Int) = mrf.atoms.get(atomID)

  /**
   * Fetch annotation from database for the given atom id. Annotation
   * exist only for non evidence atoms.
   *
   * @param atomID id of the atom
   * @return annotation TriState value (TRUE, FALSE or UNKNOWN)
   */
  @inline private def getAnnotation(atomID: Int): TriState = {
    val annotation = annotationDB(atomID.signature(mrf.mln))
    annotation(atomID)
  }

  def infer(): MRFState = {

    if(lossAugmented) {
      assert(annotationDB.nonEmpty, "Annotation database does not exist!")
      info("Running loss augmented inference...")
    }

    val sTranslation = System.currentTimeMillis()

    /* Hash maps containing pairs of unique literal keys to LP variables [y]
     * and unique clause ids to LP variables [z].
     */
    val literalLPVars = new TIntObjectHashMap[MPFloatVar]()
    val clauseLPVars = new TIntObjectHashMap[MPFloatVar]()

    /**
     * A collection of expressions of the equation that we aim to maximize.
     * Each expression has the following form:
     *
     * {{{ weight * LP variable}}}
     */
    var expressions = List[Expression]()

    val atomsIterator = mrf.atoms.iterator()

    // Step 1: Introduce variables for each ground atom
    while(atomsIterator.hasNext) {
      atomsIterator.advance()
      val atomID = math.abs(atomsIterator.key())

      literalLPVars.put(atomID, MPFloatVar("y" + atomID, 0, 1))

      /* In case of loss augmented inference, Hamming distance is used which
       * is equivalent to adding 1 to the coefficient of ground atom y if the
       * true (annotated) value of y is FALSE and subtracting 1 from the
       * coefficient of y if the true value of y is TRUE.
       */
      if(lossAugmented) {
        val loss = if (getAnnotation(atomID) == TRUE) -1.0 else 1.0
        expressions ::= loss * literalLPVars.get(atomID)
      }
    }

    val constraintsIterator = mrf.constraints.iterator()

    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()

      var constraints: List[Expression] = Nil

      // fetch the current constraint, i.e., current weighted ground clause or clique
      val constraint = constraintsIterator.value()

      whenDebug{
        val decodedConstraint = constraint.decodeFeature(mrf.weightHard).getOrElse(fatal(s"Cannot decode constraint $constraint"))
        debug(s"Ground Clause: ${constraint.getWeight} $decodedConstraint")
      }

      // Step 1: Introduce variables for each ground atom and create possible constraints
      for (literal <- constraint.literals) {
        val atomID = math.abs(literal)
        val floatVar = literalLPVars.get(atomID)

        if ((constraint.getWeight > 0 || constraint.getWeight.isInfinite || constraint.getWeight.isNaN ||
          constraint.getWeight == mrf.weightHard) && literal > 0)
          constraints ::= floatVar
        else if ((constraint.getWeight > 0 || constraint.getWeight.isInfinite || constraint.getWeight.isNaN ||
          constraint.getWeight == mrf.weightHard) && literal < 0)
          constraints ::= (1 - floatVar)
        else if (constraint.getWeight < 0 && literal < 0)
          constraints ::= floatVar
        else
          constraints ::= (1 - floatVar)
      }

      debug("Possible Constraints: [" + constraints.mkString(", ") + "]")

      val cid = constraint.id

      // Step 2: Create expressions for objective function (only for soft constraints)
      if (!constraint.getWeight.isInfinite && !constraint.getWeight.isNaN && constraint.getWeight != mrf.weightHard && constraint.getWeight != 0.0) {

        if (constraint.isUnit) {
          expressions ::= {
            if (constraint.literals(0) > 0) constraint.getWeight * literalLPVars.get(math.abs(constraint.literals(0)))
            else (-constraint.getWeight) * literalLPVars.get(math.abs(constraint.literals(0)))
          }
        }
        else { // there is no case where the same clause is going to create another z variable, so use put not putIfAbsent
          clauseLPVars.put(cid, MPFloatVar("z" + cid, 0, 1))
          expressions ::= math.abs(constraint.getWeight) * clauseLPVars.get(cid)
        }

      }

      debug("Expressions: [" + expressions.mkString(", ") + "]")

      // Step 3: Add constraints to the solver (don't introduce constraint for zero weighted constraints)
      if (constraint.isHardConstraint) {
        add(sum(constraints) >= 1)
        debug(constraints.mkString(" + ") + " >= 1")
      }
      else if (!constraint.isUnit && constraint.getWeight != 0.0) {
        val clauseVar = clauseLPVars.get(cid)
        if (constraint.getWeight > 0) {
          add(sum(constraints) >= clauseVar)
          debug(constraints.mkString(" + ") + " >= " + clauseVar.symbol)
        }
        else {
          for (c <- constraints) {
            add(c >= clauseVar)
            debug(c + " >= " + clauseVar.symbol)
          }
        }
      }
    }

    val eTranslation = System.currentTimeMillis()
    info(msecTimeToText("Translation time: ", eTranslation - sTranslation))

    info(
        "\nGround Atoms: " + mrf.numberOfAtoms +
        "\nAtom Variables: " + literalLPVars.size + " + Clauses Variables: " + clauseLPVars.size +
        " = " + (literalLPVars.size + clauseLPVars.size))

    val sSolver = System.currentTimeMillis()

    // Step 4: Optimize function subject to the constraints introduced
    maximize(sum(expressions))
    start(PreSolve.CONSERVATIVE)
    release()

    val eSolver = System.currentTimeMillis()
    info(msecTimeToText("Solver time: ", eSolver - sSolver))

    info(
        "\n=========================== Solution ===========================" +
        "\nAre constraints satisfied: " + checkConstraints() +
        "\nSolution status: " + status.toString +
        "\nObjective = " + objectiveValue)


    whenDebug {
      literalLPVars.iterator.foreach { case (k: Int, v: MPFloatVar) =>
        debug(v.symbol + " = " + v.value.getOrElse("Value does not exist for this ground atom variable!"))
      }
      clauseLPVars.iterator.foreach { case (k: Int, v: MPFloatVar) =>
        debug(v.symbol + " = " + v.value.getOrElse("Value does not exist for this constraint variable"))
      }
    }

    // Create MRF state and assume every constraint to be unsatisfied
    val state = MRFState(mrf)
    state.unfixAll()

    // Search for fractional solutions and fix atom values of non fractional solutions
    var nonIntegralSolutionsCounter = 0
    var fractionalSolutions = Vector[Int]()

    for ( (id, lpVar) <- literalLPVars.iterator() ) {
      /*
       * Round values very close to 0 and 1 in using this naive approach because they
       * probably arised from rounding error of the solver.
       */
      val value = if (lpVar.value.get > 0.99) 1.0 else lpVar.value.get

      if (value != 0.0 && value != 1.0) {
        nonIntegralSolutionsCounter += 1
        fractionalSolutions +:= id
      }
      else {
        val currentAtom = fetchAtom(id)
        currentAtom.fixedValue = if (value == 0.0) -1 else 1
        currentAtom.state = if (value == 0.0) false else true
        state.refineState(id)
      }
    }

    info("Number of non-integral solutions: " + nonIntegralSolutionsCounter)
    assert(state.countUnfixAtoms() == nonIntegralSolutionsCounter, "Variables introduced are less than actual ground atoms!")

    val sRoundUp = System.currentTimeMillis()

    if(nonIntegralSolutionsCounter > 0) {
      /*
       * RoundUp algorithm:
       *
       * Used for rounding non integral solutions produced by an LP relaxed
       * solution. It can have different results from original alchemy implementation
       * for several key reasons.
       *
       * 1. The solver return a solution before rounding takes place if there are more
       *    than one global optimus points in the objective function. In this case this
       *    points should yield equivalent solution in terms of quality
       *
       * 2. Loss of significance in alchemy during subtraction of doubles and long doubles
       *    (which have different precision) results in the phenomenon of catastrophic cancelation
       *    effect. Therefore delta can be significantly larger than zero.
       *
       * Note: Better to keep delta >= 0 for true values and < for false.
       */
      if(ilpRounding == RoundingScheme.ROUNDUP) optimize {

        for (i <- fractionalSolutions.size - 1 to 0 by -1) {
          val id = fractionalSolutions(i)
          val currentAtom = fetchAtom(id)
          if(state.computeDelta(id) >= 0) {
            currentAtom.fixedValue = 1
            currentAtom.state = true
          }
          else {
            currentAtom.fixedValue = -1
            currentAtom.state = false
          }
          state.refineState(id)
        }
      }
      // MaxWalkSAT algorithm
      else MaxWalkSAT(mrf).infer(state)
    }

    debug("Unfixed atoms: " + state.countUnfixAtoms())

    val eRoundUp = System.currentTimeMillis()
    info(msecTimeToText("Roundup time: ", eRoundUp - sRoundUp))

    state.printStatistics()
    info(msecTimeToText("Total ILP time: ", (eTranslation - sTranslation) +
                                                      (eSolver - sSolver) +
                                                      (eRoundUp - sRoundUp)
    ))

    state
  }

  /**
   * Write the results of inference into the selected output stream.
   *
   * @param out Selected output stream (default is console)
   */
  def writeResults(out: PrintStream = System.out) {


    val queryStartID = mln.space.queryStartID
    val queryEndID = mln.space.queryEndID

    val iterator = mrf.atoms.iterator()

    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key()

      if (atomID >= queryStartID && atomID <= queryEndID) {
        val groundAtom = iterator.value()
        val state = if(groundAtom.getState) 1 else 0

        atomID.decodeAtom match {
          case Success(txtAtom) if outputAll || state == 1 => out.println(txtAtom + " " + state)
          case Failure(f) => error(s"failed to decode id: $atomID", f)
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
  val GUROBI, LPSOLVE, OJALGO = Value
}
