/*
 *
 *  o                        o     o   o         o
 *  |             o          |     |\ /|         | /
 *  |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 *  |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 *  O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *              |
 *           o--o
 *  o--o              o               o--o       o    o
 *  |   |             |               |    o     |    |
 *  O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 *  |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 *  o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 *  Logical Markov Random Fields (LoMRF).
 *
 *
 */

package lomrf.mln.inference

import lomrf.logic.TRUE
import lomrf.mln.model.{ AtomIdentityFunctionOps, EvidenceDB, MLN }
import lomrf.mln.model.mrf._
import lomrf.util.time._
import lomrf.util.logging.Implicits._
import AtomIdentityFunctionOps._
import lomrf.logic.AtomSignatureOps._
import gnu.trove.map.hash.TIntObjectHashMap
import optimus.algebra._
import optimus.optimization._
import optimus.algebra.AlgebraOps._
import scala.language.postfixOps
import lomrf.util.collection.trove.TroveConversions._
import lomrf.mln.inference.RoundingScheme.RoundUp
import optimus.optimization.enums.SolverLib.LpSolve
import optimus.optimization.enums.{ PreSolve, SolverLib }
import optimus.optimization.model.{ MPFloatVar, ModelSpec }
import spire.syntax.cfor._

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
  * @param ilpRounding Rounding algorithm selection option (default is RoundUp)
  * @param ilpSolver Solver type selection option (default is LPSolve)
  */
final case class ILP(
    mrf: MRF,
    ilpSolver: SolverLib = LpSolve,
    ilpRounding: RoundingScheme = RoundUp,
    annotationDB: Option[EvidenceDB] = None) extends ModelSpec(ilpSolver) with MAPSolver {

  implicit val mln: MLN = mrf.mln

  def infer: MRFState = {

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
    while (atomsIterator.hasNext) {
      atomsIterator.advance()
      val atomID = math.abs(atomsIterator.key())

      literalLPVars.put(atomID, MPFloatVar("y" + atomID, 0, 1))

      /* In case of loss augmented inference, Hamming distance is used which
       * is equivalent to adding 1 to the coefficient of ground atom y if the
       * true (annotated) value of y is FALSE and subtracting 1 from the
       * coefficient of y if the true value of y is TRUE.
       */
      if (annotationDB.isDefined) {
        logger.info("Running loss augmented inference...")
        val annotation = annotationDB.get(atomID.signature(mrf.mln))
        val loss = if (annotation(atomID) == TRUE) -1.0 else 1.0
        expressions ::= loss * literalLPVars.get(atomID)
      }
    }

    val constraintsIterator = mrf.constraints.iterator()

    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()

      var constraints: List[Expression] = Nil

      // fetch the current constraint, i.e., current weighted ground clause or clique
      val constraint = constraintsIterator.value()

      logger.whenDebugEnabled {
        val decodedConstraint = constraint.decodeFeature(mrf.weightHard).getOrElse(logger.fatal(s"Cannot decode constraint $constraint"))
        logger.debug(s"Ground Clause: ${constraint.getWeight} $decodedConstraint")
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

      logger.debug("Possible Constraints: [" + constraints.mkString(", ") + "]")

      val cid = constraint.id

      // Step 2: Create expressions for objective function (only for soft constraints)
      if (!constraint.getWeight.isInfinite && !constraint.getWeight.isNaN && constraint.getWeight != mrf.weightHard && constraint.getWeight != 0.0) {

        if (constraint.isUnit) {
          expressions ::= {
            if (constraint.literals(0) > 0) constraint.getWeight * literalLPVars.get(math.abs(constraint.literals(0)))
            else (-constraint.getWeight) * literalLPVars.get(math.abs(constraint.literals(0)))
          }
        } else { // there is no case where the same clause is going to create another z variable, so use put not putIfAbsent
          clauseLPVars.put(cid, MPFloatVar("z" + cid, 0, 1))
          expressions ::= math.abs(constraint.getWeight) * clauseLPVars.get(cid)
        }

      }

      logger.debug("Expressions: [" + expressions.mkString(", ") + "]")

      // Step 3: Add constraints to the solver (don't introduce constraint for zero weighted constraints)
      if (constraint.isHardConstraint) {
        add(sum(constraints) >:= 1)
        logger.debug(constraints.mkString(" + ") + " >= 1")
      } else if (!constraint.isUnit && constraint.getWeight != 0.0) {
        val clauseVar = clauseLPVars.get(cid)
        if (constraint.getWeight > 0) {
          add(sum(constraints) >:= clauseVar)
          logger.debug(constraints.mkString(" + ") + " >= " + clauseVar.symbol)
        } else {
          for (c <- constraints) {
            add(c >:= clauseVar)
            logger.debug(c + " >= " + clauseVar.symbol)
          }
        }
      }
    }

    val eTranslation = System.currentTimeMillis()
    logger.info(msecTimeToText("Translation time: ", eTranslation - sTranslation))

    logger.info(
      "\nGround Atoms: " + mrf.numberOfAtoms +
        "\nAtom Variables: " + literalLPVars.size + " + Clauses Variables: " + clauseLPVars.size +
        " = " + (literalLPVars.size + clauseLPVars.size))

    val sSolver = System.currentTimeMillis()

    // Step 4: Optimize function subject to the constraints introduced
    maximize(sum(expressions))
    start(PreSolve.CONSERVATIVE)
    release()

    val eSolver = System.currentTimeMillis()
    logger.info(msecTimeToText("Solver time: ", eSolver - sSolver))

    logger.info(
      "\n=========================== Solution ===========================" +
        "\nAre constraints satisfied: " + checkConstraints() +
        "\nSolution status: " + status.toString +
        "\nObjective = " + objectiveValue)

    logger.whenDebugEnabled {
      literalLPVars.iterator.foreach {
        case (_: Int, v: MPFloatVar) =>
          logger.debug(v.symbol + " = " + v.value.getOrElse("Value does not exist for this ground atom variable!"))
      }
      clauseLPVars.iterator.foreach {
        case (_: Int, v: MPFloatVar) =>
          logger.debug(v.symbol + " = " + v.value.getOrElse("Value does not exist for this constraint variable"))
      }
    }

    // Create MRF state and assume every constraint to be unsatisfied
    val state = MRFState(mrf)
    state.unfixAll()

    // Search for fractional solutions and fix atom values of non fractional solutions
    var nonIntegralSolutionsCounter = 0
    var fractionalSolutions = Vector[Int]()

    for ((id, lpVar) <- literalLPVars.iterator()) {
      val value = lpVar.value.getOrElse(logger.fatal(s"There is no solution for variable '${lpVar.symbol}'"))

      /*
       * Round values very close to 0 and 1 in using this naive approach because they
       * probably arise from rounding error of the solver.
       */
      val normalisedValue = if (value > 0.99) 1.0 else value

      if (normalisedValue != 0.0 && normalisedValue != 1.0) {
        nonIntegralSolutionsCounter += 1
        fractionalSolutions +:= id
      } else {
        val currentAtom = mrf.atoms.get(id)
        currentAtom.fixedValue = if (normalisedValue == 0.0) -1 else 1
        currentAtom.state = if (normalisedValue == 0.0) false else true
        state.refineState(id)
      }
    }

    logger.info("Number of non-integral solutions: " + nonIntegralSolutionsCounter)
    assert(state.countUnfixAtoms() == nonIntegralSolutionsCounter, "Variables introduced are less than actual ground atoms!")

    val sRoundUp = System.currentTimeMillis()

    if (nonIntegralSolutionsCounter > 0) {
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
      if (ilpRounding == RoundUp) {

        cfor(fractionalSolutions.size - 1)(_ <= 0, _ - 1){ i: Int =>
          val id = fractionalSolutions(i)
          val currentAtom = mrf.atoms.get(id)
          if (state.computeDelta(id) >= 0) {
            currentAtom.fixedValue = 1
            currentAtom.state = true
          } else {
            currentAtom.fixedValue = -1
            currentAtom.state = false
          }
          state.refineState(id)
        }
      } // MaxWalkSAT algorithm
      else MaxWalkSAT(mrf).infer(state)
    }

    logger.debug("Unfixed atoms: " + state.countUnfixAtoms())

    val eRoundUp = System.currentTimeMillis()
    logger.info(msecTimeToText("Roundup time: ", eRoundUp - sRoundUp))

    state.printStatistics()
    logger.info(msecTimeToText(
      "Total ILP time: ",
      (eTranslation - sTranslation) + (eSolver - sSolver) + (eRoundUp - sRoundUp)))

    state
  }
}
