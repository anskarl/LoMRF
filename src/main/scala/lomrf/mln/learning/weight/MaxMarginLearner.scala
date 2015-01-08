package lomrf.mln.learning.weight

import lomrf.mln.inference.{Solver, ILP, LossFunction}
import lomrf.util._
import java.io.PrintStream
import gnu.trove.map.hash.TIntObjectHashMap
import lomrf.logic.{FALSE, TriState, TRUE, AtomSignature}
import lomrf.util.{AtomEvidenceDB, Logging}
import lomrf.mln.model.mrf._
import optimus.lqprog._
import optimus.algebra._

/**
 * This is an implementation of max-margin weight learning algorithm for parameter estimation in Markov Logic Networks.
 * The original implementation of the algorithm can be found in: [[http://http://alchemy.cs.washington.edu]].
 * Details about the max-margin algorithm for MLNs can be found in the following publications:
 *
 * <ul>
 * <li> Tuyen N. Huynh and Raymond J. Mooney. Max-Margin Weight Learning for Markov Logic Networks (2009)
 * In Proceedings of the European Conference on Machine Learning and Principles and Practice of Knowledge Discovery
 * in Databases, Part 1, pp. 564--579, Bled, Slovenia, September 2009.
 * The paper can be found in [[http://www.cs.utexas.edu/users/ai-lab/?huynh:srl09]]
 * </li>
 * </ul>
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
final class MaxMarginLearner(mrf: MRF, annotationDB: Map[AtomSignature, AtomEvidenceDB],
                             dependencyMap: TIntObjectHashMap[TIntObjectHashMap[(Int, Boolean)]],
                             nonEvidenceAtoms: Set[AtomSignature], iterations: Int = 1000, C: Double = 1e+3, epsilon: Double = 0.001,
                             lossFunction: Int = LossFunction.HAMMING, lossScale: Double = 1.0, nonMarginRescaling: Boolean = false,
                             lossAugmented: Boolean = false, ilpSolver: Int = Solver.GUROBI,
                             printLearnedWeightsPerIteration: Boolean = false) extends Logging {

  // Select the appropriate mathematical programming solver
  implicit val problem =
    if(ilpSolver == Solver.GUROBI)
      new LQProblem(SolverLib.gurobi)
    else
      new LQProblem(SolverLib.lp_solve)

  // Number of first-order clauses
  val numberOfClauses = mrf.mln.clauses.length

  val numberOfExamples = mrf.atoms.size()

  // Initialise weights of the first-order clauses (default 1)
  val weights = Array.fill[Double](numberOfClauses)(1.0)

  // Create an mrf state
  val state = MRFState(mrf)

  // ILP solver for inference
  val solver = new ILP(mrf, annotationDB = annotationDB, lossAugmented = lossAugmented)

  /**
   * Fetch atom given its literal code.
   *
   * @param literal Code of the literal
   * @return The ground atom which corresponds to the given literal code
   */
  @inline private def fetchAtom(literal: Int) = mrf.atoms.get(math.abs(literal))

  /**
   * Fetch annotation from database for the given atom id. Annotation
   * exist only for non evidence atoms.
   *
   * @param atomID id of the atom
   * @return annotation TriState value (TRUE, FALSE or UNKNOWN)
   */
  @inline private def getAnnotation(atomID: Int): TriState = {
    val annotation = annotationDB(signatureOf(atomID)(mrf.mln))
    annotation(atomID)
  }

  @inline private def countGroundings(): Array[Int] = {

    val counts = Array.fill[Int](numberOfClauses)(0)

    info("Init counts: " + counts.deep.mkString(" , "))

    val constraintIterator = mrf.constraints.iterator()

    // Keeps the count of literals satisfying the current constraint
    var nsat = 0

    // literal index of the current constraint
    var idx = 0

    while (constraintIterator.hasNext) {
      constraintIterator.advance()
      val currentConstraint = constraintIterator.value()

      // --- Compute the number of literals that satisfy the current constraint
      nsat = 0 // Reset
      idx = 0 // Reset
      while (idx < currentConstraint.literals.length) {
        val lit = currentConstraint.literals(idx)
        if ( (lit > 0) == fetchAtom(lit).state ) nsat += 1
        idx += 1
      }

      val iterator = dependencyMap.get(currentConstraint.id).iterator()

      while(iterator.hasNext) {
        iterator.advance()
        val clauseIdx = iterator.key()
        val (frequency, invertedWts) = iterator.value()
        if( (invertedWts && nsat <= 0) || (!invertedWts && nsat > 0) )
          counts(clauseIdx) += frequency
      }

      // --- --- --- --- --- --- --- --- --- ---
    }
    counts
  }

  @inline private def setAnnotatedState() = {

    val atomsIterator = mrf.atoms.iterator()

    while(atomsIterator.hasNext) {
      atomsIterator.advance()
      val atom = atomsIterator.value()
      if (getAnnotation(atom.id) == TRUE) atom.state = true
      else atom.state = false
    }
  }

  // Set the annotation as current state and count true groundings
  setAnnotatedState()
  val trueCounts = countGroundings()

  info("True Counts: [" + trueCounts.deep.mkString(", ") + "]")

  //state.evaluateState() // seem useless, se will see!

  // currently working only for Hamming loss
  @inline private def calculateLoss(): Double = {
    var totalLoss = 0.0

    info("Calculating misclassifed loss...")

    val iterator = mrf.atoms.iterator() // there are all ground atoms possible here or not?
    while(iterator.hasNext) {
      iterator.advance()
      val atom = iterator.value()
      val annotation = getAnnotation(atom.id)
      if( (atom.state && annotation == FALSE) || (!atom.state && annotation == TRUE) )
        totalLoss += 1.0
    }

    info("Total inferred error: " + totalLoss + "/" + numberOfExamples)
    totalLoss
  }

  // update constraint weights from the sum of parent weights
  @inline private def updateConstraintWeights() = {

    val constraints = mrf.constraints.iterator()
    while(constraints.hasNext) {
      constraints.advance()
      val constraint = constraints.value()
      val iterator = dependencyMap.get(constraint.id).iterator()

      constraint.weight = 0.0
      while(iterator.hasNext) {
        iterator.advance()
        val clauseIdx = iterator.key()
        val tuple = iterator.value()
        if(tuple._2) constraint.weight -= weights(clauseIdx) * tuple._1
        else constraint.weight += weights(clauseIdx) * tuple._1
      }

    }
  }

  @inline private def infer() = {
    updateConstraintWeights()
    solver.infer()
    // save as low state maybe
  }

  def learn() = {

    info("2-norm max margin weight learning using cutting plane method: \n" +
         "Number of weights: " + numberOfClauses)

    var error = 1e+5
    var slack = -1e+5
    var iteration = 1

    val LPVars = new TIntObjectHashMap[MPFloatVar]()
    var expressions = List[Expression]()
    var constraints = List[Expression]()

    // Step 1: Introduce variables for each first-order clause weight and one slack variable
    // Step 2: Create sub-expressions for objective function (quadratic problem)
    for(clauseIdx <- 0 until numberOfClauses) {
      LPVars.putIfAbsent(clauseIdx, MPFloatVar("w" + clauseIdx, Double.NegativeInfinity, Double.PositiveInfinity))
      expressions :+= ( LPVars.get(clauseIdx) * LPVars.get(clauseIdx) )
    }
    LPVars.putIfAbsent(numberOfClauses, MPFloatVar("slack")) // bounds are by default [0.0, inf]
    expressions ::= ( C * LPVars.get(numberOfClauses) )

    // Step 3: Sum the sub-expressions to create the final objective
    val objective = sum(expressions)

    while( error > (slack + epsilon) || iteration <= iterations) {

      info("Iteration: " + iteration + "/" + iterations)

      // We should try and learn the parameters on the 2nd iteration, first we must perform inference
      if(iteration > 1) {

        info("Running solver for the current QP problem...")

        // Optimize function subject to the constraints introduced
        minimize(objective)
        start()
        //release()

        info(
            "\n=========================== Solution ===========================" +
            "\nAre constraints satisfied: " + checkConstraints() +
            "\nSolution status: " + status.toString +
            "\nObjective = " + objectiveValue)

        // Check for convergence and terminate learning if required
        var converged = true
        var nonZero = 0
        for (clauseIdx <- 0 until numberOfClauses) {
          val value = LPVars.get(clauseIdx).value.get

          // set learned weights before inference
          if(weights(clauseIdx) != value) {
            weights(clauseIdx) = value
            converged = false
          }
        }

        nonZero = weights.count(w => w != 0.0)
        info("Non-zero weights: " + nonZero)

        val value = LPVars.get(numberOfClauses).value.get
        if(slack != value) slack = value

        info("Current slack value: " + slack)

        // Print learned weights so far
        if (printLearnedWeightsPerIteration) {
          info("Learned weights on iteration " + iteration + ":\n" +
            "[" + weights.deep.mkString(", ") + "]")
        }

        if (converged) iteration = iterations + 1
      }

      info("Running inference...")
      infer()
      info("Done")

      val loss = (calculateLoss() / numberOfExamples) * lossScale
      info("Current loss: " + loss)

      info("Count inferred counts")
      val inferredCounts = countGroundings()
      info("Inferred Counts: [" + inferredCounts.deep.mkString(", ") + "]")

      // Calculate true counts minus inferred counts
      var currentError = 0.0
      var delta = Array[Int](numberOfClauses)
      for (clauseIdx <- 0 until numberOfClauses) {
        if(!mrf.mln.clauses(clauseIdx).isHard)
          delta :+= (trueCounts(clauseIdx) - inferredCounts(clauseIdx)) / numberOfExamples
        currentError += weights(clauseIdx) * delta(clauseIdx)
      }

      constraints = Nil
      // add new constraints !!!! for all -> problem
      for(clauseIdx <- 0 until numberOfClauses)
        constraints ::= LPVars.get(clauseIdx) * delta(clauseIdx)
      // should be merged above!
      println(sum(constraints) >= loss - LPVars.get(numberOfClauses))
      add(sum(constraints) >= loss - LPVars.get(numberOfClauses))

      // update error for termination condition
      if (nonMarginRescaling) {
        if (loss > 0) error = 1 - currentError
        else error = 0.0
      }
      else
        error = loss - currentError

      info("Current error: " + error + "\n" +
        "Current stopping criteria: " + (slack + epsilon))

      iteration += 1
    }
  }

  def writeResults(out: PrintStream = System.out) = ???
}
