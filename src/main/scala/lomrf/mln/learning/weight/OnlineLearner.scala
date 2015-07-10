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

package lomrf.mln.learning.weight

import java.io.PrintStream
import java.text.DecimalFormat
import auxlib.log.Logging
import lomrf.app.Algorithm
import lomrf.app.Algorithm.Algorithm
import lomrf.logic.{TriState, FALSE, TRUE, AtomSignature}
import lomrf.logic.AtomSignatureOps._
import lomrf.mln.inference.{Solver, ILP}
import lomrf.mln.inference.Solver._
import lomrf.mln.model.{AtomEvidenceDB, MLN}
import lomrf.mln.model.mrf.MRF
import lomrf.util._
import scalaxy.streams.optimize
import scala.language.postfixOps

/**
 * This is an implementation of online weight learning algorithms for parameter estimation in Markov Logic Networks.
 * Details about the online algorithm for MLNs can be found in the following publications:
 *
 * <ul>
 * <li> Tuyen N. Huynh and Raymond J. Mooney. Online Max-Margin Weight Learning for Markov Logic Networks (2011)
 * In Proceedings of the Eleventh SIAM International Conference on Data Mining (SDM11) pp. 642--651, Mesa, Arizona, USA, April 2011.
 * The paper can be found in [[http://www.cs.utexas.edu/users/ai-lab/?huynh:sdm11]]
 * </li>
 * <li> Duchi J. Hazan E. and Singer Y. Adaptive Subgradient Methods for Online Learning and Stochastic Optimization (2011)
 * The Journal of Machine Learning Research, Vol. 12, pp. 2121--2159, July 2011
 * The paper can be found in [[http://jmlr.org/papers/v12/duchi11a.html]]
 * </li>
 * </ul>
 *
 * @param mln Markov logic network of the current problem
 * @param algorithm Algorithm type selection option for online optimization
 * @param lossAugmented Use loss augmented inference
 * @param lossScale Loss scaling parameter
 * @param ilpSolver Solver type selection option for ILP inference (default is LPSolve)
 * @param sigma Sigma parameter for strongly convex functions (should be positive)
 * @param lambda Regularization parameter for ADAGRAD
 * @param eta Learning rate parameter for ADAGRAD
 * @param delta Delta parameter for ADAGRAD (should be positive or equal zero)
 * @param printLearnedWeightsPerIteration Print learned weights for each iteration
 *
 */
final class OnlineLearner(mln: MLN, algorithm: Algorithm, lossAugmented: Boolean = false, lossScale: Double = 1.0,
                          ilpSolver: Solver = Solver.LPSOLVE, sigma: Double = 1.0, lambda: Double = 0.01, eta: Double = 1.0,
                          delta: Double = 1.0, printLearnedWeightsPerIteration: Boolean = false) extends Logging {

  algorithm match {
    case Algorithm.ADAGRAD_FB => info("ADAGRAD method Fobos")
    case Algorithm.COORDINATE_DUAL_ASCEND => info("Coordinate dual ascend method for strongly convex function")
  }

  // Number of first-order CNF clauses
  val numberOfClauses = mln.clauses.length

  // Initialise weights of the first-order clauses to zero (useless)
  val weights = Array.fill[Double](numberOfClauses)(0.0)

  val sumSquareGradients = Array.fill[Int](numberOfClauses)(0)

  /**
   * Fetch annotation from database for the given atom id. Annotation
   * exist only for non evidence atoms.
   *
   * @param atomID id of the atom
   * @return annotation TriState value (TRUE, FALSE or UNKNOWN)
   */
  @inline private def getAnnotation(atomID: Int, mrf: MRF, annotationDB: Map[AtomSignature, AtomEvidenceDB]): TriState = {
    val annotation = annotationDB(atomID.signature(mrf.mln))
    annotation(atomID)
  }

  /**
   * Set the annotated state as current MRF state.
   */
  @inline private def setAnnotatedState(mrf: MRF, annotationDB: Map[AtomSignature, AtomEvidenceDB]) = {

    val atomsIterator = mrf.atoms.iterator()

    while(atomsIterator.hasNext) {
      atomsIterator.advance()
      val atom = atomsIterator.value()
      if (getAnnotation(atom.id, mrf, annotationDB) == TRUE) atom.state = true
      else atom.state = false
    }
  }

  /**
   * Count the number of true groundings of each clause in the data.
   * In order to do this, we compute the satisfied literals of each
   * ground clause given an MRF state (annotation or inferred). Then
   * if the number of satisfied literals are greater than zero and
   * the weight of the clause that produced it has not been flipped
   * we can count it as true ground clause. On the other hand if the
   * weight has been flipped then in order to count it the number of
   * satisfied literals should be zero.
   *
   * @return count of true groundings of the clauses
   */
  @inline private def countGroundings(mrf: MRF): Array[Int] = {

    val dependencyMap = mrf.dependencyMap.getOrElse(sys.error("Dependency map does not exists."))

    val counts = Array.fill[Int](numberOfClauses)(0)

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
        if ( (lit > 0) == mrf.fetchAtom(lit).state ) nsat += 1
        idx += 1
      }

      val iterator = dependencyMap.get(currentConstraint.id).iterator()

      while(iterator.hasNext) {
        iterator.advance()
        val clauseIdx = iterator.key()
        val frequency = iterator.value()

        // If weight is flipped then we want to count the opposite type of grounding
        // Use math abs because frequency may be negative to indicate a weight is flipped
        if( (frequency < 0 && nsat == 0) || (frequency > 0 && nsat > 0) )
          counts(clauseIdx) += math.abs(frequency.toInt) // Clauses cannot have float frequencies at this point!
      }

      // --- --- --- --- --- --- --- --- --- ---
    }
    counts
  }

  /**
   * Calculates the total error of inferred atom truth values. The
   * result is not divided by the number of examples.
   *
   * Currently working only for Hamming loss
   *
   * @return the total error
   */
  @inline private def calculateError(mrf: MRF, annotationDB: Map[AtomSignature, AtomEvidenceDB]): Double = {
    var totalError = 0.0
    val numberOfExamples = mrf.atoms.size()

    info("Calculating misclassifed loss...")

    val iterator = mrf.atoms.iterator()
    while(iterator.hasNext) {
      iterator.advance()
      val atom = iterator.value()
      val annotation = getAnnotation(atom.id, mrf, annotationDB)
      if( (atom.state && annotation == FALSE) || (!atom.state && annotation == TRUE) )
        totalError += 1.0
    }

    info("Total inferred error: " + totalError + "/" + numberOfExamples)
    totalError
  }

  /**
   * Update constraint weights from the sum of newly found parent
   * weights in order to reconstruct the ground network faster in
   * order to run inference.
   *
   * Basic reconstruction steps:
   *
   * For each clause that produced the constaint do:
   *    If weight has been inverted then:
   *      multiply the clause weight learned so far and the number of times (frequency)
   *      this clause produced the corresponding constraint and subtract the result from
   *      the total weight of the constraint so far.
   *
   *    If weight has not been inverted then:
   *      Do the same but add the result to the total weight instead of subtract it.
   *
   * Note: In case the clause is hard then just assign the hard weight to the constraint.
   */
  @inline private def updateConstraintWeights(mrf: MRF) = {

    val dependencyMap = mrf.dependencyMap.getOrElse(sys.error("Dependency map does not exists."))

    val constraints = mrf.constraints.iterator()

    while(constraints.hasNext) {
      constraints.advance()
      val constraint = constraints.value()
      val iterator = dependencyMap.get(constraint.id).iterator()

      constraint.setWeight(0.0)
      while(iterator.hasNext) {
        iterator.advance()

        val clauseIdx = iterator.key()
        val frequency = iterator.value()

        // Frequency would never be negative because we always start using positive unit weights
        if(mrf.mln.clauses(clauseIdx).isHard) constraint.setWeight(mrf.weightHard)
        else constraint.setWeight(constraint.getWeight + weights(clauseIdx) * frequency)
      }
    }
  }

  /**
   * Reconstruct the ground network without running the grounding procedure
   * and then perform inference using the ILP solver.
   */
  @inline private def infer(mrf: MRF, annotationDB: Map[AtomSignature, AtomEvidenceDB]) = {
    updateConstraintWeights(mrf)
    val solver = new ILP(mrf, annotationDB = annotationDB, lossAugmented = lossAugmented, ilpSolver = ilpSolver)
    solver.infer()
  }

  def learningStep(t: Int, mrf: MRF, annotationDB: Map[AtomSignature, AtomEvidenceDB]) = {

    // Set the annotated state and count true groundings
    setAnnotatedState(mrf, annotationDB)
    val trueCounts = countGroundings(mrf)
    info("True Counts: [" + trueCounts.deep.mkString(", ") + "]")

    // Perform inference for the current weight vector and count true groundings
    infer(mrf, annotationDB)
    val inferredCounts = countGroundings(mrf)
    info("Inferred Counts: [" + inferredCounts.deep.mkString(", ") + "]")

    var weightedDeltaPhi = 0.0

    if(algorithm == Algorithm.COORDINATE_DUAL_ASCEND) {

      // Set default learning rate
      val defaultLearningRate = 1.0 / (sigma * t)

      val error = calculateError(mrf, annotationDB) * lossScale
      info("Current loss: " + error)

      // Calculate true counts minus inferred counts
      val delta = Array.fill[Int](numberOfClauses)(0)
      optimize{
        for (clauseIdx <- 0 until numberOfClauses) {
          if (!mrf.mln.clauses(clauseIdx).isHard)
            delta(clauseIdx) = trueCounts(clauseIdx) - inferredCounts(clauseIdx)
          weightedDeltaPhi += weights(clauseIdx) * delta(clauseIdx)
        }
      }
      val deltaPhi = delta.map( dif => Math.abs(dif) ).sum

      info("\nDelta = [" + delta.deep.mkString(", ") + "]" +
           "\nAbsolute delta phi: " + deltaPhi +
           "\nWeighted delta phi: " + weightedDeltaPhi)

      var loss = error - weightedDeltaPhi
      if(loss < 0) loss = 0.0

      val square2NormDeltaPhi = delta.map( dif => dif * dif).sum

      val learningRate = if(square2NormDeltaPhi <= 0) 0.0
      else Math.min(defaultLearningRate, (error - ((t - 1) * weightedDeltaPhi) / t) / square2NormDeltaPhi)

      info("Learning rate: " + learningRate)

      // Update weights
      optimize {
        for (clauseIdx <- 0 until numberOfClauses)
          weights(clauseIdx) = (t - 1) * weights(clauseIdx) / t + learningRate * delta(clauseIdx)
      }
    }
    else if(algorithm == Algorithm.ADAGRAD_FB) {

      // Compute subgradients for all clausese
      val subgradients = Array.fill[Int](numberOfClauses)(0)
      optimize {
        for (clauseIdx <- 0 until numberOfClauses) {
          if (!mrf.mln.clauses(clauseIdx).isHard)
            subgradients(clauseIdx) = inferredCounts(clauseIdx) - trueCounts(clauseIdx)
          weightedDeltaPhi += weights(clauseIdx) * subgradients(clauseIdx)
        }
      }

      var clauseIdx = 0
      while (clauseIdx < numberOfClauses) {

        sumSquareGradients(clauseIdx) += subgradients(clauseIdx) * subgradients(clauseIdx)

        val coefficient = eta / (this.delta + math.sqrt(sumSquareGradients(clauseIdx)))
        val value = weights(clauseIdx) - coefficient * subgradients(clauseIdx)
        val difference = math.abs(value) - (lambda * coefficient)

        if (difference > 0)
          weights(clauseIdx) = if (value >= 0) difference else -difference
        else weights(clauseIdx) = 0.0

        clauseIdx += 1
      }

      /*for (clauseIdx <- 0 until numberOfClauses) {
        sumSquareGradients(clauseIdx) += subgradients(clauseIdx) * subgradients(clauseIdx)

        val coefficient = eta / (this.delta + math.sqrt(sumSquareGradients(clauseIdx)))
        val value = weights(clauseIdx) - coefficient * subgradients(clauseIdx)
        val difference = math.abs(value) - (lambda * coefficient)

        if (difference > 0)
          weights(clauseIdx) = if (value >= 0) difference else -difference
        else weights(clauseIdx) = 0.0
      }*/
    }

    if (printLearnedWeightsPerIteration) {
      info("Learned weights on step " + t + ":\n" +
        "[" + weights.deep.mkString(", ") + "]")
    }
  }

  /**
   * Write clauses and their corresponding learned weights in the output
   * mln file together with the predicate and function schema if any exists.
   *
   * @param out Selected output stream (default is console)
   */
  def writeResults(out: PrintStream = System.out) = {

    val numFormat = new DecimalFormat("0.############")

    out.println("// Predicate definitions")
    for ((signature, args) <- mln.schema.predicates) {
      val line = signature.symbol + (
        if (args.isEmpty) "\n"
        else "(" + args.mkString(",") + ")\n")
      out.print(line)
    }

    if(mln.schema.functions.nonEmpty) {
      out.println("\n// Functions definitions")
      for ((signature, (retType, args)) <- mln.schema.functions) {
        val line = retType + " " + signature.symbol + "(" + args.mkString(",") + ")\n"
        out.print(line)
      }
    }

    val clauses = mln.clauses
    out.println("\n// Clauses")
    for(clauseIdx <- clauses.indices) {
      if(clauses(clauseIdx).isHard)
        out.println(clauses(clauseIdx).literals.map(_.toText).mkString(" v ") + ".\n")
      else out.println(numFormat.format(weights(clauseIdx)) + " " + clauses(clauseIdx).literals.map(_.toText).mkString(" v ") + "\n")
    }
  }

}