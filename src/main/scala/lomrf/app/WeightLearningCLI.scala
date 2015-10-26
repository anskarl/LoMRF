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

package lomrf.app

import java.io.{FileOutputStream, PrintStream}
import lomrf.logic.AtomSignature
import lomrf.logic.AtomSignatureOps._
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.Solver
import lomrf.mln.learning.weight.{OnlineLearner, LossFunction, MaxMarginLearner}
import lomrf.mln.model.MLN
import lomrf.util.time._

/**
 * Command line tool for weight learning.
 */
object WeightLearningCLI extends CLIApp {

  // The path to the input MLN file
  private var _mlnFileName: Option[String] = None

  // The path to the output MLN file
  private var _outputFileName: Option[String] = None

  // Input training file(s) (path)
  private var _trainingFileName: Option[List[String]] = None

  // The set of non evidence atoms (in the form of AtomName/Arity)
  private var _nonEvidenceAtoms = Set[AtomSignature]()

  // Algorithm type for learning
  private var _algorithm = Algorithm.MAX_MARGIN

  // Solver used by ILP map inference
  private var _ilpSolver = Solver.LPSOLVE

  // Add unit clauses to the MLN output file
  private var _addUnitClauses = false

  // Sigma parameter for strongly convex functions (should be positive)
  private var _sigma = 1.0

  // Lambda regularization parameter for ADAGRAD
  private var _lambda = 0.01

  // Eta learning rate parameter for ADAGRAD
  private var _eta = 1.0

  // Delta parameter for ADAGRAD (should be positive or equal zero)
  private var _delta = 1.0

  // Regularization parameter
  private var _C = 1e+3

  // Stopping parameter
  private var _epsilon = 0.001

  // Loss function
  private val _lossFunction = LossFunction.HAMMING

  // The loss value will be multiplied by this number
  private var _lossScale = 1.0

  // Perform loss augmented inference
  private var _lossAugmented = true

  // Don't scale the margin by the loss
  private var _nonMarginRescaling = false

  // Number of iterations to run learning method
  private var _iterations = 1000

  // Use L1 regularization instead of L2
  private var _L1Regularization = false

  // Print the learned weights for each iteration
  private var _printLearnedWeightsPerIteration = false

  // Eliminate negative weights, i.e. convert the clause:
  // -2 A(x) v B(x)
  // into the following two clauses:
  // 1 !A(x)
  // 1 !B(x)
  private var _noNeg = false

  // Eliminate negated unit clauses
  // For example:
  // 2 !A(x) becomes -2 A(x)
  private var _eliminateNegatedUnit = false

  private var _implPaths: Option[Array[String]] = None


  opt("i", "input", "<kb file>", "Markov Logic file", {
    v: String => _mlnFileName = Some(v)
  })

  opt("o", "output", "<output file>", "Output MLN file", {
    v: String => _outputFileName = Some(v)
  })

  opt("t", "training", "<training file | folder>", "Training database file", {
    v: String =>
      val file = new java.io.File(v)
      if(file.isDirectory) _trainingFileName = Some(file.listFiles().filter(file => file.getName.matches(".*[.]db")).map(file => file.getPath).toList)
      else _trainingFileName = Some(v.split(',').toList)
  })

  opt("ne", "non-evidence atoms", "<string>", "Comma separated non-evidence atoms. "
    + "Each atom must be defined using its identity (i.e. Name/arity). "
    + "For example the identity of NonEvidenceAtom(arg1,arg2) is NonEvidenceAtom/2", {
      _nonEvidenceAtoms ++= _.split(',').map(s => s.signature.getOrElse(fatal(s"Cannot parse the arity of atom signature: $s")))
  })

  opt("alg", "algorithm", "<MAX_MARGIN | CDA | ADAGRAD>", "Algorithm used to perform learning (default is Max-Margin).", {
    v: String => v.trim.toLowerCase match {
      case "max_margin" => _algorithm = Algorithm.MAX_MARGIN
      case "cda" => _algorithm = Algorithm.COORDINATE_DUAL_ASCEND
      case "adagrad" => _algorithm = Algorithm.ADAGRAD_FB
      case _ => fatal("Unknown parameter for learning algorithm type '" + v + "'.")
    }
  })

  doubleOpt("sigma", "sigma", "Parameter for strong convexity in CDA (default is " + _sigma + ").", {
    v: Double => if (v <= 0) fatal("Sigma value must be any number above zero, but you gave: " + v) else _sigma = v
  })

  doubleOpt("lambda", "lambda", "Regularization parameter for ADAGRAD (default is " + _lambda + ").", {
    v: Double => _lambda = v
  })

  doubleOpt("eta", "eta", "Learning rate parameter for ADAGRAD (default is " + _eta + ").", {
    v: Double => _eta = v
  })

  doubleOpt("delta", "delta", "Delta parameter for ADAGRAD (default is " + _delta + ").", {
    v: Double => if(v < 0) fatal("Delta value must be any number greater or equal to zero, but you gave: " + v) else _delta = v
  })

  doubleOpt("C", "C", "Regularization parameter (default is " + _C + ").", {
    v: Double => _C = v
  })

  doubleOpt("epsilon", "epsilon", "Stopping parameter (default is " + _epsilon + ").", {
    v: Double => _epsilon = v
  })

  doubleOpt("lossScale", "loss-scale", "The loss value will be multiplied by this number (default is " + _lossScale + ").", {
    v: Double => _lossScale = v
  })

  intOpt("iterations", "maximum-iterations", "The maximum number of iterations to run learning (default is " + _iterations + ").", {
    v: Int => if (v < 0) fatal("The maximum iterations value must be any integer above zero, but you gave: " + v) else _iterations = v
  })

  flagOpt("addUnitClauses", "add-unit-clauses", "If specified, unit clauses are included in the output MLN file.", {_addUnitClauses = true})

  flagOpt("L1Regularization", "L1-regularization", "Use L1 regularization instead of L2.", {_L1Regularization = true})

  flagOpt("printLearnedWeightsPerIteration", "print-learned-weights-per-iteration", "Print the learned weights for each iteration.", { _printLearnedWeightsPerIteration = true})

  opt("ilpSolver", "ilp-solver", "<lpsolve | ojalgo | gurobi >", "Solver used by ILP (default is lpsolve).", {
    v: String => v.trim.toLowerCase match {
      case "gurobi" => _ilpSolver = Solver.GUROBI
      case "lpsolve" => _ilpSolver = Solver.LPSOLVE
      case "ojalgo" => _ilpSolver = Solver.OJALGO
      case _ => fatal("Unknown parameter for ILP solver type '" + v + "'.")
    }
  })

  flagOpt("lossAugmented", "loss-augmented", "Perform loss augmented inference.", {_lossAugmented = true})

  flagOpt("nonMarginRescaling", "non-margin-rescaling", "Don't scale the margin by the loss.", {_nonMarginRescaling = true})

  opt("dynamic", "dynamic-implementations", "<string>", "Comma separated paths to search recursively for dynamic predicates/functions implementations (*.class and *.jar files).", {
    path: String => if (!path.isEmpty) _implPaths = Some(path.split(','))
  })

  flagOpt("noNegWeights", "eliminate-negative-weights", "Eliminate negative weight values from ground clauses.", {_noNeg = true})

  flagOpt("noNegatedUnit", "eliminate-negated-unit", "Eliminate negated unit ground clauses.", {_eliminateNegatedUnit = true})

  flagOpt("v", "version", "Print LoMRF version.", sys.exit(0))

  flagOpt("h", "help", "Print usage options.", {
    println(usage)
    sys.exit(0)
  })

  def weightLearn() = {

    val strMLNFileName = _mlnFileName.getOrElse(fatal("Please specify an input MLN file."))
    val strTrainingFileNames = _trainingFileName.getOrElse(fatal("Please specify input training file(s)."))

    val outputWriter = _outputFileName match {
      case Some(fileName) => new PrintStream(new FileOutputStream(fileName), true)
      case None => System.out
    }

    info("Parameters:"
      + "\n\t(ne) Non-evidence predicate(s): " + _nonEvidenceAtoms.map(_.toString).reduceLeft((left, right) => left + "," + right)
      + "\n\t(addUnitClauses) Include unit clauses in the output MLN file: " + _addUnitClauses
      + "\n\t(sigma) Parameter for strong convexity in CDA: " + _sigma
      + "\n\t(lambda) Regularization parameter for ADAGRAD: " + _lambda
      + "\n\t(eta) Learning rate parameter for ADAGRAD: " + _eta
      + "\n\t(delta) Delta parameter for ADAGRAD: " + _delta
      + "\n\t(C) Soft-margin regularization parameter: " + _C
      + "\n\t(epsilon) Stopping criterion parameter: " + _epsilon
      + "\n\t(lossScale) Scale the loss value by: " + _lossScale
      + "\n\t(iterations) Number of iterations for learning: " + _iterations
      + "\n\t(L1Regularization) Use L1 regularization instead of L2: " + _L1Regularization
      + "\n\t(printLearnedWeightsPerIteration) Print learned weights for each iteration: " + _printLearnedWeightsPerIteration
      + "\n\t(ilpSolver) Solver used by ILP map inference: " + ( if(_ilpSolver == Solver.GUROBI) "Gurobi" else "LPSolve")
      + "\n\t(algorithm) Algorithm used to perform learning: " + ( if(_algorithm == Algorithm.MAX_MARGIN) "Max-Margin"
                                                                   else if(_algorithm == Algorithm.COORDINATE_DUAL_ASCEND) "Cordinate Dual Ascent"
                                                                   else "ADAGRAD_FB" )
      + "\n\t(lossAugmented) Perform loss augmented inference: " + _lossAugmented
      + "\n\t(nonMarginRescaling) Don't scale the margin by the loss: " + _nonMarginRescaling
      + "\n\t(noNegWeights) Eliminate negative weights: " + _noNeg
      + "\n\t(noNegatedUnit) Eliminate negated ground unit clauses: " + _eliminateNegatedUnit
    )

    if(_algorithm == Algorithm.MAX_MARGIN) {

      if(_ilpSolver != Solver.GUROBI) {
        warn("For MAX_MARGIN training, only GUROBI solver is supported. Switching to GUROBI.")
        _ilpSolver = Solver.GUROBI
      }

      val (mln, annotationDB) = MLN.forLearning(strMLNFileName, strTrainingFileNames, _nonEvidenceAtoms, addUnitClauses = _addUnitClauses)

      mlnInfo(mln)

      info("AnnotationDB: "
        + "\n\tAtoms with annotations: " + annotationDB.keys.map(_.toString).reduceLeft((left, right) => left + "," + right)
      )

      info("Number of CNF clauses = " + mln.clauses.size)
      info("List of CNF clauses: ")
      mln.clauses.zipWithIndex.foreach { case (c, idx) => info(idx + ": " + c)}

      info("Creating MRF...")
      val mrfBuilder = new MRFBuilder(mln, noNegWeights = _noNeg, eliminateNegatedUnit = _eliminateNegatedUnit, createDependencyMap = true)
      val mrf = mrfBuilder.buildNetwork

      val learner = new MaxMarginLearner(mrf = mrf, annotationDB = annotationDB, nonEvidenceAtoms = _nonEvidenceAtoms, iterations = _iterations,
                                        ilpSolver = _ilpSolver, C = _C, epsilon = _epsilon, lossScale = _lossScale,
                                        nonMarginRescaling = _nonMarginRescaling, lossAugmented = _lossAugmented, lossFunction = _lossFunction,
                                        L1Regularization = _L1Regularization, printLearnedWeightsPerIteration = _printLearnedWeightsPerIteration)

      learner.learn()
      learner.writeResults(outputWriter)
    }
    else {

      var learner: OnlineLearner = null
      val start = System.currentTimeMillis()

      for (step <- strTrainingFileNames.indices) {

        val (mln, annotationDB) = MLN.forLearning(strMLNFileName, List(strTrainingFileNames(step)), _nonEvidenceAtoms, addUnitClauses = _addUnitClauses)

        mlnInfo(mln)

        info("AnnotationDB: "
          + "\n\tAtoms with annotations: " + annotationDB.keys.mkString(","))

        info("Number of CNF clauses = " + mln.clauses.size)
        whenDebug{
          debug("List of CNF clauses: ")
          mln.clauses.zipWithIndex.foreach { case (c, idx) => debug(idx + ": " + c)}
        }

        info("Creating MRF...")
        val mrfBuilder = new MRFBuilder(mln, noNegWeights = _noNeg, eliminateNegatedUnit = _eliminateNegatedUnit, createDependencyMap = true)
        val mrf = mrfBuilder.buildNetwork

        if(step == 0) learner = new OnlineLearner(mln = mln, algorithm = _algorithm, lossAugmented = _lossAugmented,
                                                  ilpSolver = _ilpSolver, lossScale = _lossScale, sigma = _sigma, lambda = _lambda, eta = _eta,
                                                  delta = _delta, printLearnedWeightsPerIteration = _printLearnedWeightsPerIteration)

        info("Step " + (step + 1) + ": " + strTrainingFileNames(step))
        learner.learningStep(step + 1, mrf, annotationDB)
      }

      info(msecTimeToTextUntilNow("Total learning time: ", start))
      learner.writeResults(outputWriter)
    }

  }

  private def mlnInfo(mln: MLN): Unit ={
    info("Markov Logic:"
      + "\n\tConstant domains   : " + mln.evidence.constants.size
      + "\n\tSchema definitions : " + mln.schema.predicates.size
      + "\n\tClauses            : " + mln.clauses.size
      + "\n\tEvidence atoms     : " + mln.cwa.mkString(",")
      + "\n\tNon-evidence atoms : " + mln.owa.mkString(","))
  }

  // Main:
  if (args.length == 0) println(usage)
  else if (parse(args)) weightLearn()
}

/**
 * Object holding constants for learning algorithm type.
 */
object Algorithm extends Enumeration {
  type Algorithm = Value
  val ADAGRAD_FB = Value
  val COORDINATE_DUAL_ASCEND = Value
  val MAX_MARGIN = Value
}

