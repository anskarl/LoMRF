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

package lomrf.app

import java.io._
import lomrf.logic.{ AtomSignature, Clause }
import lomrf.mln.learning.structure.ClauseConstructor.ClauseType
import lomrf.mln.learning.structure.ModeParser
import lomrf.mln.model.KB
import lomrf.mln.learning.structure._
import lomrf.logic.AtomSignatureOps._
import lomrf.mln.learning.TrainingEvidence
import lomrf.util.NaturalComparator
import lomrf.util.time._
import lomrf.util.logging.Implicits._
import optimus.optimization.enums.SolverLib

/**
  * Command line tool for structure learning
  */
object StructureLearningCLI extends CLIApp {

  // The path to the input MLN file
  private var _mlnFileName: Option[String] = None

  // The path to the output MLN file
  private var _outputFileName: Option[String] = None

  // Input training file(s) (path)
  private var _trainingFileNames: Option[Array[String]] = None

  // The set of non evidence atoms (in the form of AtomName/Arity)
  private var _nonEvidenceAtoms = Set[AtomSignature]()

  // The set of template atoms used to construct initial paths and perform predicate completion
  private var _templateAtoms = Set[AtomSignature]()

  // The path to the input MLN file
  private var _modesFileName: Option[String] = None

  // Maximum length of paths produced by relational pathfinding
  private var _maxLength: Int = 8

  // Allow clauses to have free variables
  private var _allowFreeVariables = false

  // Threshold for evaluation of new clauses
  private var _threshold: Int = 1

  // Tolerance threshold for discarding clauses having poor weights at the end of learning
  private var _theta: Double = 0

  // Clause types to be produced in clause creation
  private var _clauseType = ClauseType.BOTH

  // Solver used by ILP map inference
  private var _ilpSolver: SolverLib = SolverLib.LpSolve

  // Perform loss augmented inference
  private var _lossAugmented = false

  // Initial weight value for discovered clauses
  private var _initialWeight = 0.01

  // Lambda regularization parameter for AdaGrad
  private var _lambda = 0.01

  // Eta learning rate parameter for AdaGrad
  private var _eta = 1.0

  // Delta parameter for AdaGrad (should be positive or equal zero)
  private var _delta = 1.0

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

  opt("i", "input", "<kb file>", "Markov Logic file", {
    v: String => _mlnFileName = Some(v)
  })

  opt("o", "output", "<output file>", "Output MLN file", {
    v: String => _outputFileName = Some(v)
  })

  opt("t", "training", "<training file | folder>", "Training database file", {
    v: String =>
      val file = new java.io.File(v)
      if (file.isDirectory) _trainingFileNames =
        Some(file.listFiles().filter(file => file.getName.matches(".*[.]db")).map(file => file.getPath))
      else _trainingFileNames = Some(v.split(','))
  })

  opt("m", "modes", "<mode file>", "Mode declarations file", {
    v: String => _modesFileName = Some(v)
  })

  opt("ne", "non-evidence atoms", "<string>", "Comma separated non-evidence atoms. "
    + "Each atom must be defined using its identity (i.e. Name/arity). "
    + "For example the identity of NonEvidenceAtom(arg1,arg2) is NonEvidenceAtom/2", {
    _nonEvidenceAtoms ++= _.split(',').map(s => s.signature.getOrElse(logger.fatal(s"Cannot parse the arity of atom signature: $s")))
  })

  opt("template", "template-atoms", "<string>", "Comma separated template atoms. "
    + "Each atom must be defined using its identity (i.e. Name/arity). "
    + "For example the identity of TemplateAtom(arg1,arg2) is TemplateAtom/2", {
    _templateAtoms ++= _.split(',').map(s => s.signature.getOrElse(logger.fatal(s"Cannot parse the arity of atom signature: $s")))
  })

  intOpt("maxLength", "max-length", "The maximum length of literals for each clause produced (default is " + _maxLength + ").", {
    v: Int => if (v < 0) logger.fatal("The maximum length of literals must be any integer above zero, but you gave: " + v) else _maxLength = v
  })

  flagOpt("allowFreeVariables", "allow-free-variables", "Allow clauses to have free variables.", { _allowFreeVariables = true })

  intOpt("threshold", "threshold", "Evaluation threshold for each new clause produced (default is " + _threshold + ").", {
    v: Int => if (v < 0) logger.fatal("The evaluation threshold must be any integer above zero, but you gave: " + v) else _threshold = v
  })

  doubleOpt("theta", "tolerance-theta", "Tolerance theta threshold for discarding clauses having poor weights at the end of learning (default is " + _theta + ").", {
    v: Double => if (v < 0) logger.fatal("Theta threshold must be any integer above zero, but you gave: " + v) else _theta = v
  })

  opt("clauseType", "clause-type", "<horn | conjunction | both>", "Type of clauses to be produced (default is both).", {
    v: String =>
      v.trim.toLowerCase match {
        case "horn"        => _clauseType = ClauseType.HORN
        case "conjunction" => _clauseType = ClauseType.CONJUNCTION
        case "both"        => _clauseType = ClauseType.BOTH
        case _             => logger.fatal(s"Unknown parameter for clause type '$v'.")
      }
  })

  opt("ilpSolver", "ilp-solver", "<lpsolve | ojalgo | gurobi | mosek>", "Solver used by ILP (default is LPSolve).", {
    v: String =>
      v.trim.toLowerCase match {
        case "gurobi"  => _ilpSolver = SolverLib.Gurobi
        case "lpsolve" => _ilpSolver = SolverLib.LpSolve
        case "ojalgo"  => _ilpSolver = SolverLib.oJSolver
        case "mosek"   => _ilpSolver = SolverLib.Mosek
        case _         => logger.fatal(s"Unknown parameter for ILP solver type '$v'.")
      }
  })

  flagOpt("lossAugmented", "loss-augmented", "Perform loss augmented inference.", { _lossAugmented = true })

  doubleOpt("initialWeight", "initial-weight", "Initial weight value for discovered clauses (default is " + _initialWeight + ").", {
    v: Double => _initialWeight = v
  })

  doubleOpt("lambda", "lambda", "Regularization parameter for ADAGRAD (default is " + _lambda + ").", {
    v: Double => _lambda = v
  })

  doubleOpt("eta", "eta", "Learning rate parameter for ADAGRAD (default is " + _eta + ").", {
    v: Double => _eta = v
  })

  doubleOpt("delta", "delta", "Delta parameter for ADAGRAD (default is " + _delta + ").", {
    v: Double => if (v < 0) logger.fatal("Delta value must be any number greater or equal to zero, but you gave: " + v) else _delta = v
  })

  flagOpt("printLearnedWeightsPerIteration", "print-learned-weights-per-iteration", "Print the learned weights for each iteration.", {
    _printLearnedWeightsPerIteration = true
  })

  flagOpt("noNegWeights", "eliminate-negative-weights", "Eliminate negative weight values from ground clauses.", { _noNeg = true })

  flagOpt("noNegatedUnit", "eliminate-negated-unit", "Eliminate negated unit ground clauses.", { _eliminateNegatedUnit = true })

  flagOpt("v", "version", "Print LoMRF version.", sys.exit(0))

  flagOpt("h", "help", "Print usage options.", {
    println(usage)
    sys.exit(0)
  })

  private def structLearn(): Unit = {

    // Clauses found across all learning steps
    var learnedClauses = Vector[Clause]()

    val strMLNFileName = _mlnFileName.getOrElse(logger.fatal("Please specify an input MLN file."))

    val strTrainingFileNames = _trainingFileNames
      .map(_.sortWith(NaturalComparator.compareBool))
      .getOrElse(logger.fatal("Please specify input training file(s)."))

    val strModeFileName = _modesFileName.getOrElse(logger.fatal("Please specify an input mode declaration file."))

    val outputWriter = _outputFileName match {
      case Some(fileName) =>
        if (fileName == strMLNFileName)
          logger.fatal(s"Output file '${fileName}' cannot be the same with input MLN '${strMLNFileName} file'")

        new PrintStream(new FileOutputStream(fileName), true)
      case None => System.out
    }

    logger.info("Parameters:"
      + "\n\t(ne) Non-evidence predicate(s): " + _nonEvidenceAtoms.map(_.toString).reduceLeft((left, right) => left + "," + right)
      + "\n\t(maxLength) Maximum length of literals for each clause produced: " + _maxLength
      + "\n\t(allowFreeVariables) Allow clauses to have free variables: " + _allowFreeVariables
      + "\n\t(threshold) Evaluation threshold for each new clause produced: " + _threshold
      + "\n\t(theta) Tolerance threshold for discarding clauses having poor weights: " + _theta
      + "\n\t(clauseType) Type of clauses to be produced: " + (if (_clauseType == ClauseType.HORN) "Horn" else if (_clauseType == ClauseType.CONJUNCTION) "Conjunction" else "Both")
      + "\n\t(ilpSolver) Solver used by ILP map inference: " + _ilpSolver
      + "\n\t(lossAugmented) Perform loss augmented inference: " + _lossAugmented
      + "\n\t(initialWeight) Initial weight value for discovered clauses: " + _initialWeight
      + "\n\t(lambda) Regularization parameter for AdaGrad: " + _lambda
      + "\n\t(eta) Learning rate parameter for AdaGrad: " + _eta
      + "\n\t(delta) Delta parameter for AdaGrad: " + _delta
      + "\n\t(noNegWeights) Eliminate negative weights: " + _noNeg
      + "\n\t(noNegatedUnit) Eliminate negated ground unit clauses: " + _eliminateNegatedUnit)

    // Parse all mode declarations from file
    val modes = ModeParser.parseFrom(new File(strModeFileName))
    logger.info("Modes Declarations: \n" + modes.map(pair => "\t" + pair._1 + " -> " + pair._2).reduce(_ + "\n" + _))

    if (_templateAtoms.isEmpty) {

      val (kb, constants) = KB.fromFile(strMLNFileName, convertFunctions = true)

      val learner = OSL(kb, constants, _nonEvidenceAtoms,
                        modes, _maxLength, _allowFreeVariables,
                        _threshold, _clauseType, _ilpSolver,
                        _lossAugmented, _lambda, _eta, _delta,
                        _printLearnedWeightsPerIteration)

      val start = System.currentTimeMillis()

      for (step <- strTrainingFileNames.indices) {

        logger.info(s"Step ${step + 1} / ${strTrainingFileNames.length}: Processing chunk ${strTrainingFileNames(step)}")

        val trainingEvidence = TrainingEvidence.fromPath(kb, constants, _nonEvidenceAtoms, strTrainingFileNames(step))

        learnedClauses ++= learner.reviseTheory(trainingEvidence)

        logger.info(s"At the end of step ${step + 1}, we have learned ${learnedClauses.length} clauses")
      }

      logger.info(msecTimeToTextUntilNow("Total learning time: ", start))
      learner.writeResults(outputWriter)
    } else {

      val (kb, constants) = KB.fromFile(strMLNFileName, convertFunctions = true)

      val learner = OSLa(kb, constants, _nonEvidenceAtoms,
                         _templateAtoms, modes, _maxLength,
                         _allowFreeVariables, _threshold, _theta,
                         _ilpSolver, _lossAugmented, _initialWeight, _lambda,
                         _eta, _delta, _printLearnedWeightsPerIteration)

      val start = System.currentTimeMillis()

      for (step <- strTrainingFileNames.indices) {

        logger.info(s"Step ${step + 1} / ${strTrainingFileNames.length}: Processing chunk ${strTrainingFileNames(step)}")

        val trainingEvidence = TrainingEvidence.fromPath(kb, constants, _nonEvidenceAtoms, strTrainingFileNames(step))

        learnedClauses = learner.reviseTheory(trainingEvidence)

        logger.info(s"At the end of step ${step + 1}, we have learned ${learnedClauses.length} clauses")
      }

      logger.info(msecTimeToTextUntilNow("Total learning time: ", start))
      learner.writeResults(outputWriter)
    }
  }

  // Main:

  if (args.length == 0) println(usage)
  else if (parse(args)) structLearn()
}
