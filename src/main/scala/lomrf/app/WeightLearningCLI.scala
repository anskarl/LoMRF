package lomrf.app

import lomrf.logic.AtomSignature
import lomrf.mln.inference.LossFunction
import lomrf.util.{Logging, OptionParser, parseAtomSignature}

/**
 * Command-line tool for weight learning
 *
 * @author Anastasios Skarlatidis
 * @author Vagelis Michelioudakis
 */
object WeightLearningCLI extends OptionParser with Logging {

  // The path to the input MLN file
  private var _mlnFileName: Option[String] = None

  // The path to the output MLN file
  private var _outputFileName: Option[String] = None

  // Input training file(s) (path)
  private var _trainingFileName: Option[String] = None

  // The set of non evidence atoms (in the form of AtomName/Arity)
  private var _nonEvidenceAtoms = Set[AtomSignature]()

  // Add unit clauses to the MLN output file
  private var _noAddUnitClauses = false

  // Write out MLNs after 1, 2, 5, 10, 20, 50, etc. iterations
  private var _periodicMLNs = false

  // Regularization parameter
  private var _C = 1e+3

  // Stopping parameter
  private var _epsilon = 0.001

  // Loss function
  private var _lossFunction = LossFunction.HAMMING

  // The loss value will be multiplied by this number
  private var _lossScale = 1.0

  // Perform loss augmented inference
  private var _lossAugmented = false

  // Don't scale the margin by the loss
  private var _nonMarginRescaling = false

  // Number of iterations to run learning method
  private var _iterations = 1000

  // Use L1 regularization instead of L2
  private var _L1Regularization = false

  // Print the learned weights for each iteration
  private var _printLearnedWeightsPerIteration = false

  private def addNonEvidenceAtom(atom: String) {
    parseAtomSignature(atom) match {
      case Some(s) => _nonEvidenceAtoms += s
      case None => fatal("Cannot parse the arity of query atom: " + atom)
    }
  }

  opt("i", "input", "<kb file>", "Markov Logic file", {
    v: String => _mlnFileName = Some(v)
  })

  opt("o", "output", "<output file>", "Output MLN file", {
    v: String => _outputFileName = Some(v)
  })

  opt("t", "training", "<training file>", "Training database file", {
    v: String => _trainingFileName = Some(v)
  })

  opt("ne", "non-evidence atoms", "<string>", "Comma separated non-evidence atoms. "
    + "Each atom must be defined using its identity (i.e. Name/arity). "
    + "For example the identity of NonEvidenceAtom(arg1,arg2) is NonEvidenceAtom/2", _.split(',').foreach(v => addNonEvidenceAtom(v)))

  booleanOpt("noAddUnitClauses", "no-add-unit-clauses", "If specified, unit clauses are not included in the output MLN file" +
    " (default is " + _noAddUnitClauses + ").", _noAddUnitClauses = _)

  booleanOpt("periodic", "periodic-MLNs", "Write out MLNs after 1, 2, 5, 10, 20, 50, etc. iterations." +
    " (default is " + _periodicMLNs + ")", _periodicMLNs = _)

  doubleOpt("C", "C", "Regularization parameter (default is " + _C + ").", {
    v: Double => _C = v
  })

  doubleOpt("epsilon", "epsilon", "Stopping parameter (default is " + _epsilon + ").", {
    v: Double => _epsilon = v
  })

  opt("lossFunction", "loss-function", "<hamming>", "Loss function (default is Hamming).", {
    v: String => v.trim.toLowerCase match {
      case "hamming" => _lossFunction = LossFunction.HAMMING
      case _ => fatal("Unknown parameter for ILP rounding type '" + v + "'.")
    }
  })

  doubleOpt("lossScale", "loss-scale", "The loss value will be multiplied by this number (default is " + _lossScale + ").", {
    v: Double => _lossScale = v
  })

  booleanOpt("lossAugmented", "loss-augmented", "Perform loss augmented inference." +
    " (default is " + _lossAugmented + ")", _lossAugmented = _)

  booleanOpt("nonMarginRescaling", "non-margin-rescaling", "Don't scale the margin by the loss." +
    " (default is " + _nonMarginRescaling + ")", _nonMarginRescaling = _)

  intOpt("iterations", "maximum-iterations", "The maximum number of iterations to run learning (default is " + _iterations + ").", {
    v: Int => if (v < 0) fatal("The maximum iterations value must be any integer above zero, but you gave: " + v) else _iterations = v
  })

  booleanOpt("L1Regularization", "L1-regularization", "Use L1 regularization instead of L2." +
    " (default is " + _L1Regularization + ")", _L1Regularization = _)

  booleanOpt("printLearnedWeightsPerIteration", "print-learned-weights-per-iteration", "Print the learned weights for each iteration." +
    " (default is " + _printLearnedWeightsPerIteration + ")", _printLearnedWeightsPerIteration = _)

  flagOpt("v", "version", "Print LoMRF version.", sys.exit(0))

  flagOpt("h", "help", "Print usage options.", {
    println(usage)
    sys.exit(0)
  })

  def main(args: Array[String]) {

    println(lomrf.ASCIILogo)
    println(lomrf.BuildVersion)

    if (args.length == 0) println(usage)
    else if (parse(args)) wlearn()
  }

  def wlearn() = ???
}


