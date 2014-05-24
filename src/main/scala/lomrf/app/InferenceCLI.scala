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

package lomrf.app

import java.io.{FileOutputStream, PrintStream}
import lomrf.logic.AtomSignature
import lomrf.logic.PredicateCompletionMode._
import lomrf.logic.dynamic.{DynamicFunctionBuilder, DynamicAtomBuilder}
import lomrf.mln.inference._
import lomrf.mln.model.MLN
import lomrf.util.{OptionParser, Logging, ImplFinder, parseAtomSignature}
import scala.Some

/**
 * Command-line tool for inference.
 *
 * @author Anastasios Skarlatidis
 */
object InferenceCLI extends OptionParser with Logging {

  // The path to the input MLN file
  private var _mlnFileName: Option[String] = None

  // The path to the results file
  private var _resultsFileName: Option[String] = None

  // Input evidence file(s) (path)
  private var _evidenceFileNames: Option[List[String]] = None

  // The set of query atoms (in the form of AtomName/Arity)
  private var _queryAtoms = Set[AtomSignature]()

  // The set of closed-world assumption atoms (in the form of AtomName/Arity)
  private var _cwa = Set[AtomSignature]()

  // The set of open-world assumption atoms (in the form of AtomName/Arity)
  private var _owa = Set[AtomSignature]()

  // Perform marginal inference
  private var _marginalInference = true

  // Maximum number of samples to take
  private var _samples = 1000

  // The probability to perform a simulated annealing step.
  private var _pSA = 0.1

  // The probability to perform a greedy search.
  private var _pBest = 0.5

  // Temperature (0,1] for the simulated annealing step in MC-SAT.
  private var _saTemperature = 0.1

  // The maximum number of flips taken to reach a solution.
  private var _maxFlips = 100000

  // The maximum number of attempts taken to find a solution.
  private var _maxTries = 1

  // Any possible world having cost below this threshold is considered as a solution.
  private var _targetCost = 0.0001

  // Minimum number of flips between flipping the same atom when using MaxWalkSAT.
  private var _tabuLength = 5

  // Give the n-th solution (i.e. cost < target cost) in MC-SAT.
  private var _numSolutions = 10

  // Enable/disable late simulated annealing. When enabled, simulated annealing
  // is performed only when MC-SAT reaches a plateau. Disabling lateSA causes
  // MC-SAT to converge slower.
  private var _lateSA = true

  // Eliminate negative weights, i.e. convert the clause:
  // -2 A(x) v B(x)
  // into the following two clauses:
  // 1 !A(x)
  // 1 !B(x)
  private var _noNeg = false

  // Perform unit-propagation (only for MC-SAT)
  private var _unitProp = true

  private var _implPaths: Option[Array[String]] = None

  private var _domainPartition = false


  private def addQueryAtom(atom: String) {
    parseAtomSignature(atom) match {
      case Some(s) => _queryAtoms += s
      case None => fatal("Cannot parse the arity of query atom: " + atom)
    }
  }

  private def addCWA(atom: String) {
    parseAtomSignature(atom) match {
      case Some(s) => _cwa += s
      case None => fatal("Cannot parse the arity of CWA atom: " + atom)
    }
  }

  private def addOWA(atom: String) {
    parseAtomSignature(atom) match {
      case Some(s) => _owa += s
      case None => fatal("Cannot parse the arity of OWA atom: " + atom)
    }
  }

  opt("i", "input", "<kb file>", "Markov Logic file", {
    v: String => _mlnFileName = Some(v)
  })
  opt("e", "evidence", "<db file(s)>", "Comma separated evidence database files.", {
    v: String => _evidenceFileNames = Some(v.split(',').toList)
  })
  opt("r", "result", "<result file>", "Results file", {
    v: String => _resultsFileName = Some(v)
  })


  opt("q", "query", "<string>", "Comma separated query atoms. "
    + "Each atom must be defined using its identity (i.e. Name/arity). "
    + "For example the identity of QueryAtom(arg1,arg2) is QueryAtom/2", _.split(',').foreach(v => addQueryAtom(v)))

  opt("cwa", "closed-world-assumption", "<string>",
    "Specified non-evidence atoms (comma-separated without white-spaces) are closed world, otherwise, all non-evidence atoms are open world."
      + "Each atom must be defined using its identity (i.e. Name/arity, see the description of -q for an example)", _.split(",").foreach(v => addCWA(v)))

  opt("owa", "open-world-assumption", "<string>",
    "Specified evidence atoms (comma-separated without white-spaces) are open world, while other evidence atoms are closed-world. " +
      "Each atom must be defined using its identity (i.e. Name/arity, see the description of -q for an example)", _.split(",").foreach(v => addOWA(v)))



  opt("infer", "inference-type", "<map | marginal>", "Specify the inference type: MAP or Marginal (default is marginal).", {
    v: String => v.trim.toLowerCase match {
      case "map" => _marginalInference = false
      case "marginal" => _marginalInference = true
      case _ => fatal("Unknown parameter for inference type '" + v + "'.")
    }
  })

  intOpt("samples", "num-samples", "Number of samples to take (default is " + _samples + ").", _samples = _)

  doubleOpt("pSA", "probability-simulated-annealing", "Specify the probability to perform a simulated annealing step (default is " + _pSA + "), " +
    "when using MC-SAT for marginal inference.", {
    v: Double => if (v >= 1.0 || v <= 0.0) fatal("The pLocalSearch value must be between [0,1], but you gave: " + v) else _pSA = v
  })

  doubleOpt("pBest", "probability-best-search", "The probability to perform a greedy search (default is " + _pBest + ").", {
    v: Double => if (v >= 1.0 || v <= 0.0) fatal("The pBest value must be between [0,1], but you gave: " + v) else _pBest = v
  })

  doubleOpt("saTemperature", "simulated-annealing-temperature", "Temperature (take values in [0,1]) for the simulated annealing step in MC-SAT (default is " + _saTemperature + ").", {
    v: Double => if (v >= 1.0 || v <= 0.0) fatal("The saTemperature value must be between [0,1], but you gave: " + v) else _saTemperature = v
  })

  intOpt("maxFlips", "maximum-flips", "The maximum number of flips taken to reach a solution (default is " + _maxFlips + ").", {
    v: Int => if (v < 0) fatal("The maxFlips value must be any integer above zero, but you gave: " + v) else _maxFlips = v
  })

  doubleOpt("targetCost", "target-cost", "Any possible world having cost below this threshold is considered as a solution (default is " + _targetCost + ").", {
    v: Double => if (v < 0) fatal("The targetCost value cannot be negative, you gave: " + v) else _targetCost = v
  })


  intOpt("maxTries", "maximum-tries", "The maximum number of attempts, in order to find a solution (default is " + _maxTries + ")", {
    v: Int => if (v < 0) fatal("The maxTries value must be any integer above zero, but you gave: " + v) else _maxTries = v
  })

  intOpt("numSolutions", "number-of-solutions", "Give the n-th solution in MC-SAT (default is " + _numSolutions + ").", {
    v: Int => if (v <= 0) fatal("The numSolutions value must be an integer above zero, but you gave: " + v) else _numSolutions = v
  })

  intOpt("tabuLength", "tabu-length", "Minimum number of flips between flipping the same ground atom in successive MaxWalkSAT steps (default is " + _tabuLength + ").", {
    v: Int => if (v <= 0) fatal("The tabuLength value must be an integer above zero, but you gave: " + v) else _tabuLength = v
  })

  booleanOpt("unitProp", "use-unit-propagation", "Enable/disable unit propagation (default is " + _unitProp + ") in MC-SAT.", _unitProp = _)

  booleanOpt("lateSA", "late-simulated-annealing",
    "When enabled (= true), simulated annealing is performed only when MC-SAT reaches a plateau (i.e. a world with cost <= 'targetCost'). " +
      "Disabling lateSA (= false) causes MC-SAT to converge slower, since in every iteration simulated annealing is performed (with probability = 'pSA'). " +
      "By default lateSA is '" + _lateSA + "'", _lateSA = _)

  booleanOpt("noNeg", "eliminate-negative-weights", "Eliminate negative weight values from ground clauses (default is " + _noNeg + ").", _noNeg = _)

  opt("dynamic", "dynamic-implementations", "<string>", "Comma separated paths to search recursively for dynamic predicates/functions implementations (*.class and *.jar files).", {
    path: String => if (!path.isEmpty) _implPaths = Some(path.split(','))
  })

  flagOpt("h", "help", "Print usage options.", {
    println(usage)
    sys.exit(0)
  })

  flagOpt("v", "version", "Print LoMRF version.", sys.exit(0))

  flagOpt("f:dpart", "flag:domain-partition", "Try to partition the domain and create several smaller MLNs.", {
    _domainPartition = true
  })

  def main(args: Array[String]) {

    println(lomrf.ASCIILogo)
    println(lomrf.BuildVersion)

    if (args.length == 0) println(usage)
    else if (parse(args)) infer()
  }

  def infer() {
    //First load the KB and the evidence files
    val strMLNFileName = _mlnFileName.getOrElse(fatal("Please specify an input MLN file."))
    val strEvidenceFileNames = _evidenceFileNames.getOrElse(fatal("Please specify input evidence file(s)."))
    val resultsWriter = _resultsFileName match {
      case Some(fileName) => new PrintStream(new FileOutputStream(fileName), true)
      case None => System.out
    }

    info("Parameters:"
      + "\n\t(q) Query predicate(s): " + _queryAtoms.map(_.toString).reduceLeft((left, right) => left + "," + right)
      + "\n\t(cwa) Closed-world assumption predicate(s): " + (if (_cwa.isEmpty) "empty" else _cwa.map(_.toString).reduceLeft((left, right) => left + "," + right))
      + "\n\t(owa) Open-world assumption predicate(s): " + (if (_owa.isEmpty) "empty" else _owa.map(_.toString).reduceLeft((left, right) => left + "," + right))
      + "\n\t(marginal) Perform marginal inference: " + _marginalInference
      + "\n\t(samples) Number of samples to take: " + _samples
      + "\n\t(pSA) Probability to perform simulated annealing: " + _pSA
      + "\n\t(pBest) Probability to perform a greedy search: " + _pBest
      + "\n\t(saTemperature) Temperature for the simulated annealing step: " + _saTemperature
      + "\n\t(maxFlips) Maximum number of flips: " + _maxFlips
      + "\n\t(maxTries) Maximum number of attempts: " + _maxTries
      + "\n\t(targetCost) Target cost: " + _targetCost
      + "\n\t(tabuLength) Minimum number of flips between flipping the same atom: " + _tabuLength
      + "\n\t(numSolutions) Number of solutions in MC-SAT: " + _numSolutions
      + "\n\t(lateSA) Simulated annealing is performed only when MC-SAT reaches a plateau: " + _lateSA
      + "\n\t(noNeg) Eliminate negative weights: " + _noNeg
      + "\n\t(unitProp) Perform unit-propagation: " + _unitProp
    )

    val mln =
      _implPaths match {
        case Some(paths) =>
          val implFinder = ImplFinder(classOf[DynamicAtomBuilder], classOf[DynamicFunctionBuilder])
          implFinder.searchPaths(paths)
          MLN(strMLNFileName, strEvidenceFileNames, _queryAtoms, _cwa, _owa, pcm = Decomposed, dynamicDefinitions = Some(implFinder.result), domainPart =_domainPartition)
        case None => MLN(strMLNFileName, strEvidenceFileNames, _queryAtoms, _cwa, _owa, pcm = Decomposed, dynamicDefinitions = None, domainPart =_domainPartition)
      }


    info("Markov Logic:"
      + "\n\tConstant domains   : " + mln.constants.size
      + "\n\tSchema definitions : " + mln.schema.size
      + "\n\tFormulas           : " + mln.formulas.size)

    info("CNF clauses = " + mln.clauses.size)
    info("Creating MRF...")
    val mrfBuilder = new MRFBuilder(mln, _noNeg)
    val mrf = mrfBuilder.buildNetwork

    if (_marginalInference) {
      val solver = new MCSAT(
        mrf, pBest = _pBest, pSA = _pSA, maxFlips = _maxFlips, maxTries = _maxTries, targetCost = _targetCost, numSolutions = _numSolutions,
        saTemperature = _saTemperature, samples = _samples, lateSA = _lateSA, unitPropagation = _unitProp
      )
      solver.infer()
      solver.writeResults(resultsWriter)
    }
    else {
      val solver = new MaxWalkSAT(mrf, pBest = _pBest, maxFlips = _maxFlips, maxTries = _maxTries, targetCost = _targetCost)
      solver.infer()
      solver.writeResults(resultsWriter)
    }
  }
}




