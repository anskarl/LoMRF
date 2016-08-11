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
import lomrf.logic._
import lomrf.logic.AtomSignatureOps._
import lomrf.logic.PredicateCompletionMode._
import lomrf.logic.dynamic.{DynamicFunctionBuilder, DynamicAtomBuilder}
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference._
import lomrf.mln.model.MLN
import lomrf.util.ImplFinder

/**
 * Command line tool for inference.
 */
object InferenceCLI extends CLIApp {

  // The path to the input MLN file
  private var _mlnFileName: Option[String] = None

  // The path to the results file
  private var _resultsFileName: Option[String] = None

  // Input evidence file(s) (path)
  private var _evidenceFileNames: List[String] = Nil

  // The set of query atoms (in the form of AtomName/Arity)
  private var _queryAtoms = Set[AtomSignature]()

  // The set of closed-world assumption atoms (in the form of AtomName/Arity)
  private var _cwa = Set[AtomSignature]()

  // The set of open-world assumption atoms (in the form of AtomName/Arity)
  private var _owa = Set[AtomSignature]()

  // Perform marginal inference
  private var _marginalInference = true

  // Perform map inference using MaxWalkSAT
  private var _mws = true

  // MAP inference output type (all or positive only)
  private var _mapOutputAll = true

  // Trivially satisfy hard constrained unit clauses
  private var _satHardUnit = false

  // Satisfiability priority to hard constrained clauses
  private var _satHardPriority = false

  // Rounding algorithm for ILP map inference
  private var _ilpRounding = RoundingScheme.ROUNDUP

  // Solver used by ILP map inference
  private var _ilpSolver = Solver.LPSOLVE

  // Maximum number of samples to take
  private var _samples = 1000

  // The probability to perform a simulated annealing step.
  private var _pSA = 0.5

  // The probability to perform a greedy search.
  private var _pBest = 0.5

  // Temperature (0,1] for the simulated annealing step in MC-SAT.
  private var _saTemperature = 0.8

  // The maximum number of flips taken to reach a solution.
  private var _maxFlips = 1000000

  // The maximum number of attempts taken to find a solution.
  private var _maxTries = 1

  // Any possible world having cost below this threshold is considered as a solution.
  private var _targetCost = 0.0001

  // Minimum number of flips between flipping the same atom when using MaxWalkSAT.
  private var _tabuLength = 10

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

  // Eliminate negated unit clauses
  // For example:
  // 2 !A(x) becomes -2 A(x)
  private var _eliminateNegatedUnit = false

  // Perform unit-propagation (only for MC-SAT)
  private var _unitProp = true

  private var _implPaths: Option[Array[String]] = None

  //private var _domainPartition = false


  private def addQueryAtom(atom: String) {
    _queryAtoms += atom.signature.getOrElse(fatal(s"Cannot parse the arity of query atom: $atom"))
  }

  private def addCWA(atom: String) {
    _cwa += atom.signature.getOrElse(fatal(s"Cannot parse the arity of CWA atom: $atom"))
  }

  private def addOWA(atom: String) {
    _owa += atom.signature.getOrElse(fatal(s"Cannot parse the arity of OWA atom: $atom"))
  }

  opt("i", "input", "<path to mln file>", "Specify the path to the input knowledge base file", { v: String => _mlnFileName = Some(v) })

  opt("e", "evidence", "<path(s) to db file(s)>", "Specify the the paths of the evidence database files(comma separated, without spaces).", { v: String => _evidenceFileNames = v.split(',').toList })

  opt("r", "result", "<path to results file>", "Specify the path to the output results file", {
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

  opt("inferType", "inference-type", "<map | marginal>", "Specify the inference type: MAP or Marginal (default is marginal).", {
    v: String => v.trim.toLowerCase match {
      case "map" => _marginalInference = false
      case "marginal" => _marginalInference = true
      case _ => fatal("Unknown parameter for inference type '" + v + "'.")
    }
  })

  opt("mapType", "map-type", "<mws | ilp>", "Specify the MAP inference type: MaxWalkSAt or ILP (default is MaxWalkSAT).", {
    v: String => v.trim.toLowerCase match {
      case "ilp" => _mws = false
      case "mws" => _mws = true
      case _ => fatal("Unknown parameter for inference type '" + v + "'.")
    }
  })

  opt("mapOutput", "map-output-type", "<all | positive>", "Specify MAP inference output type: 0/1 results for all query atoms or " +
    "only positive query atoms (default is all).", {
    v: String => v.trim.toLowerCase match {
      case "all" => _mapOutputAll = true
      case "positive" => _mapOutputAll = false
      case _ => fatal("Unknown parameter for inference type '" + v + "'.")
    }
  })

  flagOpt("satHardUnit", "sat-hard-unit", "Trivially satisfy hard constrained unit clauses in MaxWalkSAT.", {
    _satHardUnit = true
  })

  flagOpt("satHardPriority", "sat-hard-priority", "Priority to hard constrained clauses in MaxWalkSAT.", {
    _satHardPriority = true
  })

  opt("ilpRounding", "ilp-rounding", "<roundup | mws>", "Specify either RoundUp (roundup) or MaxWalkSAT (mws) as " +
    "rounding algorithm to use for non-integral parts of the ILP solutions (default is RoundUp).", {
    v: String => v.trim.toLowerCase match {
      case "roundup" => _ilpRounding = RoundingScheme.ROUNDUP
      case "mws" => _ilpRounding = RoundingScheme.MWS
      case _ => fatal("Unknown parameter for ILP rounding type '" + v + "'.")
    }
  })

  opt("ilpSolver", "ilp-solver", "<lpsolve | ojalgo | gurobi>", "Specify which ILP solver use (default is LPSolve).", {
    v: String => v.trim.toLowerCase match {
      case "gurobi" => _ilpSolver = Solver.GUROBI
      case "lpsolve" => _ilpSolver = Solver.LPSOLVE
      case "ojalgo" => _ilpSolver = Solver.OJALGO
      case _ => fatal(s"Unknown parameter for ILP solver type '$v'.")
    }
  })

  intOpt("samples", "num-samples", "Specify the number of samples to take in MC-SAT (default is " + _samples + ").", _samples = _)

  doubleOpt("pSA", "probability-simulated-annealing", "Specify the probability to perform a simulated annealing step (default is " + _pSA + "), " +
    "when using MC-SAT for marginal inference.", {
    v: Double => if (v >= 1.0 || v <= 0.0) fatal("The pLocalSearch value must be between [0,1], but you gave: " + v) else _pSA = v
  })

  doubleOpt("pBest", "probability-best-search", "The probability to perform a greedy search (default is " + _pBest + "), " +
    "when using  MaxWalkSAT or MC-SAT.", {
    v: Double => if (v >= 1.0 || v <= 0.0) fatal("The pBest value must be between [0,1], but you gave: " + v) else _pBest = v
  })

  doubleOpt("saTemperature", "simulated-annealing-temperature", "Temperature (take values in (0,1]) for the simulated " +
    "annealing step in MC-SAT (default is " + _saTemperature + ").", {
    v: Double => if (v >= 1.0 || v <= 0.0) fatal("The saTemperature value must be between [0,1], but you gave: " + v) else _saTemperature = v
  })

  intOpt("maxFlips", "maximum-flips", "The maximum number of flips taken to reach a solution in MaxWalkSAT or MC-SAT (default is " + _maxFlips + ").", {
    v: Int => if (v < 0) fatal("The maxFlips value must be any integer above zero, but you gave: " + v) else _maxFlips = v
  })

  doubleOpt("targetCost", "target-cost", "In MaxWalkSAT and MC-SAT, any possible world having cost below this threshold " +
    "is considered as a solution (default is " + _targetCost + ").", {
    v: Double => if (v < 0) fatal("The targetCost value cannot be negative, you gave: " + v) else _targetCost = v
  })


  intOpt("maxTries", "maximum-tries", "The maximum number of attempts, in order to find a solution in MaxWalkSAT and " +
    "MC-SAT (default is " + _maxTries + ")", {
    v: Int => if (v < 0) fatal("The maxTries value must be any integer above zero, but you gave: " + v) else _maxTries = v
  })

  intOpt("numSolutions", "number-of-solutions", "Give the n-th solution in MC-SAT (default is " + _numSolutions + ").", {
    v: Int => if (v <= 0) fatal("The numSolutions value must be an integer above zero, but you gave: " + v) else _numSolutions = v
  })

  intOpt("tabuLength", "tabu-length", "Minimum number of flips between flipping the same ground atom in successive steps " +
    "in MaxWalkSAT and MC-SAT (default is " + _tabuLength + ").", {
    v: Int => if (v <= 0) fatal("The tabuLength value must be an integer above zero, but you gave: " + v) else _tabuLength = v
  })

  booleanOpt("unitProp", "use-unit-propagation", "Enable/disable unit propagation (default is " + _unitProp + ") in MC-SAT. " +
    "Performs unit propagation across the constraints in order to trivially satisfy as many as possible. When enabled, " +
    "the search space in MC-SAT is minimized, increases sampling performance and accuracy.", _unitProp = _)

  booleanOpt("lateSA", "late-simulated-annealing",
    "When enabled (= true), simulated annealing is performed only when MC-SAT reaches a plateau (i.e. a world with cost <= 'targetCost'). " +
      "Disabling lateSA (= false) causes MC-SAT to converge slower, since in every iteration simulated annealing is performed (with probability = 'pSA'). " +
      "By default lateSA is '" + _lateSA + "'", _lateSA = _)

  flagOpt("noNegWeights", "eliminate-negative-weights", "Eliminate negative weight values from ground clauses.", {
    _noNeg = true
  })

  flagOpt("noNegatedUnit", "eliminate-negated-unit", "Eliminate negated unit ground clauses.", {
    _eliminateNegatedUnit = true
  })

  opt("dynamic", "dynamic-implementations", "<string>", "Comma separated paths to search recursively for dynamic predicates/functions implementations (*.class and *.jar files).", {
    path: String => if (!path.isEmpty) _implPaths = Some(path.split(','))
  })

  // todo: partition the domain and create several smaller MLNs
  /*flagOpt("f:dpart", "flag:domain-partition", "Try to partition the domain and create several smaller MLNs.", {
    _domainPartition = true
  })*/

  flagOpt("v", "version", "Print LoMRF version.", sys.exit(0))

  flagOpt("h", "help", "Print usage options.", {
    println(usage)
    sys.exit(0)
  })

  if (args.length == 0) println(usage)
  else if (parse(args)) {

    // Check for query predicates
    if (_queryAtoms.isEmpty)
      fatal("You haven't specified any query predicates (see '-q' parameter).")

    // First load the KB and the evidence files
    val strMLNFileName = _mlnFileName.getOrElse(fatal("Please specify an input MLN file."))
    if (_evidenceFileNames.isEmpty) warn("You haven't specified any evidence file.")


    val resultsWriter = _resultsFileName match {
      case Some(fileName) => new PrintStream(new FileOutputStream(fileName), true)
      case None => System.out
    }

    info("Parameters:"
      + "\n\t(q) Query predicate(s): " + _queryAtoms.map(_.toString).mkString(", ")
      + "\n\t(cwa) Closed-world assumption predicate(s): " + (if (_cwa.isEmpty) "empty" else _cwa.map(_.toString).mkString(", "))
      + "\n\t(owa) Open-world assumption predicate(s): " + (if (_owa.isEmpty) "empty" else _owa.map(_.toString).mkString(", "))
      + "\n\t(marginal) Perform marginal inference: " + _marginalInference
      + "\n\t(mws) Perform MAP inference using MaxWalkSAT: " + _mws
      + "\n\t(all) Show 0/1 results for all query atoms: " + _mapOutputAll
      + "\n\t(satHardUnit) Trivially satisfy hard constrained unit clauses: " + _satHardUnit
      + "\n\t(satHardPriority) Satisfiability priority to hard constrained clauses: " + _satHardPriority
      + "\n\t(ilpRounding) Rounding algorithm used in ILP map inference: " + (if (_ilpRounding == RoundingScheme.ROUNDUP) "RoundUp" else "MaxWalkSAT")
      + "\n\t(ilpSolver) Solver used by ILP map inference: " + (if (_ilpSolver == Solver.GUROBI) "Gurobi" else "LPSolve")
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
      + "\n\t(noNegWeights) Eliminate negative weights: " + _noNeg
      + "\n\t(noNegatedUnit) Eliminate negated ground unit clauses: " + _eliminateNegatedUnit
      + "\n\t(unitProp) Perform unit-propagation: " + _unitProp
    )

    val dynamicDefinitionsOpt = _implPaths.map(paths => {
      val implFinder = ImplFinder(classOf[DynamicAtomBuilder], classOf[DynamicFunctionBuilder])
      implFinder.searchPaths(paths)
      implFinder.result
    })

    val mln = MLN.fromFile(strMLNFileName, _evidenceFileNames, _queryAtoms, _cwa, _owa, pcm = Decomposed, dynamicDefinitionsOpt)

    info(mln.toString)

    whenDebug {
      debug("List of CNF clauses: ")
      mln.clauses.zipWithIndex.foreach { case (c, idx) => debug(idx + ": " + c) }
    }

    if(mln.clauses.exists(_.weight.isNaN))
      fatal("Cannot perform inference, soft-constrained clauses with undefined weights found in the specified theory. " +
        "Please check the given MLN file(s) for soft-constrained formulas with missing weight values.")


    info("Creating MRF...")
    val mrfBuilder = new MRFBuilder(mln, noNegWeights = _noNeg, eliminateNegatedUnit = _eliminateNegatedUnit)
    val mrf = mrfBuilder.buildNetwork

    if (_marginalInference) {
      // Marginal inference methods
      val solver = new MCSAT(
        mrf, pBest = _pBest, pSA = _pSA, maxFlips = _maxFlips, maxTries = _maxTries, targetCost = _targetCost, numSolutions = _numSolutions,
        saTemperature = _saTemperature, samples = _samples, lateSA = _lateSA, unitPropagation = _unitProp, satHardPriority = _satHardPriority, tabuLength = _tabuLength
      )
      solver.infer()
      solver.writeResults(resultsWriter)
    }
    else {
      // MAP inference methods
      if (_mws) {
        val solver = new MaxWalkSAT(mrf, pBest = _pBest, maxFlips = _maxFlips, maxTries = _maxTries, targetCost = _targetCost,
          outputAll = _mapOutputAll, satHardUnit = _satHardUnit, satHardPriority = _satHardPriority, tabuLength = _tabuLength)
        solver.infer()
        solver.writeResults(resultsWriter)
      }
      else {
        val solver = new ILP(mrf, outputAll = _mapOutputAll, ilpRounding = _ilpRounding, ilpSolver = _ilpSolver)
        solver.infer()
        solver.writeResults(resultsWriter)
      }
    }
  }
}




