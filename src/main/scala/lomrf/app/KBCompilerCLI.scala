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

import java.io.PrintStream
import lomrf.app.WeightsMode._
import lomrf.logic._
import lomrf.logic.compile._
import lomrf.logic.compile.PredicateCompletionMode._
import lomrf.logic.dynamic.{ DynamicAtomBuilder, DynamicFunctionBuilder }
import lomrf.mln.model.KB
import lomrf.util.ImplFinder
import lomrf.util.logging.Implicits._
import scala.annotation.tailrec

/**
  * Command line tool for knowledge compilation.
  *
  * @note The tool can perform predicate completion, CNF transformation,
  *        FOL function conversions, as well as weight eliminations.
  */
object KBCompilerCLI extends CLIApp {

  // The path to the input MLN file
  var mlnFileName: Option[String] = None

  // The path to the output file
  var outputMLNFileName: Option[String] = None

  // Function prefix for transformation of functions to predicates
  var functionPrefix: String = lomrf.AUX_PRED_PREFIX

  // Eliminate functions
  var eliminateFunctions: Boolean = false

  // Introduce functions
  var introduceFunctions: Boolean = false

  // Include domain definition
  var includeDomain: Boolean = false

  // Include predicate definitions
  var removePredicateDefinitions: Boolean = false

  // Include function definitions
  var removeFunctionDefinitions: Boolean = false

  // Weights mode for output (keep, remove soft or remove all)
  var weightsMode: WeightsMode = Keep

  // Predicate completion mode
  var pcm: PredicateCompletionMode = Simplification

  // Convert formulas into CNF
  var cnf: Boolean = false

  // Path to dynamic implementations
  var implPaths: Option[Array[String]] = None

  opt("i", "input", "<mln file>", "Input Markov Logic file", {
    v: String => mlnFileName = Some(v)
  })

  opt("o", "output", "<mln file>", "Output Markov Logic file", {
    v: String => outputMLNFileName = Some(v)
  })

  flagOpt("cnf", "clausal-form", "Convert formulas to clausal form", {
    cnf = true
  })

  opt("w", "weights", "<keep | remove_soft | remove_all>",
    "(keep) Keep all given weights, " +
      "or (remove_soft) eliminate the weighs from all soft-constrained formulas, " +
      "or (remove_all) convert all formulas to soft-constrained without weights. " +
      "Please note, that in some cases the weights cannot be kept (e.g. simplified predicate completion).", {
      v: String =>
        weightsMode = v.trim.toLowerCase match {
          case "keep"        => Keep
          case "remove_soft" => Remove_Soft
          case "remove_all"  => Remove_All
          case _             => sys.error("Unknown parameter '" + v + "'.")
        }
    })

  opt("pcm", "predicateCompletionMode", "<standard | decomposed | simplification>",
    "Choose the type of predicate completion (default is simplification).", {
      pc: String =>
        pcm = pc.trim.toLowerCase match {
          case "simplification" => Simplification
          case "standard"       => Standard
          case "decomposed"     => Decomposed
          case _                => sys.error("Unknown predicate completion mode '" + pc + "'.")
        }
    })

  flagOpt("includeDomainDef", "domain-definitions", "Include domain definitions", {
    includeDomain = true
  })

  flagOpt("removePredicateDef", "remove-predicate-definitions", "Remove predicate definitions", {
    removePredicateDefinitions = true
  })

  flagOpt("removeFunctionsDef", "remove-function-definitions", "Remove function definitions", {
    removeFunctionDefinitions = true
  })

  opt("functionsPrefix", "functions-prefix", "<string>",
    s"Function prefix used for transformation of functions to predicates (default is '${lomrf.AUX_PRED_PREFIX}')", {
      v: String => functionPrefix = v
    })

  flagOpt("eliminateFunctions", "eliminate-functions", "Write function definitions as predicates", {
    eliminateFunctions = true
  })

  flagOpt("introduceFunctions", "introduce-functions", "Write functions produced by specific predicates", {
    introduceFunctions = true
  })

  opt("dynamic", "dynamic-implementations", "<string>",
    "Comma separated paths to search recursively for dynamic predicates/functions implementations (*.class and *.jar files).", {
      path: String => if (!path.isEmpty) implPaths = Some(path.split(','))
    })

  flagOpt("h", "help", "Print usage options.", {
    println(usage)
    sys.exit(0)
  })

  if (args.length == 0) println(usage)
  else if (parse(args)) {

    if (eliminateFunctions && introduceFunctions)
      logger.fatal("Simultaneous elimination and introduction of functions in not possible!")

    // In order to eliminate or introduce function CNF must be enabled, otherwise is not possible.
    if (eliminateFunctions && !cnf) {
      logger.warn("Function elimination enables CNF compilation")
      cnf = true
    }

    if (introduceFunctions && !cnf) {
      logger.warn("Function introduction enables CNF compilation")
      cnf = true
    }

    val inputFile = mlnFileName.getOrElse(logger.fatal("Please define the input MLN file."))
    val outputFile = outputMLNFileName.getOrElse(logger.fatal("Please define the output MLN file."))

    if (outputFile == inputFile)
      logger.fatal(s"Output file '${outputFile}' cannot be the same with input MLN '${inputFile}' file")

    compile(
      inputFile,
      outputFile,
      functionPrefix,
      includeDomain,
      removePredicateDefinitions,
      removeFunctionDefinitions,
      eliminateFunctions,
      introduceFunctions,
      weightsMode,
      pcm,
      cnf,
      implPaths)
  }

  def compile(
      source: String,
      target: String,
      functionPrefix: String,
      includeDomain: Boolean,
      removePredicateDefinitions: Boolean,
      removeFunctionDefinitions: Boolean,
      eliminateFunctions: Boolean,
      introduceFunctions: Boolean,
      weightsMode: WeightsMode,
      pcm: PredicateCompletionMode,
      cnf: Boolean,
      dynamicDefinitionPaths: Option[Array[String]]): Unit = {

    logger.info {
      s"""
         |Parameters:
         |\t(cnf) Convert formulas into CNF: $cnf
         |\t(includeDomain) Include domain definitions: $includeDomain
         |\t(removePredicateDefinitions) Remove predicate definitions: $removePredicateDefinitions
         |\t(removeFunctionDefinitions) Remove function definitions: $removeFunctionDefinitions
         |\t(functionPrefix) Function prefix used for elimination: $functionPrefix
         |\t(eliminateFunctions) Eliminate functions: $eliminateFunctions
         |\t(introduceFunctions) Introduce functions: $introduceFunctions
         |\t(weightsMode) Weights mode for output: $weightsMode
         |\t(pcm) Predicate completion mode: $pcm
       """.stripMargin
    }

    if (source == target)
      logger.fatal("Target file cannot be the same as the source file.")

    val (kb, constants) = dynamicDefinitionPaths match {
      case Some(paths) =>
        val implFinder = ImplFinder(classOf[DynamicAtomBuilder], classOf[DynamicFunctionBuilder])
        implFinder.searchPaths(paths)
        KB.fromFile(source, Some(implFinder.result))

      case None => KB.fromFile(source, None)
    }

    lazy val completedFormulas =
      PredicateCompletion(kb.formulas, kb.definiteClauses, pcm)(kb.predicateSchema, kb.functionSchema, constants)

    lazy val resultingPredicateSchema = pcm match {
      case Simplification =>
        val resultingFormulas = kb.predicateSchema -- kb.definiteClauses.map(_.clause.head.signature)

        if (resultingFormulas.isEmpty)
          logger.warn("The given theory is empty (i.e., contains empty set of non-zeroed formulas).")

        resultingFormulas
      case _ => kb.predicateSchema
    }

    logger.info(s"Source MLN: $source\n\t$kb\n")

    val output = new PrintStream(target)

    // Print domain data
    if (includeDomain && constants.nonEmpty) {
      output.println("// Domains")
      for ((name, constants) <- constants; if constants.nonEmpty)
        output.println(s"$name = { ${constants.mkString(",")} }")
      output.println()
    }

    // Print predicate definitions. In case we introduce functions do not print function predicates
    if (!removePredicateDefinitions && resultingPredicateSchema.nonEmpty) {
      output.println("// Predicate definitions")
      for ((signature, args) <- resultingPredicateSchema; if !introduceFunctions || !signature.symbol.contains(functionPrefix))
        output.println(s"${signature.symbol}${if (args.isEmpty) "" else args.mkString("(", ",", ")")}")
      output.println()
    }

    // Print function definitions or functions as predicates
    if (!removeFunctionDefinitions) {
      if (eliminateFunctions && kb.functionSchema.nonEmpty) { // in order to eliminate functions, functions must exist
        output.println("// Function definitions as predicates")
        for ((signature, (retType, args)) <- kb.functionSchema) {
          val predicate = functionPrefix + signature.symbol + "(" + retType + "," + args.mkString(",") + ")"
          output.println(predicate)
        }
      } else if (introduceFunctions) {
        output.println("// Function definitions")
        for ((signature, args) <- kb.predicateSchema) {
          if (signature.symbol.contains(functionPrefix)) {
            val function = args.head + " " + signature.symbol.replace(functionPrefix, "") + "(" + args.drop(1).mkString(",") + ")"
            output.println(function)
          }
        }
      } else if (kb.functionSchema.nonEmpty) { // in order to print functions definitions, functions must exist
        output.println("// Function definitions")
        for ((signature, (retType, args)) <- kb.functionSchema)
          output.println(s"$retType ${signature.symbol}(${args.mkString(",")})")
      }
      output.println()
    }

    // convert clauses appearing in the KB into CNF
    if (cnf) {
      var clauseCounter = 0
      output.println("// Clauses\n")
      for (formula <- completedFormulas) {
        output.println("// Source formula: " + formula.toText + "\n")
        val clauses = formula.toCNF(constants)
        for (c <- clauses) {
          output.println(clauseFormatter(c, weightsMode, eliminateFunctions, introduceFunctions, functionPrefix))
          clauseCounter += 1
          output.println()
        }
      }
      logger.info("Total " + clauseCounter + " clauses are written in '" + target + "'")
    } else {
      output.println("// Formulas")
      completedFormulas.foreach(f => output.println(s"${formulaFormatter(f, weightsMode)}\n"))
    }

    output.flush()
    output.close()
  }

  /**
    * Formats a formula.
    *
    * @param formula a formula
    * @param weightsMode a weights mode
    * @return the formatted formula
    */
  private def formulaFormatter(formula: Formula, weightsMode: WeightsMode): String = formula match {
    case WeightedFormula(w, f) =>
      weightsMode match {
        case Keep =>
          if (w.isInfinity) f.toText + "."
          else if (!w.isNaN) numFormat.format(w) + " " + f.toText
          else f.toText
        case Remove_Soft =>
          if (w.isInfinity) f.toText + "."
          else f.toText
        case Remove_All => f.toText
      }
    case _ => formula.toText
  }

  /**
    * Formats a clause.
    *
    * @param clause a clause
    * @param weightsMode a weights mode
    * @param eliminateFunctions eliminate functions flag
    * @param introduceFunctions introduce functions flag
    * @param functionPrefix function prefix
    * @return the formatted clause
    */
  @tailrec
  private def clauseFormatter(
      clause: Clause,
      weightsMode: WeightsMode,
      eliminateFunctions: Boolean,
      introduceFunctions: Boolean,
      functionPrefix: String): String = {

    /*
     * Is a unit clause with negative weight, thus negate it and convert its weight
     * into a positive value (e.g the '-w A(x)' will be converted into 'w !A(x)').
     */
    if (clause.isUnit && clause.weight < 0)
      clauseFormatter(
        Clause(Set(clause.literals.head.negate), -clause.weight),
        weightsMode,
        eliminateFunctions,
        introduceFunctions,
        functionPrefix
      )
    else {

      val convertedClause =
        if (eliminateFunctions) // Replace all functions in the given clause with auxiliary predicates
          LogicFormatter.ClauseFormatter.eliminateFunctions(clause)
        else if (introduceFunctions) // Replace all function predicates in the given clause with actual functions
          LogicFormatter.ClauseFormatter.introduceFunctions(clause)
        else clause // Just write the given clause as it is (no functions elimination or introduction)

      weightsMode match {
        case Keep => convertedClause.toText()
        case Remove_Soft =>
          if (clause.weight.isInfinity) convertedClause.toText()
          else convertedClause.toText(weighted = false)
        case Remove_All =>
          convertedClause.toText(weighted = false)
      }
    }
  }
}
