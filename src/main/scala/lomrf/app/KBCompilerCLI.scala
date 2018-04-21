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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.app

import java.io.FileWriter
import java.text.DecimalFormat

import auxlib.opt.OptionParser
import com.typesafe.scalalogging.LazyLogging
import lomrf.logic._
import lomrf.logic.PredicateCompletionMode._
import lomrf.logic.dynamic.{DynamicAtomBuilder, DynamicFunctionBuilder}
import lomrf.mln.model.{KB, MLNSchema}
import lomrf.util.ImplFinder
import lomrf.util.logging.Implicits._
import scala.annotation.tailrec

/**
 * Command line tool for knowledge compilation. In particular using this tool we can perform
 * predicate completion, CNF transformation, FOL function transformation, as well as weights elimination.
 */
object KBCompilerCLI extends LazyLogging {

  import WeightsMode._

  private val numFormat = new DecimalFormat("0.############")

  def main(args: Array[String]): Unit = {

    println(lomrf.ASCIILogo)
    println(lomrf.BuildVersion)

    val opt = new KBCOptions
    if (args.length == 0) println(opt.usage)
    else if (opt.parse(args)) {
      
      if(opt.eliminateFunctions && opt.introduceFunctions)
        logger.fatal("Simultaneous elimination and introduction of functions in not possible!")

      // In order to eliminate or introduce function CNF must be enabled, otherwise is not possible.
      if(opt.eliminateFunctions && !opt.cnf) {
        logger.warn("Function elimination enables CNF compilation")
        opt.cnf = true
      }

      if(opt.introduceFunctions && !opt.cnf){
        logger.warn("Function introduction enables CNF compilation")
        opt.cnf = true
      }

      compile(
        opt.mlnFileName.getOrElse(logger.fatal("Please define the input MLN file.")),
        opt.evidenceFileName, //.getOrElse(""),
        opt.outputMLNFileName.getOrElse(logger.fatal("Please define the output MLN file.")),
        opt.functionPrefix,
        opt.includeDomain,
        opt.removePredicateDefinitions,
        opt.removeFunctionDefinitions,
        opt.eliminateFunctions,
        opt.introduceFunctions,
        opt.weightsMode,
        opt.pcm,
        opt.cnf,
        opt.implPaths
      )
    }
  }

  def compile(source: String,
              evidenceOpt: Option[String],
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

    logger.info{
      s"""
         |Parameters:
         |\t(cnf) Convert formulas into CNF:  $cnf
         |\t(includeDomain) Include domain definitions: $includeDomain
         |\t(removePredicateDefinitions) Remove predicate definitions:  $removePredicateDefinitions
         |\t(removeFunctionDefinitions) Remove function definitions: $removeFunctionDefinitions
         |\t(functionPrefix) Function prefix used for elimination:  $functionPrefix
         |\t(eliminateFunctions) Eliminate functions: $eliminateFunctions
         |\t(introduceFunctions) Introduce functions: $introduceFunctions
         |\t(weightsMode) Weights mode for output: ${if (weightsMode == KEEP) "Keep" else if (weightsMode == RM_SOFT) "Remove soft" else "Remove all"}
         |\t(pcm) Predicate completion mode: ${if (pcm == Standard) "Standard" else if (pcm == Simplification) "Simplification" else "Decomposed"}
       """.stripMargin
    }


    if (source == target)
      logger.fatal("Target file cannot be the same with source file.")

    val (kb, constants) = dynamicDefinitionPaths match {
      case Some(paths) =>
        val implFinder = ImplFinder(classOf[DynamicAtomBuilder], classOf[DynamicFunctionBuilder])
        implFinder.searchPaths(paths)
        KB.fromFile(source, Some(implFinder.result))

      case None => KB.fromFile(source, None)
    }
    //val constants = constBuilder.result()

    lazy val completedFormulas = PredicateCompletion(kb.formulas, kb.definiteClauses, pcm)(kb.predicateSchema, kb.functionSchema, constants)

    lazy val resultingPredicateSchema = pcm match {
      case Simplification =>
        val resultingFormulas = kb.predicateSchema -- kb.definiteClauses.map(_.clause.head.signature)

        if(resultingFormulas.isEmpty)
          logger.warn("The given theory is empty (i.e., contains empty set of non-zeroed formulas).")

        resultingFormulas
      case _ => kb.predicateSchema
    }

    //lazy val mlnSchema = MLNSchema(resultingPredicateSchema, kb.functionSchema, kb.dynamicPredicates, kb.dynamicFunctions)

    logger.info{
      s"""
         |Source MLN: $source
         |\tFound ${kb.formulas.size} formulas
         |tFound ${kb.definiteClauses.size} definite clauses
         |tFound ${kb.predicateSchema.size} predicates
         |tFound ${kb.functionSchema.size} functions
       """.stripMargin
    }

    val fileWriter = new FileWriter(target)
    import fileWriter.write

    // write domain data
    if (includeDomain && constants.nonEmpty) {
      write("// Domain\n")
      for ((name, constants) <- constants; if constants.nonEmpty) write(name + "={" + constants.mkString(",") + "}\n")
      write("\n")
    }

    // write predicate definitions. In case we introduce functions do not write function predicates (beginning with function prefix)
    if (!removePredicateDefinitions && resultingPredicateSchema.nonEmpty) {
      write("// Predicate definitions\n")
      for ((signature, args) <- resultingPredicateSchema ; if !introduceFunctions || !signature.symbol.contains(functionPrefix)) {
        val line = signature.symbol + (
          if (args.isEmpty) "\n"
          else "(" + args.mkString(",") + ")\n")
        write(line)
      }
      write("\n")
    }

    // write function definitions or functions as predicates
    if (!removeFunctionDefinitions) {
      if (eliminateFunctions && kb.functionSchema.nonEmpty) { // in order to eliminate functions, functions must exist
        write("// Function definitions as predicates\n")
        for ((signature, (retType, args)) <- kb.functionSchema) {
          val predicate = functionPrefix + signature.symbol + "(" + retType + "," + args.mkString(",") + ")\n"
          write(predicate)
        }
      }
      else if(introduceFunctions) {
        write("// Function definitions\n")
        for ((signature, args) <- kb.predicateSchema) {
          if(signature.symbol.contains(functionPrefix)) {
            val function = args.head + " " + signature.symbol.replace(functionPrefix, "") + "(" + args.drop(1).mkString(",") + ")\n"
            write(function)
          }
        }
        write("\n")
      }
      else if(kb.functionSchema.nonEmpty) { // in order to write functions definitions, functions must exist
        write("// Function definitions\n")
        for ((signature, (retType, args)) <- kb.functionSchema) {
          val line = retType + " " + signature.symbol + "(" + args.mkString(",") + ")\n"
          write(line)
        }
      }
    }

    // transform clauses appearing in the KB into CNF
    if (cnf) {
      var clauseCounter = 0
      write("\n\n// Clauses\n")
      for (formula <- completedFormulas) {
        write("\n// Source formula: " + formula.toText + "\n")
        val clauses = formula.toCNF(constants)
        for (c <- clauses) {
          write(clauseFormatter(c, weightsMode, eliminateFunctions, introduceFunctions, functionPrefix))
          clauseCounter += 1
          write("\n")
        }
      }
      logger.info("Total " + clauseCounter + " clauses are written in '" + target + "'")
    }
    else {
      write("\n\n// Formulas\n")
      completedFormulas.foreach(f => {
        write(formulaFormatter(f, weightsMode)); write("\n\n")
      })
    }

    fileWriter.flush()
    fileWriter.close()
  }

  /**
   * Formula formatter for formatting a formula given a particular
   * weights mode.
   *
   * @param formula the formula
   * @param weightsMode the weights mode
   *
   * @return the formatted formula
   */
  private def formulaFormatter(formula: Formula, weightsMode: WeightsMode): String = {
    formula match {
      case WeightedFormula(w, f) =>
        weightsMode match {
          case KEEP =>
            if (w.isInfinity) f.toText + "."
            else if (!w.isNaN) numFormat.format(w) + " " + f.toText
            else f.toText
          case RM_SOFT =>
            if (w.isInfinity) f.toText + "."
            else f.toText
          case RM_ALL => f.toText
        }
      case _ => formula.toText
    }
  }

  @tailrec
  private def clauseFormatter(clause: Clause, weightsMode: WeightsMode, eliminateFunctions: Boolean,
                              introduceFunctions: Boolean, functionPrefix: String): String = {
    import lomrf.logic.{TermFunction, Variable, Term, AtomicFormula}

    val functionVarPrefix = "funcRetVar"

    /*
     * Is a unit clause with negative weight, thus negate it and convert its weight
     * into a positive value (e.g the '-w A(x)' will be converted into 'w !A(x)').
     */
    if (clause.isUnit && clause.weight < 0)
      clauseFormatter(Clause(Set(clause.literals.head.negate), -clause.weight), weightsMode, eliminateFunctions, introduceFunctions, functionPrefix)
    else {
        var txtLiterals = ""

        // Just write the given clause as it is (no functions elimination or introduction)
        if (!eliminateFunctions && !introduceFunctions)
          txtLiterals = clause.literals.view.map(_.toText).mkString(" v ")

        // Replace all functions in the given clause with utility predicates
        else if(eliminateFunctions) {

          val (literalsNoFunctions, literalsWithFunctions) = clause.literals.span(l => l.sentence.functions.isEmpty)

          if(literalsWithFunctions.nonEmpty) {

            var fMap = Map[TermFunction, (String, Literal)]()
            var functionCounter = 0

            for(function <- clause.functions) {
              fMap.get(function) match {
                case None =>
                  val functionVar = functionVarPrefix + functionCounter
                  val terms = Vector(Variable(functionVar, function.domain)) ++:  function.terms
                  val functionLiteral = NegativeLiteral(AtomicFormula(functionPrefix + function.symbol, terms))
                  fMap += (function ->(functionVar, functionLiteral))
                  functionCounter += 1
                case _ =>
              }
            }

            val replacedLiterals =
              for {
                literal <- literalsWithFunctions
                sentence = literal.sentence
                newArgs = for (arg <- sentence.terms) yield arg match {
                  case f: TermFunction =>
                    val varName = fMap(f)._1
                    Variable(varName, "")
                  case t: Term => t
                }
              } yield literal match {
                case p: PositiveLiteral => PositiveLiteral(AtomicFormula(sentence.symbol, newArgs))
                case n: NegativeLiteral => NegativeLiteral(AtomicFormula(sentence.symbol, newArgs))
              }

            var results =
              if (literalsNoFunctions.nonEmpty)
                List[String](literalsNoFunctions.map(_.toText).mkString(" v "))
              else List[String]()

            if (replacedLiterals.nonEmpty)
              results = results ::: List[String](replacedLiterals.map(_.toText).mkString(" v "))

            if (fMap.nonEmpty)
              results = results ::: List[String](fMap.values.map(_._2.toText).mkString(" v "))

            txtLiterals = results.mkString(" v ")
          }
          else
            txtLiterals = literalsNoFunctions.map(_.toText).mkString(" v ")
        }

        // Replace all function predicates in the given clause with actual functions
        else if(introduceFunctions) {
          val (literalsNoFunctions, literalsFunctions) = clause.literals.partition(l => !l.sentence.symbol.contains(functionPrefix))

          if (literalsFunctions.nonEmpty) {

            var lMap = Map[Term, TermFunction]()

            for(literal <- literalsFunctions) {
              println(literal.toText)
              val functionSymbol = literal.sentence.symbol.replace(functionPrefix, "")
              val functionVar = literal.sentence.terms.head
              val terms = literal.sentence.terms.drop(1)
              val function = TermFunction(functionSymbol, terms)
              lMap += (functionVar -> function)
            }

            val replacedLiterals =
              for {
                literal <- literalsNoFunctions
                sentence = literal.sentence
                newArgs = for (arg <- sentence.terms) yield arg match {
                  case t: Term =>
                    val term = if(lMap.contains(t)) lMap(t) else t
                    term
                }
              } yield literal match {
                case p: PositiveLiteral => PositiveLiteral(AtomicFormula(sentence.symbol, newArgs))
                case n: NegativeLiteral => NegativeLiteral(AtomicFormula(sentence.symbol, newArgs))
              }

            val results =
              if(replacedLiterals.nonEmpty)
                List[String](replacedLiterals.map(_.toText).mkString(" v "))
              else List[String]()

            txtLiterals = results.mkString(" v ")
          }
          else txtLiterals = literalsNoFunctions.map(_.toText).mkString(" v ")
        }

      weightsMode match {
        case KEEP =>
          if (clause.weight.isInfinity) txtLiterals + "."
          else if (!clause.weight.isNaN) numFormat.format(clause.weight) + " " + txtLiterals
          else txtLiterals
        case RM_SOFT =>
          if (clause.weight.isInfinity) txtLiterals + "."
          else txtLiterals
        case RM_ALL => txtLiterals
      }
    }
  }

  private class KBCOptions extends OptionParser {

    // The path to the input MLN file
    var mlnFileName: Option[String] = None

    // Input evidence file(s) (path)
    var evidenceFileName: Option[String] = None

    // The path to the output file
    var outputMLNFileName: Option[String] = None

    // Function prefix for transformation of functions to predicates
    var functionPrefix: String = lomrf.AUX_PRED_PREFIX //"ReturnValueOf"

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
    var weightsMode = KEEP

    // Predicate completion mode
    var pcm: PredicateCompletionMode = Simplification

    // Convert formulas into CNF
    var cnf: Boolean = false

    var implPaths: Option[Array[String]] = None

    opt("i", "input", "<mln file>", "Input Markov Logic file", {
      v: String => mlnFileName = Some(v)
    })

    opt("e", "evidence", "<db file>", "Evidence database file", {
      v: String => evidenceFileName = Some(v)
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
      "or (remove_all) convert all formulas to soft-constrained without weights. Please note, that in some cases the weights cannot be kept (e.g. predicate completion with simplification).", {
      v: String => weightsMode = v.trim.toLowerCase match {
        case "keep" => KEEP
        case "remove_soft" => RM_SOFT
        case "remove_all" => RM_ALL
        case _ => sys.error("Unknown parameter '" + v + "'.")
      }
    })

    opt("pcm", "predicateCompletionMode", "<standard | decomposed | simplification>", "Choose the type of predicate completion (simplification default).", {
      pc: String => pcm = pc.trim.toLowerCase match {
        case "simplification" => Simplification
        case "standard" => Standard
        case "decomposed" => Decomposed
        case _ => sys.error("Unknown predicate completion mode '" + pc + "'.")
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

    opt("functionsPrefix", "functions-prefix", "<string>", s"Function prefix used for transformation of functions to predicates (default is ${lomrf.AUX_PRED_PREFIX}})", {
      v: String => functionPrefix = v
    })

    flagOpt("eliminateFunctions", "eliminate-functions", "Write function definitions as predicates", {
      eliminateFunctions = true
    })

    flagOpt("introduceFunctions", "introduce-functions", "Write functions produced by specific predicates", {
      introduceFunctions = true
    })

    opt("dynamic", "dynamic-implementations", "<string>", "Comma separated paths to search recursively for dynamic predicates/functions implementations (*.class and *.jar files).", {
      path: String => if (!path.isEmpty) implPaths = Some(path.split(','))
    })

    flagOpt("h", "help", "Print usage options.", {
      println(usage)
      sys.exit(0)
    })

  }
}

object WeightsMode extends Enumeration {
  type WeightsMode = Value
  val KEEP, RM_SOFT, RM_ALL = Value
}
