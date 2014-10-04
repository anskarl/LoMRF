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

import java.io.FileWriter
import java.text.DecimalFormat
import lomrf.logic._
import lomrf.logic.PredicateCompletionMode._
import lomrf.logic.dynamic.{DynamicFunctionBuilder, DynamicAtomBuilder}
import lomrf.mln.model.MLN
import lomrf.util.{OptionParser, Logging, ImplFinder}

/**
 * Command-line tool for knowledge compilation. In particular with this tool we can perform
 * predicate completion, CNF transformation, FOL function transformation, as well as weights elimination.
 *
 * @author Anastasios Skarlatidis
 */
object KBCompilerCLI extends Logging {

  import WeightsMode._

  private val numFormat = new DecimalFormat("0.############")

  def main(args: Array[String]) {

    println(lomrf.ASCIILogo)
    println(lomrf.BuildVersion)

    val opt = new KBCOptions
    if (args.length == 0) println(opt.usage)
    else if (opt.parse(args)) {
      if(opt.eliminateFunctions && !opt.cnf)
        warn("Function elimination enables CNF compilation")

      if(opt.introduceFunctions && !opt.cnf)
        warn("Function introduction enables CNF compilation")

      opt.cnf = opt.eliminateFunctions || opt.introduceFunctions

      compile(
        opt.mlnFileName.getOrElse(fatal("Please define the input MLN file.")),
        opt.evidenceFileName.getOrElse(""),
        opt.outputMLNFileName.getOrElse(fatal("Please define the output MLN file.")),
        opt.profile,
        opt.includeDomain,
        opt.includePredicateDefinitions,
        opt.includeFunctionDefinitions,
        opt.eliminateFunctions,
        opt.introduceFunctions,
        opt.weightsMode,
        opt.pcm,
        opt.cnf,
        opt.implPaths
      )
    }
  }


  def compile(source: String, evidence: String, target: String, profile: Profile,
              includeDomain: Boolean,
              includePredicateDefinitions: Boolean,
              includeFunctionDefinitions: Boolean,
              eliminateFunctions: Boolean,
              introduceFunctions: Boolean,
              weightsMode: WeightsMode,
              pcm: PredicateCompletionMode,
              cnf: Boolean,
              dynamicDefinitionPaths: Option[Array[String]]) {

    if (source == target)
      fatal("Target file cannot be the same with source file.")

    //val mln = MLN(source, evidence, Set[AtomSignature](), Set[AtomSignature](), Set[AtomSignature](), pcm)

    val mln = dynamicDefinitionPaths match {
      case Some(paths) =>
        val implFinder = ImplFinder(classOf[DynamicAtomBuilder], classOf[DynamicFunctionBuilder])
        implFinder.searchPaths(paths)
        MLN(source, evidence, Set[AtomSignature](), Set[AtomSignature](), Set[AtomSignature](), pcm, dynamicDefinitions = Some(implFinder.result))
      case None => MLN(source, evidence, Set[AtomSignature](), Set[AtomSignature](), Set[AtomSignature](), pcm)
    }

    info(
      "\nSource MLN: " + source + "\n" +
        "\tFound " + mln.formulas.size + " formulas.\n" +
        "\tFound " + mln.schema.size + " predicates.\n" +
        "\tFound " + mln.functionSchema.size + " functions.")


    val fileWriter = new FileWriter(target)
    import fileWriter.write


    // write domain data
    if (includeDomain && mln.constants.nonEmpty) {
      write("// Domain\n")
      for ((name, constants) <- mln.constants; if !constants.isEmpty) {
        write(name + "={" + constants.map(_.toString).reduceLeft((left, right) => left + "," + right) + "}\n")
      }
      write("\n")
    }

    // write predicate definitions. In case we introduce functions do not write function predicates (beginning with function prefix)
    if (includePredicateDefinitions && mln.schema.nonEmpty) {
      write("// Predicate definitions\n")
      for ((signature, args) <- mln.schema ; if !introduceFunctions || !signature.symbol.contains(profile.functionPrefix)) {
        val line = signature.symbol + (
          if (args.isEmpty) "\n"
          else "(" + args.map(_.toString).reduceLeft((left, right) => left + "," + right) + ")\n")
        write(line)
      }
      write("\n")
    }

    // write function definitions or functions as predicates
    if (includeFunctionDefinitions) {
      if (eliminateFunctions && mln.functionSchema.nonEmpty) { // in order to eliminate functions, functions must exist
        write("// Function definitions as predicates\n")
        for ((signature, (retType, args)) <- mln.functionSchema) {
          val predicate = profile.functionPrefix + signature.symbol + "(" + retType + "," + args.map(_.toString).reduceLeft((left, right) => left + "," + right) + ")\n"
          write(predicate)
        }
      }
      else if(introduceFunctions) {
        write("// Function definitions\n")
        for ((signature, args) <- mln.schema) {
          if(signature.symbol.contains(profile.functionPrefix)) {
            val function = args(0) + " " + signature.symbol.replace(profile.functionPrefix, "") + "(" + args.drop(1).map(_.toString).reduceLeft((left, right) => left + "," + right) + ")\n"
            write(function)
          }
        }
        write("\n")
      }
      else if(mln.functionSchema.nonEmpty) { // in order to write functions definitions, functions must exist
        write("// Function definitions\n")
        for ((signature, (retType, args)) <- mln.functionSchema) {
          val line = retType + " " + signature.symbol + "(" + args.map(_.toString).reduceLeft((left, right) => left + "," + right) + ")\n"
          write(line)
        }
      }
    }

    if (cnf) {
      var clauseCounter = 0
      write("\n\n// Clauses\n")
      for (formula <- mln.formulas) {
        write("\n// Source formula: " + formula.toText + "\n")
        val clauses = formula.toCNF(mln.constants)
        for (c <- clauses) {
          write(clauseFormatter(c, weightsMode, eliminateFunctions, introduceFunctions)(profile, mln))
          clauseCounter += 1
          write("\n")
        }
      }
      info("Total " + clauseCounter + " clauses are written in '" + target + "'")
    }
    else {
      write("\n\n// Formulas\n")
      mln.formulas.foreach(f => {
        write(formulaFormatter(f, weightsMode)); write("\n\n")
      })
    }

    fileWriter.flush()
    fileWriter.close()
  }

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

  private def clauseFormatter(clause: Clause, weightsMode: WeightsMode, eliminateFunctions: Boolean, introduceFunctions: Boolean)(implicit profile: Profile, mln: MLN): String = {
    import lomrf.logic.{TermFunction, Variable, Term, AtomicFormula}

    val functionVarPrefix = profile.functionVarPrefix
    val functionPrefix = profile.functionPrefix

    if (clause.isUnit && clause.weight < 0) {
      // Is a unit clause with negative weight,
      // thus negate it and convert its weight into a positive value
      // (e.g the '-w A(x)' will be converted into 'w !A(x)')
      clauseFormatter(Clause(-clause.weight, Set(clause.literals.head.negate)), weightsMode, eliminateFunctions, introduceFunctions)
    } else {
      val txtLiterals =
        profile match {

          //
          // Just write the given clause as it is (no functions elimination or introduction)
          //
          case NormalProfile if !(eliminateFunctions || introduceFunctions) => clause.literals.map(_.toText).reduceLeft(_ + " v " + _)

          //
          // Replace all functions in the given clause with utility predicates
          //
          case _ if eliminateFunctions =>

            val (literalsNoFunctions, literalsWithFunctions) = clause.literals.span(l => l.sentence.functions.isEmpty)

            if(literalsWithFunctions.nonEmpty) {


              var fMap = Map[TermFunction, (String, Literal)]()
              var functionCounter = 0

              for(function <- clause.functions) {
                fMap.get(function) match {
                  case None =>
                    val functionVar = functionVarPrefix + functionCounter
                    val terms = Variable(functionVar, function.domain) :: function.terms
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


              /*var results2 = List[Literal]()

              literalsNoFunctions.foreach(literal => results2 = literal :: results2)
              replacedLiterals.foreach(literal => results2 = literal :: results2)
              fMap.values.foreach(entry => results2 = entry._2 :: results2)
              results2.map(_.toText).reduceLeft(_ + " v " + _)*/

              var results =
                if (literalsNoFunctions.nonEmpty)
                  List[String](literalsNoFunctions.map(_.toText).reduceLeft(_ + " v " + _))
                else List[String]()

              if (replacedLiterals.nonEmpty)
                results = results ::: List[String](replacedLiterals.map(_.toText).reduceLeft(_ + " v " + _))

              if (fMap.nonEmpty)
                results = results ::: List[String](fMap.values.map(_._2.toText).reduceLeft(_ + " v " + _))

              results.reduceLeft(_ + " v " + _)
            }
            else literalsNoFunctions.map(_.toText).reduceLeft(_ + " v " + _)

          //
          // Replace all function predicates in the given clause with actual functions
          //
          case _ if introduceFunctions =>

            val (literalsNoFunctions, literalsFunctions) = clause.literals.partition(l => !l.sentence.symbol.contains(functionPrefix))

            if (literalsFunctions.nonEmpty) {

              var lMap = Map[Term, TermFunction]()

              for(literal <- literalsFunctions) {
                println(literal.toText)
                val functionSymbol = literal.sentence.symbol.replace(functionPrefix, "")
                val functionVar = literal.sentence.terms(0)
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
                      val term =
                        if(lMap.contains(t))
                          lMap(t)
                        else
                          t
                      term
                  }
                } yield literal match {
                  case p: PositiveLiteral => PositiveLiteral(AtomicFormula(sentence.symbol, newArgs))
                  case n: NegativeLiteral => NegativeLiteral(AtomicFormula(sentence.symbol, newArgs))
                }

              val results =
                if(replacedLiterals.nonEmpty)
                  List[String](replacedLiterals.map(_.toText).reduceLeft(_ + " v " + _))
                else List[String]()

              results.reduceLeft(_ + " v " + _)
            }
            else literalsNoFunctions.map(_.toText).reduceLeft(_ + " v " + _)
        }

      weightsMode match {
        case KEEP =>
          //println("KEEP")
          if (clause.weight.isInfinity) txtLiterals + "."
          else if (!clause.weight.isNaN) numFormat.format(clause.weight) + " " + txtLiterals
          else txtLiterals
        case RM_SOFT =>
          //println("RM_SOFT")
          if (clause.weight.isInfinity) txtLiterals + "."
          else txtLiterals
        case RM_ALL => txtLiterals
      }
    }
  }


  private class KBCOptions extends OptionParser {

    var mlnFileName: Option[String] = None
    var evidenceFileName: Option[String] = None
    var outputMLNFileName: Option[String] = None

    var profile: Profile = AlchemyProfile
    var eliminateFunctions: Boolean = profile.functionDefinitionsAsPredicates
    var introduceFunctions: Boolean = false

    var includeDomain: Boolean = false
    var includePredicateDefinitions: Boolean = true
    var includeFunctionDefinitions: Boolean = true
    //var includeWeights: Boolean = true
    var weightsMode = KEEP
    var pcm: PredicateCompletionMode = Simplification
    var cnf: Boolean = true
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

    opt("p", "profile", "<alchemy | normal>", "Choose an output settings profile (default is Alchemy).", {
      v: String => profile = v.trim.toLowerCase match {
        case "alchemy" => AlchemyProfile
        case "normal" => NormalProfile
        case _ => sys.error("Unknown profile '" + v + "'.")
      }
    })


    opt("w", "weights", "<keep | removeSoft | removeAll>",
    "(keep) Keep all given weights, " +
      "or (removeSoft) eliminate the weighs from all soft-constrained formulas, " +
      "or (removeAll) convert all formulas to soft-constrained without weights. Please note, that in some cases the weights cannot be kept (e.g. predicate completion with simplification).", {
      v: String => weightsMode = v.trim.toLowerCase match {
        case "keep" => KEEP
        case "removesoft" => RM_SOFT
        case "removeall" => RM_ALL
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


    booleanOpt("fDomain", "flag-domain", "boolean", "Write domain definitions (default is false)", {
      v: Boolean => includeDomain = v
    })

    booleanOpt("fPredicates", "flag-predicates", "boolean", "Write predicate definitions (default is true)", {
      v: Boolean => includePredicateDefinitions = v
    })

    booleanOpt("fFunctions", "flag-functions", "boolean", "Write function definitions (default is true)", {
      v: Boolean => includeFunctionDefinitions = v
    })

    booleanOpt("fEliminateFunctions", "flag-EliminateFunctions", "boolean", "Write function definitions as predicates (override profile)", {
      v: Boolean => eliminateFunctions = v
    })

    booleanOpt("fIntroduceFunctions", "flag-IntroduceFunctions", "boolean", "Write functions produced by specific predicates (default is false)", {
      v: Boolean => introduceFunctions = v
    })

    booleanOpt("cnf", "clausal-form", "boolean", "Convert formulas to clausal form (default is true)", {
      v: Boolean => cnf = v
    })

    opt("dynamic", "dynamic-implementations", "<string>", "Comma separated paths to search recursively for dynamic predicates/functions implementations (*.class and *.jar files).", {
      path: String => if (!path.isEmpty) implPaths = Some(path.split(','))
    })

    flagOpt("h", "help", "Print usage options.", {
      println(usage)
      sys.exit(0)
    })

  }

  sealed abstract class Profile {
    def functionPrefix: String

    def functionVarPrefix: String = "funcRetVar"

    def eliminateFunctions: Boolean

    def functionDefinitionsAsPredicates: Boolean
  }

  case object AlchemyProfile extends Profile {
    def functionPrefix = "isReturnValueOf"

    def eliminateFunctions = true

    def functionDefinitionsAsPredicates = false
  }

  case object NormalProfile extends Profile {
    def functionPrefix = "ReturnValueOf"

    def eliminateFunctions = false

    def functionDefinitionsAsPredicates = false
  }

}

object WeightsMode extends Enumeration {
  type WeightsMode = Value
  val KEEP, RM_SOFT, RM_ALL = Value
}