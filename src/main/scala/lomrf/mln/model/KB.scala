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

package lomrf.mln.model

import collection.immutable.HashMap
import collection.mutable
import java.io.{BufferedReader, File, FileReader}
import java.util.regex.Pattern
import lomrf.debugMsg
import lomrf.logic.PredicateCompletionMode._
import lomrf.logic._
import lomrf.logic.dynamic.{DynamicAtomBuilder, DynamicFunctionBuilder}
import lomrf.util.{Logging, ImplFinder, ConstantsSetBuilder}

/**
 * KB
 *
 * @author Anastasios Skarlatidis
 */
private[model] class KB(val constants: mutable.HashMap[String, ConstantsSetBuilder],
                        val predicateSchema: Map[AtomSignature, Seq[String]],
                        val functionSchema: Map[AtomSignature, (String, List[String])],
                        val formulas: collection.Set[Formula],
                        val dynamicPredicates: Map[AtomSignature, List[String] => Boolean],
                        val dynamicFunctions: Map[AtomSignature, List[String] => String]) {


  override def toString: String = {
    "KB = {" +
      "\n\t# Constant domains  : " + constants.size +
      "\n\t# Predicate schemas : " + predicateSchema.size +
      "\n\t# Function schemas  : " + functionSchema.size +
      "\n\t# Formulas          : " + formulas.size +
      "}"
  }
}

private[model] object KB extends Logging {

  private val formulaRegex = Pattern.compile(".*\\s=>\\s.*|.*\\s<=>\\s.*|.*\\s:-\\s.*|.*\\s^\\s.*|.*\\sv\\s.*|.*\\s!\\s.*|\\d.*|.*\\.")
  private val ignoreRegex = Pattern.compile("\\s*\\*.*|/\\*.*|//.*|\\s+")

  private val sep = System.getProperty("file.separator")

  def apply(filename: String,
            pcMode: PredicateCompletionMode = Simplification,
            dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None): KB = {
    val kbFile = new File(filename)
    val fileReader = new BufferedReader(new FileReader(kbFile))
    val formulaMatcher = formulaRegex.matcher("")
    val commentMatcher = ignoreRegex.matcher("")

    val domainParser = new DomainParser()
    val constants = new mutable.HashMap[String, ConstantsSetBuilder]()
    var predicateSchema = new HashMap[AtomSignature, List[String]]()
    var functionSchema = new HashMap[AtomSignature, (String, List[String])]()

    // ---------------------------------------------------
    // Load predefined dynamic predicates and functions
    // ---------------------------------------------------

    // Load pre-defined dynamic predicates and functions
    var dynamicAtomBuilders = predef.dynAtomBuilders
    var dynamicFunctionBuilders = predef.dynFunctionBuilders

    // Load user-defined dynamic predicates and functions
    dynamicDefinitions match {
      case Some(definitionsMap) =>
        definitionsMap.get(classOf[DynamicAtomBuilder]) match {
          case Some(implementations) =>
            for (impl <- implementations) impl.newInstance() match {
              case builder: DynamicAtomBuilder => dynamicAtomBuilders += (builder.signature -> builder)
              case _ => //ignore
            }
          case _ => //ignore
        }

        definitionsMap.get(classOf[DynamicFunctionBuilder]) match {
          case Some(implementations) =>
            for (impl <- implementations) impl.newInstance() match {
              case builder: DynamicFunctionBuilder => dynamicFunctionBuilders += (builder.signature -> builder)
              case _ => //ignore
            }
          case _ => //ignore
        }
      case None => //do nothing
    }


    // The following utility function searches for constant values
    // that appear only in the formulas and not in the evidence.
    // Important note: this procedure is not applied to dynamic functions/predicates
    def _findUndefinedConstants(formula: Formula) {
      if (debugMsg) {
        println("Looking for constants in:  " + formula.toText)
      }

      def parseTerm(term: Term, key: String) {
        term match {
          case Constant(symbol) => constants(key) += symbol
          case f: Function if !dynamicFunctionBuilders.contains(f.signature) =>
            for ((term, idx) <- f.args.zipWithIndex) parseTerm(term, functionSchema(f.signature)._2(idx))
          case _ => //ignore
        }
      }

      formula match {
        case atom: AtomicFormula =>
          if (!dynamicAtomBuilders.contains(atom.signature))
            for ((term, idx) <- atom.terms.zipWithIndex) parseTerm(term, predicateSchema(atom.signature)(idx))
        case otherFormula: Formula =>
          val s = otherFormula.subFormulas
          _findUndefinedConstants(s.head)
          _findUndefinedConstants(s.last)
      }
    }

    // ---------------------------------------------------
    // Parse schema and constants
    // ---------------------------------------------------

    var stop = false
    var lineIdx = 0
    while (fileReader.ready() && !stop) {
      val line = fileReader.readLine()
      lineIdx += 1
      if (!line.isEmpty) {
        if (formulaMatcher.reset(line).matches()) {
          stop = true
        } else if (!commentMatcher.reset(line).matches()) {
          fileReader.mark(0)
          domainParser.parse(domainParser.definition, line) match {
            case domainParser.Success(expr, _) => expr match {
              case ConstantTypeDefinition(symbol, cons) =>
                val cb = new ConstantsSetBuilder()
                for (c <- cons) cb += c
                constants += (symbol -> cb)
              case IntegerTypeDefinition(symbol, start, end) =>
                val cb = new ConstantsSetBuilder()
                for (i <- start to end) cb += i.toString
                constants += (symbol -> cb)

              case FunctionType(retType, functionName, args) =>
                functionSchema += (AtomSignature(functionName, args.size) ->(retType, for (element <- args) yield element))
                if (!constants.contains(retType)) {
                  constants += (retType -> new ConstantsSetBuilder)
                }
                args.foreach(arg => if (!constants.contains(arg)) {
                  constants += (arg -> new ConstantsSetBuilder)
                })
              case AtomicType(predicateName, args) =>
                val atomSignature = AtomSignature(predicateName, args.size)
                predicateSchema.get(atomSignature) match {
                  case None =>
                    predicateSchema += (atomSignature -> (for (element <- args) yield element))
                    args.foreach(arg => if (!constants.contains(arg)) constants += (arg -> new ConstantsSetBuilder))
                  case _ => stop = true
                }
              case _ => sys.error("Cannot parse type definition '" + expr + "' in line " + lineIdx)
            }
            case _ => // ignore
          }
        }
      }

    } // end while


    // ---------------------------------------------------
    // Parse formulas and definite clauses
    // ---------------------------------------------------

    fileReader.reset()
    val kbParser = new ExtendedKBParser(predicateSchema, functionSchema, dynamicAtomBuilders, dynamicFunctionBuilders)

    val formulas = new mutable.LinkedHashSet[Formula]()
    var definiteClauses = new mutable.LinkedHashSet[WeightedDefiniteClause]()
    val queue = mutable.Queue[IncludeFile]()

    val kbExpressions: Seq[MLNExpression] = kbParser.parseAll(kbParser.mln, fileReader) match {
      case kbParser.Success(exprs, _) => exprs
      case x => fatal("Can't parse the following expression: " + x + " in file: " + kbFile)
    }

    for (expr <- kbExpressions) {
      if (debugMsg) {
        println("KB expression: " + expr)
      }

      expr match {
        case f: WeightedFormula =>
          formulas += f
          _findUndefinedConstants(f)
        case c: WeightedDefiniteClause =>
          definiteClauses += c
          _findUndefinedConstants(c.clause.body)
          _findUndefinedConstants(c.clause.head)
        case inc: IncludeFile => queue += inc
        case _ => warn("Ignoring expression: " + expr)
      }
    }

    while (!queue.isEmpty) {
      val inc = queue.dequeue()
      val incFile = {
        var tmp = new File(inc.filename)
        if (tmp.exists) tmp
        else {
          tmp = new File(kbFile.getParent + sep + inc.filename)
          if (tmp.exists) tmp
          else fatal("Cannot find file '" + inc.filename + "'")
        }
      }

      val curr_expressions = kbParser.parseAll(kbParser.mln, new FileReader(incFile)) match {
        case kbParser.Success(exprs, _) => exprs
        case x => fatal("Can't parse the following expression: " + x + " in file: " + incFile)
      }

      for (expr <- curr_expressions) expr match {
        case f: WeightedFormula =>
          formulas += f
          _findUndefinedConstants(f)
        case c: WeightedDefiniteClause =>
          definiteClauses += c
          _findUndefinedConstants(c.clause.body)
          _findUndefinedConstants(c.clause.head)
        case inc: IncludeFile => queue += inc
        case _ => warn("Ignoring expression: " + expr)
      }
    }

    val resultingFormulas = PredicateCompletion(formulas, definiteClauses, pcMode)(predicateSchema, functionSchema)

    // In case that some predicates are eliminated by the predicate completion,
    // remove them from the final predicate schema.
    val resultingPredicateSchema = pcMode match {
      case Simplification =>
        val eliminatedSignatures = definiteClauses.map(_.clause.head.signature).toSet
        predicateSchema -- eliminatedSignatures
      case _ => predicateSchema
    }

    if (debugMsg) {
      constants.foreach(entry => println("|" + entry._1 + "|=" + entry._2.size))
    }


    new KB(constants, resultingPredicateSchema, functionSchema, resultingFormulas, kbParser.getDynamicPredicates, kbParser.getDynamicFunctions)
  }


}
