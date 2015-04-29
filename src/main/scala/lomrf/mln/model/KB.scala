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
 * self program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * self program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with self program.  If not, see <http://www.gnu.org/licenses/>.
 */

package lomrf.mln.model

import auxlib.log.Logging
import collection.immutable.HashMap
import scala.collection.mutable
import java.io.{BufferedReader, File, FileReader}
import java.util.regex.Pattern
import lomrf.logic.PredicateCompletionMode._
import lomrf.logic._
import lomrf.logic.dynamic.{DynamicAtomBuilder, DynamicFunctionBuilder}
import lomrf.util.{ConstantsSetBuilder, ImplFinder}

/**
 * self class contains the parsed components of an MLN theory.
 *
 * @param constants holds a mapping of 'domain name' to its 'constants set builder'.
 * @param predicateSchema  holds a mapping of 'atom signature' to a sequence of the domain names of its terms.
 * @param functionSchema  holds a mapping of 'function signature' to its return domain name, as well as a sequence of the domain names of its terms.
 * @param formulas contains a collection of (weighted) formulas in first-order logic form.
 * @param dynamicPredicates contains definitions of dynamic predicates
 * @param dynamicFunctions contains definitions of dynamic functions
 */
class KB(val constants: Map[String, ConstantsSetBuilder],
                        val predicateSchema: Map[AtomSignature, Seq[String]],
                        val functionSchema: Map[AtomSignature, (String, Vector[String])],
                        val formulas: Set[Formula],
                        val dynamicPredicates: Map[AtomSignature, Vector[String] => Boolean],
                        val dynamicFunctions: Map[AtomSignature, Vector[String] => String]) {


  override def toString: String = {
    "KB = {" +
      "\n\t# Constant domains  : " + constants.size +
      "\n\t# Predicate schemas : " + predicateSchema.size +
      "\n\t# Function schemas  : " + functionSchema.size +
      "\n\t# Formulas          : " + formulas.size +
      "}"
  }
}

/**
 * Knowledge base builder with fluent interface pattern
 */
final class KBBuilder { self =>

  private var dirty = false

  private var _constantBuilders = Map.empty[String, ConstantsSetBuilder]

  private var _predicateSchema = Map.empty[AtomSignature, Vector[String]]

  private var _functionSchema = Map.empty[AtomSignature, (String, Vector[String])]

  private var _formulas = Set.empty[Formula]

  private var _dynamicPredicates = Map.empty[AtomSignature, Vector[String] => Boolean]

  private var _dynamicFunctions = Map.empty[AtomSignature, Vector[String] => String]

  def withConstantBuilders(input: Map[String, ConstantsSetBuilder]): self.type ={
    _constantBuilders = input
    self
  }

  def withPredicateSchema(input: Map[AtomSignature, Vector[String]]): self.type ={
    _predicateSchema = input
    self
  }

  def withFunctionSchema(input: Map[AtomSignature, (String, Vector[String])]): self.type ={
    _functionSchema = input
    self
  }

  def withFormulas(input: Set[Formula]): self.type ={
    _formulas = input
    self
  }

  def withDynamicPredicates(input: Map[AtomSignature, Vector[String] => Boolean]): self.type ={
    _dynamicPredicates = input
    self
  }

  def withDynamicFunctions(input: Map[AtomSignature, Vector[String] => String]): self.type ={
    _dynamicFunctions = input
    self
  }

  def result(): KB = {
    dirty = true
    new KB(_constantBuilders, _predicateSchema, _functionSchema, _formulas, _dynamicPredicates, _dynamicFunctions)
  }

  object constants {

    private def copyIfDirty(): Unit ={
      if(self.dirty) {
        _constantBuilders = _constantBuilders.map{case (k, v) => k -> v.copy()}
        dirty = false
      }
    }

    def apply(key: String) = _constantBuilders(key)

    def apply(): Map[String, ConstantsSetBuilder] = _constantBuilders

    def update(input: Map[String, ConstantsSetBuilder]): self.type ={
      _constantBuilders = input
      dirty = false
      self
    }

    def += (key: String, value: String): self.type ={
      copyIfDirty()

      _constantBuilders.get(key) match {
        case Some(builder) => builder += value
        case _ => _constantBuilders += (key -> ConstantsSetBuilder(value))
      }

      self
    }

    def += (entry: (String, String)): self.type = constants += (entry._1, entry._2)

    def ++= (entry: (String, Iterable[String])): self.type ={
      copyIfDirty()

      val (key, values) = entry

      _constantBuilders.get(key) match {
        case Some(builder) => builder ++= values
        case _ => _constantBuilders += (key -> ConstantsSetBuilder(values))
      }

      self
    }

    def clear(): Unit = _constantBuilders = Map.empty
  }


  object predicateSchema {

    def apply(key: AtomSignature) = _predicateSchema(key)

    def apply() = _predicateSchema

    def += (key: AtomSignature, value: Vector[String]): self.type ={
      _predicateSchema += (key -> value)
      self
    }

    def += (entry: (AtomSignature, Vector[String])): self.type = {
      _predicateSchema += entry
      self
    }

    def ++= (entries: Iterable[(AtomSignature, Vector[String])]): self.type ={
      entries.foreach(predicateSchema += _)
      self
    }

    def clear(): Unit = _predicateSchema = Map.empty
  }

  object functionSchema {

    def apply(key: AtomSignature) = _functionSchema(key)

    def apply() = _functionSchema

    def += (key: AtomSignature, value: (String, Vector[String])): self.type = {
      _functionSchema += (key -> value)
      self
    }

    def += (entry: (AtomSignature, (String, Vector[String]))): self.type = {
      _functionSchema += entry
      self
    }

    def ++= (entries: Iterable[(AtomSignature,(String, Vector[String]))]): self.type ={
      entries.foreach(functionSchema += _)
      self
    }

    def clear(): Unit = _functionSchema = Map.empty
  }

  object formulas {

    def apply() = _formulas

    def += ( value: Formula): self.type ={
     _formulas += value
     self
    }

    def ++= (values: Iterable[Formula]): self.type ={
      _formulas ++= values
      self
    }

    def clear(): Unit = _formulas = Set.empty
  }

  object dynamicPredicates {

    def apply(key: AtomSignature) = _dynamicPredicates(key)

    def apply() = _dynamicPredicates

    def += (key: AtomSignature, value: Vector[String] => Boolean): self.type ={
      _dynamicPredicates += (key -> value)
      self
    }

    def += (entry: (AtomSignature, Vector[String] => Boolean)): self.type ={
      _dynamicPredicates += entry
      self
    }

    def ++= (entries: Iterable[(AtomSignature, Vector[String] => Boolean)]): self.type ={
      _dynamicPredicates ++= entries
      self
    }

    def clear(): Unit = _dynamicPredicates = Map.empty
  }

  object dynamicFunctions {

    def apply(key: AtomSignature) = _dynamicFunctions(key)

    def apply() = _dynamicFunctions

    def += (key: AtomSignature, value:  Vector[String] => String): self.type ={
      _dynamicFunctions += (key -> value)
      self
    }

    def += (entry: (AtomSignature,  Vector[String] => String)): self.type ={
      _dynamicFunctions += entry
      self
    }

    def ++= (entries: Iterable[(AtomSignature,  Vector[String] => String)]): self.type ={
      _dynamicFunctions ++= entries
      self
    }

    def clear(): Unit = _dynamicFunctions = Map.empty
  }
}

object KBBuilder {

  def apply(): KBBuilder = new KBBuilder

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
    var constants =  Map[String, ConstantsSetBuilder]()
    var predicateSchema = new HashMap[AtomSignature, Vector[String]]()
    var functionSchema = new HashMap[AtomSignature, (String, Vector[String])]()

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
    // Important note: self procedure is not applied to dynamic functions/predicates
    def _findUndefinedConstants(formula: Formula) {
      debug("Looking for constants in:  " + formula.toText)

      def parseTerm(term: Term, key: String) {
        term match {
          case Constant(symbol) => constants(key) += symbol
          case f: TermFunction if !dynamicFunctionBuilders.contains(f.signature) =>
            for ((term, idx) <- f.terms.zipWithIndex) parseTerm(term, functionSchema(f.signature)._2(idx))
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
                functionSchema += (AtomSignature(functionName, args.size) -> (retType, for (element <- args) yield element))
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
    val kbParser = new KBParser(predicateSchema, functionSchema, dynamicAtomBuilders, dynamicFunctionBuilders)

    var formulas = Set[Formula]()
    var definiteClauses = Set[WeightedDefiniteClause]()
    val queue = mutable.Queue[IncludeFile]()

    val kbExpressions: Iterable[MLNExpression] = kbParser.parseAll(kbParser.mln, fileReader) match {
      case kbParser.Success(exprs, _) => exprs.asInstanceOf[Iterable[MLNExpression]]
      case x => fatal("Can't parse the following expression: " + x + " in file: " + kbFile)
    }

    def processMLNExpression(expr: MLNExpression): Unit = expr match {
      case f: WeightedFormula =>
        if(f.weight == 0.0)
          warn(s"Ignoring zero weighted formula '${f.toText}'")
        else {
          formulas += f
          _findUndefinedConstants(f)
        }
      case c: WeightedDefiniteClause =>
        if (c.weight == 0.0)
          warn(s"Ignoring zero weighted definite clause '${c.toText}'")
        else {
          definiteClauses += c
          _findUndefinedConstants(c.clause.body)
          _findUndefinedConstants(c.clause.head)
        }
      case inc: IncludeFile => queue += inc
      case _ => warn("Ignoring expression: " + expr)
    }

    kbExpressions.foreach(processMLNExpression)

    while (queue.nonEmpty) {
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

      val curr_expressions: Iterable[MLNExpression] = kbParser.parseAll(kbParser.mln, new FileReader(incFile)) match {
        case kbParser.Success(exprs, _) => exprs.asInstanceOf[Iterable[MLNExpression]]
        case x => fatal("Can't parse the following expression: " + x + " in file: " + incFile)
      }

      curr_expressions.foreach(processMLNExpression)

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

    whenDebug {
      constants.foreach(entry => debug("|" + entry._1 + "|=" + entry._2.size))
    }

    if(resultingFormulas.isEmpty)
      warn("The given theory is empty (i.e., contains empty set of non-zeroed formulas).")



    new KB(constants, resultingPredicateSchema, functionSchema, resultingFormulas, kbParser.getDynamicPredicates, kbParser.getDynamicFunctions)
  }


}
