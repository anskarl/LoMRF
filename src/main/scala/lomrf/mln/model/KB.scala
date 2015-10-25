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

package lomrf.mln.model


import auxlib.log.Logger
import scala.collection.mutable
import java.io.{BufferedReader, File, FileReader}
import java.util.regex.Pattern
import lomrf.logic._
import lomrf.logic.dynamic.{DynamicAtomBuilder, DynamicFunctionBuilder}
import lomrf.util.ImplFinder

import scala.concurrent._
import scala.util.{Failure, Success, Try}

/**
 * KB class contains the parsed components of an MLN theory.
 *
 * @param predicateSchema  holds a mapping of 'atom signature' to a sequence of the domain names of its terms.
 * @param functionSchema  holds a mapping of 'function signature' to its return domain name, as well as a sequence of the domain names of its terms.
 * @param dynamicPredicates contains definitions of dynamic predicates
 * @param dynamicFunctions contains definitions of dynamic functions
 * @param formulas contains a collection of (weighted) formulas in first-order logic form.
 */
class KB(val predicateSchema: PredicateSchema,
         val functionSchema: FunctionSchema,
         val dynamicPredicates: DynamicPredicates,
         val dynamicFunctions: DynamicFunctions,
         val formulas: Set[WeightedFormula],
         val definiteClauses: Set[WeightedDefiniteClause] = Set.empty) extends Serializable { self =>

  @transient
  lazy val schema = MLNSchema(predicateSchema, functionSchema, dynamicPredicates, dynamicFunctions)


  object signatures {

    @transient
    lazy val predicates = predicateSchema.keySet

    @transient
    lazy val function = functionSchema.keySet

    @transient
    lazy val dynamicPredicates = self.dynamicPredicates.keySet

    @transient
    lazy val dynamicFunction = self.dynamicFunctions.keySet
  }



  override def toString: String = {
    "KB = {" +
      "\n\t# Predicate schemas         : " + predicateSchema.size +
      "\n\t# Dynamic predicate schemas : " + dynamicPredicates.size +
      "\n\t# Function schemas          : " + functionSchema.size +
      "\n\t# Dynamic function schemas  : " + dynamicFunctions.size +
      "\n\t# Formulas                  : " + formulas.size +
      "}"
  }
}


object KB {

  private lazy val formulaRegex = Pattern.compile(".*\\s=>\\s.*|.*\\s<=>\\s.*|.*\\s:-\\s.*|.*\\s^\\s.*|.*\\sv\\s.*|.*\\s!\\s.*|\\d.*|.*\\.")
  private lazy val ignoreRegex = Pattern.compile("\\s*\\*.*|/\\*.*|//.*|\\s+")
  private lazy val log = Logger(this.getClass)

  def apply(predicateSchema: PredicateSchema,
            functionSchema: FunctionSchema,
            dynamicPredicates: DynamicPredicates,
            dynamicFunctions: DynamicFunctions,
            formulas: Set[WeightedFormula],
            definiteClauses: Set[WeightedDefiniteClause] = Set.empty) = {

    new KB(predicateSchema, functionSchema, dynamicPredicates, dynamicFunctions, formulas, definiteClauses)
  }



  def fromFile(filename: String,
               dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None,
               convertFunctions: Boolean = false): (KB, ConstantsDomainBuilder) = {
    import log._


    val kbFile = new File(filename)
    if(!kbFile.exists() || !kbFile.isFile || !kbFile.canRead)
      fatal(s"Cannot read input MLN file '${kbFile.getPath}'.")

    val fileReader = new BufferedReader(new FileReader(kbFile))
    val formulaMatcher = formulaRegex.matcher("")
    val commentMatcher = ignoreRegex.matcher("")

    val domainParser = new DomainParser()

    val constantsBuilder = new ConstantsDomainBuilder()
    val kbBuilder = KBBuilder(convertFunctions)

    // -------------------------------------------------------
    // Load dynamic predicates (predefined and user-defined)
    // -------------------------------------------------------
    var dynamicAtomBuilders = predef.dynAtomBuilders // predefined dynamic predicates

    for {
      implementationsMap <- dynamicDefinitions // get the user-specified implementations map
      implementations <- implementationsMap.get(classOf[DynamicAtomBuilder]) // get all user-specified Dynamic Atoms Builder
      builder <- implementations.map(_.newInstance().asInstanceOf[DynamicAtomBuilder]) // instantiate the builder (via reflection)
    } dynamicAtomBuilders += (builder.signature -> builder) // add the builder to the dynamicAtomBuilders Map, with key the atom signature.


    // -------------------------------------------------------
    // Load dynamic functions (predefined and user-defined)
    // -------------------------------------------------------
    var dynamicFunctionBuilders = predef.dynFunctionBuilders //predefined dynamic functions

    for {
      definitionsMap <- dynamicDefinitions // get the user-specified implementations map
      implementations <- definitionsMap.get(classOf[DynamicFunctionBuilder]) // get all user-specified Dynamic Functions Builder
      builder <- implementations.map(_.newInstance().asInstanceOf[DynamicFunctionBuilder]) // instantiate the builder (via reflection)
    } dynamicFunctionBuilders += (builder.signature -> builder) // add the builder to the dynamicFunctionBuilders Map, with key the function signature.

    /**
     * The following utility function searches for constant values that appear only in the formulas
     * and not in the evidence.
     *
     * Important note: self procedure is not applied to dynamic functions/predicates
     *
     * @param formula source formula
     */
    def storeUndefinedConstants(formula: Formula): Unit= {
      debug(s"Looking for constants in formula: '${formula.toText}'")

      def parseTerm(term: Term, key: String): Unit = term match {
        case Constant(symbol) => constantsBuilder(key) += symbol
        case f: TermFunction if !dynamicFunctionBuilders.contains(f.signature) =>
          // (String, Vector[String])
          val functionArgsSchema = kbBuilder.functionSchema(f.signature)._2

          for ((term, idx) <- f.terms.zipWithIndex) parseTerm(term, functionArgsSchema(idx))

        case _ => //ignore
      }


      formula match {
        case atom: AtomicFormula =>
          if (!dynamicAtomBuilders.contains(atom.signature)) {
            val predicateArgsSchema = kbBuilder.predicateSchema(atom.signature)

            for ((term, idx) <- atom.terms.zipWithIndex) parseTerm(term, predicateArgsSchema(idx))
          }
        case otherFormula: Formula => otherFormula.subFormulas.foreach(storeUndefinedConstants)
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
        if (formulaMatcher.reset(line).matches()) stop = true
        else if (!commentMatcher.reset(line).matches()) {
          fileReader.mark(0)
          domainParser.parse(domainParser.definition, line) match {
            case domainParser.Success(expr, _) => expr match {
              case ConstantTypeDefinition(symbol, cons) => constantsBuilder ++= (symbol, cons)

              case IntegerTypeDefinition(symbol, start, end) => constantsBuilder ++= (symbol, (start to end).map(_.toString))

              case FunctionType(retType, functionName, args) =>
                kbBuilder.functionSchema += (AtomSignature(functionName, args.size) -> (retType, args))
                constantsBuilder addKey retType
                constantsBuilder addKeys args

              case AtomicType(predicateName, args) =>
                val atomSignature = AtomSignature(predicateName, args.size)

                kbBuilder.predicateSchema().get(atomSignature) match {
                  case None =>
                    kbBuilder.predicateSchema += (atomSignature -> (for (element <- args) yield element))
                    constantsBuilder addKeys args

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
    val kbParser = new KBParser(kbBuilder.predicateSchema(), kbBuilder.functionSchema(), dynamicAtomBuilders, dynamicFunctionBuilders)

    var formulas = Set[WeightedFormula]()
    var definiteClauses = Set[WeightedDefiniteClause]()
    val queue = mutable.Queue[IncludeFile]()

    val kbExpressions = Try(kbParser.parseAll(kbParser.mln, fileReader)).map {
      case kbParser.Success(exprs, _) => exprs.asInstanceOf[Iterable[MLNExpression]]
      case x => fatal(s"Can't parse the following expression: '$x' in file: '$kbFile'")
    } match {
      case Success(expressions) => expressions
      case Failure(ex) => fatal(ex.getMessage)
    }

    /*val kbExpressions: Iterable[MLNExpression] = kbParser.parseAll(kbParser.mln, fileReader) match {
      case kbParser.Success(exprs, _) => exprs.asInstanceOf[Iterable[MLNExpression]]
      case x => fatal(s"Can't parse the following expression: '$x' in file: '$kbFile'")
    }*/

    def processMLNExpression(expr: MLNExpression): Unit = expr match {
      case f: WeightedFormula =>
        if(f.weight == 0.0)
          warn(s"Ignoring zero weighted formula '${f.toText}'")
        else {
          formulas += f
          storeUndefinedConstants(f)
        }
      case c: WeightedDefiniteClause =>
        if (c.weight == 0.0)
          warn(s"Ignoring zero weighted definite clause '${c.toText}'")
        else {
          definiteClauses += c
          storeUndefinedConstants(c.clause.body)
          storeUndefinedConstants(c.clause.head)
        }
      case inc: IncludeFile => queue += inc
      case _ => warn(s"Ignoring expression: '$expr'")
    }

    kbExpressions.foreach(processMLNExpression)

    val sep = System.getProperty("file.separator")

    while (queue.nonEmpty) {
      val inc = queue.dequeue()
      val incFile = {
        var tmp = new File(inc.filename)
        if (tmp.exists) tmp
        else {
          tmp = new File(kbFile.getParent + sep + inc.filename)
          if (tmp.exists) tmp
          else fatal(s"Cannot find file '${inc.filename}'")
        }
      }

      val curr_expressions: Iterable[MLNExpression] = kbParser.parseAll(kbParser.mln, new FileReader(incFile)) match {
        case kbParser.Success(exprs, _) => exprs.asInstanceOf[Iterable[MLNExpression]]
        case x => fatal(s"Can't parse the following expression: '$x' in file: '$incFile'")
      }

      curr_expressions.foreach(processMLNExpression)

    }

    /*val resultingFormulas = Future{
      PredicateCompletion(formulas, definiteClauses, pcMode)(kbBuilder.predicateSchema(), kbBuilder.functionSchema())
    }*/

    // In case that some predicates are eliminated by the predicate completion,
    // remove them from the final predicate schema.
    /*val resultingPredicateSchema = Future {
        pcMode match {
        case Simplification =>
          val resultingFormulas = kbBuilder.predicateSchema() -- definiteClauses.map(_.clause.head.signature)
          if(resultingFormulas.isEmpty)
            warn("The given theory is empty (i.e., contains empty set of non-zeroed formulas).")

          resultingFormulas
        case _ => kbBuilder.predicateSchema()
      }
    }*/

    whenDebug {
      constantsBuilder().foreach(entry => debug(s"|${entry._1}|=${entry._2.size}"))
    }

    val kb = kbBuilder
      //.withFormulas(Await.result(resultingFormulas, Duration.Inf))
      .withFormulas(formulas)
      .withDefiniteClauses(definiteClauses)
      //.withPredicateSchema(Await.result(resultingPredicateSchema, Duration.Inf))
      .withDynamicPredicates(kbParser.getDynamicPredicates)
      .withDynamicFunctions(kbParser.getDynamicFunctions)
      .result()

    info(s"Total formulas: ${kb.formulas.size}")

    (kb, constantsBuilder)
  }


}
