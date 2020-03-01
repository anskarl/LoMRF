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

package lomrf.mln.model

import scala.collection.mutable
import java.io.{ BufferedReader, File, FileReader }
import java.util.regex.Pattern
import com.typesafe.scalalogging.Logger
import lomrf.logic._
import lomrf.util.logging.Implicits._
import lomrf.logic.dynamic.{ DynamicAtomBuilder, DynamicFunctionBuilder }
import lomrf.logic.parser.{ DomainParser, KBParser }
import lomrf.mln.model.builders.{ ConstantsDomainBuilder, KBBuilder }
import lomrf.util.ImplFinder
import scala.util.{ Failure, Success, Try }

/**
  * KB (knowledge base) holds all the components of an logical theory.
  *
  * @note Function signatures are represented using [[lomrf.logic.AtomSignature]].
  *
  * @param predicateSchema a map from atom signatures to their argument domain names
  * @param functionSchema a map from function signatures to their return value domain name, argument domain names
  * @param dynamicPredicates a map from function signatures to functions of the form '''Vector[String] => Boolean'''
  * @param dynamicFunctions a map from function signatures to functions of the form '''Vector[String] => String'''
  * @param formulas a set of (weighted) first-order logic formulas
  * @param definiteClauses a set of (weighted) definite clauses
  */
class KB(
    val predicateSchema: PredicateSchema,
    val functionSchema: FunctionSchema,
    val dynamicPredicates: DynamicPredicates,
    val dynamicFunctions: DynamicFunctions,
    val formulas: Set[WeightedFormula],
    val definiteClauses: Set[WeightedDefiniteClause] = Set.empty) extends Serializable { self =>

  @transient
  lazy val schema = MLNSchema(predicateSchema, functionSchema, dynamicPredicates, dynamicFunctions)

  object signatures {

    @transient
    lazy val predicates: Set[AtomSignature] = predicateSchema.keySet

    @transient
    lazy val function: Set[AtomSignature] = functionSchema.keySet

    @transient
    lazy val dynamicPredicates: Set[AtomSignature] = self.dynamicPredicates.keySet

    @transient
    lazy val dynamicFunction: Set[AtomSignature] = self.dynamicFunctions.keySet
  }

  override def toString: String = {
    "KB = {" +
      "\n\t# Predicate schemas         : " + predicateSchema.size +
      "\n\t# Dynamic predicate schemas : " + dynamicPredicates.size +
      "\n\t# Function schemas          : " + functionSchema.size +
      "\n\t# Dynamic function schemas  : " + dynamicFunctions.size +
      "\n\t# Formulas                  : " + formulas.size +
      "\n\t# Definite clauses          : " + definiteClauses.size +
      "\n\t}"
  }
}

object KB {

  private lazy val logger = Logger(this.getClass)

  private final lazy val IgnoreRegex =
    Pattern.compile("(\\s*\\*.*|/\\*.*|//.*|\\s+)+")

  private final lazy val DomainSchemaRegex =
    Pattern.compile("(^\\s*[a-z]\\w*\\s*=\\s*[{].*[}]\\s*$)|(^\\s*[A-Z][a-zA-Z0-9(, ]*[)]\\s*$)|(^\\s*[a-z][a-zA-Z0-9]*\\s+[a-z][a-zA-Z0-9(, ]*[)]\\s*$)")

  /**
    * Parse a knowledge base from a given file.
    *
    * @param filename a knowledge base filename
    * @param dynamicDefinitions a map of dynamic definitions (default is None)
    * @param convertFunctions convert functions to auxiliary predicates (default is false)
    * @return a KB instance
    */
  def fromFile(
      filename: String,
      dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None,
      convertFunctions: Boolean = false): (KB, ConstantsDomain) = {

    val kbFile = new File(filename)
    if (!kbFile.exists || !kbFile.isFile || !kbFile.canRead)
      logger.fatal(s"Cannot read input MLN file '${kbFile.getPath}'.")

    val fileReader = new BufferedReader(new FileReader(kbFile))

    val commentMatcher = IgnoreRegex.matcher("")
    val domainMatcher = DomainSchemaRegex.matcher("")

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
    var dynamicFunctionBuilders = predef.dynFunctionBuilders // predefined dynamic functions

    for {
      definitionsMap <- dynamicDefinitions // get the user-specified implementations map
      implementations <- definitionsMap.get(classOf[DynamicFunctionBuilder]) // get all user-specified Dynamic Functions Builder
      builder <- implementations.map(_.newInstance().asInstanceOf[DynamicFunctionBuilder]) // instantiate the builder (via reflection)
    } dynamicFunctionBuilders += (builder.signature -> builder) // add the builder to the dynamicFunctionBuilders Map, with key the function signature.

      /**
        * Searches for constant values that appear only in the formulas
        * and not in the evidence.
        *
        * @note Self procedure is not applied to dynamic functions/predicates
        *
        * @param formula source formula
        */
      def storeUndefinedConstants(formula: Formula): Unit = {
        logger.debug(s"Looking for constants in formula: '${formula.toText}'")

          def parseTerm(term: Term, key: String): Unit = term match {
            case Constant(symbol) => constantsBuilder(key) += symbol
            case f: TermFunction if !dynamicFunctionBuilders.contains(f.signature) =>
              val functionArgsSchema = kbBuilder.functionSchema(f.signature)._2

              for ((term, idx) <- f.terms.zipWithIndex) parseTerm(term, functionArgsSchema(idx))

            case _ => // ignore
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
        if (commentMatcher.reset(line).matches()) {
          logger.trace(s"Parsing comment expressions: ${line}")
          fileReader.mark(0)
        } else if (domainMatcher.reset(line).matches()) {
          logger.trace(s"Parsing domain expressions: ${line}")
          fileReader.mark(0)
          domainParser.parse(domainParser.definition, line) match {
            case domainParser.Success(expr, _) => {
              expr match {
                case ConstantTypeDefinition(symbol, cons)      => constantsBuilder ++= (symbol, cons)

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
            }
            case domainParser.Failure(msg, _) => logger.error(msg)
          }
        } else {
          logger.trace(s"Formulas starting from line: ${lineIdx}")
          stop = true
        }
      }

    } // end while

    // ---------------------------------------------------
    // Parse formulas and definite clauses
    // ---------------------------------------------------

    //    fileReader.mark(0)
    fileReader.reset()

    val kbParser = new KBParser(
      kbBuilder.predicateSchema(),
      kbBuilder.functionSchema(),
      dynamicAtomBuilders,
      dynamicFunctionBuilders)

    var formulas = Set[WeightedFormula]()
    var definiteClauses = Set[WeightedDefiniteClause]()
    val queue = mutable.Queue[IncludeFile]()

    val kbExpressions = Try(kbParser.parseAll(kbParser.mln, fileReader)).map {
      case kbParser.Success(expr: Iterable[MLNExpression], _) => expr
      case x => logger.fatal(s"Can't parse the following expression: '$x' in file: '$kbFile'")
    } match {
      case Success(expressions) => expressions
      case Failure(ex)          => logger.fatal(ex.getMessage)
    }

      def processMLNExpression(expr: MLNExpression): Unit = expr match {
        case f: WeightedFormula =>
          if (f.weight == 0.0)
            logger.warn(s"Ignoring zero weighted formula '${f.toText}'")
          else {
            formulas += f
            storeUndefinedConstants(f)
          }
        case c: WeightedDefiniteClause =>
          if (c.weight == 0.0)
            logger.warn(s"Ignoring zero weighted definite clause '${c.toText}'")
          else {
            definiteClauses += c
            storeUndefinedConstants(c.clause.body)
            storeUndefinedConstants(c.clause.head)
          }
        case inc: IncludeFile => queue += inc
        case _                => logger.warn(s"Ignoring expression: '$expr'")
      }

    kbExpressions.foreach(processMLNExpression)

    val sep = System.getProperty("file.separator")

    while (queue.nonEmpty) {
      val inc = queue.dequeue
      val incFile = {
        var tmp = new File(inc.filename)
        if (tmp.exists) tmp
        else {
          tmp = new File(kbFile.getParent + sep + inc.filename)
          if (tmp.exists) tmp
          else logger.fatal(s"Cannot find file '${inc.filename}'")
        }
      }

      val currExpressions: Iterable[MLNExpression] = kbParser.parseAll(kbParser.mln, new FileReader(incFile)) match {
        case kbParser.Success(expr: Iterable[MLNExpression], _) => expr
        case x => logger.fatal(s"Can't parse the following expression: '$x' in file: '$incFile'")
      }

      currExpressions.foreach(processMLNExpression)
    }

    logger.whenDebugEnabled {
      constantsBuilder().foreach(entry => logger.debug(s"|${entry._1}|=${entry._2.size}"))
    }

    val kb = kbBuilder
      .withFormulas(formulas)
      .withDefiniteClauses(definiteClauses)
      .withDynamicPredicates(kbParser.getDynamicPredicates)
      .withDynamicFunctions(kbParser.getDynamicFunctions)
      .result()

    logger.info(s"Total formulas: ${kb.formulas.size}")

    (kb, constantsBuilder.result())
  }
}
