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

package lomrf.logic.parser

import com.typesafe.scalalogging.LazyLogging
import lomrf.logic.dynamic.{ DynamicAtomBuilder, DynamicFunctionBuilder }
import lomrf.logic._
import lomrf.util.logging.Implicits._
import scala.collection.breakOut

final class KBParser(
    predicateSchema: Map[AtomSignature, Vector[String]],
    functionSchema: Map[AtomSignature, (String, Vector[String])],
    dynamicAtomBuilders: Map[AtomSignature, DynamicAtomBuilder] = predef.dynAtomBuilders,
    dynamicFunctionBuilders: Map[AtomSignature, DynamicFunctionBuilder] = predef.dynFunctionBuilders) extends CommonsMLNParser with LazyLogging {

  private val minPrecedenceLevel = 1
  private val maxPrecedenceLevel = 3

  private var currentTypes = Vector.empty[Variable]

  private var dynamicPredicates = Map.empty[AtomSignature, Vector[String] => Boolean]
  private var dynamicFunctions = Map.empty[AtomSignature, Vector[String] => String]
  private var dynamicFunctionsInstances = Set.empty[TermFunction]

  /**
    * @return the dynamic predicate definitions
    */
  def getDynamicPredicates: Map[AtomSignature, Vector[String] => Boolean] = dynamicPredicates

  /**
    * @return the dynamic function definitions
    */
  def getDynamicFunctions: Map[AtomSignature, Vector[String] => String] = dynamicFunctions

  /**
    * @return the ground dynamic functions
    */
  def getDynamicFunctionsInstances: Set[TermFunction] = dynamicFunctionsInstances

  // ---------------------------------------------------
  // Parser functions:
  // ---------------------------------------------------

  /**
    * Parses an MLN definition. An MLN is a sequence of MLN expressions.
    *
    * @return a list of MLN expression
    */
  def mln: Parser[List[MLNExpression]] = rep(sentence)

  /**
    * Parses an MLN expression. An MLN expression can be a definite clause,
    * a first-order logic formula or an include file pointing to another
    * MLN definition.
    *
    * @return an MLN expression
    */
  def sentence: Parser[MLNExpression] =
    definiteSentence | folSentence | formula | includeFile

  // ---------------------------------------------------
  // Include parser functions:
  // ---------------------------------------------------

  /**
    * Parses an include definition for including additional files into
    * the current MLN definition.
    *
    * @example {{{
    *           #include "external.mln"
    *          }}}
    *
    * @return an include file expression
    */
  def includeFile: Parser[IncludeFile] =
    ("#include" ~> quote ~ includeID ~ quote) ^^ {
      case _ ~ filePath ~ _ => IncludeFile(filePath)
    }

  // ---------------------------------------------------
  // Definite sentence parser functions:
  // ---------------------------------------------------

  /**
    * A definite sentence is a weighted definite clause. A weighted definite
    * clause can be a deterministic (hard constrained) definite clause or
    * a soft constrained definite clause.
    *
    * @return a weighted definite clause
    */
  def definiteSentence: Parser[WeightedDefiniteClause] =
    deterministicDefiniteClause | softDefiniteClause

  /**
    * Parses a definite clause. A definite clause is an unweighted construct
    * modeling the behavior of classic definite clauses in logic programming.
    *
    * @example {{{
    *           Ancestor(x,y) :- Parent(x,y)
    *           Polygon(x) :- Square(x)
    *          }}}
    *
    * @return a definite clause
    */
  def definiteClause: Parser[DefiniteClause] = {
    atomicFormula ~ ":-" ~ definiteClauseBody ^^ {
      case head ~ ":-" ~ body => DefiniteClause(head, body)
    }
  }

  /**
    * Parses a soft constraint definite clause.
    *
    * @example {{{
    *           1.245 Ancestor(x,y) :- Parent(x,y)
    *           5.67 Polygon(x) :- Square(x)
    *          }}}
    *
    * @note In case no weight has been specified and the definite
    *       clause is not hard (indicated by a dot) then the
    *       weight is NaN.
    *
    * @return a weighted definite clause
    */
  def softDefiniteClause: Parser[WeightedDefiniteClause] = {
    opt(numDouble) ~ definiteClause ^^ {
      case Some(weight) ~ clause =>
        currentTypes = Vector.empty[Variable]
        normaliseVariableDomains(clause)
        WeightedDefiniteClause(weight.toDouble, clause)

      case None ~ clause =>
        currentTypes = Vector.empty[Variable]
        normaliseVariableDomains(clause)
        WeightedDefiniteClause(Double.NaN, clause)
    }
  }

  /**
    * Parses a hard constraint definite clause.
    *
    * @example {{{
    *           Ancestor(x,y) :- Parent(x,y).
    *           Polygon(x) :- Square(x).
    *          }}}
    *
    * @note A hard constraint definite clause always has a '.' at
    *       the end. However, internally in a KB, a deterministic
    *       definite clause is represented as a soft weighted clause
    *       having infinite weight.
    *
    * @return a weighted definite clause.
    */
  def deterministicDefiniteClause: Parser[WeightedDefiniteClause] = {
    definiteClause <~ "." ^^ {
      clause =>
        currentTypes = Vector.empty[Variable]
        normaliseVariableDomains(clause)
        WeightedDefiniteClause(Double.PositiveInfinity, clause)
    }
  }

  /**
    * Parses a definite clause body. The definite clause body always
    * consists of atomic formulas or negated formulas separated by
    * conjunction operators.
    *
    * @return a definite clause construct
    */
  def definiteClauseBody: Parser[DefiniteClauseConstruct] =
    conjunctionDefinite | atomicFormula | negatedFormula

  private def conjunctionDefinite: Parser[DefiniteClauseConstruct] =
    definiteClauseBodyNoOp * conjunctionOpDefinite

  private def definiteClauseBodyNoOp: Parser[DefiniteClauseConstruct] =
    atomicFormula | negatedFormula

  private def conjunctionOpDefinite: Parser[(DefiniteClauseConstruct, DefiniteClauseConstruct) => DefiniteClauseConstruct] =
    "^" ^^^ { (a: DefiniteClauseConstruct, b: DefiniteClauseConstruct) => And(a, b) }

  // ---------------------------------------------------
  // FOL formula parser functions:
  // ---------------------------------------------------

  /**
    * Parses a FOL sentence, that is a weighted FOL formula. A weighted
    * FOL formula can be a deterministic (hard constrained) FOL formula
    * or a soft constrained FOL formula.
    *
    * @return a weighted formula
    */
  def folSentence: Parser[WeightedFormula] = deterministicFormula | softFormula

  /**
    * Parses a soft constrained FOL formula.
    *
    * @example {{{
    *           0.5 Square(x) v Circle(x) => Polygon(x)
    *          }}}
    *
    * @note In case no weight has been specified and the formula
    *       is not hard (indicated by a dot) then the weight is NaN.
    *
    * @return a weighted formula
    */
  def softFormula: Parser[WeightedFormula] = {
    opt(numDouble) ~ formula ^^ {
      case Some(weight) ~ f =>
        currentTypes = Vector.empty[Variable]
        normaliseVariableDomains(f)
        WeightedFormula(weight.toDouble, f)

      case None ~ f =>
        currentTypes = Vector.empty[Variable]
        normaliseVariableDomains(f)
        WeightedFormula(Double.NaN, f)
    }
  }

  /**
    * Parses a hard constraint FOL formula.
    *
    * @example {{{
    *           Square(x) v Triangle(x) => Polygon(x).
    *          }}}
    *
    * @note A hard constraint FOL formula always has a '.' at
    *       the end. However, internally in a KB, a deterministic
    *       formula is represented as a soft weighted clause
    *       having infinite weight.
    *
    * @return a weighted formula.
    */
  def deterministicFormula: Parser[WeightedFormula] = {
    currentTypes = Vector[Variable]()
    formula <~ "." ^^ {
      f =>
        {
          normaliseVariableDomains(f)
          WeightedFormula(Double.PositiveInfinity, f)
        }
    }
  }

  /**
    * Normalizes variable domains by inferring the domains of variables
    * having undefined domains.
    *
    * @param formula a formula to normalize
    */
  private[logic] def normaliseVariableDomains(formula: Formula) {

    val (undefinedDomainVars, definedDomainVars) =
      formula.variables.partition(_.domainName == Variable.UNDEFINED_DOMAIN)

    if (undefinedDomainVars.nonEmpty) {
      val definedDomainVarMap = definedDomainVars.map(v => v.symbol -> v.domainName).toMap

      for (currentVar <- undefinedDomainVars) definedDomainVarMap.get(currentVar.symbol) match {
        case Some(domainName) => currentVar.domainName = domainName
        case None =>
          logger.fatal(s"Cannot determine the domain of variable '${currentVar.toText}' in (sub)formula '${formula.toText}'.")
      }
    }
  }

  /**
    * Parses a FOL formula, that is an unweighted formula construct. A formula
    * construct can contain atomic formulas or negated formulas, logical
    * operators and logical quantifiers (forall and exist).
    *
    * @return a formula construct
    */
  def formula: Parser[FormulaConstruct] = binary(minPrecedenceLevel) | formulaNoOp

  /**
    * @see [[http://jim-mcbeath.blogspot.com/2008/09/scala-parser-combinators.html]]
    *
    * @param level the operator precedence level
    * @return a formula construct
    */
  private def binary(level: Int): Parser[FormulaConstruct] =
    if (level > maxPrecedenceLevel) formulaNoOp
    else binary(level + 1) * logicalOperator(level)

  /**
    * @param level the operator precedence level
    * @return an operator function
    */
  private def logicalOperator(level: Int): Parser[(FormulaConstruct, FormulaConstruct) => FormulaConstruct] = level match {
    case 1 =>
      "=>" ^^^ {
        (a: FormulaConstruct, b: FormulaConstruct) => Implies(a, b)
      } |
        "<=>" ^^^ {
          (a: FormulaConstruct, b: FormulaConstruct) => Equivalence(a, b)
        }
    case 2 => "v" ^^^ {
      (a: FormulaConstruct, b: FormulaConstruct) => Or(a, b)
    }
    case 3 => "^" ^^^ {
      (a: FormulaConstruct, b: FormulaConstruct) => And(a, b)
    }
    case _ => logger.fatal(s"Wrong precedence level: $level in logical operators.")
  }

  /**
    * Parses a conjunction (AND) between a pair of formulas.
    *
    * @example {{{
    *           Formula_A ^ Formula_B
    *           }}}
    *
    * @return a conjunction operator construct
    */
  def conjunction: Parser[And] = formula ~ "^" ~ formula ^^ {
    case leftFormula ~ "^" ~ rightFormula => And(leftFormula, rightFormula)
  }

  /**
    * Parses a disjunction (OR) between a pair formulas.
    *
    * @example {{{
    *           Formula_A v Formula_B
    *           }}}
    *
    * @return a disjunction construct
    */
  def disjunction: Parser[Or] = formula ~ "v" ~ formula ^^ {
    case leftFormula ~ "v" ~ rightFormula => Or(leftFormula, rightFormula)
  }

  /**
    * Parses an implication between a pair of formulas.
    *
    * @example {{{
    *           Formula_A => Formula_B
    *           }}}
    *
    * @return an implication construct
    */
  def implication: Parser[Implies] = formula ~ "=>" ~ formula ^^ {
    case leftFormula ~ "=>" ~ rightFormula => Implies(leftFormula, rightFormula)
  }

  /**
    * Parses an equivalence between a pair of formulas.
    *
    * @example {{{
    *           Formula_A <=> Formula_B
    *           }}}
    *
    * @return an equivalence construct
    */
  def equivalence: Parser[Equivalence] = formula ~ "<=>" ~ formula ^^ {
    case leftFormula ~ "<=>" ~ rightFormula => Equivalence(leftFormula, rightFormula)
  }

  /**
    * Parses a formula construct alone having no connection to other formulas
    * by logical operators. Such constructs can be parenthesis around a formula,
    * a quantifier over variables of a formula, a single atomic formula or
    * a negation of a formula.
    *
    * @return a formula construct
    */
  def formulaNoOp: Parser[FormulaConstruct] =
    parenthesis | quantifier | atomicFormula | negatedFormula

  /**
    * Parses a FOL formula around parenthesis.
    *
    * @return a formula construct
    */
  def parenthesis: Parser[FormulaConstruct] = "(" ~> formula <~ ")"

  /**
    * Parser a logical quantifier over variables of a FOL formula.
    *
    * @return a quantifier construct
    */
  def quantifier: Parser[Quantifier] = (exist | forall) ~ repsep(lowerCaseID, ",") ~ formula ^^ {
    case ("Exist" | "EXIST") ~ listVarStr ~ formula =>
      val vars = makeVars(listVarStr, formula)
      if (vars.size > 1) {
        val subListSize = vars.size - 1
        vars.take(subListSize)
          .foldRight(ExistentialQuantifier(vars(subListSize), formula)) {
            (v: Variable, e: ExistentialQuantifier) => ExistentialQuantifier(v, e)
          }
      } else ExistentialQuantifier(vars.head, formula)

    case ("Forall" | "FORALL") ~ listVarStr ~ formula =>
      val vars = makeVars(listVarStr, formula)
      if (vars.size > 1) {
        val subListSize = vars.size - 1
        vars.take(subListSize)
          .foldRight(UniversalQuantifier(vars(subListSize), formula)) {
            (v: Variable, e: UniversalQuantifier) => UniversalQuantifier(v, e)
          }
      } else UniversalQuantifier(vars.head, formula)
  }

  /**
    * Creates a list of variables given the variable names and
    * the formula they belong to.
    *
    * @param listVarNames a list of variable names
    * @param f a formula containing the given variable names
    *
    * @return a list of variable names
    */
  private def makeVars(listVarNames: List[String], f: FormulaConstruct) = for {
    v <- listVarNames
    result = f.variables.find(_.symbol == v) match {
      case Some(x) => Variable(x.symbol, x.domain)
      case None    => throw new IllegalStateException(s"The variable '$v' is not defined.")
    }
  } yield result

  // ---------------------------------------------------
  // Atomic formula parser functions:
  // ---------------------------------------------------

  /**
    * Parses an atomic formula. The atomic formula can be either a classic
    * logical atom having a name followed by parenthesis containing a list
    * of terms (variables, constants and/or functions) or a dynamic atomic
    * formula that represents numerical operators (extra logical stuff).
    *
    * @note The dynamic atomic formulas are also translated into classic
    *       logical atoms having predefined names.
    *
    * @return an atomic formula
    */
  def atomicFormula: Parser[AtomicFormula] = atomicFormulaPrefix | dynamicAtomicFormulaInfix

  /**
    * Parses a negated (NOT) formula.
    *
    * @example {{{
    *           !Circle(x) => Polygon(x)
    *           !(Black(x) ^ White(x)) => Colorized(x)
    *          }}}
    *
    * @return a negated construct
    */
  def negatedFormula: Parser[Not] =
    "!" ~ atomicFormula ^^ {
      case "!" ~ f => Not(f)
    } |
      "!" ~ "(" ~ formula ~ ")" ^^ {
        case "!" ~ "(" ~ f ~ ")" => Not(f)
      }

  /**
    * Parses an atomic formula (or atom). A logical atom always starts by an
    * uppercase letter and it is followed by parenthesis that contain a list
    * of terms (variables, constants and/or functions).
    *
    * @return an atomic formula
    */
  private def atomicFormulaPrefix: Parser[AtomicFormula] =
    (lowerCaseID | upperCaseID) ~ "(" ~ repsep(functionArg | variableArg | upperCaseID, ",") ~ ")" ^^ {
      case name ~ "(" ~ arguments ~ ")" =>

        val atomSignature = AtomSignature(name, arguments.size)

        dynamicAtomBuilders.get(atomSignature) match {
          case Some(atomBuilder) =>
            val termList: Vector[Term] =
              (for (term <- arguments) yield term match {
                case symbol: String => symbol match {
                  case upperCaseID(s, _*) => Constant(s)
                  case variableArg(v, _*) => fetchTypedVariable(v)
                }
                case func: TermFunction => func
                case _                  => logger.fatal(s"Cannot parse the symbol: $term")
              })(breakOut)

            dynamicPredicates += (atomSignature -> atomBuilder.stateFunction)
            atomBuilder(termList)

          case None =>
            // the atomicFormula is a common used-defined predicate
            val argTypes: Vector[String] = predicateSchema.get(atomSignature) match {
              case Some(x) => x
              case _       => logger.fatal(s"The predicate with signature '${atomSignature.toString}' is not defined.")
            }

            val termList: Vector[Term] =
              (for ((element, argType: String) <- arguments.zip(argTypes)) yield element match {

                case symbol: String => symbol match {
                  case upperCaseID(s, _*) => Constant(s)

                  case variableArg(v, _*) =>
                    val (isFlaggedGroundPerConstant, variableSymbol) =
                      if (v.startsWith("+")) (true, v.drop(1))
                      else (false, v)

                    val variable = Variable(variableSymbol, argType, groundPerConstant = isFlaggedGroundPerConstant)
                    currentTypes = Vector(variable) ++: currentTypes
                    variable

                }
                case func: TermFunction =>
                  if (!func.isDomainDefined) {
                    val result = TermFunction(func.symbol, func.terms, argType)
                    dynamicFunctionsInstances += result
                    result
                  } else if (func.domain == argType) func
                  else logger.fatal(s"The function '${func.toText}' returns '${func.domain}', while expecting to return '$argType'.")

                case _ => logger.fatal(s"Cannot parse the symbol: $element")
              })(breakOut)

            AtomicFormula(name, termList)
        }
    }

  // ---------------------------------------------------
  // Dynamic atomic formula parser functions:
  // ---------------------------------------------------

  private def dynamicAtomicFormulaInfix: Parser[AtomicFormula] =
    eqInfix | neqInfix | ltInfix | gtInfix | ltEqInfix | gtEqInfix

  private def eqInfix: Parser[AtomicFormula] =
    (lowerCaseID | upperCaseID | functionArg) ~ "=" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
      case left ~ "=" ~ right => this.parsePredicate("equals(" + left + "," + right + ")")
    }

  private def neqInfix: Parser[AtomicFormula] =
    (lowerCaseID | upperCaseID | functionArg) ~ "!=" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
      case left ~ "!=" ~ right => this.parsePredicate("notEquals(" + left + "," + right + ")")
    }

  private def ltInfix: Parser[AtomicFormula] =
    (lowerCaseID | upperCaseID | functionArg) ~ "<" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
      case left ~ "<" ~ right => this.parsePredicate("lessThan(" + left + "," + right + ")")
    }

  private def gtInfix: Parser[AtomicFormula] =
    (lowerCaseID | upperCaseID | functionArg) ~ ">" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
      case left ~ ">" ~ right => this.parsePredicate("greaterThan(" + left + "," + right + ")")
    }

  private def ltEqInfix: Parser[AtomicFormula] =
    (lowerCaseID | upperCaseID | functionArg) ~ "=<" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
      case left ~ "=<" ~ right => this.parsePredicate("lessThanEq(" + left + "," + right + ")")
    }

  private def gtEqInfix: Parser[AtomicFormula] =
    (lowerCaseID | upperCaseID | functionArg) ~ ">=" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
      case left ~ ">=" ~ right => this.parsePredicate("greaterThanEq(" + left + "," + right + ")")
    }

  // ---------------------------------------------------
  // Term function parser functions:
  // ---------------------------------------------------

  /**
    * Parses a term function. The term function can be either a classic
    * logical function having a name followed by parenthesis containing a list
    * of terms (variables, constants and/or functions) or a dynamic function
    * that represents numerical operators (extra logical stuff).
    *
    * @note The dynamic functions are also translated into classic
    *       logical functions having predefined names.
    *
    * @return a term function
    */
  def functionArg: Parser[TermFunction] = functionArgPrefix | functionArgInfix | functionArgSuffix

  /**
    * Parses a FOL function symbol. A logical function symbol always starts by
    * a lowercase letter and it is followed by parenthesis that contain a list
    * of terms (variables, constants and/or other FOL functions).
    *
    * @note Recursive function definitions are supported.
    *
    * @return a term function
    */
  private def functionArgPrefix: Parser[TermFunction] =
    lowerCaseID ~ "(" ~ repsep(variableArg | upperCaseID | functionArg, ",") ~ ")" ^^ {
      case functionName ~ "(" ~ arguments ~ ")" =>

        val functionSignature = AtomSignature(functionName, arguments.size)

        dynamicFunctionBuilders.get(functionSignature) match {

          // Case 1. Check this function is a special (predefined) function:
          case Some(functionBuilder) =>
            // fetch the function's terms that are defined in its arguments
            val termList: Vector[Term] = (for (term <- arguments) yield term match {
              case symbol: String => symbol match {
                case upperCaseID(s, _*) => Constant(s)
                case variableArg(v, _*) => fetchTypedVariable(v)
              }
              case func: TermFunction => func
              case _                  => logger.fatal(s"Cannot parse symbol: $term")
            })(breakOut)

            // store the special function to special functions HashMap
            dynamicFunctions += (functionSignature -> functionBuilder.resultFunction)

            logger.debug(s"Parsed dynamic function: $functionSignature with terms: ${termList.mkString(", ")}")

            // Give the resulting function --- that is a function with UNDEFINED return type.
            // The return type will be determined later inside the method: "atomicFormula".
            functionBuilder(termList)

          // Case 2. The function seems to be user-defined (thus, its arguments are typed)
          case None =>
            // Take the function's input/output types from the functionSchema map
            val (retType, argTypes) = functionSchema.get(functionSignature) match {
              case Some(x) => x
              case None    => logger.fatal(s"The function: $functionSignature is not defined.")
            }
            // fetch the function's terms that are defined in its arguments,
            // and check if their types match with the function argument types
            val termList: Vector[Term] =
              (for ((symbol, argType: String) <- arguments.zip(argTypes)) yield symbol match {
                case x: String => x match {
                  case upperCaseID(s, _*) => Constant(s)

                  case variableArg(v, _*) =>
                    val (isFlaggedGroundPerConstant, variableSymbol) =
                      if (v.startsWith("+")) (true, v.drop(1))
                      else (false, v)

                    val variable = Variable(variableSymbol, argType, groundPerConstant = isFlaggedGroundPerConstant)
                    currentTypes = Vector(variable) ++: currentTypes
                    variable
                }
                case func: TermFunction =>
                  // if the function is user-defined (thus it has a return type),
                  // check if its return type is the same with the corresponding argument type.
                  if (func.isDomainDefined && func.domain != argType)
                    logger.fatal(s"The function $func returns: ${func.domain}, while expecting: $argType")

                  // Everything seems to be fine, give the function.
                  func

                case _ => logger.fatal(s"Cannot parse the symbol: $symbol")

              })(breakOut)

            TermFunction(functionName, termList, retType)
        }
    }

  // ---------------------------------------------------
  // Dynamic term function parser functions:
  // ---------------------------------------------------

  private def functionArgInfix: Parser[TermFunction] =
    plusInfix | minusInfix | timesInfix | dividedByInfix | modInfix

  private def plusInfix: Parser[TermFunction] =
    (lowerCaseID | upperCaseID) ~ "+" ~ (lowerCaseID | upperCaseID) ^^ {
      case left ~ "+" ~ right => this.parseFunction("plus(" + left + "," + right + ")")
    }

  private def minusInfix: Parser[TermFunction] =
    (lowerCaseID | upperCaseID) ~ "-" ~ (lowerCaseID | upperCaseID) ^^ {
      case left ~ "-" ~ right => this.parseFunction("minus(" + left + "," + right + ")")
    }

  private def timesInfix: Parser[TermFunction] =
    (lowerCaseID | upperCaseID) ~ "*" ~ (lowerCaseID | upperCaseID) ^^ {
      case left ~ "*" ~ right => this.parseFunction("times(" + left + "," + right + ")")
    }

  private def dividedByInfix: Parser[TermFunction] =
    (lowerCaseID | upperCaseID) ~ "/" ~ (lowerCaseID | upperCaseID) ^^ {
      case left ~ "/" ~ right => this.parseFunction("dividedBy(" + left + "," + right + ")")
    }

  private def modInfix: Parser[TermFunction] =
    (lowerCaseID | upperCaseID) ~ "%" ~ (lowerCaseID | upperCaseID) ^^ {
      case left ~ "%" ~ right => this.parseFunction("mod(" + left + "," + right + ")")
    }

  private def functionArgSuffix: Parser[TermFunction] = succSuffix | prevSuffix

  private def succSuffix: Parser[TermFunction] = (lowerCaseID | upperCaseID) ~ "++" ^^ {
    case left ~ "++" => this.parseFunction("succ(" + left + ")")
  }

  private def prevSuffix: Parser[TermFunction] = (lowerCaseID | upperCaseID) ~ "--" ^^ {
    case left ~ "--" => this.parseFunction("prec(" + left + ")")
  }

  private def fetchTypedVariable(symbol: String): Variable = {
    val (isFlaggedGroundPerConstant, variableSymbol) =
      if (symbol.startsWith("+")) (true, symbol.drop(1))
      else (false, symbol)

    currentTypes.find(v => v.symbol == variableSymbol) match {
      case Some(x) => Variable(variableSymbol, x.domain, index = x.index, groundPerConstant = isFlaggedGroundPerConstant)
      case None    => Variable(variableSymbol)
    }
  }

  //--------------------------------------------------------------------------------------------------------------------
  //--- Public API defining utility functions for parsing
  //--------------------------------------------------------------------------------------------------------------------

  /**
    * Parses a logical sentence given a string. The sentence can be
    * either a logical weighted formula or a weighted definite clause.
    *
    * @param src string representation of the logical sentence
    * @return the resulting formula
    */
  def parseLogicalSentence(src: String): Formula = parse(sentence, src) match {
    case Success(expr, _) if expr.isInstanceOf[Formula] => expr.asInstanceOf[Formula]
    case x => logger.fatal(s"Can't parse the following expression: $x")
  }

  /**
    * Parses an individual weighted formula given as a string.
    *
    * @param src string representation of the weighted sentence
    * @return the resulting weighted formula
    */
  def parseWeightedFormula(src: String): WeightedFormula = parse(folSentence, src) match {
    case Success(expr, _) if expr.isInstanceOf[WeightedFormula] => expr.asInstanceOf[WeightedFormula]
    case x => logger.fatal(s"Can't parse the following expression: $x")
  }

  /**
    * Parses an individual definite clause given as a string.
    *
    * @param src string representation of the definite clause
    * @return the resulting definite clause
    */
  def parseDefiniteClause(src: String): WeightedDefiniteClause = parse(definiteSentence, src) match {
    case Success(expr, _) if expr.isInstanceOf[WeightedDefiniteClause] => expr.asInstanceOf[WeightedDefiniteClause]
    case x => logger.fatal(s"Can't parse the following expression: $x")
  }

  /**
    * Parses an individual predicate (atom) given as a string.
    *
    * @param src string representation of the atom
    * @return the resulting atom
    */
  def parsePredicate(src: String): AtomicFormula = parse(atomicFormula, src) match {
    case Success(expr, _) if expr.isInstanceOf[AtomicFormula] => expr
    case x => logger.fatal(s"Can't parse the following expression as an Atomic Formula: $x")
  }

  /**
    * Parses an individual function given as a string.
    *
    * @param src string representation of the function
    * @return the resulting term function
    */
  def parseFunction(src: String): TermFunction = parse(functionArg, src) match {
    case Success(expr, _) if expr.isInstanceOf[TermFunction] => expr
    case x => logger.fatal(s"Can't parse the following expression as a Function: $x")
  }

  /**
    * Parses an individual literal (an atom or its negation) given as a string.
    *
    * @param src string representation of the literal
    * @return the resulting literal
    */
  def parseLiteral(src: String): Literal = parse(formula, src) match {
    case Success(expr, _) =>
      expr match {
        case Not(atom) if atom.isInstanceOf[AtomicFormula] => NegativeLiteral(atom.asInstanceOf[AtomicFormula])
        case atom: AtomicFormula => PositiveLiteral(atom)
        case _ => logger.fatal(s"Can't parse the following expression as a literal: $expr")
      }
    case x => logger.fatal(s"Can't parse the following expression as a literal: $x")
  }

  /**
    * Parses a whole MLN theory definition given as a string.
    *
    * @param theory string representation of the MLN theory
    * @return a list of resulting MLN expressions
    */
  def parseTheory(theory: String): List[MLNExpression] = {
    parse(mln, theory) match {
      case Success(result, _) => result
      case x                  => logger.fatal(s"Can't parse the given theory:\n$x")
    }
  }
}
