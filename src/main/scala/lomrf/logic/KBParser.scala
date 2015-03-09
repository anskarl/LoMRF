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

package lomrf.logic

import auxlib.log.Logging
import lomrf.logic.dynamic.{DynamicFunctionBuilder, DynamicAtomBuilder}
import scala.collection.breakOut

/**
 *
 */
final class KBParser(predicateSchema: Map[AtomSignature, Vector[String]],
                             functionSchema: Map[AtomSignature, (String, Vector[String])],
                             dynamicAtomBuilders: Map[AtomSignature, DynamicAtomBuilder] = predef.dynAtomBuilders,
                             dynamicFunctionBuilders: Map[AtomSignature, DynamicFunctionBuilder] = predef.dynFunctionBuilders) extends CommonsMLNParser with Logging {


  private val minPrecedenceLevel = 1
  private val maxPrecedenceLevel = 3

  private var currentTypes = Vector[Variable]()

  private var dynamicPredicates = Map[AtomSignature, Vector[String] => Boolean]()

  private var dynamicFunctions = Map[AtomSignature, Vector[String] => String]()
  private var dynamicFunctionsInstances = Set[TermFunction]()

  def getDynamicPredicates = dynamicPredicates

  def getDynamicFunctions = dynamicFunctions

  def getDynamicFunctionsInstances = dynamicFunctionsInstances

  def mln: Parser[List[MLNExpression]] = rep(sentence)

  def sentence: Parser[MLNExpression] = definiteSentence | folSentence | formula | includeFile

  def folSentence: Parser[WeightedFormula] = deterministicFormula | softFormula

  def definiteSentence: Parser[WeightedDefiniteClause] = deterministicDefiniteClause | softDefiniteClause

  /**
   * A parenthesis may hold a FOL formula
   */
  def parenthesis: Parser[Formula] = "(" ~> formula <~ ")"

  def formula: Parser[Formula] = binary(minPrecedenceLevel) | formulaNoOp

  def formulaNoOp: Parser[Formula] = parenthesis | quantifier | atomicFormula | negatedFormula

  def definiteClause: Parser[DefiniteClause] = {
    atomicFormula ~ ":-" ~ definiteClauseBody ^^ {
      case head ~ ":-" ~ body => DefiniteClause(head, body)
    }
  }

  def definiteClauseBody: Parser[DefiniteClauseConstruct] = conjunctionDefinite | atomicFormula | negatedFormula

  def definiteClauseBodyNoOp = atomicFormula | negatedFormula

  def conjunctionDefinite = definiteClauseBodyNoOp * conjunctionOpDefinite

  def conjunctionOpDefinite: Parser[(DefiniteClauseConstruct, DefiniteClauseConstruct) => DefiniteClauseConstruct] = {
    "^" ^^^ {
      (a: DefiniteClauseConstruct, b: DefiniteClauseConstruct) => And(a, b)
    }
  }

  def softDefiniteClause: Parser[WeightedDefiniteClause] = {
    opt(numDouble) ~ definiteClause ^^ {
      case Some(weight) ~ clause =>
        currentTypes = Vector[Variable]()
        normaliseVariableDomains(clause)
        WeightedDefiniteClause(weight.toDouble, clause)
      case None ~ clause =>
        currentTypes = Vector[Variable]()
        normaliseVariableDomains(clause)
        WeightedDefiniteClause(Double.NaN, clause)
    }
  }

  def deterministicDefiniteClause: Parser[WeightedDefiniteClause] = {
    definiteClause <~ "." ^^ {
      case clause =>
        currentTypes = Vector[Variable]()
        normaliseVariableDomains(clause)
        WeightedDefiniteClause(Double.PositiveInfinity, clause)
    }
  }

  /**
   * A weighted formula, example:
   * 0.78 AvB => C
   */
  def softFormula: Parser[WeightedFormula] = {
    opt(numDouble) ~ formula ^^ {
      case Some(weight) ~ f =>
        currentTypes = Vector[Variable]()
        normaliseVariableDomains(f)
        WeightedFormula(weight.toDouble, f)
      case None ~ f =>
        currentTypes = Vector[Variable]()
        normaliseVariableDomains(f)
        WeightedFormula(Double.NaN, f)
    }
  }

  /**
   * A deterministic (hard) formula always ends with '.' .
   * However, internally in an KB, a deterministic formula is represented as
   * a weighted (soft) formula with infinite weight.
   */
  def deterministicFormula: Parser[WeightedFormula] = {
    currentTypes = Vector[Variable]()
    formula <~ "." ^^ {
      f => {
        normaliseVariableDomains(f)
        WeightedFormula(Double.PositiveInfinity, f)
      }
    }
  }

  private def fetchTypedVariable(symbol: String): Variable = currentTypes.find(v => v.symbol == symbol) match {
    case Some(x) => Variable(symbol, x.domain, x.index)
    case None => Variable(symbol) //sys.error("Cannot determine the type of variable '" + symbol + "'.")
  }

  /**
   * Fixes variables with undefined domains.
   *
   * @param formula the formula to fix
   */
  private[logic] def normaliseVariableDomains(formula: Formula) {

    val (undefinedDomainVars, definedDomainVars) = formula.variables.partition(_.domainName == Variable.UNDEFINED_DOMAIN)
    /*println("--- > > > " + formula.toText
      + "\n--- > > > " + undefinedDomainVars
      + "\n--- > > > " + definedDomainVars)*/

    if (undefinedDomainVars.nonEmpty) {

      val definedDomainVarMap = definedDomainVars.map(v => (v.symbol, v.domainName)).toMap
      for (currentVar <- undefinedDomainVars) {
        definedDomainVarMap.get(currentVar.symbol) match {
          case Some(domainName) => currentVar.domainName = domainName
          case None => sys.error("Cannot determine the domain of variable '" + currentVar.toText + "' in (sub)formula '" + formula.toText + "'.")
        }
      }
    }
  }

  def atomicFormula = atomicFormulaPrefix | dynamicAtomicFormulaInfix

  /**
   * An atomic formula (or atom) starts with an uppercase letter and it is followed by parenthesis,
   * which contains a list of terms (variables, constants or functions).
   */
  private def atomicFormulaPrefix: Parser[AtomicFormula] = (lowerCaseID | upperCaseID) ~ "(" ~ repsep(functionArg | lowerCaseID | upperCaseID, ",") ~ ")" ^^ {
    case name ~ "(" ~ arguments ~ ")" =>

      val atomSignature = AtomSignature(name, arguments.size)

      dynamicAtomBuilders.get(atomSignature) match {
        case Some(atomBuilder) =>
          val termList: Vector[Term] =
            (for (element <- arguments) yield  element match {
                case upperCaseID(s, _*) => Constant(s)
                case lowerCaseID(v, _*) => fetchTypedVariable(v)
                case func: TermFunction => func
                case _ => sys.error("Cannot parse the symbol: " + element)
              })(breakOut)

          dynamicPredicates += (atomSignature -> atomBuilder.stateFunction)
          atomBuilder(termList)
        case None =>
          //the atomicFormula is a common used-defined predicate
          val argTypes: Vector[String] = predicateSchema.get(atomSignature) match {
            case Some(x) => x
            case _ => sys.error("The predicate: " + atomSignature + " is not defined.")
          }
          val termList: Vector[Term] =
            (for ((element, argType: String) <- arguments.zip(argTypes)) yield element match {

                case upperCaseID(s, _*) => Constant(s)

                case lowerCaseID(v, _*) =>
                  val variable = Variable(v, argType)
                  currentTypes = Vector(variable) ++: currentTypes
                  variable

                case func: TermFunction =>
                  if (!func.isDomainDefined) {
                    val result = TermFunction(func.symbol, func.terms, argType)
                    dynamicFunctionsInstances += result
                    result
                  }
                  else if (func.domain == argType) func
                  else sys.error("The function " + func + " returns: " + func.domain + ", while expecting: " + argType)

                case _ => sys.error("Cannot parse the symbol: " + element)
              }
            )(breakOut)
          AtomicFormula(name, termList)
      }
  }

  private def dynamicAtomicFormulaInfix: Parser[AtomicFormula] = eqInfix | neqInfix | ltInfix | gtInfix | ltEqInfix | gtEqInfix

  private def eqInfix: Parser[AtomicFormula] = (lowerCaseID | upperCaseID | functionArg) ~ "=" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
    case left ~ "=" ~ right => this.parsePredicate("equals(" + left + "," + right + ")")
  }

  private def neqInfix: Parser[AtomicFormula] = (lowerCaseID | upperCaseID | functionArg) ~ "=" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
    case left ~ "!=" ~ right => this.parsePredicate("!equals(" + left + "," + right + ")")
  }

  private def ltInfix: Parser[AtomicFormula] = (lowerCaseID | upperCaseID | functionArg) ~ "<" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
    case left ~ "<" ~ right => this.parsePredicate("lessThan(" + left + "," + right + ")")
  }

  private def gtInfix: Parser[AtomicFormula] = (lowerCaseID | upperCaseID | functionArg) ~ ">" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
    case left ~ ">" ~ right => this.parsePredicate("greaterThan(" + left + "," + right + ")")
  }

  private def ltEqInfix: Parser[AtomicFormula] = (lowerCaseID | upperCaseID | functionArg) ~ "=<" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
    case left ~ "=<" ~ right => this.parsePredicate("lessThanEq(" + left + "," + right + ")")
  }

  private def gtEqInfix: Parser[AtomicFormula] = (lowerCaseID | upperCaseID | functionArg) ~ ">=" ~ (lowerCaseID | upperCaseID | functionArg) ^^ {
    case left ~ ">=" ~ right => this.parsePredicate("greaterThanEq(" + left + "," + right + ")")
  }

  def functionArg = functionArgPrefix | functionArgInfix | functionArgSuffix
  /**
   * Parses FOL function symbols, as well as their arguments. The arguments may be constant symbols, variables
   * or even other FOL function definitions. Consequently, recursive function definitions are supported.
   */
  def functionArgPrefix: Parser[TermFunction] = lowerCaseID ~ "(" ~ repsep(lowerCaseID | upperCaseID | functionArg, ",") ~ ")" ^^ {
    case functionName ~ "(" ~ arguments ~ ")" =>

      val functionSignature = AtomSignature(functionName, arguments.size)

      dynamicFunctionBuilders.get(functionSignature) match {
        //Check this function is a special (predefined) function:
        case Some(functionBuilder) =>
          // fetch the function's terms that are defined in its arguments
          val termList: Vector[Term] = (for (term <- arguments) yield term match {
              case upperCaseID(s, _*) => Constant(s)
              case lowerCaseID(v, _*) => fetchTypedVariable(v)
              case func: TermFunction => func
              case _ => sys.error("Cannot parse symbol: " + term)
            })(breakOut)

          //store this special function to specialFunctions HashMap
          dynamicFunctions += (functionSignature -> functionBuilder.resultFunction)

          // Give the resulting function --- that is a function with UNDEFINED return type.
          // The return type will be determined later inside the method: "atomicFormula".
          functionBuilder(termList)
        // the function seems to be user-defined (thus, its arguments are typed)
        case None =>
          //Take the function's input/output types from the functionSchema map
          val (retType, argTypes) = functionSchema.get(functionSignature) match {
            case Some(x) => x
            case None => sys.error("The function: " + functionSignature + " is not defined.")
          }
          // fetch the function's terms that are defined in its arguments,
          // and check if their types match with the function argument types
          val termList: Vector[Term] =
            (for ((symbol, argType: String) <- arguments.zip(argTypes)) yield symbol match {
              case upperCaseID(s, _*) => Constant(s)

              case lowerCaseID(v, _*) =>
                val variable = Variable(v, argType)
                currentTypes = Vector(variable) ++: currentTypes //NEW
                variable

              case func: TermFunction =>
                //if the function is user-defined (thus it has a return type),
                //check if its return type is the same with the corresponding argument type.
                if (func.isDomainDefined && func.domain != argType)
                  sys.error("The function " + func + " returns: " + func.domain + ", while expecting: " + argType)

                // Everything seems to be fine, give the function.
                func

              case _ => sys.error("Cannot parse the symbol: " + symbol)

            })(breakOut)

          TermFunction(functionName, termList, retType)
      } //end
  }

  def functionArgInfix = plusInfix | minusInfix | timesInfix | dividedByInfix | modInfix

  def plusInfix = (lowerCaseID | upperCaseID) ~ "+" ~ (lowerCaseID | upperCaseID ) ^^ {
    case left ~ "+" ~ right => this.parseFunction("plus(" + left + "," + right + ")")
  }

  def minusInfix = (lowerCaseID | upperCaseID ) ~ "-" ~ (lowerCaseID | upperCaseID ) ^^ {
    case left ~ "-" ~ right => this.parseFunction("minus(" + left + "," + right + ")")
  }

  def timesInfix = (lowerCaseID | upperCaseID ) ~ "*" ~ (lowerCaseID | upperCaseID ) ^^ {
    case left ~ "*" ~ right => this.parseFunction("times(" + left + "," + right + ")")
  }

  def dividedByInfix = (lowerCaseID | upperCaseID ) ~ "/" ~ (lowerCaseID | upperCaseID ) ^^ {
    case left ~ "/" ~ right => this.parseFunction("dividedBy(" + left + "," + right + ")")
  }

  def modInfix = (lowerCaseID | upperCaseID) ~ "%" ~ (lowerCaseID | upperCaseID ) ^^ {
    case left ~ "%" ~ right => this.parseFunction("mod(" + left + "," + right + ")")
  }

  def functionArgSuffix = succSuffix | prevSuffix

  def succSuffix = (lowerCaseID | upperCaseID) ~ "++" ^^{
    case left ~ "++" => this.parseFunction("succ(" + left  + ")")
  }

  def prevSuffix = (lowerCaseID | upperCaseID) ~ "--" ^^{
    case left ~ "++" => this.parseFunction("prev(" + left  + ")")
  }

  /**
   * Conjunction (AND) between two formulas
   */
  def conjunction: Parser[And] = formula ~ "^" ~ formula ^^ {
    case leftFormula ~ "^" ~ rightFormula => And(leftFormula, rightFormula)
  }

  /**
   * Disjunction (OR) between two formulas
   */
  def disjunction: Parser[Or] = formula ~ "v" ~ formula ^^ {
    case leftFormula ~ "v" ~ rightFormula => Or(leftFormula, rightFormula)
  }


  def implication: Parser[Implies] = formula ~ "=>" ~ formula ^^ {
    case leftFormula ~ "=>" ~ rightFormula => Implies(leftFormula, rightFormula)
  }


  def equivalence: Parser[Equivalence] = formula ~ "<=>" ~ formula ^^ {
    case leftFormula ~ "<=>" ~ rightFormula => Equivalence(leftFormula, rightFormula)
  }

  /**
   * Negated formula, example:
   * !(AvB)=>C
   */
  def negatedFormula: Parser[Not] = "!" ~ atomicFormula ^^ {
    case "!" ~ f => Not(f)
  } |
    "!" ~ "(" ~ formula ~ ")" ^^ {
      case "!" ~ "(" ~ f ~ ")" => Not(f)
    }

  //Quantifiers
  def quantifier: Parser[Quantifier] = ("Exist" | "Forall") ~ repsep(lowerCaseID, ",") ~ formula ^^ {
    case "Exist" ~ listVarStr ~ formula =>
      val vars = makeVars(listVarStr, formula)
      if (vars.size > 1) {
        val subListSize = vars.size - 1
        vars.take(subListSize).foldRight(ExistentialQuantifier(vars(subListSize), formula))((v: Variable, e: ExistentialQuantifier) => ExistentialQuantifier(v, e))
      }
      else {
        ExistentialQuantifier(vars(0), formula)
      }

    case "Forall" ~ listVarStr ~ formula =>
      val vars = makeVars(listVarStr, formula)
      if (vars.size > 1) {
        val subListSize = vars.size - 1
        vars.take(subListSize).foldRight(UniversalQuantifier(vars(subListSize), formula))((v: Variable, e: UniversalQuantifier) => UniversalQuantifier(v, e))
      }
      else {
        UniversalQuantifier(vars(0), formula)
      }
  }

  /**
   * KB #include definition, for including additional files.
   */
  def includeFile: Parser[IncludeFile] =
    ("#include" ~> quote ~ includeID ~ quote) ^^ {
      case q1 ~ filePath ~ q2 => IncludeFile(filePath)
    }

  private def makeVars(listVarNames: List[String], f: Formula) = for {
    v <- listVarNames
    result = f.variables.find(_.symbol == v) match {
      case Some(x) => Variable(x.symbol, x.domain)
      case None => throw new IllegalStateException("The variable " + v + " is not defined!")
    }
  } yield result

  // Operators
  // http://jim-mcbeath.blogspot.com/2008/09/scala-parser-combinators.html
  private def binary(level: Int): Parser[Formula] =
    if (level > maxPrecedenceLevel) formulaNoOp
    else binary(level + 1) * logicalOperator(level)

  private def logicalOperator(level: Int): Parser[(Formula, Formula) => Formula] = level match {
    case 1 =>
      "=>" ^^^ {
        (a: Formula, b: Formula) => Implies(a, b)
      } |
        "<=>" ^^^ {
          (a: Formula, b: Formula) => Equivalence(a, b)
        }
    case 2 => "v" ^^^ {
      (a: Formula, b: Formula) => Or(a, b)
    }
    case 3 => "^" ^^^ {
      (a: Formula, b: Formula) => And(a, b)
    }
    case _ => sys.error("logicalOperator: wrong precedence level: " + level)
  }


  //--------------------------------------------------------------------------------------------------------------------
  //--- Public API with utility functions for parsing
  //--------------------------------------------------------------------------------------------------------------------



  /**
   * Parses an individual formula from text
   *
   * @param src string representation of the formula
   * @return the resulting formula
   */
  def parseFormula(src: String): Formula = parse(sentence, src) match {
    case Success(expr, _) if expr.isInstanceOf[Formula] => expr.asInstanceOf[Formula]
    case x => fatal("Can't parse the following expression: " + x)
  }

  /**
   * Parses an individual definite clause from text
   *
   * @param src string representation of the definite clause
   * @return the resulting definite clause
   */
  def parseDefiniteClause(src: String): WeightedDefiniteClause = parse(sentence, src) match {
    case Success(expr, _) if expr.isInstanceOf[WeightedDefiniteClause] => expr.asInstanceOf[WeightedDefiniteClause]
    case x => fatal("Can't parse the following expression: " + x)
  }

  /**
   * Parses an individual predicate (atom) from text
   *
   * @param src string representation of the atom
   * @return the resulting atom
   */
  def parsePredicate(src: String): AtomicFormula = parse(atomicFormula, src) match {
    case Success(expr, _) if expr.isInstanceOf[AtomicFormula] => expr
    case x => fatal("Can't parse the following expression as an Atomic Formula: " + x)
  }

  def parseFunction(src: String): TermFunction = parse(functionArg, src) match {
    case Success(expr, _) if expr.isInstanceOf[TermFunction] => expr
    case x => fatal("Can't parse the following expression as a Function: " + x)
  }

  /**
   * Parses an individual literal (an atom or its negation) from text
   *
   * @param src string representation of the literal
   * @return the resulting literal
   */
  def parseLiteral(src: String): Literal = parse(formula, src) match {
    case Success(expr, _) =>
      expr match {
        case Not(atom) if atom.isInstanceOf[AtomicFormula] => NegativeLiteral(atom.asInstanceOf[AtomicFormula])
        case atom: AtomicFormula => PositiveLiteral(atom)
        case _ => fatal("Can't parse the following expression as a literal: " + expr)
      }
    case x => fatal("Can't parse the following expression as a literal: " + x)
  }

  def parseTheory(theory: String): List[MLNExpression] = {
    parse(mln, theory) match {
      case Success(result, _) => result
      case x => fatal("Can't parse the given theory:\n" + x)
    }
  }


}
