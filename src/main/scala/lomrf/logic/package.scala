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

package lomrf


import lomrf.mln.model.{FunctionSchema, PredicateSchema}

import scala.collection.mutable
import lomrf.logic.dynamic._
import scala.annotation.tailrec
import scala.collection.breakOut
import scala.reflect._


package object logic {

  type Theta = Map[Term, Term]

  trait TermIterable extends Iterable[Term]


  /**
   * Gives (if exists) the most general predicate, for example consider the following cases:
   * <ul>
   * <li> The mgp of InitiatedAt(meet(x,y),t) and InitiatedAt(meet(x,A),t) is:  InitiatedAt(meet(x,y),t) </li>
   * <li> The mgp of InitiatedAt(meet(A,y),t) and InitiatedAt(meet(x,A),t) is:  InitiatedAt(meet(x,y),t) </li>
   * <li> The mgp of InitiatedAt(meet(A,y),t) and InitiatedAt(meet(A,B),t) is:  InitiatedAt(meet(A,y),t) </li>
   * <li> The mgp of InitiatedAt(meet(x,y),t) and InitiatedAt(f,t)  is:  InitiatedAt(f,t) </li>
   * </ul>
   *
   * @param atom1 first atom
   * @param atom2 second atom
   *
   * @return the generalisation of the given atoms
   */
  def generalisation(atom1: AtomicFormula, atom2: AtomicFormula)
                    (implicit predicateSchema: PredicateSchema, functionSchema: FunctionSchema): Option[AtomicFormula] = {

    if (atom1.signature != atom2.signature) None //the signatures are different, thus MGP cannot be applied.
    else if (atom1 == atom2) Some(atom1) //comparing the same atom
    else generalisationOf(atom1, atom2)
  }

  @inline
  private def generalisationOf(atom1: AtomicFormula, atom2: AtomicFormula)
                              (implicit predicateSchema: PredicateSchema, functionSchema: FunctionSchema): Option[AtomicFormula] = {

    val generalizedArgs: Vector[Term] =
      (for ((pair, idx) <- atom1.terms.zip(atom2.terms).zipWithIndex) yield pair match {
        case (v: Variable, _) => v
        case (_, v: Variable) => v
        case (c1: Constant, c2: Constant) =>
          if (c1 == c2) c1
          else {
            val typeName = predicateSchema(atom1.signature)(idx)
            Variable("var_" + idx, typeName, idx)
          }
        case (f1: TermFunction, f2: TermFunction) => generalisationOf(f1, f2).getOrElse(return None)
        case _ => return None
      })(breakOut)


    Some(AtomicFormula(atom1.symbol, generalizedArgs))
  }

  private def generalisationOf(f1: TermFunction, f2: TermFunction, level: Int = 0)
                              (implicit functionSchema: FunctionSchema): Option[TermFunction] = {
    val generalizedArgs: Vector[Term] =
      (for ((pair, idx) <- f1.terms.zip(f2.terms).zipWithIndex) yield pair match {
        case (v: Variable, _) => v
        case (_, v: Variable) => v
        case (c1: Constant, c2: Constant) =>
          if (c1 == c2) c1
          else {
            val typeName = functionSchema(f1.signature)._2(idx)
            Variable("var_" + "l" + level + "i" + idx, typeName, idx)
          }
        case (f1: TermFunction, f2: TermFunction) => generalisationOf(f1, f2, level + 1).getOrElse(return None)
        case _ => return None
      })(breakOut)


    Some(TermFunction(f1.symbol, generalizedArgs, f1.domain))
  }


  /**
   * Collects all variables from a list of terms
   *
   * @param terms input list of terms
   * @return The resulting set of variables found in the given list of terms, or an empty set if none is found.
   */
  def uniqueVariablesIn(terms: TermIterable): Set[Variable] = {
    val queue = mutable.Queue[Term]()
    queue ++= terms
    var result = Set[Variable]()

    while (queue.nonEmpty) queue.dequeue() match {
      case v: Variable => result += v
      case f: TermFunction => queue ++= f.terms
      case _ => //do nothing
    }
    result
  }

  def variablesIn(terms: TermIterable): Vector[Variable] = {
    val stack = mutable.Stack[Term]()
    stack.pushAll(terms)

    var result = Vector[Variable]()

    while (stack.nonEmpty) stack.pop() match {
      case v: Variable => result ++= Vector(v)
      case f: TermFunction => stack.pushAll(f.terms)
      case _ => //do nothing
    }

    result
  }

  def uniqueOrderedVariablesIn(sources: Iterable[TermIterable]): Vector[Variable] = {
    val stack = mutable.Stack[Term]()

    var variables = Set[Variable]()
    var result = Vector[Variable]()

    for (entry <- sources) {
      //stack.pushAll(literal.sentence.terms)
      stack.pushAll(entry)

      while (stack.nonEmpty) stack.pop() match {
        case v: Variable if !variables.contains(v) =>
          variables += v
          result ++= Vector(v)
        case f: TermFunction =>
          stack.pushAll(f.terms)
        case _ => //do nothing
      }

    }

    result
  }

  def uniqueOrderedVariablesIn(source: TermIterable): Vector[Variable] = {
    val stack = mutable.Stack[Term]()

    var variables = Set[Variable]()
    var result = Vector[Variable]()

    //stack.pushAll(atom.terms)
    stack.pushAll(source)

    while (stack.nonEmpty) stack.pop() match {
      case v: Variable if !variables.contains(v) =>
        variables += v
        result ++= Vector(v)
      case f: TermFunction => stack.pushAll(f.terms)
      case _ => //do nothing
    }



    result
  }


  /**
   * Collects all variables from a given list of term lists.
   *
   * @param termLists input list of term list
   *
   * @return The resulting set of variables found in the given lists of terms, or an empty set if none is found.
   */
  def uniqueVariables(termLists: Iterable[TermIterable]): Set[Variable] = {
    /**
     * Recursively collect all variables from list of term list
     *
     * @param terms list of term list
     * @param variables the current set of variables found from previous runs
     * @return the resulting set of variables
     */
    @tailrec
    def variablesRec(terms: Iterable[TermIterable], variables: Set[Variable]): Set[Variable] = {
      if (terms.nonEmpty)
        variablesRec(terms.tail, uniqueVariablesIn(terms.head) ++ variables)
      else variables
    }

    // Start collecting the variables recursively. Initially the set of variables is empty.
    variablesRec(termLists, Set.empty)
  }

  def uniqueConstantsIn(terms: TermIterable): Set[Constant] = {
    val queue = mutable.Queue[Term]()
    queue ++= terms
    var result = Set[Constant]()

    while (queue.nonEmpty) queue.dequeue() match {
      case c: Constant => result += c
      case f: TermFunction => queue ++= f.terms
      case _ => //do nothing
    }
    result
  }

  def uniqueConstants(termLists: Iterable[TermIterable]): Set[Constant] = {
    @tailrec
    def constantsRec(terms: Iterable[TermIterable], constants: Set[Constant]): Set[Constant] = {
      if (terms.nonEmpty)
        constantsRec(terms.tail, uniqueConstantsIn(terms.head) ++ constants)
      else constants
    }

    // Start collecting the constants recursively. Initially the set of constants is empty.
    constantsRec(termLists, Set.empty)
  }

  def uniqueFunctionsIn(terms: TermIterable): Set[TermFunction] = {
    val queue = mutable.Queue[Term]()
    queue ++= terms
    var result = Set[TermFunction]()

    while (queue.nonEmpty) queue.dequeue() match {
      case f: TermFunction =>
        result += f
        queue ++= f.terms
      case _ => //do nothing
    }
    result
  }

  def uniqueFunctionsInLists(termLists: Iterable[TermIterable]): Set[TermFunction] = {
    @tailrec
    def functionsRec(terms: Iterable[TermIterable], functions: Set[TermFunction]): Set[TermFunction] = {
      if (terms.nonEmpty)
        functionsRec(terms.tail, uniqueFunctionsIn(terms.head) ++ functions)
      else functions
    }

    // Start collecting the functions recursively. Initially the set of functions is empty.
    functionsRec(termLists, Set.empty)
  }


  def flatMatchedTerms(sources: Iterable[TermIterable], matcher: Term => Boolean): Vector[Term] = {
    val queue = mutable.Queue[Term]()

    var result = Vector[Term]()

    for (entry <- sources) {
      queue ++= entry
      //queue ++= literal.sentence.terms

      while (queue.nonEmpty) {
        val candidate = queue.dequeue()
        if (matcher(candidate))
          result ++= Vector(candidate)
        else candidate match {
          case f: TermFunction => queue ++= f.terms
          case _ => //do nothing
        }
      }
    }

    result
  }


  def uniqFlatMatchedTerms(sources: Iterable[TermIterable], matcher: Term => Boolean): Vector[Term] = {
    val queue = mutable.Queue[Term]()

    var memory = Set[Term]()
    var result = Vector[Term]()

    for (entry <- sources) {
      queue ++= entry
      //queue ++= literal.sentence.terms

      while (queue.nonEmpty) {
        val candidate = queue.dequeue()

        if (matcher(candidate) && !memory.contains(candidate)) {
          result ++= Vector(candidate)
          memory += candidate
        }
        else candidate match {
          case f: TermFunction => queue ++= f.terms
          case _ => //do nothing
        }
      }
    }

    result
  }


  def matchedTerms(sources: Iterable[TermIterable], matcher: Term => Boolean)(implicit tag: ClassTag[TermIterable]): Vector[Vector[Term]] = {
    val stack = mutable.Stack[Term]()

    var result = Vector[Vector[Term]]()

    for (entry <- sources) {
      stack.pushAll(entry)
      //stack.pushAll(literal.sentence.terms)
      var matchedTerms = Vector[Term]()

      while (stack.nonEmpty) {
        val candidate = stack.pop()
        if (matcher(candidate))
          matchedTerms ++= Vector(candidate)
        else candidate match {
          case f: TermFunction => stack.pushAll(f.terms)
          case _ => //do nothing
        }
      }
      result ++= Vector(matchedTerms)
    }

    result
  }

  def matchedTerms(terms: TermIterable, matcher: Term => Boolean): Vector[Term] = {
    var matchedTerms = Vector[Term]()
    val stack = mutable.Stack[Term]()

    stack.pushAll(terms)

    while (stack.nonEmpty) {
      val candidate = stack.pop()
      if (matcher(candidate))
        matchedTerms ++= Vector(candidate)
      else candidate match {
        case f: TermFunction => stack.pushAll(f.terms)
        case _ => //do nothing
      }
    }

    matchedTerms
  }


  def variableLeafs(terms: TermIterable): Vector[Variable] = {
    val stack = mutable.Stack[Term]()
    stack.pushAll(terms)

    var result = Vector[Variable]()
    while (stack.nonEmpty) stack.pop() match {
      case f: TermFunction => stack.pushAll(f.terms)
      case t: Variable => result ++= Vector(t)
      case _ => //do nothing
    }

    result
  }

  def constantLeafs(terms: TermIterable): Vector[Constant] = {
    val stack = mutable.Stack[Term]()
    stack.pushAll(terms)

    var result = Vector[Constant]()
    while (stack.nonEmpty) stack.pop() match {
      case f: TermFunction => stack.pushAll(f.terms)
      case t: Constant => result ++= Vector(t)
      case _ => //do nothing
    }

    result
  }



  def uniqueVariableLeafs(terms: TermIterable): Set[Variable] = {
    val queue = mutable.Queue[Term]()
    queue ++= terms
    var result = Set[Variable]()

    while (queue.nonEmpty) queue.dequeue() match {
      case f: TermFunction => queue ++= f.terms
      case v: Variable => result += v
      case _ => //do nothing
    }
    result
  }

  def uniqueConstantLeafs(terms: TermIterable): Set[Constant] = {
    val queue = mutable.Queue[Term]()
    queue ++= terms
    var result = Set[Constant]()

    while (queue.nonEmpty) queue.dequeue() match {
      case f: TermFunction => queue ++= f.terms
      case v: Constant => result += v
      case _ => //do nothing
    }
    result
  }

  def variabilizeAtom(atom: AtomicFormula)(implicit predicateSchema: PredicateSchema, functionSchema: FunctionSchema): AtomicFormula ={
    var anonVarCounter = 0

    def variabilizeTerms(terms: Vector[_ <: Term], currentSchema: Seq[String]): Vector[_ <: Term] = {
      terms.zip(currentSchema).map {
        case (v: Variable, t: String) => v
        case (c: Constant, t: String) =>
          val symbol = "var_"+anonVarCounter
          anonVarCounter += 1
          Variable(symbol, t)

        case (f: TermFunction, t: String) =>
          val schemaOfTerms = functionSchema(f.signature)._2
          val fTerms = variabilizeTerms(f.terms, schemaOfTerms)
          TermFunction(f.symbol, fTerms, f.domain)
      }
    }

    val variabilizedTerms = variabilizeTerms(atom.terms, predicateSchema(atom.signature))
    atom.copy(terms = variabilizedTerms)
  }




  object predef {

    val dynAtomBuilders: Map[AtomSignature, DynamicAtomBuilder] = List(
      DynEqualsBuilder(), DynLessThanBuilder(), DynLessThanEqBuilder(),
      DynGreaterThanBuilder(), DynGreaterThanEqBuilder(), DynSubstringBuilder()
    ).map(builder => builder.signature -> builder).toMap


    val dynAtoms: Map[AtomSignature, Vector[String] => Boolean] =
      dynAtomBuilders.map { case (signature, builder) => signature -> builder.stateFunction }


    val dynFunctionBuilders: Map[AtomSignature, DynamicFunctionBuilder] = List(
      DynSuccFunctionBuilder(), DynPrecFunctionBuilder(), DynPlusFunctionBuilder(),
      DynMinusFunctionBuilder(), DynTimesFunctionBuilder(), DynDividedByFunctionBuilder(),
      DynModFunctionBuilder(), DynConcatFunctionBuilder()
    ).map(builder => builder.signature -> builder).toMap

    val dynFunctions: Map[AtomSignature, Vector[String] => String] =
      dynFunctionBuilders.map { case (signature, builder) => signature -> builder.resultFunction }

  }


}
