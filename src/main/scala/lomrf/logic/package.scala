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


package object logic {

  type Theta = Map[Term, Term]

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
  def uniqueVariablesIn(terms: Iterable[_ <: Term]): Set[Variable] = {
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

  def variablesIn(terms: Iterable[_ <: Term]): Vector[Variable] = {
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

  def uniqueOrderedVariablesIn(literals: Iterable[Literal]): Vector[Variable] = {
    val stack = mutable.Stack[Term]()

    var variables = Set[Variable]()
    var result = Vector[Variable]()

    for (literal <- literals) {
      stack.pushAll(literal.sentence.terms)

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

  def uniqueOrderedVariablesIn(atom: AtomicFormula): Vector[Variable] = {
    val stack = mutable.Stack[Term]()

    var variables = Set[Variable]()
    var result = Vector[Variable]()

    stack.pushAll(atom.terms)

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
   * @return The resulting set of variables found in the given lists of terms, or an empty set if none is found.
   */
  def uniqueVariablesInLists(termLists: Iterable[Iterable[_ <: Term]]): Set[Variable] = {
    /**
     * Recursively collect all variables from list of term list
     *
     * @param terms list of term list
     * @param variables the current set of variables found from previous runs
     * @return the resulting set of variables
     */
    @tailrec
    def variablesRec(terms: Iterable[Iterable[_ <: Term]], variables: Set[Variable]): Set[Variable] = {
      if (terms.nonEmpty)
        variablesRec(terms.tail, uniqueVariablesIn(terms.head) ++ variables)
      else variables
    }

    // Start collecting the variables recursively. Initially the set of variables is empty.
    variablesRec(termLists, Set.empty)
  }

  def uniqueConstantsIn(terms: Iterable[_ <: Term]): Set[Constant] = {
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

  def uniqueConstantsInLists(termLists: Iterable[Iterable[_ <: Term]]): Set[Constant] = {
    @tailrec
    def constantsRec(terms: Iterable[Iterable[_ <: Term]], constants: Set[Constant]): Set[Constant] = {
      if (terms.nonEmpty)
        constantsRec(terms.tail, uniqueConstantsIn(terms.head) ++ constants)
      else constants
    }

    // Start collecting the constants recursively. Initially the set of constants is empty.
    constantsRec(termLists, Set.empty)
  }

  def uniqueFunctionsIn(terms: Iterable[_ <: Term]): Set[TermFunction] = {
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

  def uniqueFunctionsInLists(termLists: Iterable[Iterable[_ <: Term]]): Set[TermFunction] = {
    @tailrec
    def functionsRec(terms: Iterable[Iterable[_ <: Term]], functions: Set[TermFunction]): Set[TermFunction] = {
      if (terms.nonEmpty)
        functionsRec(terms.tail, uniqueFunctionsIn(terms.head) ++ functions)
      else functions
    }

    // Start collecting the functions recursively. Initially the set of functions is empty.
    functionsRec(termLists, Set.empty)
  }


  def flatMatchedTerms(literals: Iterable[Literal], matcher: Term => Boolean): Vector[Term] = {
    val queue = mutable.Queue[Term]()

    var result = Vector[Term]()

    for (literal <- literals) {
      queue ++= literal.sentence.terms

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


  def uniqFlatMatchedTerms(literals: Iterable[Literal], matcher: Term => Boolean): Vector[Term] = {
    val queue = mutable.Queue[Term]()

    var memory = Set[Term]()
    var result = Vector[Term]()

    for (literal <- literals) {
      queue ++= literal.sentence.terms

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

  def matchedTermsInLiterals(literals: Iterable[Literal], matcher: Term => Boolean): Vector[Vector[Term]] = {
    val stack = mutable.Stack[Term]()

    var result = Vector[Vector[Term]]()

    for (literal <- literals) {
      stack.pushAll(literal.sentence.terms)
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

  def matchedTerms(terms: Iterable[Term], matcher: Term => Boolean): Vector[Term] = {
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


  def leafs[T <: Term](terms: Iterable[_ <: Term]): Vector[T] = {
    val stack = mutable.Stack[Term]()
    stack.pushAll(terms)

    var result = Vector[T]()
    while (stack.nonEmpty) stack.pop() match {
      case f: TermFunction => stack.pushAll(f.terms)
      case t: T => result ++= Vector(t)
      case _ => //do nothing
    }

    result
  }

  def uniqueLeafs[T <: Term](terms: Iterable[_ <: Term]): Set[T] = {
    val queue = mutable.Queue[Term]()
    queue ++= terms
    var result = Set[T]()

    while (queue.nonEmpty) queue.dequeue() match {
      case f: TermFunction => queue ++= f.terms
      case v: T => result += v
      case _ => //do nothing
    }
    result
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
