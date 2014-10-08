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

package lomrf

import scala.collection.{SeqLike, IterableLike, mutable}
import lomrf.logic.dynamic._
import lomrf.util.Logging
import scala.annotation.tailrec

/**
 * @author Anastasios Skarlatidis
 */
package object logic extends Logging {

  type Theta = Map[Term, Term]


  def containsSignature(signature: AtomSignature, formula: Formula): Boolean = formula match {
    case atom: AtomicFormula => atom.signature == signature
    case _ =>
      val queue = mutable.Queue[Formula]()
      formula.subFormulas.foreach(queue.enqueue(_))
      while (queue.nonEmpty) {
        val currentFormula = queue.dequeue()
        currentFormula match {
          case atom: AtomicFormula => if (atom.signature == signature) return true
          case _ => currentFormula.subFormulas.foreach(queue.enqueue(_))
        }
      }
      false
  }


  def fetchAtom(signature: AtomSignature, formula: Formula): Option[AtomicFormula] = formula match {
    case atom: AtomicFormula => if (atom.signature == signature) Some(atom) else None
    case _ =>
      val queue = mutable.Queue[Formula]()
      formula.subFormulas.foreach(queue.enqueue(_))
      while (queue.size != 0) {
        val currentFormula = queue.dequeue()
        currentFormula match {
          case atom: AtomicFormula => if (atom.signature == signature) {
            return Some(atom)
          }
          case _ => currentFormula.subFormulas.foreach(f => queue.enqueue(f))
        }
      }
      None
  }

  def extractSignatures(formula: Formula): Set[AtomSignature] = formula match {
    case atom: AtomicFormula => Set(atom.signature)
    case _ =>
      val queue = mutable.Queue[Formula]()
      formula.subFormulas.foreach(queue.enqueue(_))
      var result = Set[AtomSignature]()

      while (queue.nonEmpty) {
        val currentFormula = queue.dequeue()
        currentFormula match {
          case atom: AtomicFormula => result += atom.signature
          case _ => currentFormula.subFormulas.foreach(queue.enqueue(_))
        }
      }

      result
  }

  def replaceAtom(targetAtom: AtomicFormula, inFormula: Formula, replacement: Formula): Option[Formula] = {
    Unify(targetAtom, inFormula) match {
      case Some(theta) if theta.nonEmpty =>
        val replacementPrime = Substitute(theta, replacement)
        val targetPrime = Substitute(theta, inFormula)
        debug("Substituted formula: " + targetPrime.toText)
        val result = _replaceAtom(targetAtom, targetPrime, replacementPrime)
        debug("Replaced formula: " + result.toText)
        Some(result)
      case _ => None // nothing to unify
    }
  }

  private def _replaceAtom(targetAtom: AtomicFormula, inFormula: Formula, withFormula: Formula): Formula = {
    inFormula match {
      case f: AtomicFormula =>
        if (f.signature == targetAtom.signature) withFormula
        else f
      case f: WeightedFormula => WeightedFormula(f.weight, _replaceAtom(targetAtom, f.formula, withFormula))
      case f: Not => Not(_replaceAtom(targetAtom, f.arg, withFormula))
      case f: And => And(_replaceAtom(targetAtom, f.left, withFormula), _replaceAtom(targetAtom, f.right, withFormula))
      case f: Or => Or(_replaceAtom(targetAtom, f.left, withFormula), _replaceAtom(targetAtom, f.right, withFormula))
      case f: UniversalQuantifier => UniversalQuantifier(f.variable, _replaceAtom(targetAtom, f.formula, withFormula))
      case f: ExistentialQuantifier => ExistentialQuantifier(f.variable, _replaceAtom(targetAtom, f.formula, withFormula))
      case f: Equivalence => Equivalence(_replaceAtom(targetAtom, f.left, withFormula), _replaceAtom(targetAtom, f.right, withFormula))
      case f: Implies => Implies(_replaceAtom(targetAtom, f.left, withFormula), _replaceAtom(targetAtom, f.right, withFormula))
      case _ => throw new IllegalStateException("Illegal formula type.")
    }
  }


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
                    (implicit predicateSchema: Map[AtomSignature, List[String]], functionSchema: Map[AtomSignature, (String, List[String])]): Option[AtomicFormula] = {

    if (atom1.signature != atom2.signature) None //the signatures are different, thus MGP cannot be applied.
    else if (atom1 == atom2) Some(atom1) //comparing the same atom
    else generalisationOf(atom1, atom2)
  }

  @inline
  private def generalisationOf(atom1: AtomicFormula, atom2: AtomicFormula)
                              (implicit predicateSchema: Map[AtomSignature, List[String]], functionSchema: Map[AtomSignature, (String, List[String])]): Option[AtomicFormula] = {

    val generalizedArgs: List[Term] = {
      for ((pair, idx) <- atom1.terms.zip(atom2.terms).zipWithIndex)
      yield pair match {
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
      }
    }

    Some(AtomicFormula(atom1.symbol, generalizedArgs))
  }

  private def generalisationOf(f1: TermFunction, f2: TermFunction, level: Int = 0)
                              (implicit functionSchema: Map[AtomSignature, (String, List[String])]): Option[TermFunction] = {
    val generalizedArgs: List[Term] = {
      for ((pair, idx) <- f1.terms.zip(f2.terms).zipWithIndex)
      yield pair match {
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
      }
    }

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

  def variablesIn(terms: Iterable[_ <: Term]): List[Variable] = {
    val stack = mutable.Stack[Term]()
    stack.pushAll(terms)

    var result = List[Variable]()

    while (stack.nonEmpty) stack.pop() match {
      case v: Variable => result ::= v
      case f: TermFunction => stack.pushAll(f.terms)
      case _ => //do nothing
    }

    result
  }

  def uniqueOrderedVariablesIn(literals: Iterable[Literal]): List[Variable] = {
    val stack = mutable.Stack[Term]()

    var variables = Set[Variable]()
    var result = List[Variable]()

    for (literal <- literals) {
      stack.pushAll(literal.sentence.terms)

      while (stack.nonEmpty) stack.pop() match {
        case v: Variable if !variables.contains(v) =>
          variables += v
          result ::= v
        case f: TermFunction =>
          stack.pushAll(f.terms)
        case _ => //do nothing
      }

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


  object predef {

    val dynAtomBuilders: Map[AtomSignature, DynamicAtomBuilder] =
      List(
        DynEqualsBuilder(), DynLessThanBuilder(), DynLessThanEqBuilder(),
        DynGreaterThanBuilder(), DynGreaterThanEqBuilder(), DynSubstringBuilder()
      ).map(builder => builder.signature -> builder).toMap


    val dynAtoms: Map[AtomSignature, List[String] => Boolean] =
      dynAtomBuilders.map { case (signature, builder) => signature -> builder.stateFunction}


    val dynFunctionBuilders: Map[AtomSignature, DynamicFunctionBuilder] =
      List(
        DynSuccFunctionBuilder(), DynPrecFunctionBuilder(), DynPlusFunctionBuilder(), DynMinusFunctionBuilder(),
        DynTimesFunctionBuilder(), DynDividedByFunctionBuilder(), DynModFunctionBuilder(), DynConcatFunctionBuilder()
      ).map(builder => builder.signature -> builder).toMap

    val dynFunctions: Map[AtomSignature, List[String] => String] =
      dynFunctionBuilders.map { case (signature, builder) => signature -> builder.resultFunction}

  }


}