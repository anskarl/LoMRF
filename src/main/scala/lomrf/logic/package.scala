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

import collection.mutable
import lomrf.logic.dynamic._
import scala.Some
import lomrf.util.Logging

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
      while (!queue.isEmpty) {
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

      while (!queue.isEmpty) {
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
      case Some(theta) if !theta.isEmpty =>
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
        case (f1: Function, f2: Function) => generalisationOf(f1, f2).getOrElse(return None)
        case _ => return None
      }
    }

    Some(AtomicFormula(atom1.symbol, generalizedArgs))
  }

  private def generalisationOf(f1: Function, f2: Function, level: Int = 0)
                              (implicit functionSchema: Map[AtomSignature, (String, List[String])]): Option[Function] = {
    val generalizedArgs: List[Term] = {
      for ((pair, idx) <- f1.args.zip(f2.args).zipWithIndex)
      yield pair match {
        case (v: Variable, _) => v
        case (_, v: Variable) => v
        case (c1: Constant, c2: Constant) =>
          if (c1 == c2) c1
          else {
            val typeName = functionSchema(f1.signature)._2(idx)
            Variable("var_" + "l" + level + "i" + idx, typeName, idx)
          }
        case (f1: Function, f2: Function) => generalisationOf(f1, f2, level + 1).getOrElse(return None)
        case _ => return None
      }
    }

    Some(Function(f1.symbol, generalizedArgs, f1.domain))
  }


  object predef {

    val dynAtomBuilders: Map[AtomSignature, DynamicAtomBuilder] =
      List(
        DynEqualsBuilder(), DynLessThanBuilder(), DynLessThanEqBuilder(),
        DynGreaterThanBuilder(), DynGreaterThanEqBuilder(),DynSubstringBuilder()
      ).map(builder => builder.signature -> builder).toMap


    val dynAtoms: Map[AtomSignature, List[String] => Boolean] =
      dynAtomBuilders.map{case (signature, builder) => signature -> builder.stateFunction}



    val dynFunctionBuilders: Map[AtomSignature, DynamicFunctionBuilder] =
      List(
        DynSuccFunctionBuilder(), DynPrecFunctionBuilder(), DynPlusFunctionBuilder(), DynMinusFunctionBuilder(),
        DynTimesFunctionBuilder(), DynDividedByFunctionBuilder(), DynModFunctionBuilder(), DynConcatFunctionBuilder()
      ).map(builder => builder.signature -> builder).toMap

    val dynFunctions: Map[AtomSignature, List[String] => String] =
      dynFunctionBuilders.map{case (signature, builder) => signature -> builder.resultFunction}

  }


}