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

package lomrf.logic

import lomrf.logic.dynamic.DynEqualsBuilder
import lomrf.mln.model.{FunctionSchema, PredicateSchema}
import scala.collection.mutable
import scala.util.Try

object LogicOps {

  private type DefiniteClausesDB = mutable.HashMap[AtomSignature, mutable.HashMap[AtomicFormula, mutable.HashSet[DefiniteClauseConstruct]]]

  /**
   * Implicit class for operations over set of definite clauses.
   */
  implicit class DefiniteClausesOps(val definiteClauses: Set[WeightedDefiniteClause]) extends AnyVal {

    def collectAndMerge(implicit predicateSchema: PredicateSchema, functionSchema: FunctionSchema): Try[(Boolean, DefiniteClausesDB)] = Try {

      val eqBuilder = new DynEqualsBuilder
      val dcDB = mutable.HashMap[AtomSignature, mutable.HashMap[AtomicFormula, mutable.HashSet[DefiniteClauseConstruct]]]()

      var canBeDecomposed = true

      // Creating DB
      for (currentClause <- definiteClauses) {
        //debug("Processing: " + currentClause.toText)
        val currentHead = currentClause.clause.head
        dcDB.get(currentHead.signature) match {
          // It is the first time that this signature is processed
          case None => dcDB(currentHead.signature) = mutable.HashMap(currentHead -> mutable.HashSet(currentClause.clause.body))
          // DB already contains some clause(s) with the same signature in the head
          case Some(mapping) =>
            mapping.get(currentHead) match {
              // It also contains clauses having exactly the same head predicate (same constants and/or variables in the arguments).
              // Consequently, put the current clause together with the rest clauses.
              case Some(storedBodies) => storedBodies += currentClause.clause.body
              case None =>
                mapping.find(entry => Unify(entry._1, currentHead).isDefined) match {
                  case None => dcDB(currentHead.signature).put(currentHead, mutable.HashSet(currentClause.clause.body))
                  case Some(entry) =>
                    val storedHead = entry._1
                    val storedBodies = entry._2
                    generalisation(storedHead, currentHead) match {
                      case Some(generalisedHead) =>

                        // collect only Variable -> Variable mappings
                        def collectVariablesToRename(theta: Map[Term, Term]) = theta.filter {
                          case (v1: Variable, v2: Variable) => true
                          case _ => false
                        }

                        // --- 1. Update current clause body ---
                        // In case where the current "head predicate" is not the same with the "generalised head predicate",
                        // cover the differences by renaming its variables and introduce additional atoms (equals) to the body.
                        val currentClauseBody = {
                          if (currentHead != generalisedHead) {
                            Unify(currentHead, generalisedHead) match {
                              case Some(thetaCurrent) =>
                                // Rename variables:
                                val thetaVariablesRenaming = collectVariablesToRename(thetaCurrent)
                                val bodyVarRenamed = currentClause.clause.body.substitute(thetaVariablesRenaming)
                                val thetaWithoutVarRenamed = thetaCurrent -- thetaVariablesRenaming.keys

                                //Insert the additional atoms to the bodies:
                                val additionalAtomsStored =
                                  for ((v, t) <- thetaWithoutVarRenamed if v.isInstanceOf[Variable])
                                    yield eqBuilder(Vector(v, t))

                                additionalAtomsStored.foldLeft(bodyVarRenamed)((rest, atom) => And(atom, rest))
                              case _ =>
                                throw new UnsupportedOperationException(s"Cannot unify '${generalisedHead.toText}' with '${currentHead.toText}' (possible bug?)")
                            }

                          }
                          else currentClause.clause.body
                        }

                        // --- 2. Update previously stored bodies ---
                        // In case where the previously stored head predicate is not the same with the generalised head predicate,
                        // cover the differences by renaming its variables and introduce additional atoms (equals) to the bodies.
                        // Otherwise, simply insert the current clause body.
                        if (storedHead != generalisedHead) {
                          Unify(storedHead, generalisedHead) match {
                            case Some(thetaStored) =>
                              // Although storedHead != generalisedHead, they may be similar but with different variable names.
                              val areSimilarPredicates = storedHead =~= generalisedHead
                              if (!areSimilarPredicates) canBeDecomposed = false

                              // Rename variables (from all bodies):
                              val thetaVariablesRenaming = collectVariablesToRename(thetaStored)

                              val bodiesVarRenamed =
                                if (areSimilarPredicates) storedBodies.map(body => body.substitute(thetaVariablesRenaming))
                                else storedBodies

                              // Insert the additional atoms to the bodies:
                              val thetaWithoutVarRenamed = thetaStored -- thetaVariablesRenaming.keys

                              val additionalAtomsStored =
                                for ((v, t) <- thetaWithoutVarRenamed if v.isInstanceOf[Variable])
                                  yield eqBuilder(Vector(v, t))

                              val updatedBodies = bodiesVarRenamed
                                .map(body => additionalAtomsStored.foldLeft(body)((rest, atom) => And(atom, rest)))

                              updatedBodies += currentClauseBody

                              // Replace the previously stored head predicate with the generalised head predicate,
                              // associated with the updated collection of bodies.
                              dcDB(storedHead.signature).remove(storedHead)
                              dcDB(generalisedHead.signature).put(generalisedHead, updatedBodies)

                            case None =>
                              throw new UnsupportedOperationException(s"Cannot unify '${generalisedHead.toText}' with '${storedHead.toText}' (possible bug?)")
                          }

                          /*val thetaStored = Unify(storedHead, generalisedHead).getOrElse(
                            fatal(s"Cannot unify '${generalisedHead.toText}' with '${storedHead.toText}' (possible bug?)"))*/


                        }
                        else {
                          // No need to perform any update to the previously stored bodies,
                          // thus we simply insert the current clause body
                          dcDB(storedHead.signature)(storedHead) += currentClauseBody
                        }
                      case None =>
                        throw new UnsupportedOperationException(s"Failed to find a generalised predicate from '${storedHead.toText}' and '${currentHead.toText}' (possible bug?).")
                    }
                }
            } //end: Some(mapping)
        }
      } // Done creating DB

      (canBeDecomposed, dcDB)
    }
  }

  /**
   * Implicit class for definite clauses operations.
   */
  implicit class DefiniteClauseOps(val definiteClause: DefiniteClause) extends AnyVal {

    /**
     * @return All atomic formulas appearing in this definite class as literals
     */
    def literals: Set[Literal] = Set(PositiveLiteral(definiteClause.head)) ++ definiteClause.bodyLiterals

    /**
     * @return All atomic formulas appearing in the body of this definite clause as literals
     */
    def bodyLiterals: Set[Literal] = extract(definiteClause.body)

    /**
     * Extracts all atomic formula appearing in the given formula construct as literals.
     *
     * @param construct a formula construct to extract literals
     * @return a set of literals appearing in the formula construct
     */
    private def extract(construct: FormulaConstruct): Set[Literal] =
      construct.subFormulas.foldRight(Set[Literal]())((current, rest) => current match {
        case negation: Not => rest + NegativeLiteral(negation.arg.asInstanceOf[AtomicFormula])
        case atom: AtomicFormula => rest + PositiveLiteral(atom)
        case _ => rest ++ extract(current)
      })
  }

  /**
   * Implicit class for formula operations.
   */
  implicit class FormulaOps[F <: Formula](val formula: F) extends AnyVal {

    /**
     * Checks if the given atom signature appears in this formula.
     *
     * @param signature the atom signature to search for
     * @return true if the atom signature exists, false otherwise
     */
    def contains(signature: AtomSignature): Boolean = formula match {
      case atom: AtomicFormula => atom.signature == signature
      case _ =>
        val queue = mutable.Queue[FormulaConstruct]()
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

    /**
     * Searches for an atomic formula having the given atom signature in this formula.
     *
     * @param signature the atom signature to search for
     * @return the first atomic formula having the given atom signature
     */
    def first(signature: AtomSignature): Option[AtomicFormula] = formula match {
      case atom: AtomicFormula => if (atom.signature == signature) Some(atom) else None
      case _ =>
        val queue = mutable.Queue[FormulaConstruct]()
        formula.subFormulas.foreach(queue.enqueue(_))
        while (queue.nonEmpty) {
          val currentFormula = queue.dequeue()
          currentFormula match {
            case atom: AtomicFormula => if (atom.signature == signature) return Some(atom)
            case _ => currentFormula.subFormulas.foreach(f => queue.enqueue(f))
          }
        }
        None
    }

    /**
     * Searches for all atomic formulas having the given atom signature in this formula.
     *
     * @param signature the atom signature to search for
     * @return a set of atomic formulas having the given atom signature
     */
    def all(signature: AtomSignature): Seq[AtomicFormula] = formula match {
      case atom: AtomicFormula => if (atom.signature == signature) Seq(atom) else Seq()
      case _ =>
        val queue = mutable.Queue[FormulaConstruct]()
        formula.subFormulas.foreach(queue.enqueue(_))
        var result = Vector[AtomicFormula]()

        while (queue.nonEmpty) {
          val currentFormula = queue.dequeue()
          currentFormula match {
            case atom: AtomicFormula =>
              if (atom.signature == signature) result :+= atom

            case _ => currentFormula.subFormulas.foreach(f => queue.enqueue(f))
          }
        }
        result
    }

    /**
     * @return All atom signatures existing in this formula.
     */
    def signatures: Set[AtomSignature] = formula match {
      case atom: AtomicFormula => Set(atom.signature)
      case _ =>
        val queue = mutable.Queue[FormulaConstruct]()
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

    /**
     * Replace a target atomic formula appearing in this formula with a given replacement
     * formula construct.
     *
     * @param targetAtom the target atomic formula
     * @param replacement the replacement formula construct
     * @return the replaced formula or None if the replacement failed
     */
    def replace(targetAtom: AtomicFormula, replacement: FormulaConstruct): Option[F] = {

      def doReplace(inFormula: FormulaConstruct, withFormula: FormulaConstruct): FormulaConstruct = inFormula match {
          case f: AtomicFormula => if (f.signature == targetAtom.signature) withFormula else f

          case f: Not => Not(doReplace(f.arg, withFormula))

          case f: And => And(doReplace(f.left, withFormula), doReplace(f.right, withFormula))

          case f: Or => Or(doReplace(f.left, withFormula), doReplace(f.right, withFormula))

          case f: UniversalQuantifier => UniversalQuantifier(f.variable, doReplace(f.formula, withFormula))

          case f: ExistentialQuantifier => ExistentialQuantifier(f.variable, doReplace(f.formula, withFormula))

          case f: Equivalence => Equivalence(doReplace(f.left, withFormula), doReplace(f.right, withFormula))

          case f: Implies => Implies(doReplace(f.left, withFormula), doReplace(f.right, withFormula))

          case _ => throw new IllegalStateException("Illegal formula type.")
        }

      def mkReplacement(f: FormulaConstruct): Option[FormulaConstruct] = {
        Unify(targetAtom, f) match {
          case Some(theta) if theta.nonEmpty =>
            val replacementPrime = replacement.substitute(theta)
            val targetPrime = f.substitute(theta)
            val resultingConstruct = doReplace(targetPrime, replacementPrime)

            Some(resultingConstruct)
          case _ => None
        }
      }

      def processDefiniteClause(clause: DefiniteClause): Option[DefiniteClause] = {
        if (replacement.isInstanceOf[DefiniteClauseConstruct]) {

          val bodyOpt = mkReplacement(clause.body).asInstanceOf[Option[DefiniteClauseConstruct]]

          val headOpt: Option[AtomicFormula] = replacement match {
            case a: AtomicFormula => mkReplacement(clause.head).asInstanceOf[Option[AtomicFormula]]
            case _ => None
          }

          if(bodyOpt.isDefined || headOpt.isDefined)
            Some(DefiniteClause(headOpt.getOrElse(clause.head), bodyOpt.getOrElse(clause.body)))
          else None
        }
        else None
      }

      formula match {
        case c: FormulaConstruct => mkReplacement(c).asInstanceOf[Option[F]]

        case WeightedFormula(w,f) =>
          mkReplacement(f).map(resultingConstruct => WeightedFormula(w, resultingConstruct)).asInstanceOf[Option[F]]

        case WeightedDefiniteClause(w, clause) =>
          processDefiniteClause(clause).map(newClause => WeightedDefiniteClause(w, newClause) ).asInstanceOf[Option[F]]

        case clause: DefiniteClause => processDefiniteClause(clause).asInstanceOf[Option[F]]

        case _ => None
      }
    }

  }

}
