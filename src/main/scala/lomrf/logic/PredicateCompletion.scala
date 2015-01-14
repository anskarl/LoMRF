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

import collection.mutable
import lomrf.logic.dynamic.DynEqualsBuilder


/**
 * Perform circumscription by predicate completion.</br>
 *
 * <p> By default all definite clauses are circumscribed. The algorithm collects all
 * head predicates of definite clauses and computes their completion. All variables
 * in the body that do not appear in the head predicate are existentially quantified.
 * </p>
 *
 * For example, consider the following definite clauses:
 * {{{
 * InitiatedAt(meeting(x,y), t) :- Happens(Event1(x),t) ^ Happens(Event2(y),t)                     (1)
 *
 * InitiatedAt(meeting(x,y), t) :- Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t          (2)
 * }}}
 * <p> In rule (1), both head and body predicates contain the same variables.
 * However, in rule (2) the body predicate !HoldsAt(moving(Y), t') contains the variable t',
 * which does not appear in the head predicate. As a result, this variable will be existentially quantified.
 * </p>
 * The resulting formulas of the predicate completion are the following:
 * {{{
 * InitiatedAt(meeting(x,y), t) <=>
 *   (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v
 *   (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )                            (3)
 * }}}
 *
 * <p> Rule (3) is a combination of the rules (1) and (2). The right side of the resulting rule (3) contains the bodies
 * of (1) and (2), separated with a disjunction. The body from rule (1) is directly included in (3), as contains the
 * same variables with its head. However, in the body from rule (2) the variable "t'" is existentially quantified, as
 * it does not appear to the head predicate.
 * </p>
 *
 * Other features:
 * <ul>
 * <li>The algorithm can exploit the equivalences created by the predicate completion (e.g. rule (3)) and
 * simplify the formulas in the knowledge base. </li>
 * <li>the algorithm can manage more complex cases, where the definite clauses contain different naming
 * in the variables, as well as cases where some head predicates contain constant values </li>
 * </ul>
 * </p>
 *
 * @author Anastasios Skarlatidis
 */
object PredicateCompletion extends Logging {

  import PredicateCompletionMode._


  private type DefiniteClausesDB = mutable.HashMap[AtomSignature, mutable.HashMap[AtomicFormula, mutable.HashSet[DefiniteClauseConstruct]]]
  private lazy val eqBuilder = new DynEqualsBuilder

  /**
   * Performs predicate completion with simplification (see [[lomrf.logic.PredicateCompletion]]).
   *
   * @param formulas the input set of formulas [[lomrf.logic.Formula]]
   * @param definiteClauses the input set of definite clauses [[lomrf.logic.DefiniteClause]]
   * @param predicateSchema predicate schema [[lomrf.mln.model.MLN]]
   * @param functionSchema function schema [[lomrf.mln.model.MLN]]
   * @return a logically stronger knowledge base (set of formulas)
   */
  def apply(formulas: collection.Set[Formula], definiteClauses: collection.Set[WeightedDefiniteClause])
           (implicit predicateSchema: Map[AtomSignature, Vector[String]], functionSchema: Map[AtomSignature, (String, Vector[String])]): collection.Set[Formula] = apply(formulas, definiteClauses, PredicateCompletionMode.Simplification)

  /**
   * Creates a logically stronger knowledge base from the given formulas, by performing predicate completion.
   * Optionally, the method can simplify the given formulas when mode is set to Simplification.
   * Predicate completion is performed only for the head predicates that appear in the input set of definite clauses.
   * <br/>
   * <br/>
   * In particular, the following modes are supported: <br/>
   *
   * '''Simplification:''' the algorithm will exploit the equivalences that are created by the predicate completion
   * in order to simplify the formulas in the knowledge base. For example, consider the predicate completion result (3):
   * {{{
   * InitiatedAt(meeting(x,y), t) <=>
   *  (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v
   *  (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )                                   (3)
   * }}}
   * and the following two Discrete Event Calculus axioms:
   * {{{
   * Next(t1,t0) ^ InitiatedAt(f,t0) => HoldsAt(f, t0).                                                    (4)
   *
   * Next(t1,t0) ^ !HoldsAt(f,t0) ^ !InitiatedAt(f,t0) => !HoldsAt(f,t0).                                  (5)
   * }}}
   *
   * The algorithm performs the following two steps:</br>
   * <ol>
   * <li>The algorithm will substitute (4) and (5) with "f = meeting(x,y)", producing the formulas below:
   * {{{
   * Next(t1,t0) ^ InitiatedAt(meeting(x,y),t0) => HoldsAt(meeting(x,y), t0).                              (6)
   *
   * Next(t1,t0) ^ !HoldsAt(f,t0) ^ !InitiatedAt(meeting(x,y),t0) => !HoldsAt(meeting(x,y),t0).            (7)
   * }}}
   * </li>
   * <li>Thereafter, the algorithm will replace the predicate InitiatedAt(meeting(x,y),t0) with its equivalence from (3).
   * Note, the variable "t" in (3) will be renamed as "t0". The resulting formulas are the following:
   * {{{
   * Next(t1,t0) ^ (
   * (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )
   * ) => HoldsAt(meeting(x,y), t0).
   *
   * Next(t1,t0) ^ !HoldsAt(f,t0) ^ !(
   * (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )
   * ) => !HoldsAt(meeting(x,y),t0).
   * }}}
   * </li>
   * </ol>
   *
   *
   * '''Decomposed:''' created equivalences are decomposed into two implications, for example the equivalence below.
   * {{{
   * InitiatedAt(meeting(x,y), t) <=>
   *  (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v
   *  (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )
   * }}}
   * will be decomposed into the following implications:
   * {{{
   * Happens(Event1(x),t) ^ Happens(Event2(y),t) => InitiatedAt(meeting(x,y), t)
   *
   * Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t => InitiatedAt(meeting(x,y), t)
   *
   * InitiatedAt(meeting(x,y), t) =>
   *  (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v
   *  (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )
   * }}}
   *
   * '''Standard:''' for standard predicate completion.
   * {{{
   * InitiatedAt(meeting(x,y), t) <=>
   *  (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v
   *  (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )
   * }}}
   *
   * @param formulas  the input set of formulas [[lomrf.logic.Formula]]
   * @param definiteClauses   the input set of definite clauses [[lomrf.logic.DefiniteClause]]
   * @param mode  predicate completion mode to use
   * @param predicateSchema   MLN predicate schema [[lomrf.mln.model.MLN]]
   * @param functionSchema  MLN function schema [[lomrf.mln.model.MLN]]

   * @return  a logically stronger knowledge base (set of formulas)
   */
  def apply(formulas: collection.Set[Formula], definiteClauses: collection.Set[WeightedDefiniteClause], mode: PredicateCompletionMode)
           (implicit predicateSchema: Map[AtomSignature, Vector[String]], functionSchema: Map[AtomSignature, (String, Vector[String])]): collection.Set[Formula] = {

    if (definiteClauses.isEmpty) {
      info("No definite clauses found in the given MLN.")
      return formulas
    }
    info("Predicate completion")
    // --- Step 1 --- Grouping definite clauses with similar head predicate
    info("\tStep 1: Grouping definite clauses with similar head predicate.")

    val (canBeDecomposed, dcDB) = collectAndMerge(definiteClauses)

    if (dcDB.isEmpty)
      fatal("Failed to parse definite clauses in the given MLN (possible bug?).")

    debug {
      val buffer = new StringBuilder("Collected/merged the following definitions:")
      for ((signature, entries) <- dcDB) {
        buffer.append("\nSignature: " + signature)
        for ((head, bodies) <- entries) {
          buffer.append("\n\thead: " + head.toText)
          buffer.append("\n\tbodies:")
          for (body <- bodies) buffer.append("\n\t\t" + body.toText)
        }
      }
      buffer.result()
    }


    // --- Step 2 --- Predicate completion
    val targetPredicates = dcDB.keySet.map("\"" + _.toString + "\"").reduceLeft((a, b) => a + ", " + b)
    info("\tStep 2: Performing predicate completion for predicates: " + targetPredicates)


    mode match {
      case Simplification => applyPCSimplification(formulas, dcDB)
      case Decomposed =>
        if (canBeDecomposed) applyPCDecomposed(formulas, definiteClauses, dcDB)
        else fatal("I'm sorry but the result of the predicate completion cannot be decomposed, " +
          "as the created equivalences appear to be more general from the heads of the given definite clauses. " +
          "Please use standard predicate completion or with simplification.")
      case Standard => applyPC(formulas, dcDB)
    }


  }

  /**
   * Free variables in the body that are not appearing in the head, will set to as existentially quantified.
   *
   * @param head atom
   * @param body atoms
   * @return the resulting body, which may be existentially quantified over some variables
   */
  private def normalise(head: AtomicFormula, body: Formula): Formula = {
    //A set of existentially quantified variables in the body
    val exQVars = body.getQuantifiers.filter(_.isInstanceOf[ExistentialQuantifier]).map(_.variable).toSet

    // Find which free variables appear in the body, but not in the head of the clause
    val diff = body.variables -- exQVars -- head.variables

    // If the variables that appear in the body are the same with the variables in the head,
    // then keep the body as it is. Otherwise, define them as existentially quantified.
    if (diff.isEmpty) body
    else diff.foldRight(body)((v, f) => ExistentialQuantifier(v, f))
  }


  private def collectAndMerge(definiteClauses: collection.Set[WeightedDefiniteClause])
                             (implicit predicateSchema: Map[AtomSignature, Vector[String]], functionSchema: Map[AtomSignature, (String, Vector[String])]): (Boolean, DefiniteClausesDB) = {


    val dcDB = mutable.HashMap[AtomSignature, mutable.HashMap[AtomicFormula, mutable.HashSet[DefiniteClauseConstruct]]]()

    var canBeDecomposed = true

    // Creating DB
    for (currentClause <- definiteClauses) {
      debug("Processing: " + currentClause.toText)
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
                          val thetaCurrent = Unify(currentHead, generalisedHead).getOrElse(fatal("Cannot unify " + generalisedHead.toText + " with " + currentHead.toText + " (possible bug?)"))
                          // Rename variables:
                          val thetaVariablesRenaming = collectVariablesToRename(thetaCurrent)
                          val bodyVarRenamed = Substitute(thetaVariablesRenaming, currentClause.clause.body)
                          val thetaWithoutVarRenamed = thetaCurrent -- thetaVariablesRenaming.keys

                          //Insert the additional atoms to the bodies:
                          val additionalAtomsStored =
                            for ((v, t) <- thetaWithoutVarRenamed if v.isInstanceOf[Variable]) yield eqBuilder(Vector(v, t)) //yield AtomicFormula("equals", List(v, t))

                          additionalAtomsStored.foldLeft(bodyVarRenamed)((rest, atom) => And(atom, rest))
                        }
                        else currentClause.clause.body
                      }

                      // --- 2. Update previously stored bodies ---
                      // In case where the previously stored head predicate is not the same with the generalised head predicate,
                      // cover the differences by renaming its variables and introduce additional atoms (equals) to the bodies.
                      // Otherwise, simply insert the current clause body.
                      if (storedHead != generalisedHead) {
                        val thetaStored = Unify(storedHead, generalisedHead).getOrElse(fatal("Cannot unify " + generalisedHead.toText + " with " + storedHead.toText + " (possible bug?)"))

                        // Although storedHead != generalisedHead, they may be similar but with different variable names.
                        val areSimilarPredicates = storedHead.isSimilarTo(generalisedHead)
                        if (!areSimilarPredicates) canBeDecomposed = false

                        // Rename variables (from all bodies):
                        val thetaVariablesRenaming = collectVariablesToRename(thetaStored)
                        val bodiesVarRenamed = if (areSimilarPredicates) storedBodies.map(body => Substitute(thetaVariablesRenaming, body)) else storedBodies

                        // Insert the additional atoms to the bodies:
                        val thetaWithoutVarRenamed = thetaStored -- thetaVariablesRenaming.keys

                        val additionalAtomsStored =
                          for ((v, t) <- thetaWithoutVarRenamed if v.isInstanceOf[Variable]) yield eqBuilder(Vector(v, t)) //yield AtomicFormula("equals", List(v, t))

                        val updatedBodies = bodiesVarRenamed.map(body => additionalAtomsStored.foldLeft(body)((rest, atom) => And(atom, rest)))
                        updatedBodies += currentClauseBody

                        // Replace the previously stored head predicate with the generalised head predicate,
                        // associated with the updated collection of bodies.
                        dcDB(storedHead.signature).remove(storedHead)
                        dcDB(generalisedHead.signature).put(generalisedHead, updatedBodies)
                      }
                      else {
                        // No need to perform any update to the previously stored bodies,
                        // thus we simply insert the current clause body
                        dcDB(storedHead.signature)(storedHead) += currentClauseBody
                      }
                    case None => fatal("Failed to find a generalised predicate from " + storedHead.toText + " and " + currentHead.toText + " (possible bug?).")
                  }
              }
          } //end: Some(mapping)
      }
    } // Done creating DB

    (canBeDecomposed, dcDB)
  }


  /**
   * Computes predicate completion with simplification (see [[lomrf.logic.PredicateCompletion]])
   *
   * @param formulas the set of FOL formulas (non-definite clauses) in the input KB
   * @param dcDB database of collected/merged definite clauses
   * @return the resulting KB
   */
  private def applyPCSimplification(formulas: collection.Set[Formula], dcDB: DefiniteClausesDB): collection.Set[Formula] = {
    val targetSignatures = dcDB.keySet
    var pcResultingKB = new mutable.HashSet[Formula]()
    pcResultingKB ++= formulas

    for (signature <- targetSignatures) {
      val lambdaPrime = new mutable.HashSet[Formula]()
      for (formula <- pcResultingKB) {
        if (containsSignature(signature, formula)) {
          for ((headPredicate, bodies) <- dcDB(signature)) {
            val replacement = bodies.map(body => normalise(headPredicate, body)).reduceLeft((left, right) => Or(left, right))
            debug("Predicates like " + headPredicate.toText + " will be replaced with following sentence:\n\t" + replacement.toText)
            val resultOpt = replaceAtom(headPredicate, formula, replacement)
            resultOpt match {
              case Some(result) => lambdaPrime += result
              case None => fatal("Predicate replacement failed (possible bug?)")
            }
          }
        }
        else {
          lambdaPrime += formula
        }
      }
      pcResultingKB = lambdaPrime
    }
    pcResultingKB
  }

  /**
   * Standard predicate completion (see [[lomrf.logic.PredicateCompletion]])
   *
   * @param formulas the set of FOL formulas (non-definite clauses) in the input KB
   * @param dcDB database of collected/merged definite clauses
   * @return the resulting KB
   */
  private def applyPC(formulas: collection.Set[Formula], dcDB: DefiniteClausesDB): collection.Set[Formula] = {

    var pcResultingKB = new mutable.HashSet[Formula]()
    pcResultingKB ++= formulas

    for ((_, entries) <- dcDB; (head, bodies) <- entries) {
      val pcFormula = WeightedFormula(Double.PositiveInfinity, Equivalence(head, bodies.map(body => normalise(head, body)).reduceLeft((left, right) => Or(left, right))))
      pcResultingKB += pcFormula
    }
    pcResultingKB
  }

  /**
   * Predicate completion with decomposed equivalences (see [[lomrf.logic.PredicateCompletion]]).<br/>
   *
   * @param formulas the set of FOL formulas (non-definite clauses) in the input KB
   * @param definiteClauses the original set of definite clauses
   * @param dcDB database of collected/merged definite clauses
   * @return the resulting KB
   */
  private def applyPCDecomposed(formulas: collection.Set[Formula], definiteClauses: collection.Set[WeightedDefiniteClause], dcDB: DefiniteClausesDB): collection.Set[Formula] = {

    var pcResultingKB = new mutable.HashSet[Formula]()
    pcResultingKB ++= formulas

    // Insert the original definite clauses as weighted formulas:
    for (dClause <- definiteClauses)
      pcResultingKB += WeightedFormula(dClause.weight, Implies(dClause.clause.body, dClause.clause.head))

    // Insert the additional "completion" formulas:
    for ((_, entries) <- dcDB; (head, bodies) <- entries)
      pcResultingKB += WeightedFormula(Double.PositiveInfinity, Implies(head, bodies.map(body => normalise(head, body)).reduceLeft((left, right) => Or(left, right))))

    pcResultingKB
  }
}

/**
 * Choose the type of predicate completion:
 * <ul>
 * <li>Standard --- standard predicate completion</li>
 * <li>Decomposed --- computes predicate completion and decomposes the created equivalences into two implications</li>
 * <li>Simplification --- computes predicate completion and simplifies the given formulas based on the created equivalences</li>
 * </ul>
 * @see for further description: [[lomrf.logic.PredicateCompletion]]
 */
object PredicateCompletionMode extends Enumeration {
  type PredicateCompletionMode = Value
  val Standard, Decomposed, Simplification = Value
}