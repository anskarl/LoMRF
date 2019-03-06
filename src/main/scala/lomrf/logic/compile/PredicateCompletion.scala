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

package lomrf.logic.compile

import lomrf.logic._
import lomrf.logic.LogicOps._
import lomrf.logic.Unify.ThetaOpt
import lomrf.logic.{ AtomSignature, AtomicFormula, DefiniteClauseConstruct }
import lomrf.mln.model.{ ConstantsDomain, FunctionSchema, PredicateSchema }
import lomrf.util.Cartesian.CartesianIterator
import lomrf.util.collectByKey
import lomrf.util.logging.Implicits._
import scala.collection.mutable
import scala.util.{ Failure, Success }
import com.typesafe.scalalogging.LazyLogging

/**
  * Perform circumscription by predicate completion.</br>
  *
  * <p>
  *   By default all definite clauses are circumscribed. The algorithm collects all
  *   head predicates of definite clauses and computes their completion. All variables
  *   in the body that do not appear in the head predicate are existentially quantified.
  * </p>
  *
  * For example, consider the following definite clauses:
  *
  * {{{
  *   InitiatedAt(meeting(x,y), t) :- Happens(Event1(x),t) ^ Happens(Event2(y),t)                     (1)
  *
  *   InitiatedAt(meeting(x,y), t) :- Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t          (2)
  * }}}
  *
  * <p>
  *   In rule (1), both head and body predicates contain the same variables. However, in rule (2)
  *   the body predicate !HoldsAt(moving(Y), t') contains the variable t', which does not appear
  *   in the head predicate. As a result, this variable will be existentially quantified.
  * </p>
  *
  * The resulting formulas of the predicate completion are the following:
  *
  * {{{
  *   InitiatedAt(meeting(x,y), t) <=>
  *     (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v
  *     (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )                            (3)
  * }}}
  *
  * <p>
  *   Rule (3) is a combination of the rules (1) and (2). The right side of the resulting rule (3)
  *   contains the bodies of (1) and (2), separated with a disjunction. The body from rule (1) is directly
  *   included in (3), as contains the same variables with its head. However, in the body from rule (2)
  *   the variable "t'" is existentially quantified, as it does not appear to the head predicate.
  * </p>
  *
  * Other features:
  *
  * <ul>
  *   <li>The algorithm can exploit the equivalences created by the predicate completion (e.g. rule (3)) and
  *   simplify the formulas in the knowledge base. </li>
  *   <li>the algorithm can manage more complex cases, where the definite clauses contain different naming
  *   in the variables, as well as cases where some head predicates contain constant values </li>
  * </ul>
  */
object PredicateCompletion extends LazyLogging {

  import PredicateCompletionMode._

  private type DefiniteClausesDB = mutable.HashMap[AtomSignature, mutable.HashMap[AtomicFormula, mutable.HashSet[DefiniteClauseConstruct]]]

  /**
    * Performs predicate completion with simplification.
    *
    * @see [[lomrf.logic.compile.PredicateCompletion]]
    * @see [[lomrf.logic.FormulaConstruct]]
    * @see [[lomrf.mln.model.MLN]]
    *
    * @param formulas the input set of formulas
    * @param definiteClauses the input set of definite clauses
    * @param predicateSchema a predicate schema
    * @param functionSchema a function schema
    *
    * @return a logically stronger knowledge base (set of formulas)
    */
  def apply(
      formulas: Set[WeightedFormula],
      definiteClauses: Set[WeightedDefiniteClause])
    (implicit
      predicateSchema: PredicateSchema,
      functionSchema: FunctionSchema,
      constants: ConstantsDomain): Set[WeightedFormula] = {

    apply(formulas, definiteClauses, PredicateCompletionMode.Simplification)
  }

  /**
    * Creates a logically stronger knowledge base from the given formulas, by performing predicate
    * completion. Optionally, the method can simplify the given formulas when mode is set to Simplification.
    * Predicate completion is performed only for the head predicates that appear in the input set
    * of definite clauses.<br/><br/>
    *
    * In particular, the following modes are supported: <br/>
    *
    * '''Simplification:''' the algorithm will exploit the equivalences that are created by the predicate
    * completion in order to simplify the formulas in the knowledge base. For example, consider the predicate
    * completion result (3):
    *
    * {{{
    *   InitiatedAt(meeting(x,y), t) <=>
    *     (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v
    *     (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )                               (3)
    * }}}
    *
    * and the following Discrete Event Calculus axioms:
    *
    * {{{
    *   Next(t1,t0) ^ InitiatedAt(f,t0) => HoldsAt(f, t0).                                                 (4)
    *
    *   Next(t1,t0) ^ !HoldsAt(f,t0) ^ !InitiatedAt(f,t0) => !HoldsAt(f,t0).                               (5)
    * }}}
    *
    * The algorithm performs the following two steps:</br>
    *
    * <ol>
    *   <li>The algorithm will substitute (4) and (5) with "f = meeting(x,y)", producing the formulas below:
    *   {{{
    *     Next(t1,t0) ^ InitiatedAt(meeting(x,y),t0) => HoldsAt(meeting(x,y), t0).                         (6)
    *
    *     Next(t1,t0) ^ !HoldsAt(f,t0) ^ !InitiatedAt(meeting(x,y),t0) => !HoldsAt(meeting(x,y),t0).       (7)
    *   }}}
    *   </li>
    *
    *   <li>Thereafter, the algorithm will replace the predicate InitiatedAt(meeting(x,y),t0) with
    *   its equivalence from (3). Note, the variable "t" in (3) will be renamed as "t0". The resulting
    *   formulas are the following:
    *   {{{
    *     Next(t1,t0) ^ (
    *       (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )
    *     ) => HoldsAt(meeting(x,y), t0).
    *
    *     Next(t1,t0) ^ !HoldsAt(f,t0) ^ !(
    *       (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )
    *     ) => !HoldsAt(meeting(x,y),t0).
    *   }}}
    *   </li>
    * </ol>
    *
    * '''Decomposed:''' created equivalences are decomposed into two implications, for example the equivalence below.
    *
    * {{{
    *   InitiatedAt(meeting(x,y), t) <=>
    *   (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v
    *   (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )
    * }}}
    *
    * will be decomposed into the following implications:
    *
    * {{{
    *   Happens(Event1(x),t) ^ Happens(Event2(y),t) => InitiatedAt(meeting(x,y), t)
    *
    *   Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t => InitiatedAt(meeting(x,y), t)
    *
    *   InitiatedAt(meeting(x,y), t) =>
    *     (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v
    *     (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )
    * }}}
    *
    * '''Standard:''' for standard predicate completion.
    *
    * {{{
    *   InitiatedAt(meeting(x,y), t) <=>
    *     (Happens(Event1(x),t) ^ Happens(Event2(y),t)) v
    *     (Exist t' Happens(Event3(x),t) ^ !HoldsAt(moving(y),t') ^ t' < t )
    * }}}
    *
    * @see [[lomrf.logic.FormulaConstruct]]
    * @see [[lomrf.mln.model.MLN]]
    *
    * @param formulas the input set of formulas
    * @param definiteClauses the input set of definite clauses
    * @param mode predicate completion mode to use
    * @param predicateSchema a predicate schema
    * @param functionSchema a function schema
    *
    * @return  a logically stronger knowledge base (set of formulas)
    */
  def apply(
      formulas: Set[WeightedFormula],
      definiteClauses: Set[WeightedDefiniteClause],
      mode: PredicateCompletionMode)
    (implicit
      predicateSchema: PredicateSchema,
      functionSchema: FunctionSchema,
      constants: ConstantsDomain): Set[WeightedFormula] = {

    if (definiteClauses.isEmpty) {
      logger.info("No definite clauses found in the given MLN.")
      return formulas
    }

    logger.info("Predicate completion")
    // --- Step 1 --- Grouping definite clauses with similar head predicate
    logger.info("\tStep 1: Grouping definite clauses with similar head predicate.")

    definiteClauses.collectAndMerge match {
      case Success((canBeDecomposed, dcDB)) =>
        if (dcDB.isEmpty)
          logger.fatal("Failed to parse definite clauses in the given MLN (possible bug?).")

        logger.debug {
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
        logger.info(s"\tStep 2: Performing predicate completion for predicates: ${dcDB.keySet.map("\"" + _.toString + "\"").mkString(",")}")

        mode match {
          case Simplification => applyPCSimplification(formulas, dcDB)
          case Decomposed =>
            if (canBeDecomposed) applyPCDecomposed(formulas, definiteClauses, dcDB, constants)
            else logger.fatal("I'm sorry but the result of the predicate completion cannot be decomposed, " +
              "as the created equivalences appear to be more general from the heads of the given definite clauses. " +
              "Please use standard predicate completion or with simplification.")

          case Standard => applyPC(formulas, dcDB)
        }

      case Failure(ex) =>
        logger.fatal("Failed to group definite clauses with similar head predicates.", ex)
    }

  }

  /**
    * Computes predicate completion with simplification.
    *
    * @see [[lomrf.logic.compile.PredicateCompletion]]
    *
    * @param formulas the set of FOL formulas (non-definite clauses) in the input KB
    * @param dcDB database of collected/merged definite clauses
    * @return the resulting KB
    */
  private def applyPCSimplification(formulas: Set[WeightedFormula], dcDB: DefiniteClausesDB): Set[WeightedFormula] = {

    val targetSignatures = dcDB.keySet
    var pcResultingKB = Set[WeightedFormula]()
    pcResultingKB ++= formulas

    for (signature <- targetSignatures) {
      var lambdaPrime = Set[WeightedFormula]()
      for (formula <- pcResultingKB) {
        if (formula.contains(signature)) {
          for ((headPredicate, bodies) <- dcDB(signature)) {
            val replacement = bodies.map(_.boundVarsNotIn(headPredicate)).reduceLeft((left, right) => Or(left, right))

            logger.debug(s"Predicates like '${headPredicate.toText}' will be replaced with following sentence: '${replacement.toText}'")

            val resultOpt = formula.replace(headPredicate, replacement)

            resultOpt match {
              case Some(result) => lambdaPrime += result
              case None         => logger.fatal("Predicate replacement failed (possible bug?)")
            }
          }
        } else lambdaPrime += formula
      }
      pcResultingKB = lambdaPrime
    }
    pcResultingKB
  }

  /**
    * Standard predicate completion.
    *
    * @see [[lomrf.logic.compile.PredicateCompletion]]
    *
    * @param formulas the set of FOL formulas (non-definite clauses) in the input KB
    * @param dcDB database of collected/merged definite clauses
    *
    * @return the resulting KB
    */
  private def applyPC(formulas: Set[WeightedFormula], dcDB: DefiniteClausesDB): Set[WeightedFormula] = {

    var pcResultingKB = Set[WeightedFormula]()
    pcResultingKB ++= formulas

    for ((_, entries) <- dcDB; (head, bodies) <- entries) {
      val pcFormula = WeightedFormula(
        Double.PositiveInfinity,
        Equivalence(head, bodies.map(_.boundVarsNotIn(head)).reduceLeft((left, right) => Or(left, right))))
      pcResultingKB += pcFormula
    }

    pcResultingKB
  }

  /**
    * Predicate completion with decomposed equivalences.
    *
    * @see [[lomrf.logic.compile.PredicateCompletion]]
    *
    * @param formulas the set of FOL formulas (non-definite clauses) in the input KB
    * @param definiteClauses the original set of definite clauses
    * @param dcDB database of collected/merged definite clauses
    *
    * @return the resulting KB
    */
  private def applyPCDecomposed(
      formulas: Set[WeightedFormula],
      definiteClauses: Set[WeightedDefiniteClause],
      dcDB: DefiniteClausesDB,
      constants: ConstantsDomain)
    (implicit predicateSchema: PredicateSchema, functionSchema: FunctionSchema): Set[WeightedFormula] = {

      def extractTheta(theta: ThetaOpt) = theta.getOrElse(Map.empty).map {
        case (k: Variable, v: Term) => k -> v.symbol
        case (k: Term, _)           => logger.fatal(s"Term $k is not a variable!")
      }

    var pcResultingKB = Set[WeightedFormula]()
    pcResultingKB ++= formulas

    // Insert the original definite clauses as weighted formulas:
    for (dClause <- definiteClauses)
      pcResultingKB += WeightedFormula(dClause.weight, Implies(dClause.clause.body, dClause.clause.head))

    for ((signature, entries) <- dcDB) {

      // 1. Insert the additional "completion" formulas
      for ((head, bodies) <- entries) {
        val completionBody = bodies.map(_.boundVarsNotIn(head)).reduceLeft((left, right) => Or(left, right))
        pcResultingKB += WeightedFormula.asHard(Implies(head, completionBody))
      }

      logger.info(s"\t\tProduced ${entries.size} completion formulas for '$signature'")

      // 2. Find which partial-grounded heads are missing, in order to introduce them
      // as negated unit clauses to the theory (= complementary clauses)
      val headPredicate = entries.head._1
      val variabilizedPredicate = variabilizeAtom(headPredicate)

      val complementaryDomains =
        collectByKey[Variable, String](entries.keys.flatMap(p => extractTheta(Unify(variabilizedPredicate, p))))
          .map {
            case (v, collectedConstants) =>
              v -> constants(v.domain).filter(c => !collectedConstants.contains(c))
          }

      // 3. Add complementary unit clauses to the resulting knowledge base
      val complementaryClauses = {
        if (complementaryDomains.nonEmpty && complementaryDomains.values.forall(_.nonEmpty))
          (for (theta <- CartesianIterator(complementaryDomains); mappedTheta: Theta = theta.mapValues(Constant).asInstanceOf[Map[Term, Term]])
            yield WeightedFormula.asHard(Not(variabilizedPredicate.substitute(mappedTheta)))).toList
        else Nil
      }

      pcResultingKB = pcResultingKB ++ complementaryClauses

      logger.info(s"\t\tAdded ${complementaryClauses.size} complementary negated unit clause(s) for '$signature'")

      logger.debug {
        s"""
            |Head predicate: ${headPredicate.toText}
            |Complementary domains: ${complementaryDomains.mkString(", ")}
            |Complementary clauses: ${complementaryClauses.map(_.toText).mkString(", ")}
          """.stripMargin
      }
    }

    pcResultingKB
  }
}
