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

package lomrf.mln.learning.structure

import auxlib.log.Logger
import lomrf.logic.LogicOps._
import lomrf.logic.{Clause, NormalForm, WeightedDefiniteClause, WeightedFormula}
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.model._
import lomrf.mln.model.mrf.MRFState

/**
 * Evaluator provides various methods for evaluating clauses and definite clauses.
 */
object Evaluator {

  private lazy val log = Logger(this.getClass)
  import log._

  /**
   * Evaluates a vector of clauses with respect to another MRF state. It calculates
   * true counts for the given annotation and inferred counts for the previous MRF
   * state. Then it keeps only clauses having difference between true and inferred counts
   * above the specified threshold. If previous state is not specified, an initial state is
   * used where everything is false.
   *
   * Note: The lower the threshold the more clauses are retained
   *
   * @param clauses vector of clauses to evaluate
   * @param schema predicate schema
   * @param space predicate space
   * @param evidence evidence
   * @param annotation annotation for non evidence atoms
   * @param previousState a previous MRF state to calculate inferred counts (optional)
   * @param threshold threshold for comparison
   *
   * @return a set of good clauses along with their calculated scores
   */
  def evaluateClauses(clauses: Vector[Clause], schema: MLNSchema,
                      space: PredicateSpace, evidence: Evidence,
                      annotation: EvidenceDB, threshold: Int,
                      previousState: Option[MRFState] = None): (Vector[Clause], Vector[Int]) = {

    var goodClauses = Vector[Clause]()
    var scores = Vector[Int]()

    val mln = MLN(schema, evidence, space, clauses)
    val mrf = new MRFBuilder(mln, createDependencyMap = true).buildNetwork

    val state = MRFState(mrf)
    state.setAnnotatedState(annotation)

    // Calculate true and inferred counts for the given clauses
    val trueCounts = state.countTrueGroundings
    debug("True Counts: [" + trueCounts.deep.mkString(", ") + "]")

    val inferredCounts = state.countTrueGroundings(previousState)
    debug("Inferred Counts: [" + inferredCounts.deep.mkString(", ") + "]")

    // For each clause compute the difference between inferred and true counts and compare it to the given threshold
    for (clauseIdx <- clauses.indices) {
      debug(s"${clauses(clauseIdx).toText()} T: ${trueCounts(clauseIdx)} I: ${inferredCounts(clauseIdx)}")
      val value = inferredCounts(clauseIdx) - trueCounts(clauseIdx)
      if(-value >= threshold) {
        scores :+= value
        goodClauses :+= clauses(clauseIdx)
      }
    }

    (goodClauses, scores)
  }

  /**
   * Evaluates a set of definite clauses with respect to another MRF state. It performs
   * predicate completion and CNF using a set of given axioms having template atoms. Then it
   * calculates true counts for the given annotation and inferred counts for the previous MRF
   * state. Then it keeps only clauses having difference between true and inferred counts
   * above the specified threshold. If previous state is not specified, an initial state is
   * used where everything is false.
   *
   * Note: The lower the threshold the more clauses are retained
   *
   * @param definiteClauses set of definite clauses to evaluate
   * @param axioms set of axioms in order to perform predicate completion
   * @param schema predicate schema
   * @param space predicate space
   * @param evidence evidence
   * @param annotation annotation for non evidence atoms
   * @param previousState a previous MRF state to calculate inferred counts (optional)
   * @param threshold threshold for comparison
   *
   * @return a set of good definite clauses
   */
  def evaluateDefiniteClauses(definiteClauses: Set[WeightedDefiniteClause], axioms: Set[WeightedFormula],
                              schema: MLNSchema, space: PredicateSpace, evidence: Evidence, annotation: EvidenceDB,
                              threshold: Int, previousState: Option[MRFState] = None): Set[WeightedDefiniteClause] = {

    var scores = Vector[Int]()

    // Perform relaxed simplification
    val definiteClauseToCompletedFormulas = RelaxedSimplification(axioms, definiteClauses)

    // Map each definite clause into their corresponding clauses
    val definiteClauseToClauses = definiteClauseToCompletedFormulas.map { case (definiteClause , completedFormula) =>
      definiteClause -> NormalForm.compileCNF(completedFormula)(evidence.constants)
    }

    val clauses = definiteClauseToClauses.values.foldLeft(Set.empty[Clause])( (current, rest) => rest ++ current ).toVector

    val mln = MLN(schema, evidence, space, clauses)
    val mrf = new MRFBuilder(mln, createDependencyMap = true).buildNetwork

    val state = MRFState(mrf)
    state.setAnnotatedState(annotation)

    // Calculate true and inferred counts for the given clauses
    val trueCounts = state.countTrueGroundings
    debug("True Counts: [" + trueCounts.deep.mkString(", ") + "]")

    val inferredCounts = state.countTrueGroundings(previousState)
    debug("Inferred Counts: [" + inferredCounts.deep.mkString(", ") + "]")

    for (clauseIdx <- clauses.indices) {
      debug(s"${clauses(clauseIdx).toText()} T: ${trueCounts(clauseIdx)} I: ${inferredCounts(clauseIdx)}")
      scores :+= inferredCounts(clauseIdx) - trueCounts(clauseIdx)
    }

   definiteClauseToClauses.filter { case (definiteClause, setOfClauses) =>
      setOfClauses.map(c => -scores(clauses.indexOf(c))).sum >= threshold
    }.keys.toSet
  }

  /**
   * Computes predicate completion with relaxed simplification. Relaxed simplification
   * replaces each definite clause independent of the other definite clauses having
   * the same predicate.
   *
   * @param formulas the set of FOL formulas (non-definite clauses) in the input KB
   * @param definiteClauses set of definite clauses
   *
   * @return a resulting map from definite clauses to completed formulas
   */
  private def RelaxedSimplification(formulas: Set[WeightedFormula],
                                    definiteClauses: Set[WeightedDefiniteClause]): Map[WeightedDefiniteClause, Set[WeightedFormula]] = {

    var pcResultingKB = Map[WeightedDefiniteClause, Set[WeightedFormula]]()

    for (wdc <- definiteClauses) {
      val currentHead = wdc.clause.head
      var lambdaPrime = Set[WeightedFormula]()
      for (formula <- formulas) {
        if(formula.contains(currentHead.signature)) {
          val replacement = wdc.clause.body.boundVarsNotIn(currentHead)
          val resultOpt = formula.replace(currentHead, replacement)

          resultOpt match {
            case Some(result) => lambdaPrime += result
            case None => fatal("Predicate replacement failed (possible bug?)")
          }
        }
      }
      pcResultingKB += wdc -> lambdaPrime
    }
    pcResultingKB
  }

}
