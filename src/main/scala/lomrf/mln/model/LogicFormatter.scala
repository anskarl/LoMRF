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

package lomrf.mln.model

import lomrf.logic._
import lomrf.logic.LogicOps._

/**
 * Logic formatter performs operations over clauses and definite clauses. These operations
 * are function introduction and elimination using auxiliary predicates.
 *
 * For example the clause Predicate(x, y) v AUXfunction(x, t, z) after function introduction operation
 * should become Predicate(function(t, z), y). The inverse process is to transform the latter formula into
 * the former and is called function elimination.
 *
 * Note that the first variable 'x' of the auxiliary predicate is also eliminated during function introduction,
 * because it represents return value of the function introduced.
 */
object LogicFormatter {

  /**
   * Formatting operations over clauses.
   */
  object ClauseFormatter {

    /**
     * Introduce functions to all clauses in the set having auxiliary predicates and return
     * another set containing the transformed clauses.
     *
     * @param clauses a set of clauses for function introduction
     * @return another set of clauses having functions
     */
    def introduceFunctions(clauses: Iterable[Clause]): Iterable[Clause] = clauses.map(introduceFunctions)

    /**
     * Introduce functions to the given clause and return another clause if any auxiliary predicates
     * exist. If the given clause is unit and have a negative weight then the returned clause would
     * have a positive weight and flipped sign.
     *
     * @param clause the clause for function introduction
     * @return another clause having functions
     */
    def introduceFunctions(clause: Clause): Clause = {

      require(!clause.literals.filter(_.isPositive).exists(_.sentence.symbol.contains(lomrf.AUX_PRED_PREFIX)),
        "Clauses does not support auxiliary predicates as positive literals!")

      if (clause.isUnit && clause.weight < 0) return introduceFunctions(Clause(Set(clause.literals.head.negate), -clause.weight))

      // Partition all clause literals into a set of literals containing function and another one not containing any function
      val (literalsNoFunctions, literalsFunctions) = clause.literals.partition(!_.sentence.symbol.contains(lomrf.AUX_PRED_PREFIX))

      // If literals containing functions exist
      if (literalsFunctions.nonEmpty) {

        var lMap = Map[Term, TermFunction]()

        // Create a map of function return values of auxiliary predicates to functions
        literalsFunctions.foreach { literal =>
          val sentence = literal.sentence
          val functionSymbol = sentence.symbol.replace(lomrf.AUX_PRED_PREFIX, "")
          val functionVar = sentence.terms.head
          val terms = sentence.terms.drop(1)
          val function = TermFunction(functionSymbol, terms, functionVar.asInstanceOf[Variable].domain)
          lMap += (functionVar -> function)
        }

        val replacedLiterals = literalsNoFunctions.map { literal =>
          val sentence = literal.sentence
          val replacedArgs = for (arg <- sentence.terms) yield arg match {
            case term: Term => if (lMap.contains(term)) lMap(term) else term
          }
          literal match {
            case p: PositiveLiteral => Literal.asPositive(AtomicFormula(sentence.symbol, replacedArgs))
            case n: NegativeLiteral => Literal.asNegative(AtomicFormula(sentence.symbol, replacedArgs))
          }
        }

        if (replacedLiterals.nonEmpty) Clause(replacedLiterals, clause.weight) else clause
      }
      else clause
    }

    /**
     * Eliminate functions to all clauses in the set and return another set containing
     * the transformed clauses having auxiliary predicates.
     *
     * @param clauses a set of clauses for function elimination
     * @return another set of clauses having auxiliary predicates
     */
    def eliminateFunctions(clauses: Iterable[Clause]): Iterable[Clause] = clauses.map(eliminateFunctions)

    /**
     * Eliminate functions to the given clause and return another clause having auxiliary predicates. If
     * the given clause is unit and have a negative weight then the returned clause would have a
     * positive weight and flipped sign.
     *
     * @param clause the clause for function elimination
     * @return another clause having auxiliary predicates
     */
    def eliminateFunctions(clause: Clause): Clause = {

      if (clause.isUnit && clause.weight < 0) return eliminateFunctions(Clause(Set(clause.literals.head.negate), -clause.weight))

      val (literalsNoFunctions, literalsFunctions) = clause.literals.span(_.sentence.functions.isEmpty)

      if(literalsFunctions.nonEmpty) {

        var fMap = Map[TermFunction, (Variable, Literal)]()
        var functionCounter = 0

        // When a function is eliminated is replaced by a negated auxiliary predicate
        clause.functions.foreach{ function =>
          fMap.get(function) match {

            case None =>
              val functionVar = Variable(lomrf.FUNC_RET_VAR_PREFIX + functionCounter, function.domain)
              val terms = Vector(functionVar) ++: function.terms
              val functionLiteral = NegativeLiteral(AtomicFormula(lomrf.AUX_PRED_PREFIX + function.symbol, terms))
              fMap += (function ->(functionVar, functionLiteral))
              functionCounter += 1

            case _ => // do nothing
          }
        }

        val replacedLiterals = literalsFunctions.map { literal =>
          val sentence = literal.sentence
          val replacedArgs = for(arg <- sentence.terms) yield arg match {
            case f: TermFunction => fMap(f)._1
            case t: Term => t
          }
          literal match {
            case p: PositiveLiteral => Literal.asPositive(AtomicFormula(sentence.symbol, replacedArgs))
            case n: NegativeLiteral => Literal.asNegative(AtomicFormula(sentence.symbol, replacedArgs))
          }
        }

        Clause(literalsNoFunctions ++ replacedLiterals ++ fMap.values.map(_._2), clause.weight)
      }
      else clause
    }

  }

  /**
   * Formatting operations over definite clauses.
   */
  object DefiniteClauseFormatter {

    /**
     * Introduce functions to all definite clauses in the set having auxiliary predicates and return
     * another set containing the transformed definite clauses.
     *
     * @param definiteClauses a set of definite clauses for function introduction
     * @return another set of definite clauses having functions
     */
    def introduceFunctions(definiteClauses: Iterable[DefiniteClause]): Iterable[DefiniteClause] = definiteClauses.map(introduceFunctions)

    /**
     * Introduce functions to the given definite clause and return another definite clause if any
     * auxiliary predicates exist.
     *
     * @param definiteClause the definite clause for function introduction
     * @return another definite clause having functions
     */
    def introduceFunctions(definiteClause: DefiniteClause): DefiniteClause = {

      require(!definiteClause.bodyLiterals.filter(_.isNegative).exists(_.sentence.symbol.contains(lomrf.AUX_PRED_PREFIX)),
        "Definite clauses does not support auxiliary predicates as negated literals!")

      val (literalsNoFunctions, literalsFunctions) = definiteClause.literals.partition(!_.sentence.symbol.contains(lomrf.AUX_PRED_PREFIX))

      // If literals containing functions exist
      if (literalsFunctions.nonEmpty) {

        var lMap = Map[Term, TermFunction]()

        // Create a map of function return values of auxiliary predicates to functions
        literalsFunctions.foreach { literal =>
          val sentence = literal.sentence
          val functionSymbol = sentence.symbol.replace(lomrf.AUX_PRED_PREFIX, "")
          val functionVar = sentence.terms.head
          val terms = sentence.terms.drop(1)
          val function = TermFunction(functionSymbol, terms, functionVar.asInstanceOf[Variable].domain)
          lMap += (functionVar -> function)
        }

        val replacedLiterals = literalsNoFunctions.map { literal =>
          val sentence = literal.sentence
          val replacedArgs = for (arg <- sentence.terms) yield arg match {
            case term: Term => if (lMap.contains(term)) lMap(term) else term
          }
          literal match {
            case p: PositiveLiteral => Literal.asPositive(AtomicFormula(sentence.symbol, replacedArgs))
            case n: NegativeLiteral => Literal.asNegative(AtomicFormula(sentence.symbol, replacedArgs))
          }
        }

        val replacedHead = replacedLiterals.find(_.sentence.symbol == definiteClause.head.symbol).get

        if (replacedLiterals.nonEmpty) DefiniteClause(replacedHead.sentence, (replacedLiterals - replacedHead).map(l => if (!l.positive) Not(l.sentence) else l.sentence).reduce(And)) else definiteClause
      }
      else definiteClause
    }

    /**
     * Eliminate functions to all definite clauses in the set and return another set containing
     * the transformed definite clauses having auxiliary predicates.
     *
     * @param definiteClauses a set of definite clauses for function elimination
     * @return another set of definite clauses having auxiliary predicates
     */
    def eliminateFunctions(definiteClauses: Iterable[DefiniteClause]): Iterable[DefiniteClause] = definiteClauses.map(eliminateFunctions)

    /**
     * Eliminate functions to the given definite clause and return another definite clause
     * having auxiliary predicates.
     *
     * @param definiteClause the definite clause for function elimination
     * @return another definite clause having auxiliary predicates
     */
    def eliminateFunctions(definiteClause: DefiniteClause): DefiniteClause = {

      val (literalsNoFunctions, literalsFunctions) = definiteClause.literals.partition(_.sentence.functions.isEmpty)

      if(literalsFunctions.nonEmpty) {

        var fMap = Map[TermFunction, (Variable, Literal)]()
        var functionCounter = 0

        definiteClause.functions.foreach{ function =>
          fMap.get(function) match {

            case None =>
              val functionVar = Variable(lomrf.FUNC_RET_VAR_PREFIX + functionCounter, function.domain)
              val terms = Vector(functionVar) ++: function.terms
              val functionLiteral = PositiveLiteral(AtomicFormula(lomrf.AUX_PRED_PREFIX + function.symbol, terms))
              fMap += (function ->(functionVar, functionLiteral))
              functionCounter += 1

            case _ => // do nothing
          }
        }

        val replacedLiterals = literalsFunctions.map { literal =>
          val sentence = literal.sentence
          val replacedArgs = for(arg <- sentence.terms) yield arg match {
            case f: TermFunction => fMap(f)._1
            case t: Term => t
          }
          literal match {
            case p: PositiveLiteral => Literal.asPositive(AtomicFormula(sentence.symbol, replacedArgs))
            case n: NegativeLiteral => Literal.asNegative(AtomicFormula(sentence.symbol, replacedArgs))
          }
        }

        val replacedHead = replacedLiterals.find(_.sentence.symbol == definiteClause.head.symbol).get

        if (replacedLiterals.nonEmpty) DefiniteClause(replacedHead.sentence, ((replacedLiterals ++ literalsNoFunctions ++ fMap.values.map(_._2)) - replacedHead).map(l => if (!l.positive) Not(l.sentence) else l.sentence).reduce(And)) else definiteClause
      }
      else definiteClause
    }

  }

  /**
   * Formatting operations over weighted definite clauses.
   */
  object WeightedDefiniteClauseFormatter {

    /**
     * Introduce functions to the given weighted definite clause and return another definite clause if any
     * auxiliary predicates exist.
     *
     * @param weightedDefiniteClause the weighted definite clause for function introduction
     * @return another weighted definite clause having functions
     */
    def introduceFunctions(weightedDefiniteClause: WeightedDefiniteClause): WeightedDefiniteClause =
      WeightedDefiniteClause(weightedDefiniteClause.weight, DefiniteClauseFormatter.introduceFunctions(weightedDefiniteClause.clause))

    /**
     * Introduce functions to all weighted definite clauses in the set having auxiliary predicates and return
     * another set containing the transformed definite clauses.
     *
     * @param weightedDefiniteClauses a set of weighted definite clauses for function introduction
     * @return another set of weighted definite clauses having functions
     */
    def introduceFunctions(weightedDefiniteClauses: Iterable[WeightedDefiniteClause]): Iterable[WeightedDefiniteClause] = weightedDefiniteClauses.map(introduceFunctions)

    /**
     * Eliminate functions to the given weighted definite clause and return another definite clause
     * having auxiliary predicates.
     *
     * @param weightedDefiniteClause the weighted definite clause for function elimination
     * @return another weighted definite clause having auxiliary predicates
     */
    def eliminateFunctions(weightedDefiniteClause: WeightedDefiniteClause): WeightedDefiniteClause =
      WeightedDefiniteClause(weightedDefiniteClause.weight, DefiniteClauseFormatter.eliminateFunctions(weightedDefiniteClause.clause))

    /**
     * Eliminate functions to all weighted definite clauses in the set and return another set containing
     * the transformed definite clauses having auxiliary predicates.
     *
     * @param weightedDefiniteClauses a set of weighted definite clauses for function elimination
     * @return another set of weighted definite clauses having auxiliary predicates
     */
    def eliminateFunctions(weightedDefiniteClauses: Iterable[WeightedDefiniteClause]): Iterable[WeightedDefiniteClause] = weightedDefiniteClauses.map(eliminateFunctions)
  }
}