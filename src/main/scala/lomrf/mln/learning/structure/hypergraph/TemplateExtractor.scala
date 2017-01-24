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

package lomrf.mln.learning.structure.hypergraph

import auxlib.log.Logger
import lomrf.logic._
import lomrf.mln.model.{ConstantsDomain, KB}
import lomrf.logic.LogicOps._
import scala.util.{Failure, Success, Try}

/**
 * Template extractor constructs the basic components for structure learning initialization. That is the Markov Logic
 * network, the annotation database, the axioms defining the path templates, as well as the path templates themselves.
 * If specified axioms have been defined (formulas having template atoms) then path templates are extracted from them
 * in order to be used for constraining the search space of the hypergraph. In any other case the mln is constructed
 * along with the annotation database as usual.
 */
object TemplateExtractor {

  private val logger = Logger(getClass)
  import logger._

  /**
   * Extracts a Markov Logic network, a annotation database for non evidence atoms and optionally a set of axioms and a
   * set of path templates from a specified knowledge base and a training evidence database. In case there are axioms defined
   * in the knowledge base (formulas having template atoms) then these axioms are extracted and the MLN is constructed without
   * them. Furthermore the axioms are used to create a set of path templates which can be used to constraint a hypergraph
   * search procedure.
   *
   * @param kb a knowledge base
   * @param nonEvidenceAtoms a set of non evidence atoms
   * @param templateAtoms a set of template atoms
   *
   * @return a Markov Logic network, a annotation database and optionally a set of axioms and a set of path templates
   */
  def apply(kb: KB, constants: ConstantsDomain, nonEvidenceAtoms: Set[AtomSignature],
            templateAtoms: Set[AtomSignature]): Try[(Set[WeightedFormula], Set[WeightedDefiniteClause], Set[Clause], Set[PathTemplate])] = {

    /**
     * @return true if the given literal has a template atom, false otherwise
     */
    def hasTemplateAtom(literal: Literal) = templateAtoms.contains(literal.sentence.signature)

    // Find all axiom formulas (e.g. formulas containing template atoms)
    val axiomFormulas = kb.formulas.filter(f => f.signatures.exists(templateAtoms.contains)) match {
      case axioms: Set[WeightedFormula] if axioms.nonEmpty => axioms
      case _ => return Failure(new Exception("Axioms not found in the given KB!"))
    }

    if (axiomFormulas.exists(f => !f.weight.isNaN && !f.weight.isInfinity))
        fatal(s"There is a weighted formula (axiom) containing template atoms! Please remove the weight to proceed.")

    info(s"Axioms having template atoms:\n ${axiomFormulas.map(a => s"\t${a.toText}").mkString("\n")}")

    // Partition the knowledge base into clauses having template atoms and the rest background clauses
    val (axioms, clauses) = kb.formulas.flatMap(_.toCNF(constants)).
                            partition(clause => clause.literals.exists(hasTemplateAtom))

    val completedFormulas =
      PredicateCompletion(
        axiomFormulas.map(a => if (a.weight.isNaN) a.copy(weight = 1.0) else a),
        kb.definiteClauses)(kb.predicateSchema, kb.functionSchema, constants)
        .filterNot(_.formula.signatures.exists(templateAtoms.contains))

    // In case template atoms are given, but there is no hard-constraint formula defined containing them -> warn or error???
    if (axioms.isEmpty && templateAtoms.nonEmpty)
      warn(s"There is no formulas (axioms) containing the template" +
           s" atoms ${templateAtoms.mkString(" ,")}. Ignoring template atoms!")

    val pathTemplates = PathTemplate.create(axioms, templateAtoms, nonEvidenceAtoms) match {
      case Success(templates) => templates
      case Failure(exception) => fatal(exception.getMessage)
    }

    // We pass copy of each axiom formula having unit weight in order to be able to perform inference during evaluation
    Success(axiomFormulas.map(a => if (a.weight.isNaN) a.copy(weight = 1.0) else a), kb.definiteClauses,
      clauses ++ NormalForm.compileCNF(completedFormulas)(constants), pathTemplates.get)
  }
}