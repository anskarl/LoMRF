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

import lomrf.logic.AtomSignature
import lomrf.logic.parser.KBParser
import lomrf.mln.model.ConstantsSet
import org.scalatest.{FunSpec, Matchers}

/**
 * Specification test for path templates.
 */
final class PathTemplateSpecTest extends FunSpec with Matchers {

  // Predicate schema having template atoms, evidence atoms and non-evidence atoms
  private val predicateSchema = Map (
    AtomSignature("TemplateAtom_1", 2) -> Vector("X", "T"),
    AtomSignature("TemplateAtom_2", 2) -> Vector("X", "T"),
    AtomSignature("EvidenceAtom_1", 2) -> Vector("Y", "T"),
    AtomSignature("EvidenceAtom_2", 2) -> Vector("T", "T"),
    AtomSignature("NonEvidenceAtom_1", 2) -> Vector("X", "T")
  )

  // Empty function schema
  private val functionsSchema = Map.empty[AtomSignature, (String, Vector[String])]

  // Constants domain
  private val constantsDomain = Map (
    "T" -> ConstantsSet((1 to 10).map(_.toString)),
    "X" -> ConstantsSet("X1", "X2", "X3", "X4"),
    "Y" -> ConstantsSet("Y1", "Y2", "Y3", "Y4")
  )

  private val parser = new KBParser(predicateSchema, functionsSchema)

  // ------------------------------------------------------------------------------------------------------------------
  // --- TEST: The Event Calculus case
  // ------------------------------------------------------------------------------------------------------------------

  // Template atoms
  private val templateAtomsPerAxiom = Seq(AtomSignature("TemplateAtom_1", 2),
                                          AtomSignature("TemplateAtom_2", 2),
                                          AtomSignature("TemplateAtom_2", 2),
                                          AtomSignature("TemplateAtom_1", 2))

  val axioms = Seq (
    "EvidenceAtom_2(t1, t0) ^ TemplateAtom_1(x, t0) => NonEvidenceAtom_1(x, t1).",
    "EvidenceAtom_2(t1, t0) ^ TemplateAtom_2(x, t0) => !NonEvidenceAtom_1(x, t1).",
    "EvidenceAtom_2(t1, t0) ^ NonEvidenceAtom_1(x, t0) ^ !TemplateAtom_2(x, t0) => NonEvidenceAtom_1(x, t1).",
    "EvidenceAtom_2(t1, t0) ^ !NonEvidenceAtom_1(x, t0) ^ !TemplateAtom_1(x, t0) => !NonEvidenceAtom_1(x, t1)."
  ).map(parser.parseLogicalSentence).flatMap(_.toCNF(constantsDomain))

  info(axioms.map(_.literals.map(_.toText).mkString(" v ")).mkString("\n"))

  //val pathTemplate = PathTemplate(Set("X", "T"))

  //axioms zip templateAtomsPerAxiom foreach { case (axiom, template) => pathTemplate + (axiom, template) }

  //info(s"$pathTemplate")

  // TODO
}
