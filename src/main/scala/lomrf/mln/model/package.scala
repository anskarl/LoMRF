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

package lomrf.mln

import lomrf.logic.AtomSignature
import lomrf.mln.model.AtomIdentityFunction

package object model {

  type ConstantsDomain = Map[String, ConstantsSet]

  type PredicateSchema = Map[AtomSignature, Seq[String]]

  type FunctionSchema = Map[AtomSignature, (String, Vector[String])]

  type DynamicPredicates = Map[AtomSignature, Vector[String] => Boolean]

  type DynamicFunctions = Map[AtomSignature, Vector[String] => String]

  type EvidenceDB = Map[AtomSignature, AtomEvidenceDB]

  type FunctionMappers = Map[AtomSignature, FunctionMapper]

  type Identities = Map[AtomSignature, AtomIdentityFunction]


  implicit class FunctionSchemaWrapped(val fs: FunctionSchema) extends AnyVal {

    def toPredicateSchema: PredicateSchema = fs.map {
      case (signature, (retType, argTypes)) =>
        val symbol = lomrf.AUX_PRED_PREFIX + signature.symbol
        val termTypes = argTypes.+:(retType)
        (AtomSignature(symbol, signature.arity + 1), termTypes)
    }
  }

}
