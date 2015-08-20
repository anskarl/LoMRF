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

import org.scalatest.{Matchers, FunSpec}

class SimilaritySpecTest extends FunSpec with Matchers {

  val f1 = AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), Variable("t0", "time")))
  val f2 = AtomicFormula("HoldsAt", Vector(Variable("f", "fluent"), Variable("t1", "time")))


  val clause1 = Clause(Set(Literal.asNegative(f1), Literal.asNegative(f2)))
  val clause2 = Clause(Set(Literal.asNegative(f1), Literal.asPositive(f2)))

  val clause3 = Clause(Set(Literal.asNegative(f2), Literal.asPositive(f1)))


  clause1 =~= clause1 // true
  clause2 =~= clause2 // true
  clause3 =~= clause3 // true

  clause1 =~= clause2 // false

  clause2 =~= clause3 // true


}
