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
package lomrf.mln.model

import org.scalatest.{Matchers, FunSpec}

class KBBuilderSpecTest extends FunSpec with Matchers {

  describe("The KB from an empty KBBuilder"){
    val builder = new KBBuilder

    val kb = builder.result()

    it("should be composed of an empty Map of constant builders"){
      assert(kb.constants.isEmpty)
    }

    it("should be composed of an empty Set of formulas"){
      assert(kb.formulas.isEmpty)
    }

    it("should be composed of an empty Map of predicate schema"){
      assert(kb.predicateSchema.isEmpty)
    }

    it("should be composed of an empty Map of function schema"){
      assert(kb.functionSchema.isEmpty)
    }

    it("should be composed of an empty Map of dynamic predicates"){
      assert(kb.dynamicPredicates.isEmpty)
    }

    it("should be composed of an empty Map of dynamic functions"){
      assert(kb.dynamicFunctions.isEmpty)
    }
  }


  //builder.withConstantBuilders(ECExampleDomain1.constants.mapValues(ConstantsSetBuilder(_)))

}
