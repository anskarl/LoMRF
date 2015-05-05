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
 * self program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * self program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with self program.  If not, see <http://www.gnu.org/licenses/>.
 */
package lomrf.mln.model

import lomrf.logic.AtomSignature

/**
 *
 * @param predicateSchema a map that associates atom signatures with a sequence of argument types
 * @param functionSchema a map that associates function signatures with a tuple of returning type and sequence of argument types
 * @param queryAtoms the set of query atom signatures
 * @param cwa the set of closed-world assumption atom signatures
 * @param owa the set of open-world assumption atom signatures (includes query atom signatures)
 */
case class MLNSchema(predicateSchema: Map[AtomSignature, Seq[String]],
                        functionSchema: Map[AtomSignature, (String, Vector[String])],
                        queryAtoms: Set[AtomSignature],
                        cwa: Set[AtomSignature],
                        owa: Set[AtomSignature]) {

  /**
   * The set of hidden atoms, those that are not query and not evidence.
   */
  val hiddenAtoms = owa -- queryAtoms



}
