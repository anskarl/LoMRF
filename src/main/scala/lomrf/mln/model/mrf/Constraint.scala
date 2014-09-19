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

package lomrf.mln.model.mrf


final class Constraint(val weight: Double, val literals: Array[Int], val isHardConstraint: Boolean, val threshold: Double, val id: Int = -1, var mode: Int = 0) {

  // ----------------------------------------------------------------
  // Mutable information: accessible only from classes of model package.
  // ----------------------------------------------------------------
  private[mln] var nsat: Int = 0
  private[mln] var inactive: Boolean = false

  val isPositive: Boolean = weight > 0

  val isUnit: Boolean = literals.length == 1

  def isSatisfied = nsat > 0

  /**
   * This is the cost when violating this constraint.
   * <ul>
   * <li>When the constraint has positive weight and its unsatisfied (nsat is 0), the cost is equal to its weight.</li>
   * <li>When the constraint has negative weight and its satisfied (nsat > 0), the cost is equal to its -weight.</li>
   * <li>Otherwise return zero cost, i.e. positive constraint and satisfied or negative constraint and unsatisfied. </li>
   * </ul>
   * @return the cost of violating this constraint
   */
  def cost: Double = { // modification testing
    if(mode == 0) {
      if (isPositive && nsat == 0) weight
      else if (!isPositive && nsat > 0) -weight
      else 0
    }
    else {
      if (isPositive && nsat == 0) 1
      else if (!isPositive && nsat > 0) 1
      else 0
    }
  }

  /**
   * Gives the number of literals satisfying this constraint
   */
  def getNSat: Int = nsat

  override def hashCode() = id

  override def equals(obj: Any) = obj match {
    case o: Constraint => o.id == id
    case _ => false
  }


}
