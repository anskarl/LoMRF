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

/**
 * A state of a ground atom can be either from the following three:
 * <ol>
 *   <li>TRUE</li>
 *   <li>FALSE</li>
 *   <li>UNKNOWN</li>
 * </ol>
 *
 *
 */
sealed trait TriState{
  val value: Byte

  /**
   * Gives the conjunction of two states
   * <table border="1">
   *   <tr><td></td>         <td>True</td>  <td>False</td> <td>Unknown</td></tr>
   *   <tr><td>True</td>     <td>True</td>  <td>False</td> <td>Unknown</td></tr>
   *   <tr><td>False</td>    <td>False</td> <td>False</td> <td>False</td></tr>
   *   <tr><td>Unknown</td>  <td>Unknown</td>  <td>False</td> <td>Unknown</td></tr>
   * </table>
   */
  def ^(other: TriState): TriState = {
    val prod = this.value * other.value
    if(prod == 1 ) TRUE
    else if(prod == -1) FALSE
    else UNKNOWN
  }

  /**
   * Gives the disjunction of two states
   * <table border="1">
   *   <tr><td></td>         <td>True</td>  <td>False</td> <td>Unknown</td></tr>
   *   <tr><td>True</td>     <td>True</td>  <td>True</td> <td>True</td></tr>
   *   <tr><td>False</td>    <td>True</td> <td>False</td> <td>Unknown</td></tr>
   *   <tr><td>Unknown</td>  <td>True</td>  <td>Unknown</td> <td>Unknown</td></tr>
   * </table>
   */
  def v(other: TriState): TriState = {
    if(this.value == 1 || other.value == 1) TRUE
    else if (this.value == 0 && other.value == 0) UNKNOWN
    else FALSE
  }

  /**
   * Flips the current state.
   * <table border="1">
   *   <tr><td>State:</td>          <td>True</td> <td>False</td> <td>Unknown</td></tr>
   *   <tr><td>Flipped state: </td> <td>False</td> <td>True</td> <td>Unknown</td></tr>
   * </table>
   */
  def flip : TriState = if(this.value == -1) TRUE else if(this.value == 1) FALSE else UNKNOWN

  override def hashCode() = value
  
  override def equals(obj: Any): Boolean = obj match{
    case other: TriState => other.value == this.value
    case _ => false
  }

  
}

/**
 * This object represents a TRUE state.
 */
case object TRUE extends TriState{
  val value: Byte = 1

  override def toString = "true"
}

/**
 * This object represents a FALSE state.
 */
case object FALSE extends TriState{
  val value: Byte = -1

  override def toString = "false"
}

/**
 * This object represents an UNKNOWN state.
 */
case object UNKNOWN extends TriState{
  val value: Byte = 0
  
  override def toString = "unknown"
}
