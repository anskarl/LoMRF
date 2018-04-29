/*
 *
 *  o                        o     o   o         o
 *  |             o          |     |\ /|         | /
 *  |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 *  |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 *  O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *              |
 *           o--o
 *  o--o              o               o--o       o    o
 *  |   |             |               |    o     |    |
 *  O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 *  |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 *  o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 *  Logical Markov Random Fields (LoMRF).
 *
 *
 */

package lomrf.mln.model.mrf

import lomrf.util.LongDoubleConversions._

final class Constraint(private var weight: Double, val literals: Array[Int], val isHardConstraint: Boolean, val threshold: Double,
    val id: Int = -1, var mode: Int = MRF.MODE_MWS) {

  // ----------------------------------------------------------------
  // Mutable information: accessible only from classes of model package.
  // ----------------------------------------------------------------

  /**
    * High precision weight used for inference computations.
    */
  private[mln] var hpWeight = new LongDouble(weight)

  /**
    * Number of literals satisfying the constraint.
    */
  private[mln] var nsat: Int = 0

  /**
    * Indicates if the constraint is inactive.
    */
  private[mln] var inactive: Boolean = false

  /**
    * Indicates if the constraint is satisfied by
    * any (at least one) fixed atom.
    */
  private[mln] var isSatisfiedByFixed = false

  /**
    * Keeps track of the first atom this constraint is
    * watching.
    */
  private[mln] var watchLit1: Int = 0

  /**
    * Keeps track of the second atom this constraint is
    * watching.
    */
  private[mln] var watchLit2: Int = 0

  /**
    * Checks if the constraint is unit, having
    * exactly one literal.
    */
  val isUnit: Boolean = literals.length == 1

  /**
    * Checks if the constraint is positive.
    */
  def isPositive: Boolean = weight > 0

  /**
    * Checks if the constraint is satisfied at the
    * current state.
    * @return true if the constraint is satisfied; otherwise false
    */
  def isSatisfied = nsat > 0

  /**
    * Checks if the given literal is contained in this constraint. It uses
    * binary search as it guarentees O(logn) complexity to find it fast. The
    * literals array is always sorted.
    *
    * @param lit the given literal number
    * @return true if the literal is contained
    */
  def containsLiteral(lit: Int) = java.util.Arrays.binarySearch(literals, lit) >= 0

  /**
    * This is the cost when violating this constraint. If we are in MaxWalkSAT mode then the cost depends on the
    * weight of the constraint, otherwise in SampleSAT mode all costs are unit.
    * <ul>
    * <li>When the constraint has positive weight and its unsatisfied (nsat is 0), the cost is equal to its weight.</li>
    * <li>When the constraint has negative weight and its satisfied (nsat > 0), the cost is equal to its -weight.</li>
    * <li>Otherwise return zero cost, i.e. positive constraint and satisfied or negative constraint and unsatisfied. </li>
    * </ul>
    *
    * @return the cost of violating this constraint
    */
  def cost: LongDouble = {
    if ((isPositive && nsat == 0) || (!isPositive && nsat > 0)) {
      // when mode is set to MaxWalkSat, return the absolute value the high precision value
      if (mode == MRF.MODE_MWS) hpWeight.abs()
      else ONE //otherwise, we assume that mode is set to MC-SAT and return the value 1.
    } else ZERO
  }

  /**
    * Returns the number of literals satisfying this constraint.
    */
  def getNSat: Int = nsat

  /**
    * Returns the weight of the constraint
    */
  def getWeight: Double = weight

  /**
    * Set the weight of the constraint. Also changes
    * the value of high precision weight.
    *
    * @param weight given weight
    */
  private[mln] def setWeight(weight: Double) = {
    hpWeight = new LongDouble(weight)
    this.weight = weight
  }

  override def hashCode() = id

  override def equals(obj: Any) = obj match {
    case o: Constraint => o.id == id
    case _             => false
  }

}
