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

/**
 * This class contains the information of a ground atom in the generated MRF.
 *
 * @param id the atom id in the generated MRF
 * @param weightHard the weight of a hard-constrained clause
 */
final class GroundAtom(val id: Int, weightHard: Double) {

  // ----------------------------------------------------------------
  // Mutable information: accessible only from classes of inference package.
  // ----------------------------------------------------------------

  /**
   * Keeps track of the last time that this atom was flipped.
   */
  private[mln] var lastFlip = 0

  /**
   * Determines whether the state is fixed to some value (1 = true, -1 = false) or not (= 0).
   */
  private[mln] var fixedValue = 0

  /**
   * The current truth state of this particular ground atom.
   */
  private[mln] var state = false

  /**
   * The truth state of this particular ground atom in a
   * previously generated world with the lowest cost so far.
   */
  private[mln] var lowState = false

  /**
   * Keeps track how many times this atom had a true state.
   */
  private[mln] var truesCounter = 0

  /**
   * The cost that will increase after flipping.
   */
  private[mln] var breakCost = 0.0

  /**
   * The cost that will decrease after flipping.
   */
  private[mln] var makeCost = 0.0

  // ----------------------------------------------------------------
  // Publicly accessible functions
  // ----------------------------------------------------------------

  /**
   * Checks if this atom is fixed to some value (true or false).
   * @return true if the atom is fixed; otherwise false
   */
  def isFixed: Boolean = fixedValue != 0

  /**
   * Returns the current state of this atom.
   * @return true if the current state is true; otherwise false
   */
  def getState: Boolean = state

  /**
   * Checks if by flipping this particular atom at least one hard
   * constraint breaks.
   * @return true when it breaks at least one hard constraint
   */
  def breaksHardConstraint = breakCost >= weightHard

  /**
   * Computes the cost produced by flipping this atom. In case it breaks
   * at least one hard-constraint by flipping it, the cost becomes equal an
   * infinite value.
   * @return the cost when this atom is flipped
   */
  def delta = {
    if(breaksHardConstraint)
      Double.MaxValue
    else
      breakCost - makeCost
  }

  /**
   * @return the number of times that this atom have been appeared as true in the generated samples
   */
  def getTruesCount: Int = truesCounter

  override def hashCode() = id

  override def equals(obj: Any) =
    if (obj.isInstanceOf[GroundAtom]) obj.hashCode() == id else false


  // ----------------------------------------------------------------
  // Functions that are accessible only from classes of model package.
  // ----------------------------------------------------------------

  /**
   * Saves current atom truth state as low state.
   */
  private[mln] def saveAsLowState() {
    lowState = state
  }

  /**
   * Restore previously saved low state as current state.
   */
  private[mln] def restoreLowState() {
    state = lowState
  }

  /**
   * Resets brake cost and make cost of this atom.
   */
  private[mln] def resetDelta() {
    breakCost = 0.0
    makeCost = 0.0
  }

  /**
   * The given constraint will become satisfied when this atom is flipped (UNSAT -> SAT).
   */
  private[mln] def assignSatPotential(constraint: Constraint): Unit = {
    // When mode is set to MaxWalkSat:
    if(constraint.mode == MRF.MODE_MWS) {
      if(constraint.isPositive) makeCost += constraint.getWeight else breakCost -= constraint.getWeight
    }
    // otherwise, we assume that mode is set to MC-SAT:
    else {
      val unit = if(constraint.getWeight > 0) 1 else -1
      if(constraint.isPositive) makeCost += unit else breakCost -= unit
    }
  }

  /**
   * The given constraint will become unsatisfied when this atom is flipped (SAT -> UNSAT).
   */
  private[mln] def assignUnsatPotential(constraint: Constraint) {
    // When mode is set to MaxWalkSat:
    if(constraint.mode == MRF.MODE_MWS) {
      if(constraint.isPositive) breakCost += constraint.getWeight else makeCost -= constraint.getWeight
    }
    // otherwise, we assume that mode is set to MC-SAT:
    else {
      val unit = if(constraint.getWeight > 0) 1 else -1
      if(constraint.isPositive) breakCost += unit else makeCost -= unit
    }
  }

  /**
   * The given constraint will no longer becomes unsatisfied when this atom is flipped.
   */
  private[mln] def revokeSatPotential(constraint: Constraint) {
    // When mode is set to MaxWalkSat:
    if(constraint.mode == MRF.MODE_MWS) {
      if(constraint.isPositive) makeCost -= constraint.getWeight else breakCost += constraint.getWeight
    }
    // otherwise, we assume that mode is set to MC-SAT:
    else {
      val unit = if(constraint.getWeight > 0) 1 else -1
      if(constraint.isPositive) makeCost -= unit else breakCost += unit
    }
  }

  /**
   * The given constraint will no longer becomes satisfied when this atom is flipped.
   */
  private[mln] def revokeUnsatPotential(constraint: Constraint) {
    // When mode is set to MaxWalkSat:
    if(constraint.mode == MRF.MODE_MWS) {
      if(constraint.isPositive) breakCost -= constraint.getWeight else makeCost += constraint.getWeight
    }
    // otherwise, we assume that mode is set to MC-SAT:
    else {
      val unit = if(constraint.getWeight > 0) 1 else -1
      if(constraint.isPositive) breakCost -= unit else makeCost += unit
    }
  }

}
