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

package lomrf.mln.model.mrf

import java.util
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicBoolean
import auxlib.log.Logging
import gnu.trove.list.array.TIntArrayList
import gnu.trove.map.hash.TIntIntHashMap
import lomrf.logic.TRUE
import lomrf.mln.model._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.collection.parallel.mutable.ParArray
import scala.util.{Failure, Success}
import lomrf.logic.AtomSignatureOps._
import scalaxy.streams.optimize

/**
 * This class represents the MRF state.
 *
 * @param mrf the source MRF
 * @param parAtoms parallel array holding the ground atoms
 * @param parConstraints parallel array holding the constraints
 * @param satHardPriority Satisfiability priority to hard constrained clauses (default is false)
 *
 */
final class MRFState private(val mrf: MRF,
                             parAtoms: ParArray[GroundAtom],
                             parConstraints: ParArray[Constraint],
                             satHardPriority: Boolean = false) extends Logging {

  import lomrf.util.LongDoubleConversions._

  private val atoms = mrf.atoms
  private var dirtyAtoms = new mutable.HashSet[GroundAtom]()
  private var priorityBuffer = new ArrayBuffer[Constraint]()

  private var totalCost = MAXVALUE
  private var lowCost = MAXVALUE
  private var lowUnsat = Int.MaxValue
  private var totalActive = mrf.numberOfConstraints

  private var mode = MRF.MODE_MWS

  @inline private def atomID(lit: Int) = math.abs(lit)

  @inline private def state(lit: Int) = atoms.get(atomID(lit)).state

  @inline private def isTrueLiteral(lit: Int): Boolean = (lit > 0) == state(lit)

  def apply(aid: Int): Boolean = state(aid)

  /**
   * Set a given annotated state as the current MRF state. Fetch annotated values
   * from database for each atom id and replace the existing ones in the MRF. Annotation
   * should exist only for non evidence atoms.
   *
   * @param annotationDB the evidence db containing the query atom annotation
   */
  def setAnnotatedState(annotationDB: EvidenceDB) = {
    require(annotationDB.keySet == mrf.mln.queryAtoms, "Annotation should exist only for non evidence atoms")
    val atomsIterator = atoms.iterator()

    while(atomsIterator.hasNext) {
      atomsIterator.advance()
      val atom = atomsIterator.value()
      if (annotationDB(atom.id.signature(mrf.mln))(atom.id) == TRUE)
        atom.state = true
      else atom.state = false
    }
  }

  /**
   * Count the true groundings of the clauses produced the constraints of
   * this state.
   *
   * @return the number of true groundings for each clause of this state.
   */
  def countTrueGroundings: Array[Int] = countGroundings()

  /**
   * Count the true groundings of the clauses produced the constraints of
   * this state with respect to the truth values of another state (probably previous).
   *
   * @param previousState another state
   * @return the number of true groundings for each clause of this state
   */
  def countTrueGroundings(previousState: Option[MRFState]): Array[Int] = countGroundings(usePreviousState = true, previousState)

  /**
   * Count the number of true groundings of each clause in the data. In order to do this, we compute the
   * satisfied literals of each ground clause given an MRF state (annotation or inferred). Then if the number
   * of satisfied literals are greater than zero and the weight of the clause that produced it has not been
   * flipped we can count it as true ground clause. On the other hand if the weight has been flipped then in
   * order to count it the number of satisfied literals should be zero.
   *
   * Note: If another state is given we count the true groundings with respect to that state using
   *       the constraints (ground clauses) of this state.
   *
   * @return count of true groundings of the clauses
   */
  private def countGroundings(usePreviousState: Boolean = false, previousState: Option[MRFState] = None): Array[Int] = {

    val dependencyMap = mrf.dependencyMap.getOrElse(sys.error("Dependency map does not exists."))

    val counts = Array.fill[Int](mrf.mln.clauses.length)(0)

    val constraintIterator = mrf.constraints.iterator()

    // Keeps the count of literals satisfying the current constraint
    var nsat = 0

    // literal index of the current constraint
    var idx = 0

    while (constraintIterator.hasNext) {
      constraintIterator.advance()
      val currentConstraint = constraintIterator.value()

      // --- Compute the number of literals that satisfy the current constraint
      nsat = 0 // Reset
      idx = 0 // Reset
      while (idx < currentConstraint.literals.length) {
        val lit = currentConstraint.literals(idx)
        val state = if (usePreviousState) previousState match {
                        case Some(otherState) => otherState.mrf.fetchAtom(lit).state
                        case None => false
                    }
                    else mrf.fetchAtom(lit).state

        if ( (lit > 0) == state ) nsat += 1
        idx += 1
      }

      val iterator = dependencyMap.get(currentConstraint.id).iterator()

      while(iterator.hasNext) {
        iterator.advance()
        val clauseIdx = iterator.key()
        val frequency = iterator.value()

        // If weight is flipped then we want to count the opposite type of grounding
        // Use math abs because frequency may be negative to indicate a weight is flipped
        if( (frequency < 0 && nsat == 0) || (frequency > 0 && nsat > 0) )
          counts(clauseIdx) += math.abs(frequency.toInt) // Clauses cannot have float frequencies at this point!
      }

      // --- --- --- --- --- --- --- --- --- ---
    }

    counts
  }

  /**
   * Set inference mode (MaxWalkSAT or SampleSAT). Used for inference in order
   * to change cost into unit.
   *
   * @param mode inference mode
   */
  def setInferenceMode(mode: Int) {
    parConstraints.foreach(_.mode = mode)
    this.mode = mode
  }

  /**
   * Computes the sum of the unsatisfied clauses where atom appears as a positive
   * literal minus the sum of the unsatisfied clauses where atom appears as a
   * negative literal.
   *
   * @param atomID atom id
   * @return delta cost
   */
  def computeDelta(atomID: Int): Double = {
    var delta = 0.0

    val pos = mrf.pLit2Constraints.get(atomID)
    val neg = mrf.nLit2Constraints.get(atomID)

    if(pos ne null) {
      val positive = pos.iterator
      while (positive.hasNext) {
        val currentConstraint = positive.next()
        if (!currentConstraint.isSatisfiedByFixed)
          if (currentConstraint.getWeight > 1000) delta += 1000 else delta += currentConstraint.getWeight
      }
    }

    if(neg ne null) {
      val negative = neg.iterator
      while (negative.hasNext) {
        val currentConstraint = negative.next()
        if (!currentConstraint.isSatisfiedByFixed)
          if (currentConstraint.getWeight > 1000) delta -= 1000 else delta -= currentConstraint.getWeight
      }
    }

    delta
  }

  /**
   * Refine current state by setting constraints to satisfied by fixed atom if the
   * previously fixed atom given appears as a positive or negative literal in
   * the constraint. Used by ILP roundup procedure.
   *
   * @param atomID atom id
   */
  def refineState(atomID: Int): Unit = {

    val atomState = state(atomID)
    val pos = mrf.pLit2Constraints.get(atomID)
    val neg = mrf.nLit2Constraints.get(atomID)

    val constraints = if(atomState) pos else neg

    // It doesn't matter if it is already true
    if(constraints ne null) {
      val constraintsIterator = constraints.iterator
      while (constraintsIterator.hasNext) {
        constraintsIterator.next().isSatisfiedByFixed = true
      }
    }
  }

  /**
   * Print MRF statistics about the current state.
   * <ul>
   * <li> Number of unsatisfied constraints (negative, positive and hard)
   * <li> Total cost of the state
   * <li> Likelihood of the state
   * <li> Likelihood upper bound (satisfy all positive and hard constraints and none of the negative)
   * <ul>
   */
  def printStatistics() {
    
    val unsatisfied = Unsatisfied.size

    var countNeg = 0

    optimize{
      for(i <- 0 until unsatisfied)
        if(Unsatisfied(i).getWeight < 0) countNeg += 1
    }

    var likelihood, likelihoodUB = ZERO

    val iterator = mrf.constraints.iterator()

    while(iterator.hasNext) {
      iterator.advance()
      val c = iterator.value()
      if(c.isSatisfied) likelihood += c.hpWeight
      if(c.getWeight > 0 || c.isHardConstraint) likelihoodUB += c.hpWeight
    }

    info {
        "MRF state statistics: \n" +
        "\tUnsatisfied clauses: " + unsatisfied + "/" + totalActive + "\n" +
        "\tUnsatisfied negative constraints: " + countNeg + "/" + unsatisfied + "\n" +
        "\tUnsatisfied positive constraints: " + (unsatisfied - Unsatisfied.numOfHard - countNeg) + "/" + unsatisfied + "\n" +
        "\tUnsatisfied hard constraints: " + Unsatisfied.numOfHard + "/" + unsatisfied + "\n" +
        "\tTotal cost: " + totalCost + "\n" +
        "\tLikelihood is e^" + likelihood + "\n" +
        "\tLikelihood upper bound is e^" + likelihoodUB
    }
  }

  /**
   * Randomises the state and re-evaluates the MRF.
   *
   * @param tabuLength the tabu length used for tabu search heuristic
   * @param withUnitPropagation enables/disables unit propagation
   */
  def reset(tabuLength: Int = 10, withUnitPropagation: Boolean = false) {
    if (withUnitPropagation) unitPropagation()

    parAtoms.foreach {
      atom =>
        // randomize state only for unfixed atoms
        if (atom.fixedValue == 0) atom.state = ThreadLocalRandom.current().nextBoolean()
        // reset break and make costs
        atom.resetDelta()
        // reset last flip
        atom.lastFlip = -(tabuLength + 1)
    }

    lowCost = evaluateCosts()
    lowUnsat = Unsatisfied.size

    saveAsLowState()

    dirtyAtoms = new mutable.HashSet[GroundAtom]()
    priorityBuffer = new ArrayBuffer[Constraint]()

    debug{
        "[Reset] Total cost: " + lowCost + "\n" +
        "[Reset] Unsatisfied: " + lowUnsat
    }
  }

  // -----------------------------------
  // State functions
  // -----------------------------------

  /**
   * Randomize the state for all non fixed atoms.
   */
  def randomise() {
    parAtoms.foreach(atom => if (atom.fixedValue == 0) atom.state = ThreadLocalRandom.current().nextBoolean())
  }

  /**
   * Counts the number of unfixed atoms in the current state.
   *
   * @return number of unfixed atoms
   */
  def countUnfixAtoms(): Int = {
    var count = 0
    val iterator = mrf.atoms.iterator()

    while(iterator.hasNext) {
      iterator.advance()
      if(iterator.value().fixedValue == 0) count += 1
    }
    count
  }

  /**
   * Unfix all atoms and constraints satisfied by them.
   */
  def unfixAll() {
    parAtoms.foreach(_.fixedValue = 0)
    parConstraints.foreach(_.isSatisfiedByFixed = false)
  }


  /**
   * Performs unit propagation across the constraints in order to trivially
   * satisfy as many as possible. It is required by MCSAT in order to minimize
   * the search space and increase sampling performance and accuracy.
   */
  private def unitPropagation() {

    /**
     * Fixes an atom to the given state, if the atom is currently unfixed.
     *
     * @param atomID the atom id
     * @param state the desired state
     */
    @inline def fixAtom(atomID: Int, state: Boolean) {
      val atom = atoms.get(atomID)
      //check for contradiction
      if (atom.fixedValue == 1 && !state || atom.fixedValue == -1 && state)
        sys.error("Contradiction found for atomID " + atomID)

      synchronized {
        if (atom.fixedValue == 0) {
          atom.fixedValue = if (state) 1 else -1
          atom.state = state
          updateSatisfiedByFix(atomID)
        }
      }
    }

    /**
     * Updates satisfied constraints by fixed atoms.
     *
     * @param atomID the fixed atom id
     */
    @inline def updateSatisfiedByFix(atomID: Int) {

      val state = mrf.atoms.get(atomID).state
      val constants = if(state) mrf.pLit2Constraints.get(atomID) else mrf.nLit2Constraints.get(atomID)

      if (constants ne null) {
        val iterator = constants.iterator
        while(iterator.hasNext) {
          val constraint = iterator.next()
          if(!constraint.inactive && !constraint.isSatisfiedByFixed)
            constraint.isSatisfiedByFixed = true
        }
      }
    }

    // 1. Unfix all atoms and reset constraints
    unfixAll()

    // 2. Process negative constraints.
    val nIterator = mrf.constraints.iterator()
    while (nIterator.hasNext) {
      nIterator.advance()
      val constraint = nIterator.value()
      if (!constraint.inactive && !constraint.isPositive && !constraint.isSatisfiedByFixed) optimize{
        for (i <- constraint.literals.indices)
          fixAtom(math.abs(constraint.literals(i)), constraint.literals(i) < 0)
      }
    }

    // 3. Process positive constraints.
    //var done = false
    val done = new AtomicBoolean(false)

    while (!done.get()) {
      //done = true
      done.set(true)
      parConstraints.foreach { constraint =>
        /*val pIterator = mrf.constraints.iterator()

        while (pIterator.hasNext) {
          pIterator.advance()
          val constraint = pIterator.value()*/
        if (!constraint.inactive && constraint.isPositive && !constraint.isSatisfiedByFixed) {

          var numOfNonFixedAtoms = 0
          var nonFixedLiteral = 0

          var idx = 0
          var isSat = false

          while (idx < constraint.literals.length) {
            val lit = constraint.literals(idx)
            val atomID = math.abs(lit)
            val atom = atoms.get(atomID)

            if (atom.fixedValue == 0) {
              nonFixedLiteral = lit
              numOfNonFixedAtoms += 1
            }
            else if( (atom.fixedValue == 1 && lit > 0) || (atom.fixedValue == -1 && lit < 0) ){
              isSat = true
              idx = constraint.literals.length
            }
            idx += 1
          }

          if(isSat) constraint.isSatisfiedByFixed = true
          else if(numOfNonFixedAtoms == 1) {
            fixAtom(math.abs(nonFixedLiteral), nonFixedLiteral > 0)
            //done = false
            done.set(false)
          }
        }
      }
    }

  }

  // -----------------------------------
  // Selection methods (Slice sampling)
  // -----------------------------------

  /**
   * Select only hard constraints, by setting all others to inactive.
   *
   * @return total active hard constraints
   */
  def selectOnlyHardConstraints(): Int = {
    totalActive = 0
    val iterator = mrf.constraints.iterator()

    while (iterator.hasNext) {
      iterator.advance()
      val constraint = iterator.value()
      if (constraint.isHardConstraint) {
        constraint.inactive = false
        totalActive += 1
      }
      else constraint.inactive = true
    }

    totalActive
  }

  /**
   * Select all constraints, by setting everything to active
   *
   * @return total active constrains
   */
  def selectAllConstraints(): Int = {
    val iterator = mrf.constraints.iterator()
    while (iterator.hasNext) {
      iterator.advance()
      iterator.value().inactive = false
    }
    totalActive = mrf.numberOfConstraints

    totalActive
  }

  /**
   * Select some satisfied constraints that either are hard constraints or
   * are soft constraints and pass a certain threshold. Used by MCSAT algorithm
   * in each sampling iteration to select the M set.
   *
   * @return total active constraints
   */
  def selectSomeSatConstraints(): Int = {
    totalActive = 0
    val iterator = mrf.constraints.iterator()
    while (iterator.hasNext) {
      iterator.advance()
      val constraint = iterator.value()
      if (constraint.isSatisfied && (constraint.isHardConstraint || (ThreadLocalRandom.current().nextDouble() <= constraint.threshold))) {
        constraint.inactive = false
        totalActive += 1
      }
      else {
        constraint.inactive = true
      }
    }

    totalActive
  }

  /**
   * Evaluates the current state. Finds satisfied and unsatisfied constraints
   * and computes the total cost of the unsatisfied ones.
   * @return total cost after evaluation
   */
  def evaluateState(ignoreInactive: Boolean = false): LongDouble = {
    totalCost = ZERO
    Unsatisfied.clear()

    // Recompute delta costs:
    val iterator = mrf.constraints.iterator()

    // Current constraint
    var currentConstraint: Constraint = null

    // Keeps the count of literals satisfying the current constraint
    var nsat = 0

    // literal index of the current constraint
    var idx = 0

    while (iterator.hasNext) {
      iterator.advance()
      currentConstraint = iterator.value()

      if( !ignoreInactive || (ignoreInactive && !currentConstraint.inactive) ) {
        // --- Compute the number of literals that satisfy the current constraint
        nsat = 0 // Reset
        idx = 0 // Reset
        while (idx < currentConstraint.literals.length) {
          val lit = currentConstraint.literals(idx)
          if (isTrueLiteral(lit)) nsat += 1
          idx += 1
        }
        currentConstraint.nsat = nsat
        // --- --- --- --- --- --- --- --- --- ---

        totalCost += currentConstraint.cost
        if (currentConstraint.cost.isPositive) Unsatisfied += currentConstraint
      }
    }
    totalCost
  }

  // -----------------------------------
  // Evaluation functions
  // TODO: Can be done in parallel only with actors, due to shared atoms
  // TODO: Note that by using actors, assign SAT and UNSAT potential functions should be messages
  // -----------------------------------

  /**
   * Evaluates the current state, finds satisfied and unsatisfied constraints
   * and computes the total cost of the unsatisfied ones. Furthermore it computes
   * the initial break and make cost of each atom.
   *
   * @return the total cost of the current state
   */
  def evaluateCosts(): LongDouble = {
    totalCost = ZERO
    Unsatisfied.clear()

    // Recompute delta costs:
    val iterator = mrf.constraints.iterator()

    // Current constraint
    var currentConstraint: Constraint = null

    // Keeps the count of literals satisfying the current constraint
    var nsat = 0

    // The last literal that satisfies the current constraint, useful when
    // the current constrain is satisfied only by a single literal
    var _lit = 0
    var _lit1 = 0
    var _lit2 = 0

    // The index in constraint.literals array
    var idx = 0

    while (iterator.hasNext) {
      iterator.advance()
      currentConstraint = iterator.value()

      // --- Proceed only when the clause is selected and is not satisfied by any fixed atom.
      // In case there is satisfied by any fixed atom there is no point to assign any sat or unsat
      // potential.
      if (!currentConstraint.inactive && !currentConstraint.isSatisfiedByFixed) {

        // --- Compute the number literals that satisfy the current constraint
        nsat = 0 // Reset
        _lit = 0 // Reset
        _lit1 = 0
        _lit2 = 0
        idx = 0 // Reset
        while (idx < currentConstraint.literals.length) {
          val lit = currentConstraint.literals(idx)
          if (isTrueLiteral(lit)) {
            nsat += 1
            _lit = lit
            if (_lit1 == 0) _lit1 = _lit
            else if (_lit1 != 0 && _lit2 == 0) _lit2 = _lit
          }
          idx += 1
        }
        currentConstraint.nsat = nsat
        // --- --- --- --- --- --- --- --- --- ---

        totalCost += currentConstraint.cost
        if (currentConstraint.cost.isPositive) Unsatisfied += currentConstraint

        if (nsat == 0) {
          // 1. Since the constraint is not satisfied, we define for each corresponding atom
          //    that the cost will be reduced if we flip its state (= the constraint becomes
          //    satisfied).
          idx = 0 // reset index
          while (idx < currentConstraint.literals.length) {
            atoms.get(atomID(currentConstraint.literals(idx))).assignSatPotential(currentConstraint)
            idx += 1
          }
        }
        else if (nsat == 1) {
          // 2. The constraint is satisfied only by a single literal. In that case, the "_lit"
          //    references to the literal that satisfies that clause. When the corresponding
          //    atom is flipped, the current constraint will become unsatisfied
          atoms.get(atomID(_lit)).assignUnsatPotential(currentConstraint)
          currentConstraint.watchLit1 = atomID(_lit)
        }
        else if(nsat > 1) {
          currentConstraint.watchLit1 = atomID(_lit1)
          currentConstraint.watchLit2 = atomID(_lit2)
        }
      }
      // --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    }

    totalCost
  }

  /**
   * Flips the given atom, updates the sets of satisfied/unsatisfied constraints and
   * the break and make costs of the corresponding atoms. Also updates the total cost
   * of the new state produced by flipping the given atom and checks if the state is
   * better than the previous low state.
   *
   * @param atom atom to flip
   * @param iteration current iteration of the inference algorithm
   */
  def flip(atom: GroundAtom, iteration: Int) {

    val pickedID = atom.id

    whenDebug {
      AtomIdentityFunction.decodeAtom(pickedID)(mrf.mln) match{
        case Success(atomStr) => debug(s"Flipping atom: '$atomStr'")
        case Failure(t) => error(s"Cannot decode atom with atomID = '$pickedID'", t)
      }
    }

    debug("= = = =\nTotal cost: " + totalCost + "\nbreakCost: "+atom.breakCost + " makeCost: "+atom.makeCost)

    // --- Flip that atom.
    atom.state = !atom.state
    atom.lastFlip = iteration

    // --- The atom is flipped, therefore we have to update the evaluation data:
    //     This atom belongs to the collection of dirty atoms, that is atoms that their state
    //     have changed after the evaluation (@link this#evaluate())
    if(!dirtyAtoms.contains(atom)) dirtyAtoms.add(atom)
    else dirtyAtoms.remove(atom)

    //---  UPDATE STATS:
    // The collection of constraints that contain this atom as a positive literal
    val pos = mrf.pLit2Constraints.get(pickedID)
    // The collection of constraints that contain this atom as a negative literal (negated atom)
    val negs = mrf.nLit2Constraints.get(pickedID)

    //--- A. Process the constraints that become satisfied after flipping the given atom

    // constraints that are satisfied by the truth value of this atom:
    val constraintsSat = if (atom.state) pos else negs

    if (constraintsSat ne null) {
      val iterator = constraintsSat.iterator
      var constraint = MRF.NO_CONSTRAINT
      while (iterator.hasNext) {
        constraint = iterator.next()

        if (!constraint.inactive && !constraint.isSatisfiedByFixed) {

          // increase by one the number of literals that satisfy this clause
          constraint.nsat += 1

          // proceed only when the current clause is active:
          val nSat = constraint.nsat

          val literals = constraint.literals

          if (nSat == 1) {
            // 1. The current constraint is becomes satisfied for the given atom.
            //
            //    a. Remove this constraint from the set of unsatisfied constraints
            //       only when it is positive, otherwise do the opposite.
            if (constraint.getWeight > 0) {
              Unsatisfied -= constraint
              totalCost -= (if(mode == MRF.MODE_MWS) constraint.hpWeight.abs() else ONE)
              if(constraint.isHardConstraint && priorityBuffer.contains(constraint)) priorityBuffer -= constraint
            }
            else {
              Unsatisfied += constraint
              totalCost += (if(mode == MRF.MODE_MWS) constraint.hpWeight.abs() else ONE)
            }

            //    b. Since the given atom satisfies this constraint, the state of the rest
            //       literals does not affect this constraint. Therefore, we can safely define
            //       that any potential change to their state does not changes the total cost.
            var idx = 0

            while (idx < literals.length) {
              atoms.get(atomID(literals(idx))).revokeSatPotential(constraint)
              idx += 1
            }
            atoms.get(pickedID).assignUnsatPotential(constraint)
            //constraint.watchLit1 = pickedID
          }
          else if (nSat == 2) {
            // 2. The current constraint is additionally satisfied by some other atom.
            //    Therefore, we need to find it and simply define that if we flip it
            //    the cost will remain unchanged.
            //            atoms.get(constraint.watchLit1).revokeUnsatPotential(constraint)
            //            constraint.watchLit2 = pickedID

            var idx = 0
            var currentLiteral = -1
            var currentID = -1
            var atom = MRF.NO_ATOM

            while (idx < literals.length) {
              currentLiteral = literals(idx)
              currentID = atomID(currentLiteral)
              if (currentID != pickedID) {
                atom = atoms.get(currentID)
                if ((currentLiteral > 0) == atom.state) {
                  atom.revokeUnsatPotential(constraint)
                  idx = literals.length // the literal is found, thus no need to search further :)
                }
              }
              idx += 1
            }

          }
        }
      }
    }

    // --- B. Process the constraints that become unsatisfied after flipping the given atom
    // constraints that are unsatisfied by the truth value of this atom:
    val constraintsUnsat = if (atom.state) negs else pos

    if (constraintsUnsat ne null) {
      val iterator = constraintsUnsat.iterator
      var constraint = MRF.NO_CONSTRAINT

      while (iterator.hasNext) {
        constraint = iterator.next()

        if (!constraint.inactive && !constraint.isSatisfiedByFixed) {

          // Since the corresponding literal does not satisfies the current constraint any longer,
          // reduce by one its number of satisfied literals.
          constraint.nsat -= 1

          // proceed only if the constraint is active and nsat is either 0 or 1
          val nSat = constraint.nsat

          val literals = constraint.literals

          if (nSat == 0) {
            // 1. The current constraint is no longer satisfied by any of its literals.
            //
            //    a. Insert this constraint to the set of unsatisfied constraints
            //       only when it is positive, otherwise do the opposite
            if (constraint.getWeight > 0) {
              Unsatisfied += constraint
              totalCost += (if(mode == MRF.MODE_MWS) constraint.hpWeight.abs() else ONE)
              if(constraint.isHardConstraint) priorityBuffer += constraint
            }
            else {
              Unsatisfied -= constraint
              totalCost -= (if(mode == MRF.MODE_MWS) constraint.hpWeight.abs() else ONE)
            }

            //    b. Since the current constraint is now unsatisfied, specify that by
            //       flipping any of its literals this constraint will become satisfied.
            //       This is already done for the chosen atom (atom.flip()), thus we only
            //       have to update the rest literals.
            var idx = 0

            while (idx < literals.length) {
              atoms.get(atomID(literals(idx))).assignSatPotential(constraint)
              idx += 1
            }
            atoms.get(pickedID).revokeUnsatPotential(constraint)
          }
          else if (nSat == 1) {
            // 2. The current constraint remains satisfied from another literal. As a result,
            //    we have to find it and simply define that if it will be flipped, then the
            //    current constraint will become satisfied.
            //            if(constraint.watchLit1 == pickedID) constraint.watchLit1 = constraint.watchLit2
            //            atoms.get(constraint.watchLit1).assignUnsatPotential(constraint)
            var idx = 0
            var currentLiteral = -1
            var currentID = -1
            var atom = MRF.NO_ATOM

            while (idx < literals.length) {
              currentLiteral = literals(idx)
              currentID = atomID(currentLiteral)

              if (currentID != pickedID) {
                atom = atoms.get(currentID)
                if ((currentLiteral > 0) == atom.state) {
                  atom.assignUnsatPotential(constraint)
                  idx = literals.length // we have found the literal, no need to search further :)
                }
              }
              idx += 1
            }
          }
          /*else {
            if(constraint.watchLit1 == pickedID) {
              var idx = 0
              var currentLiteral = -1
              var currentID = -1

              while (idx < literals.length) {
                currentLiteral = literals(idx)
                currentID = atomID(currentLiteral)
                if(isTrueLiteral(currentLiteral) && currentID != constraint.watchLit1 && currentID != constraint.watchLit2) {
                constraint.watchLit1 = currentID
                idx = literals.length
                }
                idx += 1
              }

            }
            else if(constraint.watchLit2 == pickedID) {
              var idx = 0
              var currentLiteral = -1
              var currentID = -1

              while (idx < literals.length) {
                currentLiteral = literals(idx)
                currentID = atomID(currentLiteral)
                if(isTrueLiteral(currentLiteral) && currentID != constraint.watchLit1 && currentID != constraint.watchLit2) {
                  constraint.watchLit2 = currentID
                  idx = literals.length
                }
                idx += 1
              }

            }
          }*/

        }
      }
    }

    // --- If the total cost the current state is dropped bellow the lowest so far cost,
    //     store this state as low-state.
    if (totalCost < lowCost) saveLowState(totalCost)

    debug("breakCost: "+atom.breakCost + " makeCost: "+atom.makeCost + "\nTotal cost: " + totalCost + "\n= = = =")
  }

  /**
   * Save current state of atoms as the optimum state.
   *
   * @param cost cost to be saved as the lowest cost
   */
  def saveLowState(cost: LongDouble) {
    lowCost = cost
    lowUnsat = Unsatisfied.size
    if (dirtyAtoms.nonEmpty) {
      dirtyAtoms.foreach(_.saveAsLowState())
      dirtyAtoms = new mutable.HashSet[GroundAtom]()
    }
    else parAtoms.foreach(_.saveAsLowState())
  }

  /**
   * Save current state of atoms as the optimum state.
   */
  def saveAsLowState() {
    parAtoms.foreach(_.saveAsLowState())
  }

  /**
   * Restore the best state saved so far.
   */
  def restoreLowState() {
    totalCost = lowCost
    if (dirtyAtoms.nonEmpty) {
      dirtyAtoms.foreach(_.restoreLowState())
      dirtyAtoms = new mutable.HashSet[GroundAtom]()
    }
    else parAtoms.foreach(_.restoreLowState())
  }

  /**
   * Increment trues counter for each atom that is currently true
   * in the state. Used by MCSAT.
   */
  def count() {
    parAtoms.foreach(atom => if (atom.state) atom.truesCounter += 1)
  }

  /**
   * Get total cost of unsatisfied constraints in the current state.
   *
   * @return total cost
   */
  def getCost: LongDouble = totalCost

  /**
   * Get low cost of the current state.
   *
   * @return low cost
   */
  def getLowCost: LongDouble = lowCost

  /**
   * Get total active atoms in the current state.
   *
   * @return total active atoms
   */
  def getTotalAlive: Int = totalActive

  /**
   * Get the number of unsatisfied constraints in the current state.
   *
   * @return number of unsatisfied constraints
   */
  def getNumberOfUnsatisfied: Int = Unsatisfied.size

  /**
   * Get a randomly selected ground atom.
   *
   * @return randomly selected ground atom
   */
  def getRandomAtom: GroundAtom = parAtoms(ThreadLocalRandom.current().nextInt(parAtoms.length))

  /**
   * Get a randomly selected ground atom, using a custom random generator.
   *
   * @param rand custom random generator
   * @return randomly selected ground atom
   */
  def getRandomAtom(rand: util.Random): GroundAtom = parAtoms(rand.nextInt(parAtoms.length))

  /**
   * Picks a randomly unsatisfied constraint according to some heuristics. If heuristics are
   * disabled then selection is purely random. Used by SAT solvers.
   * <ul>
   * <li>Hard constraint priority: Picks a recently broken hard constraint if any exists; otherwise selects
   * randomly a hard constraint if there is any currently unsatisfied. In any other case selection is purely random.
   * </ul>
   *
   * @return random unsatisfied constraint
   */
  def getRandomUnsatConstraint: Constraint = {

    whenDebug {
      if (Unsatisfied.isEmpty)
        assert(priorityBuffer.isEmpty)
    }

    if(Unsatisfied.isEmpty) MRF.NO_CONSTRAINT
    else if (satHardPriority && priorityBuffer.nonEmpty) priorityBuffer.remove(0)
    else if (satHardPriority &&  Unsatisfied.numOfHard > 0) Unsatisfied.getRandomHardConstraint
    else Unsatisfied(ThreadLocalRandom.current().nextInt(Unsatisfied.size))
  }

  /**
   * Object that holds information about unsatisfied constraints by the current state.
   */
  private object Unsatisfied {

    import lomrf._

    private val _constraintIds = new TIntArrayList(10, NO_ENTRY_KEY)
    private var _indices = new TIntIntHashMap(DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY, NO_ENTRY_KEY)
    private var _size: Int = 0
    private var _numOfHard: Int = 0

    def apply(idx: Int): Constraint = mrf.constraints.get(_constraintIds.getQuick(idx))

    def get(idx: Int): Option[Constraint] = {
      if (idx >= _size) None
      else Some(mrf.constraints.get(_constraintIds.getQuick(idx)))
    }

    def getRandomHardConstraint: Constraint = {
      var constraint = MRF.NO_CONSTRAINT
      var idx = ThreadLocalRandom.current().nextInt(_numOfHard) + 1
      val iterator = _constraintIds.iterator()
      while(iterator.hasNext) {
        val c = mrf.constraints.get(iterator.next())
        if(c.isHardConstraint) idx -= 1
        if(idx == 0) constraint = c
      }
      constraint
    }

    def contains(constraint: Constraint): Boolean = _indices.containsKey(constraint.id)

    def clear() {
      if (_size > 0) {
        _constraintIds.resetQuick()
        _indices = new TIntIntHashMap(DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY, NO_ENTRY_KEY)
        _size = 0
        _numOfHard = 0
      }
    }

    def isEmpty: Boolean = _size == 0

    def +=(constraint: Constraint) {
      if (_indices.putIfAbsent(constraint.id, _size) == NO_ENTRY_KEY) {
        _constraintIds.add(constraint.id)
        _size += 1
        if(constraint.isHardConstraint) _numOfHard += 1
      }
    }

    def -=(constraint: Constraint) {
      val idx = _indices.remove(constraint.id)
      if (idx != NO_ENTRY_KEY) {
        _size -= 1
        if(constraint.isHardConstraint) _numOfHard -= 1
        if (idx == _size) _constraintIds.removeAt(idx)
        else {
          val lastConstraintId = _constraintIds.getQuick(_size)
          _indices.put(lastConstraintId, idx)
          _constraintIds.setQuick(idx, lastConstraintId)
          _constraintIds.removeAt(_size)
        }
      }
    }

    def size = _size

    def elements = _constraintIds

    def indices = _indices

    def numOfHard = _numOfHard
  }
}

/**
 * MRFState companion object.
 */
object MRFState {

  /**
   * Creates parallel arrays for constraints and atoms of the grounded Markov network and
   * trivially satisfy unit hard constrained clauses if requested.
   *
   * @param mrf The ground Markov network
   * @param satHardUnit Trivially satisfy hard constrained unit clauses (default is true)
   * @param satHardPriority Satisfiability priority to hard constrained clauses (default is false)
   *
   * @return new MRFState object
   */
  def apply(mrf: MRF, satHardUnit: Boolean = false, satHardPriority: Boolean = false): MRFState = {

    @inline def fixLiteral(literal: Int) {
      val atom = mrf.atoms.get(math.abs(literal))
      val state = literal > 0

      // check for contradiction
      if (atom.fixedValue == 1 && !state || atom.fixedValue == -1 && state)
        sys.error("Contradiction found for atomID " + math.abs(literal))

      if (atom.fixedValue == 0) {
        atom.fixedValue = if (state) 1 else -1
        atom.state = state
        updateSatisfiedByFixed(atom.id)
      }
    }

    @inline def updateSatisfiedByFixed(atomID: Int) {
      val state = mrf.atoms.get(atomID).state
      val iterator = if(state) mrf.pLit2Constraints.get(atomID).iterator else mrf.nLit2Constraints.get(atomID).iterator
      while(iterator.hasNext) {
        val constraint = iterator.next()
        if(!constraint.inactive && !constraint.isSatisfiedByFixed)
          constraint.isSatisfiedByFixed = true
      }
    }

    val parConstraints = new ParArray[Constraint](mrf.constraints.size())
    val parAtom = new ParArray[GroundAtom](mrf.atoms.size())

    val atomsIterator = mrf.atoms.iterator()
    var i = 0
    while (atomsIterator.hasNext) {
      atomsIterator.advance()
      parAtom(i) = atomsIterator.value()
      i += 1
    }

    val constraintsIterator = mrf.constraints.iterator()

    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()
      parConstraints(constraint.id) = constraint

      // Trivially satisfy hard-constrained unit clauses
      if (satHardUnit && constraint.isHardConstraint && constraint.isUnit) {
        fixLiteral(constraint.literals(0))
      }
    }
    new MRFState(mrf, parAtom, parConstraints, satHardPriority)
  }
}

