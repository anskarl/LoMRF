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

import java.util
import java.util.concurrent.ThreadLocalRandom
import gnu.trove.list.array.TIntArrayList
import gnu.trove.map.TIntObjectMap
import gnu.trove.map.hash.{TIntIntHashMap, TIntObjectHashMap}
import lomrf.mln.model.MLN
import lomrf.util._
import scala.collection
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.collection.parallel.mutable.ParArray


/**
 * This class represents a ground Markov Random Field.
 *
 * @param mln the source Markov Logic.
 * @param constraints the indexed collection of ground clauses.
 * @param atoms a indexed collection of ground atoms.
 * @param pLit2Constraints a map that keeps track which clauses contain the same positive literal.
 * @param nLit2Constraints a map that keeps track which clauses contain the same negative literal.
 * @param queryAtomStartID the id (integer) of the first query atom.
 * @param queryAtomEndID  the id (integer) of the last query atom.
 * @param weightHard the estimated weight for all hard-constrained ground clauses.
 * @param maxNumberOfLiterals that is the length of the bigger ground clause in this MRF.
 *
 *
 * @author Anastasios Skarlatidis
 */
class MRF(val mln: MLN,
          val constraints: TIntObjectMap[Constraint],
          val atoms: TIntObjectMap[GroundAtom],
          val pLit2Constraints: TIntObjectMap[collection.Iterable[Constraint]],
          val nLit2Constraints: TIntObjectMap[collection.Iterable[Constraint]],
          val queryAtomStartID: Int,
          val queryAtomEndID: Int,
          val weightHard: Double,
          val maxNumberOfLiterals: Int) {

  val numberOfConstraints = constraints.size()
  val numberOfAtoms = atoms.size()

  def apply(cid: Int) = constraints.get(cid)
}

object MRF {

  val NO_ATOM_ID = 0
  val NO_CONSTRAINT_ID = -1
  val NO_ATOM = new GroundAtom(0, 0)
  val NO_CONSTRAINT: Constraint = new Constraint(Double.NaN, Array(0), true, 0)

  val MODE_MWS = 0
  val MODE_SAMPLE_SAT = 1

  def apply(mln: MLN, constraints: TIntObjectMap[Constraint], atoms: TIntObjectMap[GroundAtom], weightHard: Double, queryAtomStartID: Int, queryAtomEndID: Int): MRF = {

    //create positive-and-negative literal to constraint occurrence maps
    val iterator = constraints.iterator()
    val pLit2Constraints = new TIntObjectHashMap[List[Constraint]]()
    val nLit2Constraints = new TIntObjectHashMap[List[Constraint]]()

    var maxNumberOfLiterals = 0

    while (iterator.hasNext) {
      iterator.advance()
      val constraint = iterator.value()

      if (constraint.literals.length > maxNumberOfLiterals) maxNumberOfLiterals = constraint.literals.length

      for (literal <- constraint.literals) {
        val atomID = math.abs(literal)
        if (literal > 0) {
          val constraints = pLit2Constraints.get(atomID)
          if (constraints eq null)
            pLit2Constraints.put(atomID, List(constraint))
          else
            pLit2Constraints.put(atomID, constraint :: constraints)
        }
        else {
          val constraints = nLit2Constraints.get(atomID)
          if (constraints eq null)
            nLit2Constraints.put(atomID, List(constraint))
          else
            nLit2Constraints.put(atomID, constraint :: constraints)
        }
      }
    }

    new MRF(mln, constraints, atoms,
      pLit2Constraints.asInstanceOf[TIntObjectMap[collection.Iterable[Constraint]]],
      nLit2Constraints.asInstanceOf[TIntObjectMap[collection.Iterable[Constraint]]],
      queryAtomStartID, queryAtomEndID, weightHard, maxNumberOfLiterals)
  }
}

final class MRFState private(val mrf: MRF, parAtoms: ParArray[GroundAtom], parConstraints: ParArray[Constraint],
                             satHardPriority: Boolean = false) extends Logging {

  private val atoms = mrf.atoms
  private var dirtyAtoms = new mutable.HashSet[GroundAtom]()
  private var priorityBuffer = new ArrayBuffer[Constraint]()

  private var totalCost = BigDecimal.double2bigDecimal(Double.MaxValue)
  private var lowCost = BigDecimal.double2bigDecimal(Double.MaxValue)
  private var lowUnsat = Int.MaxValue
  private var totalActive = mrf.numberOfConstraints

  private var mode = MRF.MODE_MWS
  //private val random = new Random()

  @inline private def atomID(lit: Int) = math.abs(lit)

  @inline private def state(lit: Int) = atoms.get(atomID(lit)).state

  @inline private def isTrueLiteral(lit: Int): Boolean = (lit > 0) == state(lit)

  def apply(aid: Int): Boolean = state(aid)

  /**
   * Change inference mode (MaxWalkSAT or SampleSAT).
   * @param mode inference mode
   */
  def setInferenceMode(mode: Int) {
    parConstraints.foreach(_.mode = mode)
    this.mode = mode
  }

  /**
   * TODO: Documentation
   * @param atomID
   * @return
   */
  def computeDelta(atomID: Int): Double = {
    var delta = 0.0
    for(i <- 0 until Unsatisfied.size) {
      val constraint = Unsatisfied.get(i).get
      if(constraint.literals.contains(atomID))
        delta += constraint.weight
      else if(constraint.literals.contains(-atomID))
        delta -= constraint.weight
    }
    delta
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
  def printMRFStateStats() {
    info("Stats:")
    val unsatisfied = Unsatisfied.size
    info("Unsatisfied clauses: " + unsatisfied + "/" + totalActive)
    var countNeg = 0
    for(i <- 0 until unsatisfied) {
      if(Unsatisfied(i).weight < 0) countNeg += 1
    }
    info("Unsatisfied negative constraints: " + countNeg + "/" + unsatisfied)
    info("Unsatisfied positive constraints: " + (unsatisfied-Unsatisfied.numOfHard-countNeg) + "/" + unsatisfied)
    info("Unsatisfied hard constraints: " + Unsatisfied.numOfHard + "/" + unsatisfied)
    info("Total cost: " + totalCost)

    var likelihood, likelihoodUB = 0.0

    val iterator = mrf.constraints.iterator()
    while(iterator.hasNext) {
      iterator.advance()
      val c = iterator.value()
      if(c.isSatisfied) likelihood += c.weight
      if(c.weight > 0 || c.isHardConstraint) likelihoodUB += c.weight
    }
    info("Likelihood is e^" + likelihood)
    info("Likelihood upper bound is e^" + likelihoodUB)
  }

  /**
   * Randomises the state and re-evaluates the MRF.
   *
   * @param tabuLength the tabu length used for tabu search heuristic
   * @param unitPropagation enables/disables unit propagation
   */
  def reset(tabuLength:Int = 10, unitPropagation: Boolean = false) {
    if (unitPropagation) _unitPropagation()

    parAtoms.foreach {
      atom =>{
        // randomize state only for unfixed atoms
        if (atom.fixedValue == 0) atom.state = ThreadLocalRandom.current().nextBoolean()
        // reset break and make costs
        atom.resetDelta()
        // reset last flip
        atom.lastFlip = -(tabuLength + 1)
      }
    }
    lowCost = evaluateCosts()
    lowUnsat = Unsatisfied.size
    saveAsLowState()
    dirtyAtoms = new mutable.HashSet[GroundAtom]()
    priorityBuffer = new ArrayBuffer[Constraint]()

    debug("[Reset] Total cost: " + lowCost)
    debug("[Reset] Unsatisfied: " + lowUnsat)
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


  private def _unitPropagation() {

    @inline def fixAtom(atomID: Int, state: Boolean) {
      val atom = atoms.get(atomID)
      //check for contradiction
      if (atom.fixedValue == 1 && !state || atom.fixedValue == -1 && state) {
        sys.error("Contradiction found for atomID " + atomID)
      }
      if (atom.fixedValue == 0) {
        atom.fixedValue = if (state) 1 else -1
        atom.state = state
        updateSatisfiedByFix(atomID)
      }
    }

    @inline def updateSatisfiedByFix(atomID: Int) {
      val state = mrf.atoms.get(atomID).state
      val iterator = if(state) mrf.pLit2Constraints.get(atomID).iterator else mrf.nLit2Constraints.get(atomID).iterator
      while(iterator.hasNext) {
        val constraint = iterator.next()
        if(!constraint.inactive && !constraint.isSatisfiedByFixed)
          constraint.isSatisfiedByFixed = true
      }
    }

    // 1. Unfix all atoms and reset constraints
    unfixAll()

    // 2. Process negative constraints.
    val nIterator = mrf.constraints.iterator()
    while (nIterator.hasNext) {
      nIterator.advance()
      val constraint = nIterator.value()
      if (!constraint.inactive && !constraint.isPositive && !constraint.isSatisfiedByFixed) {
        for (i <- 0 until constraint.literals.length)
          fixAtom(math.abs(constraint.literals(i)), constraint.literals(i) < 0)
      }
    }

    // 3. Process positive constraints.
    var done = false
    while (!done) {
      done = true
      val pIterator = mrf.constraints.iterator()
      while (pIterator.hasNext) {
        pIterator.advance()
        val constraint = pIterator.value()
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
            done = false
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

    //return
    totalActive
  }

  /**
   * Select all constraints, by setting everything to active
   * @return total active constrains
   */
  def selectAllConstraints(): Int = {
    val iterator = mrf.constraints.iterator()
    while (iterator.hasNext) {
      iterator.advance()
      iterator.value().inactive = false
    }
    totalActive = mrf.numberOfConstraints

    //return
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

    /*totalActive = parConstraints.aggregate(0)((x, constraint) => x + (
      if ( constraint.isHardConstraint || ( constraint.isSatisfied && (random.nextDouble() < constraint.threshold))) {
        constraint.inactive = false
        1
      }
      else {
        constraint.inactive = true
        0
      }
      ), _ + _)*/

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

    //return
    totalActive
  }

  /**
   * Evaluates the current state. Finds satisfied and unsatisfied constraints
   * and computes the total cost of the unsatisfied ones.
   * @return total cost after evaluation
   */
  def evaluateState(ignoreInactive: Boolean = false): BigDecimal = {
    totalCost = 0.0
    Unsatisfied.clear()

    // Recompute delta costs:
    val iterator = mrf.constraints.iterator()

    // Current constraint
    var currentConstraint: Constraint = null

    // Keeps the count of literals satisfying the current constraint
    var nsat = 0

    var idx = 0
    while (iterator.hasNext) {
      iterator.advance()
      currentConstraint = iterator.value()

      if( !ignoreInactive || (ignoreInactive && !currentConstraint.inactive) ) {
        // --- Compute the number literals that satisfy the current constraint
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
        if (currentConstraint.cost > 0) Unsatisfied += currentConstraint
      }
    }

    // return total cost
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
  def evaluateCosts(): BigDecimal = {
    totalCost = 0.0
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
        if (currentConstraint.cost > 0) Unsatisfied += currentConstraint

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

    // return total cost
    totalCost
  }

  /**
   * Flips the given atom, updates the sets of satisfied/unsatisfied constraints and
   * the break and make costs of the corresponding atoms. Also updates the total cost
   * of the new state produced by flipping the given atom and checks if the state is
   * better than the previous low state.
   * @param atom atom to flip
   * @param iteration current iteration of the inference algorithm
   */
  def flip(atom: GroundAtom, iteration: Int) {

    val pickedID = atom.id

    debug(decodeAtom(pickedID)(mrf.mln).get)
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
            if (constraint.weight > 0) {
              Unsatisfied -= constraint
              totalCost -= (if(mode == MRF.MODE_MWS) math.abs(constraint.weight) else 1)
              if(constraint.isHardConstraint && priorityBuffer.contains(constraint)) priorityBuffer -= constraint
            }
            else {
              Unsatisfied += constraint
              totalCost += (if(mode == MRF.MODE_MWS) math.abs(constraint.weight) else 1)
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
            if (constraint.weight > 0) {
              Unsatisfied += constraint
              totalCost += (if(mode == MRF.MODE_MWS) math.abs(constraint.weight) else 1)
              if(constraint.isHardConstraint) priorityBuffer += constraint
            }
            else {
              Unsatisfied -= constraint
              totalCost -= (if(mode == MRF.MODE_MWS) math.abs(constraint.weight) else 1)
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
                  idx = literals.length //we have found the literal, no need to search further :)
                }
              }
              idx += 1
            }

          }
         /* else {
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
   * @param cost cost to be saved as the lowest cost
   */
  def saveLowState(cost: BigDecimal) {
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
   * @return total cost
   */
  def getCost: BigDecimal = totalCost

  /**
   * Get low cost of the current state.
   * @return low cost
   */
  def getLowCost: BigDecimal = lowCost

  /**
   * Get total active atoms in the current state.
   * @return total active atoms
   */
  def getTotalAlive: Int = totalActive

  /**
   * Get the number of unsatisfied constraints in the current state.
   * @return number of unsatisfied constraints
   */
  def getNumberOfUnsatisfied: Int = Unsatisfied.size

  /**
   * Get a randomly selected ground atom.
   * @return randomly selected ground atom
   */
  def getRandomAtom: GroundAtom = parAtoms(ThreadLocalRandom.current().nextInt(parAtoms.length))

  /**
   * Get a randomly selected ground atom, using a custom random generator.
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
    var constraint = MRF.NO_CONSTRAINT

    if(!satHardPriority && Unsatisfied.size > 0)
      constraint = Unsatisfied(ThreadLocalRandom.current().nextInt(Unsatisfied.size))
    else if(satHardPriority) {
      if(priorityBuffer.size > 0) constraint = priorityBuffer.remove(0)
      else if(Unsatisfied.size > 0) {
        if (Unsatisfied.numOfHard > 0)
          constraint = Unsatisfied.getRandomHardConstraint
        else
          constraint = Unsatisfied(ThreadLocalRandom.current().nextInt(Unsatisfied.size))
      }
    }

    constraint
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

object MRFState {

  def apply(mrf: MRF, satHardUnit: Boolean = false, satHardPriority: Boolean = false): MRFState = {

    @inline def fixLiteral(literal: Int) {
      val atom = mrf.atoms.get(math.abs(literal))
      val state = literal > 0
      // check for contradiction
      if (atom.fixedValue == 1 && !state || atom.fixedValue == -1 && state) {
        sys.error("Contradiction found for atomID " + math.abs(literal))
      }
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
