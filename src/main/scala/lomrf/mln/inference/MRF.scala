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

package lomrf.mln.inference

import scala.collection.mutable
import scala.collection.parallel.mutable.ParArray
import gnu.trove.list.array.TIntArrayList
import gnu.trove.map.TIntObjectMap
import gnu.trove.map.hash.{TIntIntHashMap, TIntObjectHashMap}
import java.util
import lomrf.mln.model.MLN
import util.concurrent.ThreadLocalRandom
import scala.util.Random
import lomrf.util._
import scala.collection
import scala.Some
import scala.collection.mutable.ArrayBuffer


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

final class MRFState private(val mrf: MRF, parAtoms: ParArray[GroundAtom], parConstraints: ParArray[Constraint]) {

  private val atoms = mrf.atoms
  private var dirtyAtoms = new mutable.HashSet[GroundAtom]()
  private var priority = new ArrayBuffer[Constraint]()

  private var totalCost = Double.MaxValue
  private var lowCost = Double.MaxValue
  private var totalActive = 0
  private val random = new Random()
  random.setSeed(1)

  @inline private def atomID(lit: Int) = math.abs(lit)

  @inline private def state(lit: Int) = atoms.get(atomID(lit)).state

  @inline private def isTrueLiteral(lit: Int): Boolean = ((lit > 0) && atoms.get(lit).state) || ((lit < 0) && !atoms.get(-lit).state)

  def apply(aid: Int): Boolean = state(aid)

  def printStats() {
    println("Stats:")
    var cNeg = 0
    for(i <- 0 until Unsatisfied.size) {
      if(Unsatisfied(i).weight < 0) cNeg += 1
      else {
        val clause = Unsatisfied(i).literals.map {
          l =>
            decodeLiteral(l)(mrf.mln) match {
              case Some(litTXT) => litTXT
              case None => sys.error("Cannot decode literal: " + l)
            }
        }.reduceLeft(_ + " v " + _)
        println(Unsatisfied(i).weight + " " + clause)
      }
    }
    println("-UnSat Constraints with negative weights: "+cNeg+"/"+Unsatisfied.size)
    println("-UnSat Constraints with positive weights: "+(Unsatisfied.size-cNeg)+"/"+Unsatisfied.size)
    var ll = 0.0
    var llub = 0.0
    val iter = mrf.constraints.iterator()
    var count = 0
    while(iter.hasNext) {
      iter.advance()
      val c = iter.value()
      if(c.isSatisfied) {
        count += 1
        if(c.isHardConstraint)
          ll += 8.93129e+15
        else
          ll += c.weight
      }
      if(c.isHardConstraint)
        llub += 8.93129e+15
      else if(c.weight > 0)
        llub += c.weight
    }
    println("-Likelihood is e^"+ll)
    println("-Likelihood UB is e^"+llub)
  }

  /**
   * Randomises the state and re-evaluates the MRF
   */
  def reset(tabuLength:Int = 5, unitPropagation: Boolean = false) {
    if (unitPropagation) _unitPropagation()
    var count = 0
    //randomise and reset delta
    parAtoms.foreach {
      atom =>{
        // random state only for unfixed atoms
        if (atom.fixedValue == 0) { count += 1 ; atom.state = random.nextBoolean()}//ThreadLocalRandom.current().nextBoolean()
        // reset delta
        atom.resetDelta()
        // reset last flip
        atom.lastFlip = -(tabuLength + 1)
      }
    }
    println("I randomized "+count+" atoms!")
    lowCost = evaluate()
    saveAsLowState() // destroy each sampling iteration???
    dirtyAtoms = new mutable.HashSet[GroundAtom]()
    //priority = new ArrayBuffer[Constraint]()
  }

  // -----------------------------------
  // State functions
  // -----------------------------------

  def randomise() {
    parAtoms.foreach(atom => if (atom.fixedValue == 0) atom.state = random.nextBoolean())//ThreadLocalRandom.current().nextBoolean())
  }

  def unfixAll() {
    parAtoms.foreach(_.fixedValue = 0)
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
      }
    }

    // 1. Unfix all atoms
    parAtoms.foreach(atom => atom.fixedValue == 0)

    // 2. Process negative constraints.
    val nIterator = mrf.constraints.iterator()
    while (nIterator.hasNext) {
      nIterator.advance()
      val constraint = nIterator.value()
      if (!constraint.inactive && !constraint.isPositive && !constraint.isSatisfied) {
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
        if (!constraint.inactive && constraint.isPositive && !constraint.isSatisfied) {

          var numOfNonFixedAtoms = 0
          var nonFixedLiteral = 0

          var idx = 0
          while (idx < constraint.literals.length) {
            val lit = constraint.literals(idx)
            val atomID = math.abs(lit)
            val atom = atoms.get(atomID)

            if (atom.fixedValue == 0) {
              nonFixedLiteral = lit
              numOfNonFixedAtoms += 1
              if (numOfNonFixedAtoms > 1) idx = constraint.literals.length //break
            }
            idx += 1
          }

          if (numOfNonFixedAtoms == 1) {
            fixAtom(math.abs(nonFixedLiteral), nonFixedLiteral > 0)
            done = false
          }
        }
      }
    }

  } // */


  def flip(atom: GroundAtom, iteration:Int) {

    val pickedID = atom.id

    // --- Flip that atom.
    // --- Update cost according to the specified atom's delta.
    val test = atom.flip
//    println("[FLIP FUNCTION ] Atom delta change: " + test)
    totalCost += test // WARNING: Sometimes total cost does not change. WHY?
    atom.lastFlip = iteration


    // --- The atom is flipped, therefore we have to update the evaluation data:
    //     This atom belongs to the collection of dirty atoms, that is atoms that their state
    //     have changed after the evaluation (@link this#evaluate())
    dirtyAtoms.add(atom)


    // --- If the total cost the current state is dropped bellow the lowest so far cost,
    // store this state as low-state.
    if (totalCost <= lowCost) saveLowState(totalCost)


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

        // increase by one the number of literals that satisfy this clause
        constraint.nsat += 1

        // proceed only when the current clause is active:
        val nSat = constraint.nsat
        if (!constraint.inactive && nSat <= 2) {
          val literals = constraint.literals
          if (nSat == 1) {
            // 1. The current constraint is becomes satisfied for the given atom.
            //
            //    a. Remove this constraint from the set of unsatisfied constraints
            //       only when it is positive, otherwise do the opposite.
            if (constraint.weight > 0) {
              Unsatisfied -= constraint
              if(constraint.isHardConstraint && priority.contains(constraint)) priority -= constraint
            }
            else Unsatisfied += constraint

            //    b. Since the given atom satisfies this constraint, the state of the rest
            //       literals does not affect this constraint. Therefore, we can safely define
            //       that any potential change to their state does not changes the total cost.
            var idx = 0
            var currentID = -1
            while (idx < literals.length) {
              currentID = atomID(literals(idx))
              if (currentID != pickedID) atoms.get(currentID).revokeSatPotential(constraint)
              idx += 1
            }
          }
          else if (nSat == 2) {
            // 2. The current constraint is additionally satisfied by some other atom.
            //    Therefore, we need to find it and simply define that if we flip it
            //    the cost will remain unchanged.
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

        // Since the corresponding literal does not satisfies the current constraint any longer,
        // reduce by one its number of satisfied literals.
        constraint.nsat -= 1

        // proceed only if the constraint is active and nsat is either 0 or 1
        val nSat = constraint.nsat
        if (!constraint.inactive && nSat <= 1) {
          val literals = constraint.literals

          if (nSat == 0) {
            // 1. The current constraint is no longer satisfied by any of its literals.
            //
            //    a. Insert this constraint to the set of unsatisfied constraints
            //       only when it is positive, otherwise do the opposite
            if (constraint.weight > 0) {
              Unsatisfied += constraint
              if(constraint.isHardConstraint) priority += constraint
            }
            else Unsatisfied -= constraint

            //    b. Since the current constraint is now unsatisfied, specify that by
            //       flipping any of its literals this constraint will become satisfied.
            //       This is already done for the chosen atom (atom.flip()), thus we only
            //       have to update the rest literals.
            var idx = 0
            var currentID = -1
            while (idx < literals.length) {
              currentID = atomID(literals(idx))
              if (currentID != pickedID) atoms.get(currentID).assignSatPotential(constraint)
              idx += 1
            }

          }
          else if (nSat == 1) {
            // 2. The current constraint remains satisfied from another literal. As a result,
            //    we have to find it and simply define that if it will be flipped, then the
            //    current constraint will become satisfied.

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
        }
      }
    }
//    if (Unsatisfied.size < 20) {
//      println("[FLIP FUNCTION ] UnSAT:")
//      for (i <- 0 until Unsatisfied.size) {
//        val clause = Unsatisfied(i).literals.map {
//          l =>
//            decodeLiteral(l)(mrf.mln) match {
//              case Some(litTXT) => litTXT
//              case None => sys.error("Cannot decode literal: " + l)
//            }
//        }.reduceLeft(_ + " v " + _)
//        println("[FLIP FUNCTION ] " + Unsatisfied(i).weight +" "+ clause)
//      }
//    }
//    println("[FLIP FUNCTION ] Flipped to "+atom.state)
  }

  // -----------------------------------
  // Selection methods (Slice sampling)
  // -----------------------------------

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
      //if ( constraint.isHardConstraint || ( constraint.isSatisfied && (random.nextDouble() < constraint.threshold))){
      if (constraint.isSatisfied && (constraint.isHardConstraint || (/*ThreadLocalRandom.current().nextDouble()*/random.nextDouble() <= constraint.threshold))) {
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


  // -----------------------------------
  // Evaluation functions (todo: can be parallelised only with actors (due to shared atoms))
  // -----------------------------------
  def evaluate(): Double = {
    totalCost = 0.0
    Unsatisfied.clear()

    // Reset delta costs:
    //parAtoms.foreach(_.resetDelta()) //no need to be don here. I moved this to this.reset()


    // Recompute delta costs:
    val iterator = mrf.constraints.iterator()

    // Current constraint
    var currentConstraint: Constraint = null
    // Keeps the count of literals satisfying the current constraint
    var nsat = 0
    // The last literal that satisfies the current constraint,
    // useful when the current constrain is satisfied only by a single literal
    var _lit = 0
    // The index in constraint.literals array
    var idx = 0

    while (iterator.hasNext) {
      iterator.advance()
      currentConstraint = iterator.value() // <-- the current constraint

      // --- Compute the number literals that satisfy the current constraint
      nsat = 0 // Reset
      _lit = 0 // Reset
      idx = 0 // Reset
      while (idx < currentConstraint.literals.length) {
        val lit = currentConstraint.literals(idx)
        if (isTrueLiteral(lit)) {
          nsat += 1
          _lit = lit
        }
        idx += 1
      }
      currentConstraint.nsat = nsat
      // --- --- --- --- --- --- ---

      // --- Proceed only when the clause is selected
      if (!currentConstraint.inactive) {
        totalCost += currentConstraint.cost
        if (currentConstraint.cost > 0) Unsatisfied += currentConstraint

        if (nsat == 0) {
          // 1. Since the constraint is not satisfied, we define to each corresponding atom
          //    that the cost will be reduced if we flip its state (= the constraint becomes
          //    satisfied).
          idx = 0 // reset index
          while (idx < currentConstraint.literals.length) {
            atoms.get(atomID(currentConstraint.literals(idx))).assignSatPotential(currentConstraint) //Note: using actors this should be a message
            idx += 1
          }
        }
        else if (nsat == 1) {
          // 2. The constraint is satisfied only by a single literal. In that case, the "_lit"
          //    references to the literal that satisfies that clause. When the corresponding
          //    atom is flipped, the current constraint will become unsatisfied
          atoms.get(atomID(_lit)).assignUnsatPotential(currentConstraint) //Note: using actors this should be a message
        }
      }
      // --- --- --- --- --- --- ---
    }

    //return
    totalCost
  }

  def saveLowState(cost: Double) {
    lowCost = cost
    if (!dirtyAtoms.isEmpty) {
      dirtyAtoms.foreach(_.saveAsLowState())
      dirtyAtoms = new mutable.HashSet[GroundAtom]()
    }
    else parAtoms.foreach(_.saveAsLowState())
  }

  def restoreLowState() {
    totalCost = lowCost
    if (!dirtyAtoms.isEmpty) {
      dirtyAtoms.foreach(_.restoreLowState())
      dirtyAtoms = new mutable.HashSet[GroundAtom]()
    }
    else parAtoms.foreach(_.restoreLowState())
  }

  def saveAsLowState() {
    parAtoms.foreach(_.saveAsLowState())
  }

  def getCost: Double = totalCost

  def getLowCost: Double = lowCost

  def getTotalAlive: Int = totalActive

  def getNumberUnsatisfied: Int = Unsatisfied.size

  def getRandomAtom: GroundAtom = parAtoms(/*ThreadLocalRandom.current().nextInt(parAtoms.length)*/random.nextInt(parAtoms.length))

  def getRandomAtom(rand: util.Random): GroundAtom = parAtoms(rand.nextInt(parAtoms.length))

  def getRandomUnsatConstraint: Constraint = {
//    if (Unsatisfied.size > 0) Unsatisfied(/*ThreadLocalRandom.current().nextInt(Unsatisfied.size)*/random.nextInt(Unsatisfied.size))
//    else MRF.NO_CONSTRAINT
    var constraint = MRF.NO_CONSTRAINT
    var found = false
    if(priority.size > 0)
      constraint = priority.remove(0)
    else if(Unsatisfied.size > 0) {
      if(Unsatisfied.hard > 0)
        constraint = Unsatisfied.getRandomHard()
      else
        constraint = Unsatisfied(random.nextInt(Unsatisfied.size))
//      for(i <- 0 until Unsatisfied.size) {
//        if(Unsatisfied(i).isHardConstraint && !found) {
//          constraint = Unsatisfied(i)
//          found = true
//        }
//      }
//      if(!found) constraint = Unsatisfied(random.nextInt(Unsatisfied.size))
      //constraint = Unsatisfied(random.nextInt(Unsatisfied.size))
    }
    constraint
  }

  def count() {
    parAtoms.foreach(atom => if (atom.state) atom.truesCounter += 1)
  }

  //def getUnSatConstraints = Unsatisfied // problem!!! TODO cannot be accessed from external sources

  private object Unsatisfied {

    import lomrf._

    private val _constraintIds = new TIntArrayList(10, NO_ENTRY_KEY)
    private var _indices = new TIntIntHashMap(DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR, NO_ENTRY_KEY, NO_ENTRY_KEY)

    private var _size: Int = 0

    private var _numHard: Int = 0

    def apply(idx: Int): Constraint = mrf.constraints.get(_constraintIds.getQuick(idx))

    def get(idx: Int): Option[Constraint] = {
      if (idx >= _size) None
      else Some(mrf.constraints.get(_constraintIds.getQuick(idx)))
    }

    def getRandomHard(): Constraint = {
      var constraint = MRF.NO_CONSTRAINT
      var idx = random.nextInt(_numHard) + 1
      val iterator = _constraintIds.iterator()
      while(iterator.hasNext) {
        val id = iterator.next()
        val c = mrf.constraints.get(id)
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
        _numHard = 0
      }
    }

    def isEmpty: Boolean = _size == 0

    def +=(constraint: Constraint) {
      if (_indices.putIfAbsent(constraint.id, _size) == NO_ENTRY_KEY) {
        _constraintIds.add(constraint.id)
        _size += 1
        if(constraint.isHardConstraint) _numHard += 1
      }
    }

    def -=(constraint: Constraint) {
      val idx = _indices.remove(constraint.id)
      if (idx != NO_ENTRY_KEY) {
        _size -= 1
        if(constraint.isHardConstraint) _numHard -= 1
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

    def hard = _numHard
  }

}

object MRFState {

  def apply(mrf: MRF): MRFState = {

    @inline def fixLiteral(literal: Int) {
      val atom = mrf.atoms.get(math.abs(literal))
      val state = literal > 0
      //check for contradiction
      if (atom.fixedValue == 1 && !state || atom.fixedValue == -1 && state) {
        sys.error("Contradiction found for atomID " + math.abs(literal))
      }
      if (atom.fixedValue == 0) {
        atom.fixedValue = if (state) 1 else -1
        atom.state = state
      }
    }

    val parConstraints = new ParArray[Constraint](mrf.constraints.size())
    val parAtom = new ParArray[GroundAtom](mrf.atoms.size())

    val atomsIterator = mrf.atoms.iterator()
    var i = 0
    var count = 0
    while (atomsIterator.hasNext) {
      atomsIterator.advance()
      parAtom(i) = atomsIterator.value()
      if(parAtom(i).isFixed) count +=1
      i += 1
    }
    println("[INFO ] Number of fixed atoms: "+count)
    val constraintsIterator = mrf.constraints.iterator()
    i = 0
    while (constraintsIterator.hasNext) {
      constraintsIterator.advance()
      val constraint = constraintsIterator.value()
      parConstraints(constraint.id) = constraint

      //Trivially satisfy hard-constrained unit clauses
      if (constraint.isHardConstraint && constraint.isUnit) {
        fixLiteral(constraint.literals(0))
        //constraint.nsat = 1
      }
      i += 1
    }
    new MRFState(mrf, parAtom, parConstraints)
  }
}
