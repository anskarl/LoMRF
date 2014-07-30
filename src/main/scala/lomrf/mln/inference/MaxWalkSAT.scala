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


import MRF.{NO_ATOM, NO_CONSTRAINT, NO_ATOM_ID}
import java.io.PrintStream
import java.util.concurrent.ThreadLocalRandom
import lomrf.util.{Utilities, Logging}


/**
 * This is an implementation of the MaxWalkSAT local-search algorithm with tabu search for weighed satisfiability solving.
 * The original implementation of the algorithm can be found in: [[http://www.cs.rochester.edu/u/kautz/walksat]].
 * Details about the (max)WalkSAT algorithm can be found in the following publications:
 *
 * <ul>
 * <li> Bart Selman, Henry Kautz, and Bram Cohen. (1993) Local Search Strategies for Satisfiability Testing.
 * Final version appears in Cliques, Coloring, and Satisfiability: Second DIMACS Implementation Challenge.
 * In David S. Johnson and Michael A. Trick, (Ed.), DIMACS Series in Discrete Mathematics and Theoretical Computer Science, vol. 26, AMS.
 * The paper can be found in [[http://www.cs.rochester.edu/u/kautz/papers/dimacs93.ps]], appendix [[http://www.cs.rochester.edu/u/kautz/papers/append-selman.etal.ps]]
 * </li>
 * <li> Henry Kautz, Bart Selman and Yueyen Jiang. A General Stochastic Approach to Solving Problems with Hard and Soft Constraints.
 * In Gu, D., Du, J., & Pardalos, P. (Eds.), The Satisfiability Problem: Theory and Applications, Vol. 35 of DIMACS Series in
 * Discrete Mathematics and Theoretical Computer Science, pp. 573–586. AMS
 * </li>
 * </ul>
 *
 *
 * @param mrf The ground Markov network
 * @param pBest The probability to perform a greedy search (default is 0.5).
 * @param maxFlips The maximum number of flips taken to reach a solution (default is 100000).
 * @param maxTries The maximum number of attempts taken to find a solution (default is 1).
 * @param targetCost Any possible world having cost below this threshold is considered as a solution (default is 0.0001)
 * @param tabuLength Minimum number of flips between flipping the same atom
 *
 * @author Anastasios Skarlatidis
 *
 * @todo merge duplicate duplicate code with MCSAT (= maxWalkSATStep).
 * @todo perform optimisations to improve the performance.
 */
final class MaxWalkSAT(mrf: MRF, pBest: Double = 0.5, maxFlips: Int = 100000, maxTries: Int = 1, targetCost: Double = 0.001, tabuLength: Int = 5) extends Logging {
  private val TARGET_COST = targetCost + 0.0001

  //private val random = new Random()

  @inline private def fetchAtom(literal: Int) = mrf.atoms.get(math.abs(literal))

  def infer(): MRFState = {
    val startTime = System.currentTimeMillis()
    val state = infer(MRFState(mrf))
    val endTime = System.currentTimeMillis()
    info(Utilities.msecTimeToText("Total Max-WalkSAT time: ", endTime - startTime))

    //return the best state
    state
  }

  private[inference] def infer(state: MRFState): MRFState = {

    val bufferAtoms = new Array[GroundAtom](mrf.maxNumberOfLiterals)
    var bufferIdx = 0
    var currentAtom = MRF.NO_ATOM
    var currentDelta = Double.MaxValue
    var numTry = 0
    var iteration = 0

    @inline def maxWalkSATStep(): GroundAtom = {
      val lucky = state.getRandomUnsatConstraint

      if (lucky.id == NO_CONSTRAINT.id) return NO_ATOM

      bufferIdx = 0
      var idx = 0

      val literals = lucky.literals

      if (ThreadLocalRandom.current().nextDouble() <= pBest) {
        //
        // Greedy move: Choose the best atom to flip
        //
        var bestDelta = Double.MaxValue
        if (lucky.isPositive) {
          // a. The chosen constraint has positive weight value.
          while (idx < literals.length) {
            currentAtom = fetchAtom(literals(idx))
            if (!currentAtom.isCritical
              && (currentAtom.brakeCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
              if (currentAtom.delta < bestDelta) {
                bestDelta = currentAtom.delta
                bufferAtoms(0) = currentAtom
                bufferIdx = 1
              }
              else if (currentAtom.delta == bestDelta) {
                bufferAtoms(bufferIdx) = currentAtom
                bufferIdx += 1
              }
            }
            idx += 1
          }
        }
        else {
          //  b. The chosen constraint have negative weight value,
          //     thus look only at true literals.
          while (idx < literals.length) {
            currentAtom = fetchAtom(literals(idx))
            currentDelta = currentAtom.delta
            if (!currentAtom.isCritical && ((literals(idx) > 0) == currentAtom.state)
              && (currentAtom.brakeCost == 0 || tabuLength < (iteration - currentAtom.lastFlip)) ) {
              if (currentDelta < bestDelta) {
                bestDelta = currentDelta
                bufferAtoms(0) = currentAtom
                bufferIdx = 1
              }
              else if (currentDelta == bestDelta) {
                bufferAtoms(bufferIdx) = currentAtom
                bufferIdx += 1
              }
            }
            idx += 1
          }
        }
      }
      else {
        //
        // Noisy move: Choose a random atom to flip
        //

        if (lucky.isPositive) {
          // a. The chosen constraint has positive weight value.
          while (idx < literals.length) {
            currentAtom = fetchAtom(literals(idx))
            if (!currentAtom.isFixed
              && (currentAtom.brakeCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
              bufferAtoms(bufferIdx) = currentAtom
              bufferIdx += 1
            }
            idx += 1
          }
        }
        else {
          //  b. The chosen constraint have negative weight value,
          //     thus look only at true literals.
          while (idx < literals.length) {
            currentAtom = fetchAtom(literals(idx))
            if (!currentAtom.isFixed && ((literals(idx) > 0) == currentAtom.state)
              && (currentAtom.brakeCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
              bufferAtoms(bufferIdx) = currentAtom
              bufferIdx += 1
            }
            idx += 1
          }
        }

      }


      // return the chosen atom
      if (bufferIdx == 1) bufferAtoms(0)
      else if (bufferIdx > 1) bufferAtoms(ThreadLocalRandom.current().nextInt(bufferIdx))
      else NO_ATOM
    }




    while (numTry < maxTries) {
      state.reset(tabuLength, unitPropagation = false)
      iteration = 0
      var chosenAtom = NO_ATOM
      while (iteration < maxFlips) {

        if (state.getCost <= TARGET_COST) {
          info("A solution is found after " + (iteration * (numTry + 1)) + " iterations.")
          iteration = maxFlips //force stop
        }
        else {
          chosenAtom = maxWalkSATStep()
          if (chosenAtom.id != NO_ATOM_ID) {
            state.flip(chosenAtom, iteration)

          }
          iteration += 1
        }
      }
      numTry += 1
    }
    state.restoreLowState()

    //return best state
    state
  }

  def writeResults(out: PrintStream = System.out) {
    import lomrf.util.decodeAtom

    implicit val mln = mrf.mln

    val iterator = mrf.atoms.iterator()
    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key()
      if (atomID >= mln.queryStartID && atomID <= mln.queryEndID) {
        val groundAtom = iterator.value()
        val state = if(groundAtom.getState) 1 else 0
        /*if (groundAtom.getState)*/ decodeAtom(iterator.key()) match {
          case Some(txtAtom) => out.println(txtAtom +" "+ state)
          case _ => error("failed to decode id:" + atomID)
        }
      }
    }
  }

}
