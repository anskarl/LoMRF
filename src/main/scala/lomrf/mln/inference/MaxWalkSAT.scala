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

package lomrf.mln.inference

import lomrf.mln.model.mrf.{ GroundAtom, MRF, MRFState }
import MRF.{ NO_ATOM, NO_ATOM_ID }
import java.util.concurrent.ThreadLocalRandom
import lomrf.util.time._
import lomrf.util.LongDoubleConversions._

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
  * @param mrf The ground Markov network
  * @param pBest The probability to perform a greedy search (default is 0.5).
  * @param maxFlips The maximum number of flips taken to reach a solution (default is 1000000).
  * @param maxTries The maximum number of attempts taken to find a solution (default is 1).
  * @param targetCost Any possible world having cost below this threshold is considered as a solution (default is 0.0001)
  * @param satHardUnit Trivially satisfy hard constrained unit clauses (default is false)
  * @param satHardPriority Satisfiability priority to hard constrained clauses (default is false)
  * @param tabuLength Minimum number of flips between flipping the same atom (default is 10)
  */
final case class MaxWalkSAT(
    mrf: MRF,
    pBest: Double = 0.5,
    maxFlips: Int = 1000000,
    maxTries: Int = 1,
    targetCost: Double = 0.001,
    satHardUnit: Boolean = false,
    satHardPriority: Boolean = false,
    tabuLength: Int = 10) extends MAPSolver {

  private val TARGET_COST = new LongDouble(targetCost + 0.0001)

  /**
    * Runs MAP inference using MaxWalkSAT.
    *
    * @return The MRFState after inference procedure is complete
    */
  def infer: MRFState = {
    val startTime = System.currentTimeMillis()
    val state = infer(MRFState(mrf, satHardUnit, satHardPriority))
    val endTime = System.currentTimeMillis()
    state.evaluateState()
    state.printStatistics()
    logger.info(msecTimeToText("Total Max-WalkSAT time: ", endTime - startTime))

    // return the best state
    state
  }

  /**
    * Runs MAP inference using MaxWalkSAT.
    *
    * @param state The current MRFState
    * @return The MRFState after inference procedure is complete
    */
  private[inference] def infer(state: MRFState): MRFState = {

    // Circular access, for better performance
    val bufferAtoms = new Array[GroundAtom](mrf.maxNumberOfLiterals)
    var bufferIdx = 0
    var currentAtom = MRF.NO_ATOM
    var currentDelta = Double.MaxValue
    var numTry = 0
    var iteration = 0

      /**
        * MaxWalkSAT step which performs noisy and greedy moves, according to a probability, in order
        * to select the next atom to flip.
        *
        * @return The ground atom that was chosen to flip
        */
      @inline def maxWalkSATStep(): GroundAtom = {
        val lucky = state.getRandomUnsatConstraint

        if (lucky.id == MRF.NO_CONSTRAINT_ID) return NO_ATOM

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
              currentAtom = mrf.fetchAtom(literals(idx))
              if (!currentAtom.isFixed
                && (currentAtom.breakCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
                if (currentAtom.delta < bestDelta) {
                  bestDelta = currentAtom.delta
                  bufferAtoms(0) = currentAtom
                  bufferIdx = 1
                } else if (currentAtom.delta == bestDelta) {
                  bufferAtoms(bufferIdx) = currentAtom
                  bufferIdx += 1
                }
              }
              idx += 1
            }
          } else {
            //  b. The chosen constraint have negative weight value,
            //     thus look only at true literals.
            while (idx < literals.length) {
              currentAtom = mrf.fetchAtom(literals(idx))
              currentDelta = currentAtom.delta
              if (!currentAtom.isFixed && ((literals(idx) > 0) == currentAtom.state)
                && (currentAtom.breakCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
                if (currentDelta < bestDelta) {
                  bestDelta = currentDelta
                  bufferAtoms(0) = currentAtom
                  bufferIdx = 1
                } else if (currentDelta == bestDelta) {
                  bufferAtoms(bufferIdx) = currentAtom
                  bufferIdx += 1
                }
              }
              idx += 1
            }
          }
        } else {
          //
          // Noisy move: Choose a random atom to flip
          //
          if (lucky.isPositive) {
            // a. The chosen constraint has positive weight value.
            while (idx < literals.length) {
              currentAtom = mrf.fetchAtom(literals(idx))
              if (!currentAtom.isFixed
                && (currentAtom.breakCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
                bufferAtoms(bufferIdx) = currentAtom
                bufferIdx += 1
              }
              idx += 1
            }
          } else {
            //  b. The chosen constraint have negative weight value,
            //     thus look only at true literals.
            while (idx < literals.length) {
              currentAtom = mrf.fetchAtom(literals(idx))
              if (!currentAtom.isFixed && ((literals(idx) > 0) == currentAtom.state)
                && (currentAtom.breakCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
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
      state.reset(tabuLength)
      iteration = 0
      var chosenAtom = NO_ATOM

      while (iteration < maxFlips) {
        iteration += 1

        if (state.getCost <= TARGET_COST) {
          logger.info("A solution is found after " + (iteration * (numTry + 1)) + " iterations.")
          iteration = maxFlips //force stop
        } else {
          chosenAtom = maxWalkSATStep()
          if (chosenAtom.id != NO_ATOM_ID) {
            state.flip(chosenAtom, iteration)
          }
        }
      }
      numTry += 1
    }
    state.restoreLowState()

    //return best state
    state
  }
}
