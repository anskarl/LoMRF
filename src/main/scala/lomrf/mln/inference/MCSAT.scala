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

import java.io.PrintStream
import java.text.DecimalFormat
import java.util.concurrent.ThreadLocalRandom
import lomrf.util.{Utilities, Logging}


/**
 * This is an implementation of the MC-SAT sampling algorithm for marginal inference in the presence
 * of (near) deterministic dependencies. The algorithm is presented in the following paper:
 *
 * <p>
 * Poon, Hoifung and Domingos, Pedro (2006). Sound and Efficient Inference with Probabilistic and
 * Deterministic Dependencies. In Proceedings of the 21th National Conference on Artificial Intelligence
 * (pp. 458-463), 2006. Boston, MA: AAAI Press.
 * </p>
 *
 * @param mrf The ground Markov network
 * @param pBest The probability to perform a greedy search (default is 0.5).
 * @param pSA The probability to perform a simulated annealing step (default is 0.1).
 * @param maxFlips The maximum number of flips taken to reach a solution (default is 100000).
 * @param maxTries The maximum number of attempts taken to find a solution (default is 1).
 * @param targetCost  Any possible world having cost below this threshold is considered as a solution (default is 0.0001)
 * @param numSolutions Give the n-th solution (i.e. cost < target cost) in MCSAT (default is 10).
 * @param saTemperature Temperature (0,1] for the simulated annealing step in MCSAT (default is 0.1).
 * @param samples Maximum number of samples to take
 * @param lateSA When its true, simulated annealing step is performed only when MC-SAT reaches a plateau.
 * @param unitPropagation Perform unit-propagation (default is true)
 *
 * @author Anastasios Skarlatidis
 *
 * @todo merge duplicate duplicate code with MaxWalkSAT (= maxWalkSATStep).
 * @todo perform optimisations to improve the performance.
 */
final class MCSAT(mrf: MRF, pBest: Double = 0.5, pSA: Double = 0.1, maxFlips: Int = 100000, maxTries: Int = 1, targetCost: Double = 0.001,
                      numSolutions: Int = 10, saTemperature: Double = 0.1, samples: Int = 1000, lateSA: Boolean = true, unitPropagation: Boolean = true, tabuLength: Int = 5) extends Logging {


  private val TARGET_COST = targetCost + 0.0001
  private val mrfAtoms = mrf.atoms
  //private val random = new Random()


  @inline private def fetchAtom(literal: Int) = mrfAtoms.get(math.abs(literal))

  def infer(): MRFState = {
    val state = MRFState(mrf)

    val bufferAtoms = new Array[GroundAtom](mrf.maxNumberOfLiterals)
    var bufferIdx = 0
    var currentAtom = MRF.NO_ATOM
    var currentDelta = Double.MaxValue
    var iteration = 0

    var mwsatTime = 0L

    @inline def initialise() {
      info("Initialising MC-SAT for marginal inference.")
      val noHard = state.selectOnlyHardConstraints()
      if (noHard > 0) {
        info("Running MaxWalkSAT on hard-constrained clauses" +
          "\n\tNumber of hard-constrained clauses: " + noHard)
        val sat = new MaxWalkSAT(mrf)
        val mwsatStartTime = System.currentTimeMillis()
        sat.infer(state)
        mwsatTime = System.currentTimeMillis() - mwsatStartTime
        info(Utilities.msecTimeToText("Total Max-WalkSAT time: ", mwsatTime))
      }
    }

    @inline def simulatedAnnealingStep() {
      currentAtom = state.getRandomAtom
      currentDelta = currentAtom.delta
      if (!currentAtom.isCritical
        && ((currentDelta <= 0) || (ThreadLocalRandom.current().nextDouble() <= math.exp(-currentDelta / saTemperature)))) state.flip(currentAtom, iteration)

    }

    @inline def maxWalkSATStep() {
      val lucky = state.getRandomUnsatConstraint

      if (lucky.id != MRF.NO_CONSTRAINT_ID) {
        bufferIdx = 0

        var idx = 0 // literals idx
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
              currentDelta = currentAtom.delta
              //if (!currentAtom.isFixed && !currentAtom.breaksHardConstraint) {
              if (!currentAtom.isCritical && (currentAtom.brakeCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
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
          else {
            //  b. The chosen constraint have negative weight value,
            //     thus look only at true literals.
            while (idx < literals.length) {
              currentAtom = fetchAtom(literals(idx))
              currentDelta = currentAtom.delta
              //if (!currentAtom.isFixed && !currentAtom.breaksHardConstraint && ((literals(idx) > 0) == currentAtom.state)) {
              if (!currentAtom.isCritical && ((literals(idx) > 0) == currentAtom.state) && (currentAtom.brakeCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
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

        // flip the atom
        if (bufferIdx == 1) state.flip(bufferAtoms(0), iteration)
        else if (bufferIdx > 1) state.flip(bufferAtoms(ThreadLocalRandom.current().nextInt(bufferIdx)), iteration)
      }
    }

    initialise()

    state.selectAllConstraints()
    state.evaluate()

    var samplesCounter = 1

    val mcsatStartTime = System.currentTimeMillis()
    while (samplesCounter <= samples) {
      if ((samplesCounter % 100) == 0)
        info("samples " + samplesCounter + Utilities.msecTimeToText(" --- time: ", System.currentTimeMillis() - mcsatStartTime))

      //-----------------------------------------------------
      state.selectSomeSatConstraints()
      state.reset(tabuLength, unitPropagation)
      //-----------------------------------------------------

      var numTry = 0
      var solutionsCounter = 0
      while (numTry < maxTries) {
        iteration = 0
        while (iteration < maxFlips) {
          iteration += 1

          if ((state.getCost <= TARGET_COST) || (!lateSA && (ThreadLocalRandom.current().nextDouble() < pSA))) simulatedAnnealingStep()
          else maxWalkSATStep()



          if ((solutionsCounter < numSolutions) && (state.getCost <= TARGET_COST)) {
            solutionsCounter += 1
            if (solutionsCounter == numSolutions) iteration = maxFlips
          }

        }
        numTry += 1


      } //while (numTry < maxTries)

      //-----------------------------------------------------
      state.restoreLowState()
      state.count()
      //-----------------------------------------------------
      samplesCounter += 1
    }
    val mcsatTime = System.currentTimeMillis() - mcsatStartTime
    info(Utilities.msecTimeToText("Total MC-SAT time: ", mcsatTime))
    info(Utilities.msecTimeToText("Total inference time: ", mcsatTime + mwsatTime))

    //return the best state
    state
  }


  def writeResults(outStream: PrintStream = System.out) {
    import lomrf.util.decodeAtom
    val numFormat = new DecimalFormat("0.0######")

    implicit val mln = mrf.mln
    //mln.queryEndID

    val iterator = mrfAtoms.iterator()
    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key()
      if (atomID >= mln.queryStartID && atomID <= mln.queryEndID) {
        val groundAtom = iterator.value()
        val probability = (groundAtom.getTruesCount * 1.0) / samples
        // Add Gaussian noise for P=0.0 and P=1.0. Also reformat the displayed probability result in order to have at maximum 7 floating point decimals
        //val txtProbability = if (probability == 0.0) "4.9995e-05" else if(probability == 1.0) "0.99995" else numFormat.format(probability)
        decodeAtom(iterator.key()) match {
          case Some(txtAtom) => outStream.println(txtAtom + " " + numFormat.format(probability))
          case _ => error("failed to decode id:" + atomID)
        }
      }
    }

  }

}
