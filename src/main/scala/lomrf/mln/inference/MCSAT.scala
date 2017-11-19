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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.mln.inference

import java.io.PrintStream
import java.text.DecimalFormat
import java.util.concurrent.ThreadLocalRandom
import auxlib.log.Logging
import lomrf.mln.model.AtomIdentityFunctionOps
import lomrf.mln.model.mrf.{GroundAtom, MRFState, MRF}
import lomrf.util.time._
import lomrf.util.LongDoubleConversions._
import scala.util.Success

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
 * @param pSA The probability to perform a simulated annealing step (default is 0.5).
 * @param maxFlips The maximum number of flips taken to reach a solution (default is 1000000).
 * @param maxTries The maximum number of attempts taken to find a solution (default is 1).
 * @param targetCost  Any possible world having cost below this threshold is considered as a solution (default is 0.0001)
 * @param numSolutions Give the n-th solution (i.e. cost < target cost) in MCSAT (default is 10).
 * @param saTemperature Temperature (0,1] for the simulated annealing step in MCSAT (default is 0.8).
 * @param samples Maximum number of samples to take
 * @param lateSA When its true, simulated annealing step is performed only when MC-SAT reaches a plateau.
 * @param unitPropagation Perform unit-propagation (default is true)
 * @param satHardPriority Satisfiability priority to hard constrained clauses (default is false)
 * @param tabuLength Minimum number of flips between flipping the same atom (default is 10)
 *
 * @todo merge duplicate code with MaxWalkSAT (= maxWalkSATStep).
 * @todo perform optimisations to improve the performance.
 */
final case class MCSAT(mrf: MRF, pBest: Double = 0.5, pSA: Double = 0.1, maxFlips: Int = 100000, maxTries: Int = 1, targetCost: Double = 0.001,
                      numSolutions: Int = 10, saTemperature: Double = 0.1, samples: Int = 1000, lateSA: Boolean = true,
                      unitPropagation: Boolean = true, satHardPriority: Boolean = false, tabuLength: Int = 10) extends Logging {

  private val TARGET_COST = new LongDouble(targetCost + 0.0001)
  implicit val mln = mrf.mln

  /**
   * Fetch atom given its literal code.
   *
   * @param literal Code of the literal
   * @return The ground atom which corresponds to the given literal code
   */
  @inline private def fetchAtom(literal: Int) = mrf.atoms.get(math.abs(literal))

  /**
   * Runs marginal inference using MCSAT.
   *
   * @return The MRFState after inference procedure is complete
   */
  def infer(): MRFState = {
    val state = MRFState(mrf, satHardPriority)

    val bufferAtoms = new Array[GroundAtom](mrf.maxNumberOfLiterals)
    var bufferIdx = 0
    var currentAtom = MRF.NO_ATOM
    var currentDelta = Double.MaxValue
    var iteration = 0

    var mwsatTime = 0L

    /**
     * Initialization of MCSAT by selecting all hard constraints and try to satisfy
     * all of them by running the MaxWalkSAT algorithm.
     */
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
        info(msecTimeToText("Total Max-WalkSAT time: ", mwsatTime))
      }
    }

    /**
     * Simulated annealing step which performs greedy moves, according to a temperature change
     * over time, in order to select the next atom to flip.
     *
     * @return The ground atom that was chosen to flip
     */
    @inline def simulatedAnnealingStep() {
      currentAtom = state.getRandomAtom
      currentDelta = currentAtom.delta
      if (!currentAtom.isFixed && !currentAtom.breaksHardConstraint
        && ((currentDelta <= 0) || (ThreadLocalRandom.current().nextDouble() <= math.exp(-currentDelta / saTemperature)))) state.flip(currentAtom, iteration)
    }

    /**
     * MaxWalkSAT step which performs noisy and greedy moves, according to a probability, in order
     * to select the next atom to flip.
     *
     * @return The ground atom that was chosen to flip
     */
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
              if (!currentAtom.isFixed && (currentAtom.breakCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
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
              if (!currentAtom.isFixed && ((literals(idx) > 0) == currentAtom.state)
                && (currentAtom.breakCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
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
                && (currentAtom.breakCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
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
                && (currentAtom.breakCost == 0 || tabuLength < (iteration - currentAtom.lastFlip))) {
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
    state.evaluateState(ignoreInactive = true)
    state.printStatistics()

    state.selectAllConstraints()
    state.evaluateState()

    state.setInferenceMode(MRF.MODE_SAMPLE_SAT)

    var samplesCounter = 1

    val mcsatStartTime = System.currentTimeMillis()
    while (samplesCounter <= samples) {
      if ((samplesCounter % 100) == 0)
        info("samples " + samplesCounter + msecTimeToText(" --- time: ", System.currentTimeMillis() - mcsatStartTime))

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
      }

      //-----------------------------------------------------
      state.restoreLowState()
      state.evaluateState()

      state.count()
      //-----------------------------------------------------
      samplesCounter += 1
    }
    val mcsatTime = System.currentTimeMillis() - mcsatStartTime
    info(msecTimeToText("Total MC-SAT time: ", mcsatTime))
    info(msecTimeToText("Total inference time: ", mcsatTime + mwsatTime))

    // return the best state
    state
  }


  /**
   * Write the results of inference into the selected output stream.
   *
   * @param result Selected output stream for results (default is console)
   */
  def writeResults(result: PrintStream = System.out) {
    import AtomIdentityFunctionOps._

    val numFormat = new DecimalFormat("0.0######")

    val queryStartID = mln.space.queryStartID
    val queryEndID = mln.space.queryEndID

    val iterator = mrf.atoms.iterator()
    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key()

      if (atomID >= queryStartID && atomID <= queryEndID) {
        val groundAtom = iterator.value()
        val probability = (groundAtom.getTruesCount * 1.0) / samples

        atomID.decodeAtom match {
          case Success(txtAtom) => result.println(txtAtom + " " + numFormat.format(probability))
          case _ => error(s"failed to decode id: $atomID")
        }
      }
    }

  }

}
