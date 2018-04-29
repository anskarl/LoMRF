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

import java.io.PrintStream
import java.text.DecimalFormat
import com.typesafe.scalalogging.LazyLogging
import lomrf.mln.model.mrf.{ MRF, MRFState }
import lomrf.mln.model.AtomIdentityFunctionOps._
import scala.util.{ Failure, Success }

sealed trait Solver extends LazyLogging {

  // an MRF used for performing inference
  protected val mrf: MRF

  /**
    * Performs inference.
    *
    * @see [[lomrf.mln.model.mrf.MRFState]]
    *
    * @return an MRF state holding the inferred truth values
    */
  def infer: MRFState
}

trait MAPSolver extends Solver {

  /**
    * Write the results of inference into a selected output stream.
    *
    * @param out an output stream (default is console)
    * @param outputAll show 0/1 results for all query atoms (default is true)
    */
  def writeResults(out: PrintStream = System.out, outputAll: Boolean = true): Unit = {

    val queryStartID = mrf.mln.space.queryStartID
    val queryEndID = mrf.mln.space.queryEndID

    val iterator = mrf.atoms.iterator

    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key

      if (atomID >= queryStartID && atomID <= queryEndID) {
        val groundAtom = iterator.value
        val state = if (groundAtom.getState) 1 else 0

        atomID.decodeAtom(mrf.mln) match {
          case Success(txtAtom) if outputAll || state == 1 => out.println(s"$txtAtom $state")
          case Failure(f)                                  => logger.error(s"Failed to decode id: $atomID", f)
        }
      }
    }
  }

}

trait MarginalSolver extends Solver {

  // the number of samples used for calculating the probabilities
  protected val samples: Int

  /**
    * Write the results of inference into a selected output stream.
    *
    * @param result an output stream (default is console)
    */
  def writeResults(result: PrintStream = System.out): Unit = {
    val numFormat = new DecimalFormat("0.0######")

    val queryStartID = mrf.mln.space.queryStartID
    val queryEndID = mrf.mln.space.queryEndID

    val iterator = mrf.atoms.iterator
    while (iterator.hasNext) {
      iterator.advance()
      val atomID = iterator.key

      if (atomID >= queryStartID && atomID <= queryEndID) {
        val groundAtom = iterator.value
        val probability = groundAtom.getTruesCount.toDouble / samples

        atomID.decodeAtom(mrf.mln) match {
          case Success(txtAtom) => result.println(s"$txtAtom ${numFormat.format(probability)}")
          case _                => logger.error(s"failed to decode id: $atomID")
        }
      }
    }
  }

}
