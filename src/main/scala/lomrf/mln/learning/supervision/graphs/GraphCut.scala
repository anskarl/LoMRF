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

package lomrf.mln.learning.supervision.graphs

import breeze.linalg.{ DenseMatrix, DenseVector, mpow, pinv }
import com.typesafe.scalalogging.LazyLogging
import scala.util.{ Failure, Success, Try }

object GraphCut extends LazyLogging {

  def LGCc(
      W: DenseMatrix[Double],
      D: DenseMatrix[Double],
      Y: DenseVector[Double],
      alpha: Double): DenseVector[Double] = {

    val Dp = mpow(D, -0.5)
    val S = Dp * W * Dp
    val I = DenseMatrix.eye[Double](S.rows)
    val IS = I - alpha * S

    Try(pinv(IS) * Y) match {
      case Success(solution) => solution
      case Failure(_) =>
        logger.warn("Not converged. Set everything to FALSE.")
        Y.map(x => if (x == 0) -1 else x)
    }
  }

  def HFc(
      W: DenseMatrix[Double],
      D: DenseMatrix[Double],
      fl: DenseVector[Double]): DenseVector[Double] = {

    val numberOfNodes = W.rows
    val numberOfLabeled = fl.length
    val numberOfUnlabeled = numberOfNodes - numberOfLabeled

    // Laplace's matrix
    val L = D - W

    // Matrix partitions
    val Lul = L(numberOfLabeled until numberOfNodes, 0 until numberOfLabeled)
    val Luu = L(numberOfLabeled until numberOfNodes, numberOfLabeled until numberOfNodes)

    Try((-pinv(Luu) * Lul) * fl) match {
      case Success(solution) => solution
      case Failure(_) =>
        logger.warn("Not converged. Set everything to FALSE.")
        DenseVector.fill(numberOfUnlabeled)(-1.0)
    }
  }
}
