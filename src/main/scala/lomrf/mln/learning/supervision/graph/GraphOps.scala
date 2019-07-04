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

package lomrf.mln.learning.supervision.graph

import breeze.linalg.{ DenseMatrix, DenseVector, mpow, pinv }
import com.typesafe.scalalogging.LazyLogging
import scala.util.{ Failure, Success, Try }
import spire.syntax.cfor._

object GraphOps extends LazyLogging {

  def LGCc(
      W: DenseMatrix[Double],
      D: DenseMatrix[Double],
      Y: DenseVector[Double]): DenseVector[Double] = {

    val numberOfNodes = W.rows
    val numberOfLabeled = Y.length
    val YY = DenseVector.vertcat(Y, DenseVector.fill(numberOfNodes - numberOfLabeled, 0.0))

    val Dp = mpow(D, -0.5)
    val S = Dp * W * Dp

    var Yt = YY; var Ytt = YY; var converged = false
    do {
      Yt = Ytt; converged = true
      Ytt = 0.5 * S * Yt + 0.5 * YY
      cfor(0)(_ < numberOfNodes, _ + 1) { i =>
        converged = converged && (math.abs(Yt(i) - Ytt(i)) < 1E-12)
      }
    } while (!converged)

    Ytt.slice(numberOfLabeled, numberOfNodes)
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
        logger.warn("Not converged or matrix is singular. Set everything to FALSE.")
        DenseVector.fill(numberOfUnlabeled)(-1.0)
    }
  }
}
