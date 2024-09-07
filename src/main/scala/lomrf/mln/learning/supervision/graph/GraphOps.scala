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

import breeze.linalg.{ DenseMatrix, DenseVector, diag, pinv }
import breeze.numerics.sqrt
import com.typesafe.scalalogging.LazyLogging
import scala.util.{ Failure, Success, Try }
import spire.syntax.cfor._

trait GraphSolver extends LazyLogging {

  /**
    * Solves the graph label propagation optimization.
    *
    * @param W an adjacency matrix for the graph
    * @param D the degree matrix
    * @param Y a vector holding the values of labelled nodes
    * @return the values for all nodes in [-1, 1]
    */
  def solve(W: DenseMatrix[Double], D: DenseMatrix[Double], Y: DenseVector[Double]): DenseVector[Double]
}

/**
  * Label propagation.
  *
  * @param iterations number of iterations (default is 50)
  */
case class LP(iterations: Int = 100) extends GraphSolver {

  def solve(
      W: DenseMatrix[Double],
      D: DenseMatrix[Double],
      Y: DenseVector[Double]): DenseVector[Double] = {

    val numberOfNodes = W.rows
    val numberOfLabeled = Y.length
    val YY = DenseVector.vertcat(Y, DenseVector.fill(numberOfNodes - numberOfLabeled, 0.0))

    if (iterations < 1) {
      val T = pinv(D) * W
      val Tul = T(numberOfLabeled until numberOfNodes, 0 until numberOfLabeled)
      val Tuu = T(numberOfLabeled until numberOfNodes, numberOfLabeled until numberOfNodes)
      return pinv(DenseMatrix.eye[Double](Tuu.cols) - Tuu) * Tul * Y
    }

    val Di = diag(diag(D).map(1 / _))

    var Yt = YY
    var Ytt = YY
    var converged = false
    var i = 1
    do {
      Yt = Ytt
      converged = true
      Ytt = Di * W * Yt
      cfor(0)(_ < numberOfNodes, _ + 1) { i =>
        converged = converged && (math.abs(Yt(i) - Ytt(i)) < 1E-12)
      }
      i += 1
    } while (!converged && i <= iterations)

    Ytt
  }
}

/**
  * Label spreading.
  *
  * @param iterations number of iterations (default is 100)
  * @param alpha clamping factor (default is 0.01)
  */
case class LGCc(iterations: Int = 100, alpha: Double = 0.01) extends GraphSolver {

  def solve(
      W: DenseMatrix[Double],
      D: DenseMatrix[Double],
      Y: DenseVector[Double]): DenseVector[Double] = {

    val numberOfNodes = W.rows
    val numberOfLabeled = Y.length
    val YY = DenseVector.vertcat(Y, DenseVector.fill(numberOfNodes - numberOfLabeled, 0.0))

    val Dp = diag(diag(D).map(1 / sqrt(_)))

    val S = Dp * W * Dp

    var Yt = YY
    var Ytt = YY
    var converged = false
    var i = 1
    do {
      Yt = Ytt
      converged = true
      Ytt = alpha * S * Yt + (1 - alpha) * YY
      cfor(0)(_ < numberOfNodes, _ + 1) { i =>
        converged = converged && (math.abs(Yt(i) - Ytt(i)) < 1E-12)
      }
      i += 1
    } while (!converged && i <= iterations)

    Ytt
  }
}

/**
  * Harmonic gaussian fields.
  */
class HFc extends GraphSolver {

  def solve(
      W: DenseMatrix[Double],
      D: DenseMatrix[Double],
      Y: DenseVector[Double]): DenseVector[Double] = {

    val numberOfNodes = W.rows
    val numberOfLabeled = Y.length
    val numberOfUnlabeled = numberOfNodes - numberOfLabeled

    // Laplace's matrix
    val L = D - W

    // Matrix partitions
    val Lul = L(numberOfLabeled until numberOfNodes, 0 until numberOfLabeled)
    val Luu = L(numberOfLabeled until numberOfNodes, numberOfLabeled until numberOfNodes)

    Try((-pinv(Luu) * Lul) * Y) match {
      case Success(solution) => DenseVector.vertcat(Y, solution)
      case Failure(_) =>
        logger.warn("Not converged or matrix is singular. Set everything to FALSE.")
        DenseVector.vertcat(Y, DenseVector.fill(numberOfUnlabeled)(-1.0))
    }
  }
}
