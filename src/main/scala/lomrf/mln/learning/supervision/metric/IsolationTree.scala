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

package lomrf.mln.learning.supervision.metric

import scala.util.Random

/**
  * An isolation tree is a binary tree. Each node splits on a randomly selected
  * feature. The left child of the node enumerates the number examples that do not
  * contain the feature. The right child enumerates all examples that contain the feature.
  *
  * @param splitFeature a split feature (optional)
  * @param left the left child (optional)
  * @param right the right child (optional)
  * @tparam T the type of features
  */
case class IsolationTree[T](
    splitFeature: Option[T],
    left: Option[IsolationTree[T]],
    right: Option[IsolationTree[T]]) {

  // the number of examples on this tree
  private var size: Long = 0

  private def isSplit(feature: T): Boolean =
    if (splitFeature.isEmpty) false
    else feature == splitFeature.get

  private def selfMass(xSeq: Seq[T]): Long = {
    if (xSeq.exists(isSplit) && right.isDefined)
      right.get.selfMass(xSeq)
    else if (!xSeq.exists(isSplit) && left.isDefined)
      left.get.selfMass(xSeq)
    else size
  }

  private def parentMass(xSeq: Seq[T], parentSize: Long): Long = {
    if (xSeq.exists(isSplit) && right.isDefined)
      right.get.parentMass(xSeq, size)
    else if (!xSeq.exists(isSplit) && left.isDefined)
      left.get.parentMass(xSeq, size)
    else parentSize
  }

  private def internalMass(xSeq: Seq[T], ySeq: Seq[T]): Long = {
    if (xSeq.exists(isSplit) && ySeq.exists(isSplit) && right.isDefined)
      right.get.internalMass(xSeq, ySeq)
    else if (!xSeq.exists(isSplit) && !ySeq.exists(isSplit) && left.isDefined)
      left.get.internalMass(xSeq, ySeq)
    else size
  }

  /**
    * @return a set of features appearing inside the tree
    */
  def collectFeatures: Set[T] =
    if (splitFeature.isEmpty) Set.empty
    else left.map(_.collectFeatures).getOrElse(Set.empty) ++
      right.map(_.collectFeatures).getOrElse(Set.empty) + splitFeature.get

  /**
    * Updates the internal counts of the nodes containing the given features.
    *
    * @param features a set of features
    */
  def updateCounts(features: Seq[T]): Unit = {
    size += 1 // update current node and then move deeper

    if (features.exists(isSplit) && right.isDefined) right.get.updateCounts(features)
    else if (!features.exists(isSplit) && left.isDefined) left.get.updateCounts(features)
  }

  /**
    * Computes the mass of the given feature sequences. The mass is defined
    * as the size of data in the deeper node of the tree that contains both
    * feature sequences divided by all data points (examples) in the tree.
    *
    * @param x a sequence of features
    * @param y another sequence of features
    * @return the mass of the given feature sequences
    */
  def mass(x: Seq[T], y: Seq[T]): Double =
    internalMass(x, y) / size.toDouble

  /**
    * Computes the relative mass of the given feature sequences. The relative mass
    * is defined as the size of data in the node containing x divided by the joint
    * mass of x and the query point q.
    *
    * @param x a sequence of features
    * @param q another sequence of features
    * @return the relative mass of the given feature sequences
    */
  def relevance(x: Seq[T], q: Seq[T]): Double =
    selfMass(q).toDouble / internalMass(x, q).toDouble

  /**
    * Computes the anomaly score of a given feature sequence. The anomaly score
    * is defined as the size of the parent node divided by the self mass, that is,
    * the size of the node containing x.
    *
    * @param x a sequence of features
    * @return the anomaly score of the given feature sequence
    */
  def anomalyScore(x: Seq[T]): Double =
    parentMass(x, size).toDouble / (selfMass(x) * size).toDouble

  /**
    * Print tree.
    */
  def printTree(): Unit = printTreeHelper("", isTail = true)

  private def printTreeHelper(prefix: String, isTail: Boolean): Unit = {
    println(s"$prefix${if (isTail) "└── " else "├── "}$splitFeature:$size")
    if (left.isDefined)
      left.get.printTreeHelper(s"$prefix${if (isTail) "    " else "│   "}", isTail = false)
    if (right.isDefined)
      right.get.printTreeHelper(s"$prefix${if (isTail) "    " else "│   "}", isTail = false)
  }
}

object IsolationTree {

  /**
    * @return an empty isolation tree.
    */
  def empty[T]: IsolationTree[T] = IsolationTree(None, None, None)

  /**
    * @param features a sequence of features
    * @return an IsolationTree of height equal to the number of features
    */
  def apply[T](features: IndexedSeq[T]): IsolationTree[T] =
    IsolationTree(features, features.length)

  /**
    * @param features a sequence of features
    * @param height the height of the tree
    * @return an IsolationTree
    */
  def apply[T](features: IndexedSeq[T], height: Int): IsolationTree[T] =
    IsolationTree(features, 0, height)

  /**
    * @param features a sequence of features
    * @param depth the current depth of the tree
    * @param height the height of the tree
    * @return an IsolationTree
    */
  private def apply[T](features: IndexedSeq[T], depth: Int, height: Int): IsolationTree[T] = {
    if (depth >= height || features.length < 1) empty
    else {
      val idx = Random.nextInt(features.length)
      val currentSplit = features(idx)

      val left = IsolationTree(features.filterNot(_ == currentSplit), depth + 1, height)
      val right = IsolationTree(features.filterNot(_ == currentSplit), depth + 1, height)

      new IsolationTree(Some(currentSplit), Some(left), Some(right))
    }
  }
}
