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

import scala.annotation.tailrec
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
    if (isLeaf) false else feature == splitFeature.get

  @tailrec
  private def selfMass(xSeq: Seq[T]): Long = {
    if (xSeq.exists(isSplit) && hasRight)
      right.get.selfMass(xSeq)
    else if (!xSeq.exists(isSplit) && hasLeft)
      left.get.selfMass(xSeq)
    else size
  }

  @tailrec
  private def parentMass(xSeq: Seq[T], parentSize: Long): Long = {
    if (xSeq.exists(isSplit) && hasRight)
      right.get.parentMass(xSeq, size)
    else if (!xSeq.exists(isSplit) && hasLeft)
      left.get.parentMass(xSeq, size)
    else parentSize
  }

  @tailrec
  private def internalMass(xSeq: Seq[T], ySeq: Seq[T]): Long = {
    if (xSeq.exists(isSplit) && ySeq.exists(isSplit) && hasRight)
      right.get.internalMass(xSeq, ySeq)
    else if (!xSeq.exists(isSplit) && !ySeq.exists(isSplit) && hasLeft)
      left.get.internalMass(xSeq, ySeq)
    else size
  }

  /**
    * @return true if the tree is a leaf node, false otherwise
    */
  def isLeaf: Boolean = splitFeature.isEmpty

  /**
    * @return true if the tree has a left subtree, false otherwise
    */
  def hasLeft: Boolean = left.nonEmpty

  /**
    * @return true if the tree has a right subtree, false otherwise
    */
  def hasRight: Boolean = right.nonEmpty

  /**
    * Updates the internal counts of the nodes containing the given features.
    *
    * @param features a sequence of features
    * @param counts counts for the given sequence of features
    */
  @tailrec
  final def updateCounts(features: Seq[T], counts: Long = 1): Unit = {
    size += counts // update current node and then move deeper

    if (features.exists(isSplit) && hasRight)
      right.get.updateCounts(features, counts)
    else if (!features.exists(isSplit) && hasLeft)
      left.get.updateCounts(features, counts)
  }

  /**
    * Create a rebalanced tree according to the given feature scores.
    *
    * @param featureScores a map of feature score tuples
    * @return a rebalanced IsolationTree
    */
  def reBalance(featureScores: Map[T, Double]): IsolationTree[T] = {

    val tree = IsolationTree(featureScores, featureScores.size)

      def inOrder(root: IsolationTree[T], path: Seq[T]): Unit = {
        if (!root.isLeaf && root.size > 0) {
          inOrder(root.left.get, path)
          inOrder(root.right.get, path :+ root.splitFeature.get)
        } else if (root.size > 0) tree.updateCounts(path, root.size)
      }

    inOrder(this, Seq.empty)
    tree
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

  override def toString: String = {
    val builder = StringBuilder.newBuilder

      def helper(root: IsolationTree[T], prefix: String = "", isTail: Boolean = true): Unit = {
        builder ++= s"$prefix${if (isTail) "└── " else "├── "}${root.splitFeature.getOrElse("")}[${root.size}]\n"

        if (root.hasLeft) helper(root.left.get, s"$prefix${if (isTail) "    " else "│   "}", isTail = false)
        if (root.hasRight) helper(root.right.get, s"$prefix${if (isTail) "    " else "│   "}", isTail = false)
      }

    helper(this)
    builder.result
  }
}

object IsolationTree {

  /**
    * @tparam T the feature type
    * @return an empty IsolationTree
    */
  def empty[T]: IsolationTree[T] = IsolationTree(None, None, None)

  /**
    * Creates an isolation tree from features. At each branch it selects
    * a random feature split.
    *
    * @param features an indexed sequence of features
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  def apply[T](features: IndexedSeq[T], height: Int): IsolationTree[T] =
    IsolationTree.fromFeatures(features, 0, height)

  /**
    * Creates an isolation tree from feature scores. At each branch it selects
    * the best feature split based on given scores.
    *
    * @param featureScores a map of feature score tuples
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  def apply[T](featureScores: Map[T, Double], height: Int): IsolationTree[T] =
    IsolationTree.fromFeatures(featureScores, 0, height)

  /**
    * Creates an isolation tree from data. At each branch it selects a random
    * feature split based on the data available.
    *
    * @param data a sequence of data (sequence of features)
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  def fromData[T](data: IndexedSeq[Seq[T]], height: Int): IsolationTree[T] =
    IsolationTree.fromData(data, 0, height)

  /**
    * Creates an isolation tree from data using a scoring function. At each branch it
    * computes the best feature split based on the data available.
    *
    * @param f a function that computes a score for each feature in the data
    * @param data a sequence of data (sequence of features)
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  def fromData[T](f: Seq[Seq[T]] => Map[T, Double])(data: IndexedSeq[Seq[T]], height: Int): IsolationTree[T] =
    IsolationTree.fromData(data, f, 0, height)

  /**
    * Creates an isolation tree from features. At each branch it selects
    * a random feature split.
    *
    * @param features an indexed sequence of features
    * @param depth the current depth of the tree
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  private def fromFeatures[T](features: IndexedSeq[T], depth: Int, height: Int): IsolationTree[T] = {
    if (depth > height || features.length < 1) empty
    else {
      val idx = Random.nextInt(features.length)
      val currentSplit = features(idx)

      val left = IsolationTree.fromFeatures(features.filterNot(_ == currentSplit), depth + 1, height)
      val right = IsolationTree.fromFeatures(features.filterNot(_ == currentSplit), depth + 1, height)

      new IsolationTree(Some(currentSplit), Some(left), Some(right))
    }
  }

  /**
    * Creates an isolation tree from feature scores. At each branch it selects
    * the best feature split based on given scores.
    *
    * @param featureScores a map of feature score tuples
    * @param depth the current depth of the tree
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  private def fromFeatures[T](featureScores: Map[T, Double], depth: Int, height: Int): IsolationTree[T] = {

      def helper(features: Seq[T], depth: Int, height: Int): IsolationTree[T] = {
        if (depth > height || features.length < 1) empty
        else {
          val currentSplit = features.head

          val left = helper(features.tail, depth + 1, height)
          val right = helper(features.tail, depth + 1, height)

          new IsolationTree(Some(currentSplit), Some(left), Some(right))
        }
      }

    helper(featureScores.toList.sortBy { case (_, score) => score }.map { case (f, _) => f }, depth, height)
  }

  /**
    * Creates an isolation tree from data. At each branch it selects a random
    * feature split based on the data available.
    *
    * @param data a sequence of data (sequence of features)
    * @param depth the current depth of the tree
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  private def fromData[T](data: IndexedSeq[Seq[T]], depth: Int, height: Int): IsolationTree[T] = {
    if (depth > height || data.length < 1) empty
    else {
      val features = data.flatten.distinct

      val idx = Random.nextInt(features.length)
      val currentSplit = features(idx)

      val (leftData, rightData) = data.partition(!_.exists(_ == currentSplit))

      val left = IsolationTree.fromData(leftData, depth + 1, height)
      left.size = leftData.size

      val right = IsolationTree.fromData(rightData.map(_.filterNot(_ == currentSplit)), depth + 1, height)
      right.size = rightData.size

      new IsolationTree(Some(currentSplit), Some(left), Some(right))
    }
  }

  /**
    * Creates an isolation tree from data using a scoring function. At each branch it
    * computes the best feature split based on the data available.
    *
    * @param data a sequence of data (sequence of features)
    * @param f a function that computes a score for each feature in the data
    * @param depth the current depth of the tree
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  private def fromData[T](
      data: Seq[Seq[T]],
      f: Seq[Seq[T]] => Map[T, Double],
      depth: Int,
      height: Int): IsolationTree[T] = {

    if (depth > height || data.length < 1) empty
    else {
      val featureScores = f(data)
      val currentSplit = featureScores.minBy { case (_, score) => score }._1

      val (leftData, rightData) = data.partition(!_.exists(_ == currentSplit))

      val left = IsolationTree.fromData(leftData, f, depth + 1, height)
      left.size = leftData.size

      val right = IsolationTree.fromData(rightData.map(_.filterNot(_ == currentSplit)), f, depth + 1, height)
      right.size = rightData.size

      new IsolationTree(Some(currentSplit), Some(left), Some(right))
    }
  }
}
