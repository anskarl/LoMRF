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
    if (xSeq.exists(isSplit) && hasRight) {
      val (a, b) = xSeq.splitAt(xSeq.indexWhere(isSplit))
      right.get.parentMass(a ++ b.tail, size)
    } else if (!xSeq.exists(isSplit) && hasLeft)
      left.get.parentMass(xSeq, size)
    else parentSize
  }

  @tailrec
  private def internalMass(xSeq: Seq[T], ySeq: Seq[T]): Long = {
    if (xSeq.exists(isSplit) && ySeq.exists(isSplit) && hasRight) {
      val (xLeft, xRight) = xSeq.splitAt(xSeq.indexWhere(isSplit))
      val (yLeft, yRight) = ySeq.splitAt(ySeq.indexWhere(isSplit))
      right.get.internalMass(xLeft ++ xRight.tail, yLeft ++ yRight.tail)
    } else if (!xSeq.exists(isSplit) && !ySeq.exists(isSplit) && hasLeft)
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
    * @param counts counts for the given sequence of features (default is 1)
    */
  @tailrec
  final def updateCounts(features: Seq[T], counts: Long = 1): Unit = {
    this.size += counts // update current node and then move deeper

    if (hasRight && features.exists(isSplit)) {
      val (a, b) = features.splitAt(features.indexWhere(isSplit))
      right.get.updateCounts(a ++ b.tail, counts)
    } else if (hasLeft && !features.exists(isSplit))
      left.get.updateCounts(features, counts)
  }

  /**
    * Updates the internal counts of the nodes according to the given isolation tree.
    *
    * @param tree an isolation tree
    */
  final def updateCounts(tree: IsolationTree[T]): Unit = {
      def traverse(root: IsolationTree[T], path: Seq[T]): Long = {
        if (root.isLeaf) root.size = tree.selfMass(path)
        else root.size = traverse(root.left.get, path) + traverse(root.right.get, path :+ root.splitFeature.get)
        root.size
      }

    this.size = traverse(this, Seq.empty)
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
    IsolationTree(features, features.map(_ -> 1).toMap, height) // the recall of each feature should be 1

  /**
    * Creates an isolation tree from features. At each branch it selects
    * a random feature split.
    *
    * @param features an indexed sequence of features
    * @param recall maximum number of appearances for each feature
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  def apply[T](features: IndexedSeq[T], recall: Map[T, Int], height: Int): IsolationTree[T] =
    IsolationTree(features, Map.empty[T, Double], recall, height)

  /**
    * Creates an isolation tree from features. At each branch it selects
    * a random feature split.
    *
    * @param features an indexed sequence of features
    * @param ranking a selection of ranked features
    * @param recall maximum number of appearances for each feature
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  def apply[T](features: IndexedSeq[T], ranking: Map[T, Double], recall: Map[T, Int], height: Int): IsolationTree[T] =
    IsolationTree.fromFeatures(features, ranking, recall, 0, height)

  /**
    * Creates an isolation tree from features. At each branch it selects
    * a random feature split according to the given feature priority.
    *
    * @note One level priority is only supported, i.e., features having values
    *       greater than zero or otherwise.
    *
    * @param features an indexed sequence of features
    * @param ranking a selection of ranked features
    * @param recall maximum number of appearances for each feature
    * @param depth the current depth of the tree
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationTree
    */
  private def fromFeatures[T](
      features: IndexedSeq[T],
      ranking: Map[T, Double],
      recall: Map[T, Int],
      depth: Int, height: Int): IsolationTree[T] = {

    if (depth > height || features.length < 1) empty
    else {

      // in case no scores are available all features should have score 1
      val (primary, secondary) = features.partition { f => ranking.getOrElse(f, 1) != 0 }
      val current = if (primary.nonEmpty) primary else secondary

      val idx = Random.nextInt(current.length)
      val currentSplit = current(idx)

      val updatedRecall = recall.updated(currentSplit, recall(currentSplit) - 1)
      val updatedFeatures = features.filter(updatedRecall(_) > 0)

      val left = IsolationTree.fromFeatures(updatedFeatures, ranking, updatedRecall, depth + 1, height)
      val right = IsolationTree.fromFeatures(updatedFeatures, ranking, updatedRecall, depth + 1, height)

      new IsolationTree(Some(currentSplit), Some(left), Some(right))
    }
  }
}
