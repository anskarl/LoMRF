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

/**
  * IsolationForest is an ensemble of IsolationTree.
  *
  * @param trees a sequence of IsolationTree
  */
case class IsolationForest[T](trees: Seq[IsolationTree[T]]) {

  // The number of trees in the forest
  lazy val numberOfTrees: Int = trees.size

  /**
    * Update the internal counts of all the trees.
    *
    * @param features a set of features
    */
  def updateCounts(features: Seq[T]): Unit = trees.foreach(_.updateCounts(features))

  /**
    * Compute the average mass of the given feature sequences.
    *
    * @param x a sequence of features
    * @param y another sequence of features
    * @return the average mass of the given feature sequences
    */
  def mass(x: Seq[T], y: Seq[T]): Double =
    trees.foldLeft(0.0) { case (sum, t) => sum + t.mass(x, y) } / numberOfTrees

  /**
    * Compute the average relative mass of the given feature sequences.
    *
    * @param x a sequence of features
    * @param q another sequence of features
    * @return the average relative mass of the given feature sequences
    */
  def relevance(x: Seq[T], q: Seq[T]): Double =
    trees.foldLeft(0.0) { case (sum, t) => sum + t.relevance(x, q) } / numberOfTrees

  /**
    * Compute the anomaly score of the given feature sequence.
    *
    * @param x a sequence of features
    * @return the anomaly score of the given feature sequence
    */
  def anomalyScore(x: Seq[T]): Double =
    trees.foldLeft(0.0) { case (sum, t) => sum + t.anomalyScore(x) } / numberOfTrees
}

object IsolationForest {

  /**
    * @param features a sequence of features
    * @param numberOfTrees the number of trees in the forest (default is 100)
    * @return an IsolationForest instance
    */
  def apply[T](features: IndexedSeq[T], numberOfTrees: Int = 100): IsolationForest[T] =
    IsolationForest(features, numberOfTrees, features.length)

  /**
    * @param features a sequence of features
    * @param numberOfTrees the number of trees in the forest
    * @param treeHeight the tree height
    * @return an IsolationForest instance
    */
  def apply[T](features: IndexedSeq[T], numberOfTrees: Int, treeHeight: Int): IsolationForest[T] =
    IsolationForest(Seq.fill(numberOfTrees)(IsolationTree(features, treeHeight)))
}
