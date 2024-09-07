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
  * @tparam T the feature type
  */
case class IsolationForest[T](features: IndexedSeq[T], recall: Map[T, Int], trees: Seq[IsolationTree[T]]) {

  // The number of trees in the forest
  lazy val numberOfTrees: Int = trees.size

  // The number of features used to create the forest
  lazy val numberOfFeatures: Int = features.size

  /**
    * Update the internal counts of all the trees.
    *
    * @param features a sequence of features
    */
  def updateCounts(features: Seq[T]): Unit =
    trees.foreach(_.updateCounts(features))

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

  /**
    * Append an isolation tree into the forest.
    *
    * @param tree an isolation tree
    * @return an IsolationForest instance
    */
  def +(tree: IsolationTree[T]): IsolationForest[T] =
    IsolationForest(features, recall, trees :+ tree)

  /**
    * Append an isolation tree sequence into the forest.
    *
    * @param trees a sequence of isolation trees
    * @return an IsolationForest instance
    */
  def ++(trees: Seq[IsolationTree[T]]): IsolationForest[T] =
    IsolationForest(features, recall, this.trees ++ trees)
}

object IsolationForest {

  /**
    * @tparam T the feature type
    * @return an empty IsolationForest
    */
  def empty[T]: IsolationForest[T] = IsolationForest(IndexedSeq.empty[T], Map.empty[T, Int], Seq.empty)

  /**
    * Creates a forest of randomly generated trees.
    *
    * @param features a sequence of features
    * @param trees a sequence of trees
    * @tparam T the feature type
    * @return an IsolationForest instance
    */
  def apply[T](features: IndexedSeq[T], trees: Seq[IsolationTree[T]]): IsolationForest[T] =
    new IsolationForest(features, Map.empty[T, Int], trees)

  /**
    * Creates a forest of randomly generated trees.
    *
    * @param features a sequence of features
    * @param numberOfTrees the number of trees in the forest
    * @tparam T the feature type
    * @return an IsolationForest instance
    */
  def apply[T](features: IndexedSeq[T], numberOfTrees: Int): IsolationForest[T] =
    IsolationForest(features, numberOfTrees, features.length)

  /**
    * Creates a forest of randomly generated trees.
    *
    * @param features a sequence of features
    * @param numberOfTrees the number of trees in the forest
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationForest instance
    */
  def apply[T](features: IndexedSeq[T], numberOfTrees: Int, height: Int): IsolationForest[T] =
    IsolationForest(features, Seq.fill(numberOfTrees)(IsolationTree(features, height)))

  /**
    * Creates a forest of randomly generated trees.
    *
    * @param features a sequence of features
    * @param recall maximum number of occurrences for each feature
    * @param numberOfTrees the number of trees in the forest (default is 100)
    * @tparam T the feature type
    * @return an IsolationForest instance
    */
  def apply[T](features: IndexedSeq[T], recall: Map[T, Int], numberOfTrees: Int): IsolationForest[T] =
    IsolationForest(features, recall, numberOfTrees, features.map(recall.getOrElse(_, 1)).sum)

  /**
    * Creates a forest of randomly generated trees.
    *
    * @param features a sequence of features
    * @param recall maximum number of appearances for each feature
    * @param numberOfTrees the number of trees in the forest
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationForest instance
    */
  def apply[T](features: IndexedSeq[T], recall: Map[T, Int], numberOfTrees: Int, height: Int): IsolationForest[T] =
    IsolationForest(features, recall, Seq.fill(numberOfTrees)(IsolationTree(features, recall, height)))

  /**
    *  Creates a forest of randomly generated trees.
    *
    * @param features a sequence of features
    * @param priority a selection of priority features
    * @param recall maximum number of appearances for each feature
    * @param numberOfTrees the number of trees in the forest
    * @tparam T the feature type
    * @return an IsolationForest instance
    */
  def apply[T](
      features: IndexedSeq[T],
      priority: Map[T, Double],
      recall: Map[T, Int],
      numberOfTrees: Int): IsolationForest[T] =
    IsolationForest(features, recall,
                    Seq.fill(numberOfTrees)(
        IsolationTree(features, priority, recall, features.map(recall.getOrElse(_, 1)).sum)
      )
    )

  /**
    *  Creates a forest of randomly generated trees.
    *
    * @param features a sequence of features
    * @param priority a selection of priority features
    * @param recall maximum number of appearances for each feature
    * @param numberOfTrees the number of trees in the forest
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationForest instance
    */
  def apply[T](
      features: IndexedSeq[T],
      priority: Map[T, Double],
      recall: Map[T, Int],
      numberOfTrees: Int,
      height: Int): IsolationForest[T] =
    IsolationForest(features, recall,
                    Seq.fill(numberOfTrees)(
        IsolationTree(features, priority, recall, height)
      )
    )
}
