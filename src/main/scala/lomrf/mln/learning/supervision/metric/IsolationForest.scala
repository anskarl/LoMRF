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
case class IsolationForest[T](trees: Seq[IsolationTree[T]]) {

  // The number of trees in the forest
  lazy val numberOfTrees: Int = trees.size

  /**
    * Update the internal counts of all the trees.
    *
    * @param features a sequence of features
    */
  def updateCounts(features: Seq[T]): Unit =
    trees.foreach(_.updateCounts(features))

  /**
    * Create a rebalanced forest according to the given feature scores.
    *
    * @param featureScores a map from features to scores
    * @return an rebalanced IsolationForest
    */
  def reBalance(featureScores: Map[T, Double]): IsolationForest[T] =
    IsolationForest(trees.map(_.reBalance(featureScores)))

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
    IsolationForest(trees :+ tree)

  /**
    * Append an isolation tree sequence into the forest.
    *
    * @param trees a sequence of isolation trees
    * @return an IsolationForest instance
    */
  def ++(trees: Seq[IsolationTree[T]]): IsolationForest[T] =
    IsolationForest(this.trees ++ trees)
}

object IsolationForest {

  /**
    * @tparam T the feature type
    * @return an empty IsolationForest
    */
  def empty[T]: IsolationForest[T] = IsolationForest(Seq.empty)

  /**
    * Creates a forest of randomly generated trees.
    *
    * @param features a sequence of features
    * @param numberOfTrees the number of trees in the forest (default is 100)
    * @tparam T the feature type
    * @return an IsolationForest instance
    */
  def apply[T](features: IndexedSeq[T], numberOfTrees: Int = 100): IsolationForest[T] =
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
    IsolationForest(Seq.fill(numberOfTrees)(IsolationTree(features, height)))

  /**
    *
    * @param featureScores a map of feature score tuples
    * @tparam Τ the feature type
    * @return an IsolationForest instance
    */
  def apply[Τ](featureScores: Map[Τ, Double]): IsolationForest[Τ] =
    IsolationForest(Seq(IsolationTree(featureScores, featureScores.size)))

  /**
    *
    * @param data a sequence of data (sequence of features)
    * @param numberOfTrees the number of trees in the forest
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationForest instance
    */
  def fromData[T](data: IndexedSeq[Seq[T]], numberOfTrees: Int, height: Int): IsolationForest[T] = {
    var remaining = data
    val batchSize = data.size / numberOfTrees
    var trees = Seq.empty[IsolationTree[T]]

    while (remaining.nonEmpty) {
      val shuffled = scala.util.Random.shuffle(remaining)
      trees :+= IsolationTree.fromData(shuffled.take(batchSize), height)
      remaining = shuffled.drop(batchSize)
    }
    IsolationForest(trees)
  }

  /**
    *
    * @param f a function that computes a score for each feature in the data
    * @param data a sequence of data (sequence of features)
    * @param numberOfTrees the number of trees in the forest
    * @param height the maximum tree height
    * @tparam T the feature type
    * @return an IsolationForest instance
    */
  def fromData[T](f: Seq[Seq[T]] => Map[T, Double])
                 (data: IndexedSeq[Seq[T]], numberOfTrees: Int, height: Int): IsolationForest[T] = {

    var remaining = data
    val batchSize = data.size / numberOfTrees
    var trees = Seq.empty[IsolationTree[T]]

    while (remaining.nonEmpty) {
      val shuffled = scala.util.Random.shuffle(remaining)
      trees :+= IsolationTree.fromData(f)(shuffled.take(batchSize), height)
      remaining = shuffled.drop(batchSize)
    }
    IsolationForest(trees)
  }
}
