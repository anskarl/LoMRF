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

import lomrf.logic.AtomSignature

/**
  * IsolationForest is an ensemble of IsolationTree.
  *
  * @param trees a sequence of IsolationTree
  */
class IsolationForest(trees: Seq[IsolationTree]) {

  /**
    * Update the internal counts of all the trees.
    *
    * @param signatures a set of signatures
    */
  def updateCounts(signatures: Set[AtomSignature]): Unit = trees.foreach(_.updateCounts(signatures))

  /**
    * Compute the average mass of the given sets of signatures.
    *
    * @param x a set of signatures
    * @param y another set of signatures
    * @return the average mass of the sets
    */
  def mass(x: Set[AtomSignature], y: Set[AtomSignature]): Double =
    trees.foldLeft(0.0) { case (sum, t) => sum + t.mass(x, y) } / trees.size
}

object IsolationForest {

  /**
    * @param signatures a sequence of atom signatures
    * @param numberOfTrees the number of trees in the forest
    * @return an IsolationForest
    */
  def apply(signatures: IndexedSeq[AtomSignature], numberOfTrees: Int = 100): IsolationForest =
    IsolationForest(signatures, numberOfTrees, signatures.length)

  /**
    * @param signatures a sequence of atom signatures
    * @param numberOfTrees the number of trees in the forest
    * @param treeHeight the tree height
    * @return an IsolationForest
    */
  def apply(signatures: IndexedSeq[AtomSignature], numberOfTrees: Int, treeHeight: Int): IsolationForest =
    new IsolationForest(for (_ <- 1 to numberOfTrees) yield IsolationTree(signatures, treeHeight))
}
