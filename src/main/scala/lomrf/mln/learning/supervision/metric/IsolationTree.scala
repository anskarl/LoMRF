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
import scala.util.Random

/**
  * An isolation tree is a binary tree. Each node splits on a randomly selected
  * feature (atom signature). The left child of the node enumerates the number examples
  * that do not contain the feature. The right child enumerates all examples that contain it.
  *
  * @param split an atom signature (optional)
  * @param left the left child (optional)
  * @param right the right child (optional)
  */
case class IsolationTree(
    split: Option[AtomSignature],
    left: Option[IsolationTree],
    right: Option[IsolationTree]) {

  // the number of examples on this tree
  private var size: Long = 0

  private def isSplit(signature: AtomSignature): Boolean =
    if (split.isEmpty) false
    else signature == split.get

  /**
    * Updates the internal counts of the nodes containing these signatures.
    *
    * @param signatures a set of atom signatures
    */
  def updateCounts(signatures: Set[AtomSignature]): Unit = {
    size += 1
    if (signatures.exists(isSplit) && right.isDefined) right.get.updateCounts(signatures)
    else if (!signatures.exists(isSplit) && left.isDefined) left.get.updateCounts(signatures)
  }

  private def internalMass(xAtomSeq: Set[AtomSignature], yAtomSeq: Set[AtomSignature]): Long = {

    /*if (split.isDefined) {
      if (xAtomSeq.exists(isSplit) && yAtomSeq.exists(isSplit)) {

        if (right.isDefined) right.get.internalMass(xAtomSeq, yAtomSeq)
        else size

      } else if (!xAtomSeq.exists(isSplit) && !yAtomSeq.exists(isSplit)) {

        if (left.isDefined) left.get.internalMass(xAtomSeq, yAtomSeq)
        else size
      } else size

    } else size*/

    if (xAtomSeq.exists(isSplit) && yAtomSeq.exists(isSplit) && right.isDefined)
      right.get.internalMass(xAtomSeq, yAtomSeq)
    else if (!xAtomSeq.exists(isSplit) && !yAtomSeq.exists(isSplit) && left.isDefined)
      left.get.internalMass(xAtomSeq, yAtomSeq)
    else size
  }

  /**
    * Calculates the mass of the given sets of signatures. The mass is
    * defined as the size of data in the deeper node of the tree that
    * contains both sets divided by all examples in the tree.
    *
    * @param x a set of signatures
    * @param y another set of signatures
    * @return the mass of the sets
    */
  def mass(x: Set[AtomSignature], y: Set[AtomSignature]): Double =
    internalMass(x, y) / size.toDouble

  /**
    * Print tree.
    */
  def printTree(): Unit = printTreeHelper("", isTail = true)

  private def printTreeHelper(prefix: String, isTail: Boolean) {
    println(s"$prefix${if (isTail) "└── " else "├── "}$split:$size")
    if (left.isDefined)
      left.get.printTreeHelper(s"$prefix${if (isTail) "    " else "│   "}", isTail = false)
    if (right.isDefined)
      right.get.printTreeHelper(s"$prefix${if (isTail) "    " else "│   "}", isTail = false)
  }
}

object IsolationTree {

  /**
    * @return an empty tree.
    */
  def empty: IsolationTree = new IsolationTree(None, None, None)

  /**
    * @param signatures a sequence of atom signatures
    * @return an IsolationTree of height equal to the number of signatures
    */
  def apply(signatures: IndexedSeq[AtomSignature]): IsolationTree =
    IsolationTree(signatures, signatures.length)

  /**
    * @param signatures a sequence of atom signatures
    * @param height the height of the tree
    * @return an IsolationTree
    */
  def apply(signatures: IndexedSeq[AtomSignature], height: Int): IsolationTree =
    IsolationTree(signatures, 0, height)

  /**
    * @param signatures a sequence of atom signatures
    * @param depth the current depth of the tree
    * @param height the height of the tree
    * @return an IsolationTree
    */
  private def apply(signatures: IndexedSeq[AtomSignature], depth: Int, height: Int): IsolationTree =
    if (depth >= height || signatures.length < 1) empty
    else {
      val idx = Random.nextInt(signatures.length)
      val currentSplit = signatures(idx)

      val left = IsolationTree(signatures.filterNot(_ == currentSplit), depth + 1, height)
      val right = IsolationTree(signatures.filterNot(_ == currentSplit), depth + 1, height)

      new IsolationTree(Some(currentSplit), Some(left), Some(right))
    }
}
