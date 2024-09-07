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

package lomrf.mln.learning.supervision.graph.caching

import gnu.trove.strategy.HashingStrategy
import lomrf.mln.learning.supervision.graph.Node

sealed trait NodeHashStrategy extends HashingStrategy[Node]

/**
  * A hashing strategy that compares nodes according to their clausal from.
  */
final class ClauseStrategy extends NodeHashStrategy {

  override def computeHashCode(n: Node): Int = n.clause.get.literals
    .map(l => l.sentence.symbol.## ^ l.sentence.constants.## ^ l.sentence.variables.size)
    .foldLeft(1)(_ ^ _)

  override def equals(n1: Node, n2: Node): Boolean = n1.clause.get =~= n2.clause.get
}

/**
  * A hashing strategy that compares nodes according to the body of their clausal form.
  */
final class BodyStrategy extends NodeHashStrategy {

  override def computeHashCode(n: Node): Int = n.literals
    .map(l => l.sentence.symbol.## ^ l.sentence.constants.## ^ l.sentence.variables.size)
    .foldLeft(1)(_ ^ _)

  override def equals(n1: Node, n2: Node): Boolean = n1.body.get =~= n2.body.get
}
