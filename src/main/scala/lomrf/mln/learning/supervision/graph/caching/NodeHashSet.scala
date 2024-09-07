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

import gnu.trove.map.hash.TCustomHashMap
import lomrf.mln.learning.supervision.graph.Node
import scala.collection.convert.ImplicitConversionsToScala._

/**
  * Node set used for grouping similar nodes together
  * according to a given hashing function.
  *
  * @param strategy a hashing strategy
  */
private[graph] class NodeHashSet(strategy: NodeHashStrategy = new BodyStrategy)
  extends TCustomHashMap[Node, Node](strategy) {

  def +=(n: Node): Unit = {
    if (this.contains(n)) this.get(n).similarNodeQueryAtoms ++= (n.similarNodeQueryAtoms + n.query)
    else super.put(n, n)
  }

  def collectNodes: IndexedSeq[Node] = keySet.toIndexedSeq
}
