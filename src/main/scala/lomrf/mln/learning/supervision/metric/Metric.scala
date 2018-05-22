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

import lomrf.logic.AtomicFormula

/**
  * A metric is defined by a distance function over individual atoms and
  * distance over sequences of atoms by specifying a matcher function.
  */
trait Metric[A <: AtomicFormula] {

  /**
    * Distance for atoms. The function must obey to the following properties:
    *
    * {{{
    * 1. d(x, y) >= 0 for all x, y and d(x, y) = 0 if and only if x = y
    * 2. d(x, y) = d(y, x) for all x, y
    * 3. d(x, y) + d(y, z) >= d(x, z) for all x, y, z (triangle inequality)
    * }}}
    *
    * @see [[lomrf.logic.AtomicFormula]]
    * @param xAtom an atom
    * @param yAtom another atom
    * @return a distance for the given atoms
    */
  def distance(xAtom: A, yAtom: A): Double

  /**
    * Distance over sequences of atoms. The function requires a matcher over double
    * numbers in order to find an assignment between individual atoms.
    *
    * @see [[lomrf.mln.learning.supervision.metric.Matcher]]
    * @param xAtomSeq a sequence of atoms
    * @param yAtomSeq another sequence of atoms
    * @param matcher a matcher function for the assignment problem
    * @return a distance for the given sequences of atoms
    */
  final def distance(
      xAtomSeq: IndexedSeq[A],
      yAtomSeq: IndexedSeq[A],
      matcher: Matcher[Double]): Double = matcher {
    xAtomSeq map (x => yAtomSeq map (y => distance(x, y)))
  }
}
