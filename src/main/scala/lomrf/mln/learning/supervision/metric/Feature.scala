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

import lomrf.logic.{ AtomSignature, AtomicFormula }
import scala.language.implicitConversions

/**
  * @param signature an atom signature
  * @param constantArgs a sequence of constants or function signatures
  */
case class Feature(signature: AtomSignature, constantArgs: Set[String]) {

  override def toString: String =
    if (constantArgs.isEmpty) signature.toString
    else s"$signature[${constantArgs.mkString(",")}]"
}

object Feature {

  implicit def fromAtomSignature(signature: AtomSignature): Feature =
    Feature(signature, Set.empty)

  implicit def fromAtomicFormula(atom: AtomicFormula): Feature =
    Feature(atom.signature, atom.functions.map(_.signature.toString) ++ atom.constants.map(_.symbol))
}
