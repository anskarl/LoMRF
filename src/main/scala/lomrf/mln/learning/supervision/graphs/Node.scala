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

package lomrf.mln.learning.supervision.graphs

import lomrf.logic._
import lomrf.util.logging.Implicits._
import com.typesafe.scalalogging.LazyLogging

/**
  * Node is a collection of evidence atoms that correspond to a query atom,
  * usually connected through a set of shared constants.
  *
  * @param query the query ground atom
  * @param evidence a sequence of evidence atoms
  * @param clause a clausal representation for the node (optional)
  * @param body a disjunction of all evidence atoms present in the node (optional)
  */
case class Node(
    query: EvidenceAtom,
    evidence: IndexedSeq[EvidenceAtom],
    clause: Option[Clause],
    body: Option[Clause],
    private val head: AtomicFormula) extends LazyLogging {

  private[graphs] var similarNodeQueryAtoms =
    scala.collection.mutable.Set.empty[EvidenceAtom]

  lazy val signatures: Set[AtomSignature] =
    evidence.map(_.signature).toSet

  lazy val atoms: IndexedSeq[AtomicFormula] =
    literals.map(_.sentence).toIndexedSeq

  lazy val literals: Set[Literal] =
    body.getOrElse(logger.fatal("Body does not exist!")).literals

  /**
    * @param value a value indicating the label of the query atoms
    * @return a sequence of labeled query atoms
    */
  def labelUsingValue(value: TriState): Seq[EvidenceAtom] =
    similarNodeQueryAtoms.toIndexedSeq.map(q => EvidenceAtom(q.symbol, q.terms, value)) :+
      EvidenceAtom(query.symbol, query.terms, value)

  /**
    * @param value a value indicating the label of the query atoms
    * @return a sequence of labeled query atoms
    */
  def labelUsingValue(value: Boolean): Seq[EvidenceAtom] =
    similarNodeQueryAtoms.toIndexedSeq.map(q => EvidenceAtom(q.symbol, q.terms, value)) :+
      EvidenceAtom(query.symbol, query.terms, value)

  /**
    * @return the current node as positive.
    */
  def toPositive: Node =
    if (isPositive) this
    else Node(
      EvidenceAtom.asTrue(query.symbol, query.terms),
      evidence,
      Some(Clause(literals + Literal.asPositive(head))),
      body,
      head
    )

  /**
    * @return the current node as negative.
    */
  def toNegative: Node =
    if (isNegative) this
    else Node(
      EvidenceAtom.asFalse(query.symbol, query.terms),
      evidence,
      Some(Clause(literals + Literal.asNegative(head))),
      body,
      head
    )

  /**
    * @return true if the node query atom has a KNOWN truth value, false otherwise.
    */
  def isLabeled: Boolean = query.state != UNKNOWN

  /**
    * @return true if the node query atom has a UNKNOWN truth value, false otherwise.
    */
  def isUnlabeled: Boolean = query.state == UNKNOWN

  /**
    * @return true if the node evidence atom sequence is empty, false otherwise.
    */
  def isEmpty: Boolean = evidence.isEmpty

  /**
    * @return true if the node evidence atom sequence is not empty, false otherwise.
    */
  def nonEmpty: Boolean = evidence.nonEmpty

  /**
    * @return the label of the node query atom (TRUE, FALSE or UNKNOWN).
    */
  def label: TriState = query.state

  /**
    * @return a value label of the node query atom (1 for TRUE, -1 for FALSE or 0 for UNKNOWN).
    */
  def value: Double = label.value.toDouble

  /**
    * @return true if the node query atom is TRUE.
    */
  def isPositive: Boolean = label == TRUE

  /**
    * @return false if the node query atom is FALSE.
    */
  def isNegative: Boolean = label == FALSE

  /**
    * @return the size of the node, that is, the number of evidence atoms.
    */
  def size: Int = evidence.length

  /**
    * @return a string representation of the node.
    */
  override def toString: String =
    s"[ $query = ${query.state} ]\n${evidence.map(_.toText).mkString("\n")}"

  override def hashCode(): Int = body.get.hashCode

  override def equals(that: Any): Boolean = that match {
    case x: Node => x.body.get =~= this.body.get
    case _       => false
  }
}
