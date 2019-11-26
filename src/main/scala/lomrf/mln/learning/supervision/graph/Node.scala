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

package lomrf.mln.learning.supervision.graph

import lomrf.logic._
import lomrf.util.logging.Implicits._
import com.typesafe.scalalogging.LazyLogging
import lomrf.mln.learning.supervision.metric.features.Feature

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
    private[graph] val head: AtomicFormula,
    private val orderIndex: Int = -1,
    private val partitionIndices: Vector[Int] = Vector.empty) extends Ordered[Node] with LazyLogging {

  private lazy val orderedTerm: Int =
    if (orderIndex > -1) query.terms(orderIndex).symbol.toInt
    else 0

  private[graph] var similarNodeQueryAtoms =
    scala.collection.mutable.Set.empty[EvidenceAtom]

  lazy val partitionTerms: Set[Term] =
    if (partitionIndices.isEmpty) query.terms.toSet
    else partitionIndices.map(query.terms).toSet

  lazy val signatures: Set[AtomSignature] =
    evidence.map(_.signature).toSet

  lazy val atoms: IndexedSeq[AtomicFormula] =
    literals.map(_.sentence).toIndexedSeq

  lazy val literals: Set[Literal] =
    body.getOrElse(logger.fatal("Body does not exist.")).literals

  lazy val opposite: Node = if (isPositive) toNegative else toPositive

  lazy val clusterSize: Int = similarNodeQueryAtoms.size + 1

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
  def toPositive: Node = {
    if (isPositive) this
    else Node(
      EvidenceAtom.asTrue(query.symbol, query.terms),
      evidence,
      Some(Clause(literals + Literal.asPositive(head))),
      body,
      head,
      orderIndex,
      partitionIndices
    )
  }

  /**
    * @return the current node as negative.
    */
  def toNegative: Node = {
    if (isNegative) this
    else Node(
      EvidenceAtom.asFalse(query.symbol, query.terms),
      evidence,
      Some(Clause(literals + Literal.asNegative(head))),
      body,
      head,
      orderIndex,
      partitionIndices
    )
  }

  /**
    * Generalize node by removing a set of given atom signatures.
    *
    * @param features a set of signatures
    * @return a generalized node
    */
  def generalise(features: Set[Feature]): Node = {
    Node(
      query,
      evidence.filterNot(e => features.exists(f => f.signature == e.signature && f.constantArgs.forall(c => e.constants.contains(Constant(c))))),
      clause.map(c => Clause(c.literals.filterNot(l => features.contains(l.sentence)))),
      body.map(c => Clause(c.literals.filterNot(l => features.contains(l.sentence)))),
      head,
      orderedTerm,
      partitionIndices
    )
  }

  def subNode(other: Node): Boolean = {
    if (size <= other.size) {
      var list = atoms.toList
      var otherList = other.atoms.toSet.filter(x => list.exists(_.signature == x.signature))
      for (a <- list) {
        val aa = otherList.find(_ =~= a)
        if (aa.isEmpty || otherList.isEmpty) return false
        otherList -= aa.get
      }
      true
    } else false
  }

  def relevantNode(other: Node): Boolean = {
    if (size < other.size) {
      atoms.map(Feature.atom2Feature).toSet.forall(s => other.atoms.map(Feature.atom2Feature).toSet.contains(s))
      /*var list = atoms.toList
      var otherList = other.atoms.toSet.filter(x => list.exists(_.signature == x.signature))
      for (a <- list) {
        val aa = otherList.find(_ =~= a)
        if (aa.isEmpty || otherList.isEmpty) return false
        otherList -= aa.get
      }
      true*/
    } else if (size == other.size) {
      atoms.map(Feature.atom2Feature).toSet == other.atoms.map(Feature.atom2Feature).toSet
    } else false
  }

  def createSubNodes: Set[Node] = {
    val domainVars = head.variables.groupBy(_.domain).filter { case (_, vars) => vars.size > 1 }
    val domainConst = query.terms.zip(head.terms.map(_.asInstanceOf[Variable].domain)).groupBy(_._2).mapValues(_.map(_._1)).filter(_._2.size > 1)
    domainVars.flatMap {
      case (domain, vars) =>
        val const = domainConst(domain).toSet
        vars.zip(const).map {
          case (v, c) =>
            val subEv = evidence.filter(e => e.constants.contains(c) && !(const - c).exists(cc => e.constants.contains(cc)))
            val subAtoms = atoms.filter(a => a.variables.contains(v) && !(vars - v).exists(vv => a.variables.contains(vv)))
            Node(
              query,
              subEv,
              clause.map(c => Clause(c.literals.filter(l => subAtoms.contains(l.sentence) || l.sentence == head))),
              body.map(c => Clause(c.literals.filter(l => subAtoms.contains(l.sentence)))),
              head,
              orderIndex,
              partitionIndices
            )
        }
    }.toSet
  }

  /**
    * @return always false
    */
  def isDongle: Boolean = false

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
    * @return a textual representation for the node
    */
  def toText: String = {
    if (isUnlabeled) s"? :- ${
      literals.toList
        .sortBy(l => l.arity + l.sentence.symbol)
        .map(_.negate.toText)
        .mkString(" ^ ")
    }"
    else s"${if (isNegative) "!" else ""}${head.toText} :- ${
      literals.toList
        .sortBy(l => l.arity + l.sentence.symbol)
        .map(_.negate.toText)
        .mkString(" ^ ")
    }"
  }

  /**
    * @return a string representation of the node.
    */
  override def toString: String =
    s"[ $query = ${query.state} ]\n${evidence.map(_.toText).mkString("\n")}"

  override def compare(that: Node): Int = that.orderedTerm - this.orderedTerm
}

class DongleNode(symbol: String, constants: Vector[Constant], potential: Double, isPositive: Boolean)
  extends Node(
    EvidenceAtom(symbol, constants, isPositive),
    IndexedSeq.empty,
    None,
    None,
    EvidenceAtom(symbol, constants, isPositive)) {

  override def isDongle: Boolean = true

  override def isLabeled: Boolean = true

  override def value: Double = potential
}
