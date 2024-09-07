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

package lomrf.mln.learning.supervision

import lomrf.logic._
import lomrf.logic.compile.LogicFormatter
import lomrf.mln.model.{ MLN, ModeDeclarations }
import scala.util.{ Failure, Success, Try }
import lomrf.{ AUX_PRED_PREFIX => PREFIX }
import breeze.linalg.DenseMatrix
import lomrf.mln.learning.supervision.graph.caching.NodeCache
import lomrf.mln.learning.supervision.metric.Feature

package object graph {

  type GraphMatrix = DenseMatrix[Double]
  type EncodedGraph = (GraphMatrix, GraphMatrix)

  /*
   * Use a very close to zero (non zero) value in order to avoid singular
   * or degenerate matrices, i.e. having zero determinant.
   */
  val UNCONNECTED = 0.0

  implicit class DenseMatrixOps(M: DenseMatrix[Double]) {
    def mkString(sep: String = " ", precision: Int = 2): String = (0 until M.rows).map { i =>
      M(i, ::).t.map(s"%1.${precision}f" format _).toArray.mkString(sep)
    }.mkString("\n")
  }

  /**
    * Generalize nodes by removing atoms that correspond to the given features.
    *
    * @param nodes an indexed sequence of nodes
    * @param features a set of features
    * @param cache node cache holding labelled nodes (optional)
    * @return a indexed sequence of generalized nodes
    */
  private[graph] def generalise(
      nodes: IndexedSeq[Node],
      features: Set[Feature],
      cache: Option[NodeCache] = None): IndexedSeq[Node] = {

    nodes.foldLeft(IndexedSeq.empty[(Node, Long)]) {
      case (reducedNodes, node) =>
        val freq = cache.map(_.getOrElse(node, 1L)).getOrElse(1L)
        val generalizedNode = node.generalise(features)

        if (generalizedNode.isEmpty ||
          reducedNodes.exists {
            case (otherNode, otherFreq) =>
              otherNode.clause.get =~= generalizedNode.clause.get && otherFreq >= freq
          } ||
          reducedNodes.exists {
            case (otherNode, otherFreq) =>
              otherNode.body.get =~= generalizedNode.body.get && otherFreq > freq
          }) reducedNodes
        else
          reducedNodes :+ generalizedNode -> freq

    }.map { case (node, _) => node }
  }

  /**
    * Combine maps into a single map by merging their values for shared keys.
    *
    * @param mapA a map
    * @param mapB another map
    * @return a combination of the given maps containing all their values
    */
  private[graph] def combine[K, V](
      mapA: Map[K, IndexedSeq[V]],
      mapB: Map[K, IndexedSeq[V]]): Map[K, IndexedSeq[V]] = {

    val keySet = mapA.keySet & mapB.keySet

    val merged = keySet.map(key =>
      key -> (mapA(key) ++ mapB(key))).toMap

    merged ++ mapA.filterKeys(!keySet.contains(_)) ++ mapB.filterKeys(!keySet.contains(_))
  }

  /**
    * Hoeffding bound is a tool that can be used as a probabilistic estimator of the generalization
    * error of a model (true expected error on the entire input), given its empirical error (observed
    * error on a training subset). Given a random variable X and the observed mean of X after N
    * independent observations, the Hoeffding Bound states that, with probability 1 - delta, the true
    * mean of the variable lies in an interval (X - epsilon, X + epsilon).
    *
    * @param x an observation 'x', is used along observation 'y' to compute the empirical error
    * @param y an observation 'y', is used along observation 'x' to compute the empirical error
    * @param N number of observations
    * @param delta delta parameter (default is 0.0001)
    * @return true in case the bound is satisfied, else false
    */
  @inline private[graph] def HoeffdingBound(x: Double, y: Double, N: Long, delta: Double = 0.0001): Boolean =
    math.abs(x - y) > math.sqrt(math.log(2 / delta) / (2 * N))

  /**
    * Produce a clause given a sequence of evidence atoms.
    *
    * @param querySignature signature of the query atom
    * @param atoms a sequence of evidence atoms
    * @param mln an MLN
    * @param modes mode declarations
    * @return a clause
    */
  private[graph] def asPattern(
      querySignature: AtomSignature,
      atoms: IndexedSeq[EvidenceAtom],
      mln: MLN, modes: ModeDeclarations): Try[Clause] = {

    // Collect all auxiliary predicates
    val auxiliary = for {
      (signature, db) <- mln.evidence.db.filter { case (signature, _) => signature.symbol.contains(PREFIX) }
      id <- db.identity.indices.filter(db(_) == TRUE)
      constants <- db.identity.decode(id).toOption
    } yield Constant(constants.head) -> (signature, constants.map(Constant))

    // Flatten evidence atoms by adding auxiliary predicates
    val flattenEvidence = atoms
      .foldLeft(Set.empty[EvidenceAtom]) {
        case (result, atom) =>
          val functionPairs = atom.terms.flatMap(auxiliary.get)
          result ++ functionPairs.map {
            case (sig, constants) => EvidenceAtom.asTrue(sig.symbol, constants.toVector)
          } + atom
      }

    // Map constants to variable symbols in order to reuse the variable symbols for the same constants
    var constantsToVar = Map.empty[String, Variable]

    val literals = flattenEvidence.map { atom =>

      // Get the predicate schema of the current signature (predicate domains)
      val schema = mln.schema.predicates.get(atom.signature) match {
        case Some(existingSchema) => existingSchema
        case None => return Failure(
          new NoSuchElementException(s"There is no predicate schema defined for signature '${atom.signature}'"))
      }

      val placeMarkers = modes(atom.signature).placeMarkers
      var terms = Vector.empty[Term]

      for (((constant, domain), placeMarker) <- atom.terms.map(_.symbol) zip schema zip placeMarkers) {
        if (placeMarker.constant) terms :+= Constant(constant)
        else if (constantsToVar.contains(constant)) terms :+= constantsToVar(constant)
        else {
          val variable = Variable(s"x${constantsToVar.size}", domain)
          constantsToVar += constant -> variable
          terms :+= variable
        }
      }

      if (atom.signature == querySignature) Literal(AtomicFormula(atom.symbol, terms), atom.state == FALSE)
      else if (atom.state == FALSE) Literal.asPositive(AtomicFormula(atom.symbol, terms))
      else Literal.asNegative(AtomicFormula(atom.symbol, terms))
    }

    Success(LogicFormatter.ClauseFormatter.introduceFunctions(Clause(literals)))
  }
}
