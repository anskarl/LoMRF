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

package object graphs {

  /*
   * Use a very close to zero (non zero) value in order to avoid singular
   * or degenerate matrices, i.e. having zero determinant.
   */
  val UNCONNECTED = 0.0001

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
  private[graphs] def HoeffdingBound(x: Double, y: Double, N: Int, delta: Double = 0.0001) =
    math.abs(x - y) > math.sqrt(math.log(1 / delta) / (2 * N))

  /**
    * Produce a clause given a sequence of evidence atoms.
    *
    * @param querySignature signature of the query atom
    * @param atoms a sequence of evidence atoms
    * @param mln an MLN
    * @param modes mode declarations
    * @return a clause
    */
  private[graphs] def asPattern(
      querySignature: AtomSignature,
      atoms: IndexedSeq[EvidenceAtom],
      mln: MLN, modes: ModeDeclarations): Try[Clause] = {

    // Collect all auxiliary predicates
    val auxiliary = for {
      (signature, db) <- mln.evidence.db.filter { case (signature, _) => signature.symbol.contains(PREFIX) }
      id <- db.identity.indices.filter(db(_) == TRUE)
      constants <- db.identity.decode(id).toOption
    } yield Constant(constants.head) -> (signature, constants.map(Constant))

    // flatten evidence atoms by adding auxiliary predicates
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
