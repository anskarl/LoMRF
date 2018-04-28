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

package lomrf.util.evaluation

import scala.collection.parallel.ParSeq
import lomrf.logic.{AtomSignature, EvidenceAtom, UNKNOWN}
import lomrf.mln.model.{AtomEvidenceDB, MLN}
import lomrf.mln.model.mrf.GroundAtom

object Evaluate {

  /**
    * @return an empty evaluation tuple
    */
  def empty: EvaluationStats  = (0, 0, 0, 0)

  /**
    * Count the number of true positives, true negatives, false positives and false negatives in the given evidence atoms,
    * regarding the specified annotation.
    *
    * @param atoms a par-iterable collection of evidence atoms
    * @param annotationDB the annotated state of ground atoms
    * @param previousEvaluation previous evaluation stats to be combined (optional)
    *
    * @return the counted true positives, true negatives, false positives and false negatives
    */
  def apply(atoms: ParSeq[EvidenceAtom],
            annotationDB: Map[AtomSignature, AtomEvidenceDB],
            previousEvaluation: Option[EvaluationStats]): EvaluationStats = {

    if (atoms.isEmpty) previousEvaluation match {
      case Some(previousStats) => return previousStats
      case None => return empty
    }

    require(atoms.forall(_.state != UNKNOWN) , "All evidence atoms should have known truth values.")

    val currentStats = atoms.map { evidenceAtom =>
        val truthValue = evidenceAtom.state.value
        val db = annotationDB(evidenceAtom.signature)
        val annotation = db(evidenceAtom.terms.map(_.toString))
        evaluateSingle(truthValue > 0, annotation)
      }.reduce(combine)

    previousEvaluation match {
      case Some(previousStats) => combine(previousStats, currentStats)
      case None => currentStats
    }
  }

  /**
    * Count the number of true positives, true negatives, false positives and false negatives in the given MRF,
    * regarding the specified annotation.
    *
    * @param atoms a par-iterable collection of ground atoms
    * @param annotationDB the annotated state of ground atoms
    * @param previousEvaluation previous evaluation stats to be combined (optional)
    *
    * @return the counted true positives, true negatives, false positives and false negatives
    */
  def apply(atoms: ParSeq[GroundAtom],
            annotationDB: Map[AtomSignature, AtomEvidenceDB],
            previousEvaluation: Option[EvaluationStats])(implicit mln: MLN): EvaluationStats = {

    val currentStats = atoms.map{ groundAtom =>
        val inferredState = groundAtom.getState
        val gid = groundAtom.id
        val signature = mln.space.signatureOf(gid)
        val annotationState = annotationDB(signature)(gid)
        evaluateSingle(inferredState, annotationState)
      }
      .reduce(combine)

    previousEvaluation match {
      case Some(previousStats) => combine(previousStats, currentStats)
      case None => currentStats
    }
  }

  /**
    * Count the number of true positives, true negatives, false positives and false negatives in the given MRF,
    * regarding the specified annotation.
    *
    * @param atoms a par-iterable collection of ground atoms
    * @param annotationDB the annotated state of ground atoms
    * @param samples the number of samples taken during marginal inference
    * @param threshold the minimum probability that we assume as true state (e.g., recognised, classified, etc.)
    * @param previousEvaluation previous evaluation stats to be combined (optional)
    *
    * @return the counted true positives, true negatives, false positives and false negatives
    */
  def apply(atoms: ParSeq[GroundAtom],
            annotationDB: Map[AtomSignature, AtomEvidenceDB],
            samples: Int, threshold: Double = 0.5,
            previousEvaluation: Option[EvaluationStats])(implicit mln: MLN): EvaluationStats = {

    require(threshold > 0.0 && threshold < 1.0, "Threshold value should be between 0 and 1.")
    require(samples > 0, "Number of samples should be great than zero.")

    val currentStats = atoms.map{ groundAtom =>
        val inferredProbability = groundAtom.getTruesCount * 1.0 / samples
        val state = inferredProbability >= threshold
        val gid = groundAtom.id
        val signature = mln.space.signatureOf(gid)
        val annotationState = annotationDB(signature)(gid)
        evaluateSingle(state, annotationState)
      }
      .reduce(combine)

    previousEvaluation match {
      case Some(previousStats) => combine(previousStats, currentStats)
      case None => currentStats
    }
  }

}
