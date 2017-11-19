/*
 * o                        o     o   o         o
 * |             o          |     |\ /|         | /
 * |    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
 * |    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
 * O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
 *             |
 *          o--o
 * o--o              o               o--o       o    o
 * |   |             |               |    o     |    |
 * O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
 * |  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
 * o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o
 *
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.util.evaluation

import lomrf.logic.AtomSignature
import lomrf.mln.model.{AtomEvidenceDB, MLN}
import lomrf.mln.model.mrf.GroundAtom
import lomrf.util._

import scala.collection.parallel.ParIterable

object Evaluate {

  /**
   * Count the number of true positives, true negatives, false positives and false negatives in the given MRF,
   * regarding the specified annotation.
   *
   * @param atoms a par-iterable collection of ground atoms
   * @param annotationDB the annotated state of ground atoms
   *
   * @return the counted true positives, true negatives, false positives and false negatives
   */
  def apply(atoms: ParIterable[GroundAtom], annotationDB: Map[AtomSignature, AtomEvidenceDB])(implicit mln: MLN): EvaluationStats ={
    atoms
      .map{ groundAtom =>
        val inferredState = groundAtom.getState
        val gid = groundAtom.id
        val signature = mln.space.signatureOf(gid)
        val annotationState = annotationDB(signature)(gid)
        evaluateSingle(inferredState, annotationState)
      }
      .reduce(combine)
  }

  /**
   * Count the number of true positives, true negatives, false positives and false negatives in the given MRF,
   * regarding the specified annotation.
   *
   * @param atoms a par-iterable collection of ground atoms
   * @param annotationDB the annotated state of ground atoms
   * @param samples the number of samples taken during marginal inference
   * @param threshold the minimum probability that we assume as true state (e.g., recognised, classified, etc.)
   *
   * @return the counted true positives, true negatives, false positives and false negatives
   */
  def apply(atoms: ParIterable[GroundAtom], annotationDB: Map[AtomSignature, AtomEvidenceDB], samples: Int, threshold: Double = 0.5)(implicit mln: MLN): EvaluationStats ={

    require( threshold > 0.0 && threshold < 1.0, "Threshold value should be between 0 and 1.")
    require( samples > 0 , "Number of samples should be great than zero.")

    atoms
      .map{ groundAtom =>
        val inferredProbability = groundAtom.getTruesCount * 1.0 / samples
        val state = inferredProbability >= threshold
        val gid = groundAtom.id
        val signature = mln.space.signatureOf(gid)
        val annotationState = annotationDB(signature)(gid)
        evaluateSingle(state, annotationState)
      }
      .reduce(combine)
  }

}
