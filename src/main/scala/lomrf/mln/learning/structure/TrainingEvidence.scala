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
 * Logical Markov Random Fields.
 *
 * Copyright (c) Anastasios Skarlatidis.
 *
 * This file is part of Logical Markov Random Fields (LoMRF).
 *
 * LoMRF is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * LoMRF is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LoMRF. If not, see <http://www.gnu.org/licenses/>.
 *
 */

package lomrf.mln.learning.structure

import java.io.File
import lomrf.logic.AtomSignature
import lomrf.mln.model._

/**
 * Training evidence contains both evidence and annotation. In case evidence
 * contains functions is also keeps a converted version of the evidence which
 * has converted each function into a auxiliary predicate.
 */
final class TrainingEvidence private(evidence: Evidence, annotation: EvidenceDB,
                                     convertedEvidence: Option[Evidence] = None) {
  /**
   * @return evidence
   */
  def getEvidence = evidence

  /**
   * @return annotation
   */
  def getAnnotation = annotation

  /**
   * @return converted evidence if exists
   */
  def getConvertedEvidence: Option[Evidence] = convertedEvidence
}

/**
 * Training evidence factory.
 */
object TrainingEvidence {

  /**
   * Create a training evidence instance without a converted version
   * in which functions have been replaced with auxiliary predicates.
   *
   * @param evidence evidence
   * @param annotation annotation db
   *
   * @return an instance of training evidence
   */
  def apply(evidence: Evidence, annotation: EvidenceDB) = {
    new TrainingEvidence(evidence, annotation, None)
  }

  /**
   * Create a training evidence instance with a converted version in
   * which functions have been replaced with auxiliary predicates.
   *
   * @param evidence evidence
   * @param annotation annotation db
   * @param convertedEvidence evidence
   *
   * @return an instance of training evidence
   */
  def apply(evidence: Evidence, annotation: EvidenceDB, convertedEvidence: Evidence) = {
    new TrainingEvidence(evidence, annotation, Some(convertedEvidence))
  }

  /**
   *
   * @param kb knowledge base
   * @param constantsDomain constant domain of the knowledge base
   * @param nonEvidenceAtoms set of non evidence atoms
   * @param trainingFileNames a list of training file names
   *
   * @return an instance of training evidence
   */
  def fromFiles(kb: KB,
                constantsDomain: ConstantsDomain,
                nonEvidenceAtoms: Set[AtomSignature],
                trainingFileNames: List[String]): TrainingEvidence = {

    @inline
    def extractAnnotation(trainingEvidence: Evidence, evidenceAtoms: Set[AtomSignature]): (Evidence, EvidenceDB) = {

      // Partition the training data into annotation and evidence databases
      var (annotationDB, atomStateDB) = trainingEvidence.db.partition(e => nonEvidenceAtoms.contains(e._1))

      // Define all non evidence atoms as unknown in the evidence database
      for (signature <- annotationDB.keysIterator)
        atomStateDB += (signature -> AtomEvidenceDB.allUnknown(trainingEvidence.db(signature).identity))

      // Define all non evidence atoms for which annotation was not given as false in the annotation database (close world assumption)
      for (signature <- nonEvidenceAtoms; if !annotationDB.contains(signature)) {
        //warn(s"Annotation was not given in the training file(s) for predicate '$signature', assuming FALSE state for all its groundings.")
        annotationDB += (signature -> AtomEvidenceDB.allFalse(trainingEvidence.db(signature).identity))
      }

      // Define all not seen evidence atoms but existing in the predicate schema as false due to close world assumption
      for (signature <- kb.predicateSchema.keysIterator; if !atomStateDB.contains(signature)) {
        if (evidenceAtoms.contains(signature))
          atomStateDB += (signature -> AtomEvidenceDB.allFalse(trainingEvidence.db(signature).identity))
      }

      (new Evidence(trainingEvidence.constants, atomStateDB, trainingEvidence.functionMappers), annotationDB)
    }

    /*
     * Very important for supervised learning: Explicitly define that all atoms except the non-evidence ones will have
     * closed-world assumption. For example, when an evidence atom is missing from the evidence db, we have to make sure
     * that it will remain as closed-world assumption.
     */
    val evidenceAtoms = kb.predicateSchema.keySet -- nonEvidenceAtoms

    val finalTrainingEvidence =
      if (kb.functionSchema.isEmpty) { // There is no functions in the knowledge base

        // Parse the training evidence database (contains the annotation, i.e., the truth values of all query/hidden atoms)
        val trainingEvidence = Evidence.fromFiles(kb, constantsDomain, nonEvidenceAtoms, Set.empty[AtomSignature],
                                                  evidenceAtoms, trainingFileNames.map(new File(_)), convertFunctions = false)

        // Extract annotation
        val (evidence, annotation) = extractAnnotation(trainingEvidence, evidenceAtoms)

        new TrainingEvidence(evidence, annotation)

      }
      else { // There are functions in the knowledge base

        assert(kb.functionSchema.keySet.map(_.symbol)
              .forall(fn => kb.predicateSchema.keySet
              .exists(as => as.symbol.contains(lomrf.AUX_PRED_PREFIX + fn))), "KB should be created with the flag 'convertFunctions = true'")

        // Parse the training evidence database and keep functions (contains the annotation, i.e., the truth values of all query/hidden atoms)
        val trainingEvidenceWithFunctions = Evidence.fromFiles(kb, constantsDomain, nonEvidenceAtoms, Set.empty[AtomSignature],
          evidenceAtoms, trainingFileNames.map(new File(_)), convertFunctions = false)

        // Parse the training evidence database and convert functions (contains the annotation, i.e., the truth values of all query/hidden atoms)
        val trainingEvidenceWithoutFunctions = Evidence.fromFiles(kb, constantsDomain, nonEvidenceAtoms, Set.empty[AtomSignature],
          evidenceAtoms, trainingFileNames.map(new File(_)), convertFunctions = true)

        val (evidence, annotation) = extractAnnotation(trainingEvidenceWithFunctions, evidenceAtoms)

        val (convertedEvidence, _) = extractAnnotation(trainingEvidenceWithoutFunctions, evidenceAtoms)

        new TrainingEvidence(evidence, annotation, Some(convertedEvidence))
      }

    finalTrainingEvidence
  }
}
