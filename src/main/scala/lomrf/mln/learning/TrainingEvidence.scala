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

package lomrf.mln.learning

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import lomrf.logic.AtomSignature
import lomrf.mln.model._

/**
  * Training evidence contains evidence along annotation.
  *
  * @note In case evidence contains functions is also keeps a converted version of
  *       the evidence, in which functions have been converted into auxiliary predicates.
  *
  * @param evidence an evidence collection
  * @param annotation a map from atom signatures to atom evidence databases
  * @param convertedEvidence an evidence collection having converted functions
  */
class TrainingEvidence private (
    evidence: Evidence,
    annotation: EvidenceDB,
    convertedEvidence: Option[Evidence] = None) {

  /**
    * @return a map from domain names to constant sets
    */
  def getConstants: ConstantsDomain = evidence.constants

  /**
    * @return the evidence collection
    */
  def getEvidence: Evidence = evidence

  /**
    * @return a map from atom signatures to atom evidence databases
    */
  def getAnnotation: EvidenceDB = annotation

  /**
    * @return an atom evidence database for the given atom signature
    */
  def getAnnotationFor(signature: AtomSignature): Option[AtomEvidenceDB] = annotation.get(signature)

  /**
    * @return an option value containing the converted evidence,
    *         or None if none exists
    */
  def getConvertedEvidence: Option[Evidence] = convertedEvidence
}

object TrainingEvidence extends LazyLogging {

  /**
    * Creates a training evidence.
    *
    * @see [[lomrf.mln.model.AtomEvidenceDB]]
    *
    * @param evidence an evidence collection
    * @param annotation a map from atom signatures to atom evidence databases
    * @return an TrainingEvidence instance
    */
  def apply(evidence: Evidence, annotation: EvidenceDB): TrainingEvidence = {
    new TrainingEvidence(evidence, annotation, None)
  }

  /**
    * Creates a training evidence.
    *
    * @note In order to convert functions to auxiliary predicates, see the
    *       constructors of [[lomrf.mln.model.Evidence]] and
    *       [[lomrf.mln.model.builders.EvidenceBuilder]].
    *
    * @param evidence an evidence collection
    * @param annotation a map from atom signatures to atom evidence databases
    * @param convertedEvidence an evidence collection having converted functions
    * @return an TrainingEvidence instance
    */
  def apply(evidence: Evidence, annotation: EvidenceDB, convertedEvidence: Evidence): TrainingEvidence = {
    new TrainingEvidence(evidence, annotation, Some(convertedEvidence))
  }

  /**
    * Creates a training evidence from a file path.
    *
    * @param kb a knowledge base
    * @param constantsDomain a map from domain names to constant sets
    * @param nonEvidenceAtoms set of non-evidence atom signatures
    * @param path a path to the training file
    * @return a TrainingEvidence instance
    */
  def fromPath(
      kb: KB,
      constantsDomain: ConstantsDomain,
      nonEvidenceAtoms: Set[AtomSignature],
      path: String): TrainingEvidence = {
    fromFiles(kb, constantsDomain, nonEvidenceAtoms, Iterable(new File(path)))
  }

  /**
    * Creates a training evidence from a list of file paths.
    *
    * @param kb a knowledge base
    * @param constantsDomain a map from domain names to constant sets
    * @param nonEvidenceAtoms set of non-evidence atom signatures
    * @param paths a list of training file paths
    * @return a TrainingEvidence instance
    */
  def fromPaths(
      kb: KB,
      constantsDomain: ConstantsDomain,
      nonEvidenceAtoms: Set[AtomSignature],
      paths: Iterable[String]): TrainingEvidence = {
    fromFiles(kb, constantsDomain, nonEvidenceAtoms, paths.map(new File(_)))
  }

  /**
    * Creates a training evidence from a file.
    *
    * @param kb a knowledge base
    * @param constantsDomain a map from domain names to constant sets
    * @param nonEvidenceAtoms set of non-evidence atom signatures
    * @param file a training file
    * @return a TrainingEvidence instance
    */
  def fromFile(
      kb: KB,
      constantsDomain: ConstantsDomain,
      nonEvidenceAtoms: Set[AtomSignature],
      file: File): TrainingEvidence = {
    fromFiles(kb, constantsDomain, nonEvidenceAtoms, Iterable(file))
  }

  /**
    * Creates a training evidence from an iterable of training files.
    *
    * @param kb a knowledge base
    * @param constantsDomain a map from domain names to constant sets
    * @param nonEvidenceAtoms set of non-evidence atom signatures
    * @param files a list of training files
    * @return a TrainingEvidence instance
    */
  def fromFiles(
      kb: KB,
      constantsDomain: ConstantsDomain,
      nonEvidenceAtoms: Set[AtomSignature],
      files: Iterable[File]): TrainingEvidence = {

      @inline
      def extractAnnotation(
          trainingEvidence: Evidence,
          evidenceAtoms: Set[AtomSignature]): (Evidence, EvidenceDB) = {

        // Partition the training data into annotation and evidence databases
        var (annotationDB, atomStateDB) = trainingEvidence.db.partition {
          case (signature, _) =>
            nonEvidenceAtoms.contains(signature)
        }

        // Define all non-evidence atoms as unknown in the evidence database
        for (signature <- annotationDB.keysIterator)
          atomStateDB += signature -> AtomEvidenceDB.allUnknown(trainingEvidence.db(signature).identity)

        // Define all non-evidence atoms, for which annotation was not given, as False in the annotation database (close-world assumption)
        for (signature <- nonEvidenceAtoms; if !annotationDB.contains(signature)) {
          annotationDB += signature -> AtomEvidenceDB.allFalse(trainingEvidence.db(signature).identity)
        }

        // Define all not seen evidence atoms but existing in the predicate schema as False due to close-world assumption
        for (signature <- kb.predicateSchema.keysIterator; if !atomStateDB.contains(signature)) {
          if (evidenceAtoms.contains(signature))
            atomStateDB += signature -> AtomEvidenceDB.allFalse(trainingEvidence.db(signature).identity)
        }

        new Evidence(trainingEvidence.constants, atomStateDB, trainingEvidence.functionMappers) -> annotationDB
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
        val trainingEvidence = Evidence.fromFiles(
          kb,
          constantsDomain,
          nonEvidenceAtoms,
          Set.empty[AtomSignature],
          evidenceAtoms,
          files,
          convertFunctions = false,
          forceCWAForAll   = true)

        // Extract annotation
        val (evidence, annotation) = extractAnnotation(trainingEvidence, evidenceAtoms)

        new TrainingEvidence(evidence, annotation)

      } else { // There are functions in the knowledge base

        assert(kb.functionSchema.keySet.map(_.symbol)
          .forall(fn => kb.predicateSchema.keySet
            .exists(as => as.symbol.contains(lomrf.AUX_PRED_PREFIX + fn))), "KB should be created with the flag 'convertFunctions = true'")

        logger.info("Constructing evidence that includes functions.")

        // Parse the training evidence database and keep functions (contains the annotation, i.e., the truth values of all query/hidden atoms)
        val trainingEvidenceWithFunctions = Evidence.fromFiles(
          kb,
          constantsDomain,
          nonEvidenceAtoms,
          Set.empty[AtomSignature],
          evidenceAtoms,
          files,
          convertFunctions = false,
          forceCWAForAll   = true)

        logger.info("Constructing evidence where functions have been converted to auxiliary predicates.")

        // Parse the training evidence database and convert functions (contains the annotation, i.e., the truth values of all query/hidden atoms)
        val trainingEvidenceWithoutFunctions = Evidence.fromFiles(
          kb,
          constantsDomain,
          nonEvidenceAtoms,
          Set.empty[AtomSignature],
          evidenceAtoms,
          files,
          convertFunctions = true,
          forceCWAForAll   = true)

        val (evidence, annotation) = extractAnnotation(trainingEvidenceWithFunctions, evidenceAtoms)

        val (convertedEvidence, _) = extractAnnotation(trainingEvidenceWithoutFunctions, evidenceAtoms)

        new TrainingEvidence(evidence, annotation, Some(convertedEvidence))
      }

    finalTrainingEvidence
  }
}
