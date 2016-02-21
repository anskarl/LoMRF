package lomrf.mln.learning.structure

import lomrf.logic.{AtomSignature, FALSE, TRUE, Clause}
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.{Solver, ILP}
import lomrf.mln.inference.Solver._
import lomrf.mln.learning.structure.ClauseConstructor.ClauseType
import lomrf.mln.learning.structure.ClauseConstructor.ClauseType._
import lomrf.mln.learning.structure.hypergraph.HyperGraph
import lomrf.mln.model.mrf.{MRFState, MRF}
import lomrf.mln.model._
import lomrf.logic.AtomSignatureOps._
import lomrf.util.time._
import lomrf.mln.model.ModelOps._
import scala.util.{Failure, Success}

/**
 * This is an implementation of online structure learning algorithm for structure and parameter estimation in
 * Markov Logic Networks. Details about the online algorithm for MLNs can be found in the following publications:
 *
 * <ul>
 * <li> Tuyen N. Huynh and Raymond J. Mooney. Online Structure Learning for Markov Logic Networks (2011)
 * In Proceedings of the European Conference on Machine Learning and Principles and Practice of Knowledge Discovery
 * in Databases (ECML-PKDD 2011), Vol. 2, pp. 81-96, September 2011.
 * The paper can be found in [[http://www.cs.utexas.edu/users/ai-lab/?huynh:ecml11]]
 * </li>
 *
 * @param kb knowledge base definition used for learning clauses
 * @param constants constant domain of the knowledge base
 * @param nonEvidenceAtoms set of non evidence atoms
 * @param modes mode declarations to guide the search
 * @param maxLength maximum length of a path
 * @param allowFreeVariables allow learned clauses to have free variables e.g. variables appearing only once
 * @param threshold evaluation threshold for each new clause produced
 * @param clauseType types of clauses to be produced [[lomrf.mln.learning.structure.ClauseConstructor.ClauseType]]
 * @param ilpSolver solver type selection option for ILP inference [[lomrf.mln.inference.Solver]]
 * @param lossAugmented use loss augmented inference
 * @param lambda regularization parameter for AdaGrad online learner
 * @param eta learning rate parameter for AdaGrad online learner
 * @param delta delta parameter for AdaGrad (should be positive or equal zero)
 * @param printLearnedWeightsPerIteration print learned weights for each iteration
 * @param backgroundClauses existing background theory (initial set of clauses)
 */
final class OSL private(kb: KB, constants: ConstantsDomain, nonEvidenceAtoms: Set[AtomSignature],
                        modes: ModeDeclarations, maxLength: Int, allowFreeVariables: Boolean,
                        threshold: Int, clauseType: ClauseType, ilpSolver: Solver, lossAugmented: Boolean,
                        lambda: Double, eta: Double, delta: Double, printLearnedWeightsPerIteration: Boolean,
                        backgroundClauses: Vector[Clause]) extends StructureLearner {

  // Current training step
  private var step: Int = 0

  // Initially learned clauses are only the background clauses, it can be empty
  private var learnedClauses: Vector[Clause] = backgroundClauses

  // Initially all weights for the background clauses are zero
  private var weights: Array[Double] = Array.fill[Double](backgroundClauses.length)(0.0)

  // Sum of square gradients for each clause over all steps
  private var sumSquareGradients = Array.fill[Int](backgroundClauses.length)(0)

  // Previous inferred MRF state
  private var previousMRFState: Option[MRFState] = None

  // Knowledge base definition used for learning clauses
  override protected val knowledgeBase = kb

  // Tolerance threshold for discarding poor clauses at the end of learning
  override protected val tolerance: Double = 0.0

  /**
   * @return a vector of learned clauses
   */
  override def getLearnedClauses = learnedClauses

  /**
   * @return a vector of learned weights
   */
  override def getLearnedWeights = weights

  /**
   * Find and return all misclassified true ground atoms as false in the previous
   * inferred state. At the initial step of the algorithm assume that everything in
   * the state is false.
   *
   * @param annotationDB annotation over the non evidence atoms
   *
   * @return all misclassified true ground atoms as false
   */
  private def calculateError(annotationDB: EvidenceDB): Vector[Int] = {

    var totalError = 0.0
    var misclassifiedTrueAtomIDs = Vector[Int]()
    val numberOfExamples = annotationDB.values.map(db => db.identity.indices.length).sum

    info("Calculating misclassified loss...")

    previousMRFState match {
      case Some(state) =>

        val atoms = state.mrf.atoms
        assert(numberOfExamples == atoms.size)

        val iterator = atoms.iterator()
        while (iterator.hasNext) {
          iterator.advance()
          val atom = iterator.value()
          val annotation = annotationDB(atom.id.signature(state.mrf.mln))(atom.id)
          if ((atom.state && annotation == FALSE) || (!atom.state && annotation == TRUE)) {
            if (annotation == TRUE) misclassifiedTrueAtomIDs :+= atom.id
            totalError += 1.0
          }
        }

      case None =>
        misclassifiedTrueAtomIDs =
          annotationDB.values.flatMap(db => db.identity.indices.filter(db.get(_) == TRUE)).toVector
        totalError = misclassifiedTrueAtomIDs.length
    }

    info("Total inferred error: " + totalError + "/" + numberOfExamples)
    misclassifiedTrueAtomIDs
  }

  /**
   * Perform inference using the ILP solver and return the inferred state.
   */
  @inline private def infer(mrf: MRF, annotationDB: EvidenceDB): MRFState = {
    mrf.updateConstraintWeights(weights)
    val solver = new ILP(mrf, annotationDB = annotationDB, lossAugmented = lossAugmented, ilpSolver = ilpSolver)
    solver.infer()
  }

  /**
   * Should revise the current theory and return clauses learned for this
   * training evidence as a vector of clauses.
   *
   * @param trainingEvidence the training evidence (includes annotation)
   *
   * @return a vector of learned clauses for the given training evidence
   */
  override def reviseTheory(trainingEvidence: TrainingEvidence) = {

    // Increment training step
    step += 1

    var trueCounts = Array[Int]()
    var inferredCounts = Array[Int]()

    //val (mln, annotationDB) = MLN.forLearning(kb.schema, trainingEvidence, nonEvidenceAtoms, learnedClauses)
    val mln = MLN(kb.schema, trainingEvidence.getEvidence, nonEvidenceAtoms, learnedClauses)
    val annotationDB = trainingEvidence.getAnnotation

    info(s"AnnotationDB: \n\tAtoms with annotations: ${annotationDB.keys.mkString(",")}")
    mln.info()

    // In case there is no initial set of clauses
    info("Creating MRF...")
    previousMRFState =
      if (mln.clauses.nonEmpty) {

      val mrf = new MRFBuilder(mln, createDependencyMap = true).buildNetwork
      val state = MRFState(mrf)

      state.setAnnotatedState(annotationDB)
      trueCounts ++= state.countTrueGroundings
      debug("True Counts: [" + trueCounts.deep.mkString(", ") + "]")

      val inferredState = infer(mrf, annotationDB)
      inferredCounts ++= inferredState.countTrueGroundings
      debug("Inferred Counts: [" + inferredCounts.deep.mkString(", ") + "]")

      Some(inferredState)
    }
    else {
      warn("MRF cannot be created, because no clauses were found!")
      None
    }

    val misclassifiedTrueAtomIDs = calculateError(annotationDB)
    info(s"Total misclassified true ground atoms as false: ${misclassifiedTrueAtomIDs.length}")

    // Construct hypergraph
    val HG = HyperGraph(mln, mln.evidence.db, annotationDB, modes)
    info(s"Hypergraph has ${HG.numberOfNodes} nodes (constants) and ${HG.numberOfEdges} edges (true ground atoms)")
    debug(s"Hypergraph Structure:\n$HG")

    // Search for paths using relational pathfinding
    val (pathFindingRuntime, paths) = measureTime { HG.findPaths(misclassifiedTrueAtomIDs, maxLength, allowFreeVariables) }
    info(s"'Relational Pathfinding': ${paths.size} paths found in ${msecTimeToText(pathFindingRuntime)}")
    debug(s"Paths:\n${paths.map(_.toText(mln)).mkString("\n")}")

    // Create clauses from paths
    val (createClausesRuntime, resultedClauses) = measureTime {
      ClauseConstructor.clauses(paths, mln.schema.predicates, modes, mln.evidence, clauseType, learnedClauses)
    }

    val clauses = resultedClauses match {
      case Success(result) =>
        info(s"'Clause Creation': ${result.size} clause(s) extracted from paths in ${msecTimeToText(createClausesRuntime)}")
        info(s"Extracted Clauses:\n${result.map(_.toText()).mkString("\n")}")
        result
      case Failure(exception) => fatal(exception.getMessage)
    }

    // Evaluate clauses
    val (goodClauses, subgradientsOfGoodClauses) =
      Evaluator.evaluateClauses(clauses, mln.schema, mln.space, mln.evidence, annotationDB, threshold, previousMRFState)

    info(s"'Clause Evaluation': ${goodClauses.size} clause(s) remained")
    info(s"Remained Clauses:\n${goodClauses.map(_.toText()).mkString("\n")}")

    /*
     * Update weights of the already learned clauses
     */
    val subgradientsOfLearnedClauses = Array.fill[Int](mln.clauses.size)(0)
    for (clauseIdx <- mln.clauses.indices) if (!mln.clauses(clauseIdx).isHard) {
      subgradientsOfLearnedClauses(clauseIdx) = inferredCounts(clauseIdx) - trueCounts(clauseIdx)
    }

    var clauseIdx = 0
    while (clauseIdx < learnedClauses.length) {

      sumSquareGradients(clauseIdx) += subgradientsOfLearnedClauses(clauseIdx) * subgradientsOfLearnedClauses(clauseIdx)

      val coefficient = eta / (this.delta + math.sqrt(sumSquareGradients(clauseIdx)))
      val value = weights(clauseIdx) - coefficient * subgradientsOfLearnedClauses(clauseIdx)
      val difference = math.abs(value) - (lambda * coefficient)

      if (difference > 0)
        weights(clauseIdx) = if (value >= 0) difference else -difference
      else weights(clauseIdx) = 0.0

      clauseIdx += 1
    }

    /*
     * Learn weights for the remained good clauses.
     */
    clauseIdx = 0
    while (clauseIdx < goodClauses.length) {

      weights :+= 0.0
      sumSquareGradients :+= subgradientsOfGoodClauses(clauseIdx) * subgradientsOfGoodClauses(clauseIdx)

      val coefficient = eta / (this.delta + math.sqrt(sumSquareGradients(learnedClauses.length + clauseIdx)))
      val value = -coefficient * subgradientsOfGoodClauses(clauseIdx)
      val difference = math.abs(value) - (lambda * coefficient)

      if (difference > 0)
        weights(learnedClauses.length + clauseIdx) = if (value >= 0) difference else -difference

      clauseIdx += 1
    }

    // Append all the remained good clauses to the learned clauses
    learnedClauses ++= goodClauses

    if (printLearnedWeightsPerIteration) {
      info("Learned weights on step " + (step + 1) + ":\n" +
        "\t[" + weights.deep.mkString(", ") + "]")
    }

    goodClauses
  }

}

/**
 * Factory for OSL algorithm
 */
object OSL {

  /**
   * Create and OSL object given an initial knowledge base, non evidence atoms and OSL parameters.
   *
   * @param kb knowledge base definition used for learning clauses
   * @param constants constant domain of the knowledge base
   * @param modes mode declarations to guide the search
   * @param maxLength maximum length of a path
   * @param allowFreeVariables allow learned clauses to have free variables e.g. variables appearing only once
   * @param threshold evaluation threshold for each new clause produced
   * @param clauseType types of clauses to be produced [[lomrf.mln.learning.structure.ClauseConstructor.ClauseType]]
   * @param ilpSolver solver type selection option for ILP inference (default is LPSolve) [[lomrf.mln.inference.Solver]]
   * @param lossAugmented use loss augmented inference (default is false)
   * @param lambda regularization parameter for AdaGrad online learner (default is 0.01)
   * @param eta learning rate parameter for AdaGrad online learner (default is 1.0)
   * @param delta delta parameter for AdaGrad (should be positive or equal zero, default is 1.0)
   * @param printLearnedWeightsPerIteration print learned weights for each iteration (default is false)
   *
   * @return an instance of OSL learner
   */
  def apply(kb: KB, constants: ConstantsDomain, nonEvidenceAtoms: Set[AtomSignature], modes: ModeDeclarations,
            maxLength: Int, allowFreeVariables: Boolean, threshold: Int, clauseType: ClauseType = ClauseType.BOTH,
            ilpSolver: Solver = Solver.LPSOLVE, lossAugmented: Boolean = false, lambda: Double = 0.01,
            eta: Double = 1.0, delta: Double = 1.0, printLearnedWeightsPerIteration: Boolean = false): OSL = {

    new OSL(kb, constants, nonEvidenceAtoms, modes, maxLength, allowFreeVariables,
            threshold, clauseType, ilpSolver, lossAugmented, lambda, eta, delta,
            printLearnedWeightsPerIteration, kb.formulas.flatMap(_.toCNF(constants)).toVector)
  }
}
