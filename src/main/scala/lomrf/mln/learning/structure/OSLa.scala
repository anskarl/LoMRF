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

import lomrf.logic._
import lomrf.mln.grounding.MRFBuilder
import lomrf.mln.inference.{ILP, Solver}
import lomrf.mln.inference.Solver._
import lomrf.mln.learning.structure.hypergraph.{TemplateExtractor, HyperGraph, PathTemplate}
import lomrf.mln.model._
import lomrf.logic.LogicOps._
import lomrf.logic.AtomSignatureOps._
import lomrf.mln.model.mrf.{MRFState, MRF}
import lomrf.util.time._
import scala.util.{Failure, Success}
import scala.language.existentials

/**
 *
 * This is an implementation of online structure learning algorithm for structure and parameter estimation in
 * Markov Logic Networks. The algorithm exploits background knowledge axioms in order to constrain the search space for
 * definite clauses. Then it uses the axioms to produce a theory and learn parameters using an online weight learning
 * algorithm. The algorithm is based on the online algorithm for MLNs presented in the following publications:
 *
 * <ul>
 * <li> Tuyen N. Huynh and Raymond J. Mooney. Online Structure Learning for Markov Logic Networks (2011)
 * In Proceedings of the European Conference on Machine Learning and Principles and Practice of Knowledge Discovery
 * in Databases (ECML-PKDD 2011), Vol. 2, pp. 81-96, September 2011.
 * The paper can be found in [[http://www.cs.utexas.edu/users/ai-lab/?huynh:ecml11]]
 * </li>
 * </ul>
 *
 * @param kb knowledge base definition used for learning clauses
 * @param constants constant domain of the knowledge base
 * @param evidenceAtoms a set of evidence atoms
 * @param nonEvidenceAtoms a set of non evidence atoms
 * @param modes mode declarations to guide the search
 * @param maxLength maximum length of a path
 * @param allowFreeVariables allow learned clauses to have free variables e.g. variables appearing only once
 * @param threshold evaluation threshold for each new clause produced
 * @param ilpSolver solver type selection option for ILP inference (default is LPSolve) [[lomrf.mln.inference.Solver]]
 * @param lossAugmented use loss augmented inference (default is false)
 * @param lambda regularization parameter for AdaGrad online learner (default is 0.01)
 * @param eta learning rate parameter for AdaGrad online learner (default is 1.0)
 * @param delta delta parameter for AdaGrad (should be positive or equal zero, default is 1.0)
 * @param printLearnedWeightsPerIteration print learned weights for each iteration (default is false)
 * @param axioms a set of weighted formulas, defining the axioms of the background knowledge
 * @param pathTemplates a set of path templates, used by the hypergraph to produce definite clauses
 * @param backgroundClauses a set of background clauses (other clauses given as background knowledge)
 */
final class OSLa private(kb: KB, constants: ConstantsDomain, evidenceAtoms: Set[AtomSignature], nonEvidenceAtoms: Set[AtomSignature],
                         modes: ModeDeclarations, maxLength: Int, allowFreeVariables: Boolean, threshold: Int, theta: Double,
                         ilpSolver: Solver, lossAugmented: Boolean, initialWeightValue: Double, lambda: Double, eta: Double, delta: Double,
                         printLearnedWeightsPerIteration: Boolean, axioms: Set[WeightedFormula], pathTemplates: Set[PathTemplate],
                         backgroundClauses: Vector[Clause]) extends StructureLearner {

  // Current training step
  private var step: Int = 0

  // Initially learned clauses are only the background clauses, it can be empty
  private var learnedClauses: Vector[Clause] = backgroundClauses

  // Initially empty
  private var learnedDefiniteClauses: Set[WeightedDefiniteClause] = Set.empty

  // Initially all weights for the background clauses are zero
  private var weights: Array[Double] = backgroundClauses.map(c => if (c.isHard) 0.0 else initialWeightValue).toArray

  // Sum of square gradients for each clause over all steps
  private var sumSquareGradients = Array.fill[Int](backgroundClauses.length)(0)

  // Previous inferred MRF state
  private var previousMRFState: Option[MRFState] = None

  // All template atoms appearing in the path templates
  private val templateAtoms = pathTemplates.flatMap(_.templateAtoms)

  // Knowledge base definition used for learning clauses
  override protected val knowledgeBase = kb

  // Tolerance threshold for discarding poor clauses at the end of learning
  override protected val tolerance: Double = theta

  /**
   * @return a vector of learned clauses
   */
  override def getLearnedClauses = learnedClauses

  /**
   * @return a vector of learned weights
   */
  override def getLearnedWeights = weights

  /**
   * Find and return all misclassified ground atoms in the previous inferred
   * state. At the initial step of the algorithm assume that everything is
   * is misclassified.
   *
   * @param annotationDB annotation over the non evidence atoms
   *
   * @return all misclassified ground atoms
   */
  private def calculateError(annotationDB: EvidenceDB): Vector[Int] = {

    var totalError = 0.0
    var misclassifiedAtomIDs = Vector[Int]()
    val numberOfExamples = annotationDB.values.map(db => db.identity.indices.length).sum

    info("Calculating misclassified loss...")

    previousMRFState match {

      case Some(state) =>

        val atoms = state.mrf.atoms
        assert(numberOfExamples == atoms.size)

        val iterator = atoms.iterator()
        while(iterator.hasNext) {
          iterator.advance()
          val atom = iterator.value()
          val annotation = annotationDB(atom.id.signature(state.mrf.mln))(atom.id)
          if( (atom.state && annotation == FALSE) || (!atom.state && annotation == TRUE) ) {
            misclassifiedAtomIDs :+= atom.id
            totalError += 1.0
          }
        }

      case None =>
        misclassifiedAtomIDs = annotationDB.values.flatMap(db => db.identity.indices).toVector
        totalError = misclassifiedAtomIDs.length
    }

    info("Total inferred error: " + totalError + "/" + numberOfExamples)
    misclassifiedAtomIDs
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
  override def reviseTheory(trainingEvidence: TrainingEvidence): Vector[Clause] = {

    // Increment training step
    step += 1

    // Construct the MLN by only using the clauses not having template atoms (axioms)
    val mln = MLN(kb.schema, trainingEvidence.getEvidence, nonEvidenceAtoms, learnedClauses)
    val annotationDB = trainingEvidence.getAnnotation

    info(s"AnnotationDB: \n\tAtoms with annotations: ${annotationDB.keys.mkString(",")}")
    info(mln.toString)

    // In case there is no initial set of clauses
    info("Creating MRF...")
    previousMRFState =
      if (mln.clauses.nonEmpty) {
        val mrf = new MRFBuilder(mln, createDependencyMap = true).buildNetwork
        val inferredState = infer(mrf, annotationDB)
        Some(inferredState)
      }
      else {
        warn("MRF cannot be created, because no clauses were found!")
        None
      }

    val misclassifiedAtomIDs = calculateError(annotationDB)
    info(s"Total misclassified ground atoms as true or false: ${misclassifiedAtomIDs.length}")

    val evidence = trainingEvidence.getConvertedEvidence match {
      case Some(obj) => obj
      case None => trainingEvidence.getEvidence
    }

    // Construct hypergraph
    val HG = HyperGraph(mln, evidence.db, annotationDB, modes, Some(pathTemplates))
    info(s"Hypergraph has ${HG.numberOfNodes} nodes (constants) and ${HG.numberOfEdges} edges (true ground atoms)")
    debug(s"Hypergraph Structure:\n$HG")

    // Search for paths using relational pathfinding
    val (pathFindingRuntime, paths) = measureTime { HG.findPaths(misclassifiedAtomIDs, maxLength, allowFreeVariables) }
    info(s"'Relational Pathfinding': ${paths.size} paths found in ${msecTimeToText(pathFindingRuntime)}")
    debug(s"Paths:\n${paths.map(_.toText(mln)).mkString("\n")}")

    val (createClausesRuntime, resultedDefiniteClauses) = measureTime {
      ClauseConstructor.definiteClauses(paths, mln.schema.predicates, modes, evidence, learnedDefiniteClauses)
    }

    val definiteClauses = resultedDefiniteClauses match {
      case Success(result) =>
        info(s"'Clause Creation': ${result.size} clause(s) extracted from paths in ${msecTimeToText(createClausesRuntime)}")
        debug(s"Extracted Clauses:\n${result.map(_.clause.toText).mkString("\n")}")
        result
      case Failure(exception) => fatal(exception.getMessage)
    }

    /*
     * If previous state does not exist, then do not evaluate the found theory, just keep everything
     */
    val goodClauses =
      if (previousMRFState.isEmpty) definiteClauses
      else if (definiteClauses.nonEmpty)
        Evaluator.evaluateDefiniteClauses(definiteClauses, axioms, mln.schema, mln.space, mln.evidence, annotationDB, threshold, previousMRFState)
      else Set.empty

    if (goodClauses.nonEmpty) {

      info(s"'Clause Evaluation': ${goodClauses.size} clause(s) remained")
      debug(s"Remained Clauses:\n${goodClauses.map(_.clause.toText).mkString("\n")}")

      learnedDefiniteClauses ++= goodClauses

      debug(s"All learned definite clauses so far:\n${learnedDefiniteClauses.map(_.clause.toText).mkString("\n")}")

      val completedFormulas =
        PredicateCompletion(axioms, learnedDefiniteClauses)(kb.predicateSchema, kb.functionSchema, trainingEvidence.getEvidence.constants)

      info("Done predicate completion")

      // Perform CNF and filter out any remained axioms
      val currentClauses = ClauseConstructor.makeCNF(completedFormulas)(trainingEvidence.getEvidence.constants)
        .filter(!_.literals.map(_.sentence.signature).exists(s => templateAtoms.contains(s)))

      findTheoryDependencies(currentClauses)
    }
    else
      info(s"No good clauses were found in iteration $step.")

    /*
     * Compute subgradients for all learned clauses. We must ground the MLN because the resulted CNF
     * is different from the previous one.
     */
    val finalMLN = MLN(kb.schema, trainingEvidence.getEvidence, nonEvidenceAtoms, learnedClauses)
    val finalMRF = new MRFBuilder(finalMLN, createDependencyMap = true).buildNetwork
    val finalState = MRFState(finalMRF)

    var trueCounts = Array[Int]()
    var inferredCounts = Array[Int]()

    finalState.setAnnotatedState(annotationDB)
    trueCounts = finalState.countTrueGroundings
    debug("True Counts: [" + trueCounts.deep.mkString(", ") + "]")

    val inferredState = infer(finalMRF, annotationDB)
    inferredCounts = inferredState.countTrueGroundings
    debug("Inferred Counts: [" + inferredCounts.deep.mkString(", ") + "]")

    val subgradientsOfLearnedClauses = Array.fill[Int](finalMLN.clauses.size)(0)
    for (clauseIdx <- finalMLN.clauses.indices) if (!finalMLN.clauses(clauseIdx).isHard) {
      subgradientsOfLearnedClauses(clauseIdx) = inferredCounts(clauseIdx) - trueCounts(clauseIdx)
    }

    /*
     * Perform weight learning step for all learned clauses
     */
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

    if (printLearnedWeightsPerIteration)
      info("Learned weights on step " + (step + 1) + ":\n" +
        "\t[" + weights.deep.mkString(", ") + "]")


    learnedClauses
  }

  @inline
  private def findTheoryDependencies(clauses: Set[Clause]) = {

    var currentTheory = clauses
    info(s"Current theory has ${currentTheory.size} clauses.")

    val (backgroundTheory, previousLearnedTheory) = learnedClauses.splitAt(backgroundClauses.length)
    val (backgroundWeights, previousLearnedWeights) = weights.splitAt(backgroundClauses.length)
    val (backgroundSubGradients, previousSubGradients) = sumSquareGradients.splitAt(backgroundClauses.length)

    info(s"Previous learned theory has ${previousLearnedTheory.size} clauses.")

    /*
     * In case previous learned theory exists, then we must find dependencies
     * in order for the new theory to inherit the learned weights.
     */
    if (previousLearnedTheory.nonEmpty) {

      var resultedTheory = Vector[Clause]()
      var resultedWeights = Array[Double]()
      var resultedSubgradients = Array[Int]()
      var usedIds = Set[Int]()

      info("Searching for identical clauses...")

      // Find all identical clauses
      for (clauseIdx <- previousLearnedTheory.indices)
        currentTheory.find(_ =~= previousLearnedTheory(clauseIdx)) match {
          case Some(identical) =>
            usedIds += clauseIdx
            currentTheory -= identical
            resultedTheory :+= identical
            resultedWeights :+= previousLearnedWeights(clauseIdx)
            resultedSubgradients :+= previousSubGradients(clauseIdx)
          case None => // proceed to the next
        }

      info(s"Found ${resultedTheory.size} identical clauses")
      info(s"Current theory has ${currentTheory.size} clauses.")
      info(s"Previous learned theory has ${previousLearnedTheory.size - usedIds.size} clauses.")
      info(s"Searching for subsumed clauses...")

      for (clauseIdx <- previousLearnedTheory.indices if !usedIds.contains(clauseIdx))
        currentTheory.filter(c => previousLearnedTheory(clauseIdx) subsumes c) match {
          case matched: Set[Clause] if matched.nonEmpty =>
            matched.foreach { m =>
              currentTheory -= m
              resultedTheory :+= m
              resultedWeights :+= previousLearnedWeights(clauseIdx)
              resultedSubgradients :+= previousSubGradients(clauseIdx)
            }
          case _ => // there is no current clause subsumed by this older one
            info(s"Ignore ${previousLearnedTheory(clauseIdx).toText(weighted = false)}")
        }

      info(s"Current theory has ${currentTheory.size} clauses.")

      learnedClauses = backgroundTheory ++ resultedTheory ++ currentTheory
      weights = backgroundWeights ++ resultedWeights ++ currentTheory.toArray.map(c => if (c.isHard) 0.0 else initialWeightValue)
      sumSquareGradients = backgroundSubGradients ++ resultedSubgradients ++ Array.fill[Int](currentTheory.size)(0)
    }

    /*
     * In case previous learned theory was empty, then append current
     */
    else {
      learnedClauses = backgroundTheory ++ currentTheory
      weights = backgroundWeights ++ currentTheory.toArray.map(c => if (c.isHard) 0.0 else initialWeightValue)
      sumSquareGradients = backgroundSubGradients ++ Array.fill[Int](currentTheory.size)(0)
    }

  }

}

/**
 * Factory for the OSLa algorithm.
 */
object OSLa {

  /**
   * Create and OSLa object given an initial knowledge base, non evidence atoms and OSLa parameters.
   *
   * @param kb knowledge base definition used for learning clauses
   * @param constants constant domain of the knowledge base
   * @param nonEvidenceAtoms a set of non evidence atoms
   * @param templateAtoms a set of template atoms
   * @param modes mode declarations to guide the search
   * @param maxLength maximum length of a path
   * @param allowFreeVariables allow learned clauses to have free variables e.g. variables appearing only once
   * @param threshold evaluation threshold for each new clause produced
   * @param ilpSolver solver type selection option for ILP inference (default is LPSolve) [[lomrf.mln.inference.Solver]]
   * @param lossAugmented use loss augmented inference (default is false)
   * @param lambda regularization parameter for AdaGrad online learner (default is 0.01)
   * @param eta learning rate parameter for AdaGrad online learner (default is 1.0)
   * @param delta delta parameter for AdaGrad (should be positive or equal zero, default is 1.0)
   * @param printLearnedWeightsPerIteration print learned weights for each iteration (default is false)
   *
   * @return an instance of OSLa learner
   */
  def apply(kb: KB, constants: ConstantsDomain, nonEvidenceAtoms: Set[AtomSignature],
            templateAtoms: Set[AtomSignature], modes: ModeDeclarations, maxLength: Int, allowFreeVariables: Boolean,
            threshold: Int, theta: Double = 0.0, ilpSolver: Solver = Solver.LPSOLVE, lossAugmented: Boolean = false,
            initialWeightValue: Double = 0.01, lambda: Double = 0.01, eta: Double = 1.0, delta: Double = 1.0,
            printLearnedWeightsPerIteration: Boolean = false): OSLa = {

    val (axioms, clauses, pathTemplates) = TemplateExtractor(kb, constants, nonEvidenceAtoms, templateAtoms) match {
      case Success(tuple) => tuple
      case Failure(exception) => throw exception
    }

    /*
     * Very important for supervised learning: Explicitly define that all atoms except the non-evidence ones will have
     * closed-world assumption. For example, when an evidence atom is missing from the evidence db, we have to make sure
     * that it will remain as closed-world assumption.
     */
    val evidenceAtoms = kb.predicateSchema.keySet -- nonEvidenceAtoms

    new OSLa(kb, constants, evidenceAtoms, nonEvidenceAtoms, modes, maxLength, allowFreeVariables, threshold, theta,
      ilpSolver, lossAugmented, initialWeightValue, lambda, eta, delta, printLearnedWeightsPerIteration, axioms,
      pathTemplates, clauses.toVector)
  }
}
