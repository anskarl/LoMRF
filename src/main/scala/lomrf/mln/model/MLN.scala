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

package lomrf.mln.model

import java.io.{ File, PrintStream }
import java.text.DecimalFormat
import lomrf.logic._
import lomrf.logic.compile._
import lomrf.util.Cartesian.CartesianIterator
import lomrf.util._
import lomrf.util.logging.Implicits._
import com.typesafe.scalalogging.Logger
import lomrf.mln.model.builders.ConstantsDomainBuilder

/**
  * A Markov Logic Networks knowledge base and evidence data.
  *
  * @param schema the domain schema (i.e., definitions of  (dynamic) predicates and (dynamic) functions)
  * @param evidence the specified evidence (i.e., constants, the state of atoms, function mappings and the domain space)
  * @param clauses collection of CNF clauses
  */
final class MLN(
    val schema: MLNSchema,
    val space: PredicateSpace,
    val evidence: Evidence,
    val clauses: Vector[Clause]) {

  /**
    * Function mappers for both user defined functions from evidence, as well as dynamic functions
    */
  val functionMappers: Map[AtomSignature, FunctionMapper] = evidence.functionMappers ++ schema.dynamicFunctions.map {
    case (signature, dynamicFunction) => signature -> FunctionMapper(dynamicFunction)
  }

  /**
    * Determine if the given atom signature corresponds to an atom with closed-world assumption.
    *
    * @param signature the atom's signature
    * @return true if the given atom signature corresponds to an atom with closed-world assumption, otherwise false.
    */
  def isCWA(signature: AtomSignature): Boolean = space.cwa.contains(signature)

  /**
    * Determine if the given atom signature corresponds to an atom with open-world assumption.
    *
    * @param signature the atom's signature
    * @return true if the given atom signature corresponds to an atom with open-world assumption, otherwise false.
    */
  def isOWA(signature: AtomSignature): Boolean = space.owa.contains(signature)

  /**
    * Determine whether the given atom signature corresponds to an atom which may have three states according to
    * the given evidence and KB, i.e. TRUE, FALSE and UNKNOWN.
    *
    * That is atoms that are OWA, Probabilistic or in some cases evidence atoms that some of their groundings are
    * explicitly defined with Unknown state (i.e. prefixed with the symbol ? ).
    *
    * @param signature the atom's signature
    *
    * @return true is the given atom signature corresponds to an atom that its groundings may have three states.
    */
  def isTriState(signature: AtomSignature): Boolean = evidence.triStateAtoms.contains(signature)

  /**
    * Determine if the given atom signature corresponds to a query atom
    *
    * @param signature the atom's signature
    * @return true if the given atom signature corresponds to a query atom, otherwise false.
    */
  def isQueryAtom(signature: AtomSignature): Boolean = space.queryAtoms.contains(signature)

  /**
    * Determine if the given atom signature corresponds to a dynamic atom.
    *
    * @param signature the atom's signature
    * @return true if the given atom signature corresponds to a dynamic atom, otherwise false.
    */
  def isDynamicAtom(signature: AtomSignature): Boolean = schema.dynamicPredicates.contains(signature)

  /**
    * Determine if the given atom signature corresponds to an evidence atom.
    *
    * @param signature the atom's signature
    * @return true if the given atom signature corresponds to an evidence atom, otherwise false.
    */
  def isEvidenceAtom(signature: AtomSignature): Boolean = space.cwa.contains(signature)

  /**
    * Determine if the given atom signature corresponds to a hidden atom (i.e. is not evidence and not query)
    *
    * @param signature the atom's signature
    * @return true if the given atom signature corresponds to a hidden atom, otherwise false.
    */
  def isHiddenAtom(signature: AtomSignature): Boolean = space.hiddenAtoms.contains(signature)

  /**
    * @param signature the atom's signature
    * @return the schema of this atom
    */
  def getSchemaOf(signature: AtomSignature) = schema.predicates.get(signature)

  /**
    * Gives the domain of the given type
    *
    * @param t the type name
    * @return a constantSet (if any).
    */
  def getConstantValuesOf(t: String): Option[ConstantsSet] = evidence.constants.get(t)

  /**
    * Gives the state of the specified ground atom.
    *
    * @param signature the atom's signature [[lomrf.logic.AtomSignature]]
    * @param atomId integer indicating a specific grounding of the given atom signature [[AtomIdentityFunction]]
    * @return TRUE, FALSE or UNKNOWN [[lomrf.logic.TriState]]
    */
  def getStateOf(signature: AtomSignature, atomId: Int) = evidence.db(signature).get(atomId)

  def queryAtoms: Set[AtomSignature] = space.queryAtoms

  def cwa: Set[AtomSignature] = space.cwa

  def owa: Set[AtomSignature] = space.owa

  def hiddenAtoms: Set[AtomSignature] = space.hiddenAtoms

  def probabilisticAtoms: Set[AtomSignature] = evidence.probabilisticAtoms

  def triStateAtoms: Set[AtomSignature] = evidence.triStateAtoms

  override def toString: String = {
    s"""
        |Markov Logic :
        |\tNumber of constant domains.............: ${evidence.constants.size}
        |\tNumber of predicate schema definitions.: ${schema.predicates.size}
        |\tNumber of function schema definitions..: ${schema.functions.size}
        |\tNumber of dynamic predicates...........: ${schema.dynamicPredicates.size}
        |\tNumber of dynamic functions............: ${schema.dynamicFunctions.size}
        |\tNumber of clauses......................: ${clauses.size}
    """.stripMargin
  }

}

object MLN {

  import PredicateCompletionMode._

  def expand(clauses: Vector[Clause], constants: ConstantsDomain): Vector[Clause] = {

    val (toPartialGround, rest) = clauses.partition(clause => clause.variables.exists(_.groundPerConstant))

    if (toPartialGround.nonEmpty) {

      val partiallyGrounded = toPartialGround.
        flatMap { clause =>
          val targetVariables = clause.variables.filter(_.groundPerConstant)

          val iterator = CartesianIterator(targetVariables.map(v => v -> constants(v.domain)).toMap)
          iterator.map(s => clause.substitute(s.mapValues(Constant).asInstanceOf[Map[Term, Term]]))

        }

      partiallyGrounded ++ rest
    } else clauses
  }

  def apply(schema: MLNSchema, evidence: Evidence, space: PredicateSpace, clauses: Vector[Clause]): MLN = {
    new MLN(schema, space, evidence, expand(clauses, evidence.constants))
  }

  def apply(
      predicateSchema: PredicateSchema,
      functionSchema: FunctionSchema,
      dynamicPredicates: DynamicPredicates,
      dynamicFunctions: DynamicFunctions,
      formulas: Set[WeightedFormula],
      constants: ConstantsDomain,
      evidenceDB: EvidenceDB,
      functionMappers: FunctionMappers,
      queryAtoms: Set[AtomSignature],
      owa: Set[AtomSignature]): MLN = {

    val hiddenPredicates = owa -- queryAtoms

    val schema = MLNSchema(predicateSchema, functionSchema, dynamicPredicates, dynamicFunctions)

    val space = PredicateSpace(predicateSchema, queryAtoms, hiddenPredicates, constants)

    val evidence = new Evidence(constants, evidenceDB, functionMappers)

    val clauses = expand(NormalForm.compileCNF(formulas)(constants).toVector, constants)

    new MLN(schema, space, evidence, clauses)
  }

  def apply(
      schema: MLNSchema,
      evidence: Evidence,
      queryAtoms: Set[AtomSignature],
      clauses: Vector[Clause]) = {

    val space = PredicateSpace(schema, queryAtoms, evidence.constants)

    new MLN(schema, space, evidence, expand(clauses, evidence.constants))
  }

  def apply(
      schema: MLNSchema,
      clauses: Vector[Clause],
      constants: ConstantsDomain,
      functionMappers: FunctionMappers,
      evidenceDB: EvidenceDB,
      space: PredicateSpace): MLN = {

    val evidence = new Evidence(constants, evidenceDB, functionMappers)

    new MLN(schema, space, evidence, expand(clauses, evidence.constants))
  }

  /**
    * Constructs a MLN instance from the specified knowledge base and evidence files.
    *
    * @param mlnFileName the path to the MLN file (.mln)
    * @param evidenceFileName the path to the evidence file (.db)
    * @param queryAtoms the set of query atoms
    * @param cwa the set of closed world assumption atoms
    * @param owa the set of open world assumption atoms
    * @param pcm the predicate completion mode to perform [[lomrf.logic.compile.PredicateCompletion]]
    * @return an MLN instance
    */
  def fromFile(
      mlnFileName: String,
      queryAtoms: Set[AtomSignature],
      evidenceFileName: String,
      cwa: Set[AtomSignature] = Set(),
      owa: Set[AtomSignature] = Set(),
      pcm: PredicateCompletionMode = Decomposed,
      dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None): MLN = {

    fromFile(mlnFileName, List(evidenceFileName), queryAtoms, cwa, owa, pcm, dynamicDefinitions)
  }

  def fromFile(
      mlnFileName: String,
      evidenceFileNames: List[String],
      queryAtoms: Set[AtomSignature],
      cwa: Set[AtomSignature],
      owa: Set[AtomSignature],
      pcm: PredicateCompletionMode,
      dynamicDefinitions: Option[ImplFinder.ImplementationsMap]): MLN = {
    val logger = Logger(this.getClass)

    logger.info {
      s"""--- Stage 0: Loading an MLN instance from data...
        |\tInput MLN file: $mlnFileName
        |\tInput evidence file(s): ${evidenceFileNames.mkString(", ")}
        |""".stripMargin
    }

    //parse knowledge base (.mln)
    val (kb, constantsDomain) = KB.fromFile(mlnFileName, dynamicDefinitions)

    val atomSignatures: Set[AtomSignature] = kb.predicateSchema.keySet

    /**
      * Check if the schema of all Query and OWA atoms is defined in the MLN file
      */
    val missingQuerySignatures = queryAtoms.diff(atomSignatures)
    if (missingQuerySignatures.nonEmpty)
      logger.fatal(s"Missing definitions for the following query predicate(s): ${missingQuerySignatures.mkString(", ")}")

    // OWA predicates
    val predicatesOWA = pcm match {
      case Simplification => queryAtoms ++ owa.intersect(atomSignatures)
      case _ =>
        val missingOWASignatures = owa.diff(atomSignatures)

        if (missingOWASignatures.nonEmpty)
          logger.fatal(s"Missing definitions for the following OWA predicate(s): ${missingOWASignatures.mkString(", ")}")

        queryAtoms ++ owa
    }

    // Check for predicates that are mistakenly defined as open and closed
    val openClosedSignatures = cwa.intersect(predicatesOWA)
    if (openClosedSignatures.nonEmpty)
      logger.fatal(s"Predicate(s): ${openClosedSignatures.mkString(", ")} defined both as closed and open.")

    //parse the evidence database (.db)
    val evidence = Evidence.fromFiles(
      kb,
      constantsDomain,
      queryAtoms,
      owa,
      cwa,
      evidenceFileNames.map(new File(_)),
      convertFunctions = false,
      forceCWAForAll   = false)

    val completedFormulas =
      PredicateCompletion(kb.formulas, kb.definiteClauses, pcm)(kb.predicateSchema, kb.functionSchema, evidence.constants)

    // In case that some predicates are eliminated by the predicate completion,
    // remove them from the final predicate schema.
    val resultingPredicateSchema = pcm match {
      case Simplification =>
        val resultingFormulas = kb.predicateSchema -- kb.definiteClauses.map(_.clause.head.signature)
        if (resultingFormulas.isEmpty)
          logger.warn("The given theory is empty (i.e., contains empty set of non-zeroed formulas).")

        resultingFormulas
      case _ => kb.predicateSchema
    }

    val mlnSchema = MLNSchema(resultingPredicateSchema, kb.functionSchema, kb.dynamicPredicates, kb.dynamicFunctions)

    val expandedClauses = expand(
      clauses   = NormalForm
        .compileCNF(completedFormulas)(evidence.constants)
        .toVector,
      constants = evidence.constants)

    val hiddenAtoms = (kb.signatures.predicates -- queryAtoms) -- evidence.cwaAtoms
    val space = PredicateSpace(kb.schema, queryAtoms, hiddenAtoms, evidence.constants)

    // Give the resulting MLN
    new MLN(mlnSchema, space, evidence, expandedClauses)
  }

  /**
    * Constructs a MLN instance and annotation from the specified knowledge base
    * and training files.
    *
    * @param mlnFileName the path to the MLN file (.mln)
    * @param trainingFileNames the path to the training file(s) (.db)
    * @param nonEvidenceAtoms the set of non evidence atoms
    * @param pcm the predicate completion mode to perform [[lomrf.logic.compile.PredicateCompletion]]
    * @param addUnitClauses add a unit clause for each predicate definition to the mln instance
    *
    * @return an MLN instance
    */
  def forLearning(
      mlnFileName: String,
      trainingFileNames: List[String],
      nonEvidenceAtoms: Set[AtomSignature],
      pcm: PredicateCompletionMode = Decomposed,
      dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None,
      addUnitClauses: Boolean = false): (MLN, EvidenceDB) = {

    val logger = Logger(getClass)

    logger.info(
      "--- Stage 0: Loading an MLN instance from data..." +
        "\n\tInput MLN file: " + mlnFileName +
        "\n\tInput training file(s): " + (if (trainingFileNames.nonEmpty) trainingFileNames.mkString(", ") else ""))

    //parse knowledge base (.mln)
    val (kb, constantsDomain) = KB.fromFile(mlnFileName, dynamicDefinitions)

    // All atom signatures
    val atomSignatures: Set[AtomSignature] = kb.predicateSchema.keySet

    // Check if the schema of all Non-Evidence atoms is defined in the MLN file
    nonEvidenceAtoms.find(s => !atomSignatures.contains(s)) match {
      case Some(x) => logger.fatal(s"The predicate '$x' that appears in the query, is not defined in the mln file.")
      case None    => // do nothing
    }

    // Very important for supervised learning: Explicitly define that all atoms except the non-evidence ones will have Closed-world assumption
    // For example, when an evidence atom is missing from the evidence db, we have to make sure that it will remain with Closed-world assumption
    val evidenceAtoms = atomSignatures -- nonEvidenceAtoms

    // Parse the training evidence database (contains the annotation, i.e., the truth values of all query/hidden atoms)
    val trainingEvidence = Evidence.fromFiles(
      kb,
      constantsDomain,
      nonEvidenceAtoms,
      Set.empty,
      atomSignatures,
      trainingFileNames.map(new File(_)),
      convertFunctions = false,
      forceCWAForAll   = true)

    val domainSpace = PredicateSpace(kb.schema, nonEvidenceAtoms, trainingEvidence.constants)

    // Partition the training data into annotation and evidence databases
    var (annotationDB, atomStateDB) = trainingEvidence.db.partition(e => nonEvidenceAtoms.contains(e._1))

    // Define all non evidence atoms as unknown in the evidence database
    for (signature <- annotationDB.keysIterator)
      atomStateDB += (signature -> AtomEvidenceDB.allUnknown(domainSpace.identities(signature)))

    // Define all non evidence atoms for which annotation was not given as false in the annotation database (close world assumption)
    for (signature <- nonEvidenceAtoms; if !annotationDB.contains(signature)) {
      logger.warn(s"Annotation was not given in the training file(s) for predicate '$signature', assuming FALSE state for all its groundings.")
      annotationDB += (signature -> AtomEvidenceDB.allFalse(domainSpace.identities(signature)))
    }

    for (signature <- kb.predicateSchema.keysIterator; if !atomStateDB.contains(signature)) {
      if (evidenceAtoms.contains(signature))
        atomStateDB += (signature -> AtomEvidenceDB.allFalse(domainSpace.identities(signature)))
    }

    val completedFormulas =
      PredicateCompletion(kb.formulas, kb.definiteClauses, pcm)(kb.predicateSchema, kb.functionSchema, trainingEvidence.constants)

    // In case that some predicates are eliminated by the predicate completion,
    // remove them from the final predicate schema.
    val resultingPredicateSchema = pcm match {
      case Simplification =>
        val resultingFormulas = kb.predicateSchema -- kb.definiteClauses.map(_.clause.head.signature)
        if (resultingFormulas.isEmpty)
          logger.warn("The given theory is empty (i.e., contains empty set of non-zeroed formulas).")

        resultingFormulas
      case _ => kb.predicateSchema
    }

    // In case we want to learn weights for unit clauses
    val formulas =
      if (addUnitClauses) {
        completedFormulas ++ resultingPredicateSchema.map {
          case (signature, termTypes) =>
            // Find variables for the current predicate
            val variables = termTypes.zipWithIndex.map {
              case (termType, idx) => Variable("v" + idx, termType)
            }.to(Vector)

            WeightedFormula.asUnit(AtomicFormula(signature.symbol, variables))
        }
      } else completedFormulas

    val mlnSchema = MLNSchema(resultingPredicateSchema, kb.functionSchema, kb.dynamicPredicates, kb.dynamicFunctions)

    logger.info(s"Initialising weight values in target formulas and computing CNF form")

      def initialiseWeight(formula: WeightedFormula): WeightedFormula = {
        if (formula.weight.isNaN) formula.copy(weight = 1.0)
        else formula
      }

    val clauses = expand(
      clauses   = NormalForm
        .compileCNF(formulas.map(initialiseWeight))(trainingEvidence.constants)
        .toVector,
      constants = constantsDomain)

    val evidence = new Evidence(trainingEvidence.constants, atomStateDB, trainingEvidence.functionMappers)

    (new MLN(mlnSchema, domainSpace, evidence, clauses), annotationDB)
  }

  def export(mln: MLN, out: PrintStream = System.out)(implicit numFormat: DecimalFormat = new DecimalFormat("0.############")): Unit = {

    out.println("// Predicate definitions")
    for ((signature, args) <- mln.schema.predicates) {
      val line = signature.symbol + (
        if (args.isEmpty) "\n"
        else "(" + args.mkString(",") + ")\n")
      out.print(line)
    }

    if (mln.schema.functions.nonEmpty) {
      out.println("\n// Functions definitions")
      for ((signature, (retType, args)) <- mln.schema.functions) {
        val line = retType + " " + signature.symbol + "(" + args.mkString(",") + ")\n"
        out.print(line)
      }
    }

    out.println("\n// Clauses")
    mln.clauses.foreach(clause => out.println(clause.toText()))
  }

  /**
    * Constructs a MLN instance and annotation from a specified mln schema,
    * a set of clauses and training files.
    *
    * @param mlnSchema the mln schema
    * @param nonEvidenceAtoms the set of non evidence atoms
    * @param clauses the set of clauses to include in the mln instance
    * @param trainingFileNames the path to the training file(s) (.db)
    *
    * @return an MLN instance
    */
  def forLearning(
      mlnSchema: MLNSchema,
      initialConstantsDomain: ConstantsDomain,
      nonEvidenceAtoms: Set[AtomSignature],
      clauses: Vector[Clause],
      trainingFileNames: List[String]): (MLN, EvidenceDB) = {

    val logger = Logger(getClass)

    logger.info(
      "--- Stage 0: Loading an MLN instance from data..." +
        "\n\tInput training file(s): " + (if (trainingFileNames.nonEmpty) trainingFileNames.mkString(", ") else ""))

    // Create constant builder and append all constants found in the clauses (constants may exist from previous data)
    val builder = ConstantsDomainBuilder.from(initialConstantsDomain)

    val expandedClauses = expand(clauses, initialConstantsDomain)

    expandedClauses.foreach { clause =>
      clause.literals.foreach { literal =>
        val domain = mlnSchema.predicates(literal.sentence.signature)
        literal.sentence.terms.zip(domain).foreach(tuple => if (tuple._1.isConstant) builder += (tuple._2, tuple._1.symbol))
      }
    }

    // Very important for supervised learning: Explicitly define that all atoms except the non-evidence ones will have Closed-world assumption
    // For example, when an evidence atom is missing from the evidence db, we have to make sure that it will remain with Closed-world assumption
    val evidenceAtoms = mlnSchema.predicates.keySet -- nonEvidenceAtoms

    // Parse the training evidence database (contains the annotation, i.e., the truth values of all query/hidden atoms)
    val trainingEvidence = Evidence.
      fromFiles(
        mlnSchema.predicates,
        mlnSchema.functions,
        builder.result(),
        mlnSchema.dynamicFunctions,
        nonEvidenceAtoms,
        Set.empty,
        evidenceAtoms,
        trainingFileNames.map(filename => new File(filename)),
        convertFunctions = false,
        forceCWAForAll   = true)

    forLearning(mlnSchema, trainingEvidence, nonEvidenceAtoms, expandedClauses)
  }

  /**
    * Constructs a MLN instance and annotation from a specified mln schema,
    * a set of clauses and a training evidence database.
    *
    * @param mlnSchema the mln schema
    * @param trainingEvidence the specified training evidence (including annotation)
    * @param nonEvidenceAtoms the set of non evidence atoms
    * @param clauses the set of clauses to include in the mln instance
    *
    * @return an MLN instance
    */
  def forLearning(
      mlnSchema: MLNSchema,
      trainingEvidence: Evidence,
      nonEvidenceAtoms: Set[AtomSignature],
      clauses: Vector[Clause]): (MLN, EvidenceDB) = {

    val logger = Logger(getClass)

    // All atom signatures
    val atomSignatures: Set[AtomSignature] = mlnSchema.predicates.keySet

    val domainSpace = PredicateSpace(mlnSchema, nonEvidenceAtoms, trainingEvidence.constants)

    // Partition the training data into annotation and evidence databases
    var (annotationDB, atomStateDB) = trainingEvidence.db.partition(e => nonEvidenceAtoms.contains(e._1))

    // Define all non evidence atoms as unknown in the evidence database
    for (signature <- annotationDB.keysIterator)
      atomStateDB += (signature -> AtomEvidenceDB.allUnknown(domainSpace.identities(signature)))

    // Define all non evidence atoms for which annotation was not given as false in the annotation database (close world assumption)
    for (signature <- nonEvidenceAtoms; if !annotationDB.contains(signature)) {
      logger.warn(s"Annotation was not given in the training file(s) for predicate '$signature', assuming FALSE state for all its groundings.")
      annotationDB += (signature -> AtomEvidenceDB.allFalse(domainSpace.identities(signature)))
    }

    val evidenceAtoms = atomSignatures -- nonEvidenceAtoms

    for (signature <- mlnSchema.predicates.keysIterator; if !atomStateDB.contains(signature)) {
      if (evidenceAtoms.contains(signature))
        atomStateDB += (signature -> AtomEvidenceDB.allFalse(domainSpace.identities(signature)))
    }

    val evidence = new Evidence(trainingEvidence.constants, atomStateDB, trainingEvidence.functionMappers)

    val expandedClauses = expand(clauses, trainingEvidence.constants)

    // Give the resulting MLN and the annotation database
    (new MLN(mlnSchema, domainSpace, evidence, expandedClauses), annotationDB)
  }
}
