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
 * Copyright (C) 2012  Anastasios Skarlatidis.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package lomrf.mln.model

import auxlib.log.Logger
import lomrf.logic._
import lomrf.util._
import scala.collection.breakOut

/**
 * A Markov Logic Networks knowledge base and evidence data.
 *
 * @param schema the domain schema (i.e., definitions of  (dynamic) predicates and (dynamic) functions)
 * @param evidence the specified evidence (i.e., constants, the state of atoms, function mappings and the domain space)
 * @param clauses collection of CNF clauses
 */
final class MLN(val schema: MLNSchema,
                val evidence: Evidence,
                val clauses: Vector[Clause]) {


  /**
   * Determine if the given atom signature corresponds to an atom with closed-world assumption.
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to an atom with closed-world assumption, otherwise false.
   */
  def isCWA(signature: AtomSignature): Boolean = evidence.predicateSpace.cwa.contains(signature)

  /**
   * Determine if the given atom signature corresponds to an atom with open-world assumption.
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to an atom with open-world assumption, otherwise false.
   */
  def isOWA(signature: AtomSignature): Boolean = evidence.predicateSpace.owa.contains(signature)

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
  def isTriState(signature: AtomSignature): Boolean = evidence.tristateAtoms.contains(signature)

  /**
   * Determine if the given atom signature corresponds to a query atom
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to a query atom, otherwise false.
   */
  def isQueryAtom(signature: AtomSignature): Boolean = evidence.predicateSpace.queryAtoms.contains(signature)

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
  def isEvidenceAtom(signature: AtomSignature): Boolean = evidence.predicateSpace.cwa.contains(signature)

  /**
   * Determine if the given atom signature corresponds to a hidden atom (i.e. is not evidence and not query)
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to a hidden atom, otherwise false.
   */
  def isHiddenAtom(signature: AtomSignature): Boolean = evidence.predicateSpace.hiddenAtoms.contains(signature)

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
   * @param atomId integer indicating a specific grounding of the given atom signature [[lomrf.util.AtomIdentityFunction]]
   * @return TRUE, FALSE or UNKNOWN [[lomrf.logic.TriState]]
   */
  def getStateOf(signature: AtomSignature, atomId: Int) = evidence.db(signature).get(atomId)

  def queryAtoms: Set[AtomSignature] = evidence.predicateSpace.queryAtoms

  def cwa: Set[AtomSignature] = evidence.predicateSpace.cwa

  def owa: Set[AtomSignature] = evidence.predicateSpace.owa

  def hiddenAtoms: Set[AtomSignature] = evidence.predicateSpace.hiddenAtoms

  def probabilisticAtoms: Set[AtomSignature] = evidence.probabilisticAtoms

  def tristateAtoms: Set[AtomSignature] = evidence.tristateAtoms

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

  def apply(schema: MLNSchema, evidence: Evidence, clauses: Vector[Clause]): MLN = new MLN(schema, evidence, clauses)

  def apply(predicateSchema: PredicateSchema,
            functionSchema: FunctionSchema,
            dynamicPredicates: DynamicPredicates,
            dynamicFunctions: DynamicFunctions,
            formulas: Set[Formula],
            constants: ConstantsDomain,
            evidenceDB: EvidenceDB,
            functionMappers: FunctionMappers,
            queryAtoms: Set[AtomSignature],
            owa: Set[AtomSignature]): MLN = {


    val hiddenPredicates = owa -- queryAtoms

    val schema = MLNSchema(predicateSchema, functionSchema, dynamicPredicates, dynamicFunctions)

    val space = PredicateSpace(predicateSchema, queryAtoms, hiddenPredicates, constants)

    val evidence = Evidence(constants, evidenceDB, functionMappers, space)

    val clauses =  NormalForm.compileCNF(formulas)(constants).toVector

    new MLN(schema, evidence, clauses)
  }


  def apply(schema: MLNSchema,
            clauses: Vector[Clause],
            constants: ConstantsDomain,
            functionMappers: FunctionMappers,
            evidenceDB: EvidenceDB,
            space: PredicateSpace): MLN = {

    val evidence = Evidence(constants,evidenceDB,functionMappers,space)

    new MLN(schema, evidence, clauses)
  }


  /**
   * Constructs a MLN instance from the specified knowledge base and evidence files.
   *
   * @param mlnFileName the path to the MLN file (.mln)
   * @param evidenceFileName the path to the evidence file (.db)
   * @param queryAtoms the set of query atoms
   * @param cwa the set of closed world assumption atoms
   * @param owa the set of open world assumption atoms
   * @param pcm the predicate completion mode to perform [[lomrf.logic.PredicateCompletion]]
   *
   * @return an MLN instance
   */
  def fromFile(mlnFileName: String,
            queryAtoms: Set[AtomSignature],
            evidenceFileName: String,
            cwa: Set[AtomSignature] = Set(),
            owa: Set[AtomSignature] = Set(),
            pcm: PredicateCompletionMode = Simplification,
            dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None,
            domainPart: Boolean = false): MLN = {

    fromFile(mlnFileName, List(evidenceFileName), queryAtoms, cwa, owa, pcm, dynamicDefinitions, domainPart)
  }

  def fromFile(mlnFileName: String,
            evidenceFileNames: List[String],
            queryAtoms: Set[AtomSignature],
            cwa: Set[AtomSignature],
            owa: Set[AtomSignature],
            pcm: PredicateCompletionMode,
            dynamicDefinitions: Option[ImplFinder.ImplementationsMap],
            domainPart: Boolean): MLN = {

    val logger = Logger(getClass)
    import logger._

    info(
      s"""--- Stage 0: Loading an MLN instance from data...
        |\tInput MLN file: $mlnFileName
        |\tInput evidence file(s): ${evidenceFileNames.mkString(", ")}""".stripMargin)


    //parse knowledge base (.mln)
    val (kb, constantsDomainBuilders) = KB.fromFile(mlnFileName, pcm, dynamicDefinitions)

    val atomSignatures: Set[AtomSignature] = kb.predicateSchema.keySet

    /**
     * Check if the schema of all Query and OWA atoms is defined in the MLN file
     */
    val missingQuerySignatures = queryAtoms.diff(atomSignatures)
    if(missingQuerySignatures.nonEmpty)
      fatal(s"Missing definitions for the following query predicate(s): ${missingQuerySignatures.mkString(", ")}")


    // OWA predicates
    val predicatesOWA = pcm match {
      case Simplification => queryAtoms ++ owa.intersect(atomSignatures)
      case _ =>
        val missingOWASignatures = owa.diff(atomSignatures)

        if(missingOWASignatures.nonEmpty)
          fatal(s"Missing definitions for the following OWA predicate(s): ${missingOWASignatures.mkString(", ")}")

        queryAtoms ++ owa
    }


    //Check for predicates that are mistakenly defined as open and closed
    val openClosedSignatures = cwa.intersect(predicatesOWA)
    if(openClosedSignatures.nonEmpty)
      fatal(s"Predicate(s): ${openClosedSignatures.mkString(", ")} defined both as closed and open.")

    //parse the evidence database (.db)
    val evidence: Evidence = Evidence.fromFiles(kb, constantsDomainBuilders, queryAtoms, owa, evidenceFileNames)

    val clauses = NormalForm.compileCNF(kb.formulas)(evidence.constants).toVector

    // Give the resulting MLN
    new MLN(kb.schema, evidence, clauses)
  }


  def forLearning(mlnFileName: String,
                  trainingFileNames: List[String],
                  nonEvidenceAtoms: Set[AtomSignature],
                  pcm: PredicateCompletionMode = Decomposed,
                  dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None,
                  addUnitClauses: Boolean = false): (MLN, EvidenceDB) = {
    val logger = Logger(getClass)
    import logger._

    info(
      "--- Stage 0: Loading an MLN instance from data..." +
        "\n\tInput MLN file: " + mlnFileName +
        "\n\tInput training file(s): " + (if (trainingFileNames.nonEmpty) trainingFileNames.mkString(", ") else ""))

    //parse knowledge base (.mln)
    val (kb, constantsDomainBuilder) = KB.fromFile(mlnFileName, pcm, dynamicDefinitions)

    val atomSignatures: Set[AtomSignature] = kb.predicateSchema.keySet

    /**
     * Check if the schema of all Non-Evidence atoms is defined in the MLN file
     */
    nonEvidenceAtoms.find(s => !atomSignatures.contains(s)) match {
      case Some(x) => fatal(s"The predicate '$x' that appears in the query, is not defined in the mln file.")
      case None => // do nothing
    }

    val evidenceAtoms = atomSignatures -- nonEvidenceAtoms

    //parse the training evidence database (contains the annotation, i.e., the truth values of all query/hidden atoms)
    val trainingEvidence = Evidence.fromFiles(kb, constantsDomainBuilder, nonEvidenceAtoms, trainingFileNames)
    val domainSpace = trainingEvidence.predicateSpace

    var (annotationDB, atomStateDB) = trainingEvidence.db.partition(e => nonEvidenceAtoms.contains(e._1))

    for (signature <- annotationDB.keysIterator)
      atomStateDB += (signature -> AtomEvidenceDB.allUnknown(domainSpace.identities(signature)))

    for (signature <- nonEvidenceAtoms; if !annotationDB.contains(signature)) {
      warn(s"Annotation was not given in the training file(s) for predicate '$signature', assuming FALSE state for all its groundings.")
      annotationDB += (signature -> AtomEvidenceDB.allFalse(domainSpace.identities(signature)))
    }

    for (signature <- kb.predicateSchema.keysIterator; if !atomStateDB.contains(signature)) {
      if (evidenceAtoms.contains(signature))
        atomStateDB += (signature -> AtomEvidenceDB.allFalse(domainSpace.identities(signature)))
    }

    // In case we want to learn weights for unit clauses
    val formulas =
      if (addUnitClauses) {
        kb.formulas ++ kb.predicateSchema.map {
          case (signature, termTypes) =>
            // Find variables for the current predicate
            val variables: Vector[Variable] = termTypes.zipWithIndex.map{
              case (termType, idx) => Variable("v" + idx, termType)
            }(breakOut)

            WeightedFormula.asUnit(AtomicFormula(signature.symbol, variables))
        }
      } else kb.formulas


    val clauses = NormalForm.compileCNF(formulas)(trainingEvidence.constants).toVector

    val evidence = new Evidence(trainingEvidence.constants, atomStateDB, trainingEvidence.functionMappers, trainingEvidence.predicateSpace)

    (new MLN(kb.schema, evidence, clauses), annotationDB)
  }

}
