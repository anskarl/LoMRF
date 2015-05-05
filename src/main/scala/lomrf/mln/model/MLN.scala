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

import auxlib.log.Logging
import lomrf.logic._
import lomrf.util._
import scala.collection.breakOut

/**
 * A Markov Logic Networks knowledge base and evidence data.
 *
 * @param schema the domain schema
 * @param clauses collection of CNF clauses
 * @param dynamicPredicates a map that associates signatures of dynamic atoms with a scala function that determines the truth state: (atoms ground arguments) => Boolean
 * @param dynamicFunctions a map that associates the identities of dynamic functions with a scala function that determines the function's result: (ground arguments) => Boolean
 * @param constants the collection of constant types associated with their domain (e.g. the domain time = {1,...100} has ''time'' as type and the set [1,100] as domain)
 * @param functionMappers associates identities to function mappers [[lomrf.util.FunctionMapper]]
 * @param probabilisticAtoms collection of probabilistic atoms
 * @param tristateAtoms collection of atoms with tri-state, i.e., open-world assumption (unknown state) with some evidence (true/false state)
 * @param evidenceDB a map tha associates atom signatures with evidence databases
 */
final class MLN(val schema: MLNSchema,
                val clauses: Vector[Clause],
                val dynamicPredicates: DynamicPredicates,
                val dynamicFunctions: DynamicFunctions,
                val constants: ConstantsDomain,
                val functionMappers: FunctionMappers,
                val probabilisticAtoms: Set[AtomSignature],
                val tristateAtoms: Set[AtomSignature],
                val evidenceDB: EvidenceDB,
                val domainSpace: DomainSpace) {



  /**
   * Determine if the given atom signature corresponds to an atom with closed-world assumption.
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to an atom with closed-world assumption, otherwise false.
   */
  def isCWA(signature: AtomSignature): Boolean = schema.cwa.contains(signature)

  /**
   * Determine if the given atom signature corresponds to an atom with open-world assumption.
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to an atom with open-world assumption, otherwise false.
   */
  def isOWA(signature: AtomSignature): Boolean = schema.owa.contains(signature)

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
  def isTriState(signature: AtomSignature): Boolean = tristateAtoms.contains(signature)

  /**
   * Determine if the given atom signature corresponds to a query atom
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to a query atom, otherwise false.
   */
  def isQueryAtom(signature: AtomSignature): Boolean = schema.queryAtoms.contains(signature)

  /**
   * Determine if the given atom signature corresponds to a dynamic atom.
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to a dynamic atom, otherwise false.
   */
  def isDynamicAtom(signature: AtomSignature): Boolean = dynamicPredicates.contains(signature)

  /**
   * Determine if the given atom signature corresponds to an evidence atom.
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to an evidence atom, otherwise false.
   */
  def isEvidenceAtom(signature: AtomSignature): Boolean = schema.cwa.contains(signature)

  /**
   * Determine if the given atom signature corresponds to a hidden atom (i.e. is not evidence and not query)
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to a hidden atom, otherwise false.
   */
  def isHiddenAtom(signature: AtomSignature): Boolean = schema.hiddenAtoms.contains(signature)

  /**
   * @param signature the atom's signature
   * @return the schema of this atom
   */
  def getSchemaOf(signature: AtomSignature) = schema.predicateSchema.get(signature)

  /**
   * Gives the domain of the given type
   *
   * @param t the type name
   * @return a constantSet (if any).
   */
  def getConstantValuesOf(t: String) = constants.get(t)

  /**
   * Gives the state of the specified ground atom.
   *
   * @param signature the atom's signature [[lomrf.logic.AtomSignature]]
   * @param atomId integer indicating a specific grounding of the given atom signature [[lomrf.util.AtomIdentityFunction]]
   * @return TRUE, FALSE or UNKNOWN [[lomrf.logic.TriState]]
   */
  def getStateOf(signature: AtomSignature, atomId: Int) = evidenceDB(signature).get(atomId)

  override def toString: String = {
    s"""
      |Markov Logic :
      |  Number of constant domains.............: ${constants.size}
      |  Number of predicate schema definitions.: ${schema.predicateSchema.size}
      |  Number of function schema definitions..: ${schema.functionSchema.size}
      |  Number of dynamic predicates...........: ${dynamicPredicates.size}
      |  Number of dynamic functions............: ${dynamicFunctions.size}
      |  Number of constant types...............: ${constants.size}
      |  Number of clauses......................: ${clauses.size}
    """.stripMargin
  }

  /*def dumpContents: String ={
    this.toString + "\n" +
      "\tPredicate schema definitions...........: {\n\t\t" +
      schema.predicateSchema
        .map{case (signature, terms) => signature+" -> ("+terms.mkString(", ")+")"}
        .mkString("\n\t\t")+"\n\t}\n"+
      "\tFunction schema definitions............: {\n\t\t"+
      schema.functionSchema
        .map{case (signature, (returnType, terms)) => signature+" -> "+returnType+" = ("+terms.mkString(", ")+")"}
        .mkString("\n\t\t")+"\n\t}\n"+
      "\tDynamic predicates.....................: {\n\t\t"+
      dynamicPredicates.keys //display only the signature
        .mkString("\n\t\t")+"\n\t}\n"+
      "\tDynamic functions......................: {\n\t\t"+
      dynamicFunctions.keys //display only the signature
        .mkString("\n\t\t")+"\n\t}\n"+
      "\tConstant types.........................: {\n\t\t"+
      constants
        .map{ case (name, constantSet) => name +" -> maps to ["+constantSet.idsRange.head+","+constantSet.idsRange.last+"]."}
        .mkString("\n\t\t")+"\n\t}\n"+""
  }*/

}

object MLN extends Logging {

  import PredicateCompletionMode._

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
  def apply(mlnFileName: String,
            queryAtoms: Set[AtomSignature],
            evidenceFileName: String,
            cwa: Set[AtomSignature] = Set(),
            owa: Set[AtomSignature] = Set(),
            pcm: PredicateCompletionMode = Simplification,
            dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None,
            domainPart: Boolean = false): MLN = {

    apply(mlnFileName, List(evidenceFileName), queryAtoms, cwa, owa, pcm, dynamicDefinitions, domainPart)
  }

  def apply(mlnFileName: String,
            evidenceFileNames: List[String],
            queryAtoms: Set[AtomSignature],
            cwa: Set[AtomSignature],
            owa: Set[AtomSignature],
            pcm: PredicateCompletionMode,
            dynamicDefinitions: Option[ImplFinder.ImplementationsMap],
            domainPart: Boolean): MLN = {

    info(
      "Stage 0: Loading an MLN instance from data..." +
      "\n\tInput MLN file: " + mlnFileName +
      "\n\tInput evidence file(s): " + (if (evidenceFileNames.nonEmpty) evidenceFileNames.mkString(", ") else ""))


    //parse knowledge base (.mln)
    val (kb, constantsDomainBuilders) = KB.fromFile(mlnFileName, pcm, dynamicDefinitions)

    val atomSignatures: Set[AtomSignature] = kb.predicateSchema.keySet

    /**
     * Check if the schema of all Query and OWA atoms is defined in the MLN file
     */
    queryAtoms.find(s => !atomSignatures.contains(s)) match {
      case Some(x) => fatal(s"The predicate '$x' that appears in the query, is not defined in the mln file.")
      case None => // do nothing
    }

    // OWA predicates
    val predicatesOWA = pcm match {
      case Simplification => queryAtoms ++ owa.intersect(atomSignatures)
      case _ =>
        owa.find(s => !atomSignatures.contains(s)) match {
          case Some(x) => fatal(s"The predicate '$x' that appears in OWA, is not defined in the mln file.")
          case None => // do nothing
        }
        queryAtoms ++ owa
    }


    //check for predicates that are mistakenly defined as open and closed
    cwa.find(s => predicatesOWA.contains(s)) match {
      case Some(x) => fatal(s"The predicate '$x' is defined both as closed and open.")
      case None => //do nothing
    }

    //parse the evidence database (.db)
    val evidence: Evidence = Evidence(kb, constantsDomainBuilders, queryAtoms, owa, evidenceFileNames)
    val domainSpace = evidence.domainSpace

    /**
     * Compute the final form of CWA/OWA and Query atoms
     *
     * By default, closed world assumption is assumed for
     * all atoms that appear in the evidence database (.db),
     * unless their signature appears in the OWA set or in the
     * query atoms set. Consequently, open world assumption is
     * assumed for the rest atoms.
     */
    var finalCWA = cwa.toSet

    for (signature <- evidence.db.keysIterator) {
      if (!atomSignatures(signature)) // Check if this signature is defined in the mln file
        fatal(s"The predicate '$signature' that appears in the evidence file, is not defined in the mln file.")
      else if (!owa.contains(signature) && !queryAtoms.contains(signature))
        finalCWA += signature
    }

    val finalOWA = atomSignatures -- finalCWA

    var functionMapperz = evidence.functionMappers
    for ((signature, func) <- kb.dynamicFunctions)
      functionMapperz += (signature -> FunctionMapper(func))


    var atomStateDB = evidence.db
    for (signature <- kb.predicateSchema.keysIterator; if !evidence.db.contains(signature)) {
      val db = if (finalCWA.contains(signature))
        AtomEvidenceDB(domainSpace.identities(signature),FALSE)
      else
        AtomEvidenceDB(domainSpace.identities(signature),UNKNOWN)

      atomStateDB += (signature -> db)
    }

    val probabilisticAtoms: Set[AtomSignature] = (for((signature, stateDB) <- atomStateDB if stateDB.isProbabilistic) yield signature)(breakOut)
    val triStateAtoms: Set[AtomSignature] = (for( (signature, stateDB) <- atomStateDB if stateDB.isTriStateDB ) yield signature)(breakOut)

    val schema = MLNSchema(kb.predicateSchema, kb.functionSchema, queryAtoms, finalCWA, finalOWA)


    // Give the resulting MLN
    //new MLN(schema, kb.formulas, kb.dynamicPredicates, kb.dynamicFunctions, evidence.constants, functionMapperz, probabilisticAtoms, triStateAtoms, atomStateDB)
    val clauses = kb.formulas.par.foldRight(Set[Clause]())((a, b) => a.toCNF(evidence.constants) ++ b).toVector

    new MLN(schema, clauses, kb.dynamicPredicates, kb.dynamicFunctions, evidence.constants, functionMapperz, probabilisticAtoms, triStateAtoms, atomStateDB, domainSpace)
  }


  def learning(mlnFileName: String,
            trainingFileNames: List[String],
            nonEvidenceAtoms: Set[AtomSignature],
            pcm: PredicateCompletionMode = Decomposed,
            dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None,
            addUnitClauses: Boolean = false): (MLN, Map[AtomSignature, AtomEvidenceDB]) = {

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

    //parse the evidence database (.db)
    val evidence: Evidence = Evidence(kb, constantsDomainBuilder, Set.empty[AtomSignature], Set.empty[AtomSignature], trainingFileNames)
    val domainSpace = evidence.domainSpace

    val finalFunctionMappers = evidence.functionMappers ++ kb.dynamicFunctions.map{
      case (signature, func) => signature -> FunctionMapper(func)
    }

    var (annotationDB, atomStateDB) = evidence.db.partition(e => nonEvidenceAtoms.contains(e._1))

    for (signature <- annotationDB.keysIterator)
      atomStateDB += (signature -> AtomEvidenceDB(domainSpace.identities(signature), UNKNOWN))

    for (signature <- nonEvidenceAtoms; if !annotationDB.contains(signature)){
      warn(s"Annotation was not given in the training file(s) for predicate '$signature', assuming FALSE state for all its groundings.")
      annotationDB += (signature -> AtomEvidenceDB(domainSpace.identities(signature), FALSE))
    }

    val probabilisticAtoms = Set.empty[AtomSignature]
    val queryAtoms = nonEvidenceAtoms
    val finalCWA = evidenceAtoms
    val finalOWA = nonEvidenceAtoms
    val triStateAtoms = atomStateDB.filter(db => db._2.isTriStateDB).keySet // is required for grounding

    for (signature <- kb.predicateSchema.keysIterator; if !atomStateDB.contains(signature)) {
      if (finalCWA.contains(signature))
        atomStateDB += (signature -> AtomEvidenceDB(domainSpace.identities(signature), FALSE))
    }

    // In case we want to learn weights for unit clauses
    val formulas = if(addUnitClauses){
      val unitClauses = kb.predicateSchema.map {
        case (signature, termTypes) =>
        // Find variables for the current predicate
        val variables: Vector[Variable] = {
          (for((termType, idx) <- termTypes.zipWithIndex) yield Variable("v" + idx, termType))(breakOut)
        }
        WeightedFormula(1.0, AtomicFormula(signature.symbol, variables))
      }
      kb.formulas ++ unitClauses
    } else {
      kb.formulas
    }

    val schema = MLNSchema(kb.predicateSchema, kb.functionSchema, queryAtoms, finalCWA, finalOWA)

    val clauses = formulas.par.foldRight(Set[Clause]())((a, b) => a.toCNF(evidence.constants) ++ b).toVector

    (new MLN(schema, clauses, kb.dynamicPredicates, kb.dynamicFunctions, evidence.constants, finalFunctionMappers, probabilisticAtoms, triStateAtoms, atomStateDB, domainSpace), annotationDB)
  }



  def apply(formulas: Set[Formula],
            predicateSchema: PredicateSchema,
            functionSchema: FunctionSchema,
            dynamicPredicates: DynamicPredicates,
            dynamicFunctions: DynamicFunctions,
            constants: ConstantsDomain,
            functionMappers: FunctionMappers,
            queryAtoms: Set[AtomSignature],
            cwa: Set[AtomSignature],
            owa: Set[AtomSignature],
            probabilisticAtoms: Set[AtomSignature],
            tristateAtoms: Set[AtomSignature],
            identityFunctions: Identities,
            evidenceDB: EvidenceDB): MLN ={

    val schema = MLNSchema(predicateSchema, functionSchema, queryAtoms, cwa, owa)
    val clauses = formulas.par.foldRight(Set[Clause]())((a, b) => a.toCNF(constants) ++ b).toVector
    val domainSpace = DomainSpace(schema,constants)

    new MLN(schema, clauses, dynamicPredicates, dynamicFunctions, constants, functionMappers, probabilisticAtoms, tristateAtoms, evidenceDB, domainSpace)

  }


}
