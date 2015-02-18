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
import scala.collection

/**
 * A Markov Logic Networks knowledge base and evidence data.
 *
 * @param formulas the collection of (possibly weighted) formulas in First-order Logic (see [[lomrf.logic.Formula]])
 * @param predicateSchema a map that associates atom signatures with a sequence of argument types
 * @param functionSchema a map that associates function signatures with a tuple of returning type and sequence of argument types
 * @param dynamicPredicates a map that associates signatures of dynamic atoms with a scala function that determines the truth state: (atoms ground arguments) => Boolean
 * @param dynamicFunctions a map that associates the identities of dynamic functions with a scala function that determines the function's result: (ground arguments) => Boolean
 * @param constants the collection of constant types associated with their domain (e.g. the domain time = {1,...100} has ''time'' as type and the set [1,100] as domain)
 * @param functionMappers associates identities to function mappers [[lomrf.util.FunctionMapper]]
 * @param queryAtoms the set of query atom signatures
 * @param cwa the set of closed-world assumption atom signatures
 * @param owa the set of open-world assumption atom signatures (includes query atom signatures)
 * @param identityFunctions a map that associates atom signatures with atom identity functions
 * @param atomStateDB a map tha associates atom signatures with evidence databases
 * @param orderedStartIDs an ordered array of integers, each one is the start id of some predicate
 * @param orderedAtomSignatures ordered atom signatures in terms of their start id
 * @param queryStartID the id of the first ground query atom
 * @param queryEndID the id of the last ground query atom
 * @author Anastasios Skarlatidis
 */
final class MLN(
           val formulas: collection.Set[Formula],
           val predicateSchema: collection.Map[AtomSignature, collection.Seq[String]],
           val functionSchema: collection.Map[AtomSignature, (String, Vector[String])],
           val dynamicPredicates: Map[AtomSignature, Vector[String] => Boolean],
           val dynamicFunctions: Map[AtomSignature, Vector[String] => String],
           val constants: Map[String, ConstantsSet],
           val functionMappers: Map[AtomSignature, FunctionMapper],
           val queryAtoms: collection.Set[AtomSignature],
           val cwa: collection.Set[AtomSignature],
           val owa: collection.Set[AtomSignature],
           val probabilisticAtoms: collection.Set[AtomSignature],
           val tristateAtoms: collection.Set[AtomSignature],
           val identityFunctions: Map[AtomSignature, AtomIdentityFunction],
           val atomStateDB: Map[AtomSignature, AtomEvidenceDB],
           val orderedStartIDs: Array[Int],
           val orderedAtomSignatures: Array[AtomSignature],
           val queryStartID: Int,
           val queryEndID: Int) {

  def this(formulas: collection.Set[Formula],
           predicateSchema: collection.Map[AtomSignature, collection.Seq[String]],
           functionSchema: collection.Map[AtomSignature, (String, Vector[String])],
           dynamicPredicates: Map[AtomSignature, Vector[String] => Boolean],
           dynamicFunctions: Map[AtomSignature, Vector[String] => String],
           constants: Map[String, ConstantsSet],
           functionMappers: Map[AtomSignature, FunctionMapper],
           queryAtoms: collection.Set[AtomSignature],
           cwa: collection.Set[AtomSignature],
           owa: collection.Set[AtomSignature],
           probabilisticAtoms: collection.Set[AtomSignature],
           tristateAtoms: collection.Set[AtomSignature],
           atomStateDB: Map[AtomSignature, AtomEvidenceDB],
           atomIdentifier: AtomIdentifier) = {
    this(formulas, predicateSchema, functionSchema, dynamicPredicates, dynamicFunctions,
      constants, functionMappers, queryAtoms, cwa, owa, probabilisticAtoms, tristateAtoms,
      atomIdentifier.identities, atomStateDB, atomIdentifier.orderedStartIDs, atomIdentifier.orderedAtomSignatures,
      atomIdentifier.queryStartID, atomIdentifier.queryEndID)
  }
  
  /**
   * The set of ground clauses
   */
  lazy val clauses = formulas.par.foldRight(Set[Clause]())((a, b) => a.toCNF(constants) ++ b).toVector

  /**
   * The set of hidden atoms, those that are not query and not evidence.
   */
  val hiddenAtoms = owa -- queryAtoms

  /**
   * Total number of query atoms
   */
  val numberOfQueryAtoms = queryAtoms.map(signature => identityFunctions(signature).length).sum

  /**
   * Determine if the given atom signature corresponds to an atom with closed-world assumption.
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to an atom with closed-world assumption, otherwise false.
   */
  def isCWA(signature: AtomSignature): Boolean = cwa.contains(signature)

  /**
   * Determine if the given atom signature corresponds to an atom with open-world assumption.
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to an atom with open-world assumption, otherwise false.
   */
  def isOWA(signature: AtomSignature): Boolean = owa.contains(signature)

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
  def isQueryAtom(signature: AtomSignature): Boolean = queryAtoms.contains(signature)

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
  def isEvidenceAtom(signature: AtomSignature): Boolean = cwa.contains(signature)

  /**
   * Determine if the given atom signature corresponds to a hidden atom (i.e. is not evidence and not query)
   *
   * @param signature the atom's signature
   * @return true if the given atom signature corresponds to a hidden atom, otherwise false.
   */
  def isHiddenAtom(signature: AtomSignature): Boolean = hiddenAtoms.contains(signature)

  /**
   * @param signature the atom's signature
   * @return the schema of this atom
   */
  def getSchemaOf(signature: AtomSignature) = predicateSchema.get(signature)

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
  def getStateOf(signature: AtomSignature, atomId: Int) = atomStateDB(signature).get(atomId)

  override def toString: String = {
    "Markov Logic { \n" +
      "\tConstant domains   = " + constants.size + "\n" +
      "\tSchema definitions = " + predicateSchema.size + "\n" +
      "\tFormulas           = " + formulas.size + "\n" +
      "}\n"
  }

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
            evidenceFileName: String,
            queryAtoms: Set[AtomSignature],
            cwa: collection.Set[AtomSignature] = Set(),
            owa: collection.Set[AtomSignature] = Set(),
            pcm: PredicateCompletionMode = Simplification,
            dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None,
            domainPart:Boolean = false): MLN = {
    apply(mlnFileName, List(evidenceFileName), queryAtoms, cwa, owa, pcm, dynamicDefinitions, domainPart)
  }


  def apply(mlnFileName: String,
            evidenceFileNames: List[String],
            queryAtoms: Set[AtomSignature],
            cwa: collection.Set[AtomSignature],
            owa: collection.Set[AtomSignature],
            pcm: PredicateCompletionMode,
            dynamicDefinitions: Option[ImplFinder.ImplementationsMap],
            domainPart: Boolean): MLN = {

    info(
      "Stage 0: Loading an MLN instance from data..." +
      "\n\tInput MLN file: " + mlnFileName +
      "\n\tInput evidence file(s): " + (if (evidenceFileNames.nonEmpty) evidenceFileNames.reduceLeft(_+", "+_) else ""))


    //parse knowledge base (.mln)
    val kb = KB(mlnFileName, pcm, dynamicDefinitions)

    val atomSignatures: collection.Set[AtomSignature] = kb.predicateSchema.keySet

    /**
     * Check if the schema of all Query and OWA atoms is defined in the MLN file
     */
    queryAtoms.find(s => !atomSignatures.contains(s)) match {
      case Some(x) => fatal("The predicate " + x + " that appears in the query, is not defined in the mln file.")
      case None => // do nothing
    }

    // OWA predicates
    val predicatesOWA = pcm match {
      case Simplification => queryAtoms ++ owa.filter(atomSignatures.contains)
      case _ =>
        owa.find(s => !atomSignatures.contains(s)) match {
          case Some(x) => fatal("The predicate " + x + " that appears in OWA, is not defined in the mln file.")
          case None => // do nothing
        }
        queryAtoms ++ owa
    }


    //check for predicates that are mistakenly defined as open and closed
    cwa.find(s => predicatesOWA.contains(s)) match {
      case Some(x) => fatal("The predicate " + x + " is defined both as closed and open.")
      case None => //do nothing
    }

    //parse the evidence database (.db)
    val evidence: Evidence = Evidence(kb, queryAtoms, owa, evidenceFileNames)

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

    for (signature <- evidence.atomsEvDB.keysIterator) {
      if (!atomSignatures(signature)) // Check if this signature is defined in the mln file
        fatal("The predicate " + signature + " that appears in the evidence file, is not defined in the mln file.")
      else if (!owa.contains(signature) && !queryAtoms.contains(signature))
        finalCWA += signature
    }

    val finalOWA = atomSignatures -- finalCWA

    var functionMapperz = evidence.functionMappers
    for ((signature, func) <- kb.dynamicFunctions) {
      functionMapperz += (signature -> FunctionMapper(func))
    }

    var atomStateDB = evidence.atomsEvDB
    for (signature <- kb.predicateSchema.keysIterator; if !evidence.atomsEvDB.contains(signature)) {
      val db = if (finalCWA.contains(signature))
        AtomEvidenceDB(evidence.identities(signature),FALSE)//, kb.predicateSchema(signature).zipWithIndex.toMap, FALSE)
      else
        AtomEvidenceDB(evidence.identities(signature),UNKNOWN)//, kb.predicateSchema(signature).zipWithIndex.toMap, UNKNOWN)

      atomStateDB += (signature -> db)
    }

    val probabilisticAtoms = atomStateDB.filter(db => db._2.isProbabilistic).map(_._1).toSet
    val triStateAtoms = atomStateDB.filter(db => db._2.isTriStateDB).map(_._1).toSet

    // Give the resulting MLN
    new MLN(kb.formulas, kb.predicateSchema, kb.functionSchema, kb.dynamicPredicates, kb.dynamicFunctions,
      evidence.constants, functionMapperz, queryAtoms, finalCWA, finalOWA, probabilisticAtoms, triStateAtoms, evidence.identities,
      atomStateDB, evidence.orderedStartIDs, evidence.orderedAtomSignatures, evidence.queryStartID,
      evidence.queryEndID)
  }


  def learning(mlnFileName: String,
            trainingFileNames: List[String],
            nonEvidenceAtoms: collection.Set[AtomSignature],
            pcm: PredicateCompletionMode = Decomposed,
            dynamicDefinitions: Option[ImplFinder.ImplementationsMap] = None): (MLN, Map[AtomSignature, AtomEvidenceDB]) = {

    info(
      "Stage 0: Loading an MLN instance from data..." +
        "\n\tInput MLN file: " + mlnFileName +
        "\n\tInput training file(s): " + (if (trainingFileNames.nonEmpty) trainingFileNames.reduceLeft(_+", "+_) else ""))

    //parse knowledge base (.mln)
    val kb = KB(mlnFileName, pcm, dynamicDefinitions)

    val atomSignatures: collection.Set[AtomSignature] = kb.predicateSchema.keySet

    /**
     * Check if the schema of all Non-Evidence atoms is defined in the MLN file
     */
    nonEvidenceAtoms.find(s => !atomSignatures.contains(s)) match {
      case Some(x) => fatal("The predicate " + x + " that appears in the query, is not defined in the mln file.")
      case None => // do nothing
    }

    val evidenceAtoms = atomSignatures -- nonEvidenceAtoms

    //parse the evidence database (.db)
    val evidence: Evidence = Evidence(kb, Set.empty[AtomSignature], Set.empty[AtomSignature], trainingFileNames)

    var functionMapperz = evidence.functionMappers
    for ((signature, func) <- kb.dynamicFunctions) {
      functionMapperz += (signature -> FunctionMapper(func))
    }

    var (annotationDB, atomStateDB) = evidence.atomsEvDB.partition(e => nonEvidenceAtoms.contains(e._1))

    for (signature <- annotationDB.keysIterator)
      atomStateDB += (signature -> AtomEvidenceDB(evidence.identities(signature),UNKNOWN))//, kb.predicateSchema(signature).zipWithIndex.toMap, UNKNOWN))

    for (signature <- nonEvidenceAtoms; if !annotationDB.contains(signature)){
      warn("Annotation was not given in the training file(s) for predicate '"+signature+"', assuming FALSE state for all its groundings.")
      annotationDB += (signature -> AtomEvidenceDB(evidence.identities(signature),FALSE))//, kb.predicateSchema(signature).zipWithIndex.toMap, FALSE))
    }

    val probabilisticAtoms = Set.empty[AtomSignature]
    val queryAtoms = nonEvidenceAtoms
    val finalCWA = evidenceAtoms
    val finalOWA = nonEvidenceAtoms
    val triStateAtoms = atomStateDB.filter(db => db._2.isTriStateDB).map(_._1).toSet // is required for grounding

    (new MLN(kb.formulas, kb.predicateSchema, kb.functionSchema, kb.dynamicPredicates, kb.dynamicFunctions,
      evidence.constants, functionMapperz, queryAtoms, finalCWA, finalOWA, probabilisticAtoms, triStateAtoms, evidence.identities,
      atomStateDB, evidence.orderedStartIDs, evidence.orderedAtomSignatures, evidence.queryStartID,
      evidence.queryEndID), annotationDB)
  }

}
