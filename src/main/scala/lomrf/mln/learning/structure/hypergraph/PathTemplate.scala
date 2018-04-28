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

package lomrf.mln.learning.structure.hypergraph

import com.typesafe.scalalogging.{LazyLogging, Logger}
import lomrf.logic.AtomSignatureOps._
import lomrf.logic._
import lomrf.util.logging.Implicits._
import lomrf.mln.model.{EvidenceDB, MLN}
import lomrf.util.collection.mutable.TuplesDB

import scala.util.{Failure, Success, Try}

/**
 * Every path template is defined over a combination of domains for the template
 * atoms. Therefore, axioms over template atoms having the same combinations of
 * domains should belong to the same path template. For example, templatePredicate_1(X, Y)
 * and templatePredicate_2(X, Y) should belong to the same path template.
 *
 * @param schema the domains combination of the template atoms. For example (Person, Location)
 */
final class PathTemplate private[hypergraph](val schema: Seq[String]) extends LazyLogging {

  private var templatePreds = Vector[AtomSignature]()
  private var axioms = Vector[Clause]()
  private val tuplesDB = new TuplesDB[Variable, String]()

  def templateAtoms = templatePreds

  /**
   * Insert an axiom along with the template atom signature it contains. The
   * axiom should contain exactly one negated (in CNF) template atom an at
   * least one non evidence atom.
   *
   * @param clause the specified axiom
   * @param templateAtom the template atom belonging to the specified atom
   */
  private[hypergraph] def +(clause: Clause, templateAtom: AtomSignature) = {
    axioms :+= clause
    templatePreds :+= templateAtom
  }

  /**
   * Find tuples of constants for the variables of a specified literal that also belong to a template
   * atom, for which the ground literal is TRUE according to the given evidence. Function are not supported
   * as part of the literals.
   *
   * @param literal the literal
   * @param templateAtomVariables variables belonging to a template atom
   * @param mln Markov logic network of the current problem
   *
   * @return a map containing for each variable a vector of constants. The vectors should be of equal length in order
   *         to create tuples.
   */
  private def getTRUETuplesOfConstants(literal: Literal, templateAtomVariables: Set[Variable])(implicit mln: MLN): Map[Variable, Vector[String]] = {

    val atom = literal.sentence
    val signature = atom.signature
    val evidenceDB = mln.evidence.db(signature)

    /*
     * If literal is positive then we only search constant for which the atom
     * state is false, otherwise true. That is because literals belong to a clause,
     * therefore belong to a CNF formula.
     */
    val state = literal.positive match {
      case true => FALSE
      case false => TRUE
    }

    var variablesPositions = Vector[Int]()
    var queries = Vector[Vector[String]]()

    // For all terms in the atom
    for(idx <- atom.terms.indices) {
      val term = atom.terms(idx)
      term match {

        case constant: Constant =>

          if(queries.nonEmpty) queries = for(tuple <- queries) yield tuple :+ constant.symbol
          else queries :+= Vector(constant.symbol)

        case variable: Variable =>

          var tempQueries = Vector[Vector[Vector[String]]]()
          val constantSet = mln.getConstantValuesOf(variable.domain)
            .getOrElse(logger.fatal(s"Constant set does not exist for domain ${variable.domain}"))

          if(templateAtomVariables.contains(variable)) variablesPositions :+= idx

          val iterator = constantSet.valuesIterator
          while(iterator.hasNext) {
            iterator.advance()
            if(queries.nonEmpty) tempQueries :+= (for(tuple <- queries) yield tuple :+ iterator.key)
            else tempQueries :+= Vector(Vector(iterator.key))
          }
          queries = tempQueries.flatMap(x => x)

        case function: TermFunction => logger.fatal("Functions are not supported!")
      }
    }

    // keep only tuples being TRUE in the evidence
    queries = queries.filter(q => evidenceDB(q) == state)
    variablesPositions.map(idx => atom.terms(idx).asInstanceOf[Variable] -> queries.map(tuple => tuple(idx))).toMap
  }

  /**
   * For each dependant literal (literal having variables also belonging to a template atom) find all
   * tuples of constants for these variables and keep the intersection for all literal constant
   * combinations.
   *
   * @param dependencyLiterals a set of dependency literals
   * @param templateAtomVariables a set of the variables belonging to the template atom
   * @param mln Markov logic network of the current problem
   *
   * @return all valid constant combinations
   */
  private def findValidTuples(dependencyLiterals: Set[Literal], templateAtomVariables: Set[Variable],
                              variablesOrdering: Vector[Variable])(implicit mln: MLN): Set[Vector[String]] = {

    var validConstantsPerLiteral = List[Map[Variable, Vector[String]]]()

    dependencyLiterals.foreach { literal =>
      validConstantsPerLiteral = getTRUETuplesOfConstants(literal, templateAtomVariables) :: validConstantsPerLiteral
    }

    validConstantsPerLiteral.foreach { tuple => tuplesDB allowOnly tuple }

    tuplesDB.getOrderedTuples(variablesOrdering).toSet
  }

  /**
   * Produce every possible initial set of constants (constants satisfying the axioms) for
   * the specified non evidence misclassified atom.
   *
   * @param atomID the non evidence misclassidied atom id
   * @param annotationDB the annotation database
   * @param mln Markov Logic network
   *
   * @return a set of initial constant set
   */
  def extractValidConstants(atomID: Int, annotationDB: EvidenceDB)(implicit mln: MLN): /*Set[Set[String]]*/ Set[Int] = {

    var initialTemplateAtomsIds = Set[Int]()
    //var initialConstants = Set[Set[String]]()

    val signature = atomID.signature(mln)
    val annotation = annotationDB(signature)

    /*
     * Use only axioms having a positive literal of the non evidence atom if the non evidence atom
     * is TRUE in the data, else only axioms having the non evidence atom as negated literal.
     */
    (axioms zip templatePreds)
      .filter(t => t._1.literals.exists(l => l.sentence.signature == signature && l.positive == (annotation(atomID) == TRUE)))
      .foreach { case (axiom, templateSignature) =>

        tuplesDB.clear()

        // template atom identity function
        val templateIdf = mln.evidence.db(templateSignature).identity

        // substitution of non evidence predicate constant in the axiom
        val constants = mln.evidence.db(signature).identity.decode(atomID).get.map(str => new Constant(str))
        val lit = axiom.literals.find(lit => lit.sentence.signature == signature).get
        val theta: Theta = (lit.sentence.variables zip constants).toMap
        val substitutedAxiom = axiom.substitute(theta)
        logger.debug(s"Substituted axiom ${substitutedAxiom.toText()}")

        // add theta to database
        val templateBeforeSub = axiom.literals.find(_.sentence.signature == templateSignature).get.sentence
        theta.foreach {
          case (v: Variable, c: Constant) =>
            if (templateBeforeSub.variables.contains(v)) tuplesDB += (v, c.symbol)
          case _ =>
        }

        val templateAtom = substitutedAxiom.literals.find(_.sentence.signature == templateSignature).get.sentence

        if (templateAtom.variables.nonEmpty) templateAtom.variables.foreach { v =>
          val dependencyLits = substitutedAxiom.literals.filter(l => l.sentence.signature != templateSignature && l.sentence.variables.contains(v))
          logger.debug("Dependency literals: " + dependencyLits.map(_.toText).mkString(", "))

          initialTemplateAtomsIds ++=
            findValidTuples(dependencyLits, templateAtom.variables, templateBeforeSub.terms.asInstanceOf[Vector[Variable]]).map(constants => templateIdf.encode(constants))

          //initialConstants ++= findValidTuples(dependencyLits, templateAtom.variables, templateBeforeSub.terms.asInstanceOf[Vector[Variable]]).map
        }
        else initialTemplateAtomsIds += templateIdf.encode(templateAtom.terms.map(_.symbol)) //initialConstants += templateAtom.constants.map(_.symbol)

        //info(s"$initialConstants")
        import lomrf.mln.model.AtomIdentityFunctionOps._
        logger.debug(s"${initialTemplateAtomsIds.map(_.decodeAtom).mkString(", ")}")
      }

    initialTemplateAtomsIds
    //initialConstants
  }

  /**
   * @return true if the specified template signature is contained in the path template, otherwise false
   */
  def contains(template: AtomSignature): Boolean = templatePreds.contains(template)

  /**
   * Check for equality of path templates using their schema. Also allows equality checking
   * of a path template directly with a given schema.
   */
  override def equals(other: Any): Boolean = other match {
    case schema: Seq[String] => schema == this.schema
    case obj: PathTemplate => obj.schema == this.schema
    case _ => false
  }

  override def toString: String = s"[ ${schema.mkString(" | ")} ] :\n\t" +
                          s"${(axioms.map(_.toText(weighted = false)) zip templatePreds)
                            .map{ case (axiom, template) => s"$axiom -> $template" }.mkString("\n\t")}"
}

/**
 * Path template object
 */
object PathTemplate {

  private lazy val logger = Logger(this.getClass)

  /**
   * Create path templates from a set of axioms given the template atoms and the
   * non evidence atoms. The axioms should have at least one non evidence atom and
   * exactly one template atom.
   *
   * @param axioms a set of axioms
   * @param templateAtoms a set of template atoms
   * @param nonEvidenceAtoms a set of non evidence atoms
   *
   * @return a set of path templates if any exist, otherwise returns none
   */
  def create(axioms: Set[Clause], templateAtoms: Set[AtomSignature],
             nonEvidenceAtoms: Set[AtomSignature]): Try[Option[Set[PathTemplate]]] = {

    /**
     * @return true if the given literal is a template atom, false otherwise
     */
    def isTemplateAtom(literal: Literal) = templateAtoms.contains(literal.sentence.signature)

    // If axioms or template atoms are empty, then template atoms does not exist
    if (axioms.isEmpty || templateAtoms.isEmpty) return Success(None)

    var pathTemplates = Set[PathTemplate]()

    // Checks if axioms exist having more than one template atoms
    if ( axioms.exists(_.literals.count(isTemplateAtom) > 1) )
      return Failure(new UnsupportedOperationException("Formula exists having more than one template literals!"))

    // Checks if there is an axiom defined having a positive literal for each template atom
    templateAtoms.foreach { signature =>
      if( !axioms.exists(_.literals.exists(literal => literal.sentence.signature == signature && !literal.positive)) )
        return Failure(new UnsupportedOperationException(s"Positive literal does not exist in any formula for template atom $signature"))
    }

    axioms.foreach { axiom =>

      val axiomSignatures =  axiom.literals.map(_.sentence.signature)
      val templateLiteral = axiom.literals.find(isTemplateAtom).get
      val axiomTemplateSignature = templateLiteral.sentence.signature

      // Check if there is at least one non evidence atom in the current axiom
      if (axiomSignatures.intersect(nonEvidenceAtoms).isEmpty)
        return Failure(new UnsupportedOperationException(
          s"Formula ${axiom.toText()} does not contain any non evidence atoms!"))

      // Check if the template literal in the axiom is negated, because of CNF. If not, ignore it!
      if (!templateLiteral.positive) {
        val schema = templateLiteral.sentence.variables.toSeq.map(_.domain)

        pathTemplates.find(_ == schema) match {
          case Some(template) => template + (axiom, axiomTemplateSignature)

          case None =>
            val template = new PathTemplate(schema)
            template + (axiom, axiomTemplateSignature)
            pathTemplates += template
        }
      }

    }

    logger.info(s"Path templates extracted:\n\t${pathTemplates.mkString("\n\t")}")
    Success(Some(pathTemplates))
  }

}