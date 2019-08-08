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

import java.io.{ BufferedReader, File, FileReader }
import lomrf.logic._
import lomrf.logic.parser.EvidenceParser
import com.typesafe.scalalogging.Logger
import lomrf.mln.model.builder.{ ConstantsDomainBuilder, EvidenceBuilder }
import lomrf.util.logging.Implicits._
import scala.util.Try

/**
  * Evidence holds the constants for each domain name. An atom database for each atom
  * signature that contains the truth values of its groundings. A function mapper for
  * each function signature that maps tuples of constants to a single return value.
  *
  * @note Function signatures are represented using [[lomrf.logic.AtomSignature]].
  *
  * @see [[lomrf.mln.model.ConstantsSet]]
  * @see [[lomrf.mln.model.FunctionMapper]]
  * @see [[lomrf.mln.model.AtomEvidenceDB]]
  *
  * @param constants a map from domain names to constant sets
  * @param db a map from atom signatures to atom evidence database
  * @param functionMappers a map from atom signatures to function mapper
  */
case class Evidence(
    constants: ConstantsDomain,
    db: EvidenceDB,
    functionMappers: FunctionMappers) {

  /** Collection of tri-state atoms , i.e., open-world assumption with some evidence */
  lazy val triStateAtoms: Set[AtomSignature] = db.filter(_._2.isTriStateDB).keySet

  /** Collection of probabilistic atom signatures */
  lazy val probabilisticAtoms: Set[AtomSignature] = db.filter(_._2.isProbabilistic).keySet

  /** Collection of close world assumption atom signatures */
  lazy val cwaAtoms: Set[AtomSignature] = db.filter(_._2.numberOfUnknown == 0).keySet

  /** Collection of open world assumption atom signatures */
  lazy val owaAtoms: Set[AtomSignature] = db.keySet -- cwaAtoms
}

object Evidence {

  private lazy val logger = Logger(this.getClass)

  /**
    * Parse evidence from a collection of files.
    *
    * @see [[lomrf.mln.model.ConstantsSet]]
    *
    * @param kb a knowledge base
    * @param constants a map from domain names to constant sets
    * @param queryPredicates a set of query atom signature
    * @param files an iterable of evidence files (.db files)
    * @param convertFunctions convert functions to auxiliary predicates
    * @param forceCWAForAll force close world assumption for ground atoms not present in the evidence files
    * @return an Evidence instance
    */
  def fromFiles(
      kb: KB,
      constants: ConstantsDomain,
      queryPredicates: Set[AtomSignature],
      files: Iterable[File],
      convertFunctions: Boolean,
      forceCWAForAll: Boolean): Evidence = {

    fromFiles(
      kb.predicateSchema,
      kb.functionSchema,
      constants,
      kb.dynamicFunctions,
      queryPredicates,
      Set.empty[AtomSignature],
      Set.empty[AtomSignature],
      files,
      convertFunctions,
      forceCWAForAll)
  }

  /**
    * Parse evidence from a collection of files.
    *
    * @see [[lomrf.mln.model.ConstantsSet]]
    *
    * @param kb a knowledge base
    * @param constants a map from domain names to constant sets
    * @param queryPredicates a set of query atom signature
    * @param owaPredicates a set of atom signatures subject to open world assumption
    * @param cwaPredicates a set of atom signatures subject to close world assumption
    * @param files an iterable of evidence files (.db files)
    * @param convertFunctions convert functions to auxiliary predicates
    * @param forceCWAForAll force close world assumption for ground atoms not present in the evidence
    * @return an Evidence instance
    */
  def fromFiles(
      kb: KB,
      constants: ConstantsDomain,
      queryPredicates: Set[AtomSignature],
      owaPredicates: Set[AtomSignature],
      cwaPredicates: Set[AtomSignature],
      files: Iterable[File],
      convertFunctions: Boolean,
      forceCWAForAll: Boolean): Evidence = {

    fromFiles(
      kb.predicateSchema,
      kb.functionSchema,
      constants,
      kb.dynamicFunctions,
      queryPredicates,
      owaPredicates,
      cwaPredicates,
      files,
      convertFunctions,
      forceCWAForAll)
  }

  /**
    * Parse evidence from a collection of files.
    *
    * @note Function signatures are represented using [[lomrf.logic.AtomSignature]].
    *
    * @see [[lomrf.mln.model.ConstantsSet]]
    *
    * @param predicateSchema a map from atom signatures to their argument domain names
    * @param functionSchema a map from function signatures to their return value domain name, argument domain names
    * @param constants a map from domain names to constant sets
    * @param dynamicFunctions a map from function signatures to functions of the form '''Vector[String] => String'''
    * @param queryPredicates a set of query atom signature
    * @param owaPredicates a set of atom signature subject to open world assumption
    * @param files an iterable of evidence files (.db files)
    * @param convertFunctions convert functions to auxiliary predicates
    * @param forceCWAForAll force close world assumption for ground atoms not present in the evidence
    * @return an Evidence instance
    */
  def fromFiles(
      predicateSchema: PredicateSchema,
      functionSchema: FunctionSchema,
      constants: ConstantsDomain,
      dynamicFunctions: DynamicFunctions,
      queryPredicates: Set[AtomSignature],
      owaPredicates: Set[AtomSignature],
      files: Iterable[File],
      convertFunctions: Boolean,
      forceCWAForAll: Boolean): Evidence = {

    fromFiles(
      predicateSchema,
      functionSchema,
      constants,
      dynamicFunctions,
      queryPredicates,
      owaPredicates,
      Set.empty,
      files,
      convertFunctions,
      forceCWAForAll)
  }

  /**
    * Parse evidence from a collection of files.
    *
    * @note Function signatures are represented using [[lomrf.logic.AtomSignature]].
    *
    * @see [[lomrf.mln.model.ConstantsSet]]
    *
    * @param predicateSchema a map from atom signatures to their argument domain names
    * @param functionSchema a map from function signatures to their return value domain name, argument domain names
    * @param constantsDomain a map from domain names to constant sets
    * @param dynamicFunctions a map from function signatures to functions of the form '''Vector[String] => String'''
    * @param queryPredicates a set of query atom signature
    * @param owaPredicates a set of atom signature subject to open world assumption
    * @param cwaPredicates a set of atom signature subject to close world assumption
    * @param files an iterable of evidence files (.db files)
    * @param convertFunctions convert functions to auxiliary predicates
    * @param forceCWAForAll force close world assumption for ground atoms not present in the evidence
    * @return an Evidence instance
    */
  def fromFiles(
      predicateSchema: PredicateSchema,
      functionSchema: FunctionSchema,
      constantsDomain: ConstantsDomain,
      dynamicFunctions: DynamicFunctions,
      queryPredicates: Set[AtomSignature],
      owaPredicates: Set[AtomSignature],
      cwaPredicates: Set[AtomSignature],
      files: Iterable[File],
      convertFunctions: Boolean,
      forceCWAForAll: Boolean): Evidence = {

    val inputFiles =
      if (files.isEmpty) {
        logger.warn("Loading from empty evidence.")
        List(createTempEmptyDBFile)
      } else {
        // Check if we can read the specified files
        files.foreach { f =>
          if (!f.exists || !f.isFile || !f.canRead)
            logger.fatal(s"Cannot read input evidence file '${f.getPath}'")
        }
        files
      }

    val evidenceParser = new EvidenceParser
    val evidenceExpressionsDB =
      for (file <- inputFiles; fileReader = new BufferedReader(new FileReader(file)))
        yield evidenceParser.parseAll(evidenceParser.evidence, fileReader) match {
        case evidenceParser.Success(expr: List[EvidenceExpression], _) => expr
        case x => logger.fatal(s"Can't parse the following expression: '$x' in file: '${file.getPath}'")
      }

    logger.info("--- Stage 1: Parsing constants")
    val constantsDomainBuilder = ConstantsDomainBuilder.from(constantsDomain)

    var evidenceSignatures = cwaPredicates

    for (evidenceExpressions <- evidenceExpressionsDB; expr <- evidenceExpressions) expr match {
      case f: FunctionMapping =>
        // Collect information for function mappings
        val (returnType, argTypes) = functionSchema
          .getOrElse(f.signature, logger.fatal(s"Function definition '${f.signature}' does not appear in the knowledge base."))

        val builder = constantsDomainBuilder
          .getOrElse(returnType, logger.fatal(s"Type '$returnType' in function '${f.signature}' is not defined."))

        builder += f.retValue

        for ((argType, argValue) <- argTypes.zip(f.values)) {
          val currBuilder = constantsDomainBuilder
            .getOrElse(argType, logger.fatal(s"Type '$argType' in function '${f.signature}' is not defined."))
          currBuilder += argValue
        }

      case atom: EvidenceAtom =>
        val argTypes = predicateSchema
          .getOrElse(atom.signature, logger.fatal(s"Unknown predicate '$atom' in the given input evidence file(s)."))

        evidenceSignatures += atom.signature

        // Append its constants into constants map
        for ((value, index) <- atom.terms.view.zipWithIndex) {
          val constantType = argTypes(index)

          constantsDomainBuilder.get(constantType) match {
            case Some(x) => x += value.symbol
            case None    => constantsDomainBuilder += (constantType -> value.symbol)
          }
        }
      case _ => // ignore
    }

    val constants: Map[String, ConstantsSet] = constantsDomainBuilder.result()

    logger.info("--- Stage 2: Inferring predicate open/closed-world assumptions.")
    val allSignatures = predicateSchema.keySet
    val userDefinedOWA = queryPredicates ++ owaPredicates

    val inferredCWASignatures = evidenceSignatures -- userDefinedOWA
    val inferredOWASignatures = allSignatures -- inferredCWASignatures

    val inferredHiddenSignatures = inferredOWASignatures -- queryPredicates

    logger.info(
      s"""
         |\t\tOWA signatures: ${inferredOWASignatures.mkString(", ")}
         |\t\tCWA signatures: ${inferredCWASignatures.mkString(", ")}
      """.stripMargin)

    logger.info("--- Stage 3: Creating function mappings, and evidence database.")

    val builder = EvidenceBuilder(
      predicateSchema,
      functionSchema,
      queryPredicates,
      inferredHiddenSignatures,
      constants,
      convertFunctions)
      .withDynamicFunctions(dynamicFunctions)
      .withCWAForAll(forceCWAForAll)

    for (evidenceExpressions <- evidenceExpressionsDB; expr <- evidenceExpressions) expr match {
      case fm: FunctionMapping => builder.functions += fm
      case atom: EvidenceAtom  => builder.evidence += atom
    }

    builder.result()
  }

  private def createTempEmptyDBFile: File = {
    val prefix = s".mln_empty_${System.currentTimeMillis}"
    Try[File](File.createTempFile(prefix, ".db")) map { tmpFile =>
      tmpFile.deleteOnExit()
      tmpFile
    } getOrElse {
      logger.fatal(s"Cannot create temporary file '$prefix.db' in the default JVM temporary file directory '${sys.props.get("java.io.tmpdir")}' (see JVM parameter 'java.io.tmpdir').")
    }
  }
}
