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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.mln.model

import java.io.{BufferedReader, File, FileReader}

import auxlib.log.Logger
import lomrf.logic._
import lomrf.logic.parser.EvidenceParser

import scala.util.Try
import scala.collection.breakOut

class Evidence(val constants: ConstantsDomain,
               val db: EvidenceDB,
               val functionMappers: FunctionMappers){

  /**
   * collection of atoms with tri-state, i.e., open-world assumption (unknown state) with some evidence (true/false state)
   */
  lazy val triStateAtoms: Set[AtomSignature] = db.filter(x => x._2.isTriStateDB).keySet

  /**
   * collection of probabilistic atoms
   */
  lazy val probabilisticAtoms: Set[AtomSignature] = db.filter(x => x._2.isProbabilistic).keySet

  lazy val cwaAtoms: Set[AtomSignature] =
    (for((signature, edb) <- db; if edb.numberOfUnknown == 0) yield signature)(breakOut)

  lazy val owaAtoms = db.keySet -- cwaAtoms

}

object Evidence {

  private lazy val log = Logger(this.getClass)

  def apply(constants: ConstantsDomain, db: EvidenceDB, fm: FunctionMappers): Evidence = {
    new Evidence(constants, db, fm)
  }


  def fromFiles(kb: KB,
                constantsDomain: ConstantsDomain,
                queryPredicates: Set[AtomSignature],
                files: Iterable[File],
                convertFunctions: Boolean,
                forceCWAForAll: Boolean): Evidence = {

    fromFiles(kb.predicateSchema, kb.functionSchema, constantsDomain, kb.dynamicFunctions, queryPredicates, Set.empty[AtomSignature], Set.empty[AtomSignature], files, convertFunctions, forceCWAForAll)
  }

  def fromFiles(kb: KB,
                constantsDomain: ConstantsDomain,
                queryPredicates: Set[AtomSignature],
                owaPredicates: Set[AtomSignature],
                cwaPredicates: Set[AtomSignature],
                files: Iterable[File],
                convertFunctions: Boolean,
                forceCWAForAll: Boolean): Evidence = {

    fromFiles(kb.predicateSchema, kb.functionSchema, constantsDomain, kb.dynamicFunctions, queryPredicates, owaPredicates, cwaPredicates, files, convertFunctions, forceCWAForAll)
  }

  def fromFiles(predicateSchema: PredicateSchema,
                functionSchema: FunctionSchema,
                constantsDomain: ConstantsDomain,
                dynamicFunctions: DynamicFunctions,
                queryPredicates: Set[AtomSignature],
                owaPredicates: Set[AtomSignature],
                files: Iterable[File],
                convertFunctions: Boolean,
                forceCWAForAll: Boolean): Evidence ={

    fromFiles(predicateSchema, functionSchema, constantsDomain, dynamicFunctions, queryPredicates, owaPredicates, Set.empty, files, convertFunctions, forceCWAForAll)
  }

  def fromFiles(predicateSchema: PredicateSchema,
                functionSchema: FunctionSchema,
                constantsDomain: ConstantsDomain,
                dynamicFunctions: DynamicFunctions,
                queryPredicates: Set[AtomSignature],
                owaPredicates: Set[AtomSignature],
                cwaPredicates: Set[AtomSignature],
                files: Iterable[File],
                convertFunctions: Boolean,
                forceCWAForAll: Boolean): Evidence = {


    import log._


    val inputFiles =
      if(files.isEmpty) {
        log.warn("Loading from empty evidence")
        List(createTempEmptyDBFile)
      } else {
        // check if we can read the specified files
        files.foreach(f => if(!f.exists() || !f.isFile || !f.canRead) fatal(s"Cannot read input evidence file '${f.getPath}'"))

        files
      }

    val evidenceParser = new EvidenceParser
    val evidenceExpressionsDB =
      for (file <- inputFiles; fileReader = new BufferedReader(new FileReader(file)))
        yield evidenceParser.parseAll(evidenceParser.evidence, fileReader) match {
          case evidenceParser.Success(expr, _) => expr
          case x => fatal(s"Can't parse the following expression: '$x' in file: '${file.getPath}'")
        }

    info("--- Stage 1: Parsing constants")
    val constantsDomainBuilder = ConstantsDomainBuilder.from(constantsDomain)

    var evidenceSignatures = cwaPredicates

    for (evidenceExpressions <- evidenceExpressionsDB; expr <- evidenceExpressions) expr match {
      case f: FunctionMapping =>
        //Collect information for functionMappings
        val (returnType, argTypes) = functionSchema.getOrElse(
          f.signature,
          fatal(s"The function definition of '${f.signature}' does not appear in the knowledge base."))

        val builder = constantsDomainBuilder.getOrElse(
          returnType,
          fatal(s"Type '$returnType' in function '${f.signature}' is not defined."))

        builder += f.retValue

        for ((argType, argValue) <- argTypes.zip(f.values)){
          val currBuilder = constantsDomainBuilder.getOrElse(
            argType,
            fatal(s"Type '$argType' in function '${f.signature}' is not defined."))

          currBuilder += argValue //.symbol
        }

      case atom: EvidenceAtom =>
        val argTypes = predicateSchema.getOrElse(atom.signature, fatal(s"Unknown predicate '$atom' in the given input evidence file(s)."))

        evidenceSignatures += atom.signature

        //append its constants into constantsMap
        for ((value, index) <- atom.terms.view.zipWithIndex) {
          val constantType = argTypes(index)

          constantsDomainBuilder.get(constantType) match {
            case Some(x) => x += value.symbol
            case None =>
              constantsDomainBuilder += (constantType -> value.symbol)
          }
        }
      case _ => //ignore
    }

    val constants: Map[String, ConstantsSet] = constantsDomainBuilder.result()

    info("--- Stage 2: Inferring predicate open/closed-world assumptions.")
    val allSignatures = predicateSchema.keySet
    val userDefinedOWA = queryPredicates ++ owaPredicates

    val inferredCWASignatures = evidenceSignatures -- userDefinedOWA
    val inferredOWASignatures = allSignatures -- inferredCWASignatures

    val inferredHiddenSignatures = inferredOWASignatures -- queryPredicates

    info(
      s"""
        |\t\tOWA predicate signatures: ${inferredOWASignatures.mkString(", ")}
        |\t\tCWA predicate signatures: ${inferredCWASignatures.mkString(", ")}
      """.stripMargin)
    
    info("--- Stage 3: Creating function mappings, and evidence database.")

    val builder =
      EvidenceBuilder(predicateSchema, functionSchema, queryPredicates, inferredHiddenSignatures, constants, convertFunctions).
        withDynamicFunctions(dynamicFunctions).
        withCWAForAll(forceCWAForAll)

    for (evidenceExpressions <- evidenceExpressionsDB; expr <- evidenceExpressions) expr match {
      case fm: FunctionMapping => builder.functions += fm
      case atom: EvidenceAtom => builder.evidence += atom
    }

    builder.result()
  }

  private def createTempEmptyDBFile: File = {
    import log._
    val filePrefix = s".mlnc_empty_${System.currentTimeMillis()}"

    Try[File](File.createTempFile(filePrefix, ".db")) map {
      tmpFile =>
        tmpFile.deleteOnExit()
        tmpFile
    } getOrElse{
      fatal(s"Cannot create temporary file '$filePrefix.db' in the default JVM temporary file directory '${sys.props.get("java.io.tmpdir")}' (see JVM parameter 'java.io.tmpdir').")
    }

  }

}
