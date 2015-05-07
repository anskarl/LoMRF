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

import java.io.{File, BufferedReader, FileReader}
import auxlib.log.Logger
import lomrf.logic._
import lomrf.util._
import scala.util.{Failure, Try, Success}


class Evidence(val constants: ConstantsDomain,
               val db: EvidenceDB,
               val functionMappers: FunctionMappers,
               val domainSpace: DomainSpace){

  /**
   * collection of atoms with tri-state, i.e., open-world assumption (unknown state) with some evidence (true/false state)
   */
  lazy val tristateAtoms: Set[AtomSignature] = db.filter(x => x._2.isTriStateDB).keySet

  /**
   * probabilisticAtoms collection of probabilistic atoms
   */
  lazy val probabilisticAtoms: Set[AtomSignature] = db.filter(x => x._2.isProbabilistic).keySet

}


object Evidence {

  private lazy val log = Logger(this.getClass)

  def apply(constants: ConstantsDomain, db: EvidenceDB, fm: FunctionMappers, space: DomainSpace): Evidence = {
    new Evidence(constants, db, fm, space)
  }

  //@deprecated
  /*def apply(kb: KB, constantsDomainBuilders: ConstantsDomainBuilder, queryPredicates: Set[AtomSignature], hiddenPredicates: Set[AtomSignature], filenames: List[String]): Evidence = {
    val fileList = if (filenames.isEmpty) List(createTempEmptyDBFile) else filenames.map(filename => new File(filename))

    fromFiles(kb, constantsDomainBuilders, queryPredicates, hiddenPredicates, fileList)
  }*/

  /*@deprecated
  def apply(kb: KB, constantsDomainBuilders: ConstantsDomainBuilder, queryPredicates: Set[AtomSignature], hiddenPredicates: Set[AtomSignature], filenameOpt: Option[String] = None): Evidence = {

    val evidenceFile = filenameOpt match {
      case Some(filename) if filename.trim != "" => new File(filename)
      case _ => createTempEmptyDBFile
    }

    if(!evidenceFile.exists())
      log.fatal(s"Evidence file '${evidenceFile.getPath}' does not exists.")

    fromFiles(kb, constantsDomainBuilders, queryPredicates, hiddenPredicates, List(evidenceFile))
  }*/

  def fromFiles(kb: KB, constantsDomainBuilders: ConstantsDomainBuilder, queryPredicates: Set[AtomSignature], hiddenPredicates: Set[AtomSignature], filenames: List[String]): Evidence ={
    val fileList = if (filenames.isEmpty) List(createTempEmptyDBFile) else filenames.map(filename => new File(filename))

    fromFiles(kb, constantsDomainBuilders, queryPredicates, hiddenPredicates, fileList)
  }


  def fromFiles(kb: KB, constantsDomainBuilders: ConstantsDomainBuilder, queryPredicates: Set[AtomSignature], hiddenPredicates: Set[AtomSignature], files: Iterable[File]): Evidence = {
    fromFiles(kb.predicateSchema, kb.functionSchema, constantsDomainBuilders, kb.dynamicFunctions, queryPredicates, hiddenPredicates, files)
  }

  def fromFiles(predicateSchema: PredicateSchema,
                functionSchema: FunctionSchema,
                constantsDomainBuilder: ConstantsDomainBuilder,
                dynamicFunctions: DynamicFunctions,
                queryPredicates: Set[AtomSignature],
                hiddenPredicates: Set[AtomSignature],
                files: Iterable[File]): Evidence = {


    import log._


    def isOWA(signature: AtomSignature) = queryPredicates.contains(signature) || hiddenPredicates.contains(signature)

    val evidenceParser = new EvidenceParser
    val evidenceExpressionsDB =
      for (file <- files; fileReader = new BufferedReader(new FileReader(file)))
        yield evidenceParser.parseAll(evidenceParser.evidence, fileReader) match {
          case evidenceParser.Success(expr, _) => expr
          case x => fatal(s"Can't parse the following expression: '$x' in file: '${file.getPath}'")
        }

    info("--- Stage 1: Parsing constants")

    for (evidenceExpressions <- evidenceExpressionsDB; expr <- evidenceExpressions) expr match {
      case f: FunctionMapping =>
        //Collect information for functionMappings
        functionSchema.get(f.signature) match {
          case Some(fSchema) =>
            val (returnType, argTypes) = fSchema
            constantsDomainBuilder.get(returnType) match {
              case Some(builder) => builder += f.retValue
              case None => error(s"Type '$returnType' in function '${f.signature}' is not defined.")
            }
            for ((argType, argValue) <- argTypes.zip(f.values)) {
              constantsDomainBuilder.get(argType) match {
                case Some(builder) => builder += argValue
                case None => error(s"Type '$returnType' in function '${f.signature}' is not defined.")
              }
            }
          case None => fatal(s"The function definition of '${f.signature}' does not appear in the knowledge base.")
        }
      case atom: EvidenceAtom =>
        predicateSchema.get(atom.signature) match {
          case Some(argTypes) =>
            //append its constants into constantsMap
            for ((value, index) <- atom.constants.view.zipWithIndex) {
              val constantType = argTypes(index)
              constantsDomainBuilder.get(constantType) match {
                case Some(x) => x += value.symbol
                case None =>
                  /*val currMap = new ConstantsSetBuilder()
                  currMap += value.symbol*/
                  constantsDomainBuilder += (constantType -> value.symbol)
              }
            }
          case _ => fatal(s"The type of '$atom' is not defined.")
        }
      case _ => //ignore
    }

    val constants: Map[String, ConstantsSet] = constantsDomainBuilder.result()

    info("--- Stage 2: Computing domain space.")

    val domainSpace = DomainSpace(predicateSchema, queryPredicates, hiddenPredicates, constants)


    info("--- Stage 3: Creating function mappings, and evidence database.")
    var functionMapperBuilders = Map[AtomSignature, FunctionMapperBuilder]()

    var atomsEvDBBuilders = Map[AtomSignature, AtomEvidenceDBBuilder]()


    //var currentAtomStartID = 1
    for (evidenceExpressions <- evidenceExpressionsDB; expr <- evidenceExpressions) expr match {
      case fm: FunctionMapping =>
        functionMapperBuilders.get(fm.signature) match {
          case Some(fMappingBuilder) => fMappingBuilder +=(fm.values, fm.retValue)
          case None =>
            val idFunction = AtomIdentityFunction(fm.signature, functionSchema(fm.signature)._2, constants, 1)
            val builder = new FunctionMapperBuilder(idFunction)
            builder +=(fm.values, fm.retValue)
            functionMapperBuilders += (fm.signature -> builder)
        }
      case atom: EvidenceAtom =>
        atomsEvDBBuilders.get(atom.signature) match {
          case Some(builder) => builder += atom
          case None =>
            val signature = atom.signature
            val atomSchema = predicateSchema(signature)
            val db = AtomEvidenceDBBuilder(signature, atomSchema, domainSpace.identities(signature), !isOWA(signature))
            db += atom
            atomsEvDBBuilders += (signature -> db)
        }
      case _ => //ignore
    }

    //val atomsEvDB = atomsEvDBBuilders.mapValues(_.result())

    var functionMappers = functionMapperBuilders.mapValues(_.result())
    for ((signature, func) <- dynamicFunctions)
      functionMappers += (signature -> FunctionMapper(func))


    val atomSignatures = predicateSchema.keySet


    var atomStateDB = atomsEvDBBuilders.mapValues(_.result())

    val owa = queryPredicates ++ hiddenPredicates
    val cwa = atomSignatures -- owa

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

    for (signature <- atomStateDB.keysIterator) {
      if (!atomSignatures(signature)) // Check if this signature is defined in the mln file
        fatal(s"The predicate '$signature' that appears in the evidence file, is not defined in the mln file.")
      else if (!owa.contains(signature) && !queryPredicates.contains(signature))
        finalCWA += signature
    }

    for {
      signature <- atomSignatures
      if !atomStateDB.contains(signature)
      idf = domainSpace.identities(signature)
      state = if (finalCWA.contains(signature)) FALSE else UNKNOWN
    } atomStateDB += (signature -> AtomEvidenceDB(idf, state))


    new Evidence(constants, atomStateDB, functionMappers, domainSpace)
  }

  /*def apply(constants: Map[String, ConstantsSet], atomsEvDB: Map[AtomSignature, AtomEvidenceDB],
            functionMappers: Map[AtomSignature, FunctionMapper], domainSpace: DomainSpace): Evidence ={

    new Evidence(constants, atomsEvDB, functionMappers, domainSpace)
  }*/

  private def createTempEmptyDBFile: File = {
    import log._
    val filePrefix = s".mlnc_empty_${System.currentTimeMillis()}"

    Try[File](File.createTempFile(filePrefix, ".db")) match{
      case Success(tmpFile) =>
        tmpFile.deleteOnExit()
        tmpFile

      case Failure(f) =>
        sys.props.get("java.io.tmpdir") match {
          case Some(e) => fatal("Cannot create temporary file in the default JVM temporary files directory [java.io.tmpdir])")
          case None => fatal("Cannot create temporary file (default JVM temporary files directory is not defined [java.io.tmpdir])")
        }

    }
  }

}
