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

package lomrf.mln.model

import java.io.{File, BufferedReader, FileReader}
import auxlib.log.Logger
import lomrf.logic._
import scala.util.Try

class Evidence(val constants: ConstantsDomain,
               val db: EvidenceDB,
               val functionMappers: FunctionMappers){

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

  def apply(constants: ConstantsDomain, db: EvidenceDB, fm: FunctionMappers): Evidence = {
    new Evidence(constants, db, fm)
  }


  def fromFiles(kb: KB,
                constantsDomainBuilders: ConstantsDomainBuilder,
                queryPredicates: Set[AtomSignature],
                files: Iterable[File],
                convertFunctions: Boolean): Evidence = {

    fromFiles(kb.predicateSchema, kb.functionSchema, constantsDomainBuilders, kb.dynamicFunctions, queryPredicates, Set.empty[AtomSignature], files, convertFunctions)
  }

  def fromFiles(kb: KB,
                constantsDomainBuilders: ConstantsDomainBuilder,
                queryPredicates: Set[AtomSignature],
                hiddenPredicates: Set[AtomSignature],
                files: Iterable[File],
                convertFunctions: Boolean): Evidence = {

    fromFiles(kb.predicateSchema, kb.functionSchema, constantsDomainBuilders, kb.dynamicFunctions, queryPredicates, hiddenPredicates, files, convertFunctions)
  }

  def fromFiles(predicateSchema: PredicateSchema,
                functionSchema: FunctionSchema,
                constantsDomainBuilder: ConstantsDomainBuilder,
                dynamicFunctions: DynamicFunctions,
                queryPredicates: Set[AtomSignature],
                hiddenPredicates: Set[AtomSignature],
                files: Iterable[File],
                convertFunctions: Boolean): Evidence = {


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

    def isOWA(signature: AtomSignature) = queryPredicates.contains(signature) || hiddenPredicates.contains(signature)

    val evidenceParser = new EvidenceParser
    val evidenceExpressionsDB =
      for (file <- inputFiles; fileReader = new BufferedReader(new FileReader(file)))
        yield evidenceParser.parseAll(evidenceParser.evidence, fileReader) match {
          case evidenceParser.Success(expr, _) => expr
          case x => fatal(s"Can't parse the following expression: '$x' in file: '${file.getPath}'")
        }

    info("--- Stage 1: Parsing constants")
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
        predicateSchema.get(atom.signature) match {
          case Some(argTypes) =>
            //append its constants into constantsMap
            for ((value, index) <- atom.terms.view.zipWithIndex) {
              val constantType = argTypes(index)

              constantsDomainBuilder.get(constantType) match {
                case Some(x) => x += value.symbol
                case None =>
                  constantsDomainBuilder += (constantType -> value.symbol)
              }
            }
          case _ => fatal(s"Unknown predicate '$atom' in the given input evidence file(s).")
        }
      case _ => //ignore
    }

    val constants: Map[String, ConstantsSet] = constantsDomainBuilder.result()

    info("--- Stage 2: Creating function mappings, and evidence database.")

    val builder = EvidenceBuilder(predicateSchema, functionSchema, queryPredicates, hiddenPredicates, constants, convertFunctions).withDynamicFunctions(dynamicFunctions)

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
