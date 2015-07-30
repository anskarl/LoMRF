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
import lomrf.util._
import scala.util.{Failure, Try, Success}


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
  private val AUX_PRED_PREFIX = "F_"

  def apply(constants: ConstantsDomain, db: EvidenceDB, fm: FunctionMappers): Evidence = {
    new Evidence(constants, db, fm)
  }

  def fromFiles(kb: KB,
                constantsDomainBuilders: ConstantsDomainBuilder,
                queryPredicates: Set[AtomSignature],
                filenames: List[String]): Evidence ={

    val fileList = if (filenames.isEmpty) List(createTempEmptyDBFile) else filenames.map(filename => new File(filename))

    fromFiles(kb, constantsDomainBuilders, queryPredicates, Set.empty[AtomSignature], fileList)
  }

  def fromFiles(kb: KB,
                constantsDomainBuilders: ConstantsDomainBuilder,
                queryPredicates: Set[AtomSignature],
                hiddenPredicates: Set[AtomSignature],
                filenames: List[String]): Evidence ={
    val fileList = if (filenames.isEmpty) List(createTempEmptyDBFile) else filenames.map(filename => new File(filename))

    fromFiles(kb, constantsDomainBuilders, queryPredicates, hiddenPredicates, fileList)
  }

  def fromFiles(kb: KB,
                constantsDomainBuilders: ConstantsDomainBuilder,
                queryPredicates: Set[AtomSignature],
                files: Iterable[File]): Evidence = {
    fromFiles(kb.predicateSchema, kb.functionSchema, constantsDomainBuilders, kb.dynamicFunctions, queryPredicates, Set.empty[AtomSignature], files)
  }

  def fromFiles(kb: KB,
                constantsDomainBuilders: ConstantsDomainBuilder,
                queryPredicates: Set[AtomSignature],
                hiddenPredicates: Set[AtomSignature],
                files: Iterable[File]): Evidence = {
    fromFiles(kb.predicateSchema, kb.functionSchema, constantsDomainBuilders, kb.dynamicFunctions, queryPredicates, hiddenPredicates, files)
  }

  def fromFiles(predicateSchema: PredicateSchema,
                functionSchema: FunctionSchema,
                constantsDomainBuilder: ConstantsDomainBuilder,
                dynamicFunctions: DynamicFunctions,
                queryPredicates: Set[AtomSignature],
                hiddenPredicates: Set[AtomSignature],
                files: Iterable[File],
                convertFunctions: Boolean = false): Evidence = {


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
          case _ => fatal(s"The type of '$atom' is not defined.")
        }
      case _ => //ignore
    }

    val constants: Map[String, ConstantsSet] = constantsDomainBuilder.result()

    info("--- Stage 2: Creating function mappings, and evidence database.")

    val builder = {
      if (convertFunctions) {
        var enhancedPredicateSchema = predicateSchema

        for (( signature, (retDomain, termDomain)) <- functionSchema) {
          val symbol = AUX_PRED_PREFIX + signature.symbol
          val arity = termDomain.size + 1
          enhancedPredicateSchema += AtomSignature.apply(symbol, arity) -> termDomain
        }

        EvidenceBuilder(enhancedPredicateSchema, Map.empty, queryPredicates, hiddenPredicates, constants)

      }
      else EvidenceBuilder(predicateSchema, functionSchema, queryPredicates, hiddenPredicates, constants)
    }

    for (evidenceExpressions <- evidenceExpressionsDB; expr <- evidenceExpressions) expr match {
      case fm: FunctionMapping =>
        if(convertFunctions) {
          val symbol = AUX_PRED_PREFIX + fm.functionSymbol
          val terms = fm.values.+:(fm.retValue).map(Constant) // prepend fm.retValue and map to Constants
          EvidenceAtom.asTrue(symbol, terms)
        }
        else builder.functions += fm
      case atom: EvidenceAtom => builder.evidence += atom
    }

    builder.result()
  }

  private def createTempEmptyDBFile: File = {
    import log._
    val filePrefix = s".mlnc_empty_${System.currentTimeMillis()}"

    Try[File](File.createTempFile(filePrefix, ".db")) match {
      case Success(tmpFile) =>
        tmpFile.deleteOnExit()
        tmpFile

      case Failure(f) =>
        sys.props.get("java.io.tmpdir") match {
          case Some(e) =>
            fatal("Cannot create temporary file in the default JVM temporary files directory [java.io.tmpdir])")
          case None =>
            fatal("Cannot create temporary file (default JVM temporary files directory is not defined [java.io.tmpdir])")
        }

    }
  }

}
