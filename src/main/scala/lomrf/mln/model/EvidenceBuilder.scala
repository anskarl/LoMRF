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

import lomrf.logic.{Constant, FunctionMapping, EvidenceAtom, AtomSignature}
import scala.collection.breakOut
import lomrf.util._

/**
 * Evidence builder (fluent interface)
 */
final class EvidenceBuilder private(predicateSpace: PredicateSpace,
                                    constants: ConstantsDomain,
                                    predicateSchema: PredicateSchema,
                                    functionSchema: FunctionSchema = Map.empty,
                                    convertFunctionsToPredicates: Boolean = false) { self =>

  private var edbBuilders = Map.empty[AtomSignature, AtomEvidenceDBBuilder]

  private var fmBuilders = Map.empty[AtomSignature, FunctionMapperBuilder]

  private val functionRegister =
    if(convertFunctionsToPredicates) new FunctionToAUXPredRegister else new DefaultFunctionRegister



  def withEvidenceBuilders(evBuilders: Map[AtomSignature, AtomEvidenceDBBuilder]): self.type = {

    val missingSignatures = evBuilders.keys.filterNot(functionSchema.contains)

    if(missingSignatures.nonEmpty)
      throw new IllegalArgumentException(
        "Cannot have atom evidence builders for predicates with unspecified schema. " +
          s"The following atom signatures are missing from the predicate schema: '${missingSignatures.mkString(", ")}'")

    val result = new EvidenceBuilder(predicateSpace, constants, predicateSchema, functionSchema)
    result.edbBuilders = evBuilders
    result
  }

  def withFunctionBuilders(fmBuilders: Map[AtomSignature, FunctionMapperBuilder]): self.type = {
    if(functionSchema.isEmpty)
      throw new IllegalArgumentException("Cannot have function mapping builders when function schema is missing.")

    val missingSignatures = fmBuilders.keys.filterNot(functionSchema.contains)

    if(missingSignatures.nonEmpty)
      throw new IllegalArgumentException(
        "Cannot have function mapping builders for functions with unspecified schema. " +
        s"The following function signatures are missing from the function schema: '${missingSignatures.mkString(", ")}'")

    val result = new EvidenceBuilder(predicateSpace, constants, predicateSchema, functionSchema)
    if(convertFunctionsToPredicates) {
      for( (signature, builder) <- fmBuilders){
        val fm = builder.result()

      }
    }
    else {
      result.fmBuilders = fmBuilders
    }

    result
  }


  def clear(): Unit = {
    edbBuilders =  Map.empty[AtomSignature, AtomEvidenceDBBuilder]
    fmBuilders = Map.empty[AtomSignature, FunctionMapperBuilder]
  }

  def result(): Evidence = {

    def mkEvidenceDB(signature: AtomSignature): AtomEvidenceDB = {
      edbBuilders.get(signature) match {
        case Some(builder) => builder.result()
        case None =>
          val idf = predicateSpace.identities(signature)
          val isCWA = predicateSpace.isCWA(signature)

          if(isCWA) AtomEvidenceDB.allFalse(idf)
          else AtomEvidenceDB.allUnknown(idf)
      }
    }


    val db: EvidenceDB = (for(signature <- predicateSchema.keys) yield signature -> mkEvidenceDB(signature))(breakOut)

    val fm = fmBuilders.map(entries => entries._1 -> entries._2.result())

    Evidence(constants, db, fm)
  }

  object evidence {

    def update(evb: Map[AtomSignature, AtomEvidenceDB]): self.type = {
      edbBuilders = evb.map {
        case (signature, db) =>
          if(!predicateSchema.contains(signature))
            throw new IllegalArgumentException(s"Unknown atom signature '$signature'")

          val builder = AtomEvidenceDBBuilder(db.identity, db.numberOfUnknown > 0)

          require(signature == db.identity.signature,
              s"Something is wrong for key signature ${AtomSignature.toString}. " +
              s"The associated AtomEvidenceDB is associated with different signature (${db.identity.signature})")

          signature -> builder
      }
      self
    }


    def update(evb: Iterable[AtomEvidenceDB]): self.type = {
      edbBuilders = evb.map(db => db.identity.signature -> AtomEvidenceDBBuilder(db.identity, db.numberOfUnknown > 0)).toMap
      self
    }

    def += (atom: EvidenceAtom): self.type = {
      insert(atom)
      self
    }

    def ++= (atoms: Iterable[EvidenceAtom]): self.type = {
      atoms.foreach(insert)
      self
    }

    def ++=(atoms: EvidenceAtom*): self.type = {
      atoms.foreach(insert)
      self
    }

    def clear(): Unit = {
      if(convertFunctionsToPredicates)
        edbBuilders = edbBuilders.filterKeys(signature => functionSchema.contains(signature))
      else
        edbBuilders = Map.empty[AtomSignature, AtomEvidenceDBBuilder]
    }


    private def insert(atom: EvidenceAtom): Unit = {
      edbBuilders.get(atom.signature) match {
        case Some(builder) =>
          builder += atom

        case None if predicateSchema.contains(atom.signature) =>

          val idf = predicateSpace.identities(atom.signature)
          val isCWA = predicateSpace.isCWA(atom.signature)
          val builder = AtomEvidenceDBBuilder(idf, isCWA)
          builder += atom

          edbBuilders += (atom.signature -> builder)

        case _ =>
          throw new IllegalArgumentException(s"Unknown atom signature for atom '${atom.toText}'")
      }
    }
  }


  object functions {

    def += (fm: FunctionMapping): self.type = {
      functionRegister.insert(fm)
      self
    }

    def ++= (fms: Iterable[FunctionMapping]): self.type ={
      fms.foreach(functionRegister.insert)
      self
    }

    def ++= (fms: FunctionMapping*): self.type ={
      fms.foreach(functionRegister.insert)
      self
    }

    def clear(): Unit = {
      if(convertFunctionsToPredicates)
        edbBuilders = edbBuilders -- functionSchema.keys

      fmBuilders = Map.empty[AtomSignature, FunctionMapperBuilder]
    }
  }

  private sealed trait FunctionRegister {
    def insert(fm: FunctionMapping): Unit
  }

  private final class DefaultFunctionRegister extends FunctionRegister{
    override def insert(fm: FunctionMapping): Unit = {
      fmBuilders.get(fm.signature) match {
        case Some(fMappingBuilder) =>
          fMappingBuilder += ( fm.values, fm.retValue)

        case None if functionSchema.contains(fm.signature) =>

          val idFunction = AtomIdentityFunction(fm.signature, functionSchema(fm.signature)._2, constants, 1)
          val builder = new FunctionMapperBuilder(idFunction)
          builder += ( fm.values, fm.retValue)

          fmBuilders += (fm.signature -> builder)

        case _ =>
          throw new IllegalArgumentException(s"Unknown function signature for function mapping '${fm.toString}'")
      }
    }
  }

  private final class FunctionToAUXPredRegister extends FunctionRegister{
    override def insert(fm: FunctionMapping): Unit = {
      if(functionSchema.contains(fm.signature)){
        val symbol = EvidenceBuilder.AUX_PRED_PREFIX + fm.functionSymbol
        val terms = fm.values.+:(fm.retValue).map(Constant) // prepend fm.retValue and map to Constants

        self.evidence += EvidenceAtom.asTrue(symbol, terms)
      }
      else {
        throw new IllegalArgumentException(s"Unknown function signature for function mapping '${fm.toString}'")
      }

      fmBuilders.get(fm.signature) match {
        case Some(fMappingBuilder) =>
          fMappingBuilder += ( fm.values, fm.retValue)

        case None if functionSchema.contains(fm.signature) =>

          val idFunction = AtomIdentityFunction(fm.signature, functionSchema(fm.signature)._2, constants, 1)
          val builder = new FunctionMapperBuilder(idFunction)
          builder += ( fm.values, fm.retValue)

          fmBuilders += (fm.signature -> builder)

        case _ =>
          throw new IllegalArgumentException(s"Unknown function signature for function mapping '${fm.toString}'")
      }
    }
  }

}

object EvidenceBuilder {

  // predicate prefix when functions are converted into auxiliary predicates
  private val AUX_PRED_PREFIX = "F_"


  def apply(predicateSchema: PredicateSchema,
            queryPredicates: Set[AtomSignature],
            hiddenPredicates: Set[AtomSignature],
            constants: ConstantsDomain): EvidenceBuilder = {

    val domainSpace = PredicateSpace(predicateSchema, queryPredicates, hiddenPredicates, constants)

    new EvidenceBuilder(domainSpace, constants, predicateSchema)
  }

  def apply(predicateSchema: PredicateSchema,
            functionSchema: FunctionSchema,
            queryPredicates: Set[AtomSignature],
            hiddenPredicates: Set[AtomSignature],
            constants: ConstantsDomain): EvidenceBuilder = {

    val domainSpace = PredicateSpace(predicateSchema, queryPredicates, hiddenPredicates, constants)

    new EvidenceBuilder(domainSpace, constants, predicateSchema, functionSchema)
  }



}
