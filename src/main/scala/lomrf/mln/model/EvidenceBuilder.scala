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

import lomrf.logic.{FunctionMapping, EvidenceAtom, AtomSignature}
import lomrf.mln.model.MLNSpace
import lomrf.util._

/**
 * Evidence builder with fluent pattern
 */
class EvidenceBuilder(val domainSpace: MLNSpace,
                      val constants: ConstantsDomain,
                      val functionSchema: FunctionSchema = Map.empty) { self =>


  private var evidenceDB = Map[AtomSignature, AtomEvidenceDBBuilder]()

  private var functionMappers = Map[AtomSignature, FunctionMapperBuilder]()

  def result(): Evidence = ???

  object evidence {

    def update(evb: Map[AtomSignature, AtomEvidenceDB]): self.type = {
      evidenceDB = evb.map {
        case (signature, db) =>
          val builder = AtomEvidenceDBBuilder(db.identity,db.numberOfUnknown > 0)

          require(signature == db.identity.signature,
              s"Something is wrong for key signature ${AtomSignature.toString}. " +
              s"The associated AtomEvidenceDB is associated with different signature (${db.identity.signature})")

          signature -> builder
      }
      self
    }


    def update(evb: Iterable[AtomEvidenceDB]): self.type = {
      evidenceDB = evb.map(db => db.identity.signature -> AtomEvidenceDBBuilder(db.identity, db.numberOfUnknown > 0)).toMap
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


    private def insert(atom: EvidenceAtom): Unit = evidenceDB.get(atom.signature) match {
      case Some(builder) => builder += atom
      case None =>
        val idf = domainSpace.identities(atom.signature)
        val isCWA = domainSpace.isCWA(atom.signature)
        val builder = AtomEvidenceDBBuilder(idf, isCWA)
        builder += atom

        evidenceDB += (atom.signature -> builder)
      }
  }


  object functions {

    def += (fm: FunctionMapping): self.type = {
      insert(fm)
      self
    }

    private def insert(fm: FunctionMapping): Unit ={
      val fmValuesStr = fm.values.map(_.symbol)

      functionMappers.get(fm.signature) match {
        case Some(fMappingBuilder) => fMappingBuilder +=(fmValuesStr, fm.retValue)

        case None =>
          val idFunction = AtomIdentityFunction(fm.signature, functionSchema(fm.signature)._2, constants, 1)
          val builder = new FunctionMapperBuilder(idFunction)
          builder += (fmValuesStr, fm.retValue)
          functionMappers += (fm.signature -> builder)
      }
    }
  }

}

object EvidenceBuilder {


  def apply(predicateSchema: PredicateSchema,
            queryPredicates: Set[AtomSignature],
            hiddenPredicates: Set[AtomSignature],
            constants: ConstantsDomain): EvidenceBuilder = {

    val domainSpace = MLNSpace(predicateSchema, queryPredicates, hiddenPredicates, constants)

    new EvidenceBuilder(domainSpace, constants)
  }

  def apply(predicateSchema: PredicateSchema,
            functionSchema: FunctionSchema,
            queryPredicates: Set[AtomSignature],
            hiddenPredicates: Set[AtomSignature],
            constants: ConstantsDomain): EvidenceBuilder = {

    val domainSpace = MLNSpace(predicateSchema, queryPredicates, hiddenPredicates, constants)

    new EvidenceBuilder(domainSpace, constants, functionSchema)
  }



}
