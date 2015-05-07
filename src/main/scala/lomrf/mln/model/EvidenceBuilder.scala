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

import lomrf.logic.{EvidenceAtom, AtomSignature}
import lomrf.util.AtomEvidenceDBBuilder

/**
 * Evidence builder with fluent pattern
 */
class EvidenceBuilder(val domainSpace: DomainSpace,
                      val constants: ConstantsDomain) { self =>


  private var _evidenceDB = Map[AtomSignature, AtomEvidenceDBBuilder]()

  private var _functionMappers: FunctionMappers = _

  def withEvidenceDB(evidenceDB: EvidenceDB): self.type = ???

  def withFunctionMappers(fm: FunctionMappers): self.type = ???

  def result(): Evidence = ???

  object evidence {

    def += (atom: EvidenceAtom): self.type = {

      _evidenceDB.get(atom.signature) match {
        case Some(builder) => builder += atom
        case None =>
          val idf = domainSpace.identities(atom.signature)
          val isCWA = domainSpace.isCWA(atom.signature)
          val builder = AtomEvidenceDBBuilder(idf, isCWA)
          builder += atom
          _evidenceDB += (atom.signature -> builder)
      }

      self
    }

  }


  object functionMappers{

  }


}
