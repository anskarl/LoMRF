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

package lomrf.mln

import lomrf.logic.AtomSignature
import lomrf.mln.learning.structure.ModeDeclaration

package object model {

  type ConstantsDomain = Map[String, ConstantsSet]

  type PredicateSchema = Map[AtomSignature, Vector[String]]

  type FunctionSchema = Map[AtomSignature, (String, Vector[String])]

  type DynamicPredicates = Map[AtomSignature, Vector[String] => Boolean]

  type DynamicFunctions = Map[AtomSignature, Vector[String] => String]

  type EvidenceDB = Map[AtomSignature, AtomEvidenceDB]

  type FunctionMappers = Map[AtomSignature, FunctionMapper]

  type Identities = Map[AtomSignature, AtomIdentityFunction]

  type ModeDeclarations = Map[AtomSignature, ModeDeclaration]

  implicit class FunctionSchemaWrapped(val fs: FunctionSchema) extends AnyVal {

    def toPredicateSchema: PredicateSchema = fs.map {
      case (signature, (retType, argTypes)) =>
        val symbol = lomrf.AUX_PRED_PREFIX + signature.symbol
        val termTypes = argTypes.+:(retType)
        (AtomSignature(symbol, signature.arity + 1), termTypes)
    }
  }

}
