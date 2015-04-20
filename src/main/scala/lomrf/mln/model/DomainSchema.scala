package lomrf.mln.model

import lomrf.logic.AtomSignature

case class DomainSchema(predicateSchema: Map[AtomSignature, collection.Seq[String]],
                        functionSchema: Map[AtomSignature, (String, Vector[String])],
                        dynamicPredicates: Map[AtomSignature, Vector[String] => Boolean],
                        dynamicFunctions: Map[AtomSignature, Vector[String] => String],
                        queryAtoms: Set[AtomSignature],
                        cwa: Set[AtomSignature],
                        owa: Set[AtomSignature],
                        probabilisticAtoms: Set[AtomSignature]) {

}
