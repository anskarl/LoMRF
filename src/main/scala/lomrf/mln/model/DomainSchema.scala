package lomrf.mln.model

import lomrf.logic.AtomSignature

/**
 *
 * @param predicateSchema a map that associates atom signatures with a sequence of argument types
 * @param functionSchema a map that associates function signatures with a tuple of returning type and sequence of argument types
 * @param queryAtoms the set of query atom signatures
 * @param cwa the set of closed-world assumption atom signatures
 * @param owa the set of open-world assumption atom signatures (includes query atom signatures)
 */
case class DomainSchema(predicateSchema: Map[AtomSignature, Seq[String]],
                        functionSchema: Map[AtomSignature, (String, Vector[String])],
                        queryAtoms: Set[AtomSignature],
                        cwa: Set[AtomSignature],
                        owa: Set[AtomSignature]) {

  /**
   * The set of hidden atoms, those that are not query and not evidence.
   */
  val hiddenAtoms = owa -- queryAtoms



}
