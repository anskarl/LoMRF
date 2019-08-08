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

package lomrf.mln.model

/**
  * MLNSchema holds the predicate and function schemas, both static and dynamic.
  *
  * @note Function signatures are also represented using [[lomrf.logic.AtomSignature]].
  *
  * @param predicates a map from atom signatures to their argument domain names
  * @param functions a map from function signatures to their return value domain name, argument domain names
  * @param dynamicPredicates a map from atom signatures to functions of the form '''Vector[String] => Boolean'''
  * @param dynamicFunctions a map from function signatures to functions of the form '''Vector[String] => String'''
  */
case class MLNSchema(
    predicates: PredicateSchema,
    functions: FunctionSchema,
    dynamicPredicates: DynamicPredicates,
    dynamicFunctions: DynamicFunctions)
