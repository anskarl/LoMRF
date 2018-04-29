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
  *
  * @param predicates a map that associates atom signatures with a sequence of argument types
  * @param functions a map that associates function signatures with a tuple of returning type and sequence of argument types
  * @param dynamicPredicates a map that associates signatures of dynamic atoms with a scala function that determines the truth state: (atoms ground arguments) => Boolean
  * @param dynamicFunctions a map that associates the identities of dynamic functions with a scala function that determines the function's result: (ground arguments) => Boolean
  */
case class MLNSchema(
    predicates: PredicateSchema,
    functions: FunctionSchema,
    dynamicPredicates: DynamicPredicates,
    dynamicFunctions: DynamicFunctions)
