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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.logic

/**
  * MLNExpression represents a KB expression that can be parsed from *.mln or *.db files, that is:
  *
  * <ul>
  *   <li> Domain type definition (see [[lomrf.logic.MLNTypeDefinition]]), e.g.
  *     {{{ time = {1,...,100} }}} </li>
  *   <li> A pointer to another MLN file (see [[lomrf.logic.IncludeFile]]), e.g.
  *     {{{ #include "file.mln" }}} </li>
  *   <li> Predicate definition (see [[lomrf.logic.AtomicType]]), e.g.
  *     {{{ HoldsAt(fluent, time) }}} </li>
  *   <li> Function definition (see [[lomrf.logic.FunctionType]]), e.g.
  *     {{{ event walking(person) }}} </li>
  *   <li> MLN formula (see [[lomrf.logic.FormulaConstruct]]), e.g.
  *     {{{ 1.386 Happens(A,t) ^ Happens(B,t) => InitiatedAt(F, t) }}} </li>
  *   <li> Evidence atom, that is a ground atom possibly having a known state (see [[lomrf.logic.EvidenceAtom]] ), e.g.
  *   {{{ Happens(Event_A, 10) }}} </li>
  * </ul>
  */
trait MLNExpression

/**
  * Represents an MLN domain type definition (see [[lomrf.logic.MLNTypeDefinition]]).
  *
  * @example {{{
  *           time = {1,...,100}
  *           persons = {Anna, Bob, Mike, George, Lisa}
  *          }}}
  */
trait MLNDomainExpression extends MLNExpression

/**
  * A pointer to another MLN file.
  *
  * @example {{{
  *           #include "file.mln"
  *          }}}
  *
  * @param filename the filename to include
  */
case class IncludeFile(filename: String) extends MLNExpression

/**
  * Predicate definition, having the syntax:
  *
  * {{{
  *   PredicateName(arg_type1, arg_type2, ..., arg_typeN).
  * }}}
  *
  * @example {{{
  *           HoldsAt(fluent, time)
  *           Happens(event, time)
  *           InitiatedAt(fluent, time)
  *           TerminatedAt(fluent, time)
  *          }}}
  * According to the above predicate definitions the predicate '''HoldsAt''' takes two arguments,
  * the former is from the domain ''fluent'' and the latter form the domain ''time''.
  *
  * @param predicateName the name of the predicate (e.g. Parent, Happens, Friends etc.)
  * @param argTypes the argument domain types (e.g. person, fluent, time, event, etc.)
  */
case class AtomicType(predicateName: String, argTypes: Vector[String]) extends MLNDomainExpression

/**
  * Represents a function definition, with syntax:
  *
  * {{{
  *   return_type FunctionName(arg_type1, arg_type2, ..., arg_typeN)
  * }}}
  *
  * @example {{{
  *           event inactive(person)
  *           event running(person)
  *           fluent fighting(person, person)
  *          }}}
  *
  * @param returnType the return type (e.g. ''event'' for the first two examples and ''fluent'' for the last one).
  * @param name the function name (e.g. inactive, running and fighting).
  * @param argTypes the argument domain types (e.g. the domain ''person'').
  */
case class FunctionType(returnType: String, name: String, argTypes: Vector[String]) extends MLNDomainExpression

/**
  * MLN formula expression (see [[lomrf.logic.FormulaConstruct]]), for instance:
  *
  * @example {{{
  *           // soft-constrained formula
  *           1.286 Happens(A,t) ^ Happens(B,t) => InitiatedAt(F, t)
  *
  *           // hard-constrained formula
  *           Happens(Exit, t) => TerminatedAt(F,t).
  *          }}}
  */
trait MLNFormulaExpression extends MLNExpression

/**
  * Expression for ground evidence atoms.
  *
  * @example {{{
  *           // Evidence atoms having true state
  *           Happens(A,0)
  *           Happens(A,1)
  *
  *           // Evidence atoms having false state
  *           !Happens(A,2)
  *           !Happens(A,4)
  *
  *           // Evidence atom having unknown state Pr(Happens(B,10)=true)=0.8
  *           Happens(B,10) 0.8
  *
  *           // Function mappings
  *           Active_ID1 = active(ID1)
  *          }}}
  */
trait EvidenceExpression extends MLNExpression
