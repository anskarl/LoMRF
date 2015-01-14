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

package lomrf.logic

/**
 * It represents a KB expression that can be parsed from *.mln or *.db files, that is:
 * <ul>
 *   <li>Domain type definition (see [[lomrf.logic.MLNTypeDefinition]]), e.g. {{{time = {1,...,100} }}}</li>
 *   <li>A pointer to another MLN file (see [[lomrf.logic.IncludeFile]]), e.g. {{{#include "file.mln"}}}</li>
 *   <li>Predicate definition, with syntax: Predicate_Name(arg_type1, arg_type2, arg_type3), e.g. {{{HoldsAt(fluent, time)}}}</li>
 *   <li>Function definition, with syntax: return_type Function_Name(arg_type1, arg_type2), e.g. {{{event walking(person)}}}</li>
 *   <li>MLN formula (see [[lomrf.logic.Formula]]), e.g. {{{ 1.386 Happens(A,t) ^ Happens(B,t) => InitiatedAt(F, t) }}}</li>
 *   <li>Evidence atom, that is a ground atom with possibly known state (see [[lomrf.logic.EvidenceAtom]] ), e.g. {{{Happens(Some_Event, 10)}}}</li>
 * </ul>
 * @author Anastasios Skarlatidis
 */
trait MLNExpression

/**
 * Represents an MLN domain type definition (see [[lomrf.logic.MLNTypeDefinition]]).
 * @example {{{
 * time = {1,...,100}
 * persons = {Anna, Bob, Mike, George, Lisa}
 * }}}
 * @author Anastasios Skarlatidis
 */
trait MLNDomainExpression extends MLNExpression

/**
 * A pointer to another MLN file.
 *
 * @example {{{#include "file.mln"}}}
 * @author Anastasios Skarlatidis
 */
case class IncludeFile(filename: String) extends MLNExpression

/**
 * Predicate definition, with syntax: Predicate_Name(arg_type1, arg_type2, arg_type3).
 *
 * @example {{{
 * HoldsAt(fluent, time)
 * Happens(event, time)
 * InitiatedAt(fluent, time)
 * TerminatedAt(fluent, time)
 *          }}}
 * According to the above predicate definitions the predicate ''HoldsAt'' takes two arguments, the first is from the domain of ''fluents'' and the second form the domain of ''time''.
 *
 * @param predicateName the name of the predicate (e.g. HoldsAt, Happens, InitiatedAt, TerminatedAt, etc.)
 * @param argTypes the argument domain types (e.g. fluent, time, event, etc.)
 * @author Anastasios Skarlatidis
 */
case class AtomicType(predicateName: String, argTypes: Vector[String]) extends MLNDomainExpression

/**
 * Represents a function definition, with syntax:
 * {{{return_type Function_Name(arg_type1, arg_type2)}}}
 * @example {{{
 * event walking(person)
 * event running(person)
 * fluent meeting(person, person)
 * }}}
 *
 * @param returnType the return type (e.g. ''event'' for the first two examples and ''fluent'' for the last one).
 * @param name the function's name (e.g. walking, running and meeting).
 * @param argTypes the argument domain types (e.g. the domain of ''persons'').
 *
 * @author Anastasios Skarlatidis
 */
case class FunctionType(returnType: String, name: String, argTypes: Vector[String]) extends MLNDomainExpression

/**
 * MLN formula (see [[lomrf.logic.Formula]]), for example:
 * {{{
 * 1.386 Happens(A,t) ^ Happens(B,t) => InitiatedAt(F, t) // soft-constrained formula
 * Happens(Exit, t) => TerminatedAt(F,t). // hard-constrained formula
 * }}}
 *
 * @author Anastasios Skarlatidis
 */
trait MLNFormulaExpression extends MLNExpression

/**
 * Expression for ground evidence atoms.
 * 
 * @example {{{
 * Happens(A,0) // state = true
 * Happens(A,1) // state = true
 * !Happens(A,2) // state = false
 * Happens(A,4) // state = true
 *
 * Happens(B,10) 0.8 //state =unknown and P(Happens(B,10)=true)= 0.8
 * }}}
 *
 * @author Anastasios Skarlatidis
 */
trait EvidenceExpression extends MLNExpression