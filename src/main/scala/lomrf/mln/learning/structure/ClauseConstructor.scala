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

package lomrf.mln.learning.structure

import lomrf.logic._
import lomrf.logic.LogicOps._
import lomrf.mln.learning.structure.ClauseConstructor.ClauseType.ClauseType
import lomrf.mln.learning.structure.hypergraph.HPath
import lomrf.mln.model._
import scala.util.{Success, Failure, Try}
import scalaxy.streams._

/**
 * Clause constructor provides various methods for constructing clauses.
 */
object ClauseConstructor {

  /**
    * Produce CNF from an iterable of formulas. This version of CNF is different from [[NormalForm.compileCNF]] because
    * it uses a hybrid distribute function in order to perform fast distributive property in formulas having specific
    * properties. These kind of formulas are appearing very often during the learning process of [[OSLa]].
    *
    * @return a set of the produced clauses
    */
  def makeCNF(formulas: Iterable[Formula])(implicit constants: Map[String, ConstantsSet] = Map.empty): Set[Clause] =
    formulas.par.foldLeft(Set[Clause]())((clauses, formula) => clauses ++ toCNF(formula))

  /**
    * @return a set of clauses produced by the given formula
    */
  private def toCNF(source: Formula)(implicit constants: Map[String, ConstantsSet] = Map.empty): Set[Clause] = {

    import lomrf.logic.NormalForm._

    def normaliseConstruct(f: FormulaConstruct) = {
      hybridDistribute(
        removeUniversalQuantifiers(
          removeExistentialQuantifiers(constants,
            standardizeVariables(NormalForm.negationsIn(removeImplications(f))))
        )
      )
    }

    val normalized = source match {
      case wf:WeightedFormula =>
        WeightedFormula(wf.weight, normaliseConstruct(wf.formula))

      case DefiniteClause(head, body) =>
        WeightedFormula.asHard(normaliseConstruct(Implies(body,head)))

      case WeightedDefiniteClause(weight, clause) =>
        WeightedFormula(weight, normaliseConstruct(Implies(clause.body, clause.head)))

      case c: FormulaConstruct =>
        WeightedFormula.asHard(normaliseConstruct(c))
    }

    extractClauses(normalized)
  }

  /**
    * Hybrid version of distribute function in [[NormalForm.distribute]]. This version checks if
    * the formula construct has the property of all AND operators appearing inside OR operators.
    * In case this property holds, then the distribution is performed in a much faster and more
    * memory efficient way, by using an integer encoding. In any other case the generic
    * [[NormalForm.distribute]] is called.
    *
    * @param source the given formula construct
    *
    * @return the formula construct resulted from the appliance of the distributive property
    */
  private def hybridDistribute(source: FormulaConstruct): FormulaConstruct = {

    var keyspace = 2
    var keyToAtom = Map[Int, FormulaConstruct]()
    var atomToKey = Map[FormulaConstruct, Int]()

    /**
      * Find all atoms appearning alone inside an OR operator. Remove them
      * from the encoding and use them as a prefix.
      *
      * Example:
      *   F = A v (B ^ C) v D v (E ^ F)
      *   Prefix = A v D
      *   Rest of the encoding = (B ^ D) v (E ^ F)
      *
      * @param source the given formula construct encoding
      *
      * @return a prefix vector containing only OR operators and the rest of the encoding
      */
    def getPrefix(source: Vector[Int]): (Vector[Int], Vector[Int]) = {

      var prefix = Vector[Int]()
      var rest = Vector[Int]()
      var zeros = 0
      var ones = 0
      var atoms = 0

      for (symbol <- source) {

        if (symbol == 0) zeros += 1
        else if (symbol == 1) {
          rest :+= 1
          ones += 1
        }
        // all else if above indicate that symbol is obviously > 1 and therefore an atom
        else if (ones == 0) {
          zeros -= 1
          prefix :+= symbol
        }
        else if (ones > 0) {
          rest :+= symbol
          atoms += 1
        }

        // if atoms are 2 and ones are 1 then we found exactly one AND operator
        if(atoms == 2 && ones == 1) {
          atoms = 0
          ones = 0
        }
        // if atoms are exactly 2 but ones are > 1 then we found a AND operator but there are more following
        else if (atoms == 2) {
          atoms -= 1
          ones -= 1
        }
      }

      (Vector.fill(prefix.length - 1)(0) ++ prefix, Vector.fill[Int](zeros)(0) ++ rest)
    }

    /**
      * Encodes the given formula construct into a unique integer sequence. The sequence
      * can be decoded back to the given construct.
      *
      * Example:
      *  F = (A ^ B) v (C ^ D)
      *  Sequence = 0123145 where A = 2, B = 3, C = 4, D = 5, AND = 1, OR = 0
      *
      * @param source the given formula construct to encode
      *
      * @return a vector of integers
      */
    def encode(source: FormulaConstruct): Vector[Int] = source match {

      case f @ (_: AtomicFormula | _: Not) =>
        if (atomToKey.contains(f))
          Vector(atomToKey(f))
        else {
          keyspace += 1
          keyToAtom += keyspace -> f
          atomToKey += f -> keyspace
          Vector(keyspace)
        }

      case Or(a, b) => Vector(0) ++ encode(a) ++ encode(b)
      case And(a, b) => Vector(1) ++ encode(a) ++ encode(b)
      case _ =>
        throw new IllegalStateException("Failed to encode due to illegal formula type: " + source.toText)
    }

    /**
      * Decode a sequence of integers back into a formula construct. See [[encode]].
      *
      * @param source a sequence of integers
      *
      * @return a decoded formula construct
      */
    def decode(source: Vector[Int]): FormulaConstruct = {
      val buffer = scala.collection.mutable.Stack[FormulaConstruct]()

      for (symbol <- source.reverseIterator) {
        if (symbol > 1) buffer push keyToAtom(symbol)
        else if (symbol == 1) buffer push And(buffer.pop(), buffer.pop())
        else if (symbol == 0) buffer push Or(buffer.pop(), buffer.pop())
      }

      buffer.head
    }

    /**
      * Splits a given formula construct encoding into two sub-vectors representing
      * the left and right part of the top level binary operator (AND, OR).
      *
      * Example:
      *   F = (A ^ B) v (C ^ D)
      *   Encoding = 0123145
      *   Split = (123), (145)
      *
      * @param code the given formula construct encoding
      *
      * @return left and right part encodings of the top level operator
      */
    def split(code: Vector[Int]): (Vector[Int], Vector[Int]) = {

      var splitIdx = -1
      var idx = 0
      var counter = 0
      var args = 0

      val list = code.iterator
      while(list.hasNext && idx < code.length) {
        val digit = list.next()
        if (splitIdx == -1) {

          if (digit < 2) counter += 1
          else if (digit > 2) args += 1

          if (counter - args == 0) splitIdx = idx
        }
        idx += 1
      }

      if (splitIdx != -1) {
        val pair = code.splitAt(splitIdx + 1)
        (pair._1.tail, pair._2)
      }
      else (Vector.empty, Vector.empty)
    }

    /**
      * Check if the fast distributive property can be
      * applied in the given encoding.
      *
      * @param encoding a given formula construct encoding
      *
      * @return true if fast distribute can be
      *         applied, false otherwise
      */
    def canFastDistribute(encoding: Vector[Int]): Boolean = {
      var found = false
      var idx = 0

      while(idx < encoding.length) {
        if (encoding(idx) == 0 && found) {
          idx = encoding.length
          found = false
        }
        else if (encoding(idx) == 1)
          found = true
        idx += 1
      }

      found
    }

    // Encode the given formula construct
    val encodedFormula = encode(source)

    // Check if the fast distributive property can be performed
    if (canFastDistribute(encodedFormula)) {

      // Prefix is all atoms appearing only inside an OR operator
      val (prefix, rest) = getPrefix(encodedFormula)

      var resultedFormulas = Vector[Vector[Int]]()

      // Initially only the prefix exists
      resultedFormulas :+= prefix
      var remained = rest

      while(remained.nonEmpty) {

        var transformed = Vector[Vector[Int]]()

        // If OR still exists in the encoding, split and get left and right parts
        val (a, b) = if(remained.contains(0)) split(remained) else (remained, Vector.empty)

        val (atoms, next) = if (a.head == 1) (a.dropWhile(_ == 1), b) else (b.dropWhile(_ == 1), a)

        var idx = 0
        while(idx < resultedFormulas.length) {
          atoms.foreach { atom =>
            if (!resultedFormulas(idx).contains(atom)) {
              val res = Vector(0) ++ resultedFormulas(idx) ++ Vector(atom)
              if (!transformed.exists(f => f.sorted == res.sorted)) transformed +:= res
            }
            else if (!transformed.exists(f => f.sorted == resultedFormulas(idx).sorted))
              transformed :+= resultedFormulas(idx)
          }
          idx += 1
        }

        remained = next
        resultedFormulas = transformed
      }

      // Decode all resulted formula construct in order to pass them into the next step
      decode(Vector.fill[Int](resultedFormulas.length - 1)(1) ++ resultedFormulas.flatten)
    }
    else NormalForm.distribute(source)
  }

  /**
    * Create specific clause types from paths, given the mode declarations for all predicates
    * appearing in the given predicate schema. Clause types can be simple conjunctions or horn
    * clauses, or both. Furthermore, one can provide a vector of pre-existing clauses in order
    * to avoid creating clauses already existing in this vector.
    *
    * @param paths set of given paths to transform into clauses
    * @param predicates predicate schema for all predicates appearing in the paths
    * @param modes mode declarations for all predicates appearing in the paths
    * @param evidence specified evidence
    * @param clauseType clause types to create [[lomrf.mln.learning.structure.ClauseConstructor.ClauseType]]
    * @param preExistingClauses set of pre-existing clauses
    *
    * @return a vector of unique clauses
    */
  def clauses(paths: Set[HPath], predicates: PredicateSchema, modes: ModeDeclarations,
              evidence: Evidence, clauseType: ClauseType = ClauseType.BOTH,
              preExistingClauses: Vector[Clause] = Vector.empty[Clause]): Try[Vector[Clause]] = {

    // The resulting set of created clauses
    var clauses = Vector[Clause]()

    // For each path create a clause
    paths.foreach { path =>

      // Map constants to variable symbols in order to reuse the variable symbols for the same constants
      var constantsToVar = Map.empty[String, Variable]

      // For each ground atom in the path replace constants using variables and add it as a sub-formula
      val literals = path.ordering.map { case (atomID, signature) =>

        // Get the predicate schema of the current signature (predicate domains)
        val schema = predicates.get(signature) match {
          case Some(existingSchema) => existingSchema
          case None => return Failure(
            new NoSuchElementException(s"There is no predicate schema defined for signature '$signature'")
          )
        }

        // Get the constants for current ground atom
        val constants = evidence.db(signature).identity.decode(atomID) match {
          case Success(result) => result
          case Failure(exception) => return Failure(exception)
        }

        val placeMarkers = modes(signature).placeMarkers
        var terms = Vector.empty[Term]

        optimize { for (i <- constants.indices) {
          val constant = constants(i)
          if (placeMarkers(i).constant) terms :+= Constant(constant)
          else if (constantsToVar.contains(constant)) terms :+= constantsToVar(constant)
          else {
            val variable = Variable(s"x${constantsToVar.size}", schema(i))
            constantsToVar += constant -> variable
            terms :+= variable
          }
        }}

        NegativeLiteral(AtomicFormula(signature.symbol, terms))
      }

      (clauseType match {
        case ClauseType.CONJUNCTION =>
          Vector(
            Clause(literals.toSet, 1.0))
        case ClauseType.HORN =>
          Vector(
            Clause(literals.init.toSet + Literal.asPositive(literals.last.sentence), 1.0))
        case ClauseType.BOTH =>
          Vector(
            Clause(literals.toSet, 1.0),
            Clause(literals.init.toSet + Literal.asPositive(literals.last.sentence), 1.0))
      }) foreach { c =>
        /*
         * It should introduce functions into the clause before checking if already exists,
         * because preExistingClauses contain only functions
         */
        val clause = LogicFormatter.ClauseFormatter.introduceFunctions(c)
        !(clauses ++ preExistingClauses).exists(c => (c subsumes clause) && (clause subsumes c)) match {
          case true => clauses :+= clause
          case false =>
        }
      }
    }

    Success(clauses)
  }

  /**
   * Create definite clauses from paths, given the mode declarations for all predicates appearing
   * in the given predicate schema. Furthermore, one can provide a vector of pre-existing definite
   * clauses in order to avoid creating definite clauses already existing in this vector. Finally
   * if paths contain auxiliary predicates they will be eliminated, resulting in definite clauses
   * containing functions.
   *
   * @param paths set of given paths to transform into definite clauses
   * @param predicates predicate schema for all predicates appearing in the paths
   * @param modes mode declarations for all predicates appearing in the paths
   * @param evidence specified evidence
   * @param preExistingDefiniteClauses set of pre-existing definite clauses
   *
   * @return a vector of unique definite clauses
   */
  def definiteClauses(paths: Set[HPath], predicates: PredicateSchema,
                      modes: ModeDeclarations, evidence: Evidence,
                      preExistingDefiniteClauses: Set[WeightedDefiniteClause] =
                      Set.empty[WeightedDefiniteClause]): Try[Set[WeightedDefiniteClause]] = {

    // Set of the created definite clauses
    var definiteClauses = Set.empty[WeightedDefiniteClause]

    paths.foreach { path =>

      // Map constants to variables in order to reuse the variables for the same constants
      var constantsToVar = Map.empty[String, Variable]

      // For each ground atom in the path replace constants with variables and add it as sub-formula
      val atoms = path.ordering.map { case (atomID, signature) =>

        // Get the predicate schema of the current signature
        val schema = predicates.get(signature) match {
          case Some(existingSchema) => existingSchema
          case None => return Failure(
            new NoSuchElementException(s"There is no predicate schema defined for signature '$signature'")
          )
        }

        // Get the constants for current ground atom
        val constants = evidence.db(signature).identity.decode(atomID) match {
          case Success(result) => result
          case Failure(exception) => return Failure(exception)
        }

        val placeMarkers = modes(signature).placeMarkers
        var terms = Vector[Term]()

        optimize { for (i <- constants.indices) {
          val constant = constants(i)
          if (placeMarkers(i).constant) terms :+= Constant(constant)
          else if (constantsToVar.contains(constant)) terms :+= constantsToVar(constant)
          else {
            val variable = Variable(s"x${constantsToVar.size}", schema(i))
            constantsToVar += constant -> variable
            terms :+= variable
          }
        }}

        AtomicFormula(signature.symbol, terms)
      }

       /*
        * It should introduce functions into the definite clause before checking if already exists,
        * because preExistingDefiniteClauses contain only functions
        */
      val definiteClause =
        LogicFormatter.WeightedDefiniteClauseFormatter.introduceFunctions(
          WeightedDefiniteClause(1.0, DefiniteClause(atoms.last, atoms.init.reduce(And)))
        )

      /*
       * A definite clause is unique iff there is NO other definite clause subsuming it.
       */
      !(definiteClauses ++ preExistingDefiniteClauses)
        .exists(dc => (dc.clause subsumes definiteClause.clause) && (definiteClause.clause subsumes dc.clause)) match {
        case true => definiteClauses += definiteClause
        case false =>
      }
    }

    Success(definiteClauses)
  }

  /**
    * Object holding constants for clause type.
    */
  object ClauseType extends Enumeration {
    type ClauseType = Value
    val HORN = Value
    val CONJUNCTION = Value
    val BOTH = Value
  }
}