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

package lomrf.mln.learning.structure

import lomrf.logic._
import lomrf.logic.LogicOps._
import lomrf.logic.compile.{ LogicFormatter, NormalForm }
import lomrf.mln.learning.structure.ClauseConstructor.ClauseType.ClauseType
import lomrf.mln.learning.structure.hypergraph.HPath
import lomrf.mln.model._
import scala.util.{ Failure, Success, Try }
import spire.syntax.cfor._

/**
  * Clause constructor provides various methods for constructing clauses.
  */
object ClauseConstructor {

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
      val literals = path.ordering.map {
        case (atomID, signature) =>

          // Get the predicate schema of the current signature (predicate domains)
          val schema = predicates.get(signature) match {
            case Some(existingSchema) => existingSchema
            case None => return Failure(
              new NoSuchElementException(s"There is no predicate schema defined for signature '$signature'"))
          }

          // Get the constants for current ground atom
          val constants = evidence.db(signature).identity.decode(atomID) match {
            case Success(result)    => result
            case Failure(exception) => return Failure(exception)
          }

          val placeMarkers = modes(signature).placeMarkers
          var terms = Vector.empty[Term]

          cfor(0) (_ < constants.length, _ + 1) { i: Int =>
            val constant = constants(i)
            if (placeMarkers(i).constant) terms :+= Constant(constant)
            else if (constantsToVar.contains(constant)) terms :+= constantsToVar(constant)
            else {
              val variable = Variable(s"x${constantsToVar.size}", schema(i))
              constantsToVar += constant -> variable
              terms :+= variable
            }
          }

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
          case true  => clauses :+= clause
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
      preExistingDefiniteClauses: Set[WeightedDefiniteClause] = Set.empty[WeightedDefiniteClause]): Try[Set[WeightedDefiniteClause]] = {

    // Set of the created definite clauses
    var definiteClauses = Set.empty[WeightedDefiniteClause]

    paths.foreach { path =>

      // Map constants to variables in order to reuse the variables for the same constants
      var constantsToVar = Map.empty[String, Variable]

      // For each ground atom in the path replace constants with variables and add it as sub-formula
      val atoms = path.ordering.map {
        case (atomID, signature) =>

          // Get the predicate schema of the current signature
          val schema = predicates.get(signature) match {
            case Some(existingSchema) => existingSchema
            case None => return Failure(
              new NoSuchElementException(s"There is no predicate schema defined for signature '$signature'"))
          }

          // Get the constants for current ground atom
          val constants = evidence.db(signature).identity.decode(atomID) match {
            case Success(result)    => result
            case Failure(exception) => return Failure(exception)
          }

          val placeMarkers = modes(signature).placeMarkers
          var terms = Vector[Term]()

          cfor (0)(_ < constants.length, _ + 1) { i: Int =>
            val constant = constants(i)
            if (placeMarkers(i).constant) terms :+= Constant(constant)
            else if (constantsToVar.contains(constant)) terms :+= constantsToVar(constant)
            else {
              val variable = Variable(s"x${constantsToVar.size}", schema(i))
              constantsToVar += constant -> variable
              terms :+= variable
            }
          }

          AtomicFormula(signature.symbol, terms)
      }

      /*
        * It should introduce functions into the definite clause before checking if already exists,
        * because preExistingDefiniteClauses contain only functions
        */
      val definiteClause =
        LogicFormatter.WeightedDefiniteClauseFormatter.introduceFunctions(
          WeightedDefiniteClause(1.0, DefiniteClause(atoms.last, atoms.init.reduce(And))))

      /*
       * A definite clause is unique iff there is NO other definite clause subsuming it.
       */
      !(definiteClauses ++ preExistingDefiniteClauses)
        .exists(dc => (dc.clause subsumes definiteClause.clause) && (definiteClause.clause subsumes dc.clause)) match {
        case true  => definiteClauses += definiteClause
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
