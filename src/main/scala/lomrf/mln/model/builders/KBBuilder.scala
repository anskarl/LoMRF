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

package lomrf.mln.model.builders

import lomrf.logic.parser.KBParser
import lomrf.logic.{ AtomSignature, WeightedDefiniteClause, WeightedFormula }
import lomrf.mln.model._

/**
  * Knowledge base builder.
  *
  * @param convertFunctions convert functions to auxiliary predicates
  */
final class KBBuilder(convertFunctions: Boolean = false) { self =>

  private var _predicateSchema: PredicateSchema = Map.empty

  private var _functionSchema: FunctionSchema = Map.empty

  private var _dynamicPredicates: DynamicPredicates = Map.empty

  private var _dynamicFunctions: DynamicFunctions = Map.empty

  private var _formulas = Set.empty[WeightedFormula]

  private var _textFormulas = Set.empty[String]

  private var _definiteClauses = Set.empty[WeightedDefiniteClause]

  private var _textDefiniteClauses = Set.empty[String]

  /**
    * Creates a KB builder having the given predicate schema.
    *
    * @param schema a map from atom signatures to their argument domain names
    * @return a KBBuilder instance
    */
  def withPredicateSchema(schema: PredicateSchema): self.type = {
    _predicateSchema = schema
    self
  }

  /**
    * Creates a KB builder having the given function schema.
    *
    * @note Function signatures are essentially atom signatures.
    *
    * @param schema a map from function signatures to their return value domain name, argument domain names
    * @return a KBBuilder instance
    */
  def withFunctionSchema(schema: FunctionSchema): self.type = {
    _functionSchema = schema
    self
  }

  /**
    * Creates a KB builder having the given dynamic predicates.
    *
    * @param predicates a map from atom signatures to functions of the form '''Vector[String] => Boolean'''
    * @return a KBBuilder instance
    */
  def withDynamicPredicates(predicates: DynamicPredicates): self.type = {
    _dynamicPredicates = predicates
    self
  }

  /**
    * Creates a KB builder having the given dynamic functions.
    *
    * @note Function signatures are essentially atom signatures.
    *
    * @param functions a map from function signatures to functions of the form '''Vector[String] => String'''
    * @return a KBBuilder instance
    */
  def withDynamicFunctions(functions: DynamicFunctions): self.type = {
    _dynamicFunctions = functions
    self
  }

  /**
    * Creates a KB builder having the given weighted formulas.
    *
    * @param formulas a set of weighted formulas
    * @return a KBBuilder instance
    */
  def withFormulas(formulas: Set[WeightedFormula]): self.type = {
    _formulas = formulas
    self
  }

  /**
    * Creates a KB builder having the given weighted definite clauses.
    *
    * @param definiteClauses a set of weighted definite clauses
    * @return a KBBuilder instance
    */
  def withDefiniteClauses(definiteClauses: Set[WeightedDefiniteClause]): self.type = {
    _definiteClauses = definiteClauses
    self
  }

  /**
    * Creates a KB from the given schemas and formulas.
    *
    * @return a KB instance
    */
  def result(): KB = {
    val finalPredicateSchema =
      if (convertFunctions) _functionSchema.toPredicateSchema ++ _predicateSchema
      else _predicateSchema

    val parser = new KBParser(finalPredicateSchema, _functionSchema)
    _formulas ++= _textFormulas.map(parser.parseWeightedFormula)
    _definiteClauses ++= _textDefiniteClauses.map(parser.parseDefiniteClause)

    new KB(finalPredicateSchema, _functionSchema, _dynamicPredicates, _dynamicFunctions, _formulas, _definiteClauses)
  }

  object predicateSchema {

    /**
      * @return the stored predicate schema
      */
    def apply(): PredicateSchema = _predicateSchema

    /**
      * @param signature an atom signature
      * @return the argument domain names for the given atom signature
      */
    def apply(signature: AtomSignature): Vector[String] = _predicateSchema(signature)

    /**
      * Replaces the underlying predicate schema.
      *
      * @param schema a map from atom signatures to their argument domain names
      * @return a KBBuilder instance
      */
    def update(schema: PredicateSchema): self.type = {
      _predicateSchema = schema
      self
    }

    /**
      * Adds the given tuple to the predicate schema.
      *
      * @param signature an atom signature
      * @param args the argument domain names of the given atom signature
      * @return a KBBuilder instance
      */
    def +=(signature: AtomSignature, args: Vector[String]): self.type = {
      _predicateSchema += (signature -> args)
      self
    }

    /**
      * Adds the given tuple to the predicate schema.
      *
      * @param entry a tuple of an atom signature and its argument domain names
      * @return a KBBuilder instance
      */
    def +=(entry: (AtomSignature, Vector[String])): self.type = {
      _predicateSchema += entry
      self
    }

    /**
      * Adds all given tuples to the predicate schema.
      *
      * @param entries an iterable of atom signatures along their argument domain names
      * @return a KBBuilder instance
      */
    def ++=(entries: Iterable[(AtomSignature, Vector[String])]): self.type = {
      _predicateSchema ++= entries
      self
    }

    /**
      * Clears the stored predicate schema.
      */
    def clear(): Unit = _predicateSchema = Map.empty
  }

  object functionSchema {

    /**
      * @return the stored function schema
      */
    def apply(): FunctionSchema = _functionSchema

    /**
      * @note Function signatures are essentially atom signatures.
      *
      * @param signature a function signature
      * @return a tuple holding the return value and the argument domain names
      */
    def apply(signature: AtomSignature): (String, Vector[String]) = _functionSchema(signature)

    /**
      * Replaces the underlying function schema.
      *
      * @note Function signatures are essentially atom signatures.
      *
      * @param schema a map from function signatures to their return value domain name, argument domain names
      * @return a KBBuilder instance
      */
    def update(schema: FunctionSchema): self.type = {
      _functionSchema = schema
      self
    }

    /**
      * Adds the given tuple to the function schema.
      *
      * @note Function signatures are essentially atom signatures.
      *
      * @param signature a function signature
      * @param tuple a tuple of the return value and the argument domain names
      * @return a KBBuilder instance
      */
    def +=(signature: AtomSignature, tuple: (String, Vector[String])): self.type = {
      _functionSchema += (signature -> tuple)
      self
    }

    /**
      * Adds the given tuple to the function schema.
      *
      * @note Function signatures are essentially atom signatures.
      *
      * @param entry a tuple of a function signature along a return value and the argument domain names
      * @return a KBBuilder instance
      */
    def +=(entry: (AtomSignature, (String, Vector[String]))): self.type = {
      _functionSchema += entry
      self
    }

    /**
      * Adds all given tuples to the function schema.
      *
      * @note Function signatures are essentially atom signatures.
      *
      * @param entries an iterable of function signatures along their return value and argument domain names
      * @return a KBBuilder instance
      */
    def ++=(entries: Iterable[(AtomSignature, (String, Vector[String]))]): self.type = {
      _functionSchema ++= entries
      self
    }

    /**
      * Clears the stored function schema.
      */
    def clear(): Unit = _functionSchema = Map.empty
  }

  object dynamicPredicates {

    /**
      * @return the stored dynamic predicates
      */
    def apply(): DynamicPredicates = _dynamicPredicates

    /**
      * @param signature an atom signature
      * @return a function from a string vector to a boolean value
      */
    def apply(signature: AtomSignature): Vector[String] => Boolean = _dynamicPredicates(signature)

    /**
      * Replaces the underlying dynamic predicate schema.
      *
      * @param predicates a map from atom signatures to functions of the form '''Vector[String] => Boolean'''
      * @return a KBBuilder instance
      */
    def update(predicates: DynamicPredicates): self.type = {
      _dynamicPredicates = predicates
      self
    }

    /**
      * Adds the given tuple to the dynamic predicate schema.
      *
      * @param signature an atom signature
      * @param function a function from a string vector to a boolean value
      * @return a KBBuilder instance
      */
    def +=(signature: AtomSignature, function: Vector[String] => Boolean): self.type = {
      _dynamicPredicates += (signature -> function)
      self
    }

    /**
      * Adds the given tuple to the dynamic predicate schema.
      *
      * @param entry a tuple of an atom signature and a function from a string vector to a boolean value
      * @return a KBBuilder instance
      */
    def +=(entry: (AtomSignature, Vector[String] => Boolean)): self.type = {
      _dynamicPredicates += entry
      self
    }

    /**
      * Adds all given tuple to the dynamic predicate schema.
      *
      * @param entries an iterable of atom signatures along functions from a string vector to a boolean value
      * @return a KBBuilder instance
      */
    def ++=(entries: Iterable[(AtomSignature, Vector[String] => Boolean)]): self.type = {
      _dynamicPredicates ++= entries
      self
    }

    /**
      * Clears the stored dynamic predicates.
      */
    def clear(): Unit = _dynamicPredicates = Map.empty
  }

  object dynamicFunctions {

    /**
      * @return the stored dynamic functions
      */
    def apply(): DynamicFunctions = _dynamicFunctions

    /**
      * @note Function signatures are essentially atom signatures.
      *
      * @param signature a function signature
      * @return a function from a string vector to a string value
      */
    def apply(signature: AtomSignature): Vector[String] => String = _dynamicFunctions(signature)

    /**
      * Replaces the underlying dynamic function schema.
      *
      * @note Function signatures are essentially atom signatures.
      *
      * @param functions a map from function signatures to functions from a string vector to a string value
      * @return a KBBuilder instance
      */
    def update(functions: DynamicFunctions): self.type = {
      _dynamicFunctions = functions
      self
    }

    /**
      * Adds the given tuple to the dynamic function schema.
      *
      * @note Function signatures are essentially atom signatures.
      *
      * @param signature a function signature
      * @param function a function from a string vector to a string value
      * @return a KBBuilder instance
      */
    def +=(signature: AtomSignature, function: Vector[String] => String): self.type = {
      _dynamicFunctions += (signature -> function)
      self
    }

    /**
      * Adds the given tuple to the dynamic function schema.
      *
      * @note Function signatures are essentially atom signatures.
      *
      * @param entry a tuple of a function signature and a function from a string vector to a string value
      * @return a KBBuilder instance
      */
    def +=(entry: (AtomSignature, Vector[String] => String)): self.type = {
      _dynamicFunctions += entry
      self
    }

    /**
      * Adds all given tuples to the dynamic functions.
      *
      * @note Function signatures are essentially atom signatures.
      *
      * @param entries an iterable of function signatures along functions from a string vector to a string value
      * @return a KBBuilder instance
      */
    def ++=(entries: Iterable[(AtomSignature, Vector[String] => String)]): self.type = {
      _dynamicFunctions ++= entries
      self
    }

    /**
      * Clears the stored dynamic functions.
      */
    def clear(): Unit = _dynamicFunctions = Map.empty
  }

  object formulas {

    /**
      * @return the stored set of weighted formulas
      */
    def apply(): Set[WeightedFormula] = _formulas

    /**
      * Adds the given weighted formula to the KB.
      *
      * @param formula a weighted formula
      * @return a KBBuilder instance
      */
    def +=(formula: WeightedFormula): self.type = {
      _formulas += formula
      self
    }

    /**
      * Adds the all weighted formulas to the KB.
      *
      * @param formulas an iterable of weighted formulas
      * @return a KBBuilder instance
      */
    def ++=(formulas: Iterable[WeightedFormula]): self.type = {
      _formulas ++= formulas
      self
    }

    /**
      * Adds the weighted formula to the KB.
      *
      * @note The given string should contain exactly one formula.
      *
      * @param text a weighted formula as text
      * @return a KBBuilder instance
      */
    def parseFrom(text: String): self.type = {
      _textFormulas += text
      self
    }

    /**
      * Clears all stored weighted formulas.
      */
    def clear(): Unit = _formulas = Set.empty
  }

  object definiteClauses {

    /**
      * @return the stored set of weighted definite clauses
      */
    def apply(): Set[WeightedDefiniteClause] = _definiteClauses

    /**
      * Adds the given weighted definite clause to the KB.
      *
      * @param definiteClause a weighted definite clause
      * @return a KBBuilder instance
      */
    def +=(definiteClause: WeightedDefiniteClause): self.type = {
      _definiteClauses += definiteClause
      self
    }

    /**
      * Adds the all weighted definite clauses to the KB.
      *
      * @param definiteClauses an iterable of weighted definite clauses
      * @return a KBBuilder instance
      */
    def ++=(definiteClauses: Iterable[WeightedDefiniteClause]): self.type = {
      _definiteClauses ++= definiteClauses
      self
    }

    /**
      * Adds the weighted definite clause to the KB.
      *
      * @note The given string should contain exactly one definite clause.
      *
      * @param text a weighted definite clause as text
      * @return a KBBuilder instance
      */
    def parseFrom(text: String): self.type = {
      _textDefiniteClauses += text
      self
    }

    /**
      * Clears all stored weighted definite clauses.
      */
    def clear(): Unit = _definiteClauses = Set.empty
  }
}

object KBBuilder {

  /**
    * Creates a knowledge base builder.
    *
    * @param convertFunctions convert functions to auxiliary predicates
    */
  def apply(convertFunctions: Boolean = false): KBBuilder = new KBBuilder(convertFunctions)
}
