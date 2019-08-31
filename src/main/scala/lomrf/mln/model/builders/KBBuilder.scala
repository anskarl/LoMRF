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

import lomrf.logic.{ AtomSignature, WeightedDefiniteClause, WeightedFormula }
import lomrf.mln.model._

/**
  * Knowledge base builder (fluent interface)
  */
final class KBBuilder(convertFunctions: Boolean = false) { self =>

  private var _predicateSchema = Map.empty[AtomSignature, Vector[String]]

  private var _functionSchema = Map.empty[AtomSignature, (String, Vector[String])]

  private var _formulas = Set.empty[WeightedFormula]

  private var _definiteClauses = Set.empty[WeightedDefiniteClause]

  private var _dynamicPredicates = Map.empty[AtomSignature, Vector[String] => Boolean]

  private var _dynamicFunctions = Map.empty[AtomSignature, Vector[String] => String]

  def withPredicateSchema(input: Map[AtomSignature, Vector[String]]): self.type = {
    _predicateSchema = input
    self
  }

  def withFunctionSchema(input: Map[AtomSignature, (String, Vector[String])]): self.type = {
    _functionSchema = input
    self
  }

  def withFormulas(input: Set[WeightedFormula]): self.type = {
    _formulas = input
    self
  }

  def withDefiniteClauses(input: Set[WeightedDefiniteClause]): self.type = {
    _definiteClauses = input
    self
  }

  def withDynamicPredicates(input: Map[AtomSignature, Vector[String] => Boolean]): self.type = {
    _dynamicPredicates = input
    self
  }

  def withDynamicFunctions(input: Map[AtomSignature, Vector[String] => String]): self.type = {
    _dynamicFunctions = input
    self
  }

  def result(): KB = {
    val finalPredicateSchema =
      if (convertFunctions) _functionSchema.toPredicateSchema ++ _predicateSchema
      else _predicateSchema

    new KB(finalPredicateSchema, _functionSchema, _dynamicPredicates, _dynamicFunctions, _formulas, _definiteClauses)
  }

  object predicateSchema {

    def apply(key: AtomSignature) = _predicateSchema(key)

    def apply() = _predicateSchema

    def update(input: Map[AtomSignature, Vector[String]]): self.type = {
      _predicateSchema = input
      self
    }

    def +=(key: AtomSignature, value: Vector[String]): self.type = {
      _predicateSchema += (key -> value)
      self
    }

    def +=(entry: (AtomSignature, Vector[String])): self.type = {
      _predicateSchema += entry
      self
    }

    def ++=(entries: Iterable[(AtomSignature, Vector[String])]): self.type = {
      entries.foreach(predicateSchema += _)
      self
    }

    def clear(): Unit = _predicateSchema = Map.empty
  }

  object functionSchema {

    def apply(key: AtomSignature) = _functionSchema(key)

    def apply() = _functionSchema

    def +=(key: AtomSignature, value: (String, Vector[String])): self.type = {
      _functionSchema += (key -> value)
      self
    }

    def +=(entry: (AtomSignature, (String, Vector[String]))): self.type = {
      _functionSchema += entry
      self
    }

    def ++=(entries: Iterable[(AtomSignature, (String, Vector[String]))]): self.type = {
      entries.foreach(functionSchema += _)
      self
    }

    def clear(): Unit = _functionSchema = Map.empty
  }

  object formulas {

    def apply() = _formulas

    def +=(value: WeightedFormula): self.type = {
      _formulas += value
      self
    }

    def ++=(values: Iterable[WeightedFormula]): self.type = {
      _formulas ++= values
      self
    }

    def clear(): Unit = _formulas = Set.empty
  }

  object definiteClauses {

    def apply() = _definiteClauses

    def +=(value: WeightedDefiniteClause): self.type = {
      _definiteClauses += value
      self
    }

    def ++=(values: Iterable[WeightedDefiniteClause]): self.type = {
      _definiteClauses ++= values
      self
    }

    def clear(): Unit = _definiteClauses = Set.empty
  }

  object dynamicPredicates {

    def apply(key: AtomSignature) = _dynamicPredicates(key)

    def apply() = _dynamicPredicates

    def +=(key: AtomSignature, value: Vector[String] => Boolean): self.type = {
      _dynamicPredicates += (key -> value)
      self
    }

    def +=(entry: (AtomSignature, Vector[String] => Boolean)): self.type = {
      _dynamicPredicates += entry
      self
    }

    def ++=(entries: Iterable[(AtomSignature, Vector[String] => Boolean)]): self.type = {
      _dynamicPredicates ++= entries
      self
    }

    def clear(): Unit = _dynamicPredicates = Map.empty
  }

  object dynamicFunctions {

    def apply(key: AtomSignature) = _dynamicFunctions(key)

    def apply() = _dynamicFunctions

    def +=(key: AtomSignature, value: Vector[String] => String): self.type = {
      _dynamicFunctions += (key -> value)
      self
    }

    def +=(entry: (AtomSignature, Vector[String] => String)): self.type = {
      _dynamicFunctions += entry
      self
    }

    def ++=(entries: Iterable[(AtomSignature, Vector[String] => String)]): self.type = {
      _dynamicFunctions ++= entries
      self
    }

    def clear(): Unit = _dynamicFunctions = Map.empty
  }
}

object KBBuilder {

  def apply(convertFunctions: Boolean = false): KBBuilder = new KBBuilder(convertFunctions)

}
