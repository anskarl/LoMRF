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

package lomrf.mln.model

import lomrf.logic.{Formula, AtomSignature}

/**
 * Knowledge base builder (fluent interface)
 */
final class KBBuilder(convertFunctions: Boolean = false) { self =>

  private var _predicateSchema = Map.empty[AtomSignature, Vector[String]]

  private var _functionSchema = Map.empty[AtomSignature, (String, Vector[String])]

  private var _formulas = Set.empty[Formula]

  private var _dynamicPredicates = Map.empty[AtomSignature, Vector[String] => Boolean]

  private var _dynamicFunctions = Map.empty[AtomSignature, Vector[String] => String]

  def withPredicateSchema(input: Map[AtomSignature, Vector[String]]): self.type ={
    _predicateSchema = input
    self
  }

  def withFunctionSchema(input: Map[AtomSignature, (String, Vector[String])]): self.type ={
    _functionSchema = input
    self
  }

  def withFormulas(input: Set[Formula]): self.type ={
    _formulas = input
    self
  }

  def withDynamicPredicates(input: Map[AtomSignature, Vector[String] => Boolean]): self.type ={
    _dynamicPredicates = input
    self
  }

  def withDynamicFunctions(input: Map[AtomSignature, Vector[String] => String]): self.type ={
    _dynamicFunctions = input
    self
  }
  
  def result(): KB = {
    val finalPredicateSchema =
      if(convertFunctions) _functionSchema.toPredicateSchema ++ _predicateSchema
      else _predicateSchema

    new KB(finalPredicateSchema, _functionSchema, _formulas, _dynamicPredicates, _dynamicFunctions)
  }

  object predicateSchema {

    def apply(key: AtomSignature) = _predicateSchema(key)

    def apply() = _predicateSchema

    def update(input: Map[AtomSignature, Vector[String]]): self.type ={
      _predicateSchema = input
      self
    }

    def += (key: AtomSignature, value: Vector[String]): self.type ={
      _predicateSchema += (key -> value)
      self
    }

    def += (entry: (AtomSignature, Vector[String])): self.type = {
      _predicateSchema += entry
      self
    }

    def ++= (entries: Iterable[(AtomSignature, Vector[String])]): self.type ={
      entries.foreach(predicateSchema += _)
      self
    }

    def clear(): Unit = _predicateSchema = Map.empty
  }

  object functionSchema {

    def apply(key: AtomSignature) = _functionSchema(key)

    def apply() = _functionSchema

    def += (key: AtomSignature, value: (String, Vector[String])): self.type = {
      _functionSchema += (key -> value)
      self
    }

    def += (entry: (AtomSignature, (String, Vector[String]))): self.type = {
      _functionSchema += entry
      self
    }

    def ++= (entries: Iterable[(AtomSignature,(String, Vector[String]))]): self.type ={
      entries.foreach(functionSchema += _)
      self
    }

    def clear(): Unit = _functionSchema = Map.empty
  }

  object formulas {

    def apply() = _formulas

    def += ( value: Formula): self.type ={
      _formulas += value
      self
    }

    def ++= (values: Iterable[Formula]): self.type ={
      _formulas ++= values
      self
    }

    def clear(): Unit = _formulas = Set.empty
  }

  object dynamicPredicates {

    def apply(key: AtomSignature) = _dynamicPredicates(key)

    def apply() = _dynamicPredicates

    def += (key: AtomSignature, value: Vector[String] => Boolean): self.type ={
      _dynamicPredicates += (key -> value)
      self
    }

    def += (entry: (AtomSignature, Vector[String] => Boolean)): self.type ={
      _dynamicPredicates += entry
      self
    }

    def ++= (entries: Iterable[(AtomSignature, Vector[String] => Boolean)]): self.type ={
      _dynamicPredicates ++= entries
      self
    }

    def clear(): Unit = _dynamicPredicates = Map.empty
  }

  object dynamicFunctions {

    def apply(key: AtomSignature) = _dynamicFunctions(key)

    def apply() = _dynamicFunctions

    def += (key: AtomSignature, value:  Vector[String] => String): self.type ={
      _dynamicFunctions += (key -> value)
      self
    }

    def += (entry: (AtomSignature,  Vector[String] => String)): self.type ={
      _dynamicFunctions += entry
      self
    }

    def ++= (entries: Iterable[(AtomSignature,  Vector[String] => String)]): self.type ={
      _dynamicFunctions ++= entries
      self
    }

    def clear(): Unit = _dynamicFunctions = Map.empty
  }
}

object KBBuilder {

  def apply(convertFunctions: Boolean = false): KBBuilder = new KBBuilder(convertFunctions)

}