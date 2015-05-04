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
 * self program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * self program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with self program.  If not, see <http://www.gnu.org/licenses/>.
 */

package lomrf.mln.model

import lomrf.logic.{Formula, AtomSignature}
import lomrf.util.ConstantsSetBuilder


/**
 * Knowledge base builder with fluent interface pattern
 */
final class KBBuilder { self =>

  private var dirty = false

  private var _constantBuilders = Map.empty[String, ConstantsSetBuilder]

  private var _predicateSchema = Map.empty[AtomSignature, Vector[String]]

  private var _functionSchema = Map.empty[AtomSignature, (String, Vector[String])]

  private var _formulas = Set.empty[Formula]

  private var _dynamicPredicates = Map.empty[AtomSignature, Vector[String] => Boolean]

  private var _dynamicFunctions = Map.empty[AtomSignature, Vector[String] => String]

  def withConstantBuilders(input: Map[String, ConstantsSetBuilder]): self.type ={
    _constantBuilders = input
    self
  }

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
    dirty = true
    new KB(_constantBuilders, _predicateSchema, _functionSchema, _formulas, _dynamicPredicates, _dynamicFunctions)
  }

  object constants {

    private def copyIfDirty(): Unit ={
      if(self.dirty) {
        _constantBuilders = _constantBuilders.map{case (k, v) => k -> v.copy()}
        dirty = false
      }
    }

    def apply(key: String) = _constantBuilders(key)

    def apply(): Map[String, ConstantsSetBuilder] = _constantBuilders

    def update(input: Map[String, ConstantsSetBuilder]): self.type ={
      _constantBuilders = input
      dirty = false
      self
    }

    def += (key: String, value: String): self.type ={
      copyIfDirty()

      _constantBuilders.get(key) match {
        case Some(builder) => builder += value
        case _ => _constantBuilders += (key -> ConstantsSetBuilder(value))
      }

      self
    }

    def += (entry: (String, String)): self.type = constants += (entry._1, entry._2)

    def ++= (entry: (String, Iterable[String])): self.type ={
      copyIfDirty()

      val (key, values) = entry

      _constantBuilders.get(key) match {
        case Some(builder) => builder ++= values
        case _ => _constantBuilders += (key -> ConstantsSetBuilder(values))
      }

      self
    }

    def += (key: String): self.type ={
      copyIfDirty()

      _constantBuilders.get(key) match {
        case None => _constantBuilders += (key -> ConstantsSetBuilder())
        case _ => // do nothing
      }
      self
    }

    def ++= (keys: Iterable[String]): self.type ={
      copyIfDirty()

      for(key <- keys) {
        _constantBuilders.get(key) match {
          case None => _constantBuilders += (key -> ConstantsSetBuilder())
          case _ => // do nothing
        }
      }

      self
    }

    def clear(): Unit = _constantBuilders = Map.empty
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

  def apply(): KBBuilder = new KBBuilder

}