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

import lomrf.mln.model._
import scala.collection.mutable

/**
  * Constants domain builder.
  */
final class ConstantsDomainBuilder
  extends mutable.Builder[(String, String), ConstantsDomain] { self =>

  private var dirty = false

  private var constantBuilders = Map.empty[String, ConstantsSetBuilder]

  /**
    * Adds the given domain name, constant symbol tuple to the builder.
    *
    * @note In case the domain name does not exists, it also creates
    *       a corresponding constants set for that domain name.
    *
    * @param key a domain name
    * @param value a constant symbol
    */
  private def addUnchecked(key: String, value: String): Unit = constantBuilders.get(key) match {
    case Some(builder) => builder += value
    case _             => constantBuilders += (key -> ConstantsSetBuilder(value))
  }

  /**
    * Copies all domain constants set to new collections in case the result function has
    * been called. That way the builder avoids side-effects, that is the resulting constants
    * per domain behave like immutable collections.
    */
  private def copyIfDirty(): Unit = if (self.dirty) {
    constantBuilders = constantBuilders.map { case (key, value) => key -> value.copy() }
    dirty = false
  }

  /**
    * @return a map from domain names to constants set builder
    */
  def apply(): Map[String, ConstantsSetBuilder] = constantBuilders

  /**
    * @param key a domain name
    * @return the ConstantSetBuilder for the given domain name
    */
  def apply(key: String): ConstantsSetBuilder = constantBuilders(key)

  /**
    * @param key a domain name
    * @return an option value containing the ConstantSetBuilder for the given domain name,
    *         or None if the domain name does not exists
    */
  def get(key: String): Option[ConstantsSetBuilder] = constantBuilders.get(key)

  /**
    * @param key a domain name
    * @param default a default computation
    * @return the ConstantSetBuilder for the given domain name if it exists, otherwise
    *         the result of the default computation
    */
  def getOrElse(key: String, default: => ConstantsSetBuilder): ConstantsSetBuilder =
    constantBuilders.getOrElse(key, default)

  /**
    * Gets the constants set builder for a given domain name.
    *
    * @note If the domain name does not exist, it creates a builder
    *       for it and returns that builder.
    *
    * @param key a domain name
    * @return the ConstantSetBuilder for the given domain name
    */
  def of(key: String): ConstantsSetBuilder = {
    constantBuilders.getOrElse(key, {
      copyIfDirty()
      val builder = ConstantsSetBuilder()
      constantBuilders += (key -> builder)
      builder
    })
  }

  /**
    * Assigns the given constants set builders to their corresponding domain names.
    *
    * @param builders a map from domain names to constants set builders
    * @return a ConstantsDomainBuilder instance
    */
  def update(builders: Map[String, ConstantsSetBuilder]): self.type = {
    constantBuilders = builders
    dirty = false
    self
  }

  /**
    * Adds the numeric constant to the constants set of the given domain name.
    *
    * @param key a domain name
    * @param value a numeric constant
    * @return a ConstantsDomainBuilder instance
    */
  def +=(key: String, value: Number): self.type = {
    copyIfDirty()
    addUnchecked(key, value.toString)
    self
  }

  /**
    * Adds the constant symbol to the constants set of the given domain name.
    *
    * @param key a domain name
    * @param value a constant symbol
    * @return a ConstantsDomainBuilder instance
    */
  def +=(key: String, value: String): self.type = {
    copyIfDirty()
    addUnchecked(key, value)
    self
  }

  /**
    * Adds the constant symbol to the constants set of the given domain name.
    *
    * @param entry a tuple of a domain name and a constant symbol
    * @return a ConstantsDomainBuilder instance
    */
  override def +=(entry: (String, String)): self.type = {
    val (key, value) = entry
    self += (key, value)
  }

  /**
    * Add all given constant symbols to the constants set of the given domain name.
    *
    * @param entry a tuple of a domain name and an iterable of constant symbols
    * @return a ConstantsDomainBuilder instance
    */
  def ++=(entry: (String, Iterable[String])): self.type = {
    copyIfDirty()
    val (key, values) = entry
    constantBuilders.get(key) match {
      case Some(builder) => builder ++= values
      case _             => constantBuilders += (key -> ConstantsSetBuilder(values))
    }
    self
  }

  /**
    * Adds all given tuples to the constants domain builder.
    *
    * @param entries an iterable of domain names to constant symbol tuples
    * @return a ConstantsDomainBuilder instance
    */
  def ++=(entries: Iterable[(String, String)]): self.type = {
    copyIfDirty()
    entries.foreach(entry => addUnchecked(entry._1, entry._2))
    self
  }

  /**
    * Adds the given domain name to the builder (if it does not already exist)
    * and creates a corresponding constants set builder.
    *
    * @param key a domain name
    * @return a ConstantsDomainBuilder instance
    */
  def addKey(key: String): self.type = {
    copyIfDirty()
    constantBuilders.get(key) match {
      case None => constantBuilders += (key -> ConstantsSetBuilder())
      case _    => // do nothing
    }
    self
  }

  /**
    * Adds all given domain names to the builder (if they do not already exist)
    * and creates a constants set builder for each of them.
    *
    * @param keys an iterable of domain names
    * @return a ConstantsDomainBuilder instance
    */
  def addKeys(keys: Iterable[String]): self.type = {
    copyIfDirty()
    for (key <- keys) {
      constantBuilders.get(key) match {
        case None => constantBuilders += (key -> ConstantsSetBuilder())
        case _    => // do nothing
      }
    }
    self
  }

  /**
    * Creates a constants domain from the given constant symbols of each domain name.
    *
    * @return a ConstantsDomain instance
    */
  override def result(): ConstantsDomain = {
    dirty = true
    constantBuilders.map { case (key, value) => key -> value.result() }
  }

  /**
    * Clears the builder.
    */
  override def clear(): Unit = constantBuilders = Map.empty
}

object ConstantsDomainBuilder {

  /**
    * Creates an empty constants domain builder.
    *
    * @return an empty ConstantsDomainBuilder instance
    */
  def apply(): ConstantsDomainBuilder = new ConstantsDomainBuilder()

  /**
    * Creates a constants domain builder.
    *
    * @param initial a map from domain names to constants set builder
    * @return a ConstantsDomainBuilder instance
    */
  def apply(initial: Map[String, ConstantsSetBuilder]): ConstantsDomainBuilder = {
    val builder = new ConstantsDomainBuilder()
    builder() = initial
    builder
  }

  /**
    * Creates a constants domain builder.
    *
    * @see [[lomrf.mln.model.ConstantsSet]]
    *
    * @param constantsDomain a map from domain names to constants set
    * @return a ConstantsDomainBuilder instance
    */
  def from(constantsDomain: ConstantsDomain): ConstantsDomainBuilder = {
    val builder = new ConstantsDomainBuilder()
    for ((domainName, constantSet) <- constantsDomain)
      builder ++= (domainName, constantSet.iterator.toIterable)
    builder
  }
}
