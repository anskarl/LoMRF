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
  * Constants domain builder (fluent interface)
  */
final class ConstantsDomainBuilder extends mutable.Builder[(String, String), ConstantsDomain] { self =>

  private var constantBuilders = Map.empty[String, ConstantsSetBuilder]

  private var dirty = false

  def apply(key: String) = {
    constantBuilders(key)
  }

  def apply(): Map[String, ConstantsSetBuilder] = constantBuilders

  def of(key: String) = {
    constantBuilders.getOrElse(key, {
      copyIfDirty()
      val builder = ConstantsSetBuilder()
      constantBuilders += (key -> builder)
      builder
    })
  }

  def update(input: Map[String, ConstantsSetBuilder]): self.type = {
    constantBuilders = input
    dirty = false
    self
  }

  def get(key: String): Option[ConstantsSetBuilder] = constantBuilders.get(key)

  def getOrElse(key: String, default: => ConstantsSetBuilder): ConstantsSetBuilder = constantBuilders.getOrElse(key, default)

  def +=(key: String, value: Number): self.type = {
    copyIfDirty()

    addUnchecked(key, value.toString)

    self
  }

  def +=(key: String, value: String): self.type = {
    copyIfDirty()

    addUnchecked(key, value)

    self
  }

  private def addUnchecked(key: String, value: String): Unit = constantBuilders.get(key) match {
    case Some(builder) => builder += value
    case _             => constantBuilders += (key -> ConstantsSetBuilder(value))
  }

  override def +=(entry: (String, String)): self.type = self += (entry._1, entry._2)

  def ++=(entry: (String, Iterable[String])): self.type = {
    copyIfDirty()

    val (key, values) = entry

    constantBuilders.get(key) match {
      case Some(builder) => builder ++= values
      case _             => constantBuilders += (key -> ConstantsSetBuilder(values))
    }

    self
  }

  def addKey(key: String): self.type = {
    copyIfDirty()

    constantBuilders.get(key) match {
      case None => constantBuilders += (key -> ConstantsSetBuilder())
      case _    => // do nothing
    }
    self
  }

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

  def ++=(entries: Iterable[(String, String)]): self.type = {
    copyIfDirty()

    entries.foreach(entry => addUnchecked(entry._1, entry._2))

    self
  }

  override def clear(): Unit = constantBuilders = Map.empty

  override def result(): ConstantsDomain = {
    dirty = true
    constantBuilders.map(e => e._1 -> e._2.result())
  }

  private def copyIfDirty(): Unit = {
    if (self.dirty) {
      constantBuilders = constantBuilders.map { case (k, v) => k -> v.copy() }
      dirty = false
    }
  }

}

object ConstantsDomainBuilder {

  def apply(): ConstantsDomainBuilder = new ConstantsDomainBuilder()

  def apply(initial: Map[String, ConstantsSetBuilder]): ConstantsDomainBuilder = {
    val builder = new ConstantsDomainBuilder()
    builder() = initial
    builder
  }

  def from(initial: ConstantsDomain): ConstantsDomainBuilder = {

    val builder = new ConstantsDomainBuilder()

    for ((domainName, constantsDomain) <- initial)
      builder ++= (domainName, constantsDomain.iterator.toIterable)

    builder
  }
}
