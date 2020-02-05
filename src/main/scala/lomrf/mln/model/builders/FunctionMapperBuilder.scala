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

import gnu.trove.TCollections
import gnu.trove.map.hash.TIntObjectHashMap
import lomrf.logic.{ Constant, FunctionMapping }
import lomrf.mln.model.{ AtomIdentityFunction, FunctionMapper, FunctionMapperDefaultImpl }
import scala.collection.{ breakOut, mutable }
import scala.language.implicitConversions
import lomrf.util.collection.trove.TroveConversions._

/**
  * Function mapper builder.
  *
  * @param identityFunction an atom identity function
  */
final class FunctionMapperBuilder(identityFunction: AtomIdentityFunction)
  extends mutable.Builder[(Vector[String], String), FunctionMapper] { self =>

  private var dirty = false

  private var args2Value = new TIntObjectHashMap[String]()

  /**
    * Copies all function mappings to new collections in case the result function has
    * been called. That way the builder avoids side-effects, that is the resulting function
    * mappers behave like immutable collections.
    */
  private def copyIfDirty(): Unit = if (dirty) {
    val cp_args2Value = new TIntObjectHashMap[String](args2Value.size)
    cp_args2Value.putAll(args2Value)
    args2Value = cp_args2Value
    dirty = false
  }

  /**
    * Inserts the given mapping into the builder.
    *
    * @param args a vector of constant arguments
    * @param value a function return value
    */
  private def insert(args: Vector[String], value: String) {
    copyIfDirty()
    val id = identityFunction.encode(args)
    args2Value.putIfAbsent(id, value)
  }

  /**
    * Adds the given mapping into the builder.
    *
    * @param args a vector of constant arguments
    * @param value a function return value
    * @return a FunctionMapperBuilder instance
    */
  def +=(args: Vector[String], value: String): self.type = {
    insert(args, value)
    self
  }

  /**
    * Adds the given mapping into the builder.
    *
    * @param mapping a tuple of a vector of constant arguments and a function return value
    * @return a FunctionMapperBuilder instance
    */
  override def +=(mapping: (Vector[String], String)): self.type = {
    insert(mapping._1, mapping._2)
    self
  }

  /**
    * Creates a function mapper from the given function mappings.
    *
    * @return a FunctionMapper instance
    */
  def result(): FunctionMapper = {
    /*
     Mark builder as dirty. Then copy collections in order to avoid side effects
     in the resulting ConstantsSet if any more constants are added.
     */
    dirty = true
    new FunctionMapperDefaultImpl(identityFunction, TCollections.unmodifiableMap(args2Value))
  }

  /**
    * Clears the builder.
    */
  def clear() {
    args2Value = new TIntObjectHashMap[String]()
    dirty = false
  }

  object decoded extends Iterable[FunctionMapping] with Traversable[FunctionMapping] {

    override def iterator: Iterator[FunctionMapping] = new Iterator[FunctionMapping] {
      private val iterator = args2Value.iterator
      private val symbol = identityFunction.signature.symbol

      override def hasNext: Boolean = iterator.hasNext

      override def next: FunctionMapping = {
        val (id, retVal) = iterator.next
        val decoded: Vector[Constant] = identityFunction
          .decode(id)
          .getOrElse(throw new IllegalStateException())
          .map(Constant)(breakOut)

        FunctionMapping(retVal, symbol, decoded)
      }

      override def hasDefiniteSize: Boolean = iterator.hasDefiniteSize

      override def isEmpty: Boolean = iterator.isEmpty

      override def isTraversableAgain: Boolean = iterator.isTraversableAgain
    }
  }
}

object FunctionMapperBuilder {

  implicit def functionMappingConversion(fm: FunctionMapping): (Vector[String], String) = (fm.values, fm.retValue)

  /**
    * Creates a function mapper builder.
    *
    * @param identityFunction an atom identity function
    * @return a FunctionMapperBuilder instance
    */
  def apply(identityFunction: AtomIdentityFunction): FunctionMapperBuilder =
    new FunctionMapperBuilder(identityFunction)
}
