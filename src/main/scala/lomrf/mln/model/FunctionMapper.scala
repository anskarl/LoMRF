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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.mln.model

import gnu.trove.TCollections
import gnu.trove.map.TIntObjectMap
import gnu.trove.map.hash.TIntObjectHashMap
import lomrf.logic.{Constant, FunctionMapping}

import scala.collection.mutable
import auxlib.trove.TroveConversions._

import scala.collection.breakOut
import scala.language.implicitConversions
import scala.util.Try

trait FunctionMapper extends Serializable {

  def apply(args: Vector[String]): String

  def get(args: Vector[String]): Option[String]
}

object FunctionMapper {
  def apply(fb: FunctionMapperBuilder): FunctionMapper = fb.result()

  def apply(idf: AtomIdentityFunction, args2Value: TIntObjectMap[String]): FunctionMapper = new FunctionMapperDefaultImpl(idf, args2Value)

  def apply(func: Vector[String] => String): FunctionMapper = new FunctionMapperSpecialImpl(func)
}

final class FunctionMapperDefaultImpl(identityFunction: AtomIdentityFunction, args2Value: TIntObjectMap[String]) extends FunctionMapper {

  def apply(args: Vector[String]): String = {
    val id = identityFunction.encode(args)
    args2Value.get(id)
  }

  def get(args: Vector[String]): Option[String] = {
    val id = identityFunction.encode(args)
    val result = args2Value.get(id)
    if (result eq null) None else Some(result)
  }

  override def toString = s"FunctionMapperDefaultImpl{signature:= ${identityFunction.signature}, size:=${args2Value.size()}"

}

final class FunctionMapperBuilder(identityFunction: AtomIdentityFunction)
  extends mutable.Builder[(Vector[String], String), FunctionMapper]
  with Traversable[(Int, String)] with Iterable[(Int, String)] {
  self =>

  private var dirty = false
  private var args2Value = new TIntObjectHashMap[String]()

  def decode(t: (Int, String)): Try[(IndexedSeq[String], String)] = {
    identityFunction.decode(t._1).map(v => (v, t._2))
  }

  override def +=(elems: (Vector[String], String)): self.type = {
    insert(elems._1, elems._2)
    self
  }

  def +=(args: Vector[String], value: String): self.type = {
    insert(args, value)
    self
  }

  private def insert(args: Vector[String], value: String) {
    checkDirty()
    val id = identityFunction.encode(args)
    args2Value.putIfAbsent(id, value)
  }

  private def checkDirty(): Unit = {
    if (dirty) {
      //copy
      val cp_args2Value = new TIntObjectHashMap[String](args2Value.size)
      cp_args2Value.putAll(args2Value)
      args2Value = cp_args2Value
      dirty = false
    }
  }

  def result(): FunctionMapper = {
    dirty = true
    new FunctionMapperDefaultImpl(identityFunction, TCollections.unmodifiableMap(args2Value))
  }

  def clear() {
    args2Value = new TIntObjectHashMap[String]()
    dirty = false
  }

  override def foreach[U](f: ((Int, String)) => U): Unit = {
    val iterator = args2Value.iterator()
    while (iterator.hasNext) {
      iterator.advance()
      f(iterator.key(), iterator.value())
    }
  }

  override def iterator: Iterator[(Int, String)] = args2Value.iterator()

  object decoded extends Iterable[FunctionMapping] with Traversable[FunctionMapping] {

    override def iterator: Iterator[FunctionMapping] = new Iterator[FunctionMapping] {
      private val iter = self.iterator
      private val symbol = identityFunction.signature.symbol

      override def hasNext: Boolean = iter.hasNext

      override def next(): FunctionMapping = {
        val (id, retVal) = iter.next()
        val decoded: Vector[Constant] = identityFunction
          .decode(id)
          .getOrElse(throw new IllegalStateException())
          .map(Constant)(breakOut)

        FunctionMapping(retVal, symbol, decoded)
      }

      override def hasDefiniteSize: Boolean = iter.hasDefiniteSize

      override def isEmpty: Boolean = iter.isEmpty

      override def isTraversableAgain: Boolean = iter.isTraversableAgain
    }
  }

}

object FunctionMapperBuilder {

  implicit def functionMappingConv(fm: FunctionMapping): (Vector[String], String) = (fm.values, fm.retValue)


  def apply(identityFunction: AtomIdentityFunction) = new FunctionMapperBuilder(identityFunction)
}

final class FunctionMapperSpecialImpl(func: Vector[String] => String) extends FunctionMapper {

  def apply(args: Vector[String]) = func(args)

  def get(args: Vector[String]): Option[String] = Some(func(args))

  override def toString = s"FunctionMapperSpecialImpl{f:= $func}"

}
