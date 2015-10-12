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

import lomrf.logic._
import lomrf.mln.model.mrf.Constraint

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import scalaxy.streams.optimize

/**
 * The AtomIdentityFunction represents a bijection between the groundings of an atom and  integer numbers. It is
 * extremely useful for encoding the entire ground Markov Network into a set of integer numbers, where each number is
 * uniquely represent a single grounding of the specified AtomSignature.
 *
 * This class provides fast and thread-safe functions for encoding ground predicates of the same FOL atom into unique
 * integer numbers, as well as for the opposite (decoding integers to ground predicates).
 *
 *
 */
final class AtomIdentityFunction private(
                                          val signature: AtomSignature,
                                          val startID: Int,
                                          val constantsAndStep: Array[(ConstantsSet, Int, Int, String, Int)],
                                          val length: Int,
                                          val schema: Seq[String]) extends Serializable {

  import AtomIdentityFunction.IDENTITY_NOT_EXIST

  val endID = startID + length

  @transient lazy val indices = startID until endID

  def encode(atom: EvidenceAtom): Int = {
    var sum = startID
    var idx = 0
    for (c <- atom.terms) {
      val constantID = constantsAndStep(idx)._1.apply(c.toString)
      if (constantID == ConstantsSet.NO_ENTRY) return IDENTITY_NOT_EXIST
      val step = constantsAndStep(idx)._2
      val offset = constantID * step
      sum += (offset + constantID)
      idx += 1
    }
    sum
  }

  def encode(constants: Seq[String]): Int = {
    var sum = startID
    var idx = 0
    for (c <- constants) {
      val constantID = constantsAndStep(idx)._1.apply(c)
      if (constantID == ConstantsSet.NO_ENTRY) return IDENTITY_NOT_EXIST
      val step = constantsAndStep(idx)._2
      val offset = constantID * step
      sum += (offset + constantID)
      idx += 1
    }

    sum
  }

  def encode(constants: IndexedSeq[String]): Int = {
    var sum = startID
    var idx = 0
    while (idx < constants.length) {
      val constantID = constantsAndStep(idx)._1.apply(constants(idx))
      if (constantID == ConstantsSet.NO_ENTRY) return IDENTITY_NOT_EXIST
      val step = constantsAndStep(idx)._2
      val offset = constantID * step
      sum += (offset + constantID)
      idx += 1
    }

    sum
  }

  def encode(constants: Array[String]): Int = {
    var sum = startID
    var idx = 0
    while (idx < constants.length) {
      val constantID = constantsAndStep(idx)._1.apply(constants(idx))
      if (constantID == ConstantsSet.NO_ENTRY) return IDENTITY_NOT_EXIST
      val step = constantsAndStep(idx)._2
      val offset = constantID * step
      sum += (offset + constantID)
      idx += 1
    }

    sum
  }

  def encodeIndices(constantIds: Array[Int]): Int = {
    var sum = startID
    var idx = 0
    while (idx < constantIds.length) {
      val constantID = constantIds(idx)
      if (constantID == ConstantsSet.NO_ENTRY)
        return IDENTITY_NOT_EXIST
      val offset = constantID * constantsAndStep(idx)._2 // offset = id * step
      sum += (offset + constantID)
      idx += 1
    }

    sum
  }

  /**
   * Gives the ID (positive integer) of the corresponding constant ids. This is the fastest
   * encoding function, as it doesn't need to perform any lookup to the ConstantsSet for fetching
   * the id of the constant.
   *
   * @param indexes the array which its elements are pointing to the correct position in constantIds
   * @param constantIds the array of constant ids (assumed to have the correct ordering)
   * @return encoded number that uniquely corresponds to a grounding of the atom
   */
  def encode(indexes: Array[Int], constantIds: Array[Int]): Int = {

    var result = startID
    var i = 0
    var constantID = ConstantsSet.NO_ENTRY

    while (i < indexes.length) {

      constantID = constantIds(indexes(i))

      if (constantID == ConstantsSet.NO_ENTRY) return IDENTITY_NOT_EXIST

      val offset = constantID * constantsAndStep(indexes(i))._2 // offset = id * step

      result += (offset + constantID)

      i += 1
    }

    result
  }

  /**
   * <p>
   * Gives the ID (positive integer) of the corresponding grounding
   * for the specified atom. The grounding is computed with the help
   * of the specified function that maps Terms to Strings.
   * </p>
   * <p>
   * The positive integer represents the unique ID of the ground atom,
   * if the grounding cannot be computed, then it returns IDENTITY_NOT_EXIST.
   * For example, the special FOL function succ(int) that returns the successive
   * number of the given integer and the predicate HoldsAt(fluent, succ(time)).
   * </p>
   * <p>
   * Let time={0,...,100} and fluent={F1,...Fn}.
   * </p>
   * <p>
   * The grounding of HoldsAt(F1, succ(99)) is HoldsAt(F1,100) which is inside the bounds of time,
   * thus the encode(...) function will return the corresponding ID.
   * However, the grounding of HoldsAt(F1, succ(100)) cannot be determined, since
   * the resulting ground atom HoldsAt(F1, 101) is out of time bounds. Therefore,
   * the encode(...) function will return IDENTITY_NOT_EXIST.
   * </p>
   *
   * @return the corresponding ID if a valid grounding exists, IDENTITY_NOT_EXIST otherwise.
   */
  def encode(atom: AtomicFormula, f: Term => String): Int = {
    var sum = startID
    var idx = 0

    for (term <- atom.terms) {
      val set: ConstantsSet = constantsAndStep(idx)._1

      val constantID = set(f(term))
      if (constantID == ConstantsSet.NO_ENTRY)
        return IDENTITY_NOT_EXIST

      val step = constantsAndStep(idx)._2
      val offset = constantID * step
      sum += (offset + constantID)
      idx += 1
    }


    sum
  }

  def decode(atomID: Int): Try[IndexedSeq[String]] = {
    // check bounds
    if (atomID < startID || atomID >= endID)
      return Failure(new IndexOutOfBoundsException(s"The given id ($atomID) is out of bounds [$startID, $endID]"))

    val baseID = atomID - startID

    // Find all id literals
    var currentID = baseID
    val result = new Array[String](constantsAndStep.length)
    var idx = result.length - 1

    while (idx > 0) {
      val sigma = constantsAndStep(idx)._3
      val constatsSet = constantsAndStep(idx)._1

      if (sigma <= currentID) {
        val tmpID = (currentID - (currentID % sigma)) / sigma
        result(idx) = constatsSet(tmpID)
        currentID -= (tmpID * sigma)
      }
      else result(idx) = constatsSet(0)

      idx -= 1
    }
    val constatsSet = constantsAndStep(idx)._1
    result(idx) = constatsSet(currentID)

    Success(result)
  }

  def extract(id: Int): Try[Array[Int]] = {
    // check bounds
    if (id < startID || id >= endID)
      return Failure(new IndexOutOfBoundsException(s"The given atom id '$id' is out of bounds, thus cannot be decoded."))

    val baseID = id - startID

    // Find all id literals
    val result = new Array[Int](constantsAndStep.length)
    var currentID = baseID
    var idx = result.length - 1

    while (idx > 0) {
      val constAndStep = constantsAndStep(idx)
      val sigma = constAndStep._3
      val offset = constAndStep._5

      if (sigma <= currentID) {
        val localID = (currentID - (currentID % sigma)) / sigma
        result(idx) = offset + localID
        currentID -= (localID * sigma)
      }
      else result(idx) = offset // + (currentID=0)

      idx -= 1
    }

    val offset = constantsAndStep(idx)._5
    result(idx) = offset + currentID

    Success(result)
  }

  //def idsIterator: Iterator[Int] = idsRange.iterator

  //def idsRange = startID to (startID + length)

  def matchesIterator(key: String, value: String): Iterator[Int] = matchesIterator(Map(key -> value))

  def matchesIterator(query: Map[String, String]): Iterator[Int] = {

    val length = constantsAndStep.length
    var rangesMap = Map[Int, Range]()
    var iteratorsMap = mutable.Map[Int, Iterator[Int]]()
    val values = new Array[Int](length)

    optimize {
      for (idx <- 0 until length) {
        val constantSet = constantsAndStep(idx)._1
        val symbol = constantsAndStep(idx)._4
        query.get(symbol) match {
          case Some(constantValue) => values(idx) = constantSet(constantValue)
          case None =>
            val range = constantSet.idsRange
            val iterator = range.iterator
            rangesMap += (idx -> range)
            iteratorsMap += (idx -> iterator)
            values(idx) = iterator.next()
        }
      }
    }


    new MatchingIDsIterator(rangesMap, iteratorsMap, values)
  }


  private class MatchingIDsIterator(rangesMap: Map[Int, Range], iteratorsMap: mutable.Map[Int, Iterator[Int]], values: Array[Int]) extends Iterator[Int] {

    import scalaxy.streams.optimize


    private val _length = rangesMap.map(_._2.size).product
    private var counter = 0
    private var sum = -1

    override def length = _length

    override def size = length

    def hasNext = counter < _length

    def next(): Int = {
      if (counter < _length) {
        sum = startID
        optimize {
          for (idx <- 0 until values.length) {

            //1. encode
            val constantID = values(idx)
            val step = constantsAndStep(idx)._2
            val offset = constantID * step
            sum += (offset + constantID)

            //2. advance
            iteratorsMap.get(idx) match {
              case Some(currentIter) =>
                values(idx) =
                  if (currentIter.hasNext) {
                    currentIter.next()
                  } else {
                    val nouvaIter = rangesMap(idx).iterator
                    iteratorsMap(idx) = nouvaIter
                    nouvaIter.next()
                  }
              case _ => //do nothing
            }

          }
        }
        counter += 1
      }
      sum
    }
  }

}

object AtomIdentityFunction {

  val IDENTITY_NOT_EXIST = 0

  def apply(signature: AtomSignature,
            schema: Seq[String],
            constants: ConstantsDomain,
            startID: Int): AtomIdentityFunction = {

    assert(startID > 0, "Atom identity function requires startID to be greater than zero and you gave: " + startID)

    val descriptor: Array[String] = schema.toArray

    var n = 0
    val constantsAndStep = new Array[(ConstantsSet, Int, Int, String, Int)](descriptor.length)


    var constOffsetMap = Map[String, Int]()

    var currentOffset = 0
    for (((k, v), idx) <- constants.zipWithIndex) {
      constOffsetMap += (k -> currentOffset)
      currentOffset += v.size
    }

    var length = 1
    val iterations = descriptor.length - 1
    var i = 0

    while (i <= iterations) {
      val symbol = descriptor(i)
      val currentDomain = constants(symbol)
      length *= currentDomain.size
      constantsAndStep(i) = if (i == 0) (currentDomain, 0, 1, symbol, constOffsetMap(symbol)) else (currentDomain, n - 1, n, symbol, constOffsetMap(symbol))
      n = if (n == 0) currentDomain.size else n * currentDomain.size
      i += 1
    }

    new AtomIdentityFunction(signature, startID, constantsAndStep, length, schema)
  }

  def decodeLiteral(literal: Int)(implicit mln: MLN): Try[String] = {

    val atomID = math.abs(literal)
    val signature = mln.space.signatureOf(atomID)
    val idf = mln.space.identities(signature)

    val negation = if (literal < 0) "!" else ""

    idf.decode(atomID).map(x => s"$negation${signature.symbol}(${x.mkString(",")})")
  }

  def decodeAtom(literal: Int)(implicit mln: MLN): Try[String] = {

    val atomID = math.abs(literal)
    val signature = mln.space.signatureOf(atomID)
    val idf = mln.space.identities(signature)

    idf.decode(atomID).map(x => s"${signature.symbol}(${x.mkString(",")})")
  }

  def decodeFeature(feature: Constraint, hardWeight: Double = 0)(implicit mln: MLN): Try[String] = {

    val buffer = new StringBuilder()

    val weight = feature.getWeight

    if (weight.isPosInfinity) {
      if (hardWeight != 0) buffer.append(hardWeight.toString)
      buffer.append(' ')
    }
    else if (!weight.isNaN) {
      buffer.append(feature.getWeight.toString)
      buffer.append(' ')
    }

    optimize {
      for (i <- 0 until feature.literals.length; tryLiteral = decodeLiteral(feature.literals(i))) tryLiteral match {
        case Success(litTXT) =>
          buffer.append(litTXT)
          if (i != feature.literals.length - 1) buffer.append(" v ")

        case f: Failure[String] => return f
      }
    }


    if (feature.getWeight.isInfinite && hardWeight != 0) buffer.append('.')


    Success(buffer.result())
  }
}

object AtomIdentityFunctionOps {

  implicit class WrappedGroundLiteral(val literal: Int) extends AnyVal {

    def decodeLiteral(implicit mln: MLN) = AtomIdentityFunction.decodeLiteral(literal)

    def decodeAtom(implicit mln: MLN) = AtomIdentityFunction.decodeAtom(literal)

  }

  implicit class WrappedConstraint(val feature: Constraint) extends AnyVal {

    def decodeFeature(hardWeight: Double = 0)(implicit mln: MLN): Try[String] = {
      AtomIdentityFunction.decodeFeature(feature, hardWeight)
    }

  }

}
