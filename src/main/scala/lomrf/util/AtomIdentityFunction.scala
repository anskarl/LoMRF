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
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package lomrf.util

import scala.collection.mutable
import lomrf.logic._

/**
 * The AtomIdentityFunction represents a bijection between the groundings of an atom and  integer numbers. It is
 * extremely useful for encoding the entire ground Markov Network into a set of integer numbers, where each number is
 * uniquely represent a single grounding of the specified AtomSignature.
 *
 * This class provides fast and thread-safe functions for encoding ground predicates of the same FOL atom into unique
 * integer numbers, as well as for the opposite (decoding integers to ground predicates).
 *
 * @author Anastasios Skarlatidis
 */
final class AtomIdentityFunction private(
                                          val signature: AtomSignature,
                                          val startID: Int,
                                          val constantsAndStep: Array[(ConstantsSet, Int, Int, String)],
                                          val length: Int,
                                          val schema: Seq[String]) {

  import AtomIdentityFunction.IDENTITY_NOT_EXIST

  val endID = startID + length

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

  def encode(constants: scala.collection.IndexedSeq[String]): Int = {
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

  /*def encode(constantIds: Array[Int]): Int = {
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
  }*/

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

  def decode(id: Int): Option[Seq[String]] = {
    if (id >= startID && id <= endID) {
      val baseID = id - startID

      //Find all id literals
      var currentID = baseID
      val resultIDs = new Array[Int](constantsAndStep.length)
      //val lastIdx = resultIDs.length - 1
      var idx = resultIDs.length - 1

      while (idx > 0) {
        val sigma = constantsAndStep(idx)._3
        if (sigma <= currentID) {
          val modulo = currentID % sigma
          resultIDs(idx) = (currentID - modulo) / sigma
          currentID -= (resultIDs(idx) * sigma)
        }
        else {
          resultIDs(idx) = 0
        }
        idx -= 1
      }
      resultIDs(0) = currentID

      val result = for (i <- 0 until resultIDs.length) yield constantsAndStep(i)._1.apply(resultIDs(i))

      return Some(result)
    }
    None
  }

  def idsIterator: Iterator[Int] = idsRange.iterator

  def idsRange = startID to (startID + length)

  def matchesIterator(key: String, value: String): Iterator[Int] = matchesIterator(Map(key -> value))

  def matchesIterator(query: Map[String, String]): Iterator[Int] = {

    val length = constantsAndStep.length
    var rangesMap = Map[Int, Range]()
    var iteratorsMap = mutable.Map[Int, Iterator[Int]]()
    val values = new Array[Int](length)

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
        optimize{
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
            constants: Map[String, ConstantsSet],
            startID: Int): AtomIdentityFunction = {

    assert(startID > 0, "Atom identity function requires startID to be greater than zero and you gave: " + startID)

    val descriptor: Array[String] = schema.toArray

    var n = 0
    val constantsAndStep = new Array[(ConstantsSet, Int, Int, String)](descriptor.size)

    var length = 1
    val iterations = descriptor.length - 1
    var i = 0

    while (i <= iterations) {
      val symbol = descriptor(i)
      val currentDomain = constants(symbol)
      length *= currentDomain.size
      constantsAndStep(i) = if (i == 0) (currentDomain, 0, 1, symbol) else (currentDomain, n - 1, n, symbol)
      n = if (n == 0) currentDomain.size else n * currentDomain.size
      i += 1
    }

    new AtomIdentityFunction(signature, startID, constantsAndStep, length, schema)
  }
}