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

package lomrf.util

import java.util

import lomrf.logic.Variable
import lomrf.mln.model.ConstantsSet

import scala.{collection => scol}
import scalaxy.streams.optimize
import scala.language.postfixOps

/**
 *
 */
object Cartesian {


  object CartesianIterator {

    def apply[T](sets: Iterable[Iterable[T]])(implicit m: Manifest[T]): Iterator[Array[T]] = {

      val iterators = new Array[Iterator[T]](sets.size)
      val elements = new Array[T](sets.size)
      val indexedSets = new Array[Iterable[T]](sets.size)

      for ((s, idx) <- sets.view.zipWithIndex) {
        iterators(idx) = s.iterator
        elements(idx) = iterators(idx).next()
        indexedSets(idx) = s
      }
      new CartesianIteratorSeqImpl(indexedSets, iterators, elements)
    }

    def apply(sets: scol.Map[Variable, Iterable[String]]): Iterator[Map[Variable, String]] = {

      val arrayIterables : Array[Iterable[String]] = sets.values.toArray
      val arrayIterators : Array[Iterator[String]] = arrayIterables.map(_.toIterator)

      val arrayKeys : Array[Variable] = sets.keys.toArray

      val valuesIterable : Iterable[String] = for { (k, v) <- sets } yield { v.mkString }
      val arrayElements : Array[String] = valuesIterable.toArray

      new CartesianIteratorMapImpl(arrayKeys, arrayIterables, arrayIterators, arrayElements)
    }

    def mkArithmetic(sets: Iterable[ConstantsSet]) = new CartesianIteratorArithmeticImpl(sets.map(_.size - 1))

    def mkArithmetic(sizes: Array[Int]) = new CartesianIteratorArithmeticImpl(sizes)


  }

  private class CartesianIteratorSeqImpl[T](
                                             sets: Array[Iterable[T]],
                                             iterators: Array[Iterator[T]],
                                             elements: Array[T]) extends Iterator[Array[T]] {
    private var has_next = true


    def next(): Array[T] = {
      val result = elements.clone()
      var currentIterator = iterators(0)

      if (currentIterator.hasNext) elements(0) = currentIterator.next()
      else {
        var idx = 0
        var stop = false

        while (!stop) {
          currentIterator = iterators(idx)
          if (currentIterator.hasNext) {

            optimize{
              for (i <- 0 until idx) {
                iterators(i) = sets(i).iterator
                elements(i) = iterators(i).next()
              }
            }

            elements(idx) = currentIterator.next()
            stop = true
          }
          else if (idx == iterators.length - 1 && !currentIterator.hasNext) {
            stop = true
            has_next = false
          }
          else idx += 1

        }
      }
      result
    }

    def hasNext = has_next
  }

  /**
   * Iterating over Cartesian products
   */
  private class CartesianIteratorMapImpl(aKeys: Array[Variable],
                                         aIterables: Array[Iterable[String]],
                                         aIterators: Array[Iterator[String]],
                                         aElements: Array[String]) extends Iterator[Map[Variable, String]] {

    private val arrayLength = aKeys.length
    private var has_next = true

    def hasNext = has_next

    def next(): Map[Variable, String] = {
      var result = Map[Variable, String]()
      var i = 0
      while (i < arrayLength) {
        result = result + (aKeys(i) -> aElements(i))
        i += 1
      }

      var stop = false
      var idx = 0

      while (!stop && (idx < arrayLength)) {
        if (aIterators(idx).hasNext) {
          var i = 0
          while (i < idx) {
            aIterators(i) = aIterables(i).iterator
            aElements(i) = aIterators(i).next()
            i += 1
          }
          aElements(idx) = aIterators(idx).next()
          stop = true
        }
        else {
          idx += 1
        }
      }

      has_next = stop || idx != arrayLength

      result

    }
  }


  /**
   * Prototype implementation for fast and lightweight Cartesian products over domain indexes (represented by
   * inverted counters).
   *
   * <p>
   * Each entry in the '''elementDomainSizes''' represents is the size of the corresponding domain. Consider,
   * for example, that the domain of persons is '''persons={Anna, Bob}''' and the domain of time is '''time={1,...,10}'''. 
   * Assume that the ordering of the elements is {persons, time}. The initial elements will be '''{2, 10}''', since the
   * sizes of the domains 'persons' and 'time' are 2 and 10, respectively.
   * </p>
   *
   * <p>
   * Beginning from the index of the last entry of each element in the  '''elementDomainSizes''', i.e., size - 1, the
   * iterator gives an array of indexes. In the above example, the first iteration will give the array '''{1, 9}'''. The
   * second iteration will give the array '''{0,9}''', the third '''{1,8}''' and the last will always an array of zeroes
   * '''{0,0}'''.
   * </p>
   *
   * 
   * @param elementDomainSizes The domain size of each individual element.
   */
  private[util] class CartesianIteratorArithmeticImpl(elementDomainSizes: Iterable[Int]) extends Iterator[Array[Int]] {

    private val lengths = elementDomainSizes.toArray
    private val elements = util.Arrays.copyOf(lengths, lengths.length)
    private var has_next = true

    def hasNext = has_next


    def next(): Array[Int] = {
      val result = new Array[Int](elements.length)
      System.arraycopy(elements, 0, result, 0, elements.length )

      var stop = false
      var idx = 0

      while(!stop && (idx < elements.length)){
        if(elements(idx) > 0) {
          if(idx > 0) System.arraycopy(lengths, 0, elements, 0, idx)
          elements(idx) -= 1
          stop = true
        }
        else idx += 1
      }

      has_next = stop || idx != elements.length

      result
    }
  }

}
