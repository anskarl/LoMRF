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
import scala.{collection => scol}
import scalaxy.loops._
import scala.language.postfixOps

/**
 * @author Anastasios Skarlatidis
 */

object Cartesian {
  def productAsStream[T](args: Iterable[Iterable[T]]): Stream[Iterable[T]] = {
    args.foldLeft(Stream.fill(1)(List[T]())) {
      (result, elements) => {
        result.flatMap((tuple: List[T]) => elements.map(e => e :: tuple))
      }
    }
  }

  def product[T](args: Iterable[Iterable[T]]): Iterable[Iterable[T]] = {
    args.foldLeft(List(List[T]())) {
      (result, elements) => {
        result.flatMap(tuple => elements.map(e => e :: tuple))
      }
    }
  }

  object CartesianIteratorList {

    def apply[T](sets: Iterable[Iterable[T]])(implicit m: Manifest[T]): Iterator[List[T]] = {

      val iterators = new Array[Iterator[T]](sets.size)
      val elements = new Array[T](sets.size)
      val indexedSets = new Array[Iterable[T]](sets.size)

      for ((s, idx) <- sets.view.zipWithIndex) {
        iterators(idx) = s.iterator
        elements(idx) = iterators(idx).next()
        indexedSets(idx) = s
      }
      new CartesianIteratorListImpl(indexedSets, iterators, elements)
    }

  }


  object CartesianIterator {
    def apply[T](sets: Iterable[Iterable[T]])(implicit m: Manifest[T]): Iterator[Seq[T]] = {

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

    def apply[K, T](sets: scol.Map[K, Iterable[T]]): Iterator[Seq[T]] = {

      val iterators = new mutable.HashMap[K, Iterator[T]]()
      val elements = new mutable.HashMap[K, T]()

      for ((k, v) <- sets.iterator) {
        val currIter = v.iterator
        iterators(k) = currIter
        elements(k) = currIter.next()
      }

      new CartesianIteratorMap(sets, iterators, elements)
    }

    def apply2[K, T](sets: scol.Map[K, Iterable[T]])(implicit m: Manifest[T]): CartesianIteratorMap2[K, T] = {


      val iterators = new Array[Iterator[T]](sets.size)
      val elements = new Array[T](sets.size)
      val indexedSets = new Array[Iterable[T]](sets.size)
      var index = Map[K, Int]()
      var indexToKeys = Map[Int, K]()

      for (((k, v), idx) <- sets.view.zipWithIndex) {
        index += (k -> idx)
        iterators(idx) = v.iterator
        elements(idx) = iterators(idx).next()
        indexedSets(idx) = v
        indexToKeys += (idx -> k)
      }

      new CartesianIteratorMap2(index, indexToKeys, indexedSets, iterators, elements)
    }


  }


  object CartesianIteratorMap {
    def apply[K: Manifest, T: Manifest](sets: scol.Map[K, Iterable[T]]): Iterator[Map[K, T]] = {
      val arrayKeys = new Array[K](sets.size)
      val arrayIterators = new Array[Iterator[T]](sets.size)
      val arrayElements = new Array[T](sets.size)
      val arrayIterables = new Array[Iterable[T]](sets.size)

      var idx = 0
      for ((k, v) <- sets.iterator) {
        arrayKeys(idx) = k
        arrayIterables(idx) = v
        arrayIterators(idx) = v.iterator
        arrayElements(idx) = arrayIterators(idx).next()
        idx += 1
      }

      new CartesianIteratorMapMapImpl(arrayKeys, arrayIterables, arrayIterators, arrayElements)
    }
  }

  private class CartesianIteratorSeqImpl[T](
                                             sets: Array[Iterable[T]],
                                             iterators: Array[Iterator[T]],
                                             elements: Array[T]) extends Iterator[Seq[T]] {
    private var has_next = true


    def next(): Seq[T] = {
      val result = elements.toList
      var currentIterator = iterators(0)

      if (currentIterator.hasNext) elements(0) = currentIterator.next()
      else {
        var idx = 0
        var stop = false

        while (!stop) {
          currentIterator = iterators(idx)
          if (currentIterator.hasNext) {
            for (i <- (0 until idx).optimized) {
              iterators(i) = sets(i).iterator
              elements(i) = iterators(i).next()
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

  private class CartesianIteratorListImpl[T](
                                              sets: Array[Iterable[T]],
                                              iterators: Array[Iterator[T]],
                                              elements: Array[T]) extends Iterator[List[T]] {
    private var has_next = true


    def next(): List[T] = {
      val result = elements.toList
      var currentIterator = iterators(0)

      if (currentIterator.hasNext) elements(0) = currentIterator.next()
      else {
        var idx = 0
        var stop = false

        while (!stop) {
          currentIterator = iterators(idx)
          if (currentIterator.hasNext) {
            for (i <- (0 until idx).optimized) {
              iterators(i) = sets(i).iterator
              elements(i) = iterators(i).next()
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

  class CartesianIteratorMap2[K, T](
                                     index: Map[K, Int],
                                     val indexToKeys: Map[Int, K],
                                     sets: Array[Iterable[T]],
                                     iterators: Array[Iterator[T]],
                                     elements: Array[T]) extends Iterator[Seq[T]] {


    private var has_next = true


    def next(): Seq[T] = {
      val result = elements.toList
      var currentIterator = iterators(0)

      if (currentIterator.hasNext) elements(0) = currentIterator.next()
      else {
        var idx = 1
        var stop = false

        while (!stop) {
          currentIterator = iterators(idx)
          if (currentIterator.hasNext) {
            for (i <- (0 until idx).optimized) {
              iterators(i) = sets(i).iterator
              elements(i) = iterators(i).next()
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

  private class CartesianIteratorMap[K, T](
                                            sets: scol.Map[K, Iterable[T]],
                                            iterators: mutable.Map[K, Iterator[T]],
                                            elements: mutable.Map[K, T]) extends Iterator[Seq[T]] {
    private var has_next = true
    private val k0 = sets.head._1

    def next(): Seq[T] = {

      val result = elements.values.toSeq

      var currentIterator = iterators(k0)

      if (currentIterator.hasNext) elements(k0) = currentIterator.next()
      else {
        val keysIter = sets.keysIterator
        var stop = false
        while (!stop) {
          val key = keysIter.next()
          currentIterator = iterators(key)
          if (currentIterator.hasNext) {
            for (currKey <- sets.keySet.view.takeWhile(k => k != key)) {
              iterators(currKey) = sets(currKey).iterator
              elements(currKey) = iterators(currKey).next()
            }
            elements(key) = currentIterator.next()
            stop = true
          }
          else if (!keysIter.hasNext && !currentIterator.hasNext) {
            stop = true
            has_next = false
          }
        }
      }
      result
    }

    def hasNext = has_next
  }


  /**
   * Iterating over Cartesian products
   */
  private class CartesianIteratorMapMapImpl[K, T](
                                                    aKeys: Array[K],
                                                    aIterables: Array[Iterable[T]],
                                                    aIterators: Array[Iterator[T]],
                                                    aElements: Array[T]) extends Iterator[Map[K, T]] {

    private val arrayLength = aKeys.length
    private var has_next = true

    def hasNext = has_next

    def next(): Map[K, T] = {
      var result = Map[K,T]()

      var i = 0
      while(i < arrayLength){
        result = result + (aKeys(i) -> aElements(i))
        i += 1
      }

      var stop = false
      var idx = 0

      while(!stop && (idx < arrayLength)){
        if(aIterators(idx).hasNext){
          var i = 0
          while(i < idx){
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

      //if(!stop && idx == arrayLength) has_next = false

      has_next = stop || idx != arrayLength

      result
    }
  }

}