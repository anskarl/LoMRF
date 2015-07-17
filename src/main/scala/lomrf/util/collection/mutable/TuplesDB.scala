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

package lomrf.util.collection.mutable

import auxlib.log.Logging

import scala.collection.{mutable=> sm}
import annotation.tailrec



/**
 * <p>This class implements a specialized database for managing Cartesian products over tuples.
 * The implementation, does not stores all Cartetian product tuples explicitly (like TuplesDB).
 * Instead, in dynamically modifies an index, that is a Map of V (key) to a Map of (C (Constant) -> ID (Integer)).
 * The index, provides a structure that contains a compressed view of the stored Cartesian products, as well as
 * a structure for performing various searching and modifying operations over the Cartetian products.</p>
 *
 * <p>Assume, for example, that initially we insert the variable 'X', that is the name of the column, which takes
 * the three constant literals, that is x0, x1 and x2. Therefore, the database view is given below:</p>
 * X <br/>
 * -----<br/>
 * x0 <br/>
 * x1 <br/>
 * x2 <br/>
 * <p>The database does not store explicitly this view in some data structure. All information is stored in the
 * index structure, as presented below: </p>
 *
 * INDEX{ <br/>
 * X -> (x0 -> Set(0), x1 -> Set(1), x2 -> Set(2))<br/>
 * }<br/>
 *
 * <p>Thereafter, we insert the variable 'Y', which takes three constant literals, that is y0, y1 and y2.
 * As a result, the database will produce the following Cartesian products:</p>
 *
 * Y,  X <br/>
 * -------- <br/>
 * y0, x0 <br/>
 * y0, x1 <br/>
 * y0, x2 <br/>
 * y1, x0 <br/>
 * y1, x1 <br/>
 * y1, x2 <br/>
 * y2, x0 <br/>
 * y2, x1 <br/>
 * y2, x2 <br/>
 *
 * <p>After the insertion the index will grow by one new Variable, the variable 'Y', and all sets will be updated
 * with the corresponding ID literals.<p/>
 *
 * INDEX{ <br/>
 * X -> ( x0 -> Set(0, 6, 3), x1 ->Set(1, 8, 5), x2 ->Set(2, 4, 7) ), <br/>
 * Y -> ( y0 -> Set(0, 2, 1), y1 -> Set(4, 3, 5), y2 -> Set(6, 7, 8)) <br/>
 * } <br/>
 *
 * <p> More specifically, the first tuple (y0, x0), is represented with ID = 0. Therefore, x0 and y0 are both associated
 * with ID = 0 inside the index structure, that is x0 -> Set(0, ...) and y0 -> Set(0, ...).  Similarly, the tuple (y2,x0)
 * is represented with ID = 6. Consequently, x0 and y2 are associated with ID = 6, that is is x0 -> Set(0, 6, ...)
 * and y2 -> Set(6, ...).
 * <p/>
 *
 *
 *
 *
 * @tparam V: is the type key to use, in order to identify the column (represents a variable)
 * @tparam C: is the type if the constant to use, for storing or generating the products of literals
 */
class TuplesDB[@specialized V, @specialized C] extends Logging {
  type IndexType = sm.HashMap[V, sm.HashMap[C, sm.HashSet[Int]]]

  private var tupleIDs = sm.HashSet[Int]()
  private val index = new IndexType()
  private var ID = Integer.MIN_VALUE
  //private var ID = 0
  private var keys = List[V]()

  def getIndex: collection.Map[V, collection.Map[C, collection.Set[Int]]] = index

  /**
   * Insert the specified Variable (V), that takes one Constant value (C), and perform
   * a Cartesian product between V and the stored variables (if any). For example: <br/>
   * <br/>
   * Before insertion: <br/>
   * X< br/>
   * ---- <br/>
   * x0 <br/>
   * x1 <br/>
   * x2 <br/>
   * <br/>
   * insert Y -> y0 <br/>
   * After insertion: <br/>
   * Y, X <br/>
   * ----- <br/>
   * y0 x0 <br/>
   * y0 x1 <br/>
   * y0 x2 <br/>
   *
   * @param v: variable name
   * @param c: constant value
   */
  def *=(v: V, c: C) {
    //Check if we already added this variable 'v',
    //we only accept new variables.
    index.get(v) match {
      case None => {
        //Its the first time that we add this variable
        if (tupleIDs.isEmpty) {
          //its the first variable
          tupleIDs += ID
          //update index
          index(v) = sm.HashMap(c -> sm.HashSet(ID))
          //Increase ID
          ID += 1
        } else {
          //update index
          val setIds = sm.HashSet[Int]()
          tupleIDs.foreach(id => setIds += id)
          index(v) = sm.HashMap(c -> setIds)
        }
        //update keys
        keys = v :: keys
      }
      case _ => //do nothing, because the variable is already indexed
    }
  }

  /**
   * Insert the specified Variable (V), that takes a set of Constant literals (C), and perform
   * a Cartesian product between V and the stored variables (if any). For example: <br/>
   * <br/>
   * Before insertion: <br/>
   * X< br/>
   * ---- <br/>
   * x0 <br/>
   * x1 <br/>
   * x2 <br/>
   * <br/>
   * insert Y -> (y0, y1) <br/>
   * After insertion: <br/>
   * Y, X <br/>
   * ----- <br/>
   * y0 x0 <br/>
   * y0 x1 <br/>
   * y0 x2 <br/>
   * y1 x0 <br/>
   * y1 x1 <br/>
   * y1 x2 <br/>
   *
   * @param v: variable name
   * @param constants: a Set of constant literals
   */
  def *=(v: V, constants: collection.Set[C]) {
    //Check if we already added this variable 'v',
    //we only accept new variables.
    index.get(v) match {
      //Its the first time that we add this variable
      case None if (!constants.isEmpty) => {
        if (tupleIDs.isEmpty) {
          //its the first variable
          index(v) = sm.HashMap[C, sm.HashSet[Int]]()
          for (c <- constants) {
            tupleIDs += ID
            //update index
            index(v)(c) = sm.HashSet(ID)
            //Increase ID
            ID += 1
          }
        } else {
          //add the Cartesian product

          //prepare index
          index(v) = sm.HashMap[C, sm.HashSet[Int]]()

          val iter = constants.iterator

          //-------------------------------------------------->
          // STEP 1: Increase tuples with the first element   |
          //--------------------------------------------------|
          val first = iter.next
          val setIdsFirst = sm.HashSet[Int]()
          tupleIDs.foreach(id => setIdsFirst += id)
          //update index for the first constant
          index(v)(first) = setIdsFirst

          //END: STEP 1 --------------------------------------<

          //-------------------------------------------------->
          // STEP 2: Create and update the cartesian product  |
          // for the rest elements                            |
          //--------------------------------------------------|
          val newIDs = sm.HashSet[Int]()

          while (iter.hasNext) {
            val currentConstant = iter.next
            for (id <- tupleIDs) {
              //STEP 2.a: update index for all the previous keys
              for {key <- keys
                   (constant, constantIDs) <- index(key)
                   if constantIDs.contains(id)} {
                constantIDs += ID
              }
              //STEP 2.b: update index for the new key
              index(v).get(currentConstant) match {
                case Some(iids) => iids += ID
                case None => index(v)(currentConstant) = sm.HashSet[Int](ID)
              }
              newIDs += ID
              ID += 1 //produce next id value
            }

          }
          tupleIDs ++= newIDs
          //END: STEP 2 --------------------------------------<
        }
        //update keys:
        keys = v :: keys
      }
      case _ => //do nothing, because the variable is already indexed
    }
  }

  /**
   * <p>Remove the specified variable from the index. Consider, for example, the following database:</p>
   * <br/>
   * Y, X <br/>
   * ----- <br/>
   * y0 x0 <br/>
   * y0 x1 <br/>
   * y0 x2 <br/>
   * y1 x0 <br/>
   * y1 x1 <br/>
   * y1 x2 <br/>
   * <br/>
   *
   * <p>If variable 'X' is removed, then the resulting database will be as follows:</p>
   * <br/>
   * Y <br/>
   * --- <br/>
   * y0 <br/>
   * y0 <br/>
   * y0 <br/>
   * y1 <br/>
   * y1 <br/>
   * y1 <br/>
   * <br/>
   *
   * <p>Note that column Y remains exatly the same as it was, i.e the duplicates remain</p>
   *
   * @param v: the variable to remove all its instantiations.
   */
  def ~=(v: V) {
    index.get(v) match {
      case Some(c) => {
        if (index.size == 1) {
          this.clear
        } else {
          index.remove(v)
          keys = keys.filterNot(p => p == v)
        }
      }
      case None =>
    }
  }

  /**
   * <p>Delete a constant from a variable, and therefore remove all stored Cartesian products that
   * contain the specified constant that is assigned into the given variable.<p/>
   *
   * <p>Assume, for example, the following database: <p/>
   * <br/>
   * Y, X <br/>
   * ----- <br/>
   * y0 x0 <br/>
   * y0 x1 <br/>
   * y0 x2 <br/>
   * y1 x0 <br/>
   * y1 x1 <br/>
   * y1 x2 <br/>
   * <br/>
   * <p>By removing the constant x0 from variable X, the resulting database will be as follows: <p/>
   * <br/>
   * Y, X <br/>
   * ----- <br/>
   * y0 x1 <br/>
   * y0 x2 <br/>
   * y1 x1 <br/>
   * y1 x2 <br/>
   * <br/>
   * <p>Therefore, all tuples that contain the constant 'x0' for the variable 'X' are removed, that is (y0,x0) and (y2,x0).<p/>
   *
   *
   * @param v: variable name
   * @param c: constant value
   */
  def -=(v: V, c: C) {
    index.get(v) match {
      case Some(constants) => {
        constants.get(c) match {
          case Some(ids) => {
            ids.foreach(id => tupleIDs.remove(id))
            constants.remove(c) // finally remove this constant from index
          }
          case None => //there is nothing to remove
        }
        if (constants.size == 0) this.clear //removeKey(v)
      }
      case None => //there is nothing to remove
    }
  }

  /**
   * Performs the same operation with @see this.-=(v:V, c: C), but for a collection of constant literals.
   *
   * @param v: the variable to remove the specified instantiations
   * @param items: a collection of constant literals to remove from the specified variable
   */
  def -=(v: V, items: collection.Iterable[C]) {
    index.get(v) match {
      case Some(constants) => {
        for (c <- items) {
          constants.get(c) match {
            case Some(ids) => {
              ids.foreach(id => tupleIDs.remove(id))
              constants.remove(c) // finally remove this constant from index
            }
            case None => //there is nothing to remove
          }
        } //end for
        if (constants.size == 0) this.clear //removeKey(v)
      }
      case None => //there is nothing to remove
    }
  }

  //def -=(v: V, constants: collection.Iterable[C]) {constants.foreach(c => -=(v, c))}


  def deleteMatchedTuples(tupleToRemove: collection.Map[V, C]) {

    getMatchedIDs(tupleToRemove) match {
      case Some(ids) => {
        ids.foreach(id => tupleIDs.remove(id))
        for ((v, c) <- tupleToRemove) {
          index.get(v) match {
            case Some(constants) => {
              constants.get(c) match {
                case Some(constantIDs) => ids.foreach(id => constantIDs.remove(id))
                case None => //ignore
              }
              if (constants.size == 0) removeKey(v)
            }
            case None => //ignore
          }
        }
      }
      case None => //nothing to remove
    }
  }

  def deleteMatchedTuples(mapping: collection.Map[V, Int], values: List[C]) {

    getMatchedIDs(mapping, values) match {
      case Some(ids) => {
        ids.foreach(id => tupleIDs.remove(id))
        for ((v, idx) <- mapping) {
          index.get(v) match {
            case Some(constants) => {
              constants.get(values(idx)) match {
                case Some(constantIDs) => ids.foreach(id => constantIDs.remove(id))
                case None => //ignore
              }
              if (constants.size == 0) removeKey(v)
            }
            case None => //ignore
          }
        }
      }
      case None => //nothing to remove
    }
  }

  def deleteNotMatchedTuples(mapping: collection.Map[V, Int], values: collection.Seq[List[C]]) {

    //collect all the ID literals, which we are going to keep
    val idValuesToKeep = sm.HashSet[Int]()
    for (v <- values) {
      //debug("v:="+v)
      getMatchedIDs(mapping, v) match {
        case Some(ids) => {
          //debug("matches: "+ids)
          ids.foreach(id => idValuesToKeep += id)
        }
        case None =>
      }
    }

    //update index
    for {
      id <- tupleIDs
      if (!idValuesToKeep.contains(id))
      (variable, constants) <- index
      (constant, constantIDs) <- constants
      if (constantIDs.contains(id))} {
      constantIDs.remove(id)
    }
    //update tupleIDs
    tupleIDs = idValuesToKeep
  }

  def growOnlyFor(inTuples: collection.Map[V, Int],
                  alone: collection.Map[V, Int],
                  values: collection.Seq[List[C]]) {
    if (inTuples.size == 0) {
      //add all literals
      //prepare index
      for ((k, v) <- alone) index(k) = sm.HashMap[C, sm.HashSet[Int]]()
      keys = alone.keySet.toList
      for (tuple <- values) {
        tupleIDs += ID
        //update index
        for ((k, idx) <- alone) {
          index(k).get(tuple(idx)) match {
            case Some(constantIDs) => constantIDs += ID
            case None => index(k)(tuple(idx)) = sm.HashSet[Int](ID)
          }
        }
        ID += 1
      }
    } else {
      val idValuesToKeep = sm.HashSet[Int]()
      //prepare index for new keys
      for ((k, v) <- alone) index(k) = sm.HashMap[C, sm.HashSet[Int]]()

      for (tuple <- values) {
        getMatchedIDs(inTuples, tuple) match {
          case Some(ids) => {
            for (id <- ids) {
              if (!idValuesToKeep.contains(id)) {
                idValuesToKeep += id
                //update index only for the new literals
                for ((k, idx) <- alone) {
                  index(k).get(tuple(idx)) match {
                    case Some(indexes) => indexes += id
                    case None => index(k)(tuple(idx)) = sm.HashSet[Int](id)
                  }
                }
              }
              else {
                //grow for a new ID
                idValuesToKeep += ID
                //update index for the old literals
                for {k <- keys
                     (constant, constantIDs) <- index(k)
                     if (constantIDs.contains(id))} {
                  constantIDs += ID
                }
                //update index for the new literals
                for ((k, idx) <- alone) {
                  index(k).get(tuple(idx)) match {
                    case Some(indexes) => indexes += ID
                    case None => index(k)(tuple(idx)) = sm.HashSet[Int](ID)
                  }
                }
                ID += 1
              }
            }
          }
          case _ =>
        }
      }
      //fatal("STOP! "+idValuesToKeep)
      //clear the index with the rest
      for {
        id <- tupleIDs
        (variable, constants) <- index
        (constant, constantIDs) <- constants
        if (!idValuesToKeep.contains(id))} {
        constantIDs.remove(id)
      }
      tupleIDs = idValuesToKeep
      keys = alone.keySet.toList ::: keys

    }
  }

  def contains(v: V, c: C): Boolean = {
    index.get(v) match {
      case Some(constants) => constants.contains(c)
      case None => false
    }
  }

  def countTuples(mapping: collection.Map[V, C]): Int = {
    getMatchedIDs(mapping) match {
      case Some(ids) => ids.size
      case None => 0
    }
  }

  def countTuples(mapping: collection.Map[V, Int], values: Seq[C]): Int = {
    getMatchedIDs(mapping, values) match {
      case Some(ids) => ids.size
      case None => 0
    }
  }

  /**
   * Smart breath first search
   */
  def contains(mapping: collection.Map[V, C]): Boolean = {
    if (mapping.size == 1) return contains(mapping.head._1, mapping.head._2)
    else if (mapping.size == 0) return false

    val iter = index.keysIterator

    def hasNext = iter.hasNext

    def getNext: (V, C) = {
      while (hasNext) {
        val key = iter.next
        mapping.get(key) match {
          case Some(c) => return (key, c)
          case None =>
        }
      }
      return null
    }

    @tailrec
    def search(ids: collection.Set[Int]): Boolean = {
      if (hasNext) {
        val (v, c) = getNext
        index(v).get(c) match {
          case Some(idx) => search(idx.intersect(ids))
          case None => false
        }
      } else {
        return ids.size > 0
      }
    }

    val (v, c) = getNext
    index(v).get(c) match {
      case Some(x) => search(x)
      case None => false
    }
  }

  /**
   * Smart breath first search
   */
  def contains(mapping: collection.Map[V, Int], values: Seq[C]): Boolean = {
    if (mapping.size == 1) return contains(mapping.head._1, values(mapping.head._2))
    else if (mapping.size == 0) return false

    val iter = index.keysIterator

    def hasNext = iter.hasNext

    def getNext: (V, C) = {
      while (hasNext) {
        val key = iter.next
        mapping.get(key) match {
          case Some(c) => return (key, values(c))
          case None =>
        }
      }
      return null
    }

    @tailrec
    def search(ids: collection.Set[Int]): Boolean = {
      if (hasNext) {
        val (v, c) = getNext
        index(v).get(c) match {
          case Some(idx) => search(idx.intersect(ids))
          case None => false
        }
      } else {
        return ids.size > 0
      }
    }

    val (v, c) = getNext
    index(v).get(c) match {
      case Some(x) => search(x)
      case None => false
    }
  }


  def valuesIterator: Iterator[List[C]] = new Iterator[List[C]] {
    val idsIterator = tupleIDs.iterator

    def next(): List[C] = {
      val id = idsIterator.next
      val result = for {
        key <- keys
        (constant, constantIDs) <- index(key)
        if constantIDs.contains(id)
      } yield constant

      return result
    }

    def hasNext = idsIterator.hasNext
  }

  def valuesIterator(keyList: List[V]) = new Iterator[List[C]] {
    require(keyList.forall(k => index.contains(k)))
    //private val reversedKeys = keyList.reverse

    val idsIterator = tupleIDs.iterator

    def next(): List[C] = {
      val id = idsIterator.next
      val result = for {
        key <- keyList
        (constant, constantIDs) <- index(key)
        if constantIDs.contains(id)
      } yield constant

      return result
    }

    def hasNext = idsIterator.hasNext
  }

  def containsKey(v: V): Boolean = index.contains(v)

  def variables: List[V] = keys

  def size = tupleIDs.size

  def clear {
    tupleIDs.clear
    index.clear
    ID = Integer.MIN_VALUE
    keys = List[V]()
  }

  private def removeID(id: Int) {
    tupleIDs.remove(id)
    for {
      (variable, constants) <- index
      (constant, constantIDs) <- constants
      if (constantIDs.contains(id))} {
      constantIDs.remove(id)
    }
  }

  private def removeKey(k: V) {
    index.remove(k)
    keys = keys.filterNot(p => p == k)
  }

  private def getMatchedIDs(mapping: collection.Map[V, C]): Option[collection.Set[Int]] = {
    if (mapping.size == 1) {
      if (contains(mapping.head._1, mapping.head._2)) return Some(tupleIDs)
    }
    else if (mapping.size == 0) return None

    val iter = index.keysIterator

    def hasNext = iter.hasNext

    def getNext: (V, C) = {
      while (hasNext) {
        val key = iter.next
        mapping.get(key) match {
          case Some(c) => return (key, c)
          case None =>
        }
      }
      return null
    }

    @tailrec
    def search(ids: collection.Set[Int]): Option[collection.Set[Int]] = {
      if (hasNext) {
        val (v, c) = getNext
        index(v).get(c) match {
          case Some(idx) => search(idx.intersect(ids))
          case None => None
        }
      } else {
        return if (ids.size > 0) Some(ids) else None
      }
    }

    val (v, c) = getNext
    index(v).get(c) match {
      case Some(x) => search(x)
      case None => None
    }
  }

  //NOTE: contains a new implementation for iteration across consistent keys
  //NOTE: contains correct implementation when mapping.size == 1
  private def getMatchedIDs(mapping: collection.Map[V, Int], values: Seq[C]): Option[collection.Set[Int]] = {
    if (mapping.size == 1) {
      return index.get(mapping.head._1) match {
        case Some(constants) => constants.get(values(mapping.head._2)) match {
          case Some(ids) => Some(ids)
          case None => None
        }
        case None => None
      }
    }
    else if (mapping.size == 0) return None


    val iterator = new Iterator[(V, C)] {
      private val matchedKeysIterator = keys.view.filter(k => mapping.contains(k)).iterator

      def next(): (V, C) = {
        val nextKey = matchedKeysIterator.next
        mapping.get(nextKey) match {
          case Some(idx) => (nextKey, values(idx))
          case None => null
        }
      }

      def hasNext = matchedKeysIterator.hasNext
    }

    @tailrec
    def search(ids: collection.Set[Int]): Option[collection.Set[Int]] = {
      if (iterator.hasNext) {
        val (v, c) = iterator.next
        index(v).get(c) match {
          case Some(idx) => search(idx.intersect(ids))
          case None => None
        }
      } else {
        return if (ids.size > 0) Some(ids) else None
      }
    }
    if (iterator.hasNext) {
      val (v, c) = iterator.next
      index(v).get(c) match {
        case Some(x) => search(x)
        case None => None
      }
    } else None
  }

}