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
import scala.annotation.tailrec
import scala.collection.immutable._

/**
 * <p>This class implements a specialized database for managing Cartesian products over tuples. The implementation,
 * does not stores all Cartesian product tuples explicitly. Instead, in dynamically modifies an index, that is a
 * Map of K (key) to a Map of (V (Value) -> ID (Integer)). The index, provides a structure that contains a
 * compressed view of the stored Cartesian products, as well as a structure for performing various searching,
 * modifying and constraint operations over the Cartesian products.</p>
 *
 * <br></br>
 *
 * <p>Assume, for example, that initially we insert the key 'X', that is the name of the column, which takes
 * the three values x0, x1 and x2. Therefore, the database view is given below:</p>
 *
 * <br></br>
 *
 * X <br/>
 * --- <br/>
 * x0 <br/>
 * x1 <br/>
 * x2 <br/>
 *
 * <br></br>
 *
 * <p>The database does not store explicitly this view in some data structure. All information is stored in the
 * index structure, as presented below: </p>
 *
 * <br></br>
 *
 * INDEX[ X -> (x0 -> Set(0), x1 -> Set(1), x2 -> Set(2)) ]
 *
 * @tparam K is the key type, used in order to identify the column
 * @tparam V is the value type, used for storing or generating the products of constants
 *
 */
final class TuplesDB[@specialized K, @specialized V] extends Logging {

  type IndexType = collection.mutable.HashMap[K, HashMap[V, HashSet[Int]]]

  private var tID = Int.MinValue
  private var tupleIDs = HashSet[Int]()
  private var lockedVariables = HashSet[K]()
  private val index = new IndexType
  private var keys = List[K]()

  /**
   * Remove the specified variable key from the index.
   */
  private def removeKey(key: K) = {
    index.remove(key)
    keys = keys.filterNot(k => k == key)
    lockedVariables = lockedVariables.filterNot(k => k == key)
  }

  /**
   * @param key the specified key to search for
   * @return a map of values to tuples identifiers
   */
  def apply(key: K) = index.get(key)

  /**
   * <p>Insert the specified key (of type K), that takes one constant value (of type V), and performs
   * a Cartesian product between the specified value and the already stored tuples (if any). The insertion
   * would be successful only if the key is not locked over some constraints (specific values). For example: <br/>
   * <br/>
   * Before insertion: <br/>
   * X<br/>
   * ---- <br/>
   * x0 <br/>
   * x1 <br/>
   * x2 <br/>
   * <br/>
   * After insertion of Y -> y0: <br/>
   * Y, X <br/>
   * ----- <br/>
   * y0 x0 <br/>
   * y0 x1 <br/>
   * y0 x2 <br/>
   *
   * @param key: key entity
   * @param value: value entity
   */
  def +=(key: K, value: V): this.type = {
    
      if (!lockedVariables.contains(key)) index.get(key) match {
  
      /*
       * If key does not exist and there are no tuples (first entry in the database)
       * or there are tuples from other keys.
       */
      case None =>
        if(tupleIDs.isEmpty) {
          tupleIDs += tID
          index(key) = HashMap(value -> HashSet(tID))
          tID += 1
        }
        else index(key) = HashMap(value -> tupleIDs)
        keys = key :: keys
  
      /*
       * If the key does exist
       */
      case Some(values) =>
        /*
         * A previous value for this key would have appeared in all
         * distinct combinations of the other keys values.
         */
        val distinctTupleIds = values.values.head
  
        // If value does not already exist for this key
        if(!values.contains(value) && keys.length > 1) {
          var newTupleIds = HashSet[Int]()
  
          for (i <- 0 until distinctTupleIds.size) {
            tupleIDs += tID
            newTupleIds += tID
            tID += 1
          }
          index(key) = values + (value -> newTupleIds)
  
          assert(distinctTupleIds.size == newTupleIds.size)
  
          /*
           * Find for all other keys the values that appear in the same distinct tuples
           * and for these values append the same new tuple id in their tuple ids set.
           */
          distinctTupleIds zip newTupleIds foreach { case (d_tid , n_tid) =>
            for {otherKey <- keys.filter(_ != key)
                 (otherValue, otherValueIds) <- index(otherKey)
                 if otherValueIds.contains(d_tid)} {
              index(otherKey) = index(otherKey) + (otherValue -> (otherValueIds + n_tid))
            }
          }
  
        }
        else if(!values.contains(value)) {
          tupleIDs += tID
          index(key) = values + (value -> HashSet(tID))
          tID += 1
        }
    }
    
    this
  }

  /**
   * Performs insert operation but for a collection of values.
   *
   * @param key: the key to insert the specified instantiations
   * @param values: a collection of values to insert for the specified key
   */
  def +=(key: K, values: Iterable[V]): this.type = {
    if (!lockedVariables.contains(key)) values.foreach( value => this += (key, value) )
    this
  }

  /**
   * <p>Delete a value from a key, and therefore remove all stored Cartesian products that
   * contain the specified value that is assigned into the given key. This operation also deletes
   * the key if the value given for deletion is the last one in the domain of this key. In case the
   * key is removed then it is also unlocked if it was locked over some constraints (specific values).<p/>
   *
   * <p>Assume, for example, the following database:<p/>
   *
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
   * <p>By removing the value x0 from key X, the resulting database will be as follows: <p/>
   *
   * Y, X <br/>
   * ----- <br/>
   * y0 x1 <br/>
   * y0 x2 <br/>
   * y1 x1 <br/>
   * y1 x2 <br/>
   * <br/>
   *
   * <p>Therefore, all tuples that contain the value x0 for the key X are removed, that
   * is tuples (y0, x0) and (y2, x0).<p/>
   *
   */
  def -=(key: K, value: V): this.type = {
    
    index.get(key) match {
      case Some(values) => values.get(value) match {

        case Some(ids) if values.size > 1 =>
          ids.foreach(id => tupleIDs -= id)
          index(key) = values - value // delete this value from index

        // if this is the last value of the key, then delete the key
        case Some(ids) if values.size == 1 => removeKey(key)

        case None => // there is nothing to delete, because this value does not exist
      }

      case None => // there is nothing to delete, because this key does not exist
    }
    
    this
  }

  /**
   * Performs delete operation but for a collection of values.
   *
   * @param key: the key to delete the specified instantiations
   * @param values: a collection of values to delete from the specified key
   */
  def -=(key: K, values: Iterable[V]): this.type = {
    values.foreach(value => this -= (key, value))
    this
  }

  /**
   * Delete all matched tuples to the given tuple or subset of a tuple.
   *
   * @param tuple a tuple or subset of a tuple
   */
  def deleteMatchedTuples(tuple: Map[K, V]) = getMatchedTupleIDs(tuple) match {
    case Some(ids) => ids.foreach { id =>
      tupleIDs -= id

      tuple.foreach { case (key, value) =>
        index.get(key) match {
          case Some(values) => values.get(value) match {
            case Some(valueIds) =>

              // if tuple ids set of a value becomes empty, then delete the specific value
              val result = valueIds - id
              if (result.nonEmpty) index(key) = values + (value -> result)
              else index(key) = values - value

            case None => // nothing to delete, because there is no such value
          }
            if (size == 0) clear() // if there no tuples remaining, clear the database

          case None => // nothing to delete, because there is no such key
        }
      }
    }

    case None => // nothing to delete, because there are no matched tuples
  }

  /**
   * Delete all tuples not matching the given tuple or subset of a tuple.
   *
   * @param tuple a tuple or subset of a tuple
   */
  def deleteNotMatchedTuples(tuple: Map[K, V]) = getMatchedTupleIDs(tuple) match {
    case Some(ids) =>

      val notMatchedIDs = tupleIDs diff ids

      if(notMatchedIDs.nonEmpty) {
        for {
          id <- tupleIDs
          if notMatchedIDs.contains(id)
          (key, values) <- index
          (value, valueIDs) <- values
          if valueIDs.contains(id)
        } {
          // if tuple ids set of a value becomes empty, then delete the specific value
          val result = valueIDs - id
          if (result.nonEmpty) index(key) = values + (value -> result)
          else index(key) = values - value
        }

        tupleIDs = tupleIDs intersect ids // keep only matched
      }
    // If not matched ids are empty, there is nothing to delete

    case None => clear() // all tuples does not match so delete them
  }

  /**
   * Unlocks all keys, in order to be able to insert any combination.
   */
  def allowEverything() = {
    lockedVariables = HashSet[K]()
  }

  /**
   * Allow only tuples in the database matching the one of the given
   * mappings. The usage of this method locks variables used in the mappings in order
   * to prevent the insertion of values other than the ones specified here. In order
   * to unlock the keys use the allowEverything() method.
   *
   * @param tuples a set of tuples, therefore each sequence must have the same size
   */
  def allowOnly(tuples: Map[K, Seq[V]]) = {

    val sizes = tuples.values.map(seq => seq.length)
    require(sizes.forall(size => sizes.head == size), fatal("Uneven tuples!"))

    // Insert each constant of each variable in the database and lock the variable
    tuples.foreach { case (key, values) =>
      this += (key, values.toSet)
      lockedVariables += key
    }

    var matchedIDs = Set[Int]()
    for(i <- tuples.values.head.indices) {
      var tuple = Map[K, V]()
      tuples.keySet.foreach { key => tuple += key -> tuples(key)(i) }
      val matched = getMatchedTupleIDs(tuple) match {
        case Some(ids) => ids
        case None => Set[Int]()
      }
      matchedIDs ++= matched
    }

    if(matchedIDs.nonEmpty) {
      val notMatchedIDs = tupleIDs diff matchedIDs

      for {
        id <- tupleIDs
        if notMatchedIDs.contains(id)
        (key, values) <- index
        (value, valueIDs) <- values
        if valueIDs.contains(id)
      } {
        val result = valueIDs - id
        if (result.nonEmpty) index(key) = values + (value -> result)
        else index(key) = values - value
      }

      tupleIDs = tupleIDs intersect matchedIDs // keep only matched
    }
    else clear()
  }

  /**
   * @return true if the given value exists in the domain of the
   *         specified key, otherwise false
   */
  def contains(key: K, value: V): Boolean = index.get(key) match {
    case Some(values) => values.contains(value)
    case None => false
  }

  /**
   * @return true if the given key exists, otherwise false
   */
  def contains(key: K): Boolean = index.contains(key)

  /**
   * Count tuples matching the specified mapping of key values.
   *
   * @param mapping the mapping of keys to values
   * @return the number of tuples matching the mapping
   */
  def countTuples(mapping: Map[K, V]) = getMatchedTupleIDs(mapping) match {
    case Some(ids) => ids.size
    case None => 0
  }

  /**
   * @return all stored keys in the database
   */
  def getKeys: List[K] = keys

  /**
   * @return true if the specified key is locked
   */
  def isLocked(v: K) = lockedVariables.contains(v)

  /**
   * @return the number of stored keys
   */
  def numberOfKeys = keys.length

  /**
   * @return the number of values for the specified key
   */
  def numberOfValues(key: K) = index.getOrElse(key, fatal("Key not found!")).size

  /**
   * @return the number of tuples in the database
   */
  def size = tupleIDs.size

  /**
   * Clear everything in the database
   */
  def clear() = {
    tupleIDs = HashSet[Int]()
    index.clear()
    tID = Integer.MIN_VALUE
    keys = List[K]()
    lockedVariables = HashSet[K]()
  }

  /**
   * Find all tuple ids that match the given mapping pairs of keys and values
   * if any exists.
   *
   * @param mapping the mapping of keys to values
   * @return all tuple ids matching the given mapping
   */
  private def getMatchedTupleIDs(mapping: Map[K, V]): Option[Set[Int]] = {

    // If no mappings are given there are no matched tuples
    if (mapping.isEmpty) return None

    val iterator =  index.keySet.intersect(mapping.keySet).iterator

    def hasNext = iterator.hasNext

    def getNext: (K, V) = {
      while (hasNext) {
        val key = iterator.next()
        mapping.get(key) match {
          case Some(value) => return (key, value)
          case None =>
        }
      }
      null
    }

    @tailrec
    def search(ids: Set[Int]): Option[Set[Int]] = {
      if (hasNext) {
        val (k, v) = getNext
        index(k).get(v) match {
          case Some(idx) => search(idx.intersect(ids))
          case None => None
        }
      }
      else if (ids.nonEmpty) Some(ids)
      else None
    }

    val (k, v) = getNext
    index(k).get(v) match {
      case Some(ids) => search(ids)
      case None => None
    }
  }

  /**
   * @return all tuples in vectors of values
   */
  def getTuples: Vector[Vector[V]] = {
    var tuples = Vector[Vector[V]]()
    val keys = getKeys.reverse

    tupleIDs.foreach { tid =>
      var tuple = Vector[V]()
      keys.foreach { key =>
        index(key).foreach { case (value, ids) =>
          if(ids.contains(tid)) tuple :+= value
        }
      }
      tuples :+= tuple
    }

    tuples
  }

  /**
   * <p>Transform database into a columnar representation<br/>
   *
   * tuples : 2 <br/>
   * tid : X | Y | Z <br/>
   * 1 : 5   4   8 <br/>
   * 2 : 1   0   5 <br/>
   */
  override def toString = {
    val columns = "tid : " + index.keySet.mkString(" | ")
    var tuples = Vector[Vector[V]]()
    val orderedIDs = tupleIDs.toVector.sorted

    val keys = index.keySet
    orderedIDs.foreach { tid =>
      var tuple = Vector[V]()
      keys.foreach { key =>
        index(key).foreach { case (value, ids) =>
          if(ids.contains(tid)) tuple :+= value
        }
      }
      tuples :+= tuple
    }

    "tuples : " + tuples.length + "\n" + columns + "\n" +
      (orderedIDs zip tuples.map(_.mkString("   ") + "\n")).map(pair => pair._1 + " : " + pair._2).mkString("")
  }

}
