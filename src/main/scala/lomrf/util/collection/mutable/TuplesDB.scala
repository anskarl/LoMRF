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
 * Map of K (key) to a Map of (V (Constant) -> ID (Integer)). The index, provides a structure that contains a
 * compressed view of the stored Cartesian products, as well as a structure for performing various searching,
 * modifying and constraint operations over the Cartesian products.</p>
 *
 * <br></br>
 *
 * <p>Assume, for example, that initially we insert the variable 'X', that is the name of the column, which takes
 * the three constant literals, that is x0, x1 and x2. Therefore, the database view is given below:</p>
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
 * @tparam K is the key type, used in order to identify the column (represents a variable)
 * @tparam V is the constant type, used for storing or generating the products of literals
 *
 */
final class TuplesDB[@specialized K, @specialized V] extends Logging {

  type IndexType = collection.mutable.HashMap[K, HashMap[V, HashSet[Int]]]

  private var tID = Int.MinValue
  private var tupleIDs = HashSet[Int]()
  private var lockedVariables = HashSet[K]()
  private val index = new IndexType
  private var keys = List[K]()


  def apply(key: K) = index.get(key)

  /**
   * <p>Insert the specified Variable (K), that takes one Constant value (V), and perform
   * a Cartesian product between K and the stored variables (if any). The insertion would be
   * successful only if the variable is not locked over some constraint. For example: <br/>
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
   * @param key: variable entity
   * @param value: constant entity
   */
  def +=(key: K, value: V): this.type = {
      if (!lockedVariables.contains(key)) index.get(key) match {
  
      /*
       * If variable does not exist and there are no tuples (first entry in the database)
       * or there are tuples from other variables.
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
       * If the variable does exist
       */
      case Some(constants) =>
        /*
         * A previous constant for this variable would have appeared in all
         * distinct combinations of the other variables constants.
         */
        val distinctTupleIds = constants.values.head
  
        // If constant does not already exist for this variable
        if(!constants.contains(value) && keys.length > 1) {
          var newTupleIds = HashSet[Int]()
  
          for (i <- 0 until distinctTupleIds.size) {
            tupleIDs += tID
            newTupleIds += tID
            tID += 1
          }
          index(key) = constants + (value -> newTupleIds)
  
          assert(distinctTupleIds.size == newTupleIds.size)
  
          /*
           * Find for all other variables the constants that appear in the same distinct tuples
           * and for these constants append the same new tuple id in their tuple ids set.
           */
          distinctTupleIds zip newTupleIds foreach { case (d_tid , n_tid) =>
            for {variable <- variables.filter(_ != key)
                 (constant, constantIDs) <- index(variable)
                 if constantIDs.contains(d_tid)} {
              index(variable) = index(variable) + (constant -> (constantIDs + n_tid))
            }
          }
  
        }
        else if(!constants.contains(value)) {
          tupleIDs += tID
          index(key) = constants + (value -> HashSet(tID))
          tID += 1
        }
    }
    
    this
  }

  /**
   * Performs insert operation but for a collection of constant literals.
   *
   * @param key: the variable to insert the specified instantiations
   * @param constants: a collection of constant literals to insert from the specified variable
   */
  def +=(key: K, constants: Iterable[V]): this.type = {
    if (!lockedVariables.contains(key)) constants.foreach( c => this += (key, c) )

    this
  }

  /**
   * <p>Delete a constant from a variable, and therefore remove all stored Cartesian products that
   * contain the specified constant that is assigned into the given variable. This operation also deletes
   * the variable if the constant given for deletion is the last one in the domain of this variable. In
   * case the variable is removed then it is also unlocked in case there was locked over some constraint.<p/>
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
   * <p>By removing the constant x0 from variable X, the resulting database will be as follows: <p/>
   *
   * Y, X <br/>
   * ----- <br/>
   * y0 x1 <br/>
   * y0 x2 <br/>
   * y1 x1 <br/>
   * y1 x2 <br/>
   * <br/>
   *
   * <p>Therefore, all tuples that contain the constant x0 for the variable X are removed, that
   * is tuples (y0, x0) and (y2, x0).<p/>
   *
   */
  def -=(key: K, value: V): this.type = {
    index.get(key) match {
      case Some(constants) => constants.get(value) match {

        case Some(ids) if constants.size > 1 =>
          ids.foreach(id => tupleIDs -= id)
          index(key) = constants - value // delete this constant from index

        // if this is the last constant of the variable, then delete the variable
        case Some(ids) if constants.size == 1 => removeKey(key)

        case None => // there is nothing to delete, because this constant does not exist
      }

      case None => // there is nothing to delete, because this variable does not exist
    }
    this
  }

  /**
   * Performs delete operation but for a collection of constant literals.
   *
   * @param key: the variable to delete the specified instantiations
   * @param values: a collection of values to delete from the specified variable
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

      tuple.foreach { case (v, c) =>
        index.get(v) match {
          case Some(constants) => constants.get(c) match {
            case Some(constantIds) =>

              // if tuple ids set of a constant becomes empty, then delete the specific constant
              val result = constantIds - id
              if (result.nonEmpty) index(v) = constants + (c -> result)
              else index(v) = constants - c

            case None => // nothing to delete, because there is no such constant
          }
            if (size == 0) clear() // if there no tuples remaining, clear the database

          case None => // nothing to delete, because there is no such variable
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
          (variable, constants) <- index
          (constant, constantIDs) <- constants
          if constantIDs.contains(id)
        } {
          // if tuple ids set of a constant becomes empty, then delete the specific constant
          val result = constantIDs - id
          if (result.nonEmpty) index(variable) = constants + (constant -> result)
          else index(variable) = constants - constant
        }

        tupleIDs = tupleIDs intersect ids // keep only matched
      }
    // If not matched ids are empty, there is nothing to delete

    case None => clear() // all tuples does not match so delete them
  }

  /**
   * Unlocks all variables, in order to be able to insert any combination.
   */
  def allowEverything() = {
    lockedVariables = HashSet[K]()
  }

  /**
   * Allow only tuples in the database matching the one of the given
   * mappings. The usage of this method locks variables used in the mappings in order
   * to prevent the insertion of constants other than the ones specified here. In order
   * to unlock the variables use the allowEverything() method.
   *
   * @param tuples a set of tuples, therefore each sequence must have the same size
   */
  def allowOnly(tuples: Map[K, Seq[V]]) = {

    val sizes = tuples.values.map(seq => seq.length)
    require(sizes.forall(size => sizes.head == size), fatal("Uneven tuples!"))

    // Insert each constant of each variable in the database and lock the variable
    tuples.foreach { case (v, constants) =>
      this += (v, constants.toSet)
      lockedVariables += v
    }

    var matchedIDs = Set[Int]()
    for(i <- tuples.values.head.indices) {
      var tuple = Map[K, V]()
      tuples.keySet.foreach { v => tuple += v -> tuples(v)(i) }
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
        (variable, constants) <- index
        (constant, constantIDs) <- constants
        if constantIDs.contains(id)
      } {
        val result = constantIDs - id
        if (result.nonEmpty) index(variable) = constants + (constant -> result)
        else index(variable) = constants - constant
      }

      tupleIDs = tupleIDs intersect matchedIDs // keep only matched
    }
    else clear()
  }

  /**
   * @return true if the given constant exists in the domain of the
   *         specified variable, otherwise false
   */
  def contains(key: K, value: V): Boolean = index.get(key) match {
    case Some(constants) => constants.contains(value)
    case None => false
  }

  /**
   * @return true if the given variable exists, otherwise false
   */
  def contains(key: K): Boolean = index.contains(key)

  /**
   * Count tuples matching the specified mapping of variable constants.
   *
   * @param mapping the mapping of variables to constants
   * @return the number of tuples matching the mapping
   */
  def countTuples(mapping: Map[K, V]) = getMatchedTupleIDs(mapping) match {
    case Some(ids) => ids.size
    case None => 0
  }

  /**
   * @return all stored variables in the database
   */
  def variables: List[K] = keys

  /**
   * @return true if the specified variable is locked
   */
  def isLocked(v: K) = lockedVariables.contains(v)

  /**
   * @return the number of stored variables
   */
  def numberOfVariables = keys.length

  /**
   * @return the number of constants for a specified variable
   */
  def numberOfConstants(v: K) = index.getOrElse(v, fatal("Variable key not found!")).size

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
   * Find all tuple ids that match the given mapping pairs of variables and constants
   * if any exists.
   *
   * @param mapping the mapping of variables to constants
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
        val (v, c) = getNext
        index(v).get(c) match {
          case Some(idx) => search(idx.intersect(ids))
          case None => None
        }
      }
      else if (ids.nonEmpty) Some(ids)
      else None
    }

    val (v, c) = getNext
    index(v).get(c) match {
      case Some(ids) => search(ids)
      case None => None
    }
  }

  /**
   * @return all tuples in vectors of constants
   */
  def getTuples: Vector[Vector[V]] = {
    var tuples = Vector[Vector[V]]()
    val keys = variables.reverse

    tupleIDs.foreach { tid =>
      var tuple = Vector[V]()
      keys.foreach { v =>
        index(v).foreach { pair =>
          if(pair._2.contains(tid)) tuple :+= pair._1
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
    val variables = "tid : " + index.keySet.mkString(" | ")
    var tuples = Vector[Vector[V]]()
    val orderedIDs = tupleIDs.toVector.sorted

    val keys = index.keySet
    orderedIDs.foreach { tid =>
      var tuple = Vector[V]()
      keys.foreach { v =>
        index(v).foreach { pair =>
          if(pair._2.contains(tid)) tuple :+= pair._1
        }
      }
      tuples :+= tuple
    }

    "tuples : " + tuples.length + "\n" + variables + "\n" +
      (orderedIDs zip tuples.map(_.mkString("   ") + "\n")).map(pair => pair._1 + " : " + pair._2).mkString("")
  }

  /**
   * Remove the specified variable key from the index.
   */
  private def removeKey(key: K) = {
    index.remove(key)
    keys = keys.filterNot(k => k == key)
    lockedVariables = lockedVariables.filterNot(k => k == key)
  }

}
