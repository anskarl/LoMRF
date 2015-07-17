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
 * Map of V (key) to a Map of (C (Constant) -> ID (Integer)). The index, provides a structure that contains a
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
 * @tparam V is the key type, used in order to identify the column (represents a variable)
 * @tparam C is the constant type, used for storing or generating the products of literals
 *
 */
final class TuplesDB[@specialized V, @specialized C] extends Logging {

  type IndexType = collection.mutable.HashMap[V, HashMap[C, HashSet[Int]]]

  private var tID = Int.MinValue
  private var tupleIDs = HashSet[Int]()
  private var lockedVariables = HashSet[V]()
  private val index = new IndexType
  private var keys = List[V]()

  /**
   * Remove the specified variable key from the index.
   */
  private def removeKey(v: V) = {
    index.remove(v)
    keys = keys.filterNot(k => k == v)
    lockedVariables = lockedVariables.filterNot(k => k == v)
  }

  /**
   * <p>Insert the specified Variable (V), that takes one Constant value (C), and perform
   * a Cartesian product between V and the stored variables (if any). The insertion would be
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
   * @param v: variable entity
   * @param c: constant entity
   */
  def +(v: V, c: C) = if (!lockedVariables.contains(v)) index.get(v) match {

    /*
     * If variable does not exist and there are no tuples (first entry in the database)
     * or there are tuples from other variables.
     */
    case None =>
      if(tupleIDs.isEmpty) {
        tupleIDs += tID
        index(v) = HashMap(c -> HashSet(tID))
        tID += 1
      }
      else index(v) = HashMap(c -> tupleIDs)
      keys = v :: keys

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
      if(!constants.contains(c) && keys.length > 1) {
        var newTupleIds = HashSet[Int]()

        for (i <- 0 until distinctTupleIds.size) {
          tupleIDs += tID
          newTupleIds += tID
          tID += 1
        }
        index(v) = constants + (c -> newTupleIds)

        assert(distinctTupleIds.size == newTupleIds.size)

        /*
         * Find for all other variables the constants that appear in the same distinct tuples
         * and for these constants append the same new tuple id in their tuple ids set.
         */
        distinctTupleIds zip newTupleIds foreach { case (d_tid , n_tid) =>
          for {variable <- variables.filter(_ != v)
               (constant, constantIDs) <- index(variable)
               if constantIDs.contains(d_tid)} {
            index(variable) = index(variable) + (constant -> (constantIDs + n_tid))
          }
        }

      }
      else if(!constants.contains(c)) {
        tupleIDs += tID
        index(v) = constants + (c -> HashSet(tID))
        tID += 1
      }
  }

  /**
   * Performs insert operation but for a collection of constant literals.
   *
   * @param v: the variable to insert the specified instantiations
   * @param constants: a collection of constant literals to insert from the specified variable
   */
  def +(v: V, constants: Iterable[C]): Unit = if (!lockedVariables.contains(v)) constants.foreach( c => this + (v, c) )

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
  def -(v: V, c: C) = index.get(v) match {
    case Some(constants) => constants.get(c) match {

      case Some(ids) if constants.size > 1 =>
        ids.foreach(id => tupleIDs -= id)
        index(v) = constants - c // delete this constant from index

      // if this is the last constant of the variable, then delete the variable
      case Some(ids) if constants.size == 1 => removeKey(v)

      case None => // there is nothing to delete, because this constant does not exist
    }

    case None => // there is nothing to delete, because this variable does not exist
  }

  /**
   * Performs delete operation but for a collection of constant literals.
   *
   * @param v: the variable to delete the specified instantiations
   * @param constants: a collection of constant literals to delete from the specified variable
   */
  def -(v: V, constants: Iterable[C]): Unit = constants.foreach(c => this - (v, c))

  /**
   * Delete all matched tuples to the given tuple or subset of a tuple.
   *
   * @param tuple a tuple or subset of a tuple
   */
  def deleteMatchedTuples(tuple: Map[V, C]) = getMatchedTupleIDs(tuple) match {
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
  def deleteNotMatchedTuples(tuple: Map[V, C]) = getMatchedTupleIDs(tuple) match {
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
    lockedVariables = HashSet[V]()
  }

  /**
   * Allow only tuples in the database matching the one of the given
   * mappings. The usage of this method locks variables used in the mappings in order
   * to prevent the insertion of constants other than the ones specified here. In order
   * to unlock the variables use the allowEverything() method.
   *
   * @param tuples a set of tuples, therefore each sequence must have the same size
   */
  def allowOnly(tuples: Map[V, Seq[C]]) = {

    val sizes = tuples.values.map(seq => seq.length)
    require(sizes.forall(size => sizes.head == size), fatal("Uneven tuples!"))

    // Insert each constant of each variable in the database and lock the variable
    tuples.foreach { case (v, constants) =>
      this + (v, constants.toSet)
      lockedVariables += v
    }

    var matchedIDs = Set[Int]()
    for(i <- tuples.values.head.indices) {
      var tuple = Map[V, C]()
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
   * Future work. This method is an expansion of allowOnly method, in order
   * to allow other mapping to expand the constrainted space.
   */
  private def allowMore(tuples: Map[V, Seq[C]]) = ???

  /**
   * @return true if the given constant exists in the domain of the
   *         specified variable, otherwise false
   */
  def contains(v: V, c: C): Boolean = index.get(v) match {
    case Some(constants) => constants.contains(c)
    case None => false
  }

  /**
   * @return true if the given variable exists, otherwise false
   */
  def contains(v: V): Boolean = index.contains(v)

  /**
   * Count tuples matching the specified mapping of variable constants.
   *
   * @param mapping the mapping of variables to constants
   * @return the number of tuples matching the mapping
   */
  def countTuples(mapping: Map[V, C]) = getMatchedTupleIDs(mapping) match {
    case Some(ids) => ids.size
    case None => 0
  }

  /**
   * @return all stored variables in the database
   */
  def variables: List[V] = keys

  /**
   * @return true if the specified variable is locked
   */
  def isLocked(v: V) = lockedVariables.contains(v)

  /**
   * @return the number of stored variables
   */
  def numberOfVariables = keys.length

  /**
   * @return the number of constants for a specified variable
   */
  def numberOfConstants(v: V) = index.getOrElse(v, fatal("Variable key not found!")).size

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
    keys = List[V]()
    lockedVariables = HashSet[V]()
  }

  /**
   * Find all tuple ids that match the given mapping pairs of variables and constants
   * if any exists.
   *
   * @param mapping the mapping of variables to constants
   * @return all tuple ids matching the given mapping
   */
  private def getMatchedTupleIDs(mapping: Map[V, C]): Option[Set[Int]] = {

    // If no mappings are given there are no matched tuples
    if (mapping.isEmpty) return None

    val iterator =  index.keySet.intersect(mapping.keySet).iterator

    def hasNext = iterator.hasNext

    def getNext: (V, C) = {
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
  def getTuples: Vector[Vector[C]] = {
    var tuples = Vector[Vector[C]]()
    val keys = variables.reverse

    tupleIDs.foreach { tid =>
      var tuple = Vector[C]()
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
    var tuples = Vector[Vector[C]]()
    val orderedIDs = tupleIDs.toVector.sorted

    val keys = index.keySet
    orderedIDs.foreach { tid =>
      var tuple = Vector[C]()
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

}
