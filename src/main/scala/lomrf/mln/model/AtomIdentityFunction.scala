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

package lomrf.mln.model

import lomrf.logic._
import lomrf.mln.model.mrf.Constraint
import scala.collection.mutable
import scala.util.{ Failure, Success, Try }
import spire.syntax.cfor._

/**
  * AtomIdentityFunction represents a one-to-one mapping between the groundings of an atom
  * and integer numbers. It is extremely useful for encoding the entire ground Markov Network
  * into a set of integer numbers, where each number uniquely represents a single grounding of
  * the specified AtomSignature.
  *
  * @note This class provides fast and thread-safe functions for encoding ground atoms of the
  *       same atom signature into unique integer numbers, as well as the inverse, that is, decoding
  *       integers to ground atoms.
  *
  * @param signature an atom signature
  * @param startID the start id of the ground atoms
  * @param constantsAndStep an array holding the constants set for each argument domain along step information
  * @param length the number of groundings
  * @param schema the domains of the atom arguments
  */
final class AtomIdentityFunction private (
    val signature: AtomSignature,
    val startID: Int,
    val constantsAndStep: Array[(ConstantsSet, Int, Int, String, Int)],
    val length: Int,
    val schema: Seq[String]) extends Serializable {

  import AtomIdentityFunction.IDENTITY_NOT_EXIST

  /** the last id of the ground atoms */
  val endID: Int = startID + length

  /** the indices for all ground atoms */
  @transient lazy val indices: Range = startID until endID

  /**
    * Uniquely encodes a given evidence atom into an ID (positive integer).
    *
    * @param atom an evidence atom
    * @return the encoded integer (atom id) representing the given evidence atom
    */
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

  /**
    * Uniquely encodes a sequence of string arguments into an ID (positive integer).
    *
    * @param constants a sequence of string arguments
    * @return the encoded integer representing the given string arguments
    */
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

  /**
    * Uniquely encodes an indexed sequence of string arguments into an ID (positive integer).
    *
    * @param constants an indexed sequence of string arguments
    * @return the encoded integer representing the given string arguments
    */
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

  /**
    * Uniquely encodes an array of string arguments into an ID (positive integer).
    *
    * @param constants an array of string arguments
    * @return the encoded integer representing the given string arguments
    */
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

  /**
    * Uniquely encodes an array of argument IDs into an integer.
    *
    * @param constantIds an array of argument IDs
    * @return the encoded integer representing the given argument IDs
    */
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
    * Gives the unique ID (positive integer) of the corresponding grounding for the
    * specified atom. The grounding is computed using a given function that maps
    * terms to strings. If the grounding cannot be computed, then it returns IDENTITY_NOT_EXIST.
    *
    * @example {{{
    *           The special FOL function succ(int) returns the successive number
    *           of any given integer. Suppose we are given the predicate
    *           HoldsAt(fluent, succ(time)).
    *
    *           Let time={0,...,100} and fluent={F1,...Fn}.
    *
    *           The grounding of HoldsAt(F1, succ(99)) is HoldsAt(F1,100) which is inside the
    *           bounds of 'time' domain, thus the encode function should return a proper ID.
    *           However, the grounding of HoldsAt(F1, succ(100)) cannot be determined, since
    *           the resulting ground atom HoldsAt(F1, 101) is out of the 'time' domain bounds.
    *           Therefore, the encode function should return IDENTITY_NOT_EXIST.
    * }}}
    *
    * @param atom an atomic formula
    * @param f a function from term to string
    * @return the corresponding ID if a valid grounding exists, IDENTITY_NOT_EXIST otherwise.
    */
  def encode(atom: AtomicFormula, f: Term => String): Int = {
    var sum = startID
    var idx = 0
    for (term <- atom.terms) {
      val set: ConstantsSet = constantsAndStep(idx)._1
      val constantID = set(f(term))
      if (constantID == ConstantsSet.NO_ENTRY) return IDENTITY_NOT_EXIST
      val step = constantsAndStep(idx)._2
      val offset = constantID * step
      sum += (offset + constantID)
      idx += 1
    }
    sum
  }

  /**
    * Decodes a given atom ID (positive integer) into a sequence
    * of string arguments representing the ground atom.
    *
    * @param atomID an atom id
    * @return a sequence of string arguments
    */
  def decode(atomID: Int): Try[IndexedSeq[String]] = {
    // Check bounds
    if (atomID < startID || atomID >= endID)
      return Failure(new IndexOutOfBoundsException(
        s"The given id '$atomID' for predicate '$signature' is out of bounds [$startID, $endID]"))

    val baseID = atomID - startID

    // Find all id literals
    var currentID = baseID
    val result = new Array[String](constantsAndStep.length)
    var idx = result.length - 1

    while (idx > 0) {
      val sigma = constantsAndStep(idx)._3
      val constantsSet = constantsAndStep(idx)._1

      if (sigma <= currentID) {
        val tmpID = (currentID - (currentID % sigma)) / sigma
        result(idx) = constantsSet(tmpID)
        currentID -= (tmpID * sigma)
      } else result(idx) = constantsSet(0)

      idx -= 1
    }
    val constantsSet = constantsAndStep(idx)._1
    result(idx) = constantsSet(currentID)

    Success(result)
  }

  /**
    * Extracts a sequence of encoded arguments IDs from
    * a given atom ID (positive integer).
    *
    * @param atomID an atom id
    * @return a sequence of encoded argument ids
    */
  def extract(atomID: Int): Try[Array[Int]] = {
    // check bounds
    if (atomID < startID || atomID >= endID)
      return Failure(new IndexOutOfBoundsException(
        s"The given atom id '$atomID' is out of bounds, thus cannot be decoded."))

    val baseID = atomID - startID

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
      } else result(idx) = offset

      idx -= 1
    }

    val offset = constantsAndStep(idx)._5
    result(idx) = offset + currentID

    Success(result)
  }

  /**
    * Gives an iterator that matches only the atom IDs having the specified
    * constant value for a given domain argument.
    *
    * @param key a domain argument
    * @param value a constant value for the domain argument
    * @return an iterator over the reduced space of atom IDs
    */
  def matchesIterator(key: String, value: String): Iterator[Int] = matchesIterator(Map(key -> value))

  /**
    * Gives an iterator that matches only the atom IDs having the specified
    * constant values for the given domain arguments.
    *
    * @param query a map that associates domain arguments to constant values
    * @return an iterator over the reduced space of atom IDs
    */
  def matchesIterator(query: Map[String, String]): Iterator[Int] = {

    val length = constantsAndStep.length
    var rangesMap = Map[Int, Range]()
    var iteratorsMap = mutable.Map[Int, Iterator[Int]]()
    val values = new Array[Int](length)

    cfor(0)(_ < length, _ + 1) { idx: Int =>
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

  private class MatchingIDsIterator(
      rangesMap: Map[Int, Range],
      iteratorsMap: mutable.Map[Int, Iterator[Int]],
      values: Array[Int]) extends Iterator[Int] {

    private val _length = rangesMap.map(_._2.size).product
    private var counter = 0
    private var sum = -1

    override def length: Int = _length

    override def size: Int = length

    def hasNext: Boolean = counter < _length

    def next(): Int = {
      if (counter < _length) {
        sum = startID

        cfor(0) (_ < values.length, _ + 1) { idx: Int =>

          // 1. Encode
          val constantID = values(idx)
          val step = constantsAndStep(idx)._2
          val offset = constantID * step
          sum += (offset + constantID)

          // 2. Advance
          iteratorsMap.get(idx) match {
            case Some(currentIter) =>
              values(idx) =
                if (currentIter.hasNext) {
                  currentIter.next
                } else {
                  val newIter = rangesMap(idx).iterator
                  iteratorsMap(idx) = newIter
                  newIter.next
                }
            case _ => // do nothing
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

  /**
    * Creates an atom identity function.
    *
    * @param signature an atom signature
    * @param schema the domain arguments
    * @param constants a constants domain
    * @param startID the start id of the ground atoms
    * @return an AtomIdentityFunction instance
    */
  def apply(
      signature: AtomSignature,
      schema: Seq[String],
      constants: ConstantsDomain,
      startID: Int): AtomIdentityFunction = {

    assert(startID > 0, "Atom identity function requires startID to be greater than zero and you gave: " + startID)

    val descriptor: Array[String] = schema.toArray

    var n = 0
    val constantsAndStep = new Array[(ConstantsSet, Int, Int, String, Int)](descriptor.length)

    var constOffsetMap = Map[String, Int]()

    var currentOffset = 0
    for ((k, v) <- constants) {
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
      constantsAndStep(i) =
        if (i == 0) (currentDomain, 0, 1, symbol, constOffsetMap(symbol))
        else (currentDomain, n - 1, n, symbol, constOffsetMap(symbol))
      n = if (n == 0) currentDomain.size else n * currentDomain.size
      i += 1
    }

    new AtomIdentityFunction(signature, startID, constantsAndStep, length, schema)
  }

  /**
    * Decodes a given literal ID (integer) into a textual representation.
    *
    * @note If the ID is positive the literal is positive,
    *       otherwise the literal is negative.
    *
    * @param literal a literal ID
    * @param mln an MLN
    * @return the textual representation of the literal
    */
  def decodeLiteral(literal: Int)(implicit mln: MLN): Try[String] = {

    val atomID = math.abs(literal)
    val signature = mln.space.signatureOf(atomID)
    val idf = mln.space.identities(signature)

    val negation = if (literal < 0) "!" else ""

    idf.decode(atomID).map(x => s"$negation${signature.symbol}(${x.mkString(",")})")
  }

  /**
    * Decodes a given literal ID (integer) into an atomic textual representation.
    *
    * @param literal a literal ID
    * @param mln an MLN
    * @return the textual representation of the underlying atom
    */
  def decodeAtom(literal: Int)(implicit mln: MLN): Try[String] = {

    val atomID = math.abs(literal)
    val signature = mln.space.signatureOf(atomID)
    val idf = mln.space.identities(signature)

    idf.decode(atomID).map(x => s"${signature.symbol}(${x.mkString(",")})")
  }

  /**
    * Decodes a given constraint (ground clause) into a textual representation.
    *
    * @param feature a constraint
    * @param mln an MLN
    * @return the textual representation of the constraint
    */
  def decodeFeature(feature: Constraint, hardWeight: Double = 0)(implicit mln: MLN): Try[String] = {

    val buffer = new StringBuilder()

    val weight = feature.getWeight

    if (weight.isPosInfinity) {
      if (hardWeight != 0) buffer.append(hardWeight.toString)
      buffer.append(' ')
    } else if (!weight.isNaN) {
      buffer.append(feature.getWeight.toString)
      buffer.append(' ')
    }

    cfor(0) (_ < feature.literals.length, _ + 1) { i: Int =>
      val tryLiteral = decodeLiteral(feature.literals(i))

      tryLiteral match {
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
    /**
      * @param mln an MLN
      * @return the textual representation of the literal ID
      */
    def decodeLiteral(implicit mln: MLN): Try[String] = AtomIdentityFunction.decodeLiteral(literal)

    /**
      * @param mln an MLN
      * @return the textual representation of the underlying atom for the literal ID
      */
    def decodeAtom(implicit mln: MLN): Try[String] = AtomIdentityFunction.decodeAtom(literal)
  }

  implicit class WrappedConstraint(val feature: Constraint) extends AnyVal {
    /**
      * @param mln an MLN
      * @return the textual representation of the constraint
      */
    def decodeFeature(hardWeight: Double = 0)(implicit mln: MLN): Try[String] = {
      AtomIdentityFunction.decodeFeature(feature, hardWeight)
    }
  }
}
