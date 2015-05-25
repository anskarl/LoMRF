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

package lomrf.mln.model

import java.io.PrintStream

import auxlib.log.Logging
import gnu.trove.TCollections
import gnu.trove.map.TIntDoubleMap
import gnu.trove.map.hash.TIntDoubleHashMap
import gnu.trove.set.TIntSet
import gnu.trove.set.hash.TIntHashSet
import lomrf.logic._
import lomrf.mln.model
import lomrf.mln.model.AtomIdentityFunction


/**
 * Atoms evidence database is responsible for holding the state (True, False and Unknown)
 * of ground atoms that are given as evidence. Use [[model.AtomEvidenceDBBuilder]]
 * in order to instantiate the appropriate database implementation.
 *
 * @param identity the atom identity [[model.AtomIdentityFunction]]
 *
 *
 */
abstract class AtomEvidenceDB(val identity: AtomIdentityFunction) {

  // The atom's first id in the space of ground atom ids
  protected val bottomBound = identity.startID

  // The atom's last id in the space of ground atom ids
  protected val upperBound = identity.startID + identity.length

  // The atom's arity
  protected val arity = identity.signature.arity

  /**
   * Get the state (see [[lomrf.logic.TriState]]) for the given atom ID.
   *
   * @param id integer number indicating a specific grounding of the atom
   * @return the state of the given atom, i.e. TRUE, FALSE or UNKNOWN (due to open-world assumption, or the atom is probabilistic).
   */
  def apply(id: Int): TriState = get(id)

  /**
   * Get the state (see [[lomrf.logic.TriState]]) for the given ground atom's arguments.
   *
   * @param args the atom's constants
   * @return the state of the given atom, i.e. TRUE, FALSE or UNKNOWN (due to open-world assumption, or the atom is probabilistic).
   */
  def apply(args: Seq[String]): TriState = get(args)

  /**
   * The probability of the given atom being true
   *
   * @param id integer number indicating a specific grounding of the atom
   *
   * @return a double indicating the probability of this atom to be true
   */
  def probability(id: Int): Double = Double.NaN

  /**
   * The probability of the given atom being true
   *
   * @param args the atom's constants
   *
   * @return a double indicating the probability of this atom to be true
   */
  def probability(args: Seq[String]): Double = Double.NaN

  /**
   *
   * @param args the atom's arguments
   * @return true if the ground atom is contained in this databased
   */
  def contains(args: Seq[String]): Boolean = {
    checkLength(args)
    val id = identity.encode(args)
    isBetweenBounds(id)
  }

  /**
   * Get the state (see [[lomrf.logic.TriState]]) for the given ground atom's arguments.
   *
   * @param args the atom's constants
   * @return the state of the given atom, i.e. TRUE, FALSE or UNKNOWN (due to open-world assumption, or the atom is probabilistic).
   */
  def get(args: Seq[String]): TriState = {
    checkLength(args)
    val id = identity.encode(args)
    fetch(id)
  }

  /**
   * Get the state (see [[lomrf.logic.TriState]]) for the given atom ID.
   *
   * @param id integer number indicating a specific grounding of the atom
   *
   * @return the state of the given atom, i.e. TRUE, FALSE or UNKNOWN (due to open-world assumption, or the atom is probabilistic).
   */
  def get(id: Int): TriState = {
    checkBounds(id)
    fetch(id)
  }

  /**
   * This function gives an iterator (if any) that holds atoms with their state.
   * These atoms match with the specified constant value. For example, consider,
   * the following domain:
   * {{{
   * event = {Alpha,Beta,Gamma}
   * time = {1,..,10}
   * }}}
   * and the following predicate:
   * {{{Happens(event,time)}}}
   *
   * Let ''dbHappens'', be the instance of the database for the predicate ''Happens''.
   * {{{
   * val iterator = dbHappens.get("event","Alpha") match {
   *  case Some(x) => x
   *  case None => println("No groundings found matching the Happens(event,time) with event='Alpha'. ")
   * }
   * }}}
   * With the resulting iterator we can get the state of all groundings of ''Happens'', where the ''event''
   * argument is equal with the constant ''Alpha''.
   *
   * @param key the argument type
   * @param value the constant value for this argument type
   *
   * @return an iterator that gives the state for all ground atoms that match with the given query
   */
  def get(key: String, value: String): Option[Iterator[(Int, TriState)]] = get(Map(key -> value))

  /**
   * This function gives an iterator (if any) that holds atoms with their state.
   * These atoms match with the specified constant value. For example, consider,
   * the following domain:
   * {{{
   * event = {Alpha,Beta,Gamma}
   * fluent = {F1,F2,F3,F4}
   * time = {1,..,10}
   * }}}
   * and the following predicate:
   * {{{Initiates(event, fluent, time)}}}
   *
   * Let ''dbInitiates'', be the instance of the database for the predicate ''Initiates''.
   * {{{
   * val iterator = dbHappens.get(Map("event" -> "Alpha", "time" -> "3")) match {
   *  case Some(x) => x
   *  case None => println("No groundings found matching the Initiates(event, fluent, time) with event='Alpha' and time='3'.")
   * }
   * }}}
   * With the resulting iterator we can get the state of all groundings of ''Initiates'', where the ''event''
   * argument is equal with the constant ''Alpha'' and the ''time'' argument is equal with ''3''.
   *
   * @param query a map that associates argument types with constants
   *
   * @return an iterator that gives the state for all ground atoms that match with the given query
   */
  def get(query: Map[String, String]): Option[Iterator[(Int, TriState)]] = {
    val iter = identity.matchesIterator(query)
    if (iter.isEmpty) None
    else Some(iter.collect {
      case id: Int => (id, fetch(id))
    })
  }

  /**
   * Gives the number of groundings with True state
   */
  def numberOfTrue: Int

  /**
   * Gives the number of groundings with False state
   */
  def numberOfFalse: Int

  /**
   * Gives the number of groundings with unknown state
   */
  def numberOfUnknown: Int

  /**
   * Gives the number of probabilistic groundings
   */
  def numberOfProbabilistic: Int

  /**
   * Gives the number of groundings
   */
  def length = identity.length

  /**
   * Gives the number of groundings with known (True or False) state.
   */
  def numberOfKnown = identity.length - numberOfUnknown

  def isTriStateDB: Boolean = numberOfUnknown > 0

  def isProbabilistic: Boolean = numberOfProbabilistic > 0

  def getBuilder: AtomEvidenceDBBuilder

  protected def fetch(id: Int): TriState

  protected final def checkBounds(id: Int) {
    require(isBetweenBounds(id), s"The value of the specified id ($id) is out of bounds!")
  }

  protected final def checkLength(args: Seq[String]) {
    require(args.length == arity, s"The length of the arguments is not the same with the predicate arity of this database (${args.size} != $arity).")
  }

  private final def isBetweenBounds(id: Int): Boolean = id <= upperBound && id >= bottomBound

  override def toString = {
    "AtomEvidenceDB[ " + identity.signature + " ]{" +
      "\n\tnumberOfTrue = " + numberOfTrue +
      "\n\tnumberOfFalse = " + numberOfFalse +
      "\n\tnumberOfUnknown = " + numberOfUnknown +
      "\n\tnumberOfKnown = " + numberOfKnown +
      "\n\tlength = " + length +
      "\n\tStart ID: " + identity.startID +
      "\n\tEnd ID: " + identity.endID +
      "\n\t" + identity.constantsAndStep.map(_.toString()).toList +
      "\n}"

  }

  def dumpContents(implicit out: PrintStream = System.out)


}

object AtomEvidenceDB {

  object CWA {

    /**
     * Creates an AtomEvidenceDB with closed-world assumption, i.e. contains all ground evidence atoms
     * with True state value, while the state of the rest is assumed to be False.
     *
     * @param positives the collection of positive ground atom ids (i.e. their state is fixed to True).
     * @param identity the identity of the atom [[model.AtomIdentityFunction]]
     *
     * @return an AtomEvidenceDB instance
     */
    def apply(positives: TIntSet, identity: AtomIdentityFunction): AtomEvidenceDB =
      new DBCWA(positives, identity)

    /**
     * Creates an AtomEvidenceDB containing atoms where their state is either True or Unknown.
     * The state of the rest, unspecified, groundings is assumed as False (closed-world assumption).
     *
     * @param positives a collection of ground atom ids, where their state is explicitly defined as True
     * @param unknown a collection of ground atom ids, where their state is explicitly defined as Unknown
     * @param identity the identity of the atom [[model.AtomIdentityFunction]]
     *
     * @return an AtomEvidenceDB instance
     */
    def apply(positives: TIntSet, unknown: TIntSet, identity: AtomIdentityFunction): AtomEvidenceDB =
      new DBCWA_UNK(positives, unknown, identity)

    /**
     * Creates an AtomEvidenceDB with closed-world assumption and probabilistic evidence.
     *
     * @param positives the collection of positive ground atom ids (i.e. their state is fixed to True).
     * @param probabilistic the collection of probabilistic ground atom ids (i.e. their state is True with some probability).
     * @param identity the identity of the atom [[model.AtomIdentityFunction]]
     *
     * @return an AtomEvidenceDB instance
     */
    def apply(positives: TIntSet, probabilistic: TIntDoubleMap, identity: AtomIdentityFunction): AtomEvidenceDB =
      new DBProbCWA(positives, probabilistic, identity)

  }

  object OWA {

    /**
     * Creates an AtomEvidenceDB with open-world assumption.
     *
     * @param identity the identity of the atom [[model.AtomIdentityFunction]]
     *
     * @return an AtomEvidenceDB instance
     */
    def apply(identity: AtomIdentityFunction): AtomEvidenceDB = new DBOWA(new TIntHashSet(), new TIntHashSet(), identity)


    /**
     * Creates an AtomEvidenceDB with open-world assumption.
     *
     * @param positives the collection of positive ground atom ids (i.e. their state is fixed to True).
     * @param negatives the collection of negative ground atom ids (i.e. their state is fixed to False).
     * @param identity the identity of the atom [[model.AtomIdentityFunction]]
     *
     * @return an AtomEvidenceDB instance
     */
    def apply(positives: TIntSet, negatives: TIntSet, identity: AtomIdentityFunction): AtomEvidenceDB = new DBOWA(positives, negatives, identity)

    /**
     * Creates an AtomEvidenceDB with open-world assumption and probabilistic evidence
     *
     * @param positives the collection of positive ground atom ids (i.e. their state is fixed to True).
     * @param negatives the collection of negative ground atom ids (i.e. their state is fixed to False).
     * @param probabilistic the collection of probabilistic ground atom ids (i.e. their state is True with some probability).
     * @param identity the identity of the atom [[model.AtomIdentityFunction]]
     *
     * @return an AtomEvidenceDB instance
     */
    def apply(positives: TIntSet, negatives: TIntSet, probabilistic: TIntDoubleMap, identity: AtomIdentityFunction): AtomEvidenceDB = {
      new DBProbOWA(positives, negatives, probabilistic, identity)
    }

  }


  /**
   * Creates an AtomEvidenceDB, where all its atoms have the specified state.
   */
  def apply(identity: AtomIdentityFunction, state: TriState): AtomEvidenceDB = {
    state match {
      case TRUE => new DummyDBTrue(identity)
      case FALSE => new DummyDBFalse(identity)
      case UNKNOWN => new DummyDBUnknown(identity)
    }
  }

  def allFalse(identity: AtomIdentityFunction): AtomEvidenceDB = new DummyDBFalse(identity)

  def allTrue(identity: AtomIdentityFunction): AtomEvidenceDB = new DummyDBTrue(identity)

  def allUnknown(identity: AtomIdentityFunction): AtomEvidenceDB = new DummyDBUnknown(identity)

}

private class DBCWA(positives: TIntSet, override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int): TriState = if (positives.contains(id)) TRUE else FALSE

  def numberOfUnknown = 0

  def numberOfFalse = identity.length - numberOfTrue

  def numberOfTrue = positives.size()

  def getBuilder = AtomEvidenceDBBuilder(identity, isCWA = true, positivesOpt = Some(positives))

  def numberOfProbabilistic = 0

  def dumpContents(implicit out: PrintStream = System.out) {
    out.println("Positives:")
    val iterator = positives.iterator()
    while (iterator.hasNext) {
      val atomID = iterator.next()
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.mkString(",") + ")")
    }
  }
}

private class DBCWA_UNK(positives: TIntSet, unknown: TIntSet,
                        override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int): TriState = {
    if (positives.contains(id)) TRUE
    else if (unknown.contains(id)) UNKNOWN
    else FALSE
  }

  def numberOfUnknown = unknown.size()

  def numberOfFalse = identity.length - (numberOfTrue + numberOfUnknown)

  def numberOfTrue = positives.size() - numberOfUnknown

  def numberOfProbabilistic = 0

  def getBuilder = AtomEvidenceDBBuilder(identity, isCWA = true, positivesOpt = Some(positives), unknownOpt = Some(unknown))

  def dumpContents(implicit out: PrintStream = System.out) {
    out.println("Positives:")
    val iterator = positives.iterator()
    while (iterator.hasNext) {
      val atomID = iterator.next()
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.mkString(",") + ")")
    }

    out.println("Unknown:")
    val iteratorUnk = positives.iterator()
    while (iteratorUnk.hasNext) {
      val atomID = iteratorUnk.next()
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.mkString(",") + ")")
    }
  }
}

private class DBOWA(positives: TIntSet, negatives: TIntSet, override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  private val allUnknown = positives.isEmpty && negatives.isEmpty

  protected def fetch(id: Int): TriState = {
    if (allUnknown) UNKNOWN
    else if (positives.contains(id)) TRUE
    else if (negatives.contains(id)) FALSE
    else UNKNOWN
  }

  def numberOfUnknown = identity.length - (positives.size() + negatives.size())

  def numberOfFalse = negatives.size()

  def numberOfTrue = positives.size()

  def numberOfProbabilistic = 0

  def getBuilder = AtomEvidenceDBBuilder(identity, isCWA = false, positivesOpt = Some(positives), negativesOpt = Some(negatives))

  def dumpContents(implicit out: PrintStream = System.out) {
    out.println("Positives:")
    val iteratorPos = positives.iterator()
    while (iteratorPos.hasNext) {
      val atomID = iteratorPos.next()
      val constants = identity.decode(atomID).get
      println(identity.signature.symbol + "(" + constants.mkString(",") + ")")
    }

    out.println("\nNegatives:")
    val iteratorNeg = positives.iterator()
    while (iteratorNeg.hasNext) {
      val atomID = iteratorNeg.next()
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.map(_.toString).reduceLeft(_ + "," + _) + ")")
    }

  }

}

private class DummyDBUnknown(override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int) = UNKNOWN

  def numberOfUnknown = identity.length

  def numberOfFalse = 0

  def numberOfTrue = 0

  def numberOfProbabilistic = 0

  def getBuilder = AtomEvidenceDBBuilder(identity, isCWA = false)

  def dumpContents(implicit out: PrintStream = System.out) {
    out.println("Everything is unknown.")
  }
}

private class DummyDBFalse(override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int) = FALSE

  def numberOfUnknown = 0

  def numberOfFalse = identity.length

  def numberOfTrue = 0

  def numberOfProbabilistic = 0

  def getBuilder = AtomEvidenceDBBuilder(identity, isCWA = true)

  def dumpContents(implicit out: PrintStream = System.out) {
    out.println("Everything is false.")
  }


}

private class DummyDBTrue(override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int) = TRUE

  def numberOfUnknown = 0

  def numberOfFalse = 0

  def numberOfTrue = identity.length

  def numberOfProbabilistic = 0

  def getBuilder = AtomEvidenceDBBuilder(identity, isCWA = true)


  def dumpContents(implicit out: PrintStream = System.out) {
    out.println("Everything is true.")
  }
}


private class DBProbOWA(positives: TIntSet, negatives: TIntSet, val probabilistic: TIntDoubleMap, override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {
  private val allUnknown = positives.isEmpty && negatives.isEmpty

  protected def fetch(id: Int): TriState = {
    if (allUnknown) UNKNOWN
    else if (positives.contains(id)) TRUE
    else if (negatives.contains(id)) FALSE
    else UNKNOWN
  }

  def numberOfUnknown = identity.length - (numberOfTrue + numberOfFalse)

  def numberOfFalse = negatives.size()

  def numberOfTrue = positives.size()

  def numberOfProbabilistic = probabilistic.size()

  override def probability(id: Int) = probabilistic.get(id)

  override def probability(args: Seq[String]) = {
    checkLength(args)
    val id = identity.encode(args)
    probabilistic.get(id)
  }

  def getBuilder = AtomEvidenceDBBuilder(identity, isCWA = false, positivesOpt = Some(positives), negativesOpt = Some(negatives), probabilisticOpt = Some(probabilistic))

  def dumpContents(implicit out: PrintStream = System.out) {
    out.println("Positives:")
    val iteratorPos = positives.iterator()
    while (iteratorPos.hasNext) {
      val atomID = iteratorPos.next()
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.map(_.toString).reduceLeft(_ + "," + _) + ")")
    }

    out.println("\nNegatives:")
    val iteratorNeg = positives.iterator()
    while (iteratorNeg.hasNext) {
      val atomID = iteratorNeg.next()
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.map(_.toString).reduceLeft(_ + "," + _) + ")")
    }

    out.println("\nProbabilistic:")
    val iteratorProb = probabilistic.iterator()
    while (iteratorProb.hasNext) {
      iteratorProb.advance()
      val atomID = iteratorProb.key()
      val probability = iteratorProb.value()
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.map(_.toString).reduceLeft(_ + "," + _) + ")\t" + probability)
    }
  }

}


private class DBProbCWA(positives: TIntSet, val probabilistic: TIntDoubleMap, override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int): TriState = {
    if (positives.contains(id)) TRUE
    else if (probabilistic.containsKey(id)) UNKNOWN
    else FALSE
  }

  def numberOfUnknown = probabilistic.size()

  def numberOfFalse = identity.length - (numberOfUnknown + numberOfTrue)

  def numberOfTrue = positives.size()

  def numberOfProbabilistic = probabilistic.size()

  override def probability(id: Int) = probabilistic.get(id)

  override def probability(args: Seq[String]) = {
    checkLength(args)
    val id = identity.encode(args)
    probabilistic.get(id)
  }

  def getBuilder = AtomEvidenceDBBuilder(identity, isCWA = true, positivesOpt = Some(positives), probabilisticOpt = Some(probabilistic))

  def dumpContents(implicit out: PrintStream = System.out) {
    out.println("Positives:")
    val iteratorPos = positives.iterator()
    while (iteratorPos.hasNext) {
      val atomID = iteratorPos.next()
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.map(_.toString).reduceLeft(_ + "," + _) + ")")
    }

    out.println("\nProbabilistic:")
    val iteratorProb = probabilistic.iterator()
    while (iteratorProb.hasNext) {
      iteratorProb.advance()
      val atomID = iteratorProb.key()
      val probability = iteratorProb.value()
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.map(_.toString).reduceLeft(_ + "," + _) + ")\t" + probability)
    }
  }
}

final class AtomEvidenceDBBuilder private(val signature: AtomSignature,
                                          schema: Seq[String],
                                          identity: AtomIdentityFunction,
                                          isCWA: Boolean,
                                          private var positives: TIntHashSet,
                                          private var negatives: TIntHashSet,
                                          private var unknown: TIntHashSet,
                                          private var probabilistic: TIntDoubleHashMap) extends Logging {

  def this(signature: AtomSignature, schema: Seq[String], identity: AtomIdentityFunction, isCWA: Boolean) =
    this(signature, schema, identity, isCWA, new TIntHashSet(), new TIntHashSet(), new TIntHashSet(), new TIntDoubleHashMap())


  private var dirty = false

  def +=(atom: EvidenceAtom) {
    require(atom.signature == signature, "You are trying to store atom: " + atom + " in a database for " + signature + " atoms.")
    copyIfDirty()

    val id = identity.encode(atom)

    if(id == AtomIdentityFunction.IDENTITY_NOT_EXIST)
      throw new NoSuchElementException(s"Failed to compute the unit identification number for atom: '${atom.toText}'")

    atom.state match {
      case TRUE =>
        if (negatives.contains(id)) error(s"Atom '${atom.toText}' is defined both as positive and negative in the given evidence db. Will keep only the first definition, which is negative.")
        else if (probabilistic.contains(id)) error(s"Atom '${atom.toText}' is defined both as positive and probabilistic/unknown in the given evidence db. Will keep only the first definition, which is probabilistic/unknown.")
        else positives.add(id)

      case FALSE =>
        if (positives.contains(id)) error(s"Atom '${atom.toText}' is defined both as positive and negative in the given evidence db. Will keep only the first definition, which is positive.")
        else if (probabilistic.contains(id)) error(s"Atom '${atom.toText}' is defined both as negative and probabilistic/unknown in the given evidence db. Will keep only the first definition, which is probabilistic/unknown.")
        else negatives.add(id)

      case UNKNOWN =>
        if (positives.contains(id)) error(s"Atom '${atom.toText}' is defined both as positive and probabilistic/unknown in the given evidence db. Will keep only the first definition, which is positive.")
        else if (negatives.contains(id)) error(s"Atom '${atom.toText}' is defined both as negative and probabilistic/unknown in the given evidence db. Will keep only the first definition, which is negative.")
        else {
          atom.probability match {
            case 1.0 => putLiteral(id) // interpret as non-probabilistic evidence (state = TRUE)
            case 0.0 => putLiteral(-id) // interpret as non-probabilistic evidence (state = FALSE)
            case _ =>
              if (atom.probability == 0.5 || atom.probability.isNaN) {

                // A. Interpret it as evidence atom with unknown values:
                if(probabilistic.containsKey(id))
                  error(s"Cannot reassign the probabilistic atom '${atom.toText}' as non-probabilistic with unknown state.")
                else unknown.add(id)
              }
              else {

                // B. Interpret it as probabilistic evidence atom:
                require(atom.probability <= 1.0 && atom.probability >= 0.0,
                  "The specified probability value (" + atom.probability + ") of atom " + atom + " is outside the range [0,1].")

                val storedProb = probabilistic.putIfAbsent(id, atom.probability)

                if(storedProb != probabilistic.getNoEntryValue)
                  error(s"Cannot reassign a different probability (${atom.probability}) for atom '${atom.toText}' (stored probability is $storedProb).")

                /*assert((storedProb == probabilistic.getNoEntryValue) || (storedProb == atom.probability),
                  "Cannot reassign a different probability (" + atom.probability + ") for atom " + atom + " (stored probability is " + storedProb + ").")*/
              } // end case _
          } // end match atom.probability
        } // end else // end case UNKNOWN
    } //end match atom.state
  }

  def += (literal: Int) {
    require(identity.idsRange.contains(math.abs(literal)), "The given literal has incompatible id.")
    copyIfDirty()
    putLiteral(literal)
  }

  private def putLiteral(literal: Int) {
    if (literal > 0) {
      if (negatives.contains(literal)) error("Atom " + literal + " is defined both as positive and negative.")
      else if (probabilistic.contains(literal)) error("Atom " + literal + " is defined both as positive and probabilistic/unknown.")
      else positives.add(literal)
    }
    else {
      val id = -literal
      if (positives.contains(id)) error("Atom " + id + " is defined both as positive and negative.")
      else if (probabilistic.contains(id)) error("Atom " + id + " is defined both as negative and probabilistic/unknown.")
      else negatives.add(id)
    }
  }


  def result(): AtomEvidenceDB = {
    dirty = true

    if (isCWA) {
      (probabilistic.isEmpty, unknown.isEmpty) match {
        // Evidence atom with standard closed-world assumption, that is all unspecified atoms have False state.
        case (true, true) => AtomEvidenceDB.CWA(TCollections.unmodifiableSet(positives), identity)
        // Evidence atoms where their state is either True or Unknown. For the rest (unspecified states) we use closed-world assumption.
        case (true, false) => AtomEvidenceDB.CWA(TCollections.unmodifiableSet(positives), TCollections.unmodifiableSet(unknown), identity)
        // Probabilistic evidence atoms, where their state is either True or Probabilistic. For the rest (unspecified states) we use closed-world assumption.
        case (false, true) => AtomEvidenceDB.CWA(TCollections.unmodifiableSet(positives), TCollections.unmodifiableMap(probabilistic), identity)
        // The same with the previous, but all collected atoms with unknown state are translated as probabilistic with probability equal to 0.5
        case (false, false) =>
          val iterator = unknown.iterator()
          while (iterator.hasNext) probabilistic.put(iterator.next(), 0.5)

          AtomEvidenceDB.CWA(TCollections.unmodifiableSet(positives), TCollections.unmodifiableMap(probabilistic), identity)
      }
    }
    else {
      if (probabilistic.isEmpty)
        AtomEvidenceDB.OWA(TCollections.unmodifiableSet(positives), TCollections.unmodifiableSet(negatives), identity)
      else
        AtomEvidenceDB.OWA(TCollections.unmodifiableSet(positives), TCollections.unmodifiableSet(negatives), TCollections.unmodifiableMap(probabilistic), identity)
    }
  }

  private def copyIfDirty(): Unit ={
    if (dirty) {
      val cp_positives = new TIntHashSet(positives)
      val cp_negatives = new TIntHashSet(negatives)
      val cp_unknown = new TIntHashSet(unknown)
      val cp_probabilistic = new TIntDoubleHashMap(probabilistic)
      positives = cp_positives
      negatives = cp_negatives
      unknown = cp_unknown
      probabilistic = cp_probabilistic
      dirty = false
    }
  }
}

object AtomEvidenceDBBuilder {

  def apply(identity: AtomIdentityFunction, isCWA: Boolean,
            positivesOpt: Option[TIntSet] = None, negativesOpt: Option[TIntSet] = None,
            unknownOpt: Option[TIntSet] = None, probabilisticOpt: Option[TIntDoubleMap] = None): AtomEvidenceDBBuilder = {

    val positives = positivesOpt match {
      case Some(set) => new TIntHashSet(set)
      case None => new TIntHashSet()
    }

    val negatives = negativesOpt match {
      case Some(set) => new TIntHashSet(set)
      case None => new TIntHashSet()
    }

    val probabilistic = probabilisticOpt match {
      case Some(x) => new TIntDoubleHashMap(x)
      case None => new TIntDoubleHashMap()
    }

    val unknown = unknownOpt match {
      case Some(x) => new TIntHashSet(x)
      case None => new TIntHashSet()
    }

    new AtomEvidenceDBBuilder(identity.signature, identity.schema, identity, isCWA, positives, negatives, unknown, probabilistic)
  }

  def apply(identity: AtomIdentityFunction,
            isCWA: Boolean): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(identity.signature, identity.schema, identity, isCWA)

  def apply(schema: Seq[String],
            identity: AtomIdentityFunction,
            isCWA: Boolean): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(identity.signature, schema, identity, isCWA)

  def apply(signature: AtomSignature,
            schema: Seq[String],
            identity: AtomIdentityFunction,
            isCWA: Boolean): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(signature, schema, identity, isCWA)


  def apply(signature: AtomSignature,
            schema: Seq[String],
            constants: Map[String, ConstantsSet],
            startID: Int,
            isCWA: Boolean): AtomEvidenceDBBuilder = {

    require(signature.arity == schema.size, "The arity of the specified signature must be equal to the number of schema elements.")

    new AtomEvidenceDBBuilder(signature: AtomSignature, schema, AtomIdentityFunction(signature, schema, constants, startID), isCWA)
  }

  object CWA {

    def apply(identity: AtomIdentityFunction): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(identity.signature, identity.schema, identity, true)

    def apply(schema: Seq[String],
              identity: AtomIdentityFunction): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(identity.signature, schema, identity, true)

    def apply(signature: AtomSignature,
              schema: Seq[String],
              identity: AtomIdentityFunction): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(signature, schema, identity, true)


    def apply(signature: AtomSignature,
              schema: Seq[String],
              constants: Map[String, ConstantsSet],
              startID: Int): AtomEvidenceDBBuilder = {

      require(signature.arity == schema.size, "The arity of the specified signature must be equal to the number of schema elements.")

      new AtomEvidenceDBBuilder(signature: AtomSignature, schema, AtomIdentityFunction(signature, schema, constants, startID), true)
    }
  }

  object OWA {

    def apply(identity: AtomIdentityFunction): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(identity.signature, identity.schema, identity, false)

    def apply(schema: Seq[String],
              identity: AtomIdentityFunction): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(identity.signature, schema, identity, false)

    def apply(signature: AtomSignature,
              schema: Seq[String],
              identity: AtomIdentityFunction): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(signature, schema, identity, false)


    def apply(signature: AtomSignature,
              schema: Seq[String],
              constants: Map[String, ConstantsSet],
              startID: Int): AtomEvidenceDBBuilder = {

      require(signature.arity == schema.size, "The arity of the specified signature must be equal to the number of schema elements.")

      new AtomEvidenceDBBuilder(signature: AtomSignature, schema, AtomIdentityFunction(signature, schema, constants, startID), false)
    }
  }
}

