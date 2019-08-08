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

import java.io.PrintStream
import gnu.trove.map.TIntDoubleMap
import gnu.trove.set.TIntSet
import gnu.trove.set.hash.TIntHashSet
import lomrf.logic._
import lomrf.mln.model.builder.AtomEvidenceDBBuilder

/**
  * Atoms evidence database holds the state (True, False or Unknown) of ground atoms
  * that are given as evidence.
  *
  * @see [[lomrf.mln.model.builder.AtomEvidenceDBBuilder]]
  *
  * @param identity an atom identity function
  */
abstract class AtomEvidenceDB(val identity: AtomIdentityFunction) {

  // The atom's first atomID in the space of ground atom ids
  protected val bottomBound: Int = identity.startID

  // The atom's last atomID in the space of ground atom ids
  protected val upperBound: Int = identity.startID + identity.length

  // The atom's arity
  protected val arity: Int = identity.signature.arity

  /**
    * Get the state (see [[lomrf.logic.TriState]]) of the given atom ID.
    *
    * @param atomID a positive integer indicating a specific grounding of the atom
    * @return the state of the given atom ID, i.e. TRUE, FALSE or UNKNOWN
    *         (due to open-world assumption, or because the atom is probabilistic).
    */
  def apply(atomID: Int): TriState = get(atomID)

  /**
    * Get the state (see [[lomrf.logic.TriState]]) of the given ground atom's arguments.
    *
    * @param args the ground atom's arguments (constants)
    * @return the state of the given atom, i.e. TRUE, FALSE or UNKNOWN
    *         (due to open-world assumption, or because the atom is probabilistic).
    */
  def apply(args: Seq[String]): TriState = get(args)

  /**
    * The probability of the given atom ID being true.
    *
    * @param atomID a positive integer indicating a specific grounding of the atom
    * @return the probability of the atom to be true
    */
  def probability(atomID: Int): Double = Double.NaN

  /**
    * The probability of the given ground atom's argument being true
    *
    * @param args the ground atom's arguments (constants)
    * @return the probability of the ground atom's arguments to be true
    */
  def probability(args: Seq[String]): Double = Double.NaN

  /**
    * Checks if the given atom ID exists in the database.
    *
    * @param atomID a positive integer indicating a specific grounding of the atom
    * @return true if the atom ID is contained in the database
    */
  def contains(atomID: Int): Boolean = isBetweenBounds(atomID)

  /**
    * Checks if the given ground atom's arguments exist in the database.
    *
    * @param args the ground atom's arguments (constants)
    * @return true if the ground atom's arguments are contained in the database
    */
  def contains(args: Seq[String]): Boolean = {
    checkLength(args)
    val id = identity.encode(args)
    isBetweenBounds(id)
  }

  /**
    * Get the state (see [[lomrf.logic.TriState]]) for the given atom ID.
    *
    * @param atomID a positive integer indicating a specific grounding of the atom
    * @return the state of the given atom, i.e. TRUE, FALSE or UNKNOWN
    *         (due to open-world assumption, or because the atom is probabilistic).
    */
  def get(atomID: Int): TriState = {
    checkBounds(atomID)
    fetch(atomID)
  }

  /**
    * Get the state (see [[lomrf.logic.TriState]]) for the given ground atom's arguments.
    *
    * @param args the ground atom's arguments (constants)
    * @return the state of the given atom, i.e. TRUE, FALSE or UNKNOWN
    *         (due to open-world assumption, or because the atom is probabilistic).
    */
  def get(args: Seq[String]): TriState = {
    checkLength(args)
    val id = identity.encode(args)
    fetch(id)
  }

  /**
    * Gives an iterator (if any) that matches only atom IDs, along their state,
    * having the specified constant for a given domain argument.
    *
    * @example {{{
    *         Consider the following domains:
    *
    *         event = {Alpha, Beta, Gamma} and time = {1,...,10}
    *
    *         and the following predicate:
    *
    *         Happens(event,time)
    *
    * }}}
    *
    * Let ''db'', be the instance of the database for the predicate ''Happens''.
    *
    * {{{
    *       val iterator = db.get("event", "Alpha") match {
    *         case Some(x) => x
    *         case None => println("No groundings found matching the Happens(event,time) with event='Alpha'.")
    *       }
    * }}}
    *
    * The resulting iterator gives us the state of all groundings of ''Happens'', where the ''event''
    * argument is equal to the constant value ''Alpha''.
    *
    * @param key a domain argument
    * @param value the constant value for the domain argument
    * @return an iterator over the reduced space of ground atoms, along their states, that match the given query
    */
  def get(key: String, value: String): Option[Iterator[(Int, TriState)]] = get(Map(key -> value))

  /**
    *
    * Gives an iterator (if any) that matches only atom IDs, along their state,
    * having the specified constant values for a given domain arguments.
    *
    * @example {{{
    *           Consider the following domains:
    *
    *           event = {Alpha, Beta, Gamma} fluent = {F1, F2, F3, F4} and time = {1,...,10}
    *
    *           and the following predicate:
    *
    *           Initiates(event, fluent, time)
    * }}}
    *
    * Let ''db'', be the instance of the database for the predicate ''Initiates''.
    *
    * {{{
    *           val iterator = db.get(Map("event" -> "Alpha", "time" -> "3")) match {
    *             case Some(x) => x
    *             case None => println("No groundings found matching the Initiates(event, fluent, time) with event='Alpha' and time='3'.")
    *           }
    * }}}
    *
    * The resulting iterator gives us the state of all groundings of ''Initiates'', where the ''event''
    * argument is equal to the constant value ''Alpha'' and the ''time'' argument is equal to ''3''.
    *
    * @param query a map that associates domain arguments to constant values
    * @return an iterator over the reduced space of ground atoms, along their states, that match the given query
    */
  def get(query: Map[String, String]): Option[Iterator[(Int, TriState)]] = {
    val iter = identity.matchesIterator(query)
    if (iter.isEmpty) None
    else Some(iter.collect {
      case id: Int => (id, fetch(id))
    })
  }

  /**
    * @return the number of groundings having true state
    */
  def numberOfTrue: Int

  /**
    * @return the number of groundings having false state
    */
  def numberOfFalse: Int

  /**
    * @return the number of groundings having unknown state
    */
  def numberOfUnknown: Int

  /**
    * @return the number of probabilistic groundings
    */
  def numberOfProbabilistic: Int

  /**
    * @return the number of groundings
    */
  def length: Int = identity.length

  /**
    * @return the number of groundings having a known (True or False) state
    */
  def numberOfKnown: Int = identity.length - numberOfUnknown

  /**
    * @return true if the database contains unknown atoms, false otherwise
    */
  def isTriStateDB: Boolean = numberOfUnknown > 0

  /**
    * @return true if the database contains probabilistic atoms, false otherwise
    */
  def isProbabilistic: Boolean = numberOfProbabilistic > 0

  /**
    * @return an atom evidence builder
    */
  def getBuilder: AtomEvidenceDBBuilder

  /**
    * Dump the database into an output stream.
    *
    * @param out a print stream (default is console)
    */
  def dumpContents(implicit out: PrintStream = System.out)

  /**
    * Fetch the state of the given atom ID.
    *
    * @param atomID an atom ID (positive integer) indicating a specific grounding of the atom
    * @return the state of the given atom, i.e. TRUE, FALSE or UNKNOWN
    *         (due to open-world assumption, or because the atom is probabilistic).
    */
  protected def fetch(atomID: Int): TriState

  /**
    * Checks if the given atom ID is between bounds.
    *
    * @param atomID an atom ID (positive integer) indicating a specific grounding of the atom
    */
  protected final def checkBounds(atomID: Int): Unit = {
    if (!isBetweenBounds(atomID)) {
      println(identity.signature)
      println(identity.decode(atomID))
    }
    require(isBetweenBounds(atomID), s"The value of the specified atomID ($atomID) is out of bounds!")
  }

  /**
    * Checks if the length of the given ground atom's arguments is
    * equal to the atom's arity as defined by the atom signature.
    *
    * @param args the ground atom's arguments (constants)
    */
  protected final def checkLength(args: Seq[String]): Unit = {
    require(args.length == arity,
      s"The length of the arguments is not the same with the predicate arity (${args.size} != $arity).")
  }

  /**
    * Check if the given atom ID is between bounds.
    *
    * @param atomID an atom ID (positive integer) indicating a specific grounding of the atom
    * @return true if the atom ID is between the start and last IDs
    */
  private final def isBetweenBounds(atomID: Int): Boolean = atomID >= bottomBound && atomID <= upperBound

  override def toString: String = {
    s"""
       |AtomEvidenceDB[ ${identity.signature}" ]{
       |\tnumberOfTrue    = $numberOfTrue
       |\tnumberOfFalse   = $numberOfFalse
       |\tnumberOfUnknown = $numberOfUnknown
       |\tnumberOfKnown   = $numberOfKnown
       |\tlength          = $length
       |\tStart ID        = ${identity.startID}
       |\tEnd ID          = ${identity.endID}
       |\t${identity.constantsAndStep.map(_.toString).toList}
       |}
    """.stripMargin
  }
}

object AtomEvidenceDB {

  object CWA {

    /**
      * Creates an AtomEvidenceDB having closed-world assumption, i.e. contains all ground evidence
      * atoms having True state values, while the state of the rest is assumed to be False.
      *
      * @param positives a set of positive ground atom ids (i.e. their state is True)
      * @param identity an atom identity function
      * @return an AtomEvidenceDB instance
      */
    def apply(positives: TIntSet, identity: AtomIdentityFunction): AtomEvidenceDB =
      new DB_CWA(positives, identity)

    /**
      * Creates an AtomEvidenceDB containing atoms having either True or Unknown state values.
      * The state of the rest, unspecified, groundings is assumed to be False (closed-world assumption).
      *
      * @param positives a set of positive ground atom ids (i.e. their state is True)
      * @param unknown a set of unknown ground atom ids (i.e. their state is Unknown)
      * @param identity an atom identity function
      * @return an AtomEvidenceDB instance
      */
    def apply(positives: TIntSet, unknown: TIntSet, identity: AtomIdentityFunction): AtomEvidenceDB =
      new DB_CWA_UNK(positives, unknown, identity)

    /**
      * Creates an AtomEvidenceDB having closed-world assumption and probabilistic evidence.
      *
      * @param positives a set of positive ground atom ids (i.e. their state is True)
      * @param probabilistic a set of probabilistic ground atom ids (i.e. their state is True with some probability)
      * @param identity an atom identity function
      * @return an AtomEvidenceDB instance
      */
    def apply(positives: TIntSet, probabilistic: TIntDoubleMap, identity: AtomIdentityFunction): AtomEvidenceDB =
      new DBProbCWA(positives, probabilistic, identity)
  }

  object OWA {

    /**
      * Creates an empty AtomEvidenceDB having open-world assumption.
      *
      * @param identity an atom identity function
      * @return an AtomEvidenceDB instance
      */
    def apply(identity: AtomIdentityFunction): AtomEvidenceDB =
      new DB_OWA(new TIntHashSet(), new TIntHashSet(), identity)

    /**
      * Creates an AtomEvidenceDB having open-world assumption.
      *
      * @param positives a set of positive ground atom ids (i.e. their state is True)
      * @param negatives a set of negative ground atom ids (i.e. their state is False)
      * @param identity an atom identity function
      * @return an AtomEvidenceDB instance
      */
    def apply(positives: TIntSet, negatives: TIntSet, identity: AtomIdentityFunction): AtomEvidenceDB =
      new DB_OWA(positives, negatives, identity)

    /**
      * Creates an AtomEvidenceDB having open-world assumption and probabilistic evidence
      *
      * @param positives a set of positive ground atom ids (i.e. their state is True)
      * @param negatives a set of negative ground atom ids (i.e. their state is False)
      * @param probabilistic a set of probabilistic ground atom ids (i.e. their state is True with some probability)
      * @param identity an atom identity function
      * @return an AtomEvidenceDB instance
      */
    def apply(
        positives: TIntSet,
        negatives: TIntSet,
        probabilistic: TIntDoubleMap,
        identity: AtomIdentityFunction): AtomEvidenceDB = {
      new DBProbOWA(positives, negatives, probabilistic, identity)
    }
  }

  /**
    * Creates an AtomEvidenceDB, where all atoms have the given state.
    *
    * @param identity an atom identity function
    * @param state the state of the atoms in the database
    * @return an AtomEvidenceDB instance
    */
  def apply(identity: AtomIdentityFunction, state: TriState): AtomEvidenceDB = state match {
    case TRUE    => new DummyDBTrue(identity)
    case FALSE   => new DummyDBFalse(identity)
    case UNKNOWN => new DummyDBUnknown(identity)
  }

  /**
    * Creates an AtomEvidenceDB, where all atoms are True.
    *
    * @param identity an atom identity function
    * @return an AtomEvidenceDB instance
    */
  def allTrue(identity: AtomIdentityFunction): AtomEvidenceDB = new DummyDBTrue(identity)

  /**
    * Creates an AtomEvidenceDB, where all atoms are False.
    *
    * @param identity an atom identity function
    * @return an AtomEvidenceDB instance
    */
  def allFalse(identity: AtomIdentityFunction): AtomEvidenceDB = new DummyDBFalse(identity)

  /**
    * Creates an AtomEvidenceDB, where all atoms are Unknown.
    *
    * @param identity an atom identity function
    * @return an AtomEvidenceDB instance
    */
  def allUnknown(identity: AtomIdentityFunction): AtomEvidenceDB = new DummyDBUnknown(identity)
}

private class DB_CWA(
    positives: TIntSet,
    override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int): TriState = if (positives.contains(id)) TRUE else FALSE

  def numberOfTrue: Int = positives.size

  def numberOfFalse: Int = identity.length - numberOfTrue

  def numberOfUnknown = 0

  def numberOfProbabilistic = 0

  def getBuilder = AtomEvidenceDBBuilder(identity, isCWA = true, positivesOpt = Some(positives))

  def dumpContents(implicit out: PrintStream = System.out): Unit = {
    out.println("Positives:")
    val iterator = positives.iterator
    while (iterator.hasNext) {
      val atomID = iterator.next
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.mkString(",") + ")")
    }
  }
}

private class DB_CWA_UNK(
    positives: TIntSet,
    unknown: TIntSet,
    override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int): TriState = {
    if (positives.contains(id)) TRUE
    else if (unknown.contains(id)) UNKNOWN
    else FALSE
  }

  def numberOfTrue: Int = positives.size - numberOfUnknown

  def numberOfFalse: Int = identity.length - (numberOfTrue + numberOfUnknown)

  def numberOfUnknown: Int = unknown.size

  def numberOfProbabilistic: Int = 0

  def getBuilder: AtomEvidenceDBBuilder =
    AtomEvidenceDBBuilder(identity, isCWA = true, positivesOpt = Some(positives), unknownOpt = Some(unknown))

  def dumpContents(implicit out: PrintStream = System.out) {
    out.println("Positives:")
    val iterator = positives.iterator
    while (iterator.hasNext) {
      val atomID = iterator.next
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.mkString(",") + ")")
    }

    out.println("Unknown:")
    val iteratorUnk = positives.iterator()
    while (iteratorUnk.hasNext) {
      val atomID = iteratorUnk.next
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.mkString(",") + ")")
    }
  }
}

private class DB_OWA(
    positives: TIntSet,
    negatives: TIntSet,
    override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  private val allUnknown = positives.isEmpty && negatives.isEmpty

  protected def fetch(id: Int): TriState = {
    if (allUnknown) UNKNOWN
    else if (positives.contains(id)) TRUE
    else if (negatives.contains(id)) FALSE
    else UNKNOWN
  }

  def numberOfTrue: Int = positives.size

  def numberOfFalse: Int = negatives.size

  def numberOfUnknown: Int = identity.length - (positives.size + negatives.size)

  def numberOfProbabilistic: Int = 0

  def getBuilder: AtomEvidenceDBBuilder =
    AtomEvidenceDBBuilder(identity, isCWA = false, positivesOpt = Some(positives), negativesOpt = Some(negatives))

  def dumpContents(implicit out: PrintStream = System.out): Unit = {
    out.println("Positives:")
    val iteratorPos = positives.iterator
    while (iteratorPos.hasNext) {
      val atomID = iteratorPos.next
      val constants = identity.decode(atomID).get
      println(identity.signature.symbol + "(" + constants.mkString(",") + ")")
    }

    out.println("\nNegatives:")
    val iteratorNeg = positives.iterator
    while (iteratorNeg.hasNext) {
      val atomID = iteratorNeg.next
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.map(_.toString).reduceLeft(_ + "," + _) + ")")
    }
  }
}

private class DummyDBUnknown(override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int): TriState = UNKNOWN

  def numberOfTrue: Int = 0

  def numberOfFalse: Int = 0

  def numberOfUnknown: Int = identity.length

  def numberOfProbabilistic: Int = 0

  def getBuilder: AtomEvidenceDBBuilder = AtomEvidenceDBBuilder(identity, isCWA = false)

  def dumpContents(implicit out: PrintStream = System.out) {
    out.println("Everything is unknown.")
  }
}

private class DummyDBFalse(override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int): TriState = FALSE

  def numberOfTrue: Int = 0

  def numberOfFalse: Int = identity.length

  def numberOfUnknown: Int = 0

  def numberOfProbabilistic: Int = 0

  def getBuilder: AtomEvidenceDBBuilder = AtomEvidenceDBBuilder(identity, isCWA = true)

  def dumpContents(implicit out: PrintStream = System.out): Unit = out.println("Everything is false.")
}

private class DummyDBTrue(override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int): TRUE.type = TRUE

  def numberOfTrue: Int = identity.length

  def numberOfFalse: Int = 0

  def numberOfUnknown: Int = 0

  def numberOfProbabilistic: Int = 0

  def getBuilder: AtomEvidenceDBBuilder = AtomEvidenceDBBuilder(identity, isCWA = true)

  def dumpContents(implicit out: PrintStream = System.out): Unit = out.println("Everything is true.")
}

private class DBProbOWA(
    positives: TIntSet,
    negatives: TIntSet,
    val probabilistic: TIntDoubleMap,
    override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  private val allUnknown = positives.isEmpty && negatives.isEmpty

  protected def fetch(id: Int): TriState = {
    if (allUnknown) UNKNOWN
    else if (positives.contains(id)) TRUE
    else if (negatives.contains(id)) FALSE
    else UNKNOWN
  }

  def numberOfTrue: Int = positives.size

  def numberOfFalse: Int = negatives.size

  def numberOfUnknown: Int = identity.length - (numberOfTrue + numberOfFalse)

  def numberOfProbabilistic: Int = probabilistic.size

  override def probability(id: Int): Double = probabilistic.get(id)

  override def probability(args: Seq[String]): Double = {
    checkLength(args)
    val id = identity.encode(args)
    probabilistic.get(id)
  }

  def getBuilder: AtomEvidenceDBBuilder = {
    AtomEvidenceDBBuilder(
      identity, isCWA = false,
      positivesOpt     = Some(positives),
      negativesOpt     = Some(negatives),
      probabilisticOpt = Some(probabilistic))
  }

  def dumpContents(implicit out: PrintStream = System.out): Unit = {
    out.println("Positives:")
    val iteratorPos = positives.iterator
    while (iteratorPos.hasNext) {
      val atomID = iteratorPos.next
      val constants = identity.decode(atomID).get
      out.println(identity.signature.symbol + "(" + constants.map(_.toString).reduceLeft(_ + "," + _) + ")")
    }

    out.println("\nNegatives:")
    val iteratorNeg = positives.iterator
    while (iteratorNeg.hasNext) {
      val atomID = iteratorNeg.next
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

private class DBProbCWA(
    positives: TIntSet,
    val probabilistic: TIntDoubleMap,
    override val identity: AtomIdentityFunction) extends AtomEvidenceDB(identity) {

  protected def fetch(id: Int): TriState = {
    if (positives.contains(id)) TRUE
    else if (probabilistic.containsKey(id)) UNKNOWN
    else FALSE
  }

  def numberOfTrue: Int = positives.size

  def numberOfFalse: Int = identity.length - (numberOfUnknown + numberOfTrue)

  def numberOfUnknown: Int = probabilistic.size

  def numberOfProbabilistic: Int = probabilistic.size

  override def probability(id: Int): Double = probabilistic.get(id)

  override def probability(args: Seq[String]): Double = {
    checkLength(args)
    val id = identity.encode(args)
    probabilistic.get(id)
  }

  def getBuilder: AtomEvidenceDBBuilder = {
    AtomEvidenceDBBuilder(
      identity,
      isCWA            = true,
      positivesOpt     = Some(positives),
      probabilisticOpt = Some(probabilistic))
  }

  def dumpContents(implicit out: PrintStream = System.out): Unit = {
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
