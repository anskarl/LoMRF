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

package lomrf.mln.model.builders

import com.typesafe.scalalogging.LazyLogging
import gnu.trove.TCollections
import gnu.trove.map.TIntDoubleMap
import gnu.trove.map.hash.TIntDoubleHashMap
import gnu.trove.set.TIntSet
import gnu.trove.set.hash.TIntHashSet
import lomrf.logic.{ AtomSignature, EvidenceAtom, FALSE, TRUE, UNKNOWN }
import lomrf.mln.model.{ AtomEvidenceDB, AtomIdentityFunction, ConstantsDomain }

/**
  * Atom evidence database builder.
  *
  * @param signature an atom signature
  * @param schema a sequence of domain arguments
  * @param identity an atom identity function
  * @param isCWA force close-world assumption
  * @param positives a set of True atom IDs
  * @param negatives a set of False atom IDs
  * @param unknown a set of Unknown atom IDs
  * @param probabilistic a map from atom IDs to probabilities
  */
final class AtomEvidenceDBBuilder private (
    val signature: AtomSignature,
    schema: Seq[String],
    identity: AtomIdentityFunction,
    isCWA: Boolean,
    private var positives: TIntHashSet,
    private var negatives: TIntHashSet,
    private var unknown: TIntHashSet,
    private var probabilistic: TIntDoubleHashMap) extends LazyLogging {

  private var dirty = false

  def this(signature: AtomSignature, schema: Seq[String], identity: AtomIdentityFunction, isCWA: Boolean) =
    this(signature, schema, identity, isCWA, new TIntHashSet(), new TIntHashSet(), new TIntHashSet(), new TIntDoubleHashMap())

  /**
    * Copies all evidence atoms to new collections in case the result function has
    * been called. That way the builder avoids side-effects, that is the resulting atom
    * evidence database behave like immutable collections.
    */
  private def copyIfDirty(): Unit = if (dirty) {
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

  /**
    * Puts a literal ID to the appropriate collection of atoms.
    *
    * @param literal a literal ID
    */
  private def putLiteral(literal: Int): Unit = {
    if (literal > 0) {
      if (negatives.contains(literal))
        logger.error(s"Atom $literal is defined both as positive and negative.")
      else if (probabilistic.contains(literal))
        logger.error(s"Atom $literal is defined both as positive and probabilistic/unknown.")
      else positives.add(literal)
    } else {
      val id = -literal
      if (positives.contains(id))
        logger.error(s"Atom $id is defined both as positive and negative.")
      else if (probabilistic.contains(id))
        logger.error(s"Atom $id is defined both as negative and probabilistic/unknown.")
      else negatives.add(id)
    }
  }

  /**
    * Adds the given evidence atom to the evidence database.
    *
    * @see [[lomrf.mln.model.AtomIdentityFunction]]
    * @param atom an evidence atom
    * @throws NoSuchElementException in case the given atom cannot be encoded into a valid ID.
    */
  def +=(atom: EvidenceAtom): Unit = {
    require(atom.signature == signature, s"You are trying to store atom $atom in a database for $signature atoms.")
    copyIfDirty()

    val id = identity.encode(atom)

    if (id == AtomIdentityFunction.IDENTITY_NOT_EXIST)
      throw new NoSuchElementException(s"Failed to compute the unique identification number for atom: '${atom.toText}'")

    atom.state match {
      case TRUE =>
        if (negatives.contains(id)) logger.error(s"Atom '${atom.toText}' is defined both as positive and negative in the given evidence db. Will keep only the first definition, which is negative.")
        else if (probabilistic.contains(id)) logger.error(s"Atom '${atom.toText}' is defined both as positive and probabilistic/unknown in the given evidence db. Will keep only the first definition, which is probabilistic/unknown.")
        else positives.add(id)

      case FALSE =>
        if (positives.contains(id)) logger.error(s"Atom '${atom.toText}' is defined both as positive and negative in the given evidence db. Will keep only the first definition, which is positive.")
        else if (probabilistic.contains(id)) logger.error(s"Atom '${atom.toText}' is defined both as negative and probabilistic/unknown in the given evidence db. Will keep only the first definition, which is probabilistic/unknown.")
        else negatives.add(id)

      case UNKNOWN =>
        if (positives.contains(id)) logger.error(s"Atom '${atom.toText}' is defined both as positive and probabilistic/unknown in the given evidence db. Will keep only the first definition, which is positive.")
        else if (negatives.contains(id)) logger.error(s"Atom '${atom.toText}' is defined both as negative and probabilistic/unknown in the given evidence db. Will keep only the first definition, which is negative.")
        else {
          atom.probability match {
            case 1.0 => putLiteral(id) // interpret as non-probabilistic evidence (state = TRUE)
            case 0.0 => putLiteral(-id) // interpret as non-probabilistic evidence (state = FALSE)
            case _ =>
              if (atom.probability == 0.5 || atom.probability.isNaN) {

                // A. Interpret it as evidence atom with unknown values:
                if (probabilistic.containsKey(id))
                  logger.error(s"Cannot reassign the probabilistic atom '${atom.toText}' as non-probabilistic with unknown state.")
                else unknown.add(id)
              } else {

                // B. Interpret it as probabilistic evidence atom:
                require(
                  atom.probability <= 1.0 && atom.probability >= 0.0,
                  "The specified probability value (" + atom.probability + ") of atom " + atom + " is outside the range [0,1].")

                val storedProb = probabilistic.putIfAbsent(id, atom.probability)

                if (storedProb != probabilistic.getNoEntryValue)
                  logger.error(s"Cannot reassign a different probability (${atom.probability}) for atom '${atom.toText}' (stored probability is $storedProb).")
              } // end case _
          } // end match atom.probability
        } // end case UNKNOWN
    } // end match atom.state
  }

  /**
    * Adds the given literal ID to the evidence database.
    *
    * @note An ID corresponds to a True literal if the given integer is positive,
    *       otherwise the atom is assumed to be False.
    * @see [[lomrf.mln.model.AtomIdentityFunction]]
    * @param literal a literal ID
    */
  def +=(literal: Int): Unit = {
    val atomID = math.abs(literal)
    require(atomID >= identity.startID && atomID < identity.endID, "The given literal has incompatible id.")
    copyIfDirty()
    putLiteral(literal)
  }

  /**
    * Creates an AtomEvidenceDB from all the given evidence atoms.
    *
    * @note If CWA is forced then every unspecified atom is assumed to be False.
    * @return an AtomEvidenceDB instance
    */
  def result(): AtomEvidenceDB = {
    dirty = true

    if (isCWA) {
      (probabilistic.isEmpty, unknown.isEmpty) match {
        // Evidence atom with standard closed-world assumption, that is all unspecified atoms have False state.
        case (true, true)  => AtomEvidenceDB.CWA(TCollections.unmodifiableSet(positives), identity)
        // Evidence atoms where their state is either True or Unknown. For the rest (unspecified states) we use closed-world assumption.
        case (true, false) => AtomEvidenceDB.CWA(TCollections.unmodifiableSet(positives), TCollections.unmodifiableSet(unknown), identity)
        // Probabilistic evidence atoms, where their state is either True or Probabilistic. For the rest (unspecified states) we use closed-world assumption.
        case (false, true) => AtomEvidenceDB.CWA(TCollections.unmodifiableSet(positives), TCollections.unmodifiableMap(probabilistic), identity)
        // The same with the previous, but all collected atoms with unknown state are translated as probabilistic with probability equal to 0.5
        case (false, false) =>
          val iterator = unknown.iterator
          while (iterator.hasNext) probabilistic.put(iterator.next, 0.5)

          AtomEvidenceDB.CWA(TCollections.unmodifiableSet(positives), TCollections.unmodifiableMap(probabilistic), identity)
      }
    } else {
      if (probabilistic.isEmpty)
        AtomEvidenceDB.OWA(TCollections.unmodifiableSet(positives), TCollections.unmodifiableSet(negatives), identity)
      else
        AtomEvidenceDB.OWA(TCollections.unmodifiableSet(positives), TCollections.unmodifiableSet(negatives), TCollections.unmodifiableMap(probabilistic), identity)
    }
  }
}

object AtomEvidenceDBBuilder {

  /**
    * Creates an atom evidence builder.
    *
    * @param identity an atom identity function
    * @param isCWA force close-world assumption
    * @param positivesOpt a set of True atom IDs (optional)
    * @param negativesOpt a set of False atom IDs (optional)
    * @param unknownOpt a set of Unknown atom IDs (optional)
    * @param probabilisticOpt a map from atom IDs to probabilities (optional)
    * @return an AtomEvidenceDBBuilder instance
    */
  def apply(
      identity: AtomIdentityFunction,
      isCWA: Boolean,
      positivesOpt: Option[TIntSet] = None,
      negativesOpt: Option[TIntSet] = None,
      unknownOpt: Option[TIntSet] = None,
      probabilisticOpt: Option[TIntDoubleMap] = None): AtomEvidenceDBBuilder = {

    val positives = positivesOpt match {
      case Some(set) => new TIntHashSet(set)
      case None      => new TIntHashSet()
    }

    val negatives = negativesOpt match {
      case Some(set) => new TIntHashSet(set)
      case None      => new TIntHashSet()
    }

    val probabilistic = probabilisticOpt match {
      case Some(x) => new TIntDoubleHashMap(x)
      case None    => new TIntDoubleHashMap()
    }

    val unknown = unknownOpt match {
      case Some(x) => new TIntHashSet(x)
      case None    => new TIntHashSet()
    }

    new AtomEvidenceDBBuilder(identity.signature, identity.schema, identity, isCWA, positives, negatives, unknown, probabilistic)
  }

  /**
    * Creates an atom evidence builder.
    *
    * @param identity an atom identity function
    * @param isCWA force close-world assumption
    * @return an AtomEvidenceDBBuilder instance
    */
  def apply(
      identity: AtomIdentityFunction,
      isCWA: Boolean): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(identity.signature, identity.schema, identity, isCWA)

  /**
    * Creates an atom evidence builder.
    *
    * @param schema a sequence of domain arguments
    * @param identity an atom identity function
    * @param isCWA force close-world assumption
    * @return an AtomEvidenceDBBuilder instance
    */
  def apply(
      schema: Seq[String],
      identity: AtomIdentityFunction,
      isCWA: Boolean): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(identity.signature, schema, identity, isCWA)

  /**
    * Creates an atom evidence builder.
    *
    * @param signature an atom signature
    * @param schema a sequence of domain arguments
    * @param identity an atom identity function
    * @param isCWA force close-world assumption
    * @return an AtomEvidenceDBBuilder instance
    */
  def apply(
      signature: AtomSignature,
      schema: Seq[String],
      identity: AtomIdentityFunction,
      isCWA: Boolean): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(signature, schema, identity, isCWA)

  /**
    * Creates an atom evidence builder.
    *
    * @param signature an atom signature
    * @param schema a sequence of domain arguments
    * @param constants a constants domain
    * @param startID the start ID of the evidence atoms
    * @param isCWA force close-world assumption
    * @return an AtomEvidenceDBBuilder instance
    */
  def apply(
      signature: AtomSignature,
      schema: Seq[String],
      constants: ConstantsDomain,
      startID: Int,
      isCWA: Boolean): AtomEvidenceDBBuilder = {

    require(signature.arity == schema.size,
      "The arity of the specified signature must be equal to the number of schema elements.")

    new AtomEvidenceDBBuilder(signature, schema, AtomIdentityFunction(signature, schema, constants, startID), isCWA)
  }

  object CWA {

    /**
      * Creates a close-world assumption atom evidence builder.
      *
      * @param identity an atom identity function
      * @return an AtomEvidenceDBBuilder instance
      */
    def apply(identity: AtomIdentityFunction): AtomEvidenceDBBuilder =
      new AtomEvidenceDBBuilder(identity.signature, identity.schema, identity, true)

    /**
      * Creates a close-world assumption atom evidence builder.
      *
      * @param schema a sequence of domain arguments
      * @param identity an atom identity function
      * @return an AtomEvidenceDBBuilder instance
      */
    def apply(
        schema: Seq[String],
        identity: AtomIdentityFunction): AtomEvidenceDBBuilder =
      new AtomEvidenceDBBuilder(identity.signature, schema, identity, true)

    /**
      * Creates a close-world assumption atom evidence builder.
      *
      * @param signature an atom signature
      * @param schema a sequence of domain arguments
      * @param identity an atom identity function
      * @return an AtomEvidenceDBBuilder instance
      */
    def apply(
        signature: AtomSignature,
        schema: Seq[String],
        identity: AtomIdentityFunction): AtomEvidenceDBBuilder =
      new AtomEvidenceDBBuilder(signature, schema, identity, true)

    /**
      * Creates a close-world assumption atom evidence builder.
      *
      * @param signature an atom signature
      * @param schema a sequence of domain arguments
      * @param constants a constants domain
      * @param startID the start ID of the evidence atoms
      * @return an AtomEvidenceDBBuilder instance
      */
    def apply(
        signature: AtomSignature,
        schema: Seq[String],
        constants: ConstantsDomain,
        startID: Int): AtomEvidenceDBBuilder = {

      require(signature.arity == schema.size,
        "The arity of the specified signature must be equal to the number of schema elements.")

      new AtomEvidenceDBBuilder(signature, schema, AtomIdentityFunction(signature, schema, constants, startID), true)
    }
  }

  object OWA {

    /**
      * Creates a open-world assumption atom evidence builder.
      *
      * @param identity an atom identity function
      * @return an AtomEvidenceDBBuilder instance
      */
    def apply(identity: AtomIdentityFunction): AtomEvidenceDBBuilder =
      new AtomEvidenceDBBuilder(identity.signature, identity.schema, identity, false)

    /**
      * Creates a open-world assumption atom evidence builder.
      *
      * @param schema a sequence of domain arguments
      * @param identity an atom identity function
      * @return an AtomEvidenceDBBuilder instance
      */
    def apply(
        schema: Seq[String],
        identity: AtomIdentityFunction): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(identity.signature, schema, identity, false)

    /**
      * Creates a open-world assumption atom evidence builder.
      *
      * @param signature an atom signature
      * @param schema a sequence of domain arguments
      * @param identity an atom identity function
      * @return an AtomEvidenceDBBuilder instance
      */
    def apply(
        signature: AtomSignature,
        schema: Seq[String],
        identity: AtomIdentityFunction): AtomEvidenceDBBuilder = new AtomEvidenceDBBuilder(signature, schema, identity, false)

    /**
      * Creates a open-world assumption atom evidence builder.
      *
      * @param signature an atom signature
      * @param schema a sequence of domain arguments
      * @param constants a constants domain
      * @param startID the start ID of the evidence atoms
      * @return an AtomEvidenceDBBuilder instance
      */
    def apply(
        signature: AtomSignature,
        schema: Seq[String],
        constants: ConstantsDomain,
        startID: Int): AtomEvidenceDBBuilder = {

      require(signature.arity == schema.size,
        "The arity of the specified signature must be equal to the number of schema elements.")

      new AtomEvidenceDBBuilder(signature, schema, AtomIdentityFunction(signature, schema, constants, startID), false)
    }
  }
}

