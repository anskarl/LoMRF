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

import lomrf.logic.{ AtomSignature, Constant, EvidenceAtom, FunctionMapping }
import lomrf.mln.model._
import scala.collection.breakOut

/**
  * Evidence builder.
  *
  * @param predicateSpace a predicate space that encodes ground predicates using integers
  * @param constants a constants domain
  * @param predicateSchema a map from atom signatures to their argument domain names
  * @param functionSchema a map from function signatures to their return value domain name, argument domain names
  * @param convertFunctions convert functions to auxiliary predicates
  */
final class EvidenceBuilder private (
    predicateSpace: PredicateSpace,
    constants: ConstantsDomain,
    predicateSchema: PredicateSchema,
    functionSchema: FunctionSchema,
    convertFunctions: Boolean) { self =>

  private var forceCWA = false

  private var atomEvidenceBuilders = Map.empty[AtomSignature, AtomEvidenceDBBuilder]

  private var functionMapperBuilders = Map.empty[AtomSignature, FunctionMapperBuilder]

  private var dynamicFunctions = Map.empty[AtomSignature, Vector[String] => String]

  private val functionRegister =
    if (convertFunctions) new FunctionToAUXPredRegister
    else new DefaultFunctionRegister

  private sealed trait FunctionRegister {
    def insert(fm: FunctionMapping): Unit
  }

  private final class DefaultFunctionRegister extends FunctionRegister {
    override def insert(fm: FunctionMapping): Unit = {
      functionMapperBuilders.get(fm.signature) match {
        case Some(fMappingBuilder) =>
          fMappingBuilder += (fm.values, fm.retValue)

        case None if functionSchema.contains(fm.signature) =>

          val idFunction = AtomIdentityFunction(fm.signature, functionSchema(fm.signature)._2, constants, 1)
          val builder = new FunctionMapperBuilder(idFunction)
          builder += (fm.values, fm.retValue)

          functionMapperBuilders += (fm.signature -> builder)

        case _ =>
          throw new IllegalArgumentException(s"Unknown function signature for function mapping '${fm.toString}'")
      }
    }
  }

  private final class FunctionToAUXPredRegister extends FunctionRegister {
    override def insert(fm: FunctionMapping): Unit = {
      if (functionSchema.contains(fm.signature)) {
        val symbol = lomrf.AUX_PRED_PREFIX + fm.functionSymbol
        val terms = fm.values.+:(fm.retValue).map(Constant) // prepend fm.retValue and map to Constants

        self.evidence += EvidenceAtom.asTrue(symbol, terms)
      } else {
        throw new IllegalArgumentException(s"Unknown function signature for function mapping '${fm.toString}'")
      }

      functionMapperBuilders.get(fm.signature) match {
        case Some(fMappingBuilder) =>
          fMappingBuilder += (fm.values, fm.retValue)

        case None if functionSchema.contains(fm.signature) =>

          val idFunction = AtomIdentityFunction(fm.signature, functionSchema(fm.signature)._2, constants, 1)
          val builder = new FunctionMapperBuilder(idFunction)
          builder += (fm.values, fm.retValue)

          functionMapperBuilders += (fm.signature -> builder)

        case _ =>
          throw new IllegalArgumentException(s"Unknown function signature for function mapping '${fm.toString}'")
      }
    }
  }

  /**
    * Creates an evidence builder having the given atom evidence database builders per atom signature.
    *
    * @param builders a map from atom signatures to atom evidence database builder
    * @throws IllegalArgumentException in case a builder exists for unspecified atom signatures
    * @return an EvidenceBuilder instance
    */
  def withEvidenceBuilders(builders: Map[AtomSignature, AtomEvidenceDBBuilder]): EvidenceBuilder = {

    val missingSignatures = builders.keys.filterNot(functionSchema.contains)

    if (missingSignatures.nonEmpty)
      throw new IllegalArgumentException(
        "Cannot have atom evidence builders for predicates with unspecified schema. " +
          s"The following atom signatures are missing from the predicate schema: '${missingSignatures.mkString(", ")}'")

    val result = new EvidenceBuilder(predicateSpace, constants, predicateSchema, functionSchema, convertFunctions)
    result.atomEvidenceBuilders = builders
    result
  }

  /**
    * Creates an evidence builder having the given function mapper builders per function signature.
    *
    * @note Function signatures are essentially atom signatures.
    *
    * @param builders a map from function signatures to function mapper builder
    * @throws IllegalArgumentException in case builder exists for unspecified function signatures
    * @return an EvidenceBuilder instance
    */
  def withFunctionBuilders(builders: Map[AtomSignature, FunctionMapperBuilder]): EvidenceBuilder = {
    if (functionSchema.isEmpty)
      throw new IllegalArgumentException("Cannot have function mapping builders when function schema is missing.")

    val missingSignatures = builders.keys.filterNot(functionSchema.contains)

    if (missingSignatures.nonEmpty)
      throw new IllegalArgumentException(
        "Cannot have function mapping builders for functions with unspecified schema. " +
          s"The following function signatures are missing from the function schema: '${missingSignatures.mkString(", ")}'")

    val result = new EvidenceBuilder(predicateSpace, constants, predicateSchema, functionSchema, convertFunctions)
    if (convertFunctions) builders.values foreach {
      builder => self.functions ++= builder.decoded
    }
    else {
      result.functionMapperBuilders = builders
    }

    result
  }

  /**
    * Creates an evidence builder having the given dynamic functions.
    *
    * @param functions a map from function signatures to functions of the form '''Vector[String] => String'''
    * @return an EvidenceBuilder instance
    */
  def withDynamicFunctions(functions: DynamicFunctions): EvidenceBuilder = {
    val result = new EvidenceBuilder(predicateSpace, constants, predicateSchema, functionSchema, convertFunctions)
    result.dynamicFunctions = functions
    result
  }

  /**
    * Creates an evidence builder having close-world assumption.
    *
    * @param forceCWA force close-world assumption
    * @return an EvidenceBuilder instance
    */
  def withCWAForAll(forceCWA: Boolean = true): EvidenceBuilder = {
    val result = new EvidenceBuilder(predicateSpace, constants, predicateSchema, functionSchema, convertFunctions)
    result.forceCWA = forceCWA
    result
  }

  /**
    * Creates an evidence collection from the given evidence atoms and function mappings.
    *
    * @return an Evidence instance
    */
  def result(): Evidence = {
      def mkEvidenceDB(signature: AtomSignature): AtomEvidenceDB = {
        atomEvidenceBuilders.get(signature) match {
          case Some(builder) => builder.result()
          case None =>
            val idf = predicateSpace.identities(signature)
            if (forceCWA) AtomEvidenceDB.allFalse(idf)
            else {
              val isCWA = predicateSpace.isCWA(signature)
              if (isCWA) AtomEvidenceDB.allFalse(idf)
              else AtomEvidenceDB.allUnknown(idf)
            }
        }
      }

    val db: EvidenceDB = (
      for (signature <- predicateSchema.keys)
        yield signature -> mkEvidenceDB(signature))(breakOut)

    val dynamicFunctionMappers = dynamicFunctions.mapValues(new FunctionMapperSpecialImpl(_))

    val fm =
      if (convertFunctions) dynamicFunctionMappers
      else dynamicFunctionMappers ++ functionMapperBuilders.map { case (sig, fmb) => sig -> fmb.result() }

    new Evidence(constants, db, fm)
  }

  /**
    * Clears the builder.
    */
  def clear(): Unit = {
    atomEvidenceBuilders = Map.empty[AtomSignature, AtomEvidenceDBBuilder]
    functionMapperBuilders = Map.empty[AtomSignature, FunctionMapperBuilder]
  }

  object evidence {

    private def insert(atom: EvidenceAtom): Unit = {
      atomEvidenceBuilders.get(atom.signature) match {
        case Some(builder) =>
          builder += atom

        case None if predicateSchema.contains(atom.signature) =>

          val idf = predicateSpace.identities(atom.signature)
          val isCWA = if (self.forceCWA) true else predicateSpace.isCWA(atom.signature)
          val builder = AtomEvidenceDBBuilder(idf, isCWA)
          builder += atom

          atomEvidenceBuilders += (atom.signature -> builder)

        case _ =>
          throw new IllegalArgumentException(s"Unknown atom signature for atom '${atom.toText}'")
      }
    }

    def update(evb: Map[AtomSignature, AtomEvidenceDB]): self.type = {
      atomEvidenceBuilders = evb.map {
        case (signature, db) =>
          if (!predicateSchema.contains(signature))
            throw new IllegalArgumentException(s"Unknown atom signature '$signature'")

          val builder = AtomEvidenceDBBuilder(db.identity, db.numberOfUnknown > 0)

          require(
            signature == db.identity.signature,
            s"Something is wrong for key signature ${AtomSignature.toString}. " +
              s"The associated AtomEvidenceDB is associated with different signature (${db.identity.signature})")

          signature -> builder
      }
      self
    }

    def update(evb: Iterable[AtomEvidenceDB]): self.type = {
      atomEvidenceBuilders = evb.map(db => db.identity.signature -> AtomEvidenceDBBuilder(db.identity, db.numberOfUnknown > 0)).toMap
      self
    }

    /**
      * Adds the given evidence atom to the builder.
      *
      * @param atom an evidence atom
      * @return an EvidenceBuilder instance
      */
    def +=(atom: EvidenceAtom): self.type = {
      insert(atom)
      self
    }

    /**
      * Adds all given evidence atoms to the builder.
      *
      * @param atoms an iterable of evidence atoms
      * @return an EvidenceBuilder instance
      */
    def ++=(atoms: Iterable[EvidenceAtom]): self.type = {
      atoms.foreach(insert)
      self
    }

    /**
      * Adds all given evidence atoms to the builder.
      *
      * @param atoms a sequence of evidence atoms
      * @return an EvidenceBuilder instance
      */
    def ++=(atoms: EvidenceAtom*): self.type = {
      atoms.foreach(insert)
      self
    }

    /**
      *  Clears the stored evidence atoms.
      */
    def clear(): Unit = {
      if (convertFunctions) atomEvidenceBuilders = atomEvidenceBuilders.filterKeys(functionSchema.contains)
      else atomEvidenceBuilders = Map.empty[AtomSignature, AtomEvidenceDBBuilder]
    }
  }

  object functions {

    /**
      * Adds the given function mapping to the builder.
      *
      * @param fm a function mapping
      * @return an EvidenceBuilder instance
      */
    def +=(fm: FunctionMapping): self.type = {
      functionRegister.insert(fm)
      self
    }

    /**
      * Adds all given function mappings to the builder.
      *
      * @param fms an iterable of function mappings
      * @return an EvidenceBuilder instance
      */
    def ++=(fms: Iterable[FunctionMapping]): self.type = {
      fms.foreach(functionRegister.insert)
      self
    }

    /**
      * Adds all given function mappings to the builder.
      *
      * @param fms a sequence of function mappings
      * @return an EvidenceBuilder instance
      */
    def ++=(fms: FunctionMapping*): self.type = {
      fms.foreach(functionRegister.insert)
      self
    }

    /**
      * Creates function mappers from the given function mappings.
      *
      * @return a FunctionMappers instance
      */
    def result(): FunctionMappers = functionMapperBuilders.map { case (sig, fmb) => sig -> fmb.result() }

    /**
      * Clears the stored function mappings.
      */
    def clear(): Unit = {
      if (convertFunctions) atomEvidenceBuilders = atomEvidenceBuilders -- functionSchema.keys
      functionMapperBuilders = Map.empty[AtomSignature, FunctionMapperBuilder]
    }
  }
}

object EvidenceBuilder {

  /**
    * Creates an evidence builder.
    *
    * @see [[lomrf.mln.model.ConstantsSet]]
    *
    * @param predicateSchema a map from atom signatures to their argument domain names
    * @param queryPredicates a set of query atom signatures
    * @param hiddenPredicates a set of hidden atom signatures
    * @param constants a map from domain names to constants set
    * @return an EvidenceBuilder instance
    */
  def apply(
      predicateSchema: PredicateSchema,
      queryPredicates: Set[AtomSignature],
      hiddenPredicates: Set[AtomSignature],
      constants: ConstantsDomain): EvidenceBuilder = {

    val domainSpace = PredicateSpace(predicateSchema, queryPredicates, hiddenPredicates, constants)

    new EvidenceBuilder(domainSpace, constants, predicateSchema, functionSchema = Map.empty, convertFunctions = false)
  }

  /**
    * Creates an evidence builder.
    *
    * @see [[lomrf.mln.model.ConstantsSet]]
    *
    * @param predicateSchema a map from atom signatures to their argument domain names
    * @param functionSchema a map from function signatures to their return value domain name, argument domain names
    * @param queryPredicates a set of query atom signatures
    * @param hiddenPredicates a set of hidden atom signatures
    * @param constants a map from domain names to constants set
    * @param convertFunctions convert functions to auxiliary predicates
    * @return an EvidenceBuilder instance
    */
  def apply(
      predicateSchema: PredicateSchema,
      functionSchema: FunctionSchema,
      queryPredicates: Set[AtomSignature],
      hiddenPredicates: Set[AtomSignature],
      constants: ConstantsDomain,
      convertFunctions: Boolean = false): EvidenceBuilder = {

    /*
     * When 'convertFunctionsToPredicates' is true,
     * we should convert all entries in 'functionSchema' as the entries in 'predicateSchema'
     */
    val finalPredicateSchema =
      if (convertFunctions) predicateSchema ++ functionSchema.toPredicateSchema
      else predicateSchema

    val domainSpace = PredicateSpace(finalPredicateSchema, queryPredicates, hiddenPredicates, constants)

    new EvidenceBuilder(domainSpace, constants, finalPredicateSchema, functionSchema, convertFunctions)
  }
}
