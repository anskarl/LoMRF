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

/**
  * Evidence builder (fluent interface)
  */
final class EvidenceBuilder private (
    predicateSpace: PredicateSpace,
    constants: ConstantsDomain,
    predicateSchema: PredicateSchema,
    functionSchema: FunctionSchema,
    convertFunctionsToPredicates: Boolean) { self =>

  private var edbBuilders = Map.empty[AtomSignature, AtomEvidenceDBBuilder]

  private var fmBuilders = Map.empty[AtomSignature, FunctionMapperBuilder]

  private var dynFunctions = Map.empty[AtomSignature, Vector[String] => String]

  private var forceCWA = false

  private val functionRegister =
    if (convertFunctionsToPredicates) new FunctionToAUXPredRegister else new DefaultFunctionRegister

  def withEvidenceBuilders(evBuilders: Map[AtomSignature, AtomEvidenceDBBuilder]): EvidenceBuilder = {

    val missingSignatures = evBuilders.keys.filterNot(functionSchema.contains)

    if (missingSignatures.nonEmpty)
      throw new IllegalArgumentException(
        "Cannot have atom evidence builders for predicates with unspecified schema. " +
          s"The following atom signatures are missing from the predicate schema: '${missingSignatures.mkString(", ")}'")

    val result = new EvidenceBuilder(predicateSpace, constants, predicateSchema, functionSchema, convertFunctionsToPredicates)
    result.edbBuilders = evBuilders
    result
  }

  def withFunctionBuilders(fmBuilders: Map[AtomSignature, FunctionMapperBuilder]): EvidenceBuilder = {
    if (functionSchema.isEmpty)
      throw new IllegalArgumentException("Cannot have function mapping builders when function schema is missing.")

    val missingSignatures = fmBuilders.keys.filterNot(functionSchema.contains)

    if (missingSignatures.nonEmpty)
      throw new IllegalArgumentException(
        "Cannot have function mapping builders for functions with unspecified schema. " +
          s"The following function signatures are missing from the function schema: '${missingSignatures.mkString(", ")}'")

    val result = new EvidenceBuilder(predicateSpace, constants, predicateSchema, functionSchema, convertFunctionsToPredicates)
    if (convertFunctionsToPredicates) {
      fmBuilders.values foreach {
        builder => self.functions ++= builder.decoded
      }
    } else {
      result.fmBuilders = fmBuilders
    }

    result
  }

  def withDynamicFunctions(df: DynamicFunctions): EvidenceBuilder = {
    val result = new EvidenceBuilder(predicateSpace, constants, predicateSchema, functionSchema, convertFunctionsToPredicates)
    result.dynFunctions = df
    result
  }

  def withCWAForAll(v: Boolean = true): EvidenceBuilder = {
    val result = new EvidenceBuilder(predicateSpace, constants, predicateSchema, functionSchema, convertFunctionsToPredicates)
    result.forceCWA = v
    result
  }

  def clear(): Unit = {
    edbBuilders = Map.empty[AtomSignature, AtomEvidenceDBBuilder]
    fmBuilders = Map.empty[AtomSignature, FunctionMapperBuilder]
  }

  def result(): Evidence = {

      def mkEvidenceDB(signature: AtomSignature): AtomEvidenceDB = {
        edbBuilders.get(signature) match {
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
        yield signature -> mkEvidenceDB(signature)).to(Map)

    val dynamicFunctionMappers = dynFunctions.view.mapValues(new FunctionMapperSpecialImpl(_)).to(Map)

    val fm =
      if (convertFunctionsToPredicates) dynamicFunctionMappers
      else dynamicFunctionMappers ++ fmBuilders.view.mapValues(_.result()).to(Map)

    new Evidence(constants, db, fm)
  }

  object evidence {

    def update(evb: Map[AtomSignature, AtomEvidenceDB]): self.type = {
      edbBuilders = evb.map {
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
      edbBuilders = evb.map(db => db.identity.signature -> AtomEvidenceDBBuilder(db.identity, db.numberOfUnknown > 0)).toMap
      self
    }

    def +=(atom: EvidenceAtom): self.type = {
      insert(atom)
      self
    }

    def ++=(atoms: Iterable[EvidenceAtom]): self.type = {
      atoms.foreach(insert)
      self
    }

    def ++=(atoms: EvidenceAtom*): self.type = {
      atoms.foreach(insert)
      self
    }

    def clear(): Unit = {
      if (convertFunctionsToPredicates)
        edbBuilders = edbBuilders.view.filterKeys(signature => functionSchema.contains(signature)).toMap
      else
        edbBuilders = Map.empty[AtomSignature, AtomEvidenceDBBuilder]
    }

    private def insert(atom: EvidenceAtom): Unit = {
      edbBuilders.get(atom.signature) match {
        case Some(builder) =>
          builder += atom

        case None if predicateSchema.contains(atom.signature) =>

          val idf = predicateSpace.identities(atom.signature)
          val isCWA = if (self.forceCWA) true else predicateSpace.isCWA(atom.signature)
          val builder = AtomEvidenceDBBuilder(idf, isCWA)
          builder += atom

          edbBuilders += (atom.signature -> builder)

        case _ =>
          throw new IllegalArgumentException(s"Unknown atom signature for atom '${atom.toText}'")
      }
    }
  }

  object functions {

    def +=(fm: FunctionMapping): self.type = {
      functionRegister.insert(fm)
      self
    }

    def ++=(fms: Iterable[FunctionMapping]): self.type = {
      fms.foreach(functionRegister.insert)
      self
    }

    def ++=(fms: FunctionMapping*): self.type = {
      fms.foreach(functionRegister.insert)
      self
    }

    def clear(): Unit = {
      if (convertFunctionsToPredicates)
        edbBuilders = edbBuilders -- functionSchema.keys

      fmBuilders = Map.empty[AtomSignature, FunctionMapperBuilder]
    }

    def result(): Map[AtomSignature, FunctionMapper] = fmBuilders.map(entries => entries._1 -> entries._2.result())
  }

  private sealed trait FunctionRegister {
    def insert(fm: FunctionMapping): Unit
  }

  private final class DefaultFunctionRegister extends FunctionRegister {
    override def insert(fm: FunctionMapping): Unit = {
      fmBuilders.get(fm.signature) match {
        case Some(fMappingBuilder) =>
          fMappingBuilder += (fm.values, fm.retValue)

        case None if functionSchema.contains(fm.signature) =>

          val idFunction = AtomIdentityFunction(fm.signature, functionSchema(fm.signature)._2, constants, 1)
          val builder = new FunctionMapperBuilder(idFunction)
          builder += (fm.values, fm.retValue)

          fmBuilders += (fm.signature -> builder)

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

      fmBuilders.get(fm.signature) match {
        case Some(fMappingBuilder) =>
          fMappingBuilder += (fm.values, fm.retValue)

        case None if functionSchema.contains(fm.signature) =>

          val idFunction = AtomIdentityFunction(fm.signature, functionSchema(fm.signature)._2, constants, 1)
          val builder = new FunctionMapperBuilder(idFunction)
          builder += (fm.values, fm.retValue)

          fmBuilders += (fm.signature -> builder)

        case _ =>
          throw new IllegalArgumentException(s"Unknown function signature for function mapping '${fm.toString}'")
      }
    }
  }

}

object EvidenceBuilder {

  /**
    * Construct an EvidenceBuilder for the specified predicate schema, query predicates, hidden predicates and constants.
    *
    * @param predicateSchema a mapping of atomic signature (i.e., Predicate_Symbol/Arity) to the predicate's term types
    * @param queryPredicates a collection atomic signatures of the query predicates
    * @param hiddenPredicates a collection atomic signatures of the hidden predicates
    * @param constants a collection of the domain of constants
    *
    * @return new instance of an EvidenceBuilder
    * @see PredicateSchema
    * @see AtomSignature
    */
  def apply(
      predicateSchema: PredicateSchema,
      queryPredicates: Set[AtomSignature],
      hiddenPredicates: Set[AtomSignature],
      constants: ConstantsDomain): EvidenceBuilder = {

    val domainSpace = PredicateSpace(predicateSchema, queryPredicates, hiddenPredicates, constants)

    new EvidenceBuilder(domainSpace, constants, predicateSchema, functionSchema = Map.empty, convertFunctionsToPredicates = false)
  }

  /**
    * Construct an EvidenceBuilder for the specified predicate and function schema, as well as, for the specified
    * query predicates, hidden predicates and constant domains.
    *
    * @param predicateSchema a mapping of atomic signature (i.e., Predicate_Symbol/Arity) to the predicates' term types
    * @param functionSchema a mapping of atomic signature (i.e., Function_Symbol/Arity) to the functions' term types
    * @param queryPredicates a collection atomic signatures of the query predicates
    * @param hiddenPredicates a collection atomic signatures of the hidden predicates
    * @param constants a collection of the domain of constants
    * @param convertFunctionsToPredicates (optional, default is false) when it is given as true, all specified functions
    *                                     will be converted and represented as auxiliary predicates.
    *
    * @return new instance of an EvidenceBuilder
    * @see PredicateSchema
    * @see AtomSignature
    */
  def apply(
      predicateSchema: PredicateSchema,
      functionSchema: FunctionSchema,
      queryPredicates: Set[AtomSignature],
      hiddenPredicates: Set[AtomSignature],
      constants: ConstantsDomain,
      convertFunctionsToPredicates: Boolean = false): EvidenceBuilder = {

    // When `convertFunctionsToPredicates` is true,
    // we should convert all entries in `functionSchema` as the entries in `predicateSchema`
    val finalPredicateSchema =
      if (convertFunctionsToPredicates) predicateSchema ++ functionSchema.toPredicateSchema
      else predicateSchema

    val domainSpace = PredicateSpace(finalPredicateSchema, queryPredicates, hiddenPredicates, constants)

    new EvidenceBuilder(domainSpace, constants, finalPredicateSchema, functionSchema, convertFunctionsToPredicates)
  }

}
