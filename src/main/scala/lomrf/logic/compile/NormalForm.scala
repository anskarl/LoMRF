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

package lomrf.logic.compile

import lomrf.logic._
import lomrf.mln.model.ConstantsSet
import scala.annotation.tailrec
import scala.collection.mutable
import com.typesafe.scalalogging.LazyLogging
import scala.collection.parallel.CollectionConverters._

/**
  * Contains functions that convert a First Order Logic (FOL) formula into a normal form,
  * such as Negation Normal Form (NNF), Prenex Normal Form (PNF) and Clausal Normal Form (CNF).
  */
object NormalForm extends LazyLogging {

  /**
    * <p>
    * Clausal or Conjunctive Normal Form (CNF) is a conjunction of clauses,
    * where each clause is a disjunction of ground literals.
    * </p>
    * <p>
    * In order to transform a formula into a CNF, the following operations are performed:
    * <ol>
    * <li> '''I:''' Eliminate all implications and equivalences </li>
    * <li> '''N:''' Move inwards all negations </li>
    * <li> '''S:''' Standardize Variables </li>
    * <li> '''E:''' Remove Existential Quantifiers</li>
    * <li> '''A:''' Remove Universal Quantifiers</li>
    * <li> '''D:''' Distribute operators (disjunctions and conjunctions) </li>
    * <li> '''O:''' Remove operators</li>
    * </ol>
    * </p>
    *
    * <p>
    * Note that ''Skolemization'' is not applied at the ''E step'', as it is not sound in general in FOL. However,
    * we assume that the domain is finite, therefore an existentially quantified formula can simply be replaced
    * by a disjunction of its groundings.
    * </p>
    *
    * @see Wikipedia article
    *      [[http://en.wikipedia.org/wiki/Clausal_normal_form]]
    * @see Standford course CS157: Computational Logic, topic: Resolution Preliminaries (lecture 9)
    *      [[http://logic.stanford.edu/classes/cs157/2008/lectures/lecture09.pdf]]
    * @see Russell, S.J. and Norvig, P. and Canny, J.F. and Malik, J. and Edwards, D.D.
    *      Artificial Intelligence: A Modern Approach, chapter 9.5.1 Conjunctive normal form for first-order logic
    *      [[http://aima.cs.berkeley.edu/]]
    *
    * @param source a source formula
    * @param constants a constant set for each domain
    * @return a set of clauses
    */
  def toCNF(source: Formula)(implicit constants: Map[String, ConstantsSet] = Map.empty): Set[Clause] = {

      def normaliseConstruct(f: FormulaConstruct): FormulaConstruct = {
        distribute(
          removeUniversalQuantifiers(
            removeExistentialQuantifiers(
              constants,
              standardizeVariables(negationsIn(removeImplications(f))))))
      }

    val normalized = source match {
      case wf: WeightedFormula =>
        WeightedFormula(wf.weight, normaliseConstruct(wf.formula))

      case DefiniteClause(head, body) =>
        WeightedFormula.asHard(normaliseConstruct(Implies(body, head)))

      case WeightedDefiniteClause(weight, clause) =>
        WeightedFormula(weight, normaliseConstruct(Implies(clause.body, clause.head)))

      case c: FormulaConstruct =>
        WeightedFormula.asHard(normaliseConstruct(c))
    }

    extractClauses(normalized)
  }

  /**
    * Compile a sequence of formulas into conjunctive normal form.
    *
    * @param formulas a sequence of input formulas
    * @param constants a constant set for each domain
    * @return a set of clauses
    */
  def compileCNF(formulas: Iterable[Formula])(implicit constants: Map[String, ConstantsSet] = Map.empty): Set[Clause] = {
    formulas.par.foldLeft(Set.empty[Clause])((clauses, formula) => clauses ++ toCNF(formula))
  }

  /**
    * Compiles a formula to CNF. This version of CNF uses a fast distribute function in order to perform
    * fast distributive property in formulas having specific properties.
    *
    * @param source a source formula
    * @param constants a constant set for each domain
    * @return a set of clauses
    */
  def toFastCNF(source: Formula)(implicit constants: Map[String, ConstantsSet] = Map.empty): Set[Clause] = {

      def normaliseConstruct(f: FormulaConstruct): FormulaConstruct = {
        fastDistribute(
          removeUniversalQuantifiers(
            removeExistentialQuantifiersStack(
              constants,
              standardizeVariables(negationsIn(removeImplications(f))))))
      }

    val normalized = source match {
      case wf: WeightedFormula =>
        WeightedFormula(wf.weight, normaliseConstruct(wf.formula))

      case DefiniteClause(head, body) =>
        WeightedFormula.asHard(normaliseConstruct(Implies(body, head)))

      case WeightedDefiniteClause(weight, clause) =>
        WeightedFormula(weight, normaliseConstruct(Implies(clause.body, clause.head)))

      case c: FormulaConstruct =>
        WeightedFormula.asHard(normaliseConstruct(c))
    }

    extractClauses(normalized)
  }

  /**
    * Compile a sequence of formulas into conjunctive normal form.
    *
    * @note It uses a faster CNF implementation.
    *
    * @param formulas a sequence of input formulas
    * @param constants a constant set for each domain
    * @return a set of clauses
    */
  def compileFastCNF(formulas: Iterable[Formula])(implicit constants: Map[String, ConstantsSet] = Map.empty): Set[Clause] = {
    formulas.par.foldLeft(Set.empty[Clause])((clauses, formula) => clauses ++ toFastCNF(formula))
  }

  /**
    * Converts the given formula into Negation Normal Form (NNF). A logical formula is in NNF if negation
    * occurs only immediately above elementary propositions and (!,v,&#94;) are the only allowed boolean connectives.
    *
    * <ul>
    *   <li> '''I:''' Eliminate all implications and equivalences </li>
    *   <li> '''N:''' Move inwards all negations </li>
    * </ul>
    *
    * @param source an input formula
    * @tparam F the type of the formula
    * @return a formula in negation normal form (NNF)
    */
  def toNNF[F <: Formula](source: F): Formula = {
      def normaliseConstruct(f: FormulaConstruct): FormulaConstruct = negationsIn(removeImplications(f))

    source match {
      case WeightedFormula(weight, formula) =>
        WeightedFormula(weight, normaliseConstruct(formula))

      case DefiniteClause(head, body) =>
        normaliseConstruct(Implies(body, head))

      case WeightedDefiniteClause(weight, clause) =>
        WeightedFormula(weight, normaliseConstruct(Implies(clause.body, clause.head)))

      case c: FormulaConstruct =>
        normaliseConstruct(c)
    }
  }

  /**
    * Compile a sequence of formulas into negation normal form (NNF).
    *
    * @param formulas a sequence of input formulas
    * @return a set of formulas in negation normal form (NNF)
    */
  def compileNNF(formulas: Iterable[Formula]): Set[Formula] = {
    formulas.par.foldLeft(Set.empty[Formula])((formulas, formula) => formulas + toNNF(formula))
  }

  /**
    * Converts the given formula into Prenex Normal Form (PNF).
    *
    * <ul>
    *   <li> '''I:''' Eliminate all implications and equivalences </li>
    *   <li> '''N:''' Move inwards all negations </li>
    *   <li> '''M:''' Move all quantifiers outwards</li>
    * </ul>
    *
    * @param source an input formula
    * @tparam F the type of the formula
    * @return a formula in prenex normal form (PNF)
    */
  def toPNF[F <: Formula](source: F): Formula = {

      def normaliseConstruct(f: FormulaConstruct): FormulaConstruct =
        moveQuantifiersOutside(negationsIn(removeImplications(f)))

    source match {
      case WeightedFormula(weight, formula) =>
        WeightedFormula(weight, normaliseConstruct(formula))

      case DefiniteClause(head, body) =>
        normaliseConstruct(Implies(body, head))

      case WeightedDefiniteClause(weight, clause) =>
        WeightedFormula(weight, normaliseConstruct(Implies(clause.body, clause.head)))

      case c: FormulaConstruct =>
        normaliseConstruct(c)
    }
  }

  /**
    * Compile a sequence of formulas into prenex normal form.
    *
    * @param formulas a sequence of input formulas
    * @return a set of formulas
    */
  def compilePNF(formulas: Iterable[Formula]): Set[Formula] = {
    formulas.par.foldLeft(Set.empty[Formula])((formulas, formula) => formulas + toPNF(formula))
  }

  private def moveQuantifiersOutside(source: FormulaConstruct): FormulaConstruct = {
    val quantifiers = mutable.Queue[Quantifier]()

      def collect(src: FormulaConstruct): FormulaConstruct = src match {
        case f: AtomicFormula => f
        case q: Quantifier =>
          quantifiers.enqueue(q)
          collect(q.formula)
        case f: And => And(collect(f.left), collect(f.right))
        case f: Or  => Or(collect(f.left), collect(f.right))
        case f: Not => Not(collect(f.arg))
        case _ =>
          throw new IllegalStateException("Failed to move quantifiers outside, illegal formula: " + src.toText)
      }

      @tailrec
      def construct(insideFormula: FormulaConstruct): FormulaConstruct = {
        if (quantifiers.nonEmpty)
          quantifiers.dequeue() match {
            case f: ExistentialQuantifier => construct(ExistentialQuantifier(f.variable, insideFormula))
            case f: UniversalQuantifier   => construct(UniversalQuantifier(f.variable, insideFormula))
          }
        else insideFormula
      }

    val inside = collect(source)
    construct(inside)
  }

  /**
    * Eliminates all implications (=>) and equivalences (<=>) from the given formula.
    *
    * @param source an input formula construct
    * @return an equivalent formula construct without implications or equivalences
    */
  def removeImplications(source: FormulaConstruct): FormulaConstruct = {
    source match {
      case f: AtomicFormula         => f
      case f: Not                   => Not(removeImplications(f.arg))
      case f: And                   => And(removeImplications(f.left), removeImplications(f.right))
      case f: Or                    => Or(removeImplications(f.left), removeImplications(f.right))
      case f: UniversalQuantifier   => UniversalQuantifier(f.variable, removeImplications(f.formula))
      case f: ExistentialQuantifier => ExistentialQuantifier(f.variable, removeImplications(f.formula))

      case f: Equivalence           => removeImplications(And(Or(Not(f.left), f.right), Or(f.left, Not(f.right))))
      case f: Implies               => removeImplications(Or(Not(f.left), f.right))

      case _ =>
        throw new IllegalStateException("Failed to remove implications, illegal formula: " + source.toText)
    }
  }

  /**
    * Moves all negations inwards.
    *
    * @example
    * {{{ !(P(x) ^ !Q(x)) }}}
    *
    * should result to:
    *
    * {{{ !P(x) v Q(x) }}}
    *
    * @param source an input formula construct
    * @return an equivalent formula construct having only inward negations
    */
  def negationsIn(source: FormulaConstruct): FormulaConstruct = {
    source match {
      case f: AtomicFormula => f
      case Not(arg) => arg match { // Move negation inward

        // !(PvQ) -> !P ^ !Q
        case And(left, right)                     => negationsIn(Or(Not(left), Not(right)))

        // !(P^Q) -> !P v !Q
        case Or(left, right)                      => negationsIn(And(Not(left), Not(right)))

        // !(Exist x Q(x))-> Forall x !Q(x)
        case ExistentialQuantifier(vars, formula) => negationsIn(UniversalQuantifier(vars, Not(formula)))

        // !(Forall x Q(x)) -> Exist x Q(x)
        case UniversalQuantifier(vars, formula)   => negationsIn(ExistentialQuantifier(vars, Not(formula)))

        // !(!P(x)) -> P(x)
        case Not(formula)                         => negationsIn(formula)
        case _                                    => source
      }
      case f: And                   => And(negationsIn(f.left), negationsIn(f.right))
      case f: Or                    => Or(negationsIn(f.left), negationsIn(f.right))
      case f: UniversalQuantifier   => UniversalQuantifier(f.variable, negationsIn(f.formula))
      case f: ExistentialQuantifier => ExistentialQuantifier(f.variable, negationsIn(f.formula))
      case _ =>
        throw new IllegalStateException("Failed to move negations inside, illegal formula: " + source.toText)
    }
  }

  /**
    * Standardize variables to avoid mistakes when some variable names reappear in the formula,
    * but inside different quantification scopes.
    *
    * @example
    * {{{ Forall x P(x) ^ Exists x R(x)  }}}
    *
    * should be re-written as follows:
    *
    * {{{ Forall x P(x) ^ Exists x0 R(x0) }}}
    *
    * @param source an input formula construct
    * @return an equivalent formula construct having standardized variables
    */
  def standardizeVariables(source: FormulaConstruct): FormulaConstruct = {
    val vars = mutable.Map[Variable, Variable]()

      /**
        * Create a next variable name
        */
      def nextVar(x: Variable): Variable = {

          def produceNext(v: Variable): Variable = new Variable(v.symbol, v.domain, v.index + 1)

        val newVar = vars.get(x) match {
          case Some(v1) => produceNext(v1)
          case None     => produceNext(x)
        }

        vars(x) = newVar

        newVar
      }

      def stdVar(f: FormulaConstruct): FormulaConstruct = {
        f match {
          case x: AtomicFormula => x
          case x: And           => And(stdVar(x.left), stdVar(x.right))
          case x: Or            => Or(stdVar(x.left), stdVar(x.right))
          case x: Not           => Not(stdVar(x.arg))
          case x: ExistentialQuantifier =>
            val newVar = nextVar(x.variable)
            ExistentialQuantifier(newVar, stdVar(x.formula.substitute(Map(x.variable -> newVar))))
          case x: UniversalQuantifier =>
            val newVar = nextVar(x.variable)
            UniversalQuantifier(newVar, stdVar(x.formula.substitute(Map(x.variable -> newVar))))
          case _ => throw new IllegalStateException("Failed to standardize variables, illegal formula: " + f.toText)
        }
      }

    stdVar(source)
  }

  /**
    * Existentially quantified variables in formula's predicates are replaced with the disjunction of their groundings.
    *
    * @example Consider the following formula and that the domain of variable y is [0,...,3].
    * {{{ Exist y A(x,y) ^ B(y) }}}
    *
    * then, the formula should be replaced by:
    *
    * {{{ (A(x,0) ^ B(0)) v (A(x,1) ^ B(1)) v (A(x,2) ^ B(2)) v (A(x,3) ^ B(3)) }}}
    *
    * @param constants a constant set for each domain
    * @param source an input formula construct
    * @return an equivalent formula construct in which all existential quantifiers are eliminated
    */
  def removeExistentialQuantifiers(constants: Map[String, ConstantsSet], source: FormulaConstruct): FormulaConstruct = {

    source match {
      case x: AtomicFormula          => x
      case And(left, right)          => And(removeExistentialQuantifiers(constants, left), removeExistentialQuantifiers(constants, right))
      case Or(left, right)           => Or(removeExistentialQuantifiers(constants, left), removeExistentialQuantifiers(constants, right))
      case Not(f)                    => Not(removeExistentialQuantifiers(constants, f))
      case UniversalQuantifier(v, f) => UniversalQuantifier(v, removeExistentialQuantifiers(constants, f))
      case ExistentialQuantifier(v, f) =>
        // Get variable domain
        val constantsSet = constants.get(v.domain) match {
          case Some(x) => x
          case _       => throw new IllegalStateException("Cannot find constant literals for variable: " + v)
        }
        // Produce disjunctions
        val sliceIdx = constantsSet.size - 2
        if (sliceIdx >= 0) {
          val iter = constantsSet.iterator
          val zeta = Or(
            f.substitute(Map(v -> Constant(iter.next()))),
            f.substitute(Map(v -> Constant(iter.next()))))
          val result = iter.foldRight(zeta)((c, other: Or) =>
            Or(other, f.substitute(Map(v -> Constant(c)))))
          removeExistentialQuantifiers(constants, result)
        } else {
          removeExistentialQuantifiers(constants, f.substitute(Map(v -> Constant(constantsSet.head))))
        }
      case _ =>
        throw new IllegalStateException("Failed to remove existential quantifiers, illegal formula:" + source.toText)
    }
  }

  sealed trait Combiner
  private case class UnaryCombiner(c: FormulaConstruct => FormulaConstruct) extends Combiner
  private case class AndCombiner(c: (FormulaConstruct, FormulaConstruct) => FormulaConstruct) extends Combiner
  private case class OrCombiner(c: (FormulaConstruct, FormulaConstruct) => FormulaConstruct) extends Combiner

  /**
    * Existentially quantified variables in formula's predicates are replaced with the disjunction of their groundings.
    *
    * @example Consider the following formula and that the domain of variable y is [0,...,3].
    * {{{ Exist y A(x,y) ^ B(y) }}}
    *
    * then, the formula should be replaced by:
    *
    * {{{ (A(x,0) ^ B(0)) v (A(x,1) ^ B(1)) v (A(x,2) ^ B(2)) v (A(x,3) ^ B(3)) }}}
    *
    * @param constants a constant set for each domain
    * @param source an input formula construct
    * @return an equivalent formula construct in which all existential quantifiers are eliminated
    */
  def removeExistentialQuantifiersStack(constants: Map[String, ConstantsSet], source: FormulaConstruct): FormulaConstruct = {

    val stack = scala.collection.mutable.Stack[Either[Combiner, FormulaConstruct]]()
    val reconstructionStack = scala.collection.mutable.Stack[FormulaConstruct]()

    stack.push(Right(source))

    while (stack.nonEmpty) {
      val construct = stack.pop

      construct match {
        case Right(x: AtomicFormula) =>
          reconstructionStack.push(x)

        case Left(x: UnaryCombiner) =>
          reconstructionStack.push(x.c(reconstructionStack.pop))

        case Left(x: AndCombiner) =>
          val right = reconstructionStack.pop
          val left = reconstructionStack.pop
          reconstructionStack.push(x.c(left, right))

        case Left(x: OrCombiner) =>
          val right = reconstructionStack.pop
          val left = reconstructionStack.pop
          reconstructionStack.push(x.c(left, right))

        case Right(x: And) =>
          stack.push(Left(AndCombiner((l: FormulaConstruct, r: FormulaConstruct) => And(l, r))))
          stack.push(Right(x.right))
          stack.push(Right(x.left))

        case Right(x: Or) =>
          stack.push(Left(OrCombiner((l: FormulaConstruct, r: FormulaConstruct) => Or(l, r))))
          stack.push(Right(x.right))
          stack.push(Right(x.left))

        case Right(x: Not) =>
          stack.push(Left(UnaryCombiner((fc: FormulaConstruct) => Not(fc))))
          stack.push(Right(x.arg))

        case Right(x: UniversalQuantifier) =>
          stack.push(Left(UnaryCombiner((fc: FormulaConstruct) => UniversalQuantifier(x.v, fc))))
          stack.push(Right(x.f))

        case Right(x: ExistentialQuantifier) =>
          // Get variable domain
          val constantsSet = constants.get(x.v.domain) match {
            case Some(y) => y
            case _       => throw new IllegalStateException("Cannot find constant literals for variable: " + x.v)
          }

          val formula = x.f

          // Produce disjunctions
          val sliceIdx = constantsSet.size - 2
          if (sliceIdx >= 0) {
            val iterator = constantsSet.iterator
            val zeta = Or(
              formula.substitute(Map(x.v -> Constant(iterator.next))),
              formula.substitute(Map(x.v -> Constant(iterator.next))))

            val result = iterator.foldRight(zeta)((c, other: Or) =>
              Or(other, formula.substitute(Map(x.v -> Constant(c)))))

            stack.push(Right(result))
          } else stack.push(Right(formula.substitute(Map(x.v -> Constant(constantsSet.head)))))

        case _ =>
          throw new IllegalStateException("Failed to remove existential quantifiers, illegal formula:" + source.toText)
      }
    }

    reconstructionStack.pop
  }

  /**
    * Removes all universal quantifiers from the given formula.
    *
    * @param source an input formula construct
    * @return an equivalent formula construct without universal quantifiers
    */
  def removeUniversalQuantifiers(source: FormulaConstruct): FormulaConstruct = {
    source match {
      case x: AtomicFormula          => x
      case And(left, right)          => And(removeUniversalQuantifiers(left), removeUniversalQuantifiers(right))
      case Or(left, right)           => Or(removeUniversalQuantifiers(left), removeUniversalQuantifiers(right))
      case Not(f)                    => Not(removeUniversalQuantifiers(f))
      case UniversalQuantifier(_, f) => removeUniversalQuantifiers(f)
      case _ =>
        throw new IllegalStateException("Failed to remove universal quantifiers, illegal formula:" + source.toText)
    }
  }

  /**
    * Distribute all disjunctions inside all conjunctions in order to create the final clausal form.
    *
    * @param source an input formula construct
    * @return a formula construct in conjunctive normal form
    */
  def distribute(source: FormulaConstruct): FormulaConstruct = {
    var isDone = true

      def dist(source: FormulaConstruct): FormulaConstruct = {
        source match {
          case f: Not if f.arg.isInstanceOf[AtomicFormula] => f
          case f: AtomicFormula                            => f
          //(a ^ b) v (c ^ d) -> (a v c) ^ (a v d) ^ (b v c) ^ (b v d)
          case Or(And(a, b), And(c, d)) =>
            isDone = false
            dist(And(Or(a, c), And(Or(a, d), And(Or(b, c), Or(b, d)))))

          // a v ( c ^ d) -> a v c ^ a v d
          case Or(other, And(c, d)) if !other.isInstanceOf[And] =>
            isDone = false
            dist(And(Or(other, c), Or(other, d)))

          // (a ^ b) v d -> a v d ^ b v d
          case Or(And(a, b), other) if !other.isInstanceOf[And] =>
            isDone = false
            dist(And(Or(a, other), Or(b, other)))

          case Or(left, right)  => Or(dist(left), dist(right))

          case And(left, right) => And(dist(left), dist(right))

          case _ =>
            throw new IllegalStateException("Failed to distribute disjunctions inside conjunctions, illegal formula type: " + source.toText)
        }
      }

    var result = dist(source)
    while (!isDone) {
      isDone = true
      result = dist(result)
    }

    result
  }

  /**
    * Extracts clauses from the given formula (produced by [[lomrf.logic.compile.NormalForm.distribute]]).
    *
    * <p>
    * In an effort to preserve the original formula as much as possible, all single literals
    * in a conjunction are kept together by negating the formula: the weight is negated and the
    * formula becomes a disjunction of the negated literals.
    * </p>
    *
    * @example
    * {{{ 2 P(x) ^ Q(x) ^ (R(x) v S(x)) }}}
    *
    * should result into the following clauses:
    *
    * {{{
    *   -1 !P(x) v !Q(x)
    *
    *   1 R(x) v S(x)
    * }}}
    *
    * @param formula an input formula as resulted from [[lomrf.logic.compile.NormalForm.distribute]]
    * @return a set of clauses
    */
  @tailrec
  def extractClauses(formula: Formula): Set[Clause] = {

      def extractLiterals(source: Formula): (Set[Literal], Set[Set[Literal]]) = {

        var units = Set[Literal]()
        var nonUnits = Set[Set[Literal]]()

          def collectLiterals(source: Formula) {
            source match {
              case f: WeightedFormula    => collectLiterals(f.formula)
              case f: AtomicFormula      => units += PositiveLiteral(f)
              case Not(f: AtomicFormula) => units += NegativeLiteral(f)
              case And(left, right) =>
                collectLiterals(left)
                collectLiterals(right)
              case f: Or =>

                var literals = Set[Literal]()
                var isTautology = false

                  def walkInsideDisjunction(src: FormulaConstruct) {
                    src match {
                      case Not(s: AtomicFormula) =>
                        // check for tautology
                        if (!literals.contains(PositiveLiteral(s))) literals += NegativeLiteral(s)
                        else isTautology = true
                      case s: AtomicFormula =>
                        if (!literals.contains(NegativeLiteral(s))) literals += PositiveLiteral(s)
                        else isTautology = true
                      case a: And => collectLiterals(a)
                      case Or(left, right) =>
                        walkInsideDisjunction(left)
                        walkInsideDisjunction(right)
                      case _ => throw new IllegalStateException("Failed to walk inside a disjunction, illegal formula: " + src.toText)
                    }
                  }

                walkInsideDisjunction(f)

                if (literals.size == 1) {
                  units += literals.head
                } else if (literals.size > 1) {
                  if (!isTautology) nonUnits += literals
                  else logger.warn("tautology clause is produced")
                }
              case _ => throw new IllegalStateException("Failed to collect literals, illegal formula: " + source.toText)
            }
          }

        collectLiterals(formula)
        (units, nonUnits)
      }

      // Creates a set of weighted clauses from a set of clauses
      def createWeightedClauses(weightVal: Double, cs: Set[Set[Literal]]): Set[Clause] =
        cs.foldRight(Set[Clause]())((a, b) => Set(new Clause(weightVal, a)) ++ b)

    formula match {
      case wdc: WeightedDefiniteClause =>
        extractClauses(wdc.toWeightedFormula)

      case dc: DefiniteClause =>
        extractClauses(dc.toWeightedFormula)

      case WeightedFormula(weight, f) =>
        val (unit, nonUnit) = extractLiterals(f)
        assert((unit.size + nonUnit.size) > 0)

        val clauseWeight =
          if (weight == Double.PositiveInfinity) weight
          else if (unit.nonEmpty) weight / (nonUnit.size + 1)
          else weight / nonUnit.size

        // Return the extracted clauses:
        // one unit clause and possibly some other non-unit clauses
        if (unit.size == 1) createWeightedClauses(clauseWeight, nonUnit) + Clause(unit, clauseWeight)
        else if (unit.size > 1) {
          // when more than one unit clauses are produced merge them into a single clause.
          val mergedUnitsClauses = Clause(unit.map(_.negate), -clauseWeight)
          Set(mergedUnitsClauses) ++ createWeightedClauses(clauseWeight, nonUnit)
        } else createWeightedClauses(clauseWeight, nonUnit) // no unit clauses.

      case _ => throw new IllegalStateException("Failed to extract clauses, illegal formula: " + formula.toText)
    }
  }

  /**
    * A fast version of the distribute function. It checks if the formula construct has the property of
    * all AND operators appearing inside OR operators. In case this property holds, then the distribution
    * is performed in a much faster and more memory efficient way, by using an integer encoding. In any other
    * case the generic distribute function is called.
    *
    * @param source an input formula construct
    * @return an equivalent formula construct resulted from the appliance of the distributive property
    */
  private def fastDistribute(source: FormulaConstruct): FormulaConstruct = {

    var keyspace = 2
    var keyToAtom = Map[Int, FormulaConstruct]()
    var atomToKey = Map[FormulaConstruct, Int]()

      /**
        * Find all atoms appearing alone inside an OR operator. Remove them
        * from the encoding and use them as a prefix.
        *
        * Example:
        *   F = A v (B ^ C) v D v (E ^ F)
        *   Prefix = A v D
        *   Rest of the encoding = (B ^ D) v (E ^ F)
        *
        * @param source an input formula construct encoding
        * @return a prefix vector containing only OR operators and the rest of the encoding
        */
      def getPrefix(source: Vector[Int]): (Vector[Int], Vector[Int]) = {

        var prefix = Vector[Int]()
        var rest = Vector[Int]()
        var zeros = 0
        var ones = 0
        var atoms = 0

        for (symbol <- source) {

          if (symbol == 0) zeros += 1
          else if (symbol == 1) {
            rest :+= 1
            ones += 1
          } // all else if above indicate that symbol is obviously > 1 and therefore an atom
          else if (ones == 0) {
            zeros -= 1
            prefix :+= symbol
          } else if (ones > 0) {
            rest :+= symbol
            atoms += 1
          }

          // if atoms are 2 and ones are 1 then we found exactly one AND operator
          if (atoms == 2 && ones == 1) {
            atoms = 0
            ones = 0
          } // if atoms are exactly 2 but ones are > 1 then we found a AND operator but there are more following
          else if (atoms == 2) {
            atoms -= 1
            ones -= 1
          }
        }

        (Vector.fill(prefix.length - 1)(0) ++ prefix, Vector.fill[Int](zeros)(0) ++ rest)
      }

      /**
        * Encodes the given formula construct into a unique integer sequence. The sequence
        * can be decoded back to the given construct.
        *
        * Example:
        *  F = (A ^ B) v (C ^ D)
        *  Sequence = 0123145 where A = 2, B = 3, C = 4, D = 5, AND = 1, OR = 0
        *
        * @param source an input formula construct to encode
        * @return a vector of integers
        */
      def encode(source: FormulaConstruct): Vector[Int] = source match {

        case f @ (_: AtomicFormula | _: Not) =>
          if (atomToKey.contains(f))
            Vector(atomToKey(f))
          else {
            keyspace += 1
            keyToAtom += keyspace -> f
            atomToKey += f -> keyspace
            Vector(keyspace)
          }

        case Or(a, b)  => Vector(0) ++ encode(a) ++ encode(b)
        case And(a, b) => Vector(1) ++ encode(a) ++ encode(b)
        case _ =>
          throw new IllegalStateException("Failed to encode due to illegal formula type: " + source.toText)
      }

      /**
        * Decode a sequence of integers back into a formula construct.
        *
        * @param source a sequence of integers
        * @return a decoded formula construct
        */
      def decode(source: Vector[Int]): FormulaConstruct = {
        val buffer = scala.collection.mutable.Stack[FormulaConstruct]()

        for (symbol <- source.reverseIterator) {
          if (symbol > 1) buffer push keyToAtom(symbol)
          else if (symbol == 1) buffer push And(buffer.pop(), buffer.pop())
          else if (symbol == 0) buffer push Or(buffer.pop(), buffer.pop())
        }

        buffer.head
      }

      /**
        * Splits a given formula construct encoding into two sub-vectors representing
        * the left and right part of the top level binary operator (AND, OR).
        *
        * Example:
        *   F = (A ^ B) v (C ^ D)
        *   Encoding = 0123145
        *   Split = (123), (145)
        *
        * @param code an input formula construct encoding
        * @return left and right part encodings of the top level operator
        */
      def split(code: Vector[Int]): (Vector[Int], Vector[Int]) = {

        var splitIdx = -1
        var idx = 0
        var counter = 0
        var args = 0

        val list = code.iterator
        while (list.hasNext && idx < code.length) {
          val digit = list.next()
          if (splitIdx == -1) {

            if (digit < 2) counter += 1
            else if (digit > 2) args += 1

            if (counter - args == 0) splitIdx = idx
          }
          idx += 1
        }

        if (splitIdx != -1) {
          val pair = code.splitAt(splitIdx + 1)
          (pair._1.tail, pair._2)
        } else (Vector.empty, Vector.empty)
      }

      /**
        * Check if the fast distributive property can be
        * applied in the given encoding.
        *
        * @param encoding an input formula construct encoding
        * @return true if fast distribute can be
        *         applied, false otherwise
        */
      def canFastDistribute(encoding: Vector[Int]): Boolean = {
        var found = false
        var idx = 0

        while (idx < encoding.length) {
          if (encoding(idx) == 0 && found) {
            idx = encoding.length
            found = false
          } else if (encoding(idx) == 1)
            found = true
          idx += 1
        }

        found
      }

    // Encode the given formula construct
    val encodedFormula = encode(source)

    // Check if the fast distributive property can be performed
    if (canFastDistribute(encodedFormula)) {

      // Prefix is all atoms appearing only inside an OR operator
      val (prefix, rest) = getPrefix(encodedFormula)

      var resultedFormulas = Vector[Vector[Int]]()

      // Initially only the prefix exists
      resultedFormulas :+= prefix
      var remained = rest

      while (remained.nonEmpty) {

        var transformed = Vector[Vector[Int]]()

        // If OR still exists in the encoding, split and get left and right parts
        val (a, b) = if (remained.contains(0)) split(remained) else (remained, Vector.empty)

        val (atoms, next) = if (a.head == 1) (a.dropWhile(_ == 1), b) else (b.dropWhile(_ == 1), a)

        var idx = 0
        while (idx < resultedFormulas.length) {
          atoms.foreach { atom =>
            if (!resultedFormulas(idx).contains(atom)) {
              val res = Vector(0) ++ resultedFormulas(idx) ++ Vector(atom)
              if (!transformed.exists(f => f.sorted == res.sorted)) transformed +:= res
            } else if (!transformed.exists(f => f.sorted == resultedFormulas(idx).sorted))
              transformed :+= resultedFormulas(idx)
          }
          idx += 1
        }

        remained = next
        resultedFormulas = transformed
      }

      // Decode all resulted formula construct in order to pass them into the next step
      decode(Vector.fill[Int](resultedFormulas.length - 1)(1) ++ resultedFormulas.flatten)
    } else NormalForm.distribute(source)
  }
}
