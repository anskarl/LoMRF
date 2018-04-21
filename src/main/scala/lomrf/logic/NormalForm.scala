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
 * Logical Markov Random Fields LoMRF (LoMRF).
 */

package lomrf.logic



import com.typesafe.scalalogging.LazyLogging
import lomrf.mln.model.ConstantsSet
import lomrf.util.Cartesian.CartesianIterator

import annotation.tailrec
import collection.mutable

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
   *
   * @see Wikipedia article [[http://en.wikipedia.org/wiki/Clausal_normal_form]]
   * @see Standford course CS157: Computational Logic, topic: Resolution Preliminaries  (lecture 9) [[http://logic.stanford.edu/classes/cs157/2008/lectures/lecture09.pdf]]
   * @see Russell, S.J. and Norvig, P. and Canny, J.F. and Malik, J. and Edwards, D.D. Artificial Intelligence: A Modern Approach, chapter 9.5.1 Conjunctive normal form for first-order logic [[http://aima.cs.berkeley.edu/]]
   */
  def toCNF(source: Formula)(implicit constants: Map[String, ConstantsSet] = Map.empty): Set[Clause] ={

    def normaliseConstruct(f: FormulaConstruct) = {
      distribute(
        removeUniversalQuantifiers(
          removeExistentialQuantifiers(constants,
            standardizeVariables(negationsIn(removeImplications(f))))
        )
      )
    }

    val normalized = source match {
      case wf: WeightedFormula =>
        WeightedFormula(wf.weight, normaliseConstruct(wf.formula))

      case DefiniteClause(head, body) =>
        WeightedFormula.asHard(normaliseConstruct(Implies(body,head)))

      case WeightedDefiniteClause(weight, clause) =>
        WeightedFormula(weight, normaliseConstruct(Implies(clause.body, clause.head)))

      case c: FormulaConstruct =>
        WeightedFormula.asHard(normaliseConstruct(c))
    }

    extractClauses(normalized)

  }

  def compileCNF(formulas: Iterable[Formula])(implicit constants: Map[String, ConstantsSet] = Map.empty): Set[Clause] ={
    formulas.par.foldLeft(Set[Clause]())((clauses, formula) => clauses ++ toCNF(formula))
  }

  /**
   * Converts the given formula into Negation Normal Form (NNF). A logical formula is in NNF if negation
   * occurs only immediately above elementary propositions and (!,v,&#94;) are the only allowed boolean connectives.
   * <ul>
   * <li> '''I:''' Eliminate all implications and equivalences </li>
   * <li> '''N:''' Move inwards all negations </li>
   * </ul>
   */
  def toNNF[F <: Formula](source: F): Formula = {
    def normaliseConstruct(f: FormulaConstruct) = negationsIn(removeImplications(f))

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
   * Converts the given formula into Prenex Normal Form (PNF).
   *
   * <ul>
   * <li> '''I:''' Eliminate all implications and equivalences </li>
   * <li> '''N:''' Move inwards all negations </li>
   * <li> '''M:''' Move all quantifiers outwards</li>
   * </ul>
   */
  def toPNF[F <: Formula](source: F): Formula = {

    def normaliseConstruct(f: FormulaConstruct) = moveQuantifiersOutside(negationsIn(removeImplications(f)))

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

  private def moveQuantifiersOutside(source: FormulaConstruct): FormulaConstruct = {
    val quantifiers = mutable.Queue[Quantifier]()

    //@tailrec cannot be applied
    def collect(src: FormulaConstruct): FormulaConstruct = src match {
      case f: AtomicFormula => f
      //case f: WeightedFormula => WeightedFormula(f.weight, collect(f.formula))
      case q: Quantifier =>
        quantifiers.enqueue(q)
        collect(q.formula)
      case f: And => And(collect(f.left), collect(f.right))
      case f: Or => Or(collect(f.left), collect(f.right))
      case f: Not => Not(collect(f.arg))
      case _ => throw new IllegalStateException("Failed to move quantifiers outside, illegal formula: " + src.toText)
    }

    @tailrec
    def construct(insideFormula: FormulaConstruct): FormulaConstruct = {
      if (quantifiers.nonEmpty)
        quantifiers.dequeue() match {
          case f: ExistentialQuantifier => construct(ExistentialQuantifier(f.variable, insideFormula))
          case f: UniversalQuantifier => construct(UniversalQuantifier(f.variable, insideFormula))
        }
      else insideFormula
    }

    val inside = collect(source)

    construct(inside)
  }

  /**
   * Eliminates all implications (=>) and (<=>) from the given formula.
   */
  def removeImplications(source: FormulaConstruct): FormulaConstruct = {
    source match {
      case f: AtomicFormula => f
      //case f: WeightedFormula => WeightedFormula(f.weight, removeImplications(f.formula))
      case f: Not => Not(removeImplications(f.arg))
      case f: And => And(removeImplications(f.left), removeImplications(f.right))
      case f: Or => Or(removeImplications(f.left), removeImplications(f.right))
      case f: UniversalQuantifier => UniversalQuantifier(f.variable, removeImplications(f.formula))
      case f: ExistentialQuantifier => ExistentialQuantifier(f.variable, removeImplications(f.formula))

      case f: Equivalence => removeImplications(And(Or(Not(f.left), f.right), Or(f.left, Not(f.right))))
      case f: Implies => removeImplications(Or(Not(f.left), f.right))

      case _ => throw new IllegalStateException("Failed to remove implications, illegal formula: " + source.toText)
    }
  }

  /**
   * Moves all negations inwards, for example:
   * {{{!(P(x) ^ !Q(x))}}}
   * will result to:
   * {{{!P(x) v Q(x)}}}
   */
  def negationsIn(source: FormulaConstruct): FormulaConstruct = {
    //println("Internal step for: "+source)
    source match {
      case f: AtomicFormula => f
      //case WeightedFormula(weight, formula) => WeightedFormula(weight, negationsIn(formula))
      case Not(arg) => //Move negation inward
        arg match {
          case And(left, right) => negationsIn(Or(Not(left), Not(right))) //!(PvQ) -> !P ^ !Q
          case Or(left, right) => negationsIn(And(Not(left), Not(right))) // !(P^Q) -> !P v !Q
          case ExistentialQuantifier(vars, formula) => negationsIn(UniversalQuantifier(vars, Not(formula))) //!(Exist x Q(x))-> Forall x !Q(x)
          case UniversalQuantifier(vars, formula) => negationsIn(ExistentialQuantifier(vars, Not(formula))) //!(Forall x Q(x)) -> Exist x Q(x)
          case Not(formula) => negationsIn(formula) // !(!P(x)) -> P(x)
          case _ => source
        }
      case f: And => And(negationsIn(f.left), negationsIn(f.right))
      case f: Or => Or(negationsIn(f.left), negationsIn(f.right))
      case f: UniversalQuantifier => UniversalQuantifier(f.variable, negationsIn(f.formula))
      case f: ExistentialQuantifier => ExistentialQuantifier(f.variable, negationsIn(f.formula))
      case _ => throw new IllegalStateException("Failed to move negations inside, illegal formula: " + source.toText)
    }
  }

  /**
   * Standardizes variables to avoid mistakes when some variable names reappear in the formula, but inside different quantification scopes.
   * {{{ Forall x P(x) ^ Exists x R(x)  }}}
   * will be re-written as follows:
   * {{{ Forall x P(x) ^ Exists x0 R(x0)}}}
   *
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
        case None => produceNext(x)
      }

      vars(x) = newVar

      newVar
    }


    def stdVar(f: FormulaConstruct): FormulaConstruct = {
      f match {
        case x: AtomicFormula => x
        case x: And => And(stdVar(x.left), stdVar(x.right))
        case x: Or => Or(stdVar(x.left), stdVar(x.right))
        case x: Not => Not(stdVar(x.arg))
        case x: ExistentialQuantifier =>
          val newVar = nextVar(x.variable)
          //x.substitute(Map(x.variable -> newVar))
          // ExistentialQuantifier(newVar, stdVar(Substitute(Map(x.variable -> newVar), x.formula)))

          ExistentialQuantifier(newVar, stdVar(x.formula.substitute(Map(x.variable -> newVar))))
        case x: UniversalQuantifier =>
          val newVar = nextVar(x.variable)
          UniversalQuantifier(newVar, stdVar(x.formula.substitute(Map(x.variable -> newVar))))
        //case x: WeightedFormula => WeightedFormula(x.weight, stdVar(x.formula))
        case _ => throw new IllegalStateException("Failed to standardize variables, illegal formula: " + f.toText)
      }
    }

    stdVar(source)
  }


  /**
   * Existentially quantified variables in formula's predicates are replaced in with the disjunction of their groundings.
   * For example, consider, the following formula and that the domain of variable y is [0,...,3].
   *
   * {{{Exist y A(x,y) ^ B(y)}}}
   *
   * this formula will be replaced by:
   *
   * {{{ (A(x,0) ^ B(0)) v (A(x,1) ^ B(1)) v (A(x,2) ^ B(2)) v (A(x,3) ^ B(3)) }}}
   *
   * @param constants  domains of constants
   * @param source input formula
   *
   * @return the equivalent formula in which all existential quantifiers are eliminated.
   */
  def removeExistentialQuantifiers(constants: Map[String, ConstantsSet], source: FormulaConstruct): FormulaConstruct = {
    //TODO: re-implement this function into a non-recursive version, in order to avoid "out of memory" errors in large domains

    source match {
      case x: AtomicFormula => x
      //case WeightedFormula(w, f) => WeightedFormula(w, removeExistentialQuantifiers(constants, f))
      case And(left, right) => And(removeExistentialQuantifiers(constants, left), removeExistentialQuantifiers(constants, right))
      case Or(left, right) => Or(removeExistentialQuantifiers(constants, left), removeExistentialQuantifiers(constants, right))
      case Not(f) => Not(removeExistentialQuantifiers(constants, f))
      case UniversalQuantifier(v, f) => UniversalQuantifier(v, removeExistentialQuantifiers(constants, f))
      case e @ ExistentialQuantifier(v, f) =>
        //println("e="+e.toText)
        //Get variable domain
        val constantsSet = constants.get(v.domain) match {
          case Some(x) => x
          case _ => throw new IllegalStateException("Cannot find constant literals for variable: " + v)
        }
        //Produce disjunctions
        val sliceIdx = constantsSet.size - 2
        if (sliceIdx >= 0) {
          val iter = constantsSet.iterator
          val zeta = Or(
            f.substitute(Map(v -> Constant(iter.next()))),
            f.substitute(Map(v -> Constant(iter.next())))
          )
          val result = iter.foldRight(zeta)((c, other: Or) =>
            Or(other, f.substitute(Map(v -> Constant(c))))
          )
          removeExistentialQuantifiers(constants, result)
        } else {
          removeExistentialQuantifiers(constants, f.substitute(Map(v -> Constant(constantsSet.head))))
        }
      case _ =>
        throw new IllegalStateException("Failed to remove existential quantifiers, illegal formula:" + source.toText)
    }
  }

  /**
   * Removes all universal quantifiers from the specified formula
   */
  def removeUniversalQuantifiers(source: FormulaConstruct): FormulaConstruct = {
    source match {
      case x: AtomicFormula => x
      //case WeightedFormula(w, f) => WeightedFormula(w, removeUniversalQuantifiers(f))
      case And(left, right) => And(removeUniversalQuantifiers(left), removeUniversalQuantifiers(right))
      case Or(left, right) => Or(removeUniversalQuantifiers(left), removeUniversalQuantifiers(right))
      case Not(f) => Not(removeUniversalQuantifiers(f))
      case UniversalQuantifier(v, f) => removeUniversalQuantifiers(f)
      case _ => throw new IllegalStateException("Failed to remove universal quantifiers, illegal formula:" + source.toText)
    }
  }

  /**
   * Distribute all disjunctions inside all conjunctions in order to create the final clausal form.
   */
  def distribute(source: FormulaConstruct): FormulaConstruct = {
    var isDone = true

    def dist(source: FormulaConstruct): FormulaConstruct = {
      source match {
        //case w: WeightedFormula => WeightedFormula(w.weight, dist(w.formula))
        case f: Not if f.arg.isInstanceOf[AtomicFormula] => f
        case f: AtomicFormula => f
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

        case Or(left, right) => Or(dist(left), dist(right))

        case And(left, right) => And(dist(left), dist(right))

        case _ => throw new IllegalStateException("Failed to distribute disjunctions inside conjunctions, illegal formula type: " + source.toText)
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
   * Extracts clauses from the given formula (produced by [[lomrf.logic.NormalForm.distribute]]).
   *
   * <p>
   * In an effort to preserve the original formula as much as possible, all single literals
   * in a conjunction are kept together by negating the formula: the weight is negated and the
   * formula becomes a disjunction of the negated literals. For instance, the formula:
   * </p>
   * {{{2 P(x) ^ Q(x) ^ (R(x) v S(x)) }}}
   *
   * will be extracted into the following clauses:
   * {{{
   * -1 !P(x) v !Q(x)
   *
   * 1 R(x) v S(x)
   * }}}
   *
   * @param formula the input formula as it is resulted from  [[lomrf.logic.NormalForm.distribute]]
   *
   * @return the extracted clauses
   */
  @tailrec
  def extractClauses(formula: Formula): Set[Clause] = {

    def extractLiterals(source: Formula): (Set[Literal], Set[Set[Literal]]) = {

      var units = Set[Literal]()
      var nonUnits = Set[Set[Literal]]()

      //@tailrec cannot be applied
      def collectLiterals(source: Formula) {
        source match {
          case f: WeightedFormula => collectLiterals(f.formula)
          case f: AtomicFormula => units += PositiveLiteral(f)
          case Not(f: AtomicFormula) => units += NegativeLiteral(f)
          case And(left, right) =>
            collectLiterals(left)
            collectLiterals(right)
          case f: Or =>

            var literals = Set[Literal]()
            var isTautology = false

            //@tailrec cannot be applied
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
            } //end of "walkInsideDisjunction"

            walkInsideDisjunction(f)

            if (literals.size == 1) {
              units += literals.head
            } else if (literals.size > 1) {
              if (!isTautology) nonUnits += literals
              else logger.warn("tautology clause is produced")
            } // end of "Or"
          case _ => throw new IllegalStateException("Failed to collect literals, illegal formula: " + source.toText)
        }
      }

      collectLiterals(formula)

      (units, nonUnits)
    }

    //Creates a set of weighted clauses from a set of clauses
    def createWeightedClauses(weightVal: Double, cs: Set[Set[Literal]]) =
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
        // * one unit clause and possibly some other non-unit clauses
        if (unit.size == 1) createWeightedClauses(clauseWeight, nonUnit) + Clause(unit, clauseWeight)
        else if (unit.size > 1) {
          // * when more than one unit clauses are produced merge them into a single clause.
          val mergedUnitsClauses = Clause(unit.map(_.negate), -clauseWeight)
          Set(mergedUnitsClauses) ++ createWeightedClauses(clauseWeight, nonUnit)
        }
        else createWeightedClauses(clauseWeight, nonUnit) //no unit clauses.

      case _ => throw new IllegalStateException("Failed to extract clauses, illegal formula: " + formula.toText)
    }
  }
}
