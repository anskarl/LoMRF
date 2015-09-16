# Knowledge Base

Knowledge base files in LoMRF are text files having the suffix `.mln` (e.g., `file_name.mln`). The contents of a knowledge base file in LoMRF can be composed of the following:

  1. [Optional] [Domain types](#domain-types) with their possible values.

  2. [Predicate definitions](#predicate-definitions), representing the structure of each predicate.

  3. [Optional] [Function definitions](#function-definitions), representing the structure of a function.

  4. [Optional] [Formulas](#first---order-logic-formulas) represented in [First-order logic](https://en.wikipedia.org/wiki/First-order_logic), expressing the template for producing Markov Networks.

  5. [Optional] [Definite clauses](#definite-clauses), a special case of formulas.


## Domain Types
Domain types with their possible constant symbols. Each domain type has some unique name and represents a
particular collection of constant symbols. All constant symbols in LoMRF are finite and discrete.

Consider, for example, the domain of *time* ranging from 1 to 12. This domain is defined in LoMRF with the following statement:

```lang-none
time = {1,...,12}
```
For domains containing integer symbols, like the example domain of time, we can define a sequence of integer numbers using the `...` symbol.  

Similarly, a domain that represents a collection of *person* names is defined as:

```lang-none
person = {Achilles, Agamemnon, Menelaus, Helen, Odysseus}
```

The possible values of domain types can automatically discovered from the specified knowledge base and evidence file(s). Specifically, for each domain type, LoMRF computes the union of unique constant symbols from the knowledge base and the evidence file(s). For example, we may not explicitly specify all person names, but can be automatically discovered from the evidence files and/or the contents of the given knowledge base.

## Predicate Definitions

Predicate definitions, express the structure of each predicate. For example, the predicate that is named with the symbol `Brothers` defines that two persons are brothers. Therefore, the definition of predicate `Brothers` has two arguments, where each one takes constants from the domain of person:

```lang-none
Brothers(person, person)
```

Similarly, the predicate `Happens` associates the domain of *action* with *time* is defined as:

```lang-none
Happens(action, event)
```

Therefore, all predicates have some name (e.g., `Brothers`, `Happens`, etc) and some specific number of arguments. Each argument has some domain type (e.g., `person`, `action`, `time`, etc).

## Function Definitions

Function definitions, express the structure of a function. All functions in LoMRF are finite with known domain types. Each function definition has some name, zero or more domain types as arguments and a resulting domain type.

For example, the function that is named as `monthOf` with a single argument that takes constants from the domain of time and returns constants from the domain of *month* (i.e., the month number):

```lang-none
month monthOf(time)
```

Similarly a function `next` that takes a single argument from the domain of time and returns the next one is defined as follows:

```lang-none
time next(time)
```

## Build-in functions and predicates

Build-in functions and predicates are supported internally in the LoMRF and do
not require any schema definition in the knowledge base file.

### Functions

| Function                  | Description                                |
|:-------------------------:|:-------------------------------------------|
| x++                       | Increase the integer number *x* by one     |
| x--                       | Degrease the integer number *x* by one     |
| x + y                     | Sum *x* with *y*                           |
| x - y                     | Subtract *y* from *x*                      |
| x * y                     | Multiply *x* with *y*                      |
| x / y                     | Divide *y* from *x*                        |
| x % y                     | The remainder of *x* divided by *y*        |
| concat(x, y)              | Concatenate *x* with *y*                   |



### Predicates

| Predicate                  | Description                                 |
|:--------------------------:|:--------------------------------------------|
|  a = b                     | Term *a* is equal with term *b*             |
|  a < b                     | Term *a* is less than term *b*              |
|  a <= b                    | Term *a* is less than or equal with *b*     |
|  a > b                     | Term *a* is greater than term *b*           |
|  a >= b                    | Term *a* is greater than or equal with  *b* |
| substr(a, b)               | Term *a* is substring of *b*                |



## First-Order Logic Formulas

**Terms:**

Terms are intuitively represent objects and can be any of the following:
  * Constants, are starting with upper-case letter or numeric symbols, e.g., Achilles, Agamemnon, 1, 2, etc.
  * Variables, are starting with lower-case letter symbols, expressing any constant of a specific domain type, e.g., x, y, z, t, id, name, etc.
  * Functions, are starting with lower-case letter symbols, and may contain zero or more terms as arguments, e.g., motherOf(X), fatherOf(Agamemnon), etc.

**Predicates:**

Similar to any first-order logic representation, a predicate expresses a relation among its terms that can be either True or False.
In LoMRF, predicates start with an upper-case letter. For example, the statement that *Agamemnon* and *Menelaus* are ***brothers***, is represented by the predicate `Brothers(Agamemnon, Menelaus)`. Predicates may have constants, variables and function symbols in their arguments. For example, `Brothers(Agamemnon, x)` is composed of the constant `Agamemnon` and the variable `x`.

Please note that when a predicate does not contain any variable is called ***ground***. For example, the predicates `Brothers(Agamemnon, Menelaus)` and `Brothers(Atlas, fatherOf(Patroclus))` are ground, while the predicates `Brothers(x, y)`, `Brothers(Agamemnon, y)` or `Brothers(Atlas, fatherOf(y))` are not.

**Formulas:**

Formulas are represented in [First-order logic](https://en.wikipedia.org/wiki/First-order_logic), expressing a template for producing Markov Networks.
  * Each formula imposes a constraint over some predicates.
  * Each formula can be associated with some *weight* value, that is a positive or negative real number.
  The higher the value of weight, the stronger the constraint represented by the formula.
  In contrast to classical logic, all *worlds* (i.e., [Herbrand Interpretations](https://en.wikipedia.org/wiki/Herbrand_interpretation))
  are possible with a certain probability. The main idea behind this is that the probability of a world increases as the number of formulas
  it violates decreases.

Formulas may contain one or more predicates, connected to each other with logical connectives and quantified symbols.
The logical connectives and quantifiers in LoMRF are outlined in the following table:

| Symbol | Description                                |
|:------:|:-------------------------------------------|
|  ^     | Logical conjunction (And)                  |
|  v     | Logical disjunction (Or)                   |
|  !     | Logical negation (Not)                     |
|  =>    | Logical implication                        |
| <=>    | Logical equivalence (if and only if)       |
| Forall | Universal quantification                   |
| Exist  | Existential quantification                 |

*By default all are variables implicitly assumed to be universally quantified unless otherwise indicated.*

A knowledge base may contain both *hard and soft-constrained* formulas. Hard-constrained formulas are associated
with an infinite weight value and capture the knowledge which is assumed to be certain. Therefore, an acceptable
world must at least satisfy the hard constraints. Soft constraints, on the other hand, capture imperfect knowledge
in the domain, allowing for the existence of worlds in which this knowledge is violated.
  * Hard-constrained formulas, do not have weights (the weight is assumed to be infinite) and capture the knowledge which is assumed to be certain.
  * Soft-constrained formulas are always associated with weights and capture imperfect knowledge, allowing for the existence of worlds in which this knowledge is violated.

## Definite Clauses

A special case of formulas are the [Definite clauses](https://en.wikipedia.org/w/index.php?title=Definite_clause),
which can be used to define declarations of rules. The definite clauses, are processed by the LoMRF and automatically
translated to equivalent formulas. Their syntax is simpler from the syntax of formulas and are ideal for defining
domain-specific/common-sense knowledge.

A definite clause can be either hard-constrained or soft-constrained and is composed of two parts:
  1. The ***head*** part is a single positive literal.
  2. The ***body*** part is single literal or a conjunction of two or more literals.
  The literals can be either positive or negative (i.e., negated predicate). In contrast to first-order
  formulas, disjunctions, quantifiers, implications and equivalences are not allowed.

Example of soft-constrained definite clause in LoMRF, with a single positive literal in the body:
```lang-none
3.2 HeadPredicate(x, y) :- BodyPredicate(x, y)
```

Example of hard-constrained definite clause in LoMRF, in which the body is composed by
one positive (i.e., `BodyPredicate1(x)`) and one negative (i.e., `!BodyPredicate2(y)`) literals:
```lang-none
HeadPredicate(x, y) :- BodyPredicate1(x) ^ !BodyPredicate2(y).
```

It is possible to have more than one definite clauses for the same head predicate,
and thus define alternative definitions (i.e., disjunctions) for the same head statement.
```lang-none
2 Head(f(x), t) :- FooPredicate(x, t) ^ !BarPredicate(z(t))
-1.68 Head(f(x), t) :- AnotherPredicate(x, t)
```

LoMRF uses definite clauses to create formulas that express *if-and-only-if* conditions to head predicates.
In that way LoMRF implicitly introduces closed-world assumption to head predicates.
To better illustrate this, assume that we have the following definite clauses:

```lang-none
Head(f(x), t) :- FooPredicate(x, t) ^ !BarPredicate(z(t)).
-1.68 Head(f(x), t) :- AnotherPredicate(x, t)
```
LoMRF will translate the given clauses into the following formulas:

```lang-none
//
// 1. Convert definite clauses to: (weight) body => head
//
FooPredicate(x, t) ^ !BarPredicate(z(t)) => Head(f(x), t).
-1.68 AnotherPredicate(x, t) => Head(f(x), t)

//
// 2. Introduce closed-world assumption via predicate completion
//    for the head predicate Head(f(x), t), as hard-constraint
//
Head(f(x), t) => (FooPredicate(x, t) ^ !BarPredicate(z(t))) v AnotherPredicate(x, t).
```
As we can see from the resulting translation, LoMRF produces *2 + 1* formulas for the definitions of `Head(f(x), t)`.
The first two formulas are straightforward translations of the definite clauses to `(weight) body => head` formulas.
Specifically, if the definite clause is hard-constrainted, the corresponding translation will also remain hard-constrainted.
Otherwise, the resulting translation will keep the same weight value. The last resulting hard-constrained formula
introduces the opposite direction, that is `head => disjunction of all body parts`. That formula is always
hard-constraint and states that in order to have *head* satisfied, at least one of the body parts must also be satisfied.
Any other possibility doesn't affect the state of the head predicate. With that translation LoMRF implicitly introduces
closed-world assumption to head predicates, using a technique that is  called predicate completion (see [McCarthy, 1980; Lifschitz, 1994](#references)).

LoMRF tries to simplify the resulting knowledge base, by specialising as much as possible the predicate completion for each distinct head predicate.
Specifically, the original predicate completion will result to a more general form, producing a single formula with an equivalence:

```lang-none
Head(a, t) <=> (a = f(x) ^ FooPredicate(x, t) ^ !BarPredicate(z(t))) v (a = f(x) ^ AnotherPredicate(x, t))
```
The problem with that translation is that we cannot keep the weight values from the original definite clauses and that increases the
number variables (i.e., the additional variable `a`).

> In contrast to the original predicate completion approach, LoMRF keeps expresses the completion into a decomposed form, that is one
formula per definite clause and one single formula with the disjunctions of body parts. The implementation is based on the
transformations presented in [Skarlatidis et. al. (2011, 2015)](#references).

> Technically, the definite clauses are translated into logically stronger first-order formulas.
In particular, LoMRF performs predicate completion for each *unique* head predicate in the knowledge base.
In cases where some definitions of head predicates are missing, the corresponding missing definition are implicitly
considered as *False*. For further details see [Skarlatidis et. al. (2011, 2015)](#references), as well as
the [Advanced Cases](#advanced-cases) section.

### Advanced Cases ###

***Variables that appear only in the body part***

In situations where there are variables in the body part that do not appear in the head, then existential quantification is introduced.
For example, in the following clause the variable `z` appears only in the body part:
```lang-none
1.86 Head(x,y) :- Foo(x, z) ^ Bar(z, y)
```
In such case, the translated formulas will be the following:

```lang-none
1.86 Exist z Foo(x, z) ^ Bar(z, y) => Head(x, y)

Head(x,y) => Exist z Foo(x, z) ^ Bar(z, y).
```
*Please note that during grounding existentially quantified formulas are replaced by the disjunction of their groundings
(see [Domingos and Lowd, 2009](#references)). This may lead to a large number of disjunctions and a combinatorial explosion of the number
of clauses, producing unmanageably large Markov networks.* ***This type of clauses should be avoided, if it is possible.***

***Partial definitions***

In some cases we may not have definitions for each unique head predicate. Consider, for example, the following knowledge base:

```lang-none
time = {1,...,12}
values = {Foo, Bar}

Head(values, time)
P(time)
Q(time)

Head(Foo, t) :- P(t) ^ Q(t).
```
In the given knowledge base, the domain type `values` is composed of two constants, that is `Foo` and `Bar`.
Furthermore, the knowledge base contains a single definite clause for the head predicate `Head(Foo, t)`.
LoMRF will compute the decomposed predicate completion for the predicates having `Head/2` as signature.
However, the definition for the head predicate `Head(Foo, t)` is missing and thus LoMRF will implicitly
define it as false - i.e., `Head(Foo, t) <=> False`. As a result, the final form of the knowledge base is given below:
```lang-none
BodyPredicate1(t) ^ BodyPredicate2(t) => Head(Foo, t).

Head(Foo, t) => BodyPredicate1(t) ^ BodyPredicate2(t).

!Head(Bar, t).
```
As we can see from the resulting knowledge base `Head(Foo, t) <=> False` is equivalently expressed
as single negated unit clause `!Head(Bar, t).`.


***Limitations of the decomposed form***

In some cases we may have a knowledge base with definite clauses of a particular head predicate,
but the heads are not contain the same level of variables. For example, we may have the following
two definite clauses:
```lang-none
Head(Foo, t) :- P(t) ^ Q(t).
Head(x, t) :- R(x) ^ Q(t).
```
The first term of the head predicate of first clause is a constant (*Foo*), while the in the second one the
corresponding term is a variable (*x*). In that case the second clause contains a higher level of variables
in the head predicate (`Head(x, t)`).

In such situations, LoMRF computes the predicate completion with respect to the head predicate with the higher level of variables.
Therefore, the first clause is handled as:
```lang-none
Head(x, t) :- x = Foo ^ P(t) ^ Q(t).
```
and the resulting knowledge base is the following:

```lang-none
x = Foo ^ P(t) ^ Q(t) => Head(x, t).
R(x) ^ Q(t) => Head(x, t).

Head(x, t) => (x = Foo ^ P(t) ^ Q(t)) v (R(x) ^ Q(t)).
```

## References

Domingos, P., and Lowd, D. (2009). Markov Logic: An Interface Layer for Artificial Intelligence. Synthesis Lectures on Artiﬁcial Intelligence and Machine Learning. Morgan & Claypool Publishers. ([link](http://www.morganclaypool.com/doi/abs/10.2200/S00206ED1V01Y200907AIM007))

Lifschitz, V. (1994). Circumscription. In Handbook of logic in Artificial Intelligence and Logic Programming, Vol. 3, pp. 297-352. ([link](http://www.jstor.org/stable/420980))

McCarthy, J. (1980). Circumscription - A Form of Non-Monotonic Reasoning. Artificial Intelligence, 13 (1-2), 27-39. ([link](http://www-formal.stanford.edu/jmc/circumscription.ps))

Skarlatidis A., Paliouras G., Vouros G. and Artikis. (2011) A. Probabilistic Event Calculus based on Markov Logic Networks.
Proceedings of International Symposium on Rules (RuleML@BRF), Springer. ([link](http://link.springer.com/chapter/10.1007%2F978-3-642-24908-2_19))

Skarlatidis A., Paliouras G., Artikis A. and Vouros G. (2015). Probabilistic Event Calculus for Event Recognition. ACM
Transactions on Computational Logic, 16, 2, Article 11, pp. 11:1-11:37. ([link](http://dx.doi.org/10.1145/2699916))
