# Quick Start

Assuming that you have successfully build a LoMRF distribution and added LoMRF executables in your default `PATH` (see
[Building and Linking](6_building_and_linking.md)). In the following paragraphs we will present our first LoMRF model and
perform probabilistic inference.

## Running Example

For our running example we use the [Yale Shooting Problem](https://en.wikipedia.org/wiki/Yale_shooting_problem),
a well-known AI test case of non-monotonic temporal reasoning which was proposed by Steve Hanks and Drew McDermott[^1].
In brief, a hunter tries to kill a prey with his gun.  Only when the hunter shoots with a loaded gun the prey is being
killed.

In the example scenario there is a sequence of *actions* (e.g., loading the gun) and *situations* (e.g., the prey is
alive). The series of actions and situations occur in linear time. Furthermore, at each instant of time some actions may
happen (e.g., the hunter loads the gun), causing related situations to hold or not (e.g., the gun is loaded).

To demonstrate the core features of  LoMRF, we model the Yale Shooting scenario by using the
[Event Calculus](https://en.wikipedia.org/wiki/Event_calculus) logical action formalism. In the Event Calculus we
represent *actions* with *events* and *situations* with *fluents*. More formally, a fluent is a property whose value may
change over time. When an event occurs it may change the value of a fluent. The underlying time  model is linear and we
represent time-points as integer numbers. The core domain-independent axioms of the Event Calculus define whether a
fluent holds or not at a specific time-point. Moreover, the axioms incorporate the common sense *law of inertia*,
according to which fluents persist over time, unless they are affected by the occurrence of some event. For example,
the gun remains loaded (fluent) until the hunter shoots (event). Below we outline the events and the fluents that we
use in the example:

| Event | Description                |
|:------|:---------------------------|
| Shoot | Hunter shoots              |
| Load  | Hunter loads the gun       |

| Fluent | Description               |
|:-------|:--------------------------|
| Loaded | The gun is loaded         |
| Alive  | The prey is alive         |
| Dead   | The prey is dead          |


The scenario that we follow in this example is outlined by the following narrative:

  1. At the beginning (i.e., initial state), the prey is alive and the hunter's gun is empty.
  2. The hunter is shooting with an empty gun. We expecting that nothing changes and therefore the prey remains alive.
  3. The hunter loads the gun, waiting for a moment, and then he shoots at the prey. As a result, the prey becomes dead.
  4. From that point in time and after, the prey should remain dead, no matter what is happening (e.g., reloading the gun
  and shooting again).

## Writing your first knowledge base

We have described our running example, we now begin by formulating our knowledge base.
The knowledge base file has the suffix `.mln` and in this example is named as `theory.mln`.

The contents of the `theory.mln` file is can be the following:

  1. Domain types and (optionally) their possible values, e.g., the domain of *time* ranging from 0 to 10.
  2. Predicate definitions, representing the structure of each predicate. For example, the predicate that is named as `Foo`
  with a single argument that takes constants from the domain of time, i.e., `Foo(time)`.
  3. (optional) Function definitions, representing the structure of a function. For example, the function that is named
  as `bar` with a single argument that takes constants from the domain of time and returns constants from the domain of
  *month* (i.e., representing the month number), i.e., `month = bar(time)`.
  4. Formulas represented in First-Order logic, expressing the template for producing Markov Networks.
     * The syntax of logical formulas can be found [here](1_syntax.md)
     * Each formula imposes a constraint.
     * Each formula can be associated with some *weight* value, that is a positive or negative real number. The higher
    the value of weight, the stronger the constraint represented by the formula.
     * Hard-constrained formulas, do not have weights (the weight is assumed to be infinite) and capture the knowledge which
    is assumed to be certain.
     * Soft-constrained formulas are always associated with weights and capture imperfect knowledge, allowing for the
    existence of worlds in which this knowledge is violated.
  5. A special case of formulas are the definite clauses, which can be used to define declarations of rules. The definite
  clauses, are processed by the LoMRF and automatically translated to equivalent formulas. Their syntax is simpler from the syntax of formulas and are ideal for defining domain-specific knowledge.

Please note that variables, domain types and *functions* are starting with a lower-case letter. All variables are assumed to be universally quantified unless otherwise indicated. *Predicates* and *constants* start with an upper-case letter.

### Domain types

That part is optional, since the LoMRF can collect all possible constant symbols for each domain type that appears in
our formulas, as well as in the specified evidence. Furthermore, if the have constant symbols in both evidence and our
theory (domain types, as well as in formulas), the LoMRF will automatically compute the union of all constant symbols.
On the other hand, in situations where the evidence might not contain all possible constant symbols, then it is advised
to define that subset of symbols in the theory.  

The domain types that we are using in our example belong to the domains of *time*, *event* and *fluent*. The values where
each one domain takes are constant symbols and are finite. Their possible finite symbols can be explicitly defined in our
`theory.mln` file. For example, the domain of fluents is represented below:

```lang-none
fluent = {Loaded, Alive, Dead}
```
The name of the domain is `fluent` and is starting with a lower-case letter. In the brackets we add the possible values
that the domain `fluent` can take. Each value is a constant symbol, therefore is starting with an upper-case letter.

For integer-valued domains like the domain of time, we can use the following notation:

```lang-none
time = {1,...,100}
```

The resulting domain of time will contain the range of 1 to 100 as constant symbols.

### Predicate definitions

We have to define the schema of each predicate that we will use in our example. Each predicate has some symbol name which
is followed by a parenthesis containing its arguments. Furthermore, the symbol with the number of its arguments (called
arity) defines the unique atomic signature of the predicate. For example, the schema of the predicate with symbol 'Foo'
and the two arguments 'event' and 'time' has atomic signature `Foo/2` (i.e., symbol/arity) and is defined as follows:

```lang-none
Foo(event, time)
```

Since we are employing the Event Calculus formalism, we use the Event Calculus predicates (outlined in the table below).

| Predicate                  | Meaning                                                                |
|:---------------------------|:-----------------------------------------------------------------------|
| Happens(event, time)       | An *event* occurs at some point in *time*.                             |
| HoldsAt(fluent, time)      | A *fluent* holds at some point in *time*.                              |
| InitiatedAt(fluent, time)  | A *fluent* is initiated at some point in *time*, i.e., begins to hold. |
| TerminatedAt(fluent, time) | A *fluent* is terminated at some point in *time*, i.e., stops to hold. |

The schema of our predicates are given below:

```lang-none
HoldsAt(fluent, time)
InitiatedAt(fluent, time)
TerminatedAt(fluent, time)
Happens(event, time)
```

All predicates names are starting with upper-case letters, while the domain types of their arguments are expressed with
lower-case letters.

### Domain-independent axioms (expressed in First-Order Logic)

First of all we have to express the Event Calculus core axioms that are domain-independent. That axioms does not express knowledge specific for our running example, but they express the conditions under which a fluent value changes or persists.

```lang-none
InitiatedAt(f, t) => HoldsAt(f, t++).

TerminatedAt(f, t) => !HoldsAt(f, t++).

HoldsAt(f, t) ^ !TerminatedAt(f, t) => HoldsAt(f, t++).

!HoldsAt(f, t) ^ !InitiatedAt(f, t) => !HoldsAt(f, t++).
```

Variables are expressed with lower-case symbols (i.e., `f` and `t`) and are all implicitly universally quantified.
The logical symbols `!`, `^`, `=>` and `<=>` express logical negation, conjunction, implication and equivalence,
respectively. The `++` is a special build-in postfix function that increases the current value of `t` by one, thus
resulting to the next time-point.

For example the first formula expresses that when a fluent, indicated by the variable `f`, is initiated at some time-point
`t`, will hold at the next time-point `t++`. The opposite is defined in the second formula, i.e., when the fluent `f` is
terminated at `t`, it will not hold at the next time-point `t++`. The third formula expresses that the fluent `f` continues
to hold at the next time-point when it is not terminated at the current time-point `t`. Similarly, the last formula
expresses that the fluent continues not to hold at the next time-point when the it is not initiated at the current
time-point.

All four formulas are hard-constrained, thus they do not have any weight value, instead they have a dot at the end. The reason that
we use hard-constrained formulas to express the core Event Calculus axioms, is that we want to hold with absolute
certainty. Further details about the probabilistic Event Calculus formalism of our example can be found in [^2].

### Domain-dependent axioms (expressed as definite clauses)

Having formalised the Event Calculus axioms in LoMRF, we continue with the domain-dependent axioms that describe the
knowledge for our running example. For simplicity we will use the definite clause notation in this part. All definite
clauses are composed of a *head* predicate and a declaration of *body* predicates (might be negated) that are connected
with conjunctions (logical and). The basic definite clause syntax is given below, where the head predicate is separated
by the predicates in the body with a the declaration symbol `:-`:

```lang-none
head :- body
```

In LoMRF, clauses can be either soft or hard-constrained. Similarly with the first-order formula syntax, hard-constrained clauses end with a dot symbol, while soft-constrained are prepended with a weight value.


We would like to express the rule that the gun becomes loaded if and only if the hunter loads the gun. This rule is
expressed with the following formula:

```lang-none
InitiatedAt(Loaded, t) :- Happens(Load, t)
```

In order to express a soft-constrained formula, we have to associated it with a weight value. Lets say that by using a
weight learning algorithm (see [Weight Learning](doc/3_weight_learning.md)), the weight of the first rule is estimated
to be equal with 2.0.

```lang-none
2 InitiatedAt(Loaded, t) :- Happens(Load, t)
```

By following the same formulation the rest of domain-dependent axioms is given below. For simplicity we are using
the same weight value for all soft-constrained definite clauses.

```lang-none
// The gun becomes loaded if and only if the hunter loads the gun.
2 InitiatedAt(Loaded,t) :- Happens(Load,t)

// The gun stops from being loaded after a shot.
2 TerminatedAt(Loaded,t) :- Happens(Shoot,t) ^ HoldsAt(Loaded,t)

// When the gun shoots and it is loaded, then the prey is being killed.
2 InitiatedAt(Dead,t) :- Happens(Shoot,t) ^ HoldsAt(Loaded,t)

// When the gun shoots and it is loaded, then the prey stops from being alive.
2 TerminatedAt(Alive,t) :- Happens(Shoot,t) ^ HoldsAt(Loaded,t)
```

Finally we can express the initial state (i.e., at time-point 0) of our running example with the following formulas:

```lang-none
// Initially the prey is alive
HoldsAt(Alive, 0).

// Initially the prey is not dead
!HoldsAt(Dead, 0).

// Initially the gun is not loaded
!HoldsAt(Loaded, 0).

// Initially nothing is initiated
!InitiatedAt(f,0).

// Initially nothing is terminated
!TerminatedAt(f,0).
```

### Resulting MLN file

The final form of our knowledge base (`theory.mln`) is given below:

```lang-none
fluent = {Loaded, Alive, Dead}
time = {0,...,13}

// Predicate schema
HoldsAt(fluent, time)
InitiatedAt(fluent, time)
TerminatedAt(fluent, time)
Happens(event, time)

/**
 * Event Calculus domain-independent axioms
 */

InitiatedAt(f, t) => HoldsAt(f, t++).

TerminatedAt(f, t) => !HoldsAt(f, t++).

HoldsAt(f, t) ^ !TerminatedAt(f, t) => HoldsAt(f, t++).

!HoldsAt(f, t) ^ !InitiatedAt(f, t) => !HoldsAt(f, t++).

/**
 * Domain-dependent rules
 */

// The gun becomes loaded if and only if the hunter loads the gun.
2 InitiatedAt(Loaded,t) :- Happens(Load,t)

// The gun stops from being loaded after a shot.
2 TerminatedAt(Loaded,t) :- Happens(Shoot,t) ^ HoldsAt(Loaded,t)

// When the gun shoots and it is loaded, then the prey is being killed.
2 InitiatedAt(Dead,t) :- Happens(Shoot,t) ^ HoldsAt(Loaded,t)

// When the gun shoots and it is loaded, then the prey stops from being alive.
2 TerminatedAt(Alive,t) :- Happens(Shoot,t) ^ HoldsAt(Loaded,t)

/**
 * Initial state
 */

// Initially the prey is alive
HoldsAt(Alive,0).

// Initially the prey is not dead
!HoldsAt(Dead,0).

// Initially the gun is not loaded
!HoldsAt(Loaded, 0).

// Initially nothing is initiated
!InitiatedAt(f,0).

// Initially nothing is terminated
!TerminatedAt(f,0).
```

## Writing your first evidence

We will now define the input evidence, in order to represent our example scenario. The evidence is given in a separate
file (`evidence.db`) and contains ground predicates (i.e., predicates that contain only constants in their arguments).

Specifically, we will represent the following scenario:

1. At time-point 2, the hunter shoots with empty gun.
2. At time-point 3, the hunter loads the gun.
3. At time-point 5, the hunter shoots with loaded gun.
4. At time-point 9, the hunter reloads the gun.
5. Finally at time-point 11, the hunter shoots again.

This scenario is represented by the following ground predicates:

```lang-none
Happens(Shoot, 2)
Happens(Load, 3)
Happens(Shoot, 5)
Happens(Shoot, 8)
Happens(Load, 9)
Happens(Shoot, 11)
```

## Perform inference

In order to perform inference, we have to define the following:
  * The type of the inference. In LoMRF two inference can be performed:
    1. Marginal inference: for computing the probabilities for all possible instantiations of query predicates of being true, given the evidence.
    2. Maximum a-posteriori inference (MAP): for computing the truth values (0 = False and 1 = True) of all possible instantiations that together represent the most probable state.
  * Input theory file, e.g., `theory.mln`.
  * Input evidence, e.g., `evidence.db`.
  * Output result file, e.g., `output.result`.
  * Which atomic signatures define the query predicates ('-q' option), for our example: `HoldsAt/2`. For all query
  predicates LoMRF takes [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption).
  * (Optionally) Which atomic signatures define predicates with [Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption) ('-cwa' option).
  By default, all evidence predicates have Closed-world assumption. For our example, `Happens/2` is evidence predicate,
  thus Closed-world assumption is taken for all its possible instantiations.
  * (Optionally) Which atomic signatures define predicates with [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) ('-owa' option).
  By default, all non-evidence predicates have Open-world assumption. For our example, `InitiatedAt/2` and `TerminatedAt/2` are non-evidence predicates,
  thus Open-world assumption is taken for all their possible instantiations.

### Marginal inference

Marginal inference computes the conditional probability of query predicates (e.g., HoldsAt), given the evidence (e.g., Happens).

```lang-none
lomrf -infer marginal -i theory.mln -e evidence.db -r marginal-out.result -q HoldsAt/2 -owa InitiatedAt/2,TerminatedAt/2 -cwa Happens/2
```

The following results are stored in the `marginal-out.result` file:

```lang-none
HoldsAt(Alive,0) 1.0
HoldsAt(Alive,1) 1.0
HoldsAt(Alive,2) 1.0
HoldsAt(Alive,3) 1.0
HoldsAt(Alive,4) 1.0
HoldsAt(Alive,5) 1.0
HoldsAt(Alive,6) 0.197
HoldsAt(Alive,7) 0.197
HoldsAt(Alive,8) 0.197
HoldsAt(Alive,9) 0.185
HoldsAt(Alive,10) 0.185
HoldsAt(Alive,11) 0.185
HoldsAt(Alive,12) 0.02
HoldsAt(Alive,13) 0.02
HoldsAt(Alive,14) 0.02
HoldsAt(Dead,0) 0.0
HoldsAt(Dead,1) 0.0
HoldsAt(Dead,2) 0.0
HoldsAt(Dead,3) 0.0
HoldsAt(Dead,4) 0.0
HoldsAt(Dead,5) 0.0
HoldsAt(Dead,6) 0.82
HoldsAt(Dead,7) 0.82
HoldsAt(Dead,8) 0.82
HoldsAt(Dead,9) 0.835
HoldsAt(Dead,10) 0.835
HoldsAt(Dead,11) 0.835
HoldsAt(Dead,12) 0.963
HoldsAt(Dead,13) 0.963
HoldsAt(Dead,14) 0.963
HoldsAt(Loaded,0) 0.0
HoldsAt(Loaded,1) 0.0
HoldsAt(Loaded,2) 0.0
HoldsAt(Loaded,3) 0.0
HoldsAt(Loaded,4) 0.945
HoldsAt(Loaded,5) 0.945
HoldsAt(Loaded,6) 0.101
HoldsAt(Loaded,7) 0.101
HoldsAt(Loaded,8) 0.101
HoldsAt(Loaded,9) 0.007
HoldsAt(Loaded,10) 0.916
HoldsAt(Loaded,11) 0.916
HoldsAt(Loaded,12) 0.118
HoldsAt(Loaded,13) 0.118
HoldsAt(Loaded,14) 0.118
```


### MAP inference

MAP inference, on the other hand, identifies the most probable assignment among all query predicate instantiations that
are consistent with the given evidence. This task reduces to finding the truth assignment of all query predicate
instantiations that maximises the sum of weights of satisfied ground clauses. This is equivalent to the weighted maximum
satisfiability problem.

```lang-none
lomrf -infer map -i theory.mln -e evidence.db -r map-out.result -q HoldsAt/2 -owa InitiatedAt/2,TerminatedAt/2 -cwa Happens/2
```

The following results are stored in the `map-out.result` file:

```lang-none
HoldsAt(Alive,0) 1
HoldsAt(Alive,1) 1
HoldsAt(Alive,2) 1
HoldsAt(Alive,3) 1
HoldsAt(Alive,4) 1
HoldsAt(Alive,5) 1
HoldsAt(Alive,6) 0
HoldsAt(Alive,7) 0
HoldsAt(Alive,8) 0
HoldsAt(Alive,9) 0
HoldsAt(Alive,10) 0
HoldsAt(Alive,11) 0
HoldsAt(Alive,12) 0
HoldsAt(Alive,13) 0
HoldsAt(Alive,14) 0
HoldsAt(Dead,0) 0
HoldsAt(Dead,1) 0
HoldsAt(Dead,2) 0
HoldsAt(Dead,3) 0
HoldsAt(Dead,4) 0
HoldsAt(Dead,5) 0
HoldsAt(Dead,6) 1
HoldsAt(Dead,7) 1
HoldsAt(Dead,8) 1
HoldsAt(Dead,9) 1
HoldsAt(Dead,10) 1
HoldsAt(Dead,11) 1
HoldsAt(Dead,12) 1
HoldsAt(Dead,13) 1
HoldsAt(Dead,14) 1
HoldsAt(Loaded,0) 0
HoldsAt(Loaded,1) 0
HoldsAt(Loaded,2) 0
HoldsAt(Loaded,3) 0
HoldsAt(Loaded,4) 1
HoldsAt(Loaded,5) 1
HoldsAt(Loaded,6) 0
HoldsAt(Loaded,7) 0
HoldsAt(Loaded,8) 0
HoldsAt(Loaded,9) 0
HoldsAt(Loaded,10) 1
HoldsAt(Loaded,11) 1
HoldsAt(Loaded,12) 0
HoldsAt(Loaded,13) 0
HoldsAt(Loaded,14) 0
```

## References

[^1]: Hanks S. and McDermott D. Nonmonotonic logic and temporal projection. Artificial Intelligence 33.3, 379-412. 1987.

[^2]: Skarlatidis A., Paliouras G., Artikis A. and Vouros G. Probabilistic Event Calculus for Event Recognition. ACM
Transactions on Computational Logic, 16, 2, Article 11, pp. 11:1-11:37, 2015.
