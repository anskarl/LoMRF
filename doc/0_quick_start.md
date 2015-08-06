# Quick Start

Assuming that you have successfully build a LoMRF distribution and added LoMRF executables in your default `PATH` (see 
[Building and Linking](5_building_and_linking.md)). In the following paragraphs we will write our first LoMRF model and
perform probabilistic inference.

## Running Example

For our first example we will use the [Yale Shooting Problem](https://en.wikipedia.org/wiki/Yale_shooting_problem), 
a well-known test case of non-monotonic temporal reasoning which is proposed by Steve Hanks and Drew McDermott[^1]. 
In brief, a hunter tries to shoot a prey with his gun. In order to shoot the prey, the gun must be loaded. When
the hunter shoots with a loaded gun, the prey is being killed.

In the example scenario there is a sequence of *actions* (e.g., loading the gun) and *situations* (e.g., the prey is
alive). The series of actions and situations occur in time. For example, at each instant of time some situation holds
(e.g., the gun is loaded) or an event may happen (e.g., load the gun).

In this example, we model the Yale Shooting scenario by using [Event Calculus](https://en.wikipedia.org/wiki/Event_calculus) 
logical action formalism. In the Event Calculus we represent *actions* with *events* and *situations* with *fluents*.
More formally, a fluent is a property whose value may change over time. When an event occurs it may change the value of
a fluent. The underlying time  model is linear and we represent time-points as integer numbers. The core
domain-independent axioms of the Event Calculus define whether a fluent holds or not at a specific time-point. Moreover,
the axioms incorporate the common sense *law of inertia*, according to which fluents persist over time, unless they are
affected by the occurrence of some event. For example, the gun remains loaded (fluent) until the hunter shoots (event).
Below we outline the events and the fluents that we will use in the example:

| Event | Description                |
|:------|:---------------------------|
| Shoot | Gun is shoot at the prey   |
| Load  | Load the gun               |

| Fluent | Description               |
|:-------|:--------------------------|
| Loaded | The gun is loaded         |
| Alive  | The prey is alive         |
| Dead   | The prey is dead          |


The scenario that we are going to follow is described below:

  1. At the beginning (i.e., initial state), the prey is alive and the hunter's gun is unloaded.
  2. The hunter is shooting with an unloaded gun. We expecting that nothing changes and therefore the prey remains alive.
  3. The hunter loads the gun, waiting for a moment, and then he shoots the prey. As a result, the prey becomes dead.
  4. From that point in time and after, the prey should remain dead, no matter what is happening (e.g., reloading the gun).

## Writing your first knowledge base

Once we have described the running example, we now begin with formulating our knowledge base. The knowledge base file
has suffix `.mln` and in this example is named as `theory.mln`.

The contents of the `theory.mln` file is can be the following:

  1. Domain types and (optionally) their possible values, e.g., the domain of *time* ranging from 0 to 10.
  2. Predicate definitions, representing the structure of a predicate. For example, the predicate that is named as `Foo`
  with a single argument that takes constants from the domain of time, i.e., `Foo(time)`.
  3. (optional) Function definitions, representing the structure of a function. For example, the function that is named
  as `bar` with a single argument that takes constants from the domain of time and returns constants from the domain
  *month* (i.e., representing the month number), i.e., `month = bar(time)`.
  4. Formulas represented in First-Order logic, expressing the template for producing Markov Networks.
     * The syntax of logical formulas can be found [here]()
     * Each formula imposes a constraint.
     * Each formula can be associated with some *weight* value, that is a positive or negative real number. The higher
    the value of weight, the stronger the constraint represented by the formula.
     * Hard-constrained formulas, do not have weights (the weight is assumed infinite) and capture the knowledge which
    is assumed to be certain.
     * Soft-constrained formulas are always associated with weights and capture imperfect knowledge, allowing for the
    existence of worlds in which this knowledge is violated.

Please note that variables and domain types are starting with a lower-case letter. All variables are assumed to be
universally quantified unless otherwise indicated. *Predicates*, *functions* and *constants* start with an upper-case
letter.

### Domain types

That part is optional, since the LoMRF can collect all possible constant symbols for each domain type that appears in
our formulas, as well as in the specified evidence. Furthermore, if the have constant symbols in both evidence and our theory (domain types, as well as in formulas), the
LoMRF will automatically compute the union of all constant symbols.

The domain types that we will use in our example are the domains of *time*, *event* and *fluent*. The values where each
one domain takes are constant symbols and are finite. Their possible finite symbols can be explicitly defined in our
`theory.mln` file. For example, the domain of fluents is represented below:

```
fluent = {Loaded, Alive, Dead}
```
The name of the domain is `fluent` and is starting with a lower-case letter. In the brackets we add the possible values
that the domain `fluent` can take. Each value is a constant symbol, therefore is starting with an upper-case letter.

For integer-valued domains like the domain of time, we can use the following notation:

```
time = {1,...,100}
```

The resulting domain of time will contain the range of 1 to 100 as constant symbols.

### Predicate definitions

We have to define the schema of each predicate that we will use in our example. Since we are employing the Event Calculus
formalism, we will use the Event Calculus predicates (outlined in the table below).

| Predicate                  | Meaning                                                                |
|:---------------------------|:-----------------------------------------------------------------------|
| Happens(event, time)       | An *event* occurs at some point in *time*.                             |
| HoldsAt(fluent, time)      | A *fluent* holds at some point in *time*.                              |
| InitiatedAt(fluent, time)  | A *fluent* is initiated at some point in *time*, i.e., begins to hold. |
| TerminatedAt(fluent, time) | A *fluent* is terminated at some point in *time*, i.e., stops to hold. |

Furthermore, to express the sequence of time we use the predicate `Next(time,time)`, in order to associate successive
time-points. The schema of our predicates are given below:

```
HoldsAt(fluent,time)
InitiatedAt(fluent,time)
TerminatedAt(fluent,time)
Happens(event,time)
Next(time,time)
```

All predicates names are starting with upper-case letters, while the domain types of their arguments are expressed with
lower-case letters.

### Formulas

First of all we have to express the Event Calculus core axioms that are domain-independent. That axioms are not express
knowledge specific for our running example, but express the conditions under which a fluent value changes or persists.

```
Next(t1,t0) ^ InitiatedAt(f, t0) => HoldsAt(f, t1).
Next(t1,t0) ^ TerminatedAt(f, t0) => !HoldsAt(f, t1).

Next(t1,t0) ^ HoldsAt(f,t0) ^ !TerminatedAt(f, t0) => HoldsAt(f,t1).
Next(t1,t0) ^ !HoldsAt(f,t0) ^ !InitiatedAt(f, t0) => !HoldsAt(f,t1).
```
Variables are expressed with lower-case symbols (e.g., `f`, `t0` and `t1`) and are all implicitly universally quantified.
The logical symbols `^`, `=>` and `<=>` express logical conjunction, implication and equivalence, respectively.

For example the first formula expresses that when a fluent, indicated by the variable `f`, is initiated at some time-point
`t0`, will hold at the next time point `t1`. Similarly, the last formula expresses a fluent continues to not hold at `t1`
when the fluent is not initiated at the previous time-point `t0`.

All four formulas are hard-constrained, thus they do not have any weight value and have a dot at the end. The reason that
we use hard-constrained formulas to express the core Event Calculus axioms, is that we want to hold with absolute
certainty. Further details about the Event Calculus formalism that we are using in our example can be found in [^2].

Having formalised the Event Calculus axioms in LoMRF, we continue with the domain-dependent axioms that describe the
knowledge for our running example.

We would like to express the rule that the gun becomes loaded if and only if the hunter loads the gun. This rule is
expressed with the following formula:

```
Happens(Load, t) <=> InitiatedAt(Loaded, t)
```
For reasons that have to do with the addition of weights we choose to equivalently represent that rule by using the
following two separate formulas.

```
Happens(Load, t) => InitiatedAt(Loaded, t)

InitiatedAt(Loaded, t) => Happens(Load, t)
```
The first formula states that the fluent `Loaded` is being initiated when the event `Load` occurs, while the second
formula restricts the rules out the possibility that the fluent `Loaded` has been initiated when other irrelevant events
occur.

In order to express a soft-constrained formula, we have to associated with a weight value. Lets say that by using a
weight learning algorithm (see [Weight Learning](doc/2_weight_learning.md)), the weight of the first rule is estimated
to be equal with 2.0. On the other hand, we restrict the second rule to remain hard-constrained. Therefore, the final
form of these two formulas is given below:


```
2 Happens(Load, t) => InitiatedAt(Loaded, t)

InitiatedAt(Loaded, t) => Happens(Load, t).
```
The first formula is soft-constrained and it is associated with a weight value, equal with 2. The second formula is
hard-constrained, thus it ends with a dot.

By following the same formulation the rest set of domain-dependent axioms is given below. For simplicity we are using
the same weight value for all soft-constrained formulas.

```
// The gun becomes loaded if and only if the hunter loads the gun.
2 Happens(Load,t) => InitiatedAt(Loaded,t)
InitiatedAt(Loaded,t) => Happens(Load,t).

// The stops from being loaded after a shot.
2 Happens(Shoot,t) ^ HoldsAt(Loaded,t) => TerminatedAt(Loaded,t)
TerminatedAt(Loaded,t) => Happens(Shoot,t) ^ HoldsAt(Loaded,t).

// When the gun shoots and it is loaded, then the prey is being killed.
2 Happens(Shoot,t) ^ HoldsAt(Loaded,t) => InitiatedAt(Dead,t)
InitiatedAt(Dead,t) => Happens(Shoot,t) ^ HoldsAt(Loaded,t).

// When the gun shoots and it is loaded, then the pread is stops from being alive.
2 Happens(Shoot,t) ^ HoldsAt(Loaded,t) => TerminatedAt(Alive,t)
TerminatedAt(Alive,t) => Happens(Shoot,t) ^ HoldsAt(Loaded,t).

// There isn't any event that can stop a dead prey from being dead
!TerminatedAt(Dead, t).

// There isn't any event that can make a prey alive
!InitiatedAt(Alive,t).
```

Finally we can express the initial state (i.e., at time-point 0) of our running example with the following formulas:

```
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

### Resulting MLN file

The final form of our knowledge base (`theory.mln`) is given below:

```
fluent = {Loaded, Alive, Dead}

// Predicate schema
HoldsAt(fluent,time)
InitiatedAt(fluent,time)
TerminatedAt(fluent,time)
Happens(event,time)
Next(time,time)

/**
 * Event Calculus domain-independent axioms
 */

Next(t1,t0) ^ InitiatedAt(f, t0) => HoldsAt(f, t1).
Next(t1,t0) ^ TerminatedAt(f, t0) => !HoldsAt(f, t1).

Next(t1,t0) ^ HoldsAt(f,t0) ^ !TerminatedAt(f, t0) => HoldsAt(f,t1).
Next(t1,t0) ^ !HoldsAt(f,t0) ^ !InitiatedAt(f, t0) => !HoldsAt(f,t1).

/**
 * Domain-dependent rules
 */

// The gun becomes loaded if and only if the hunter loads the gun.
2 Happens(Load,t) => InitiatedAt(Loaded,t)
InitiatedAt(Loaded,t) => Happens(Load,t).

// The gun stops from being loaded after a shot.
2 Happens(Shoot,t) ^ HoldsAt(Loaded,t) => TerminatedAt(Loaded,t)
TerminatedAt(Loaded,t) => Happens(Shoot,t) ^ HoldsAt(Loaded,t).

// When the gun shoots and it is loaded, then the prey is being killed.
2 Happens(Shoot,t) ^ HoldsAt(Loaded,t) => InitiatedAt(Dead,t)
InitiatedAt(Dead,t) => Happens(Shoot,t) ^ HoldsAt(Loaded,t).

// When the gun shoots and it is loaded, then the pread is stops from being alive.
2 Happens(Shoot,t) ^ HoldsAt(Loaded,t) => TerminatedAt(Alive,t)
TerminatedAt(Alive,t) => Happens(Shoot,t) ^ HoldsAt(Loaded,t).

// There isn't any event that can stop a dead prey from being dead
!TerminatedAt(Dead, t).

// There isn't any event that can make a prey alive
!InitiatedAt(Alive,t).

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

1. At time-point 2, the hunter shoots.
2. At time-point 3, the hunter loads the gun.
3. At time-point 5, the hunter shoots.
4. At time-point 9, the hunter loads the gun.
5. Finally at time-point 11, the hunter the hunter shoots again.

Furthermore the time sequence (0 to 13) is represented ground `Next` predicates. This scenario is represented by the
following ground predicates:

```
Happens(Shoot, 2)
Happens(Load, 3)
Happens(Shoot, 5)
Happens(Shoot, 8)
Happens(Load, 9)
Happens(Shoot, 11)

Next(1,0)
Next(2,1)
Next(3,2)
Next(4,3)
Next(5,4)
Next(6,5)
Next(7,6)
Next(8,7)
Next(9,8)
Next(10,9)
Next(11,10)
Next(12,11)
Next(13,12)
Next(14,13)
```

## Perform inference

Two types of inference can be performed in LoMRF, marginal inference and maximum a-posteriori inference (MAP).  

### Marginal inference
Marginal inference computes the conditional probability of query predicates (e.g., HoldsAt), given the evidence (e.g., Happens 
and Next).
```
lomrf -infer marginal -i theory.mln -e evidence.db -r marginal.result -q HoldsAt/2 -owa InitiatedAt/2,TerminatedAt/2 -cwa Happens/2,Next/2
```

### MAP inference

MAP inference, on the other hand, identifies the most probable assignment among all query predicate instantiations that 
are consistent with the given evidence. This task reduces to finding the truth assignment of all query predicate 
instantiations that maximises the sum of weights of satisfied ground clauses. This is equivalent to the weighted maximum 
satisfiability problem. 
```
lomrf -infer map -i theory.mln -e evidence.db -r marginal.result -q HoldsAt/2 -owa InitiatedAt/2,TerminatedAt/2 -cwa Happens/2,Next/2
```

## References

[^1]: Hanks S. and McDermott D. Nonmonotonic logic and temporal projection. Artificial Intelligence 33.3, 379-412. 1987.
[^2]: Skarlatidis A., Paliouras G., Artikis A. and Vouros G. Probabilistic Event Calculus for Event Recognition. ACM Transactions on Computational Logic, 16, 2, Article 11, pp. 11:1-11:37, 2015.