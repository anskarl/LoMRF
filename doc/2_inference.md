# Inference #

## Types of inference in LoMRF

In order to perform inference, we have to define the following:
  * The type of the inference. In LoMRF two types inference can be performed:
    1. Marginal inference: computes the probabilities for all possible instantiations of query predicates of being true, given the evidence.
    2. Maximum a-posteriori inference (MAP): computes the truth values (0 = False and 1 = True) of all possible instantiations
    (groundings) that together represent the most probable state.
  * Input theory file, e.g., `theory.mln`.
  * Input evidence, e.g., `evidence.db`.
  * Output result file, e.g., `output.result`.
  * Which atomic signatures define the query predicates ('-q' option). Please note that for all query predicates LoMRF takes [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption).
  * (Optionally) Which atomic signatures define predicates with [Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption) ('-cwa' option).
  By default, all evidence predicates have Closed-world assumption.
  * (Optionally) Which atomic signatures define predicates with [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) ('-owa' option).
  By default, all non-evidence predicates have Open-world assumption.

### Inference using the `lomrf` commmand-line tool

To demonstrate the usage of LoMRF from commmand-line interface, assume that we one knowledge base
file, named as `theoy.mln`, and one evidence file, named as `evidence.db`.  

In our example knowledge-base, we have the following predicates:

| Predicate Name | Number of arguments | Predicate identity | Description |
|:--------------:|:-------------------:|:------------------:|:------------|
| Query_A | 2 | `Query_A/2` | first query predicate (Open-world assumption)
| Query_B | 2 | `Query_B/2` | second query predicate (Open-world assumption)
| Ev_A | 1 | `EV_A/1` | first evidence predicate (Closed-world assumption)
| Ev_B | 1 | `EV_B/1` | second evidence predicate (Closed-world assumption)
| Hidden | 1 | `Hidden/1`| non-evidence predicate (Open-world assumption)

As it is presented in the above table, there are two query predicates, `Query_A` and `Query_B`, where each one takes two terms as arguments. Therefore their atomic identities are `Query_A/2` and `Query_B/2`. Similarly, there are two evidence predicates `Ev_A` and `Ev_B` that they take one term as argument. Therefore, the atomic identities of `Ev_A` and `Ev_B` are `Ev_A/1` and `Ev_B/1`, respectively. Finally, there is one non-evidence predicate, named as `Hidden`, that takes one term as parameter, thus it's identity is `Hidden/1`.

#### Marginal inference

Marginal inference computes the conditional probability of query predicates, given some evidence.

```lang-none
lomrf -infer marginal -i theory.mln -e evidence.db -r marginal-out.result -q Query_A/2,Query_B/2 -cwa Ev_A/1,Ev_B/1 -owa Hidden/1
```
Since `Hidden/1` is a non-evidence atom and thus does not contain any fact in the `evidence.db`, we can omit the `-owa` parameter. Furthermore, when the evidence file (evidence.db) contains at least one fact for Ev_A/1 and Ev_B/1 predicates, then we can also omit the parameter `-cwa`.
In that case, LoMRF can automatically infer which predicates have Open-world or Closed-world assumption. As a result, we can simply give the following parameters and take the same results:

```lang-none
lomrf -infer marginal -i theory.mln -e evidence.db -r marginal-out.result -q Query_A/2,Query_B/2
```

Also the default inference type for LoMRF is marginal, we can further simplify the example a give the following parameters:

```lang-none
lomrf -i theory.mln -e evidence.db -r marginal-out.result -q Query_A/2,Query_B/2
```

The results from marginal inference will be stored in the `marginal-out.result` (see paramter `-r`)

#### MAP inference

MAP inference, on the other hand, identifies the most probable assignment among all query predicate instantiations that are consistent with the given evidence. This task reduces to finding
the truth assignment of all query predicate instantiations that maximises the sum of weights of  
satisfied ground clauses. This is equivalent to the weighted maximum satisfiability problem.

```lang-none
lomrf -infer map -i theory.mln -e evidence.db -r map-out.result -q Query_A/2,Query_B/2 -cwa Ev_A/1,Ev_B/1 -owa Hidden/1
```

or simply:

```lang-none
lomrf -infer map -i theory.mln -e evidence.db -r map-out.result -q Query_A/2,Query_B/2
```

The results from MAP inference will be stored in the `map-out.result` (see paramter `-r`)


## Command-line Interface Options ##

By executing the ```lomrf -h``` (or ```lomrf --help```) command from the command-line interface, we take the a print of
multiple parameters. Below we explain all LoMRF inference command-line interface parameters:

*** Basic inference options: ***

* `-i, --input <kb file>` **[Required]** Specify the input knowledge base file, that is the file that contains the
 theory (see [Syntax](1_syntax.md) and [Quick Start](0_quick_start) for further information). You can specify either
 full or relative path or only the filename (when the file is in the current working path). For example,
 (1) full path `-i /full/path/to/theory.mln` in a Unix-based OS or `-i c:\full\path\to\theory.mln` in Windows,
 (2) relative path `-i path/to/theory.mln` in a Unix-based OS or `-i path\to\theory.mln` in Windows and
 (3) current working path `-i theory.mln`.

* `-e, --evidence <db file(s)>` [Optional] Specify the input evidence file(s) (see [Syntax](1_syntax.md) and
[Quick Start](0_quick_start) for further information). Multiple evidence files are allowed by separating them with
commas *without spaces*. For example, `-e file1.db,file2.db,file3.db`. Similarly with the `-i` option, you can specify
either full or relative path or only the filename (when the file is in the current working path). In case that there is
no evidence, this option can be omitted.

* `-r, --result <result file>` **[Required]** Specify the output file name to write the results of the inference.
For example, `-r output.result`. Similarly with the `-i` option, you can specify either full or relative path or only
the filename (when the file is in the current working path). In case that there is no evidence, this option can be omitted.

* `-q, --query <string>` **[Required]** Specify the atomic signatures or identities (i.e., predicate_name/arity) of
predicates that we would like to query. For example, the query predicate `Male(person)` has name `Male` and arity 1
(single argument), therefore we should give the argument `-q Male/1`. Multiple query atoms are allowed in LoMRF and
they are defined as comma-separated identities without white-spaces. For example, `-q Male/1,Female/1`. Please note
that LoMRF takes [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) for all query atoms and
this cannot be overridden.

* `-cwa, --closed-world-assumption <string>` **[Optional]** Specify the atomic signatures or identities (i.e., predicate_name/arity)
of predicates that we would like have [Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption).
By default, LoMRF takes Closed-world assumption for all evidence atoms, that is atoms that have at least one fact in
the specified evidence files (see `-e` option). All non-evidence atoms are open-world in LoMRF, except when are included
in the `-cwa` option. For example, assume that in the evidence file(s) we have facts about the predicate `Foo/1` but not
for the predicate `Bar/1` and we would like to have Closed-world for both Foo/1 and Bar/1 predicates. In that case, we
have to explicitly define that `Bar/1` is also closed-world, therefore we should give `-cwa Bar/1`. Please note that in
this example, it is not required to specify that the predicate `Foo/1` is closed-world, because it has at least one fact
in the evidence file(s). If it is desirable, we can also define explicitly closed-world for `Foo/1`, i.e., `-cwa Foo/1,Bar/1`
(multiple identities are defined using comma-separated values, without spaces).

* `-owa, --open-world-assumption <string>` **[Optional]** Specify the atomic signatures or identities (i.e., predicate_name/arity)
of predicates that we would like have [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption).
By default, all non-evidence atoms are open-world in LoMRF, except when are included in the `-cwa` option.

* `-infer, --inference-type <map | marginal>` **[Optional]** Specify the inference type, either [MAP](https://en.wikipedia.org/wiki/Maximum_a_posteriori_estimation)
or Marginal. By default LoMRF uses marginal inference, in order to estimate the marginal probabilities of all possible
query predicate instantiations. MAP inference can be performed using either through local-search algorithm ([MaxWalkSAT](http://www.cs.rochester.edu/u/kautz/walksat/)[^1][^2])
or using an Integer Linear Programming ([ILP](https://en.wikipedia.org/wiki/Integer_programming)) solver [^3]. Marginal inference
is estimated using the [MC-SAT](http://alchemy.cs.washington.edu/papers/poon06/)[^4] algorithm, that is a [Metropolis–Hastings](https://en.wikipedia.org/wiki/Metropolis%E2%80%93Hastings_algorithm) algorithm
(a [Markov chain Monte Carlo](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo) method)
that combines [Simulated-Annealing](https://en.wikipedia.org/wiki/Simulated_annealing) with a local-search [SAT solver](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem)
([MaxWalkSAT](http://www.cs.rochester.edu/u/kautz/walksat/)), using a [Slice-sampling](https://en.wikipedia.org/wiki/Slice_sampling)
technique.


*** Advanced inference options: ***

* `-mapType, --map-type <mws | ilp>` **[Optional]** When inference is set to MAP (i.e., `-infer map`) we can define
either [MaxWalkSAT](http://www.cs.rochester.edu/u/kautz/walksat/)[^1][^2] or relaxed [Integer Linear Programming](https://en.wikipedia.org/wiki/Integer_programming)[^3]
as MAP inference algorithm to use. By default LoMRF uses a variant of the MaxWalkSAT algorithm (with [TABU search](https://en.wikipedia.org/wiki/Tabu_search)).

* `-mapOutput, --map-output-type <all | positive>` **[Optional]** When MAP inference is selected (i.e., `-infer map`) we
can specify the format in the result file (i.e., `-r` option). We can choose either (1) `all`, in order to contain all possible
groundings of the resulting query predicates associated with their truth value (i.e., 0 for false and 1 for true) or (2)
`positive`, in order to contain only the ground atoms that have positive state (true). By default, LoMRF outputs all groundings
with their truth values.

* `-satHardUnit, --sat-hard-unit` **[Optional]** Try to trivially satisfy all hard-constrained unit clauses in MaxWalkSAT.
After the grounding process, hard-constrained ground unit-clauses may produced. Such clauses contain only one ground literal
and therefore can be handled like fact predicates. If the literal is positive, then the state that satisfies this clause is true.
Similarly, if the literal is negative then the state that satisfies this clause is false. Consider, for example the ground hard-constrained
unit clause `Male(Odysseus)`. The literal is positive (i.e., is not negated), thus the clause is satisfied when `Male(Odysseus) = True`.

* `-satHardPriority, --sat-hard-priority` **[Optional]** This option applies to MaxWalkSAT and MC-SAT algorithms.
Enables satisfiability priority to hard-constrained clauses. Specifically, when the local-search algorithm (either
MaxWalkSAT or MC-SAT) requires to select an unsatisfied clause and the `satHardPriority` is enabled, then the algorithm
picks a recently unsatisfied hard-constrained clause (if any exists). In any other case the selection is random. Therefore,
`satHardPriority` adds an additional bias to satisfy hard-constrained clauses.


* `-ilpRounding, --ilp-rounding <roundup | mws>` **[Optional]** Specify which rounding algorithm to use in ILP. LoMRF uses the
algorithm proposed by T. Huynh and R. Mooney [^3], in which the ILP problem is relaxed as a standard LP problem.
The resulting LP solution, however, may not integral. As a result the solution may contain ground query predicates in
which their state may be any number in the interval from 1 to 2, exclusive. Since, in MAP inference the solution should
contain only 0/1 states for each resulting ground predicate, LoMRF employs a rounding algorithm to change the non-integral
solutions to 0/1 state values. By default LoMRF uses the RoundUp algorithm of [^3] (i.e., `-ilpRounding roundup`).
Alternatively, LoMRF can run the local-search MaxWalkSAT only for the non-integral part (i.e., `-ilpRounding mws`).

* `-ilpSolver, --ilp-solver <lpsolve | ojalgo | gurobi>` **[Optional]** When MAP inference is chosen to be solved using
an ILP solver, we can specify which solver we can use. We can choose between the open-source solvers
[LPSolve](http://lpsolve.sourceforge.net/5.5/) and [ojAlgo](http://ojalgo.org/),
as well as the commercial solver [Gurobi](http://www.gurobi.com/). By default LoMRF uses the open-source solver LPSolve.

* `-samples, --num-samples <value>` **[Optional]** Specify the number of samples to take in MC-SAT (default is 1000).

* `-pSA, --probability-simulated-annealing <value>` **[Optional]** Specify the probability to perform a simulated
annealing step (default is 0.5), when using MC-SAT for marginal inference. Every step in MC-SAT may involve multiple
iterations, in which the state of ground atoms is flipped. At each iteration of the MC-SAT algorithm performs either
simulated annealing or a standard MaxWalkSAT step. This option adjusts the probability to choose a simulated annealing
step.

* `-pBest, --probability-best-search <value>` **[Optional]** The probability to perform a greedy search (default is 0.5)
in MaxWalkSAT or MC-SAT. The algorithms perform either a noisy or a greedy move. The algorithm takes a random unsatisfied
clause and in noisy move chooses a random atom to flip. In a greedy move, on the other hand, the algorithm chooses to flip
the atom with the best score (satisfies more clauses).

* `-saTemperature, --simulated-annealing-temperature <value>` **[Optional]** Adjust the temperature of the simulated annealing
step in the MC-SAT algorithm. The value should range between 0 and 1. The default value is 0.8.

* `-maxFlips, --maximum-flips <value>`  **[Optional]** Specify the maximum number of flips taken to reach a solution in
MaxWalkSAT and MC-SAT (default is 1000000).

* `-targetCost, --target-cost <value>` **[Optional]** In MaxWalkSAT and MC-SAT, any possible world having cost below this
threshold will be considered as a solution (default is 0.0001).

* `-maxTries, --maximum-tries <value>` **[Optional]** Specify the maximum number of attempts to find a solution in
MaxWalkSAT and MC-SAT (default is 1)

* `-numSolutions, --number-of-solutions <value>` **[Optional]** Give the n-th solution (i.e., cost < target cost) in MC-SAT (default is 10).

* `-tabuLength, --tabu-length <value>` **[Optional]** Specify the minimum number of flips between flipping the same atom
in MaxWalkSAT and MC-SAT (default is 10)

* `-unitProp, --use-unit-propagation <value>` **[Optional]** Perform unit-propagation in MC-SAT (default is true).
 Performs unit propagation across the constraints in order to trivially satisfy as many as possible. When enabled, the
 search space in MC-SAT is minimized, increases sampling performance and accuracy.

* `-lateSA, --late-simulated-annealing <value>` **[Optional]**  When enabled (i.e., true), simulated annealing is
performed only when MC-SAT reaches a plateau (i.e. a world with cost <= 'targetCost'). Disabling lateSA (= false)
causes MC-SAT to converge slower, since in every iteration simulated annealing is performed (with probability = 'pSA').
By default lateSA is 'true'.

* `-noNegWeights, --eliminate-negative-weights` **[Optional]** When it is enabled, LoMRF eliminates negative
weight values from (ground) clauses. Specifically, the sign of negative weights in clauses is inverted, as well as all
disjunctions become conjunctions (due to de Morgan's law). For example, using the de Morgan's law, the weighted
clause `-2 A(x) v B(x)` is equivalent to `-2 !(!A(x) ^ !B(x))`. In MLN this is also equivalent to `2 !A(x) ^ !B(x)`,
which produces the following two unit clauses: `1 A(x)` and `1 B(x)`.

* `-noNegatedUnit, --eliminate-negated-unit` **[Optional]** When it is enabled, unit clauses with negative
literal become unit clauses with positive literal and inverted sign in their corresponding weight.

* `-dynamic, --dynamic-implementations <string>` **[Optional]** Comma separated paths to search recursively for dynamic
predicates/functions implementations (*.class and *.jar files).

## Examples

Below we provide some example MLNs, in order to demonstrate the LoMRF inference command-line tool:

### Uniform distribution

We would like to model a simple coin flip scenario. We state that the outcome of a flip is heads with the predicate Heads(flip). Flip ranges from 1 to 20. If Heads(n) is true, then flip n was heads, otherwise it was tails ([see original example](http://alchemy.cs.washington.edu/tutorial/2_1Uniform_Distribution.html)).

Knowledge base (uniform.mln):

```lang-none
flip = {1,...,20}

Heads(flip)
```

We do not have any evidence in this example, thus we omit the `-e` parameter and we run our example with the following parameters:

```lang-none
lomrf -i uniform.mln -q Heads/1 -r uniform.result
```

The contents of the result file (uniform.result) are the following:

```lang-none
Heads(20) 0.493
Heads(19) 0.508
Heads(18) 0.515
Heads(17) 0.499
Heads(16) 0.503
Heads(15) 0.513
Heads(14) 0.496
Heads(13) 0.532
Heads(12) 0.503
Heads(11) 0.464
Heads(10) 0.494
Heads(9) 0.508
Heads(8) 0.504
Heads(7) 0.496
Heads(6) 0.527
Heads(5) 0.504
Heads(4) 0.479
Heads(3) 0.506
Heads(2) 0.473
Heads(1) 0.512
```

### Binomial distribution ###

To model a binomial distribution we simply add the weighted unit clause `1 Heads(f)` ([see original example](http://alchemy.cs.washington.edu/tutorial/2_1Uniform_Distribution.html)).

Knowledge base (binomial.mln):

```lang-none
flip = {1,...,20}

Heads(flip)

// unit clause
1 Heads(f)
```

We run our example with the following parameters:

```lang-none
lomrf -i binomial.mln -q Heads/1 -r binomial.result
```

The contents of the result file (binomial.result) are the following:

```lang-none
Heads(20) 0.728
Heads(19) 0.739
Heads(18) 0.74
Heads(17) 0.74
Heads(16) 0.718
Heads(15) 0.748
Heads(14) 0.736
Heads(13) 0.715
Heads(12) 0.76
Heads(11) 0.731
Heads(10) 0.725
Heads(9) 0.722
Heads(8) 0.749
Heads(7) 0.785
Heads(6) 0.742
Heads(5) 0.684
Heads(4) 0.742
Heads(3) 0.698
Heads(2) 0.73
Heads(1) 0.746
```

### Multinomial distribution ###

In this example we model the outcome of a six-faced die for 20 throws. The outcome of each throw with is modeled by the predicate `Outcome(throw, face)` and two formulas stating that each throw has exactly one outcome ([see original example](http://alchemy.cs.washington.edu/tutorial/2_3Multinomial_Distribution.html)).

Knowledge base (multinomial.mln):

```lang-none
throw = {1,...,20}
face = {1,...,6}

Outcome(throw, face)

// At least one outcome must occur for each throw
Exist f Outcome(t,f).

// At most one outcome must occur
Outcome(t,f0) ^ !(f0 = f1) => !Outcome(t,f1).
```

We run our example with the following parameters:
```
lomrf -i multinomial.mln -q Outcome/2 -r multinomial.result
```

The contents of the result file (multinomial.result) are the following:
```
Outcome(20,6) 0.164
Outcome(19,6) 0.151
Outcome(18,6) 0.165
Outcome(17,6) 0.158
Outcome(16,6) 0.181
Outcome(15,6) 0.148
Outcome(14,6) 0.177
Outcome(13,6) 0.158
Outcome(12,6) 0.16
Outcome(11,6) 0.177
Outcome(10,6) 0.146
Outcome(9,6) 0.175
Outcome(8,6) 0.174
Outcome(7,6) 0.153
Outcome(6,6) 0.176
Outcome(5,6) 0.169
Outcome(4,6) 0.176
Outcome(3,6) 0.164
Outcome(2,6) 0.187
Outcome(1,6) 0.152
Outcome(20,5) 0.177
Outcome(19,5) 0.18
Outcome(18,5) 0.164
Outcome(17,5) 0.173
Outcome(16,5) 0.16
Outcome(15,5) 0.18
Outcome(14,5) 0.161
Outcome(13,5) 0.146
Outcome(12,5) 0.158
Outcome(11,5) 0.161
Outcome(10,5) 0.191
Outcome(9,5) 0.155
Outcome(8,5) 0.165
Outcome(7,5) 0.162
Outcome(6,5) 0.172
Outcome(5,5) 0.166
Outcome(4,5) 0.165
Outcome(3,5) 0.169
Outcome(2,5) 0.166
Outcome(1,5) 0.189
Outcome(20,4) 0.184
Outcome(19,4) 0.2
Outcome(18,4) 0.179
Outcome(17,4) 0.164
Outcome(16,4) 0.17
Outcome(15,4) 0.181
Outcome(14,4) 0.172
Outcome(13,4) 0.168
Outcome(12,4) 0.159
Outcome(11,4) 0.142
Outcome(10,4) 0.156
Outcome(9,4) 0.179
Outcome(8,4) 0.163
Outcome(7,4) 0.164
Outcome(6,4) 0.163
Outcome(5,4) 0.155
Outcome(4,4) 0.164
Outcome(3,4) 0.169
Outcome(2,4) 0.16
Outcome(1,4) 0.161
Outcome(20,3) 0.157
Outcome(19,3) 0.168
Outcome(18,3) 0.175
Outcome(17,3) 0.165
Outcome(16,3) 0.16
Outcome(15,3) 0.174
Outcome(14,3) 0.158
Outcome(13,3) 0.172
Outcome(12,3) 0.18
Outcome(11,3) 0.166
Outcome(10,3) 0.163
Outcome(9,3) 0.167
Outcome(8,3) 0.176
Outcome(7,3) 0.165
Outcome(6,3) 0.156
Outcome(5,3) 0.181
Outcome(4,3) 0.158
Outcome(3,3) 0.178
Outcome(2,3) 0.176
Outcome(1,3) 0.18
Outcome(20,2) 0.166
Outcome(19,2) 0.141
Outcome(18,2) 0.148
Outcome(17,2) 0.166
Outcome(16,2) 0.171
Outcome(15,2) 0.135
Outcome(14,2) 0.174
Outcome(13,2) 0.176
Outcome(12,2) 0.171
Outcome(11,2) 0.176
Outcome(10,2) 0.175
Outcome(9,2) 0.158
Outcome(8,2) 0.164
Outcome(7,2) 0.169
Outcome(6,2) 0.163
Outcome(5,2) 0.161
Outcome(4,2) 0.176
Outcome(3,2) 0.159
Outcome(2,2) 0.16
Outcome(1,2) 0.171
Outcome(20,1) 0.152
Outcome(19,1) 0.16
Outcome(18,1) 0.169
Outcome(17,1) 0.174
Outcome(16,1) 0.158
Outcome(15,1) 0.182
Outcome(14,1) 0.158
Outcome(13,1) 0.18
Outcome(12,1) 0.172
Outcome(11,1) 0.178
Outcome(10,1) 0.169
Outcome(9,1) 0.166
Outcome(8,1) 0.158
Outcome(7,1) 0.187
Outcome(6,1) 0.17
Outcome(5,1) 0.168
Outcome(4,1) 0.161
Outcome(3,1) 0.161
Outcome(2,1) 0.151
Outcome(1,1) 0.147
```

### Does Marcus hate Caesar? ###

Example in natural language:
  1. Marcus is a person.
  2. Marcus is a Pompeian.
  3. All Pompeians are Roman.
  4. Caesar is a ruler.
  5. All Romans are either loyal to Caesar or hate Caesar.
  6. Everyone is loyal to someone.
  7. People only try to assassinate rulers to whom they are not loyal.
  8. Marcus tried to assassinate Caesar.

** Knowledge base (theory.mln) **

Predicate schema:
```lang-none
// Query predicates:
Hate(person,person)

// Evidence predicates:
People(person)
Ruler(person)
Pompeian(person)
Assassinate(person, person)

// Other non-evidence predicates:
Loyal(person, person)
Roman(person)
```
Formulas:

1. All Pompeians are Roman (hard-constrained):
```lang-none
Forall x Pompeian(x) => Roman(x).
```
This formula can also written as following, since by default all variables implicitly assumed to be universally quantified unless otherwise indicated:
```lang-none
Pompeian(x) => Roman(x).
```

2. All Romans were either loyal to Caesar or hated him or both (hard-constrained):

```lang-none
Roman(x) => Loyal(x, Caesar) v Hate(x, Caesar).
```

3. Usually, everyone is loyal to someone (soft-constrained):

```lang-none
1 Forall x Exist y Loyal(x, y)
```

4. People may try to assassinate rulers to whom they are not loyal (soft-constrained):

```lang-none
2 Forall x,y People(x) ^ Ruler(y) ^ Assassinate(x,y) => !Loyal(x, y)
```
5. Usually nobody hates himself (soft-constrained):

```lang-none
1 !Hate(x, x)
```

Final form of the knowledge base file (theory.mln):
```lang-none
// Query predicates:
Hate(person,person)

// Evidence predicates:
People(person)
Ruler(person)
Pompeian(person)
Assassinate(person, person)

// Other non-evidence predicates:
Loyal(person, person)
Roman(person)

// All Pompeians are Roman (hard-constrained)
Pompeian(x) => Roman(x).

// All Romans were either loyal to Caesar or hated him or both (hard-constrained)
Roman(x) => Loyal(x, Caesar) v Hate(x, Caesar).

// Usually, everyone is loyal to someone (soft-constrained)
1 Exist y Loyal(x,y)

// People may try to assassinate rulers to whom they are not loyal (soft-constrained)
2 People(x) ^ Ruler(y) ^ Assassinate(x,y) => !Loyal(x, y)

// Usually, nobody hates himself (soft-constrained)
1 !Hate(x, x)
```

** Evidence (evidence.db) **

```lang-none
People(Markus)
Pompeian(Marcus)
Ruler(Caesar)
Assassinate(Marcus, Caesar)
```

** Inference execution **

```lang-none
lomrf -infer marginal -i theory.mln -e evidence.db -r output.result -q Hate/2 -owa Loyal/2,Roman/1 -cwa People/1,Ruler/1,Pompeian/1,Assassinate/2
```

** Inference result (output.result) **

```lang-none
Hate(Marcus,Marcus) 0.251
Hate(Caesar,Marcus) 0.532
Hate(Marcus,Caesar) 0.855
Hate(Caesar,Caesar) 0.315
```

### Yale Shooting Scenario ###

See the temporal reasoning example in [Quick Start](0_quick_start.md).

## References

[^1] Bart Selman, Henry Kautz, and Bram Cohen. (1993) Local Search Strategies for Satisfiability Testing.  Final version appears
in Cliques, Coloring, and Satisfiability: Second DIMACS Implementation Challenge. In David S. Johnson and Michael A. Trick, (Ed.),
DIMACS Series in Discrete Mathematics and Theoretical Computer Science, vol. 26, AMS.

[^2] Henry Kautz, Bart Selman and Yueyen Jiang. A General Stochastic Approach to Solving Problems with Hard and Soft Constraints.
In Gu, D., Du, J. and Pardalos, P. (Eds.), The Satisfiability Problem: Theory and Applications, Vol. 35 of DIMACS Series in
Discrete Mathematics and Theoretical Computer Science, pp. 573–586. AMS

[^3] Tuyen N. Huynh and Raymond J. Mooney. Max-Margin Weight Learning for Markov Logic Networks. In Proceedings of the
European Conference on Machine Learning and Principles and Practice of Knowledge Discovery in Databases (ECML-PKDD 2011),
Vol. 2, pp. 81-96, 2011.

[^4] Poon, Hoifung and Domingos, Pedro (2006). Sound and Efficient Inference with Probabilistic and Deterministic Dependencies.
In Proceedings of the 21th National Conference on Artificial Intelligence (pp. 458-463), 2006. Boston, MA: AAAI Press.
