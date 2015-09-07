# Inference #

## Types of Inference
In order to perform inference, we have to define the following:
  * The type of the inference. In LoMRF two inference can be performed:
    1. Marginal inference: for computing the probabilities for all possible instantiations of query predicates of being true, given the evidence.
    2. Maximum a-posteriori inference (MAP): for computing the truth values (0 = False and 1 = True) of all possible instantiations that together represent the most probable state.
  * Input theory file, e.g., `theory.mln`.
  * Input evidence, e.g., `evidence.db`.
  * Output result file, e.g., `output.result`.
  * Which atomic signatures define the query predicates ('-q' option). Please note that for all query predicates LoMRF takes [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption).
  * (Optionally) Which atomic signatures define predicates with [Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption) ('-cwa' option).
  By default, all evidence predicates have Closed-world assumption.
  * (Optionally) Which atomic signatures define predicates with [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) ('-owa' option).
  By default, all non-evidence predicates have Open-world assumption.

### Marginal inference

Marginal inference computes the conditional probability of query predicates (e.g., HoldsAt/2), given the evidence (e.g., Happens/2).

```lang-none
lomrf -infer marginal -i theory.mln -e evidence.db -r marginal-out.result -q HoldsAt/2 -owa InitiatedAt/2,TerminatedAt/2 -cwa Happens/2
```

### MAP inference

MAP inference, on the other hand, identifies the most probable assignment among all query predicate instantiations that
are consistent with the given evidence. This task reduces to finding the truth assignment of all query predicate
instantiations that maximises the sum of weights of satisfied ground clauses. This is equivalent to the weighted maximum
satisfiability problem.

```lang-none
lomrf -infer map -i theory.mln -e evidence.db -r map-out.result -q HoldsAt/2 -owa InitiatedAt/2,TerminatedAt/2 -cwa Happens/2
```


## Command-line Interface Options ##

By executing the ```lomrf -h``` (or ```lomrf --help```) command from the command-line interface, we take the following print of usage options:

```lang-none
o                        o     o   o         o
|             o          |     |\ /|         | /
|    o-o o--o    o-o  oo |     | O |  oo o-o OO   o-o o   o
|    | | |  | | |    | | |     |   | | | |   | \  | |  \ /
O---oo-o o--O |  o-o o-o-o     o   o o-o-o   o  o o-o   o
            |
         o--o
o--o              o               o--o       o    o
|   |             |               |    o     |    |
O-Oo   oo o-o   o-O o-o o-O-o     O-o    o-o |  o-O o-o
|  \  | | |  | |  | | | | | |     |    | |-' | |  |  \
o   o o-o-o  o  o-o o-o o o o     o    | o-o o  o-o o-o

LoMRF: 0.4.1

Usage: [options]

  [-i, --input <kb file>]
        Markov Logic file
  [-e, --evidence <db file(s)>]
        Comma separated evidence database files.
  [-r, --result <result file>]
        Results file
  [-q, --query <string>]
        Comma separated query atoms. Each atom must be defined using its
        identity (i.e. Name/arity). For example the identity of
        QueryAtom(arg1,arg2) is QueryAtom/2
  [-cwa, --closed-world-assumption <string>]
        Specified non-evidence atoms (comma-separated without white-spaces) are
        closed world, otherwise, all non-evidence atoms are open world.Each atom
        must be defined using its identity (i.e. Name/arity, see the description
        of -q for an example)
  [-owa, --open-world-assumption <string>]
        Specified evidence atoms (comma-separated without white-spaces) are open
        world, while other evidence atoms are closed-world. Each atom must be
        defined using its identity (i.e. Name/arity, see the description of -q
        for an example)
  [-infer, --inference-type <map | marginal>]
        Specify the inference type: MAP or Marginal (default is marginal).
  [-mapType, --map-type <mws | ilp>]
        Specify the MAP inference type: MaxWalkSAT or ILP (default is
        MaxWalkSAT).
  [-mapOutput, --map-output-type <all | positive>]
        Specify MAP inference output type: 0/1 results for all query atoms or
        only positive query atoms (default is all).
  [-satHardUnit, --sat-hard-unit]
        Trivially satisfy hard constrained unit clauses in MaxWalkSAT.
  [-satHardPriority, --sat-hard-priority]
        Priority to hard constrained clauses in MaxWalkSAT.
  [-ilpRounding, --ilp-rounding <roundup | mws>]
        Rounding algorithm for ILP (default is RoundUp).
  [-ilpSolver, --ilp-solver <lpsolve | ojalgo | gurobi>]
        Solver used by ILP (default is LPSolve).
  [-samples, --num-samples <value>]
        Number of samples to take (default is 1000).
  [-pSA, --probability-simulated-annealing <value>]
        Specify the probability to perform a simulated annealing step (default
        is 0.5), when using MC-SAT for marginal inference.
  [-pBest, --probability-best-search <value>]
        The probability to perform a greedy search (default is 0.5).
  [-saTemperature, --simulated-annealing-temperature <value>]
        Temperature (take values in [0,1]) for the simulated annealing step in
        MC-SAT (default is 0.8).
  [-maxFlips, --maximum-flips <value>]
        The maximum number of flips taken to reach a solution (default is
        1000000).
  [-targetCost, --target-cost <value>]
        Any possible world having cost below this threshold is considered as a
        solution (default is 1.0E-4).
  [-maxTries, --maximum-tries <value>]
        The maximum number of attempts, in order to find a solution (default is
        1)
  [-numSolutions, --number-of-solutions <value>]
        Give the n-th solution in MC-SAT (default is 10).
  [-tabuLength, --tabu-length <value>]
        Minimum number of flips between flipping the same ground atom in
        successive MaxWalkSAT steps (default is 10).
  [-unitProp, --use-unit-propagation <value>]
        Enable/disable unit propagation (default is true) in MC-SAT.
  [-lateSA, --late-simulated-annealing <value>]
        When enabled (= true), simulated annealing is performed only when MC-SAT
        reaches a plateau (i.e. a world with cost <= 'targetCost'). Disabling
        lateSA (= false) causes MC-SAT to converge slower, since in every
        iteration simulated annealing is performed (with probability = 'pSA').
        By default lateSA is 'true'
  [-noNegWeights, --eliminate-negative-weights]
        Eliminate negative weight values from ground clauses.
  [-noNegatedUnit, --eliminate-negated-unit]
        Eliminate negated unit ground clauses.
  [-dynamic, --dynamic-implementations <string>]
        Comma separated paths to search recursively for dynamic
        predicates/functions implementations (*.class and *.jar files).
  [-v, --version]
        Print LoMRF version.
  [-h, --help]
        Print usage options.
```

### Explanation of inference options ###

***Basic options:***

* `-i, --input <kb file>` **[Required]** Specify the input knowledge base file, that is the file that contains the the theory (see [Syntax](1_syntax.md) and [Quick Start](0_quick_start) for further information). You can specify either full or relative path or only the filename (when the file is in the current working path). For example, (1) full path `-i /full/path/to/theory.mln` in a Unix-based OS or `-i c:\full\path\to\theory.mln` in Windows, (2) relative path `-i path/to/theory.mln` in a Unix-based OS or `-i path\to\theory.mln` in Windows and (3) current working path `-i theory.mln`.

* `-e, --evidence <db file(s)>` [Optional] Specify the input evidence file(s) (see [Syntax](1_syntax.md) and [Quick Start](0_quick_start) for further information). Multiple evidence files are allowed by separating them with commas *without spaces*. For example, `-e file1.db,file2.db,file3.db`. Similarly with the `-i` option, you can specify either full or relative path or only the filename (when the file is in the current working path). In case that there is no evidence, this option can be omitted.

* `-r, --result <result file>` **[Required]** Specify the output file name to write the results of the inference. For example, `-r output.result`. Similarly with the `-i` option, you can specify either full or relative path or only the filename (when the file is in the current working path). In case that there is no evidence, this option can be omitted.

* `-q, --query <string>` **[Required]** Specify the atomic signatures or identities (i.e., predicate_name/arity) of predicates that we would like to query. For example, the query predicate `Male(person)` has name `Male` and arity 1 (single argument), therefore we should give the argument `-q Male/1`. Multiple query atoms are allowed in LoMRF and they are defined as comma-separated identities without white-spaces. For example, `-q Male/1,Female/1`. Please note that LoMRF takes [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) for all query atoms and this cannot be overridden.

* `-cwa, --closed-world-assumption <string>` **[Optional]** Specify the atomic signatures or identities (i.e., predicate_name/arity) of predicates that we would like have [Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption). By default, LoMRF takes Closed-world assumption for all evidence atoms, that is atoms that have at least one fact in the specified evidence files (see `-e` option). All non-evidence atoms are open-world in LoMRF, except when are included in the `-cwa` option. For example, assume that in the evidence file(s) we have facts about the predicate `Foo/1` but not for the predicate `Bar/1` and we would like to have Closed-world for both Foo/1 and Bar/1 predicates. In that case, we have to explicitly define that `Bar/1` is also closed-world, therefore we should give `-cwa Bar/1`. Please note that in this example, it is not required to specify that the predicate `Foo/1` is closed-world, because it has at least one fact in the evidence file(s). If it is desirable, we can also define explicitly closed-world for `Foo/1`, i.e., `-cwa Foo/1,Bar/1` (mutiple identities are defined using comma-separated values, without spaces).

* `-owa, --open-world-assumption <string>` **[Optional]** Specify the atomic signatures or identities (i.e., predicate_name/arity) of predicates that we would like have [Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption). By default, all non-evidence atoms are open-world in LoMRF, except when are included in the `-cwa` option.

* `-infer, --inference-type <map | marginal>` **[Optional]** Specify the inference type, either MAP or Marginal. By default LoMRF uses marginal.


***Advanced Inference options: ***

* `-mapType, --map-type <mws | ilp>` **[Optional]** When inference is set to MAP (i.e., `-infer map`) we can define either [MaxWalkSAT](http://www.cs.rochester.edu/u/kautz/walksat/) or relaxed ILP (Integer Linear Programming) as MAP inference algorithm to use. By default LoMRF uses a variant of the MaxWalkSAT algorithm.

* `-mapOutput, --map-output-type <all | positive>` **[Optional]**

* `-satHardUnit, --sat-hard-unit` **[Optional]**

* `-satHardPriority, --sat-hard-priority` **[Optional]**

* `-ilpRounding, --ilp-rounding <roundup | mws>` **[Optional]**

* `-ilpSolver, --ilp-solver <lpsolve | ojalgo | gurobi>` **[Optional]**

* `-samples, --num-samples <value>` **[Optional]**

* `-pSA, --probability-simulated-annealing <value>` **[Optional]**

* `-pBest, --probability-best-search <value>` **[Optional]**

* `-saTemperature, --simulated-annealing-temperature <value>` **[Optional]**

* `-maxFlips, --maximum-flips <value>`  **[Optional]**

* `-targetCost, --target-cost <value>` **[Optional]**

* `-maxTries, --maximum-tries <value>` **[Optional]**

* `-numSolutions, --number-of-solutions <value>` **[Optional]**

* `-tabuLength, --tabu-length <value>` **[Optional]**

* `-unitProp, --use-unit-propagation <value>` **[Optional]**

* `-lateSA, --late-simulated-annealing <value>` **[Optional]**

* `-noNegWeights, --eliminate-negative-weights` **[Optional]**

* `-noNegatedUnit, --eliminate-negated-unit` **[Optional]**

* `-dynamic, --dynamic-implementations <string>` **[Optional]**
