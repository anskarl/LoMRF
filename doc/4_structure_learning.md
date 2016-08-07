# Structure Learning #

LoMRF employs structure learning using on-line algorithms, in order to update the structure and its weights at each learning step according to a sequence of training examples (micro-batches). Compare to batch learning methods, the main benefits of on-line structure learning is that it can scale to problems with large amount of data and that can revise/refine an MLN model according to new training examples.

The supported algorithms are OSL and OSLa -- for details see [Huynh and Mooney (2011)](#references) and [Michelioudakis et. al. (2016)](#references).


## Types of structure learning in LoMRF

In order to perform structure learning in LoMRF the following definitions are required:

* Input theory file containing the predicate schema and optionally in the case of OSLa a function schema as well as a set of axioms, e.g., `theory.mln`.
* A directory of many training data files (micro-batches) containing both evidence and supervision, e.g., `/path/to/training/data/micro/batches/`.
* Output learned file, e.g., `output.mln`.
* A mode declaration file, e.g., `name.modes`
* The atomic signatures (identities) that define the non-evidence predicates ('-ne' option), that is the predicates for which training data contains supervision. Please note that for all non-evidence predicates LoMRF takes [Closed-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) and therefore the learning procedure assumes fully supervision is present. In case of missing annotations LoMRF would assume their truth values are *False*.
* In the case of `OSLa`, the atomic signatures that define the template atoms (`-template` option) present in the defined axioms of the input theory. If these template atoms are given then LoMRF will automatically switch to the `OSLa` algorithm, otherwise the standard `OSL` will run.

**Important note:** Structure learning in the current version of LoMRF does not support Functions and Dynamic functions. The given First-order logic theory must be function free.

### Structure learning using the `lomrf-slearn` command-line tool

To demonstrate the usage of LoMRF from command-line interface for structure learning, assume that we have one knowledge base file, named as `theory.mln`, and a set of training files, named as `training1.db`, `training2.db` etc, containing both the evidence and the supervision.

In our example knowledge-base we have the following predicates:

| Predicate Name | Number of arguments | Predicate identity | Description |
|:--------------:|:-------------------:|:------------------:|:------------|
| NonEvidence_A | 2 | `NonEvidence_A/2` | first non-evidence predicate
| NonEvidence_B | 2 | `NonEvidence_B/2` | second non-evidence predicate
| Template_A | 2 | `Template_A/2` | first template predicate
| Template_B | 2 | `Template_B/2` | second template predicate
| Ev_A | 1 | `EV_A/1` | first evidence predicate
| Ev_B | 1 | `EV_B/1` | second evidence predicate

As it is presented in the above table, there are two non-evidence predicates, `NonEvidence_A` and `NonEvidence_B`, where each one takes two terms as arguments. Therefore their atomic signatures are `NonEvidence_A/2` and `NonEvidence_B/2`. Similarly, there are two evidence predicates `Ev_A` and `Ev_B` that they take one term as argument. Therefore, the atomic signatures of `Ev_A` and `Ev_B` are `Ev_A/1` and `Ev_B/1`, respectively. As stated above, for all of them LoMRF assumes Closed-world assumption. Finally, we can optionally define template predicates. In the above table there are two template predicates `Template_A` and `Template_B` where each one takes two terms as arguments. Therefore their atomic signatures are `Template_A/2` and `Template_B/2`.

#### *OSL* learning

```lang-none
lomrf-slearn -i theory.mln -t /path/to/training/batches/ -o learned.mln -ne NonEvidence_A/2,NonEvidence_B/2
```

#### *OSLa* learning

```lang-none
lomrf-slearn -i theory.mln -t /path/to/training/batches/ -o learned.mln -ne NonEvidence_A/2,NonEvidence_B/2 -template Template_A/2,Template_B/2
```

The results of learning are stored in the `learned.mln` (see parameter `-o`)

## Structure Learning Examples

For a detailed structure learning tutorial in LoMRF, see Sections [Structure Learning Examples](4_1_structure_learning_examples.md). Sources from the examples are located in the LoMRF-data project (follow the instructions in [Download Example Data](6_2_download_example_data.md)).

## Command-line Interface Options ##

By executing the ```lomrf-slearn -h``` (or ```lomrf-slearn --help```) command from the command-line interface, we get a print of multiple parameters. Below we explain all LoMRF structure learning command-line interface parameters:

### Basic structure learning options

* `-i, --input <kb file>` **[Required]** Specify the input knowledge base file, that is the file that contains the predicate schema and optionally function schema and a set of axioms (see [Syntax](1_syntax.md) and [Quick Start](0_quick_start.md) for further information). You can specify either full or relative path or only the filename (when the file is in the current working path). For example, (1) full path `-i /full/path/to/theory.mln` in a Unix-based OS or `-i c:\full\path\to\theory.mln` in Windows, (2) relative path `-i path/to/theory.mln` in a Unix-based OS or `-i path\to\theory.mln` in Windows and (3) current working path `-i theory.mln`.

* `-o, --output <output file>` **[Required]** Specify the output file name to write the resulting weighted formulas learned by the structure learning. For example, `-o output.mln`. Similarly with the `-i` option, you can specify either full or relative path or only the filename (when the file is in the current working path).

* `-t, --training <training file | directory>` **[Required]** Specify the input directory of micro-batches (see [Syntax](1_syntax.md) and [Quick Start](0_quick_start) for further information). Similarly with the `-i` option, you can specify either full or relative path or only the filename (when the file is in the current working path).

* `-ne, --non-evidence atoms <string>` **[Required]** Specify the atomic signatures or identities (i.e., predicate_name/arity) of predicates for which supervision exists. For example, the non-evidence predicate `Male(person)` has name `Male` and arity 1 (single argument), therefore we should give the argument `-ne Male/1`. Multiple non-evidence atoms are allowed in LoMRF and they are defined as comma-separated identities without white-spaces. For example, `-ne Male/1,Female/1`. Please note that LoMRF takes [Closed-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) for all non-evidence atoms and this cannot be overridden.

* `-template, --template-atoms <string>` **[Optional]** Specify the atomic signatures or identities (i.e., predicate_name/arity) of template predicates. For example, the template predicate `InitiatedAt(fluent, time)` has name `InitiatedAt` and arity 2 (single argument), therefore we should give the argument `-template InitiatedAt/2`. Multiple template atoms are allowed in LoMRF and they are defined as comma-separated identities without white-spaces. In case template predicates are given LoMRF will switch to OSLa learner.

### Advanced structure learning options

* `-maxLength, --max-length <value>` **[Optional]** Specify the maximum length of literals for each discovered clause during search. If function schema are given in the input knowledge base file the maximum length should also consider functions as predicates. For example lets say we have defined an evidence predicate `Evidence_A(FooReturnValue)` and a function `FooReturnValue foo(Time)`. That means that the predicate `Evidence_A` has an argument whose domain is the domain of the return type of function `foo` and therefore we would like to learn clauses having literals of the form `Evidence_A(foo(t))`. In this case the maximum length should be set not to `1` but to `2` because LoMRF will translate the function into an auxiliary predicate `AUXfoo(FooReturnValue, Time)` and search for clauses of the form `Evidence_A(FooReturnValue) ^ AUXfoo(FooReturnValue, Time)`. Then after the search has been terminated the conjunction will be translated back into the simpler form `Evidence_A(foo(t))`.

* `-allowFreeVariables, --allow-free-variables` **[Optional]** Enable free variables into the learned clauses. For example the clause `Evidence_A(x, y) ^ Evidence_B(x, z)` has two free variables (`y`,`z`) because these variables does not appear in any other predicate in the conjunction. In case this option is not specified these kind of clauses will be omitted.

* `-threshold, --threshold <value>` **[Optional]** Specify the evaluation threshold (default is 1). The larger the evaluation threshold the stricter is the evaluator and less clauses will be retained into the resulting set of structures.

* `-theta, --tolerance-theta <value>` **[Optional]** Specify the theta threshold (default is 0.0). This threshold prunes clauses that their absolute weight value is below this threshold. The default value retains all learned clauses.

* `-clauseType --clause-type <horn | conjunction | both>` **[Optional]** Specify the type of clauses to be learned by the `OSL` method. `OSLa` learns clauses independent of this option based on its own strategy.

* `-ilpSolver --ilp-solver <lpsolve | ojalgo | gurobi>` **[Optional]** Specify which solver to use. We can choose between the open-source solvers [LPSolve](http://lpsolve.sourceforge.net/5.5/) and [ojAlgo](http://ojalgo.org/), as well as the commercial solver [Gurobi](http://www.gurobi.com/). By default LoMRF uses the open-source solver LPSolve.

* `-initialWeight, --initial-weight <value>` **[Optional]** Specify the initial weight value for new clauses.

* `-lossAugmented --loss-augmented` **[Optional]** Enables loss augmented inference (also known as seperation oracle) using the Hamming loss function by adding to the objective function during inference additional loss terms.

* `-lambda, --lambda <value>` **[Optional]** Regularization parameter for the AdaGrad online learner (default is 0.01). It defines how much you want to avoid misclassification.

* `-eta --eta <value>` **[Optional]** Learning rate value for the AdaGrad online learner (default is 1.0).

* `-delta --delta <value>` **[Optional]** Delta parameter of the AdaGrad online learner (default is 1.0).

* `-printLearnedWeightsPerIteration --print-learned-weights-per-iteration` **[Optional]** Print the learned weight in each iteration of weight learning. Iterations for online algorithms are the sequential steps processing a different micro-batch.

* `-noNegWeights, --eliminate-negative-weights` **[Optional]** When it is enabled, LoMRF eliminates negative
weight values from (ground) clauses. Specifically, the sign of negative weights in clauses is inverted, as well as all disjunctions become conjunctions (due to de Morgan's law). For example, using the de Morgan's law, the weighted clause `-2 A(x) v B(x)` is equivalent to `-2 !(!A(x) ^ !B(x))`. In MLN this is also equivalent to `2 !A(x) ^ !B(x)`, which produces the following two unit clauses: `1 A(x)` and `1 B(x)`.

* `-noNegatedUnit, --eliminate-negated-unit` **[Optional]** When it is enabled, unit clauses with negative literal become unit clauses with positive literal and inverted sign in their corresponding weight.

## References

* Tuyen N. Huynh and Raymond J. Mooney. (2011) Online Structure Learning for Markov Logic Networks (2011). In Proceedings of the European Conference on Machine Learning and Principles and Practice of Knowledge Discovery in Databases (ECML-PKDD 2011), Vol. 2, pp. 81-96. ([link](http://www.cs.utexas.edu/users/ai-lab/?huynh:ecml11))

* Michelioudakis E., Skarlatidis A., Paliouras G. and Artikis A. (2016) OSLa: Online Structure Learning using Background Knowledge Axiomatization. In Proceedings of the European Conference on Machine Learning and Principles and Practice of Knowledge Discovery in Databases (ECML-PKDD 2016).
