# Weight Learning

The weights of the soft-constrained clauses in Markov Logic Networks (MLNs) can be estimated from training data using supervised learning techniques. The goal is to learn a model (i.e., weighted clauses) by estimating the weight values from training examples. Since the learning paradigm is supervised, the trainin examples is a collection of input ground atoms (i.e., input evidence) and their corresponding output query atoms (i.e., annotation). The learned model should be capable of inferring as *correct as possible* the marginal probabilities or the maximum a-posteriory estimation of query atoms, given an input of ground evidence facts.

There are several weight estimation methods in the literature, the majority of them are batch learning methods that optimise the conditional log likelihood of the Markov Logic model -- for example see the works of [Singla and Domingos (2005)](8_references.md) and [Lowd and Domingos (2007)](8_references.md). LoMRF implements batch and on-line weight learning methods that optimize the max-margin, that is the ratio between the probability of correct truth assignment of query atoms (taken by the training data) and the closest competing incorrect truth assignment (estimated by the trained model) -- for details see the works of [Huynh and Mooney, (2009, 2011)](8_references.md) and [Duchi et al. (2011)](8_references.md). Max-margin training is better suited to problems where the goal is to maximise the classification accuracy.

## Weight learning in LoMRF

In order to perform weight learning in LoMRF the following definitions are required:

  * *The weight learning method*. LoMRF supports both batch and online weight learning procedures:
    1. *Max-Margin* learner: It processes all training data at once (**batch learning**) and estimates the weights of a given set of formulas. In the presence of new data the procedure must be repeated.
    2. *CDA* or *AdaGrad* learner: They can process the training data in micro-batches (**online learning**) and update the weights of a given set of formulas at each step of the online procedure.
  * Input theory file containing the formulas to be trained, e.g., `theory.mln`.
  * Single file of input training data (containing both evidence and supervision) or a directory of many training data micro-batches, e.g., a signle file `training.db` or a collection of training files that are in the directory `/path/to/training/data/micro/batches/`.
  * Output filename for the resulting learned MLN file, e.g., `output.mln` or with a path prefix `/path/to/output.mln`.
  * The atomic signatures (identities) that define the non-evidence predicates (specified by the '-ne' option), that is the predicates for which training data contains supervision. For example `-ne Foo/2,Bar/3`. Please note that for all non-evidence predicates LoMRF takes [Closed-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) and therefore the learning procedure assumes fully supervision is present. In case of **missing annotations** LoMRF would assume that their truth values are **False**.

### Weight learning using the `lomrf-wlearn` command-line tool

To demonstrate the usage of LoMRF from command-line interface for weight learning, assume that we have one knowledge base file, named as `theory.mln`, and one training file, named as `training.db` containing both the evidence and the supervision. Please note that the supervision is a collection of the expected ground atoms of the non-evidence atoms (i.e., query atoms).

In our example knowledge-base we have the following predicates:

| Predicate Name | Number of arguments | Predicate identity | Description |
|:--------------:|:-------------------:|:------------------:|:------------|
| NonEvidence_A | 2 | `NonEvidence_A/2` | first non-evidence predicate
| NonEvidence_B | 2 | `NonEvidence_B/2` | second non-evidence predicate
| Ev_A | 1 | `EV_A/1` | first evidence predicate
| Ev_B | 1 | `EV_B/1` | second evidence predicate

As it is presented in the above table, there are two non-evidence predicates, `NonEvidence_A` and `NonEvidence_B`, where each one takes two terms as arguments. Therefore their atomic signatures are `NonEvidence_A/2` and `NonEvidence_B/2`. Similarly, there are two evidence predicates `Ev_A` and `Ev_B` that they take one term as argument. Therefore, the atomic signatures of `Ev_A` and `Ev_B` are `Ev_A/1` and `Ev_B/1`, respectively. As stated above, for all of them LoMRF assumes Closed-world assumption.

## Batch Max-Margin learning

Max-margin learning considers all training data at once in order to estimate the weights of the given formulas defined in the input theory file.

```lang-none
lomrf-wlearn -alg MAX_MARGIN -i theory.mln -t training.db -o learned.mln -ne NonEvidence_A/2,NonEvidence_B/2
```
Where the parameter '-alg MAX_MARGIN' defines that we are using max-margin training method. The paramter '-i theory.mln' is the input theory that contains all MLN logic formulas (both soft-constrained and hard-constrained). The method will try to estimate the weights of all resulting soft-constrained clauses of the MLN logic formulas. All hard-constrained clauses will remain the same. Parameter '-t training.db' is the input training file, that is the file that contains all ground predicates (both evidence and non-evidence). The parameter '-o learned.mln' specifies the resulting output file. Finally, the parameter '-ne NonEvidence_A/2,NonEvidence_B/2' defines which predicates are non-evidence (thus will be the query atoms).

**Important Note:** Max-margin learner requires the proprietary [Gurobi](http://www.gurobi.com/) solver installed on your system, because the learning algorithm needs to solve a [Quadratic Optimization problem](https://en.wikipedia.org/wiki/Quadratic_programming). In case the solver is not present the learning procedure will terminated and notify you to install the software.

## Online weight learning (CDA or AdaGrad)

Suppose that we have multiple training micro-batches of training data named as `training1.db`, `training2.db`, etc. Then we can run an online weight learner to estimate the weights of the formulas defined in the input theory file, given a path of input training files.

```lang-none
lomrf-wlearn -alg CDA -i theory.mln -t /path/to/training/batches/ -o learned.mln -ne NonEvidence_A/2,NonEvidence_B/2
```
or
```lang-none
lomrf-wlearn -alg ADAGRAD -i theory.mln -t /path/to/training/batches/ -o learned.mln -ne NonEvidence_A/2,NonEvidence_B/2
```
Where the parameter '-alg' defines that we are using either 'CDA' or 'ADAGRAD' training method. The paramter '-i theory.mln' is the input theory that contains all MLN logic formulas (both soft-constrained and hard-constrained). The method will try to estimate the weights of all resulting soft-constrained clauses of the MLN logic formulas. All hard-constrained clauses will remain the same. Parameter '-t /path/to/training/batches/' is the path to the directory that contains all training files (*.db), where each one contains ground predicates (both evidence and non-evidence) that belong to the same micro-batch. The parameter '-o learned.mln' specifies the resulting output file. Finally, the parameter '-ne NonEvidence_A/2,NonEvidence_B/2' defines which predicates are non-evidence (thus will be the query atoms).

## Weight Leaning Examples

For a detailed weight learning tutorial in LoMRF, see Sections [Weight Learning Examples](3_1_weight_learning_examples.md) and [Temporal Weight Learning Examples](3_2_temporal_weight_learning_examples.md). Sources from the examples are located in the [LoMRF-data](https://github.com/anskarl/LoMRF-data) project (follow the instructions in [Download Example Data](6_2_download_example_data.md)).

## Command-line Interface Options for Weight Learning

By executing the ```lomrf-wlearn -h``` (or ```lomrf-wlearn --help```) command from the command-line interface, we get a print of multiple parameters. Below we explain all LoMRF weight learning command-line interface parameters:

### Basic weight learning options

* `-i, --input <kb file>` **[Required]** Specify the input knowledge base file, that is the file that contains the theory (see [Syntax](1_syntax.md) and [Quick Start](0_quick_start) for further information). You can specify either full or relative path or only the filename (when the file is in the current working path). For example, (1) full path `-i /full/path/to/theory.mln` in a Unix-based OS or `-i c:\full\path\to\theory.mln` in Windows, (2) relative path `-i path/to/theory.mln` in a Unix-based OS or `-i path\to\theory.mln` in Windows and (3) current working path `-i theory.mln`.

* `-o, --output <output file>` **[Required]** Specify the output file name to write the resulting weighed formulas learned by the weight learning. For example, `-o output.mln`. Similarly with the `-i` option, you can specify either full or relative path or only the filename (when the file is in the current working path).

* `-t, --training <training file | directory>` **[Required]** Specify the input training file or directory of micro-batches (see [Syntax](1_syntax.md) and [Quick Start](0_quick_start) for further information). Similarly with the `-i` option, you can specify either full or relative path or only the filename (when the file is in the current working path).

* `-ne, --non-evidence atoms <string>` **[Required]** Specify the atomic signatures or identities (i.e., predicate_name/arity) of predicates for which supervision exists. For example, the non-evidence predicate `Male(person)` has name `Male` and arity 1 (single argument), therefore we should give the argument `-ne Male/1`. Multiple non-evidence atoms are allowed in LoMRF and they are defined as comma-separated identities without white-spaces. For example, `-ne Male/1,Female/1`. Please note that LoMRF takes [Closed-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) for all non-evidence atoms and this cannot be overridden.

* `-alg, --algorithm <MAX_MARGIN | CDA | ADAGRAD>` **[Optional]** Specify the weight learning algorithm for estimating the weights of the given formulas present in the input knowledge base. In case this option is omitted the Max-Margin will be employed by default.

### Advanced weight learning options

* `-sigma --sigma <value>` **[Optional]** Parameter controlling strong convexity in CDA online learning (default is 1.0). Essentially this parameter defines the number of aggressive updates (in terms of learning steps).

* `-lambda, --lambda <value>` **[Optional]** Regularization parameter for the AdaGrad online learner (default is 0.01). It defines how much you want to avoid misclassification.

* `-eta --eta <value>` **[Optional]** Learning rate value for the AdaGrad online learner (default is 1.0).

* `-delta --delta <value>` **[Optional]** Delta parameter of the AdaGrad online learner (default is 1.0).

* `-C --C <value>` **[Optional]** Regularization parameter for Max-Margin learning (default is 1000). It defines how much you want to avoid misclassification. For large values of C, the optimization will choose a smaller-margin hyperplane if that hyperplane does a better job of getting all the training points classified correctly. Conversely, a very small value of C will cause the optimizer to look for a larger-margin separating hyperplane, even if that hyperplane misclassifies more points. For very tiny values of C, you should get misclassified examples, often even if your training data is linearly separable.

* `-epsilon --epsilon <value>` **[Optional]** Specify a stopping criterion for Max-Margin learning (default is 0.001). Roughly speaking this value controls the relative error of the resulting solution.

* `-lossScale --loss-scale <value>` **[Optional]** Specify a scaling constant used for scaling the loss function output computed in each learning iteration (default is 1). This option allows for overestimate or underestimate the loss.

* `-iterations --maximum-iterations <value>` **[Optional]** Specify the maximum number of iterations to run Max-Margin learning (default is 1000). Note that Max-Margin can terminate earlier than the specified iterations if convergence is achieved through a second criterion specified by the parameter `-epsilon`.

* `-addUnitClauses --add-unit-clauses` **[Optional]** Appends to the given set of formulas one unit clause for each predicate present in the given knowledge base in order to learn a weight for each of them. The weights of the unit clauses can (roughly speaking) capture the marginal distribution of the corresponding predicate, leaving the weights of the non-unit clauses free to model only dependencies between predicates.

* `-L1Regularization --L1-regularization` **[Optional]** Run Max-Margin using *L1* regularization instead of *L2* which is the default setting.

* `-printLearnedWeightsPerIteration --print-learned-weights-per-iteration` **[Optional]** Print the learned weight in each iteration of weight learning. Iterations for online algorithms are the sequential steps processing a different micro-batch.

* `-ilpSolver --ilp-solver <lpsolve | ojalgo | gurobi>` **[Optional]** Specify which solver to use. We can choose between the open-source solvers [LPSolve](http://lpsolve.sourceforge.net/5.5/) and [ojAlgo](http://ojalgo.org/), as well as the commercial solver [Gurobi](http://www.gurobi.com/). By default LoMRF uses the open-source solver LPSolve.

* `-lossAugmented --loss-augmented` **[Optional]** Enables loss augmented inference (also known as seperation oracle) using the Hamming loss function by adding to the objective function during inference additional loss terms.

* `-nonMarginRescaling --non-margin-rescaling` **[Optional]** Do not rescale the margin by the loss during Max-Margin learning.

* `-dynamic, --dynamic-implementations <string>` **[Optional]** Comma separated paths to search recursively for dynamic predicates/functions implementations (\*.class and \*.jar files).

* `-noNegWeights, --eliminate-negative-weights` **[Optional]** When it is enabled, LoMRF eliminates negative weight values from (ground) clauses. Specifically, the sign of negative weights in clauses is inverted, as well as all disjunctions become conjunctions (due to de Morgan's law). For example, using the de Morgan's law, the weighted clause `-2 A(x) v B(x)` is equivalent to `-2 !(!A(x) ^ !B(x))`. In MLN this is also equivalent to `2 !A(x) ^ !B(x)`, which produces the following two unit clauses: `1 A(x)` and `1 B(x)`.

* `-noNegatedUnit, --eliminate-negated-unit` **[Optional]** When it is enabled, unit clauses with negative literal become unit clauses with positive literal and inverted sign in their corresponding weight.