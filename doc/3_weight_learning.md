# Weight Learning #

## Types of weight learning in LoMRF

In order to perform weight learning in **LoMRF** the following definitions are required:

  * The weight learning method. LoMRF supports both batch and online weight learning procedures:
    1. *Max-Margin* learner: It processes all training data at once (batch learning) and estimates the weights of a given set of formulas. In the presence of new data the procedure must be repeated.
    2. *CDA* or *AdaGrad* learner: They can process the training data in micro-batches (online learning) and update the weights of a given set of formulas at each step of the online procedure.
  * Input theory file containing the formulas to be trained, e.g., `theory.mln`.
  * Single file of input training data (containing both evidence and supervision) or a directory of many training data micro-batches, e.g., `training.db` or `\path\to\training\data\micro\batches\`.
  * Output learned file, e.g., `output.mln`.
  * The atomic signatures (identities) that define the non-evidence predicates ('-ne' option), that is the predicates for which training data contains supervision. Please note that for all non-evidence predicates LoMRF takes [Closed-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption) and therefore the learning procedure assumes fully supervision is present. In case of missing annotations LoMRF would assume their truth values are *False*.

### Weight learning using the `lomrf-wlearn` commmand-line tool

To demonstrate the usage of LoMRF from command-line interface for weight learning, assume that we have one knowledge base file, named as `theory.mln`, and one training file, named as `training.db` containing both the evidence and the supervision.

In our example knowledge-base we have the following predicates:

| Predicate Name | Number of arguments | Predicate identity | Description |
|:--------------:|:-------------------:|:------------------:|:------------|
| NonEvidence_A | 2 | `NonEvidence_A/2` | first non-evidence predicate
| NonEvidence_B | 2 | `NonEvidence_B/2` | second non-evidence predicate
| Ev_A | 1 | `EV_A/1` | first evidence predicate
| Ev_B | 1 | `EV_B/1` | second evidence predicate

As it is presented in the above table, there are two non-evidence predicates, `NonEvidence_A` and `NonEvidence_B`, where each one takes two terms as arguments. Therefore their atomic signatures are `NonEvidence_A/2` and `NonEvidence_B/2`. Similarly, there are two evidence predicates `Ev_A` and `Ev_B` that they take one term as argument. Therefore, the atomic signatures of `Ev_A` and `Ev_B` are `Ev_A/1` and `Ev_B/1`, respectively. As stated above, for all of them LoMRF assumes Closed-world assumption.

## Batch *Max-Margin* learning

Max-margin learning considers all training data at once in order to estimate the weights of the given formulas defined in the input theory file.

```lang-none
lomrf-wlearn -alg MAX_MARGIN -i theory.mln -t training.db -o learned.mln -ne NonEvidence_A/2,NonEvidence_B/2
```

The results of learning are stored in the `learned.mln` (see parameter `-o`)

**Note:** Max-margin learner requires the proprietary *Gurobi* solver is installed on your system. That is because the algorithm needs to solve a quadratic optimization problem. In case the solver is not present the learning procedure will terminated and notify you to install the software.

## Online weight learning (*CDA* or *AdaGrad*)

Suppose that we have multiple training micro-batches of training data named as `training1.db`, `training2.db` etc. Then alternatively we can run an online weight learner to estimate the weights of the formulas defined in the input theory file.

```lang-none
lomrf-wlearn -alg CDA -i theory.mln -t /path/to/training/batches/ -o learned.mln -ne NonEvidence_A/2,NonEvidence_B/2
```
or
```lang-none
lomrf-wlearn -alg ADAGRAD -i theory.mln -t /path/to/training/batches/ -o learned.mln -ne NonEvidence_A/2,NonEvidence_B/2
```

The results of learning are stored in the `learned.mln` (see parameter `-o`)

## Command-line Interface Options ##

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

* `-noNegWeights, --eliminate-negative-weights` **[Optional]** When it is enabled, LoMRF eliminates negative
weight values from (ground) clauses. Specifically, the sign of negative weights in clauses is inverted, as well as all disjunctions become conjunctions (due to de Morgan's law). For example, using the de Morgan's law, the weighted clause `-2 A(x) v B(x)` is equivalent to `-2 !(!A(x) ^ !B(x))`. In MLN this is also equivalent to `2 !A(x) ^ !B(x)`, which produces the following two unit clauses: `1 A(x)` and `1 B(x)`.

* `-noNegatedUnit, --eliminate-negated-unit` **[Optional]** When it is enabled, unit clauses with negative
literal become unit clauses with positive literal and inverted sign in their corresponding weight.

## References

* Tuyen N. Huynh and Raymond J. Mooney. (2011). Max-Margin Weight Learning for Markov Logic Networks. In Proceedings of the European Conference on Machine Learning and Principles and Practice of Knowledge Discovery in Databases (ECML-PKDD 2011), Vol. 2, pp. 81-96. ([link](http://www.ai.sri.com/~huynh/papers/huynh_mooney_ecmlpkdd09.pdf))

* Tuyen N. Huynh and Raymond J. Mooney. (2011). Online Max-Margin Weight Learning for Markov Logic Networks. In Proceedings of the Eleventh SIAM International Conference on Data Mining (SDM11). ([link](http://www.ai.sri.com/~huynh/papers/huynh_mooney_sdm11.pdf))

* Duchi, J., Hazan, E., Singer, Y. (2011). Adaptive Subgradient Methods for Online Learning and Stochastic Optimization. The Journal of Machine Learning Research, Vol. 12, pp. 2121--2159. ([link](http://jmlr.org/papers/v12/duchi11a.html))
