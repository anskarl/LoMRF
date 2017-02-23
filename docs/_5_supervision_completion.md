# Supervision Completion #

LoMRF employs online supervision completion in order to complete potentially missing labels in a sequence of training files
(micro-batches). Then these *completed* training files can be used as an input into any learning algorithm provided by the LoMRF
CLIs. The main benefit of the on-line approach is that it can scale to problems with large amount of data.

**Note**: Supervision completion is a new feature and its currently experimental.

## Types of supervision completion in LoMRF

In order to perform supervision completion in LoMRF the following definitions are required:

* Input theory file containing the predicate schema and a function schema if any exists, e.g., `schema.mln`.
* A directory of many training data files (micro-batches) containing evidence and partial supervision, e.g., `/path/to/training/data/micro/batches/`.
* A mode declaration file, e.g., `name.modes`
* The atomic signatures (identities) that define the non-evidence predicates ('-ne' option), that is the predicates
  for which training data contains supervision.

### Supervision completion using the `lomrf supervision` command-line tool

To demonstrate the usage of LoMRF from command-line interface for supervision completion, assume that we have one knowledge
base file, named as `schema.mln` containing predicate and function schema, and a sequence of training files, named as
`training1.db`, `training2.db` etc, containing evidence and the partial supervision.

In our example, lets assume a knowledge-base having the following predicates:

| Predicate Name | Number of arguments | Predicate identity | Description |
|:--------------:|:-------------------:|:------------------:|:------------|
| NonEvidence_A | 2 | `NonEvidence_A/2` | first non-evidence predicate
| NonEvidence_B | 2 | `NonEvidence_B/2` | second non-evidence predicate
| Ev_A | 1 | `EV_A/1` | first evidence predicate
| Ev_B | 1 | `EV_B/1` | second evidence predicate

As it is presented in the above table, there are two non-evidence predicates, `NonEvidence_A` and `NonEvidence_B`, where
each one takes two terms as arguments. Therefore their atomic signatures are `NonEvidence_A/2` and `NonEvidence_B/2`.
Similarly, there are two evidence predicates `Ev_A` and `Ev_B` that they take one term as argument. Therefore, the atomic
signatures of `Ev_A` and `Ev_B` are `Ev_A/1` and `Ev_B/1`, respectively. 

#### supervision completion

```lang-none
lomrf supervision -i schema.mln -t /path/to/training/batches/ -ne NonEvidence_A/2,NonEvidence_B/2 -m schema.modes
```

The resulting *completed* micro-batches are stored in a folder having the name of the strategy used (by default will be kNN.2.something)

## Command-line Interface Options ##

By executing the ```lomrf supervision -h``` (or ```lomrf supervision --help```) command from the command-line interface,
we get a print of multiple parameters. Below we explain all LoMRF supervision completion command-line interface parameters:

### Basic supervision completion options

* `-i, --input <kb file>` **[Required]** Specify the input knowledge base file, that is the file that contains the
predicate schema and optionally function schema (see [Syntax](1_syntax.md) and [Quick Start](0_quick_start.md)
for further information). You can specify either full or relative path or only the filename (when the file is in the current working path).
For example, (1) full path `-i /full/path/to/theory.mln` in a Unix-based OS or `-i c:\full\path\to\theory.mln` in Windows,
(2) relative path `-i path/to/theory.mln` in a Unix-based OS or `-i path\to\theory.mln` in Windows and (3) current working
path `-i theory.mln`.

* `-t, --training <training file | directory>` **[Required]** Specify the input directory of the training micro-batches
(see [Syntax](1_syntax.md) and [Quick Start](0_quick_start) for further information). Similarly with the `-i` option, you
 can specify either full or relative path or only the filename (when the file is in the current working path).

* `-a, --annotation <annotation file | directory>` **[Optional]** Specify the input directory of annotation files for
calculating statistics on the performance of the online supervision completion. Similarly with the `-i` option,
you can specify either full or relative path or only the filename (when the file is in the current working path).

* `-r, --result <result file>` **[Optional]** Specify the output file name to write statistics about supervision completion.
For example, `-r output.result`. Similarly with the `-i` option, you can specify either full or relative path or only
the filename (when the file is in the current working path).

* `-ne, --non-evidence atoms <string>` **[Required]** Specify the atomic signatures or identities (i.e., predicate_name/arity)
of predicates for which supervision should be completed. For example, the non-evidence predicate `Male(person)` has name `Male`
and arity 1 (single argument), therefore we should give the argument `-ne Male/1`. Multiple non-evidence atoms are allowed in
LoMRF and they are defined as comma-separated identities without white-spaces. For example, `-ne Male/1,Female/1`.

* `-m, --modes <mode file>` **[Required]** Specify the input mode declaration file, that is the file that contains the
predicate and functions modes (see [OSL Examples](4_1_structure_learning_examples.md) for further information). Similarly
with the `-i` option, you can specify either full or relative path or only the filename
(when the file is in the current working path).

* `-c --connector <kNN | eNN>` **[Optional]** "Specify a connection heuristic for the graph (default is kNN).

* `-e --epsilon <value>` **[Optional]** Epsilon parameter for eNN connector (default is 0.75).

* `-k --kappa <value>` **[Optional]** Kappa parameter for the kNN connector (default is 2).

* `-cache --cache-labels` **[Optional]** Cache labels for online supervision completion. We strongly recommend to enable