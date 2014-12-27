## LoMRF: Logical Markov Random Fields

LoMRF is an experimental library for Markov Logic Networks (MLN) written in [Scala programming language](http://scala-lang.org).

## Licence 

This program comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; See the [GNU General Public License v3 for more details](http://www.gnu.org/licenses/gpl-3.0.html).

#### Features overview:

1. MLN knowledge base compilation (mlnc):
  * Predicate completion.
  * Knowledge base simplification.
  * Clausal form transformation.
  * Replacement of functions with utility predicates.
  * Reads and produces Alchemy compatible MLN files.
2. Support for marginal (MC-SAT) and MAP (via MaxWalkSAT and lp-relaxed Integer Linear Programming) inference.
3. Parallel grounding algorithm based on [Akka actors library](http://akka.io/).
4. Can export ground MRF in various formats (mrfwriter).

What is missing? Many features, but most important weight learning algorithms.


## Instrunctions to build LoMRF from source

In order to build LoMRF from source, you need to have Java 7 and [sbt](http://www.scala-sbt.org/) installed in your system. Furthermore, LoMRF build depends on the [auxlib](https://github.com/anskarl/auxlib) (v0.1-SNAPSHOT), as well as to [lp_solve](http://lpsolve.sourceforge.net), [Gurobi](http://www.gurobi.com/) and [OscaR](http://oscarlib.bitbucket.org/).

Step 1. To clone and publish localy the auxlib project, type the following commands:

```
$ git clone https://github.com/anskarl/auxlib.git
$ cd auxlib
$ sbt publishLocal
```

Step 2. Include lp_solve, Gurobi and OscaR library dependencies to `./lib`, as it is illustrated in the tree below:

```
.
|-- glpk-java.jar
|-- gurobi.jar
|-- lpsolve55j.jar
|-- native
|   `-- linux
|       |-- x86_32
|       `-- x86_64
|           |-- libGurobiJni60.so
|           |-- libglpk.so.0
|           |-- libglpk_java.so
|           |-- libgurobi.so.6.0.0
|           |-- libgurobi60.so -> ./libgurobi.so.6.0.0
|           |-- liblpsolve55.so
|           `-- liblpsolve55j.so
|-- oscar-algebra_2.10-1.0.0.jar
|-- oscar-algo_2.10-1.0.0.jar
|-- oscar-cbls_2.10-1.0.0.jar
|-- oscar-cp_2.10-1.0.0.jar
|-- oscar-des_2.10-1.0.0.jar
|-- oscar-dfo_2.10-1.0.0.jar
|-- oscar-invariants_2.10-1.0.0.jar
|-- oscar-linprog_2.10-1.0.0.jar
|-- oscar-util_2.10-1.0.0.jar
|-- oscar-visual_2.10-1.0.0.jar
`-- oscar_2.10-1.0.0.jar

```

Step 3. To start building the LoMRF distributon, type the following command:

```
$ sbt dist
```

After a successful compilation, the LoMRF distributon is located inside the `./target/universal/lomrf-<version>.zip` file. You can extract this file and add the `path/to/lomrf-<version>/bin` in your PATH, in order to execute the LoMRF scripts from terminal. The distributon contains all library dependencies and requires only a Java 7 (or higher runtime). Sources, documentation and the compiled library (without dependencies) are archived as jar files into the `./target/scala-2.10/` directory.

The resulting documentation is located inside the `./target/site/scaladocs` directory.

##### LoMRF command line tools:

* mlnc: Compiles MLN files (predicate completion, CNF, etc.; write `mlnc -h` for help).
* mrfwriter: Exports ground MRF into various formats (MLN, DIMACS and libDAI Factor graph; write `mrfwriter -h` for help).
* lomrf: Performs probabilistic inference (see `lomrf -h` for help).
* mlndiff: Displays differences between MLN files. The theories are compated only in [CNF](http://en.wikipedia.org/wiki/Conjunctive_normal_form) form.


##### Memory configuration

Depending your requirements you may want to adjust the heap memory parameters of the Java VM. You can edit `inc.env.sh` bash file (located inside the `bin` directory) and change the `-Xms` and `-Xmx` parameters (for details write `java -X` in the command line).


##### References

* Domingos, P., & Lowd, D. (2009). Markov Logic: An Interface Layer for Artificial Intelligence. Synthesis Lectures on Artiﬁcial Intelligence and Machine Learning. Morgan & Claypool Publishers.

* Kautz, H., Selman, B., & Jiang, Y. (1997). A General Stochastic Approach to Solving Problems with Hard and Soft Constraints. In Gu, D., Du, J., & Pardalos, P. (Eds.), The Satisﬁability Problem: Theory and Applications, Vol. 35 of DIMACS Series in Discrete Mathematics and Theoretical Computer Science, pp. 573-586. AMS

* Lifschitz, V. (1994). Circumscription. In Handbook of logic in Artificial Intelligence and Logic Programming, Vol. 3, pp. 297-352.

* McCarthy, J. (1980). Circumscription - A Form of Non-Monotonic Reasoning. Artificial Intelligence, 13 (1-2), 27-39.

* Nonnengart, A., & Weidenbach, C. (2001). Computing Small Clause Normal Forms. In Robinson, J. A., & Voronkov, A. (Eds.), Handbook of Automated Reasoning, pp. 335-367. Elsevier and MIT Press.

* Poon, H., & Domingos, P. (2006). Sound and Efficient Inference with Probabilistic and Deterministic Dependencies. In Proceedings of the 21st AAAI Conference on Artificial Intelligence, pp. 458-463. AAAI Press.

* Shavlik, J. W., & Natarajan, S. (2009). Speeding up inference in Markov Logic Networks by preprocessing to reduce the size of the resulting grounded network. In Proceedings of the 21st International Joint Conference on Artificial Intelligence (IJCAI), pp. 1951-1956.

* Skarlatidis A., Paliouras G., Artikis A. and Vouros G. Probabilistic Event Calculus for Event Recognition. ACM Transactions on Computational Logic, to appear.

* Tuyen N. Huynh and Raymond J. Mooney. Max-Margin Weight Learning for Markov Logic Networks. In Proceedings of the European Conference on Machine Learning and Principles and Practice of Knowledge Discovery in Databases (ECML-PKDD 2011), Vol. 2, pp. 81-96, 2011.


