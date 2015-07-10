## LoMRF: Logical Markov Random Fields

LoMRF is an open-source library for Markov Logic Networks (MLN) written in [Scala programming language](http://scala-lang.org).

## Licence 

LoMRF comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; See the [GNU Lesser General Public License v3 for more details](http://www.gnu.org/licenses/lgpl-3.0.html).

#### Features overview:

1. Parallel grounding algorithm based on [Akka actors library](http://akka.io/).
2. Marginal (MC-SAT) and MAP (MaxWalkSAT and LP-relaxed Integer Linear Programming) inference.
3. Batch and online weight learning (max-margin and ADAGRAD) (lomrf-wlearn).
4. MLN knowledge base compilation (mlnc):
  * Predicate completion.
  * Knowledge base simplification.
  * Clausal form transformation.
  * Replacement of functions with utility predicates and vice versa.
  * Reads and produces Alchemy compatible MLN files.
5. Can export ground MRF in various formats (mrfwriter).
6. Can compare MLN theories (mlndiff).  


## Instrunctions to build LoMRF from source

In order to build LoMRF from source, you need to have Java SE Development Kit (e.g., OpenJDK) version 8 or higher and [sbt](http://www.scala-sbt.org/) (v0.13.x) installed in your system. Furthermore, LoMRF build depends on the [auxlib](https://github.com/anskarl/auxlib) (v0.1-SNAPSHOT), as well as to [Optimus](https://github.com/vagm/Optimus) and (optionally) [Gurobi](http://www.gurobi.com/).

Step 1. Clone and publish locally the auxlib project:

```
$ git clone https://github.com/anskarl/auxlib.git
$ cd auxlib
$ sbt ++2.11.7 publishLocal
```

Step 2. Clone and publish locally the Optimus project (see instructions [here](https://github.com/vagm/Optimus)).

Step 3 (optional). Please note that Gurobi is required for max-margin weight learning and optionally can also be used for MAP inference. In order to enable support for Gurobi, you have to build Optimus with Gurobi support ([see instructions](https://github.com/vagm/Optimus)) and then include Gurobi library dependencies to the `./lib` subdirectory inside the cloned to LoMRF directory (create if not already exists), as it is illustrated in the tree below:

```
LoMRF
|--lib/
    |-- gurobi.jar
```

Step 4. Build the LoMRF distribution, type the following command:

```
$ sbt dist
```

After a successful compilation, the LoMRF distribution is located inside the `./target/universal/lomrf-<version>.zip` file. You can extract this file and add the `path/to/lomrf-<version>/bin` in your PATH, in order to execute the LoMRF scripts from terminal. The distribution contains all library dependencies and requires only a Java 8 (or higher runtime). Sources, documentation and the compiled library (without dependencies) are archived as jar files into the `./target/scala-2.11/` directory.

The resulting documentation is located inside the `./target/site/scaladocs` directory.

##### LoMRF command line tools:

* mlnc: Compiles MLN files (predicate completion, CNF, etc.; write `mlnc -h` for help).
* mrfwriter: Exports ground MRF into various formats (MLN, DIMACS and libDAI Factor graph; write `mrfwriter -h` for help).
* lomrf: Performs probabilistic inference (see `lomrf -h` for help).
* lomrf-wlearn: Performs weight learning (see `lomrf-wlearn -h` for help)
* mlndiff: Displays differences between MLN files. The theories are compated only in [CNF](http://en.wikipedia.org/wiki/Conjunctive_normal_form) form.


##### Memory configuration

Depending your requirements you may want to adjust the heap memory parameters of the Java VM. You can edit `inc.env.sh` bash file (located inside the `bin` directory) and change the `-Xms` and `-Xmx` parameters (for details write `java -X` in the command line).

##### The development of LoMRF is powered by:

[![Java profiler](http://www.ej-technologies.com/images/product_banners/jprofiler_large.png)](http://www.ej-technologies.com/products/jprofiler/overview.html)

[![Intellij IDEA](https://www.jetbrains.com/idea/docs/logo_intellij_idea.png)](https://www.jetbrains.com/idea/)


##### References

Many of algorithms implemented in LoMRF are based on the following publications:


* Domingos, P., & Lowd, D. (2009). Markov Logic: An Interface Layer for Artificial Intelligence. Synthesis Lectures on Artiﬁcial Intelligence and Machine Learning. Morgan & Claypool Publishers.

* Duchi, J., Hazan, E., Singer, Y. (2011). Adaptive Subgradient Methods for Online Learning and Stochastic Optimization. The Journal of Machine Learning Research, Vol. 12, pp. 2121--2159.

* Kautz, H., Selman, B., & Jiang, Y. (1997). A General Stochastic Approach to Solving Problems with Hard and Soft Constraints. In Gu, D., Du, J., & Pardalos, P. (Eds.), The Satisﬁability Problem: Theory and Applications, Vol. 35 of DIMACS Series in Discrete Mathematics and Theoretical Computer Science, pp. 573-586. AMS

* Lifschitz, V. (1994). Circumscription. In Handbook of logic in Artificial Intelligence and Logic Programming, Vol. 3, pp. 297-352.

* McCarthy, J. (1980). Circumscription - A Form of Non-Monotonic Reasoning. Artificial Intelligence, 13 (1-2), 27-39.

* Nonnengart, A., & Weidenbach, C. (2001). Computing Small Clause Normal Forms. In Robinson, J. A., & Voronkov, A. (Eds.), Handbook of Automated Reasoning, pp. 335-367. Elsevier and MIT Press.

* Poon, H., & Domingos, P. (2006). Sound and Efficient Inference with Probabilistic and Deterministic Dependencies. In Proceedings of the 21st AAAI Conference on Artificial Intelligence, pp. 458-463. AAAI Press.

* Shavlik, J. W., & Natarajan, S. (2009). Speeding up inference in Markov Logic Networks by preprocessing to reduce the size of the resulting grounded network. In Proceedings of the 21st International Joint Conference on Artificial Intelligence (IJCAI), pp. 1951-1956.

* Skarlatidis A., Paliouras G., Artikis A. and Vouros G. (2015) Probabilistic Event Calculus for Event Recognition. ACM Transactions on Computational Logic, 16, 2, Article 11, pp. 11:1-11:37.

* Tuyen N. Huynh and Raymond J. Mooney. (2011). Max-Margin Weight Learning for Markov Logic Networks. In Proceedings of the European Conference on Machine Learning and Principles and Practice of Knowledge Discovery in Databases (ECML-PKDD 2011), Vol. 2, pp. 81-96.

* Tuyen N. Huynh and Raymond J. Mooney. (2011). Online Max-Margin Weight Learning for Markov Logic Networks. In Proceedings of the Eleventh SIAM International Conference on Data Mining (SDM11).

