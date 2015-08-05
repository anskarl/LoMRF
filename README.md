# LoMRF: Logical Markov Random Fields

LoMRF is an open-source implementation of Markov Logic Networks (MLN) written in [Scala programming language](http://scala-lang.org).

## License 

LoMRF comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; See the [GNU Lesser General Public License v3 for more details](http://www.gnu.org/licenses/lgpl-3.0.html).

## Features overview:

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

## Building

See [Building and Linking](doc/5_building_and_linking.md).

## Documentation
  - [Quick-start guide](doc/0_quick_start.md)
  - [Inference](doc/1_inference.md)
  - [Weight Learning](doc/2_weight_learning.md)
  - [Structure Learning](doc/3_structure_learning.md)
  - [CLI Tools](doc/4_tools.md)
  - [Building and Linking](doc/5_building_and_linking.md)
  - [Configuration](doc/5_building_and_linking.md)
  - [References](doc/7_references.md)

## The development of LoMRF is powered by:

[![Java profiler](http://www.ej-technologies.com/images/product_banners/jprofiler_large.png)](http://www.ej-technologies.com/products/jprofiler/overview.html)

[![Intellij IDEA](https://www.jetbrains.com/idea/docs/logo_intellij_idea.png)](https://www.jetbrains.com/idea/)


