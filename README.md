<pre>
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

    Logical Markov Random Fields.
</pre>

# LoMRF: Logical Markov Random Fields

LoMRF is an open-source implementation of [Markov Logic Networks](https://en.wikipedia.org/wiki/Markov_logic_network) (MLNs) written in [Scala programming language](http://scala-lang.org).

## Features overview:

1. Parallel grounding algorithm based on [Akka Actors library](http://akka.io).
2. Marginal (MC-SAT) and MAP (MaxWalkSAT and LP-relaxed Integer Linear Programming) inference (**lomrf**).
3. Batch and on-line Weight Learning (Max-Margin, AdaGrad and CDA) (**lomrf-wlearn**).
4. On-line Structure Learning (OSL and OSLa) (**lomrf-slearn**).
5. MLN knowledge base compilation (**mlnc**):
  * Predicate completion.
  * Clausal form transformation.
  * Replacement of functions with utility predicates and vice versa.
  * Reads and produces [Alchemy](http://alchemy.cs.washington.edu/alchemy1.html) compatible MLN files.
6. Can export ground MRF in various formats (**mrfwriter**).
7. Can compare MLN theories (**mlndiff**).

## License

LoMRF comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it
under certain conditions; See the [GNU Lesser General Public License v3 for more details](http://www.gnu.org/licenses/lgpl-3.0.html).

## Reference in Scientific Publications
Please use the following BibTex entry when you cite LoMRF in your papers:
```
@misc{LoMRF,
	author = {Anastasios Skarlatidis},
	title = {Logical Markov Random Fields (LoMRF): an open-source implementation of Markov Logic Networks},
	url = {https://github.com/anskarl/LoMRF}
}
```

## Building

See [Build and Link LoMRF](doc/6_1_build_and_link_lomrf.md).

## Documentation
  - [Quick-start guide](doc/0_quick_start.md)
  - [Syntax](doc/1_syntax.md)
    - [Knowledge base](doc/1_1_knowledge_base.md)
    - [Evidence](doc/1_2_evidence.md)
  - [Inference](doc/2_inference.md)
    - [Probabilistic Inference Examples](doc/2_1_inference_examples.md)
    - [Temporal Probabilistic Inference Examples](doc/2_2_temporal_inference_examples.md)
  - [Weight Learning](doc/3_weight_learning.md)
    - [Weight Learning Examples](doc/3_1_weight_learning_examples.md)
    - [Temporal Weight Learning Examples](doc/3_2_temporal_weight_learning_examples.md)
  - [Structure Learning](doc/4_structure_learning.md)
    - [Structure Learning Examples](doc/4_1_structure_learning_examples.md)
  - [CLI Tools](doc/5_tools.md)
  - [Build from source](doc/6_build_test_lomrf.md)
    - [Build and Link](doc/6_1_build_and_link_lomrf.md)
    - [Download example data](doc/6_2_download_example_data.md)
  - [Configuration](doc/7_configuration.md)
  - [References](doc/8_references.md)

## The development of LoMRF is powered by:

[![Java profiler](http://www.ej-technologies.com/images/product_banners/jprofiler_large.png)](http://www.ej-technologies.com/products/jprofiler/overview.html)

[![Intellij IDEA](https://www.jetbrains.com/img/logos/logo_intellij_idea.png)](https://www.jetbrains.com/idea/)
