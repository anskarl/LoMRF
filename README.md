# LoMRF: Logical Markov Random Fields

LoMRF is an open-source implementation of [Markov Logic Networks](https://en.wikipedia.org/wiki/Markov_logic_network) (MLNs) written in [Scala programming language](http://scala-lang.org).

## Features overview:

1. Parallel grounding algorithm based on [Akka Actors library](http://akka.io).
2. Marginal (MC-SAT) and MAP (MaxWalkSAT and LP-relaxed Integer Linear Programming) inference.
3. Batch and online weight learning (max-margin and ADAGRAD) (lomrf-wlearn).
4. MLN knowledge base compilation (mlnc):
  * Predicate completion.
  * Clausal form transformation.
  * Replacement of functions with utility predicates and vice versa.
  * Reads and produces [Alchemy](http://alchemy.cs.washington.edu/alchemy1.html) compatible MLN files.
5. Can export ground MRF in various formats (mrfwriter).
6. Can compare MLN theories (mlndiff).  

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
  - [Structure Learning](doc/4_structure_learning.md)
  - [CLI Tools](doc/5_tools.md)
  - [Build and Test LoMRF](doc/6_build_test_lomrf.md)
    - [Build and Link](doc/6_1_build_and_link.md)
    - [Download example data](doc/6_2_download_example_data.md)
  - [Configuration](doc/7_configuration.md)
  - [References](doc/8_references.md)

## The development of LoMRF is powered by:

[![Java profiler](http://www.ej-technologies.com/images/product_banners/jprofiler_large.png)](http://www.ej-technologies.com/products/jprofiler/overview.html)

[![Intellij IDEA](https://www.jetbrains.com/img/logos/logo_intellij_idea.png)](https://www.jetbrains.com/idea/)
