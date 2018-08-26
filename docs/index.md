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
2. Marginal (MC-SAT) and MAP (MaxWalkSAT and LP-relaxed Integer Linear Programming) inference (**lomrf infer**).
3. Batch and on-line Weight Learning (Max-Margin, AdaGrad and CDA) (**lomrf wlearn**).
4. On-line Structure Learning (OSL and OSLa) (**lomrf slearn**).
5. MLN knowledge base compilation (**lomrf compile**):
  * Predicate completion.
  * Clausal form transformation.
  * Replacement of functions with utility predicates and vice versa.
  * Reads and produces [Alchemy](http://alchemy.cs.washington.edu/alchemy1.html) compatible MLN files.
6. Can export ground MRF in various formats (**lomrf export**).
7. Can compare MLN theories (**lomrf diff**).
8. Online supervision completion on semi-supervised training sets [*currently experimental*] (**lomrf supervision**)


## Building

See [Build and Link LoMRF](7_1_build_and_link_lomrf.md).

## Documentation contents

  - [Quick-start guide](0_quick_start.md)
  - [Syntax](1_syntax.md)
    - [Knowledge base](1_1_knowledge_base.md)
    - [Evidence](1_2_evidence.md)
  - [Inference](2_inference.md)
    - [Probabilistic Inference Examples](2_1_inference_examples.md)
    - [Temporal Probabilistic Inference Examples](2_2_temporal_inference_examples.md)
  - [Weight Learning](3_weight_learning.md)
    - [Weight Learning Examples](3_1_weight_learning_examples.md)
    - [Temporal Weight Learning Examples](3_2_temporal_weight_learning_examples.md)
  - [Structure Learning](4_structure_learning.md)
    - [Structure Learning Examples](4_1_structure_learning_examples.md)
  - [Supervision Completion](5_supervision_completion.md)
  - [CLI Tools](6_tools.md)
  - [Build from source](7_build_test_lomrf.md)
    - [Build and Link](7_1_build_and_link_lomrf.md)
    - [Download example data](7_2_download_example_data.md)
  - [Configuration](8_configuration.md)
  - [References](9_references.md)


## License

Copyright 2014-2018 Anastasios Skarlatidis and Evangelos Michelioudakis

LoMRF is licensed under the Apache License, Version 2.0: [https://www.apache.org/licenses/LICENSE-2.0](https://www.apache.org/licenses/LICENSE-2.0)

## Reference in Scientific Publications

Please use the following BibTex entry when you cite LoMRF in your papers:
```
@misc{LoMRF,
	author = {Anastasios Skarlatidis and Evangelos Michelioudakis},
	title = {{Logical Markov Random Fields (LoMRF): an open-source implementation of Markov Logic Networks}},
	url = {https://github.com/anskarl/LoMRF},
	year = {2014}
}
```