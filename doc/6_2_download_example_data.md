# Download Example and Unit Testing Data

Optionally if you are interest to run examples or Unit tests, you need to pull the sources of the LoMRF-data git sub-module. In order to download the contents of LoMRF-data as a git sub-modules, you need to give the following commands in the root directory of the project (e.g., `/path/to/LoMRF`)

**Only for the first time after cloning LoMRF**
```bash
$ git submodule update --init
```

**Every time that you want get the latest version of LoMRF-data**
```bash
$ git submodule foreach --recursive git pull
```

LoMRF examples, as well as, files that are used by Unit testing are located in the sub-directory `Data` and it has the
following directory structure:

```
Data
├── Examples
│   ├── Inference
│   │   ├── Activity_Recognition
│   │   ├── Marcus_Caesar
│   │   ├── distributions
│   │   └── yale_shooting
│   ├── Structure_Learning
│   │   ├── OSL_NLP
│   │   └── OSLa_CAVIAR
│   └── Weight_Learning
│       ├── Activity_Recognition
│       ├── Car_Traffic
│       └── Friends_Smokers
└── Unit_Tests
    ├── DependencyMap
    ├── inference
    └── learning
```

**Example Files**

All inference-related examples are located in `Data/Examples/Inference` (see [Inference](2_inference.md)). All weight learning related examples are located in `Data/Examples/Weight_Learning` (see [Weight Learning](3_weight_learning.md)). Similarly, all structure learning related examples are located in `Data/Examples/Structure_Learning` (see [Structure Learning](4_structure_learning.md)).

Please note for space efficiency the following input data files for Weight Learning and Structure Learning are compressed:
```
Data/Examples/Structure_Learning/OSL_NLP/training.tar.bz2
Data/Examples/Structure_Learning/OSLa_CAVIAR/training.tar.bz2
Data/Examples/Weight_Learning/Activity_Recognition/training.tar.bz2
```

**Unit Testing Files**
All files that are used in Unit testing are located in `Data/Unit_Tests`.