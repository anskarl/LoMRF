# Download Example and Unit Testing Data

Optionally if you are interest to run examples or Unit tests, you need to pull the sources of the LoMRF-data git 
sub-module. In order to download the contents of LoMRF-data as a git sub-modules, you need to give the following 
commands in the root directory of the project (e.g., `/path/to/LoMRF`)

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
│   └── Inference
│       ├── Activity_Recognition
│       ├── Marcus_Caesar
│       ├── distributions
│       └── yale_shooting
└── Unit_Tests
    ├── DependencyMap
    ├── inference
    └── learning
```

All inference-related examples are located in `Data/Examples/Inference` (see [Inference](2_inference.md))

Similarly, all files that are used in Unit testing are located in `Data/Unit_Tests`. 