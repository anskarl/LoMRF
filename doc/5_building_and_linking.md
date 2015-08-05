# Building and Linking LoMRF

In order to build LoMRF from source, you need to have Java SE Development Kit (e.g., OpenJDK) version 7 or higher 
(however we strongly recommend use Java SE 8 or higher) and [SBT](http://www.scala-sbt.org/) (v0.13.x) installed in 
your system. Furthermore, LoMRF build depends on the [auxlib](https://github.com/anskarl/auxlib) (v0.1-SNAPSHOT), 
as well as to [Optimus](https://github.com/vagm/Optimus) and (optionally) [Gurobi](http://www.gurobi.com/). 

**To enable  optimisations, we strongly recommend use Java SE 8 or higher for the following steps, as well as for the 
use of LoMRF.**

## Instructions to build LoMRF from source

Step 1. Clone and publish locally the auxlib project:

```bash
$ git clone https://github.com/anskarl/auxlib.git
$ cd auxlib
$ sbt ++2.11.7 publishLocal
```

Step 2. Clone and publish locally the Optimus project (see instructions [here](https://github.com/vagm/Optimus)).

Step 3 (optional). Please note that Gurobi is required for max-margin weight learning and optionally can also be used for MAP inference. In order to enable support for Gurobi, you have to build Optimus with Gurobi support ([see instructions](https://github.com/vagm/Optimus)) and then include Gurobi library dependencies to the `./lib` subdirectory inside the cloned to LoMRF directory (create if not already exists), as it is illustrated in the tree below:

```bash
LoMRF
|--lib/
    |-- gurobi.jar
```

Step 4. Build the LoMRF distribution, type the following command:

```bash
$ sbt dist
```

After a successful compilation, the LoMRF distribution is located inside the `./target/universal/lomrf-<version>.zip` file. You can extract this file and add the `path/to/lomrf-<version>/bin` in your PATH, in order to execute the LoMRF scripts from terminal. The distribution contains all library dependencies and requires only a Java 8 (or higher runtime). Sources, documentation and the compiled library (without dependencies) are archived as jar files into the `./target/scala-2.11/` directory.

The resulting documentation is located inside the `./target/site/scaladocs` directory.


## Using LoMRF as a library

Follow the first 2 steps of Section 'Instructions to build LoMRF from source' and then publish locally the LoMRF:

```bash
$ sbt publishLocal
```

Thereafter, in order to link LoMRF (e.g., version 0.4) to your [SBT](http://www.scala-sbt.org/) project, add the 
following dependency:

```scala
libraryDependencies += "com.github.anskarl" %% "lomrf" % "0.4"
```

Similarly, in an [Apache Maven](https://maven.apache.org/) pom file:

```xml
<dependency>
    <groupId>com.github.anskarl</groupId>
    <artifactId>lomrf_2.11</artifactId>
    <version>0.4</version>
</dependency>
```