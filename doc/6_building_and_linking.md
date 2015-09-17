# Building and Linking LoMRF

In order to build LoMRF from source, you need to have Java SE Development Kit (e.g., OpenJDK) version 7 or higher
(however we strongly recommend use Java SE 8 or higher) and [SBT](http://www.scala-sbt.org/) (v0.13.x) installed in
your system. Furthermore, LoMRF build depends on the [auxlib](https://github.com/anskarl/auxlib) (v0.1-SNAPSHOT),
as well as to [Optimus](https://github.com/vagm/Optimus) and (optionally) [Gurobi](http://www.gurobi.com/) and
[LPSolve](http://lpsolve.sourceforge.net).

**To enable  optimisations, we strongly recommend use Java SE 8 or higher for the following steps, as well as for the
use of LoMRF.**

## LPSolve installation instructions

### Linux distributions

For example, on a ***Debian-based*** distribution, write the following command:
```bash
$ sudo apt-get install lp-solve
```

### Apple MacOS X

Either download and install from the [LPSolve website](http://lpsolve.sourceforge.net)
or from your favorite package manager.

For example, from [macports](https://www.macports.org):
```bash
$ sudo port install lp_solve
```

or from [homebrew](http://brew.sh):
```bash
$ brew tap homebrew/science
$ brew install lp_solve
```

### Microsoft Windows
Download LPSolve v5.5.x with java bindings (*lp_solve_5.5.2.0_java.zip*) from the [LPSolve website](http://lpsolve.sourceforge.net)
and follow the installation instructions that are located inside the zip file (*README.html*)

## Gurobi installation instructions
Please follow the installation instructions from the [Gurobi website](http://www.gurobi.com).

## Instructions to build LoMRF from source

Step 1. Clone and publish locally the auxlib project:

```bash
$ git clone https://github.com/anskarl/auxlib.git
$ cd auxlib
$ sbt ++2.11.7 publishLocal
```

Step 2. Clone and publish locally the Optimus project (see instructions [here](https://github.com/vagm/Optimus)).

Step 3 (optional). Please note that Gurobi is required for running max-margin weight learning and optionally can also be used for MAP inference. In order to enable support for Gurobi, you have to build Optimus with Gurobi support ([see instructions](https://github.com/vagm/Optimus)) and then include Gurobi library dependencies to the `./lib` subdirectory inside the cloned LoMRF directory (create if not already exists), as it is illustrated in the tree below:

```bash
LoMRF
|--lib/
    |-- gurobi.jar
```

Step 4. To build the LoMRF distribution, type the following command:

```bash
$ sbt dist
```

After a successful compilation, the LoMRF distribution is located inside the `./target/universal/lomrf-<version>.zip`
file. You can extract this file and add the `path/to/lomrf-<version>/bin` in your PATH, in order to execute the LoMRF
scripts from terminal (see next Section). The distribution contains all library dependencies and requires only a Java 7
(or higher runtime). Sources, documentation and the compiled library (without dependencies) are archived as jar files
into the `./target/scala-2.11/` directory. The resulting documentation is located inside the `./target/site/scaladocs`
directory.

## Only for Microsoft Windows XP users

LoMRF depends on the JANSI library, in order to print colored log messages in the console. JANSI depends on a JNI library
that is available from "Microsoft Visual C++ 2008 SP1 Redistributable". You can get a free copy from Microsoft at the
following link: http://www.microsoft.com/en-us/download/details.aspx?displaylang=en&id=5582

## Add LoMRF executables in your default PATH

You can add all CLI tools to your default PATH, in order to directly call LoMRF tools from anywhere in the command line
interface. Depending on your OS configuration you have the add and export the path `/path/to/lomrf-<version>/bin` to
the `PATH` variable.

For example, lets say that the LoMRF distribution version 0.4 is being installed in your home directory in `$HOME/lomrf-0.4`,
the directory structure inside the LoMRF directory is the following:

```bash
lomrf-0.4/
|-- bin
|-- etc
|-- lib
```
###Linux, Unix and MacOS X

The `bin` sub-directory contains all LoMRF executable tools. In order to add this sub-directory in your default `PATH`
add the following line in you profile file.


**BASH** e.g., inside `.profile`, `.bashrc` or `.bash_profile` file in your home directory:
```bash
export PATH=$PATH:$HOME/lomrf-0.4/bin
```

**CSH/TCSH** e.g., inside `~/.login` file in your home directory:
```csh
set path = ($path $HOME/lomrf-0.4/bin .)
```
or in `~/.cshrc` file in your home directory:
```csh
setenv PATH $PATH:$HOME/lomrf-0.4/bin:.
```


###Microsoft Windows Operating Systems

For MS Windows OS add the location of LoMRF sub-folder (e.g., `C:\path\to\lomrf-0.4\bin`) in your PATH environmental variable.

**Windows 8:**
  1. Drag the Mouse pointer to the Right bottom corner of the screen
  2. Click on the Search icon and type: Control Panel
  3. Click on -> Control Panel -> System -> Advanced
  4. Click on Environment Variables, under System Variables, find PATH, and click on it.
  5. In the Edit windows, modify PATH by adding the location of the LoMRF bin sub-folder (e.g., `C:\path\to\lomrf-0.4\bin`)
  to the value for PATH. If you do not have the item PATH, you may select to add a new variable and add PATH as the name
  and the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.4\bin`) as the value.
  6. Close the window.
  7. Reopen Command prompt window, and run your LoMRF experiments.

**Windows 7:**
  1. Select Computer from the Start menu
  2. Choose System Properties from the context menu
  3. Click Advanced system settings -> Advanced tab
  4. Click on Environment Variables, under System Variables, find PATH, and click on it.
  5. In the Edit windows, modify PATH by adding the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.4\bin`)
  to the value for PATH. If you do not have the item PATH, you may select to add a new variable and add PATH as the name
  and the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.4\bin`) as the value.
  6. Reopen Command prompt window, and run your LoMRF experiments.

**Windows Vista:**
  1. Right click My Computer icon
  2. Choose Properties from the context menu
  3. Click Advanced tab (Advanced system settings link in Vista)
  4. In the Edit windows, modify PATH by adding the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.4\bin`)
     to the value for PATH. If you do not have the item PATH, you may select to add a new variable and add PATH as the name
     and the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.4\bin`) as the value.
  5. Reopen Command prompt window, and run your LoMRF experiments.

**Windows XP:**
  1. Start -> Control Panel -> System -> Advanced
  2. Click on Environment Variables, under System Variables, find PATH, and click on it.
  3. In the Edit windows, modify PATH by adding the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.4\bin`)
     to the value for PATH. If you do not have the item PATH, you may select to add a new variable and add PATH as the name
     and the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.4\bin`) as the value.
  4. Close the window.
  5. Reopen Command prompt window, and run your LoMRF experiments.

##Using LoMRF as a library

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
