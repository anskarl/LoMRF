# Build and Link LoMRF

In order to build LoMRF from source, you need to have Java SE Development Kit (e.g., OpenJDK) version 8 or higher and 
[SBT](http://www.scala-sbt.org/) (v0.13.x) installed in your system. Furthermore, LoMRF build depends on the 
[auxlib](https://github.com/anskarl/auxlib), [Optimus](https://github.com/vagm/Optimus) and optionally to [
Gurobi](http://www.gurobi.com/) and [LPSolve](http://lpsolve.sourceforge.net).

## Instructions to build LoMRF from source

**Optional pre-build step.** Please note that Gurobi is required for running max-margin weight learning and optionally 
can also be used for MAP inference. In order to enable support for Gurobi, you have to include Gurobi library 
dependencies to the `./lib` subdirectory inside the cloned LoMRF directory (create if not already exists), as it is 
illustrated in the tree below:

```bash
LoMRF
|--lib
    |-- gurobi.jar
```

To build the LoMRF distribution, give the following command:

```bash
$ sbt dist
```

After a successful compilation, the LoMRF distribution is located inside the `./target/universal/lomrf-<version>.zip`
file. You can extract this file and add the `path/to/lomrf-<version>/bin` in your PATH, in order to execute the LoMRF
scripts from terminal (see Section [Add LoMRF executables in your default PATH](#Add-LoMRF-executables-in-your-default-PATH)). 
The distribution contains all library dependencies and requires Java 8
(or higher runtime). Sources, documentation and the compiled library (without dependencies) are archived as jar files
into the `./target/scala-2.11/` directory. The resulting documentation is located inside the `./target/site/scaladocs`
directory.

**Optional step.** Run Unit tests.
In order to run Unit tests, you need to download example data. Please follow the instructions in 
[Download Example and Unit Testing Data](7_2_download_example_data.md). LoMRF uses the [ScalaTest](http://www.scalatest.org) 
framework for unit testing.

To run all tests (**requires LPSolve and Gurobi solvers installed**), give the following command inside the SBT console:
```
test
```

If you want to run a specific test, e.g., `lomrf.mln.inference.MaxWalkSATSpecTest`:
```
testOnly lomrf.mln.inference.MaxWalkSATSpecTest
```



## Only for Microsoft Windows XP users

LoMRF depends on the JANSI library, in order to print colored log messages in the console. JANSI depends on a JNI library
that is available from "Microsoft Visual C++ 2008 SP1 Redistributable". You can get a free copy from Microsoft at the
following link: http://www.microsoft.com/en-us/download/details.aspx?displaylang=en&id=5582

## Add LoMRF executables in your default PATH

You can add all CLI tools to your default PATH, in order to directly call LoMRF tools from anywhere in the command line
interface. Depending on your OS configuration you have the add and export the path `/path/to/lomrf-<version>/bin` to
the `PATH` variable.

For example, lets say that the LoMRF distribution version 0.7.0 is being installed in your home directory in `$HOME/lomrf-0.7.0`,
the directory structure inside the LoMRF directory is the following:

```bash
lomrf-0.7.0/
|-- bin
|-- etc
|-- lib
```

### Linux, Unix and MacOS X

The `bin` sub-directory contains all LoMRF executable tools. In order to add this sub-directory in your default `PATH`
add the following line in you profile file.


**BASH** e.g., inside `.profile`, `.bashrc` or `.bash_profile` file in your home directory:
```bash
export PATH=$PATH:$HOME/lomrf-0.7.0/bin
```

**CSH/TCSH** e.g., inside `~/.login` file in your home directory:
```csh
set path = ($path $HOME/lomrf-0.7.0/bin .)
```
or in `~/.cshrc` file in your home directory:
```csh
setenv PATH $PATH:$HOME/lomrf-0.7.0/bin:.
```

### Microsoft Windows Operating Systems

For MS Windows OS add the location of LoMRF sub-folder (e.g., `C:\path\to\lomrf-0.7.0\bin`) in your PATH environment variable.

**Windows 8:**
  1. Drag the Mouse pointer to the Right bottom corner of the screen
  2. Click on the Search icon and type: Control Panel
  3. Click on -> Control Panel -> System -> Advanced
  4. Click on Environment Variables, under System Variables, find PATH, and click on it.
  5. In the Edit windows, modify PATH by adding the location of the LoMRF bin sub-folder (e.g., `C:\path\to\lomrf-0.7.0\bin`)
  to the value for PATH. If you do not have the item PATH, you may select to add a new variable and add PATH as the name
  and the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.7.0\bin`) as the value.
  6. Close the window.
  7. Reopen Command prompt window, and run your LoMRF experiments.

**Windows 7:**
  1. Select Computer from the Start menu
  2. Choose System Properties from the context menu
  3. Click Advanced system settings -> Advanced tab
  4. Click on Environment Variables, under System Variables, find PATH, and click on it.
  5. In the Edit windows, modify PATH by adding the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.7.0\bin`)
  to the value for PATH. If you do not have the item PATH, you may select to add a new variable and add PATH as the name
  and the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.7.0\bin`) as the value.
  6. Reopen Command prompt window, and run your LoMRF experiments.

**Windows Vista:**
  1. Right click My Computer icon
  2. Choose Properties from the context menu
  3. Click Advanced tab (Advanced system settings link in Vista)
  4. In the Edit windows, modify PATH by adding the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.7.0\bin`)
     to the value for PATH. If you do not have the item PATH, you may select to add a new variable and add PATH as the name
     and the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.7.0\bin`) as the value.
  5. Reopen Command prompt window, and run your LoMRF experiments.

**Windows XP:**
  1. Start -> Control Panel -> System -> Advanced
  2. Click on Environment Variables, under System Variables, find PATH, and click on it.
  3. In the Edit windows, modify PATH by adding the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.7.0\bin`)
     to the value for PATH. If you do not have the item PATH, you may select to add a new variable and add PATH as the name
     and the location of LoMRF bin sub-folder (i.e., `C:\path\to\lomrf-0.7.0\bin`) as the value.
  4. Close the window.
  5. Reopen Command prompt window, and run your LoMRF experiments.

## LPSolve installation instructions (optional)

### Linux distributions 

For example, on a ***Debian-based*** distribution, write the following command:
```bash
$ sudo apt-get install lp-solve
```
  
To install Java Native Interface support for LPSolve v5.5.x you need follow the  instructions below:
* Download LPSolve dev, 64bit *lp_solve_5.5.2.x_dev_ux64.zip* or for 32bit *lp_solve_5.5.2.x_dev_ux32.zip*, from [LPSolve official repository](http://sourceforge.net/projects/lpsolve/files/lpsolve/5.5.2.0/).
  * Extract the file
  * We only need the `lpsolve55.so` file.
* Download LPSolve java bindings (lp_solve_5.5.2.x_java.zip) from [LPSolve official repository](http://sourceforge.net/projects/lpsolve/files/lpsolve/5.5.2.0/).
    * Extract the file
    * We only need the `lpsolve55j.so` files
* Create a directory containing the `lpsolve55.so` and `lpsolve55j.so` files, e.g., `$HOME/lib/lpsolve55`    
* Add this directory to `LD_LIBRARY_PATH` in your profile file:

**BASH** e.g., inside `.profile`, `.bashrc` or `.bash_profile` file in your home directory:
```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/lib/lpsolve55
```

**CSH/TCSH** e.g., inside `~/.login` file in your home directory:
```csh
set LD_LIBRARY_PATH = ($LD_LIBRARY_PATH $HOME/lib/lpsolve55 .)
```
or in `~/.cshrc` file in your home directory:
```csh
setenv LD_LIBRARY_PATH $LD_LIBRARY_PATH:$HOME/lib/lpsolve55:.
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

To install the Java Native Interface support for LPSolve v5.5.x you need follow the  instructions below:
* Copy `liblpsolve55.dylib` to Java Extensions dir:
`$ sudo cp /usr/local/Cellar/lp_solve/5.5.2.0/lib/liblpsolve55.dylib /Library/Java/Extensions/`

* Download `lp_solve_5.5.2.0_source.tar.gz` and `lp_solve_5.5.2.0_java.zip` from [LPSolve official repository](http://sourceforge.net/projects/lpsolve/files/lpsolve/5.5.2.0/)
and read the instructions `lp_solve_5.5_java/lib/mac/build-osx` to build `liblpsolve55j.jnilib`. Then copy it to Java Extensions dir:
`$ sudo cp lp_solve_5.5_java/lib/mac/build-osx/liblpsolve55j.jnilib /Library/Java/Extensions/`

* If you prefer to copy these files elsewhere (instead of `/Library/Java/Extensions/`), then you need to set-up `LD_LIBRARY_PATH` and, in case of OSX El Capitan, [disable SIP protection](https://github.com/oracle/node-oracledb/issues/231).


### Microsoft Windows
To install LPSolve v5.5.x in your system, follow the instructions below:
  * Download LPSolve dev, 64bit *lp_solve_5.5.2.x_dev_win64.zip* or for 32bit *lp_solve_5.5.2.x_dev_win64.zip*, from [LPSolve official repository](http://sourceforge.net/projects/lpsolve/files/lpsolve/5.5.2.0/).
    * Extract the file
    * We only need the `lpsolve55.dll` file.
  * Download LPSolve java bindings (lp_solve_5.5.2.x_java.zip) from [LPSolve official repository](http://sourceforge.net/projects/lpsolve/files/lpsolve/5.5.2.0/).
    * Extract the file
    * We only need the `lpsolve55j.jar` and `lpsolve55j.dll` files
  * Create a directory containing the `lpsolve55.dll`, `lpsolve55j.jar` and `lpsolve55j.dll` files, e.g., `C:\path\to\lpsolve55`
  * Add this directory to the PATH environment variable in your system environment variables (see [instructions](#microsoft-windows-operating-systems))

## Gurobi installation instructions (optional)
Please follow the installation instructions from the [Gurobi website](http://www.gurobi.com).


## Using LoMRF as a library

Follow the first 2 steps of Section [Instructions to build LoMRF from source](#instructions-to build-lomrf-from-source) and then publish locally the LoMRF:

```bash
$ sbt publishLocal
```

Thereafter, in order to link LoMRF (e.g., version 0.7.0) to your [SBT](http://www.scala-sbt.org/) project, add the
following dependency:

```
libraryDependencies += "com.github.anskarl" %% "lomrf" % "0.7.0"
```

Similarly, in an [Apache Maven](https://maven.apache.org/) pom file:

```xml
<dependency>
    <groupId>com.github.anskarl</groupId>
    <artifactId>lomrf_2.11</artifactId>
    <version>0.7.0</version>
</dependency>
```
