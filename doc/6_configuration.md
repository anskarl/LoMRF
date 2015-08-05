# Configuration Parameters

Assuming that you have successfully created an LoMRF distribution from sources (see [Building and Linking](5_building_and_linking.md))
and that you have extracted the distribution into some path (e.g., `/path/to/your/compiled/LoMRF/`). You may want to 
adjust the following runtime parameters.


## Memory and JVM parameters

Depending your requirements you may want to adjust the heap memory parameters or other options of the Java VM. In order
to adjust the JVM options, you can chane the  `VM_ARGS` variable in the `inc.env.sh` file (located inside the `bin` 
directory) and add or adjust any JVM parameter (for the available parameters write `java -X` in the command line). 

For example, you may adjust the following default options

```bash
#
# JVM options:
#
VM_ARGS=" -XX:+DoEscapeAnalysis -XX:+UseFastAccessorMethods -XX:+OptimizeStringConcat -Xss32m "
```

and set the heap memory parameters of the JVM, in order to define 1 GB as initial heap memory size (i.e., `-Xms1g`) and 
4 GB as the maximum heap memory size (i.e., `-Xmx4g`), as it is presented below:

```bash
#
# JVM options:
#
VM_ARGS=" -XX:+DoEscapeAnalysis -XX:+UseFastAccessorMethods -XX:+OptimizeStringConcat -Xss32m -Xms1g -Xmx4g "
```

**Please do not forget to leave spaces at the beginning and at the end of the `VM_ARGS` variable.**


## Quickly enable debug logging

To enable debug information, export the environment variable `LOMRF_DEBUG=1` for storing further debug information in 
`debug.log` file (the file will be created into the current working path), otherwise use default (INFO level) for 
console-only output logging.

For example, calling the `lomrf` CLI tool with extra debug logging from the command line:

```bash
$ export LOMRF_DEBUG=1; lomrf <your option list ...>
```


## Change the logging configuration

You can override the default logging configuration by placing your logback configuration file (i.e., `logback.xml`) 
inside the `/path/to/your/compiled/LoMRF/etc` directory.


