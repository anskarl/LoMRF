#!/usr/bin/env bash

PRJ_COMMONS_CLASSPATH="."

for curr_lib in `find ${lib_dir} -name "*.jar"`
do
  PRJ_COMMONS_CLASSPATH=$PRJ_COMMONS_CLASSPATH:$curr_lib
done

SCALA_LIBS=""

TERMINAL_WITH=$(tput cols)

ETC_DIR="$base_dir/etc"

#
# JVM options:
#

VM_ARGS=" -XX:+DoEscapeAnalysis -XX:+UseFastAccessorMethods -XX:+OptimizeStringConcat "

#
# Architecture dependent JVM options:
#
#architecture=`uname -m`
#if [ $architecture = 'x86_64' ]; then
#  VM_ARGS=$VM_ARGS" -Xms1g -Xmx4g -XX:+UseCompressedOops -Xss32m "
#else
#  VM_ARGS=$VM_ARGS" -Xms2g -Xmx3g -Xss32m "
#  architecture='x86_32'
#fi

#
# Logging configuration
#
# To enable debug information, export LOMRF_DEBUG=1 for storing further 
# debug information in 'debug.log' file, otherwise use default (INFO level)
# console output logging.
#
if [[ -n $LOMRF_DEBUG && $LOMRF_DEBUG -eq 1 ]]; then
	if [ ! -f $ETC_DIR/logback-debug.xml ]; then
		echo "Cannot find logging configuration file '$ETC_DIR/logback-debug.xml'"
		exit 1
	fi
	echo "Debug output is enabled (LOMRF_DEBUG=1)."
	echo "Debug output will be stored into the 'debug.log' file."

	VM_ARGS=" -Dlogback.configurationFile=$ETC_DIR/logback-debug.xml "
else
	if [ ! -f $ETC_DIR/logback.xml ]; then
		echo "Cannot find logging configuration file '$ETC_DIR/logback.xml'"
		exit 1
	fi
  	VM_ARGS=$VM_ARGS" -Dlogback.configurationFile=$ETC_DIR/logback.xml "	
fi
