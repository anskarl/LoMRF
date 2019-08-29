#!/usr/bin/env bash

PRJ_COMMONS_CLASSPATH="."

for curr_lib in `find ${lib_dir} -name "*.jar"`
do
  PRJ_COMMONS_CLASSPATH=$PRJ_COMMONS_CLASSPATH:$curr_lib
done

SCALA_LIBS=""

ETC_DIR="$base_dir/etc"


#
# Load poor mans logger ;)
#
. `dirname $0`/poor.mans.logger.sh


#
# JVM options:
#
if [[ -z ${LOMRF_JVM_ARGS+x} ]]; then
  LOMRF_JVM_ARGS=" -XX:+DoEscapeAnalysis -XX:+OptimizeStringConcat "
else
  log_info "User-defined JVM args: $LOMRF_JVM_ARGS"
fi


#
# Logging configuration
#
# To enable debug information, export LOMRF_DEBUG=1 for storing further 
# debug information in 'debug.log' file, otherwise use default (INFO level)
# console output logging.
#
if [[ -n $LOMRF_DEBUG && $LOMRF_DEBUG -eq 1 ]]; then
	if [[ ! -f $ETC_DIR/logback-debug.xml ]]; then
		echo "Cannot find logging configuration file '$ETC_DIR/logback-debug.xml'"
		exit 1
	fi

	log_warn "Debug output is enabled (LOMRF_DEBUG=1)."
	log_warn "Debug output will be stored into the 'debug.log' file."

	LOMRF_JVM_ARGS=" -Dlogback.configurationFile=$ETC_DIR/logback-debug.xml "
else
	if [[ ! -f $ETC_DIR/logback.xml ]]; then
		exit_error "Cannot find logging configuration file '$ETC_DIR/logback.xml'"
	fi

  	LOMRF_JVM_ARGS=" $LOMRF_JVM_ARGS -Dlogback.configurationFile=$ETC_DIR/logback.xml "
fi

