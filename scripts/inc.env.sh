#!/usr/bin/env bash

PRJ_COMMONS_CLASSPATH="."

for curr_lib in `find ${lib_dir} -name "*.jar"`
do
  PRJ_COMMONS_CLASSPATH=$PRJ_COMMONS_CLASSPATH:$curr_lib
done

SCALA_LIBS=""

TERMINAL_WITH=$(tput cols)

VM_ARGS=""

ETC_DIR="$base_dir/etc"

#if [ -f $ETC_DIR/application.conf ]; then
#  VM_ARGS=$VM_ARGS" -Dconfig.resource=$ETC_DIR/application.conf "
#fi

if [ -f $ETC_DIR/logback.xml ]; then
  VM_ARGS=$VM_ARGS" -Dlogback.configurationFile=$ETC_DIR/logback.xml "
fi

VM_ARGS=$VM_ARGS" -XX:+DoEscapeAnalysis -XX:+UseFastAccessorMethods -XX:+OptimizeStringConcat"

#
# Architecture specific JVM options
#
architecture=`uname -m`
if [ $architecture = 'x86_64' ]; then
  VM_ARGS=$VM_ARGS" -Xms4g -Xmx4g -XX:+UseCompressedOops -XX:PermSize=256m -XX:MaxPermSize=256m -Xss32m "
else
  VM_ARGS=$VM_ARGS" -Xms2g -Xmx2g "
  architecture='x86_32'
fi
