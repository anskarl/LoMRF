#!/bin/bash


input=$1


name=`basename $input .result`
if [ "$name" = "$input" ]; then
  exit 1
fi

sed -e 's/4.9995e-05/0.0/g' -e 's/HoldsAt(CE,\([0-9][0-9]*\))/\1/g' < $input | sort -n > $name".data"
