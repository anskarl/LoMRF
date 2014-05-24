#!/bin/bash

#
# Generates a sequence of 100 timepoints (from 0 to 99)
#

for t0 in `seq 0 99`
do
  t1=$(($t0 + 1))
  echo "Next("$t1","$t0")"
done
