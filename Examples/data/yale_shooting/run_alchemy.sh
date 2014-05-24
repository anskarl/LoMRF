#!/bin/bash

infile="theory_cnf.mln"
evfile="narrative.db"

resfile=`basename $infile .mln`"_alchemy.result"
seed=`date +%M`
infer -ms  -seed $seed \
	-i $infile \
	-e $evfile \
	-cw Happens,Next \
	-ow InitiatedAt,TerminatedAt \
	-r $resfile \
	-q HoldsAt && cat $resfile | awk ' $2 != "4.9995e-05" { print $0 }'

