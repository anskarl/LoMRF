#!/bin/bash

infile="theory_cnf.mln"
evfile="narrative.db"

resfile=`basename $infile .mln`"_alchemy.result"
seed=`date +%M`
infer -m  -seed $seed \
	-i $infile \
	-e $evfile \
	-cw Happens,Next \
	-r $resfile \
	-q HoldsAt,InitiatedAt,TerminatedAt && cat $resfile | awk ' $2 != "4.9995e-05" { print $0 }'

