#!/bin/bash

infile="theory.mln"
evfile="narrative.db"

resfile=`basename $infile .mln`"_lomrf.result"


lomrf -infer marginal -i $infile -e $evfile -r $resfile \
	-q HoldsAt/2 \
	-owa InitiatedAt/2,TerminatedAt/2 \
	-cwa Happens/2,Next/2 &&  grep "HoldsAt" $resfile | awk ' $2 != "4.9995e-05" { print $0 }' | sort

