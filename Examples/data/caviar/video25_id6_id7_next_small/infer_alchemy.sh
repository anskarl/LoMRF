#!/bin/bash

if [ ! -f dec7a-cnf.mln ]; then
   mlnc -i dec7a.mln -o dec7a-cnf.mln
fi

infer -ms -i dec7a-cnf.mln -e fra1gt_evidence.db -r alchemy_output.result \
	-q HoldsAt -ow InitiatedAt,TerminatedAt \
	-cw Next,Close,OrientationMove,Happens,StartTime > out.log

