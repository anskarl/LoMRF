#!/bin/bash




#infer -ms -i dec7a-cnf-hard.mln -e fra1gt_evidence.db -r alchemy_output-hard.result \
#        -q HoldsAt -ow InitiatedAt,TerminatedAt \
#        -cw Next,Close,OrientationMove,Happens,StartTime


lomrf -infer marginal -i  dec7a_ext-hard.mln -e fra1gt_evidence.db -r lomrf_output-hard.result \
	-q HoldsAt/2 -owa InitiatedAt/2,TerminatedAt/2 \
	-cwa Next/2,Close/4,OrientationMove/3,Happens/2,StartTime/1

