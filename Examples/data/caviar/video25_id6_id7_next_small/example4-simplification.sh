#!/bin/bash


lomrf -infer marginal -i dec7a_pc.mln -e fra1gt_evidence.db -r lomrf-output-simplification.result \
	-q HoldsAt/2  \
	-cwa Happens/2,Close/4,OrientationMove/3,StartTime/1,Next/2 $@
