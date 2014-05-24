#!/bin/bash


infer -ms -i dec7a-cnf-hard.mln -e fra1gt_evidence.db -r alchemy_output-hard.result \
	-q HoldsAt -ow InitiatedAt,TerminatedAt \
	-cw Next,Close,OrientationMove,Happens,StartTime

