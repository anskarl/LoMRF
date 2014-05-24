#!/bin/bash


infer -ms -i dec7a_pc-alchemy.mln -e fra1gt_evidence.db -r dec7a_pc-alchemy_output.result \
	-q HoldsAt \
	-cw Next,Close,OrientationMove,Happens,StartTime \
	-breakHardClauses true

