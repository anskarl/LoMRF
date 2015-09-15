#!/bin/bash

lomrf -infer marginal \
	-i theory.mln \
	-e narrative.db \
	-r marginal-out.result \
	-q HoldsAt/2 \
	-cwa StartTime/1,Happens/2,Close/4,OrientationMove/3 \
        -owa InitiatedAt/2,TerminatedAt/2
