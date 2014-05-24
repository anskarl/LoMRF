#!/bin/bash

lomrf -infer marginal -i  dec7a_pc.mln -e fra1gt_evidence.db -r lomrf_output-pc.result \
	-q HoldsAt/2  \
	-cwa Next/2,Close/4,OrientationMove/3,Happens/2,StartTime/1

