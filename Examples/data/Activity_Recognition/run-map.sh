#!/bin/bash

lomrf -infer map \
	-i theory.mln \
	-e narrative.db \
	-r map-out.result \
	-q HoldsAt/2 \
	-cwa StartTime/1,Happens/2,Close/4,OrientationMove/3

