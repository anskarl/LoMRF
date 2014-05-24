#!/bin/bash


#infer -ms -maxSteps 100000\
#  -i example_si.mln -e evidence.db -r si.result -q HoldsAt -cw Happens,Next,Initially

#infer -ms -maxSteps 100000\
#  -i example_siw.mln -e evidence.db -r siw.result -q HoldsAt -cw Happens,Next,Initially


lomrf -infer marginal -samples 100000 -i example_si.mln -e evidence.db -r lomrf_si.result \
	-q HoldsAt/2 -cwa Happens/2,Next/2,Initially/1 \
