#!/bin/bash


infer -ms -maxSteps 100000\
  -i example_si.mln -e evidence.db -r si.result -q HoldsAt -cw Happens,Next,Initially

infer -ms -maxSteps 100000\
  -i example_siw.mln -e evidence.db -r siw.result -q HoldsAt -cw Happens,Next,Initially
