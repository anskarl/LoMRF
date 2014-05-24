#!/bin/bash


infer -ms -maxSteps 100000 \
  -i example_sit.mln -e evidence.db -r sit.result -q HoldsAt -cw Happens,Next,Initially

infer -ms -maxSteps 100000 \
  -i example_sitw.mln -e evidence.db -r sitw.result -q HoldsAt -cw Happens,Next,Initially
