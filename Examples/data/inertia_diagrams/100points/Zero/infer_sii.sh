#!/bin/bash


infer -ms -maxSteps 100000 \
  -i example_sii.mln -e evidence.db -r sii.result -q HoldsAt -cw Happens,Next,Initially

infer -ms -maxSteps 100000 \
  -i example_siiw.mln -e evidence.db -r siiw.result -q HoldsAt -cw Happens,Next,Initially
