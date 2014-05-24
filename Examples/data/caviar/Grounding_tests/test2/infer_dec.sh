#!/bin/bash


infer -ms \
  -q "HoldsAt" \
  -i dec-cnf.mln \
  -r dec-cnf.result \
  -e "fra1gt_evidence_0.db" \
  -ow "TerminatedAt,InitiatedAt" \
  -cw "Happens,Close,OrientationMove,Next,StartTime"


