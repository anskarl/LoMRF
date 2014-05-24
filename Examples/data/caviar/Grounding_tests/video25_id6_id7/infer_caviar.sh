#!/bin/bash


infer -ms \
  -q "HoldsAt" \
  -i caviar_dec-cnf.mln \
  -r caviar_dec-cnf-out.result \
  -e "fra1gt_evidence.db" \
  -ow "InitiatedAt,TerminatedAt" \
  -cw "Happens,Close,OrientationMove,Next,StartTime"


