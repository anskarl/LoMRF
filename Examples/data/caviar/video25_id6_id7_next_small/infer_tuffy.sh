#!/bin/bash

if [ ! -f dec7a-cnf-tuffy.mln ]; then
   mlnc -i dec7a.mln -o dec7a-cnf-tuffy.mln  -fFunctionsAsPredicates true -p normal
fi

tuffy.sh -i dec7a-cnf-tuffy.mln -e fra1gt_evidence-tuffy.db -r tuffy_output.result \
	-q HoldsAt,InitiatedAt,TerminatedAt \
        -cw Next,Close,OrientationMove,Happens,StartTime,ReturnValueOfactive,ReturnValueOfenter,ReturnValueOfrunning,ReturnValueOffight,ReturnValueOfwalking,ReturnValueOfinactive,ReturnValueOfexit \
	-nopart  -mcsatSamples 1000  -maxFlips 100000 -mcsatParam 100 -marginal -activateAll
