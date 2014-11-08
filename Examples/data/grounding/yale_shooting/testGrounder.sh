#!/bin/bash

infile="theory.mln"
evfile="narrative.db"
outputFile="ground-network.txt"

CWA="Happens/2,Next/2"
OWA="InitiatedAt/2,TerminatedAt/2"
QUERY="HoldsAt/2"

mrfwriter -i $infile -e $evfile -o $outputFile \
	-f "GROUND_CNF" \
	-q $QUERY \
	-cwa $CWA \
	-owa $OWA
