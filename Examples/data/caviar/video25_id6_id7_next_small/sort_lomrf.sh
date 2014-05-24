#!/bin/bash


if [ -f lomrf-output.result ]; then
    grep "HoldsAt" lomrf-output.result | sort -  > lomrf-output-sorted.result
fi

if [ -f lomrf-output-decomposed.result ]; then
    grep "HoldsAt" lomrf-output-decomposed.result | sort -  > lomrf-output-decomposed-sorted.result
fi

if [ -f lomrf-output-simplification.result ]; then
    grep "HoldsAt" lomrf-output-simplification.result | sort -  > lomrf-output-simplification-sorted.result
fi


if [ -f lomrf-output-ext2-decomposed.result ]; then
    grep "HoldsAt" lomrf-output-ext2-decomposed.result | sort -  > lomrf-output-ext2-decomposed-sorted.result
fi

if [ -f lomrf-output-exp2-simplification.result ]; then
    grep "HoldsAt" lomrf-output-exp2-simplification.result | sort -  > lomrf-output-ext2-simplification-sorted.result
fi



if [ -f lomrf_output-hard.result ]; then
	grep "HoldsAt" lomrf_output-hard.result | sort -  > lomrf_output-hard-sorted.result
fi
