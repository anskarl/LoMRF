#!/bin/bash

lomrf -i uniform.mln -q Heads/1 -r uniform.result

lomrf -i binomial.mln -q Heads/1 -r binomial.result

lomrf -i multinomial.mln -q Outcome/2 -r multinomial.result
