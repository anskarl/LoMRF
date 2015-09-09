#!/bin/bash

lomrf -i theory.mln -e evidence.db -r output.result \
 -q Hate/2 \
 -owa Loyal/2,Roman/1 \
 -cwa People/1,Ruler/1,Pompeian/1,Assasinate/2 && cat output.result

