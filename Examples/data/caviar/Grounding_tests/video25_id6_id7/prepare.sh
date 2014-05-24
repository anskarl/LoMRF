#!/bin/bash

if [ ! -f "empty.db" ]; then
    touch empty.db
fi


if [ -f caviar_dec.mln ]; then
  mlnc -i caviar_dec.mln -e empty.db -o caviar_dec-cnf.mln -fDomain false
fi

if [ -f dec.mln ]; then
  mlnc -i dec.mln -e empty.db -o dec-cnf.mln -fDomain false
fi

if [ -f "empty.db" ]; then
    rm empty.db
fi
