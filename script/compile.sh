#!/bin/bash

PSFILE="$1"
NAME=`basename "${PSFILE%.*}"`
CFILE="$NAME".c
EFILE=e"$NAME"

PScript $PSFILE eg/stdlib.pscript >$CFILE && gcc -Ofast -o $EFILE $CFILE -lgc
