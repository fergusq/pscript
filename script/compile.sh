#!/bin/bash

PSFILE="$1"
NAME=`basename "${PSFILE%.*}"`
CFILE="$NAME".c
EFILE=e"$NAME"

PScript --path eg $PSFILE >$CFILE && gcc -Ofast -o $EFILE $CFILE -lgc
