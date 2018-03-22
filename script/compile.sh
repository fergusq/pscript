#!/bin/bash

PSFILE="$1"
NAME=`basename "${PSFILE%.*}"`
CFILE="$NAME".c
EFILE=e"$NAME"

PScript compile --path lib $PSFILE >$CFILE && gcc $CFLAGS -g -Ofast -o $EFILE $CFILE -lgc
