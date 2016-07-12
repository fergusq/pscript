#!/bin/bash

PSFILE="$1"
NAME=`basename "${PSFILE%.*}"`
CFILE="$NAME".c
EFILE=e"$NAME"

cpp $PSFILE | sed '/^#/d' | PScript >$CFILE && gcc -Ofast -o $EFILE $CFILE -lgc
