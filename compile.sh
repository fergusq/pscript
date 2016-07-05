#!/bin/bash

PSFILE="$1"
NAME="${PSFILE%.*}"
CFILE="$NAME".c
EFILE=e"$NAME"

cpp $PSFILE | sed '/^#/d' | PScript >$CFILE && gcc -g -o $EFILE $CFILE -lgc
