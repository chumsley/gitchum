#!/bin/sh
STEM=`dirname $1`
awk -F / "/^[^D]/ {print \"$STEM/\"\$2}" "$1/Entries"
