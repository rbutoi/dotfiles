#!/usr/bin/env bash

pkill -f "tail -f $SWAYSOCK.wob" # also kills the wob it feeds it seems

rm -f $SWAYSOCK.wob
mkfifo $SWAYSOCK.wob
tail -f $SWAYSOCK.wob 2>~/.cache/.wob_tail_errors | wob -O* 2> ~/.cache/.wob_errors
