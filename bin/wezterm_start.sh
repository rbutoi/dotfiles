#!/bin/bash
# this wrapper is needed by programs that can't take any extra arguments with
# their custom-specified terminal program

[[ "$1" == "-e" ]] && shift 1   # remove gnome-terminal's flag

wezterm --config 'exit_behavior = "CloseOnCleanExit"' start -- "$@"
