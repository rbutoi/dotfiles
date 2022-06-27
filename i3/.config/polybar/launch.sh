#!/usr/bin/env bash

# Terminate already running bar instances
# If all your bars have ipc enabled, you can use 
polybar-msg cmd quit
# Otherwise you can use the nuclear option:
# killall -q polybar

# Launch bar0 and bar1
echo "---" | tee -a /tmp/polybar0.log /tmp/polybar0.log
polybar bar0 2>&1 | tee -a /tmp/polybar0.log & disown
polybar bar1 2>&1 | tee -a /tmp/polybar1.log & disown

echo "Bars launched..."
