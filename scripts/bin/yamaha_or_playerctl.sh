#!/usr/bin/env bash
set -euo pipefail

yamaha_cmd=$1
playerctl_cmd=${yamaha_cmd/_/-}
URL=192.168.1.17/YamahaExtendedControl/v1/netusb

# nmcli | grep -q 'i like turtles' &&
# only lape
if curl -s "${URL}/getPlayInfo" | jq -r .playback | grep -q 'play\|pause'; then
    set -x; curl -s "${URL}/setPlayback?playback=${yamaha_cmd}"
else
    set -x; playerctl "${playerctl_cmd}"
fi
