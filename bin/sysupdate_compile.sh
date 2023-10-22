#!/bin/bash -x
# Without any manual intervention, update & install development packages which
# require a lengthy compile. Run as a systemd timer.
#
# Should™ be safe since it only updates non-core/system packages.

. ~/.config/path.sh # need to set PATH for Doom

LOAD_THRESHOLD=${1:-1300}

if ! load_below $LOAD_THRESHOLD && [[ $(date +%H) -gt 15 ]]; then
  echo "system load over threshold && it's 3pm-midnight, not updating. loadavg:"
  choose 0..3 </proc/loadavg
  exit 0
fi

os="$(lsb_release -si)"
if [[ $os == Arch ]]; then
  pgrep paru || paru -Syu --aur --devel --noconfirm --skipreview
elif [[ $os == Debian ]]; then
  topgrade --disable remotes
fi
