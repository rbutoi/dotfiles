#!/bin/bash -x
# Without any manual intervention, update & install development packages which
# require a lengthy compile. Run as a systemd timer.
#
# Should™ be safe since it only updates non-core/system packages.

. ~/.config/path.sh # need to set PATH for Doom

if ! load_below; then
  echo system load over threshold, not updating
  exit 0
fi

os="$(lsb_release -si)"
if [[ $os == Arch ]]; then
  paru -Sua --devel --noconfirm

  # isn't -git, needs to be updated nightly
  pacman -Qq | grep nightly | xargs paru -S --noconfirm

  topgrade --only custom_commands
elif [[ $os == Debian ]]; then
  topgrade --disable remotes
fi
