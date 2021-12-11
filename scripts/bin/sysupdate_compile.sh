#!/bin/bash
# Automatically update & install AUR/devel packages (and Emacs, requiring an
# elisp compilation). Should™ be safe since it only updates non-core/system
# packages.

. ~/.config/path.sh # need to set PATH for Doom

if ! load_below; then
  echo system load over threshold, not updating
  exit 0
fi

paru -Sua --devel --noconfirm

doom -y upgrade
# libvterm needs recompiling after Emacs (scraped from `vterm.el`)
cd ~/oss/doom-emacs/.local/straight/build-*/vterm/; mkdir -p build; cd build
cmake -G 'Unix Makefiles'  ..
make
