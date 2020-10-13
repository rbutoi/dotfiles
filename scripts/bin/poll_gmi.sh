#!/bin/sh -e

# macOS compat
if command -v brew >/dev/null; then
  export PYTHONPATH="$(brew --prefix)/lib/python3.9/site-packages:$PYTHONPATH"
fi

command -v gmi >/dev/null && GMI=gmi || GMI=~/bin/gmi

[ -e ~/.mail/work ]     && $GMI sync -C ~/.mail/work     &
[ -e ~/.mail/personal ] && $GMI sync -C ~/.mail/personal &
[ -e ~/.mail/hotmail  ] &&    mbsync            hotmail  &
wait

notmuch new # to run hooks
