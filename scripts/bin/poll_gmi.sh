#!/bin/sh -e

# macOS compat
if command -v brew >/dev/null; then
  export PYTHONPATH="$(brew --prefix)/lib/python3.7/site-packages:$PYTHONPATH"
fi

command -v gmi >/dev/null && GMI=gmi || GMI=~/bin/gmi

[ -e ~/.mail/work ] && $GMI sync -q -C ~/.mail/work
$GMI sync -q -C ~/.mail/personal

notmuch new --quiet # to run hooks
