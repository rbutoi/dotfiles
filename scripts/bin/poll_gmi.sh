#!/bin/sh -e

command -v gmi >/dev/null && GMI=gmi || GMI=~/bin/gmi

[ -e ~/.mail/work ] && $GMI sync -C ~/.mail/work
$GMI sync -C ~/.mail/personal

notmuch new # to run hooks
