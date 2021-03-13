#!/usr/bin/env bash
set -euo pipefail

systemctl --user import-environment

rm -f $SWAYSOCK.wob
mkfifo $SWAYSOCK.wob
tail -f $SWAYSOCK.wob 2>/dev/null | wob -O*
