#!/bin/bash

set -euo pipefail

# reinstall if necessary, on kernel updates(?)
sudo -A $HOME/bin/battery-get-charge-levels || (cd ~/oss/tpacpi-bat; ./install.pl)

cap=`cat /sys/class/power_supply/BAT0/capacity`
if [ `sudo -A /usr/local/bin/tpacpi-bat -g ST 1 | awk '{print $1}'` == 0 ] &&
   [ `sudo -A /usr/local/bin/tpacpi-bat -g SP 1 | awk '{print $1}'` == 0 ]; then
  battery-set-normal-charge-levels
  status=`cat /sys/class/power_supply/BAT0/status`
  notify-send "$cap%: $status (limited charging)" "$(battery-get-charge-levels)"
else
  battery-charge-full
  status=`cat /sys/class/power_supply/BAT0/status`
  notify-send "$cap%: $status"                    "charging to full"
fi
