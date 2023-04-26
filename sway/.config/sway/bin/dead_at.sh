#!/bin/bash

acpi="$(acpi | grep -E 'until|remaining')"
dead_at=$(date +%H:%M \
  -d "$(echo \"$acpi\" | grep -Po '\d\d:\d\d:\d\d' |
        sd '(\d\d):(\d\d):(\d\d)' '+ $1 hours $2 minutes $3 seconds')")

if echo "$acpi" | grep -q Discharging; then
  s="🪫️@$dead_at"
elif echo "$acpi" | grep -q Charging && ! echo "$acpi" | grep -q 99; then
  s="🔋@$dead_at"
fi

[[ ! -z "$s" ]] && echo $s      # fully charged or other: don't say anything

# less often than every minute - 10

# TODO: broken, and don't log for now
# .config/sway/bin/dead_at.sh: line 17: 09: value too great for base (error token is "09")

# if [[ $(($(date +%M) % 10)) -eq 0 ]]; then
#   logger "$state at $dead_at"
# fi
