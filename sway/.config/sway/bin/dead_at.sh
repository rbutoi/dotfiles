#!/bin/bash

acpi="$(acpi | grep 'Battery 0')"
dead_at=$(date +%H:%M \
  -d "$(echo \"$acpi\" | grep -Po '\d\d:\d\d:\d\d' |
        sd '(\d\d):(\d\d):(\d\d)' '+ $1 hours $2 minutes $3 seconds')")

if echo "$acpi" | grep -q Discharging; then
  s="🪫️@$dead_at"
elif echo "$acpi" | grep -q Charging; then
  s="🔋@$dead_at"
fi

[ ! -z "$s" ] && echo $s          # fully charged or other: don't say anything

# less often than every minute - 10
if [[ $(($(date +%M) % 10)) -eq 0 ]]; then
  logger "$state at $dead_at"
fi
