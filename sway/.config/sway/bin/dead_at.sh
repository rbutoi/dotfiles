#!/bin/bash

acpi="$(acpi)"
dead_at=$(date +%H:%M \
  -d "$(echo \"$acpi\" | grep -Po '\d\d:\d\d:\d\d' |
        sd '(\d\d):(\d\d):(\d\d)' '+ $1 hours $2 minutes $3 seconds')")

if   echo "$acpi" | grep -q Discharging; then
  echo "@ $dead_at"
  state=dead
elif echo "$acpi" | grep -q Charging; then
  state=full
else
  exit 0                        # fully charged or other: don't say anything
fi

# less often than every minute - 10
if [[ $(($(date +%M) % 10)) -eq 0 ]]; then
  logger "$state at $dead_at"
fi
