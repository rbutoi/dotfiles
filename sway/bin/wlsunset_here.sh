#!/bin/bash
set -euo pipefail

loc=$(curl -s https://am.i.mullvad.net/json)
wlsunset -l $(echo $loc | jq .latitude) -L $(echo $loc | jq .longitude)
