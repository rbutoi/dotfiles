#!/bin/bash

# https://www.adminsub.net/mac-address-finder/b4:2e:99
ip neigh | grep -q '192.168.1.2.*b4:2e:99' || exit 1

info="$(curl -s 192.168.1.2:9002/api/v2/transfer/info)"
down=$(echo "$info" | jaq .dl_info_speed)
up=$(  echo "$info" | jaq .up_info_speed)

logger --tag $(basename $0) "$down,$up"

echo "$(echo $down | numfmt --to=iec-i) ⬇️⬆️ $(echo $up | numfmt --to=iec-i)"
