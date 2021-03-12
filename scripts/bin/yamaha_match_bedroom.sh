#!/usr/bin/env bash
set -euo pipefail

# could've been just one python script but /shrug

extra_bedroom_factor=1.3

receiver_state=`curl -s http://192.168.1.17/YamahaExtendedControl/v1/main/getStatus`
receiver_vol=`echo $receiver_state | jq .volume`
receiver_max_vol=`echo $receiver_state | jq .max_volume`

bedroom_state=`curl -s http://192.168.1.9/YamahaExtendedControl/v1/main/getStatus`
bedroom_max_vol=`echo $bedroom_state | jq .max_volume`

desired_bedroom_vol=`python -c "print(int($receiver_vol / $receiver_max_vol * $bedroom_max_vol * $extra_bedroom_factor))"`
curl -s http://192.168.1.9/YamahaExtendedControl/v1/main/setVolume?volume=$desired_bedroom_vol > /dev/null

echo -n "receiver volume: "
curl -s http://192.168.1.17/YamahaExtendedControl/v1/main/getStatus | jq -j .actual_volume.value,.actual_volume.unit
echo -en "\nbedroom  volume: "
curl -s  http://192.168.1.9/YamahaExtendedControl/v1/main/getStatus | jq -j .volume
echo " / $bedroom_max_vol"
