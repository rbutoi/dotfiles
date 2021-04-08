#!/usr/bin/env bash
set -euo pipefail

if `curl -s http://192.168.1.17/YamahaExtendedControl/v1/dist/getDistributionInfo | grep -q cafebabe`; then
  curl -s -X POST -H "Content-Type: application/json"  http://192.168.1.9/YamahaExtendedControl/v1/dist/setClientInfo -d '{"group_id":"", "zone":["main"]}' >/dev/null
  curl -s -X POST -H "Content-Type: application/json" http://192.168.1.17/YamahaExtendedControl/v1/dist/setServerInfo -d '{"group_id":"", "zone":["main"], "type":"remove", "client_list": ["192.168.1.9"]}' >/dev/null
  title="Yamaha unlinked"
else
  curl -s -X POST -H "Content-Type: application/json"  http://192.168.1.9/YamahaExtendedControl/v1/dist/setClientInfo -d '{"group_id":"cafebabecafebabecafebabecafebabe", "zone":["main"]}' >/dev/null
  curl -s -X POST -H "Content-Type: application/json" http://192.168.1.17/YamahaExtendedControl/v1/dist/setServerInfo -d '{"group_id":"cafebabecafebabecafebabecafebabe", "zone":["main"], "type":"add", "client_list": ["192.168.1.9"]}' >/dev/null
  title="Yamaha linked"
fi
curl -s http://192.168.1.17/YamahaExtendedControl/v1/dist/startDistribution?num=0 >/dev/null

notify-send -t 1500 "$title" "$(~/bin/yamaha_match_bedroom.py)"
