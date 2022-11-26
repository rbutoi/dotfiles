#!/bin/bash

set -euo pipefail

tree="$(swaymsg -t get_tree)"
name="$(echo $tree | jq '..|try select(.focused == true).name')"
sticky="$(echo $tree | jq '..|try select(.focused == true).sticky')"

notify-send -h int:transient:1 "[sticky: $sticky] $name"
