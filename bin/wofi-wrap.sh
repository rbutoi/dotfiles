#!/bin/bash
# adapted from wofi(7)

wofi_args="${1:---show=dmenu,drun,run}"

j_or_j() {
  jaq "$@" || jq "$@"
}

swaymsg -t get_tree |
  j_or_j -r '.nodes[].nodes[] |
    if .nodes then [recurse(.nodes[])] else [] end + .floating_nodes | .[] |
    select(.nodes==[]) | ((.id | tostring) + " " + .name)' | grep -v __i3_ |
  wofi "$wofi_args" | {
  read -r id name
  swaymsg "[con_id=$id]" focus
}
