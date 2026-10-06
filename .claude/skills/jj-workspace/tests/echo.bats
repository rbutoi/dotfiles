#!/usr/bin/env bats
# Every command that CHANGES something is printed before it runs. What these scripts do to
# history is a handful of ordinary jj commands, and the printed line is what makes an unexpected
# result reproducible by hand instead of reconstructable only from the resulting graph.

load 'helper'

@test "no mutating command escapes the echo" {
  # Static, because the failure mode is silent: a rewrite happens and nothing in the transcript
  # says what ran. Reads (jj log, jj status, jj workspace root) are deliberately not listed —
  # echoing those would bury the two lines that matter.
  run grep -nE '^[[:space:]]*(jj (rebase|new|abandon|describe|workspace (add|forget))|rm -rf)' \
    "$scripts"/ws-create.sh
  [ "$status" -ne 0 ] || {
    echo "these mutate without going through run_cmd:"
    echo "$output"
    return 1
  }
}

@test "ws-create echoes the workspace add" {
  repo=$(new_repo)
  run env -C "$repo" "$scripts/ws-create.sh" feat
  [ "$status" -eq 0 ]
  [[ "$output" == *'+ jj workspace add --name feat'* ]]
}
