#!/usr/bin/env bats
#
# The SessionStart hook runs on EVERY session on this machine, so the interesting assertions are
# mostly about staying silent. The one case that produced a plausible-looking bug in review — a
# session started in a subdirectory of a workspace, where `.jj` does not exist — is pinned twice,
# once per side of the main/secondary split.

load 'helper'

hook="$BATS_TEST_DIRNAME/../scripts/ws-session-context.sh"

# The hook reads cwd, so every case is "run it from here and see".
in_dir() { env -C "$1" bash "$hook"; }

@test "silent in a plain jj repo (main workspace), at the root" {
  repo=$(new_repo)
  run in_dir "$repo"
  [ "$status" -eq 0 ]
  [ -z "$output" ]
}

@test "silent in a main workspace subdirectory" {
  repo=$(new_repo)
  mkdir -p "$repo/deep/sub"
  run in_dir "$repo/deep/sub"
  [ "$status" -eq 0 ]
  [ -z "$output" ]
}

@test "silent outside a jj repo entirely" {
  d="$BATS_TEST_TMPDIR/plain"
  mkdir -p "$d"
  run in_dir "$d"
  [ "$status" -eq 0 ]
  [ -z "$output" ]
}

@test "fires at the root of a secondary workspace" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat)
  run in_dir "$ws"
  [ "$status" -eq 0 ]
  [[ "$output" == *'"hookEventName":"SessionStart"'* ]]
  [[ "$output" == *'secondary jj workspace'* ]]
}

@test "fires in a SUBDIRECTORY of a secondary workspace" {
  # `.jj` lives only at the workspace root, so the obvious `[ -f .jj/repo ]` check is a false
  # negative here — the hook would go quiet exactly where the work happens.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat)
  mkdir -p "$ws/deep/sub"
  run in_dir "$ws/deep/sub"
  [ "$status" -eq 0 ]
  [[ "$output" == *'secondary jj workspace'* ]]
}

@test "the payload is valid JSON on one line" {
  # It is hand-rolled rather than jq'd, so the escaping is the thing most likely to rot. Backticks
  # and single quotes in the message have burned this before it was a test.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat)
  run in_dir "$ws"
  [ "${#lines[@]}" -eq 1 ]
  echo "$output" | python3 -c 'import json,sys; json.load(sys.stdin)'
}

@test "the message carries the commit rule and the main checkout path" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat)
  run in_dir "$ws"
  ctx=$(echo "$output" |
    python3 -c 'import json,sys; print(json.load(sys.stdin)["hookSpecificOutput"]["additionalContext"])')
  [[ "$ctx" == *'jj commit -m'* ]]
  [[ "$ctx" == *"$(cd "$repo" && pwd -P)"* ]]
  [[ "$ctx" == *'not integrate'* ]]
}

@test "a colocated repo's main workspace is still silent" {
  # Colocated repos have a .git alongside .jj; the discriminator must not be confused by it.
  d="$BATS_TEST_TMPDIR/colo"
  mkdir -p "$d"
  ( cd "$d" && jj git init --colocate >/dev/null 2>&1 &&
    printf 'a\n' >a.txt && jj commit -m first >/dev/null 2>&1 ) || return 1
  run in_dir "$d"
  [ "$status" -eq 0 ]
  [ -z "$output" ]
}
