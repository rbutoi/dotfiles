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
    "$scripts"/ws-create.sh "$scripts"/ws-merge.sh "$scripts"/ws-remove.sh
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

@test "ws-merge echoes the rebase, both halves of it" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$repo" "$scripts/ws-merge.sh" feat --yes
  [ "$status" -eq 0 ]
  [[ "$output" == *'+ jj rebase -s '* ]]
  [[ "$output" == *'+ jj rebase -r @ -d '* ]]
}

@test "ws-merge echoes the merge, with the message quoted so the line is pasteable" {
  # `-m Merge feat` is a different command from `-m 'Merge feat'`; an echo that can't be pasted
  # is worse than none, because it looks authoritative.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 2)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$repo" "$scripts/ws-merge.sh" feat -m 'Merge feat' --yes
  [ "$status" -eq 0 ]
  [[ "$output" == *"+ jj new "*" -m 'Merge feat'"* ]]
}

@test "ws-remove echoes the forget and the rm -rf" {
  # The rm -rf above all: it is the one irreversible thing in the skill.
  repo=$(new_repo)
  new_ws "$repo" feat 0 >/dev/null
  run env -C "$repo" "$scripts/ws-remove.sh" feat
  [ "$status" -eq 0 ]
  [[ "$output" == *'+ jj workspace forget feat'* ]]
  # Matched on the tail, not "$repo-feat": the script asks jj for the path and jj answers with
  # the physical one, so on macOS the echo says /private/tmp/... where $repo says /tmp/...
  [[ "$output" == *'+ rm -rf '*'/repo-feat'* ]]
}
