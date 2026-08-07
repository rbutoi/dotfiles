#!/usr/bin/env bats

load 'helper'

@test "refuses the default workspace" {
  repo=$(new_repo)
  run env -C "$repo" "$scripts/ws-remove.sh" default
  [ "$status" -ne 0 ]
  [[ "$output" == *"refusing to remove the 'default'"* ]]
}

@test "refuses an unknown workspace" {
  repo=$(new_repo)
  run env -C "$repo" "$scripts/ws-remove.sh" nosuch
  [ "$status" -ne 0 ]
  [[ "$output" == *'cannot locate'* ]]
}

@test "refuses to run from inside the workspace it would delete" {
  # rm -rf of the directory you're standing in leaves the shell somewhere that no longer exists.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  run env -C "$ws" "$scripts/ws-remove.sh" feat
  [ "$status" -ne 0 ]
  [[ "$output" == *'from outside'* ]]
}

@test "uncommitted changes block the delete, and survive the refusal" {
  # The one genuinely lossy case. This also covers the snapshot bug: the edit is made but never
  # committed, so an unsnapshotted read of `feat@` looks empty and the rm would have proceeded.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  printf 'dirty\n' >"$ws/dirty.txt"
  run env -C "$repo" "$scripts/ws-remove.sh" feat
  [ "$status" -ne 0 ]
  [[ "$output" == *'refusing to delete'* ]]
  # Same rule as ws-merge.sh's dry-run footer, at this script's own point of temptation: the
  # refusal names the flag that overrides it, and that flag is an irreversible rm of work the
  # user has never seen.
  [[ "$output" == *"--force is the user's decision, not the agent's"* ]]
  [ -d "$ws" ]
  [ -f "$ws/dirty.txt" ]
}

@test "a stale working copy is refused rather than read as clean" {
  # jj won't answer questions about a stale working copy, and a swallowed error would look like
  # "no uncommitted changes" — deleting them. Routine, not exotic: integrating a single-commit
  # stack rebases it, and a rebase run from anywhere else leaves that workspace stale.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  printf 'dirty\n' >"$ws/dirty.txt"
  env -C "$ws" jj status >/dev/null 2>&1 # snapshot the dirt into feat@
  root=$(env -C "$ws" jj log --no-pager --no-graph -r "$STACK_ROOT" -T 'change_id.short()')
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  env -C "$repo" jj rebase -s "$root" -d 'default@-' >/dev/null 2>&1

  run env -C "$repo" "$scripts/ws-remove.sh" feat
  [ "$status" -ne 0 ]
  [[ "$output" == *stale* ]]
  [[ "$output" == *'update-stale'* ]] # and names the fix
  [ -f "$ws/dirty.txt" ]
}

@test "--force deletes despite uncommitted changes, and reports what it did" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  printf 'dirty\n' >"$ws/dirty.txt"
  run env -C "$repo" "$scripts/ws-remove.sh" feat --force
  [ "$status" -eq 0 ]
  [[ "$output" == *'forgot workspace feat'* ]]
  [ ! -d "$ws" ]
}

@test "committed work survives removal" {
  # `forget` keeps every real commit and only auto-abandons the trailing empty @ — which is why
  # an unmerged stack is reported as a note rather than blocking.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  kept=$(env -C "$ws" jj log --no-pager --no-graph -r 'description(substring:"ws work 1")' \
    -T 'change_id.short()')
  # Assert the fixture resolved: `description()` defaults to an EXACT match and jj stores a
  # trailing newline, so the unqualified form silently matches nothing — which made this
  # comparison "" = "" and the test pass while checking nothing at all.
  [ -n "$kept" ]
  env -C "$repo" "$scripts/ws-remove.sh" feat >/dev/null 2>&1
  [ "$(env -C "$repo" jj log --no-pager --no-graph -r "$kept" -T 'change_id.short()')" = "$kept" ]
}

@test "a workspace created at a custom --path is removable by name" {
  # The regression this pins: removal used to re-derive ws-create's sibling naming convention,
  # so anything made with --path could not be removed by name. It asks jj for the path now.
  repo=$(new_repo)
  custom="$BATS_TEST_TMPDIR/elsewhere"
  env -C "$repo" "$scripts/ws-create.sh" odd --path "$custom" >/dev/null 2>&1
  [ -d "$custom" ]
  run env -C "$repo" "$scripts/ws-remove.sh" odd
  [ "$status" -eq 0 ]
  [ ! -d "$custom" ]
}
