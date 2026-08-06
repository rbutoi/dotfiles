#!/usr/bin/env bats

load 'helper'

@test "a name is required" {
  run "$scripts/ws-create.sh"
  [ "$status" -ne 0 ]
  [[ "$output" == *'workspace name is required'* ]]
}

@test "odd characters in a name are rejected" {
  # The name becomes a directory name and a jj workspace id, so it stays boring.
  run "$scripts/ws-create.sh" 'bad name!'
  [ "$status" -ne 0 ]
  [[ "$output" == *'must be alphanumeric'* ]]
}

@test "refuses to nest a workspace inside the repo" {
  # Anything inside the repo gets scanned by formatters, watchers and test runners. The check
  # compares physical paths, because /tmp vs /private/tmp on macOS silently defeated it once.
  repo=$(new_repo)
  run env -C "$repo" "$scripts/ws-create.sh" feat --path "$repo/inside"
  [ "$status" -ne 0 ]
  [[ "$output" == *'refusing to nest'* ]]
}

@test "reports the path, the stack root, and the next step" {
  repo=$(new_repo)
  run env -C "$repo" "$scripts/ws-create.sh" feat
  [ "$status" -eq 0 ]
  [[ "$output" == *'path:'* ]]
  [[ "$output" == *'stack root:'* ]]
  [ -d "$repo-feat" ]
}

@test "the printed next-step paths point at the scripts that actually ran" {
  # Not a hard-coded ~/.claude path: this skill has to work from any install location, and the
  # user's ~/.claude is itself a symlink into a dotfiles repo.
  repo=$(new_repo)
  run env -C "$repo" "$scripts/ws-create.sh" feat
  [[ "$output" == *"$scripts/ws-merge.sh"* ]]
  [[ "$output" == *"$scripts/ws-remove.sh"* ]]
}

@test "the integrate hint says whose call it is" {
  repo=$(new_repo)
  run env -C "$repo" "$scripts/ws-create.sh" feat
  [[ "$output" == *'ws-merge.sh feat'* ]]
  [[ "$output" == *'never on your own initiative'* ]]
}

@test "the integrate hint is a command that actually runs" {
  # It once printed `ws-merge.sh` with nothing identifying the stack, which the merge script then
  # required — so the suggested line failed outright. Asserting on its text would not have caught
  # that; running it does.
  repo=$(new_repo)
  out=$(env -C "$repo" "$scripts/ws-create.sh" feat)
  ( cd "$repo-feat" && printf 'w\n' >w.txt && jj commit -m 'ws work 1' >/dev/null 2>&1 )
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  line=$(printf '%s\n' "$out" | grep -o "$scripts/ws-merge.sh.*" | head -1)
  [ -n "$line" ]
  run bash -c "cd '$repo' && $line"
  [ "$status" -eq 0 ]
  [[ "$output" == *plan* ]]
}

@test "a duplicate workspace name is refused" {
  repo=$(new_repo)
  env -C "$repo" "$scripts/ws-create.sh" feat >/dev/null 2>&1
  run env -C "$repo" "$scripts/ws-create.sh" feat
  [ "$status" -ne 0 ]
  [[ "$output" == *'already exists'* ]]
}

@test "a missing lockfile is reported rather than passed over in silence" {
  # Ignored files are not materialised, so a fresh workspace has no dependencies installed.
  # Saying nothing would read as "nothing to do".
  repo=$(new_repo)
  run env -C "$repo" "$scripts/ws-create.sh" nolock
  [[ "$output" == *'no pnpm/uv/cargo/go lockfile found'* ]]
}
