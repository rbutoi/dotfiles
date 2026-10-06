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

@test "the primary next step is starting a session in the workspace" {
  # Claude resets the shell cwd after every call unless the directory is in the session's allowed
  # set, and a workspace is a SIBLING of the repo, so it never is. Nothing in a skill can call
  # /add-dir. A session started in the workspace has it as its own cwd, so nothing needs prefixing.
  repo=$(new_repo)
  # The script canonicalizes with `pwd -P`, and $BATS_TEST_TMPDIR sits under a symlinked
  # /var/folders on macOS — so compare against the resolved path, not the one new_repo returned.
  ws="$(cd "${repo%/*}" && pwd -P)/${repo##*/}-feat"
  run env -C "$repo" "$scripts/ws-create.sh" feat
  [[ "$output" == *"cd $ws; claude"* ]]
  [[ "$output" == *'start a session in it'* ]]
}

@test "a root lockfile is folded into the launch line, a subproject one is not" {
  repo=$(new_repo)
  ( cd "$repo" && mkdir -p sub && : >pnpm-lock.yaml && : >sub/uv.lock &&
    jj commit -m 'locks' >/dev/null 2>&1 )
  ws="$(cd "${repo%/*}" && pwd -P)/${repo##*/}-feat"
  run env -C "$repo" "$scripts/ws-create.sh" feat
  [[ "$output" == *"cd $ws; pnpm install; claude"* ]]
  # The subproject one still gets listed, just not inlined — it would need a cd back.
  [[ "$output" == *'uv sync'* ]]
  [[ "$output" != *'uv sync; claude'* ]]
}

@test "the output tells you to commit as you go" {
  # Observed failure, twice: an agent works in the workspace for a whole task and lands 2-3 logical
  # changes in one nameless @, because jj auto-snapshots and so nothing ever reads as uncommitted.
  # SKILL.md says it too, but this output is what's in front of you when work starts.
  repo=$(new_repo)
  run env -C "$repo" "$scripts/ws-create.sh" feat
  [[ "$output" == *'commit after each logical change'* ]]
  [[ "$output" == *'jj commit -m'* ]]
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

@test "a workspace can still be created while standing on an empty merge" {
  # `@-` means both parents there, which is how a hand-rolled `jj new A B` (or a merge another
  # tool left unsealed) leaves the main workspace.
  repo=$(new_repo)
  new_ws "$repo" feat 2 >/dev/null
  env -C "$repo" jj new 'default@-' 'feat@-' >/dev/null 2>&1
  run env -C "$repo" "$scripts/ws-create.sh" next
  [ "$status" -eq 0 ]
  [[ "$output" == *'sealing it into a commit first'* ]]
  # Based on the merge, so both sides of the join are in the new workspace's tree.
  [ -f "$repo-next/w2.txt" ]
}

@test "created from inside another workspace, the new one is a sibling of the main checkout" {
  # The name hangs off the main checkout, not the workspace it ran in: `foo-bar` inside `foo` would
  # make the list of workspaces a chain instead of a flat set.
  repo=$(new_repo)
  foo=$(new_ws "$repo" foo)
  run env -C "$foo" "$scripts/ws-create.sh" bar
  [ "$status" -eq 0 ]
  [ -d "$repo-bar" ]
  [ ! -e "$repo-foo-bar" ]
  [[ "$output" == *"path:        $(cd "$repo-bar" && pwd -P)"* ]]
}

@test "the sibling name still anchors on the current workspace when there is no 'default'" {
  # A repo whose main workspace was renamed has nothing to anchor on, so it falls back rather than
  # failing.
  repo=$(new_repo)
  env -C "$repo" jj workspace rename main >/dev/null 2>&1
  run env -C "$repo" "$scripts/ws-create.sh" bar
  [ "$status" -eq 0 ]
  [ -d "$repo-bar" ]
}
