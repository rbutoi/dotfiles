#!/usr/bin/env bats
#
# The hook REPLACES Claude's worktree handling in every repo once registered, so both halves of
# the jj/git split are pinned against the real tools: a jj repo must get a jj workspace, and a
# plain git repo must still get a working git worktree rather than an error.

load 'helper'

hook="$scripts/ws-worktree-hook.sh"

# create_in <cwd> <name>: the hook as Claude calls it, JSON on stdin.
create_in() { jq -n --arg cwd "$1" --arg name "$2" '{hook_event_name:"WorktreeCreate",cwd:$cwd,name:$name}' | env HOME="$BATS_TEST_TMPDIR/home" "$hook"; }
remove_at() { jq -n --arg p "$1" '{hook_event_name:"WorktreeRemove",worktree_path:$p}' | env HOME="$BATS_TEST_TMPDIR/home" "$hook"; }

new_git_repo() {
  local d="$BATS_TEST_TMPDIR/gitrepo"
  mkdir -p "$d" "$BATS_TEST_TMPDIR/home"
  # gpgsign off: the machine-wide config signs commits, which would block on a key prompt here.
  (cd "$d" && git init -q && git -c user.name=t -c user.email=t@t -c commit.gpgsign=false commit -q --allow-empty -m init) || return 1
  printf '%s' "$d"
}

@test "jj repo: creates a workspace through ws-create.sh and prints only its path" {
  repo=$(new_repo)
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  ws=$(cd "$repo-feat" && pwd -P)
  [ "$output" = "$ws" ]
  [ -n "$(env -C "$repo" jj workspace list --no-pager -T 'if(name == "feat", name)')" ]
}

@test "jj repo: a name with slashes becomes a valid workspace name" {
  repo=$(new_repo)
  run create_in "$repo" 'feat/x y'
  [ "$status" -eq 0 ]
  [ -d "$repo-feat-x-y" ]
}

@test "jj repo: creating a name that already exists resumes it" {
  repo=$(new_repo)
  first=$(create_in "$repo" feat)
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "$output" = "$first" ]
}

@test "jj repo: ws-create.sh's refusal is passed through for 'default'" {
  # `default` is the main checkout; handing it back as a worktree would let a remove target it.
  repo=$(new_repo)
  run create_in "$repo" default
  [ "$status" -ne 0 ]
  [[ "$output" == *"already exists"* ]]
}

@test "jj repo: removes the workspace by path, whatever its name" {
  repo=$(new_repo)
  ws=$(create_in "$repo" feat)
  run remove_at "$ws"
  [ "$status" -eq 0 ]
  [ ! -d "$ws" ]
  [ -z "$(env -C "$repo" jj workspace list --no-pager -T 'if(name == "feat", name)')" ]
}

@test "jj repo: refuses to remove a workspace with uncommitted changes" {
  repo=$(new_repo)
  ws=$(create_in "$repo" feat)
  printf 'x\n' >"$ws/new.txt"
  run remove_at "$ws"
  [ "$status" -ne 0 ]
  [[ "$output" == *"refusing to delete"* ]]
  [ -f "$ws/new.txt" ]
}

@test "git repo: still makes a git worktree on worktree-<name>" {
  repo=$(new_git_repo)
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  # Physical path: git reports /private/var where the fixture says /var (macOS tmp). `run` merges
  # stderr (git's progress lines) into $output; stdout, which Claude reads, is the last line.
  path=${lines[-1]}
  [ "$path" = "$(cd "$repo" && pwd -P)/.claude/worktrees/feat" ]
  [ "$(git -C "$path" rev-parse --abbrev-ref HEAD)" = worktree-feat ]
}

@test "git repo: re-entering an existing worktree resumes it instead of failing" {
  repo=$(new_git_repo)
  first=$(create_in "$repo" feat | tail -1)
  printf 'work\n' >"$first/wip.txt"
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "${lines[-1]}" = "$first" ]
  [ -f "$first/wip.txt" ]
}

@test "git repo: resumes a worktree that is on a branch not named after it" {
  # A worktree's branch is often renamed to a ticket-style name after it is made, so the directory
  # name alone has to identify it.
  repo=$(new_git_repo)
  path=$(create_in "$repo" topic | tail -1)
  git -C "$path" branch -m feat/ABC-1
  run create_in "$repo" topic
  [ "$status" -eq 0 ]
  [ "${lines[-1]}" = "$path" ]
  [ "$(git -C "$path" rev-parse --abbrev-ref HEAD)" = feat/ABC-1 ]
}

@test "git repo: reuses the branch when only the directory is gone" {
  repo=$(new_git_repo)
  path=$(create_in "$repo" feat | tail -1)
  git -C "$path" -c user.name=t -c user.email=t@t -c commit.gpgsign=false commit -q --allow-empty -m kept
  tip=$(git -C "$path" rev-parse HEAD)
  git -C "$repo" worktree remove "$path"
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "$(git -C "${lines[-1]}" rev-parse HEAD)" = "$tip" ]
}

@test "git repo: removes the worktree and its branch" {
  repo=$(new_git_repo)
  ws=$(create_in "$repo" feat)
  run remove_at "$ws"
  [ "$status" -eq 0 ]
  [ ! -d "$ws" ]
  [ -z "$(git -C "$repo" branch --list worktree-feat)" ]
}

@test "git repo: refuses to remove a worktree with uncommitted files" {
  repo=$(new_git_repo)
  ws=$(create_in "$repo" feat)
  printf 'x\n' >"$ws/new.txt"
  tracked() { git -C "$ws" add new.txt; }
  tracked
  run remove_at "$ws"
  [ "$status" -ne 0 ]
  [ -f "$ws/new.txt" ]
}

@test "neither jj nor git: fails with a message instead of printing nothing" {
  mkdir -p "$BATS_TEST_TMPDIR/plain" "$BATS_TEST_TMPDIR/home"
  run create_in "$BATS_TEST_TMPDIR/plain" feat
  [ "$status" -ne 0 ]
  [[ "$output" == *"not a jj or git repository"* ]]
}
