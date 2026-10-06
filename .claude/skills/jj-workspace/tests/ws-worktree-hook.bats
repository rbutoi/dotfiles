#!/usr/bin/env bats
#
# The hook REPLACES Claude's worktree handling in every repo once registered, so both halves of
# the jj/git split are pinned against the real tools: a jj repo must get a jj workspace, and a
# plain git repo must still get a working git worktree rather than an error.

load 'helper'

# The machine-wide git config may enable fsmonitor, whose per-repo daemon is spawned detached with
# bats' output pipe inherited and outlives the test, so a run never reaches EOF and hangs. Tests
# don't need it, and the hook under test inherits this too.
export GIT_CONFIG_COUNT=1 GIT_CONFIG_KEY_0=core.fsmonitor GIT_CONFIG_VALUE_0=false

hook="$scripts/ws-worktree-hook.sh"

# The hook gets its own HOME (where it reads settings.json); fixtures keep the real one.
setup() {
  home="$BATS_TEST_TMPDIR/home"
  mkdir -p "$home"
}

# create_in <cwd> <name>: the hook as Claude calls it, JSON on stdin.
create_in() { jq -n --arg cwd "$1" --arg name "$2" '{hook_event_name:"WorktreeCreate",cwd:$cwd,name:$name}' | env HOME="$home" "$hook"; }
remove_at() { jq -n --arg p "$1" '{hook_event_name:"WorktreeRemove",worktree_path:$p}' | env HOME="$home" "$hook"; }

# The path create_in printed: stdout is its last line, and `run` merges git's progress on stderr in.
mk_wt() { create_in "$@" | tail -1; }

# Physical path: git and jj report /private/var where the fixture says /var (macOS tmp).
phys() { (cd "$1" && pwd -P); }

# The branch checked out in a worktree.
branch_of() { git -C "$1" rev-parse --abbrev-ref HEAD; }

# plant_debris <path> [content]: a directory holding a file, as a package install leaves behind.
plant_debris() {
  mkdir -p "$1/common/temp"
  printf '%s\n' "${2:-x}" >"$1/common/temp/f.txt"
}

# git with identity and signing pinned: the machine-wide config signs commits, which would block
# on a key prompt here.
gc() { git -c user.name=t -c user.email=t@t -c commit.gpgsign=false "$@"; }

# new_git_repo [dir]: a repo with one empty commit.
new_git_repo() {
  local d="$BATS_TEST_TMPDIR/${1:-gitrepo}"
  mkdir -p "$d"
  (cd "$d" && git init -q && gc commit -q --allow-empty -m init) || return 1
  printf '%s' "$d"
}

# A repo whose origin/main is one commit AHEAD of its local main, with origin/HEAD set.
new_repo_behind_origin() {
  local r o="$BATS_TEST_TMPDIR/origin.git" other="$BATS_TEST_TMPDIR/other"
  r=$(new_git_repo)
  git -C "$r" branch -M main
  git init -q --bare -b main "$o"
  git -C "$r" remote add origin "$o"
  git -C "$r" push -q origin main 2>/dev/null
  git -C "$r" remote set-head origin main >/dev/null
  git clone -q "$o" "$other" 2>/dev/null
  gc -C "$other" commit -q --allow-empty -m "ahead on origin"
  git -C "$other" push -q origin main 2>/dev/null
  printf '%s' "$r"
}

# ---- jj repos -------------------------------------------------------------------------------

@test "jj repo: creates a workspace through ws-create.sh and prints only its path" {
  repo=$(new_repo)
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "$output" = "$(phys "$repo-feat")" ]
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

@test "jj repo: refuses to remove the main checkout" {
  repo=$(new_repo)
  run remove_at "$repo"
  [ "$status" -ne 0 ]
  [[ "$output" == *"main checkout"* ]]
  [ -d "$repo/.jj" ]
}

@test "jj repo: removing keeps the workspace's committed work reachable" {
  repo=$(new_repo)
  ws=$(create_in "$repo" feat)
  (cd "$ws" && printf 'w\n' >w.txt && jj commit -m 'kept' >/dev/null 2>&1)
  run remove_at "$ws"
  [ "$status" -eq 0 ]
  [ ! -d "$ws" ]
  [ -n "$(env -C "$repo" jj log --no-pager --no-graph -r 'description(exact:"kept\n")' -T 'change_id')" ]
}

@test "jj repo: a repo path containing spaces" {
  repo=$(new_repo 'my repo')
  ws=$(create_in "$repo" feat)
  [ -d "$ws" ]
  run remove_at "$ws"
  [ "$status" -eq 0 ]
  [ ! -d "$ws" ]
}

@test "jj repo: asked from a subdirectory" {
  repo=$(new_repo)
  mkdir -p "$repo/deep/sub"
  run create_in "$repo/deep/sub" feat
  [ "$status" -eq 0 ]
  [ -d "$repo-feat" ]
}

@test "jj repo: asked from inside a secondary workspace" {
  repo=$(new_repo)
  first=$(create_in "$repo" one)
  run create_in "$first" two
  [ "$status" -eq 0 ]
  # Flat: a sibling of the main checkout, not `<repo>-one-two`.
  [ "${lines[-1]}" = "$(phys "$repo-two")" ]
  [ ! -e "$repo-one-two" ]
}

@test "jj repo: removing with a trailing slash on the path" {
  repo=$(new_repo)
  ws=$(create_in "$repo" feat)
  run remove_at "$ws/"
  [ "$status" -eq 0 ]
  [ ! -d "$ws" ]
}

# ---- git repos: creating --------------------------------------------------------------------

@test "git repo: still makes a git worktree on worktree-<name>" {
  repo=$(new_git_repo)
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  path=${lines[-1]}
  [ "$path" = "$(phys "$repo")/.claude/worktrees/feat" ]
  [ "$(branch_of "$path")" = worktree-feat ]
}

@test "git repo: re-entering an existing worktree resumes it instead of failing" {
  repo=$(new_git_repo)
  first=$(mk_wt "$repo" feat)
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
  path=$(mk_wt "$repo" topic)
  git -C "$path" branch -m feat/ABC-1
  run create_in "$repo" topic
  [ "$status" -eq 0 ]
  [ "${lines[-1]}" = "$path" ]
  [ "$(branch_of "$path")" = feat/ABC-1 ]
}

@test "git repo: reuses the branch when only the directory is gone" {
  repo=$(new_git_repo)
  path=$(mk_wt "$repo" feat)
  gc -C "$path" commit -q --allow-empty -m kept
  tip=$(git -C "$path" rev-parse HEAD)
  git -C "$repo" worktree remove "$path"
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "$(git -C "${lines[-1]}" rev-parse HEAD)" = "$tip" ]
}

@test "git repo: a name with a slash is flattened, not nested inside another worktree" {
  # `feat/x` as a path would land inside the `feat` worktree. Claude flattens '/' to '+'.
  repo=$(new_git_repo)
  outer=$(mk_wt "$repo" feat)
  run create_in "$repo" feat/x
  [ "$status" -eq 0 ]
  inner=${lines[-1]}
  [ "$inner" = "$(phys "$repo")/.claude/worktrees/feat+x" ]
  [[ "$inner" != "$outer"/* ]]
  [ -d "$inner" ]
}

@test "git repo: a name cannot escape .claude/worktrees" {
  repo=$(new_git_repo)
  run create_in "$repo" '../escape'
  [ "$status" -eq 0 ]
  [[ "${lines[-1]}" == "$(phys "$repo")/.claude/worktrees/"* ]]
  [ ! -e "$repo/.claude/escape" ]
}

@test "git repo: a name with spaces or a leading dash makes a valid branch" {
  repo=$(new_git_repo)
  run create_in "$repo" '-my topic'
  [ "$status" -eq 0 ]
  git -C "${lines[-1]}" check-ref-format --branch "$(branch_of "${lines[-1]}")"
}

@test "git repo: names git cannot use fail instead of making a worktree" {
  repo=$(new_git_repo)
  for bad in '..' '' 'trailing.' 'a..b' 'x.lock'; do
    run create_in "$repo" "$bad"
    [ "$status" -ne 0 ]
  done
  [ "$(git -C "$repo" worktree list | wc -l)" -eq 1 ]
}

@test "git repo: asked from inside a linked worktree, the new one still lands under the main repo" {
  repo=$(new_git_repo)
  first=$(mk_wt "$repo" one)
  run create_in "$first" two
  [ "$status" -eq 0 ]
  [ "${lines[-1]}" = "$(phys "$repo")/.claude/worktrees/two" ]
}

@test "git repo: asked from a subdirectory of the repo" {
  repo=$(new_git_repo)
  mkdir -p "$repo/deep/sub"
  run create_in "$repo/deep/sub" feat
  [ "$status" -eq 0 ]
  [ "${lines[-1]}" = "$(phys "$repo")/.claude/worktrees/feat" ]
}

@test "git repo: a worktree whose directory was deleted by hand can be created again" {
  # rm -rf leaves a registration behind; git then refuses the branch and the path until pruned.
  repo=$(new_git_repo)
  path=$(mk_wt "$repo" feat)
  rm -rf "$path"
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ -d "${lines[-1]}" ]
}

@test "git repo: a non-worktree directory holding files at the path is moved aside intact" {
  # Debris must neither block the name nor lose anything.
  repo=$(new_git_repo)
  plant_debris "$repo/.claude/worktrees/feat" mine
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "${lines[-1]}" = "$(phys "$repo")/.claude/worktrees/feat" ]
  [ "$(branch_of "${lines[-1]}")" = worktree-feat ]
  [ "$(cat "$repo"/.git/orphaned-worktrees/feat-*/common/temp/f.txt)" = mine ]
}

@test "git repo: a non-worktree tree of empty directories at the path is just removed" {
  repo=$(new_git_repo)
  mkdir -p "$repo/.claude/worktrees/feat/a/b"
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ ! -e "$repo/.git/orphaned-worktrees" ]
}

@test "git repo: a name can be re-entered after removal even if debris took its place" {
  repo=$(new_git_repo)
  path=$(mk_wt "$repo" feat)
  run remove_at "$path"
  [ "$status" -eq 0 ]
  plant_debris "$path"
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "$(branch_of "${lines[-1]}")" = worktree-feat ]
}

@test "git repo: a branch with no worktree and an occupied path still yields a worktree" {
  repo=$(new_git_repo)
  git -C "$repo" branch worktree-feat
  plant_debris "$repo/.claude/worktrees/feat"
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "$(branch_of "${lines[-1]}")" = worktree-feat ]
}

@test "git repo: a creation that fails leaves no branch behind" {
  # `git worktree add -b` makes the branch before the rest can fail; here `.claude` is a file, so
  # the worktrees directory cannot be made.
  repo=$(new_git_repo)
  printf 'not a directory\n' >"$repo/.claude"
  run create_in "$repo" feat
  [ "$status" -ne 0 ]
  [ -z "$(git -C "$repo" branch --list worktree-feat)" ]
}

@test "git repo: a repo with no commits fails rather than printing a path" {
  d="$BATS_TEST_TMPDIR/empty"
  mkdir -p "$d"
  git -C "$d" init -q
  run create_in "$d" feat
  [ "$status" -ne 0 ]
}

@test "git repo: a repo path containing spaces" {
  repo=$(new_git_repo 'my repo')
  path=$(mk_wt "$repo" feat)
  [ -d "$path" ]
  run remove_at "$path"
  [ "$status" -eq 0 ]
}

@test "neither jj nor git: fails with a message instead of printing nothing" {
  mkdir -p "$BATS_TEST_TMPDIR/plain"
  run create_in "$BATS_TEST_TMPDIR/plain" feat
  [ "$status" -ne 0 ]
  [[ "$output" == *"not a jj or git repository"* ]]
}

# ---- git repos: where a new worktree starts -------------------------------------------------

@test "git repo: baseRef fresh branches from origin's tip, not the local checkout" {
  repo=$(new_repo_behind_origin)
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "$(git -C "${lines[-1]}" rev-parse HEAD)" = "$(git -C "$repo" rev-parse origin/main)" ]
  [ "$(git -C "${lines[-1]}" rev-parse HEAD)" != "$(git -C "$repo" rev-parse main)" ]
}

@test "git repo: baseRef head branches from the local checkout" {
  repo=$(new_repo_behind_origin)
  mkdir -p "$home/.claude"
  printf '{"worktree":{"baseRef":"head"}}\n' >"$home/.claude/settings.json"
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "$(git -C "${lines[-1]}" rev-parse HEAD)" = "$(git -C "$repo" rev-parse main)" ]
}

@test "git repo: an unreachable origin falls back to the local checkout" {
  repo=$(new_repo_behind_origin)
  git -C "$repo" remote set-url origin "$BATS_TEST_TMPDIR/nonexistent.git"
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "$(git -C "${lines[-1]}" rev-parse HEAD)" = "$(git -C "$repo" rev-parse main)" ]
}

@test "git repo: origin/HEAD unset but origin/main present still bases on origin" {
  repo=$(new_repo_behind_origin)
  git -C "$repo" remote set-head origin -d >/dev/null
  run create_in "$repo" feat
  [ "$status" -eq 0 ]
  [ "$(git -C "${lines[-1]}" rev-parse HEAD)" = "$(git -C "$repo" rev-parse origin/main)" ]
}

# ---- git repos: removing --------------------------------------------------------------------

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
  git -C "$ws" add new.txt
  run remove_at "$ws"
  [ "$status" -ne 0 ]
  [ -f "$ws/new.txt" ]
}

@test "remove: a git worktree on a branch the hook did not name keeps that branch" {
  repo=$(new_git_repo)
  path=$(mk_wt "$repo" topic)
  git -C "$path" branch -m feat/ABC-1
  run remove_at "$path"
  [ "$status" -eq 0 ]
  [ ! -d "$path" ]
  [ -n "$(git -C "$repo" branch --list feat/ABC-1)" ]
}

@test "remove: unmerged commits on the hook's branch are not lost with the worktree" {
  repo=$(new_git_repo)
  path=$(mk_wt "$repo" feat)
  gc -C "$path" commit -q --allow-empty -m "unmerged work"
  tip=$(git -C "$path" rev-parse HEAD)
  run remove_at "$path"
  [ "$status" -eq 0 ]
  [ ! -d "$path" ]
  git -C "$repo" cat-file -e "$tip"
  [ -n "$(git -C "$repo" branch --list worktree-feat)" ]
}

@test "remove: a path that is already gone succeeds" {
  repo=$(new_git_repo)
  run remove_at "$repo/.claude/worktrees/never-existed"
  [ "$status" -eq 0 ]
}
