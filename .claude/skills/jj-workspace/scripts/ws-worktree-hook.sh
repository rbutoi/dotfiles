#!/usr/bin/env bash
# Claude Code WorktreeCreate / WorktreeRemove hook: makes `claude --worktree <name>`,
# EnterWorktree and worktree-isolated subagents create jj workspaces in a jj repo: creation goes
# through ws-create.sh, so naming and base are defined there only, and removal refuses to delete
# uncommitted work. Reads the hook's JSON on stdin; WorktreeCreate prints the new path on stdout.
# Outside jj it makes the git worktree itself. DESIGN.md ("The WorktreeCreate / WorktreeRemove
# hook") says why that is necessary and why this is a settings hook rather than a plugin.
#
# Register it in ~/.claude/settings.json (not tracked by this repo), one entry per event:
#
#   "hooks": {
#     "WorktreeCreate": [{"hooks": [{"type": "command", "command": "$HOME/.claude/skills/jj-workspace/scripts/ws-worktree-hook.sh"}]}],
#     "WorktreeRemove": [{"hooks": [{"type": "command", "command": "$HOME/.claude/skills/jj-workspace/scripts/ws-worktree-hook.sh"}]}]
#   }

# event, cwd, raw_name and worktree_path are assigned by the eval below.
# shellcheck disable=SC2154
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd -P)
# shellcheck source=_common.sh
. "$here/_common.sh"

# One jq call for every field. `@sh` quotes each value, so nothing Claude passes as a name can
# reach the shell as syntax.
eval "$(jq -r '@sh "event=\(.hook_event_name) cwd=\(.cwd // "") raw_name=\(.name // "") worktree_path=\(.worktree_path // "")"')"

# Matches Claude's own worktree branches, so the hook's and the built-in's are interchangeable.
branch_prefix=worktree-

# jj: the name becomes a directory and a workspace id; keep it to what ws-create.sh accepts.
sanitize() { printf '%s' "$1" | sed -E 's/[^A-Za-z0-9._-]/-/g; s/^[^A-Za-z0-9]+//'; }

# git: the name also becomes a branch and ONE directory under .claude/worktrees, so a '/' is
# flattened to '+' as Claude does (a nested path would land inside another worktree), and leading
# punctuation goes, which also stops `..` climbing out. git itself judges what is left.
git_name() { printf '%s' "$1" | tr '/' '+' | sed -E 's/[^A-Za-z0-9._+-]/-/g; s/^[^A-Za-z0-9]+//'; }

git_common_dir() { git -C "$1" rev-parse --path-format=absolute --git-common-dir 2>/dev/null; }

# The repo's main worktree root, also when standing in a linked worktree.
git_root() {
  local common
  common=$(git_common_dir "$1") || return 1
  case "$common" in
    */.git) printf '%s' "${common%/.git}" ;;
    *) git -C "$1" rev-parse --show-toplevel ;;
  esac
}

in_jj() { env -C "$1" jj workspace root >/dev/null 2>&1; }

# Where a new git worktree starts: origin's default branch, freshly fetched, unless settings say
# `worktree.baseRef: head`. No origin, or one that can't be reached, means the local HEAD.
resolve_base() {
  local root=$1 ref
  if [ "$(jq -r '.worktree.baseRef // ""' "$HOME/.claude/settings.json" 2>/dev/null || true)" = head ]; then
    echo HEAD
    return
  fi
  # `git remote add` (unlike clone) leaves origin/HEAD unset, hence the guesses after it.
  for ref in "$(git -C "$root" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null || true)" origin/main origin/master; do
    [ -n "$ref" ] || continue
    git -C "$root" show-ref --verify --quiet "refs/remotes/$ref" || continue
    if git -C "$root" fetch --quiet origin "${ref#origin/}" >&2; then
      echo "$ref"
      return
    fi
    break
  done
  echo HEAD
}

create_jj() {
  local name out path
  name=$(sanitize "$raw_name")
  # Re-entering a workspace that exists is a resume, as in git. `default` is the main checkout,
  # never a worktree to hand back, so that one falls through to ws-create.sh's refusal.
  if [ "$name" != default ] && path=$(env -C "$cwd" jj workspace root --name "$name" 2>/dev/null); then
    printf '%s\n' "$path"
    return
  fi
  # ws-create.sh narrates; its handles are `key: value` lines, and the path is the one needed.
  out=$(env -C "$cwd" "$here/ws-create.sh" "$name" 2>&1) || {
    printf '%s\n' "$out" >&2
    exit 1
  }
  path=$(sed -n 's/^path: *//p' <<<"$out" | head -1)
  [ -n "$path" ] || die "ws-create.sh printed no path: line"
  printf '%s\n' "$path"
}

# Make $2 (the worktree path for name $3) free to `git worktree add` into; $4 is the repo's
# `worktree list --porcelain`. Two things can occupy it without being a worktree.
free_path() {
  local root=$1 path=$2 name=$3 registered=$4 aside
  # A worktree deleted by hand leaves its registration behind, and git then refuses both the path
  # and the branch until it is pruned. Prune only for that case: it also forgets every other
  # missing worktree (an unmounted drive's, say), which is not ours to touch otherwise. (`add -f`
  # would avoid the prune, but also overrides "branch is checked out elsewhere".)
  if [ ! -e "$path" ]; then
    grep -qxF "worktree $path" <<<"$registered" && git -C "$root" worktree prune >&2
    return 0
  fi
  # Debris: a tool whose cwd was a removed worktree (a package install, say) recreated the
  # directory. Git refuses to add over it. A tree of empty directories is removed; anything holding
  # a file is moved into the repo's .git, where nothing scans or commits it and no file is lost.
  # The pid keeps two runs within the same second from landing in one directory.
  if [ -z "$(find "$path" ! -type d -print -quit)" ]; then
    rm -rf "$path"
  else
    aside="$(git_common_dir "$root")/orphaned-worktrees/$name-$(date +%s)-$$"
    mkdir -p "${aside%/*}"
    mv "$path" "$aside"
    warn "moved the leftover directory at $path to $aside"
  fi
}

create_git() {
  local root name path branch registered
  root=$(git_root "$cwd") || die "not a jj or git repository: $cwd"
  name=$(git_name "$raw_name")
  branch=$branch_prefix$name
  { [ -n "$name" ] && git check-ref-format "refs/heads/$branch"; } || die "'$raw_name' cannot be used as a worktree name"
  path="$root/.claude/worktrees/$name"
  registered=$(git -C "$root" worktree list --porcelain)

  # `--worktree <name>` re-enters a worktree that is already there — whatever branch it is on —
  # rather than failing because the name is taken. `worktree list` prints physical paths, so
  # compare against the physical form of ours.
  if [ -d "$path" ] && grep -qxF "worktree $(cd "$path" && pwd -P)" <<<"$registered"; then
    printf '%s\n' "$path"
    return
  fi
  free_path "$root" "$path" "$name" "$registered"
  # A surviving branch keeps its commits: check it out again, since `-b` refuses to reuse a name.
  if git -C "$root" show-ref --verify --quiet "refs/heads/$branch"; then
    git -C "$root" worktree add "$path" "$branch" >&2
  else
    # `add -b` makes the branch before anything else can fail (an unwritable parent, a full disk),
    # so a failure must not strand a branch with no worktree for the next attempt to trip over.
    git -C "$root" worktree add -b "$branch" "$path" "$(resolve_base "$root")" >&2 || {
      git -C "$root" branch -D "$branch" >/dev/null 2>&1
      exit 1
    }
  fi
  printf '%s\n' "$path"
}

remove() {
  local root branch name='' candidate physical probe
  [ -d "$worktree_path" ] || return 0

  if in_jj "$worktree_path"; then
    # The workspace's name isn't its directory's (ws-create.sh appends it to the repo's), so ask
    # jj which workspace lives here.
    physical=$(cd "$worktree_path" && pwd -P)
    while IFS= read -r candidate; do
      if [ "$(env -C "$worktree_path" jj workspace root --name "$candidate" 2>/dev/null)" = "$physical" ]; then
        name=$candidate
        break
      fi
    done < <(env -C "$worktree_path" jj workspace list --no-pager -T 'name ++ "\n"')
    [ -n "$name" ] || die "no jj workspace is rooted at $worktree_path"
    [ "$name" != default ] || die "refusing to remove the main checkout ($worktree_path)"
    # Uncommitted changes are the one lossy case: `forget` leaves every commit in the repo, so
    # committed work survives the rm. jj only records a working copy when a command runs with that
    # workspace as its cwd, so probe from inside it — that snapshots first, and a stale working copy
    # (its commits were rebased underneath it) fails here instead of reading as clean.
    probe=$(env -C "$worktree_path" jj status --no-pager 2>&1) ||
      die "could not read workspace '$name' (stale? run \`jj workspace update-stale\` in it): ${probe%%$'\n'*}"
    [ -z "$(env -C "$worktree_path" jj log --no-pager --no-graph -r '@ & ~empty()' -T '1' 2>/dev/null)" ] ||
      die "refusing to delete workspace '$name': it has uncommitted changes ($worktree_path). Commit or abandon them first."
    env -C "$worktree_path" jj workspace forget "$name" >&2
    rm -rf "$worktree_path"
    return
  fi

  root=$(git_root "$worktree_path") || return 0
  branch=$(git -C "$worktree_path" rev-parse --abbrev-ref HEAD 2>/dev/null || true)
  # No --force: a worktree with uncommitted files is refused, mirroring the jj side. Only a branch
  # this hook (or Claude) named is deleted, and `-d` keeps one with unmerged commits.
  git -C "$root" worktree remove "$worktree_path" >&2
  case "$branch" in
    "$branch_prefix"*) git -C "$root" branch -d "$branch" >&2 || true ;;
  esac
}

case "$event" in
  WorktreeCreate) if in_jj "$cwd"; then create_jj; else create_git; fi ;;
  WorktreeRemove) remove ;;
esac
