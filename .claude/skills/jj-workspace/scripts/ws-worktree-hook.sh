#!/usr/bin/env bash
# Claude Code WorktreeCreate / WorktreeRemove hook: makes `claude --worktree <name>`,
# EnterWorktree and worktree-isolated subagents create jj workspaces in a jj repo, via
# ws-create.sh / ws-remove.sh, so the naming, base and uncommitted-work guards live in those
# scripts only. Reads the hook's JSON on stdin; WorktreeCreate prints the new path on stdout.
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

# The repo's main worktree root, also when standing in a linked worktree.
git_root() {
  local common
  common=$(git -C "$1" rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || return 1
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
  # A worktree deleted by hand leaves its registration behind, and git then refuses both the path
  # and the branch until it is pruned. Prune only for that case: it also forgets every other
  # missing worktree (an unmounted drive's, say), which is not ours to touch otherwise. (`add -f`
  # would avoid the prune, but also overrides "branch is checked out elsewhere".)
  if [ ! -e "$path" ] && grep -qxF "worktree $path" <<<"$registered"; then
    git -C "$root" worktree prune >&2
  fi
  # A surviving branch keeps its commits: check it out again, since `-b` refuses to reuse a name.
  if git -C "$root" show-ref --verify --quiet "refs/heads/$branch"; then
    git -C "$root" worktree add "$path" "$branch" >&2
  else
    git -C "$root" worktree add -b "$branch" "$path" "$(resolve_base "$root")" >&2
  fi
  printf '%s\n' "$path"
}

remove() {
  local root branch name='' candidate physical
  [ -d "$worktree_path" ] || return 0

  if in_jj "$worktree_path"; then
    # The workspace's name isn't its directory's (ws-create.sh appends it to the repo's), so ask
    # jj which workspace lives here. ws-remove.sh refusing to delete uncommitted work is the
    # guard; its --force is the user's call and is never passed from here.
    physical=$(cd "$worktree_path" && pwd -P)
    while IFS= read -r candidate; do
      if [ "$(env -C "$worktree_path" jj workspace root --name "$candidate" 2>/dev/null)" = "$physical" ]; then
        name=$candidate
        break
      fi
    done < <(env -C "$worktree_path" jj workspace list --no-pager -T 'name ++ "\n"')
    [ -n "$name" ] || die "no jj workspace is rooted at $worktree_path"
    env -C "$worktree_path" "$here/ws-remove.sh" "$name" >&2
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
