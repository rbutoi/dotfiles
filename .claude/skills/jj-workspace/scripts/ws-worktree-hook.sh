#!/usr/bin/env bash
# Claude Code WorktreeCreate / WorktreeRemove hook: makes `claude --worktree <name>`,
# EnterWorktree and worktree-isolated subagents create jj workspaces in a jj repo, via
# ws-create.sh / ws-remove.sh, so the naming, base and uncommitted-work guards live in those
# scripts only. Reads the hook's JSON on stdin; WorktreeCreate prints the new path on stdout.
#
# Register it in ~/.claude/settings.json (not tracked by this repo), one entry per event:
#
#   "hooks": {
#     "WorktreeCreate": [{"hooks": [{"type": "command", "command": "$HOME/.claude/skills/jj-workspace/scripts/ws-worktree-hook.sh"}]}],
#     "WorktreeRemove": [{"hooks": [{"type": "command", "command": "$HOME/.claude/skills/jj-workspace/scripts/ws-worktree-hook.sh"}]}]
#   }
#
# A configured hook REPLACES Claude's own worktree handling in every repo — printing nothing is
# an error, not a fall-through — so outside jj this makes the git worktree itself, as Claude
# does: <repo>/.claude/worktrees/<name> on branch worktree-<name>, from origin's default branch
# (falling back to HEAD when that can't be fetched, or when settings `worktree.baseRef` is
# `head`). Claude's extras beyond that (copying local settings, symlinked directories) are not
# reproduced.
#
# A mod (plugin hooks module) cannot do this job: the engine bypasses user-tier hooks on these
# events, and `--worktree` creates the worktree before plugin modules load.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd -P)

input=$(cat)
event=$(jq -r '.hook_event_name' <<<"$input")

# Names reach a directory and a branch; keep them to what ws-create.sh accepts.
sanitize() { printf '%s' "$1" | sed -E 's/[^A-Za-z0-9._-]/-/g; s/^[^A-Za-z0-9]+//'; }

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

create() {
  local cwd name out path root base branch ref
  cwd=$(jq -r '.cwd' <<<"$input")
  name=$(jq -r '.name' <<<"$input")

  if in_jj "$cwd"; then
    name=$(sanitize "$name")
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
    path=$(printf '%s\n' "$out" | sed -n 's/^path: *//p' | head -1)
    [ -n "$path" ] || {
      printf 'ws-create.sh printed no path: line:\n%s\n' "$out" >&2
      exit 1
    }
    printf '%s\n' "$path"
    return
  fi

  root=$(git_root "$cwd") || {
    printf 'not a jj or git repository: %s\n' "$cwd" >&2
    exit 1
  }
  path="$root/.claude/worktrees/$name"
  branch="worktree-$name"

  # `--worktree <name>` re-enters a worktree that is already there — whatever branch it is on —
  # rather than failing because the name is taken. `worktree list` prints physical paths, so
  # compare against the physical form of ours.
  if [ -d "$path" ] && git -C "$root" worktree list --porcelain |
    grep -qxF "worktree $(cd "$path" && pwd -P)"; then
    printf '%s\n' "$path"
    return
  fi
  # The directory went away but the branch (and its commits) did not: check that branch out again
  # instead of `-b`, which refuses to reuse a name.
  if git -C "$root" show-ref --verify --quiet "refs/heads/$branch"; then
    git -C "$root" worktree add "$path" "$branch" >&2
    printf '%s\n' "$path"
    return
  fi

  base=HEAD
  if [ "$(jq -r '.worktree.baseRef // "fresh"' "$HOME/.claude/settings.json" 2>/dev/null || echo fresh)" = fresh ]; then
    ref=$(git -C "$root" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null || true)
    # `origin/main` -> fetch `main`; no origin or an offline fetch leaves HEAD as the base.
    if [ -n "$ref" ] && git -C "$root" fetch --quiet origin "${ref#origin/}" >&2 2>&1; then
      base=$ref
    fi
  fi
  git -C "$root" worktree add -b "$branch" "$path" "$base" >&2
  printf '%s\n' "$path"
}

remove() {
  local path name root candidate branch
  path=$(jq -r '.worktree_path' <<<"$input")
  [ -d "$path" ] || exit 0

  if in_jj "$path"; then
    # The workspace's name isn't its directory's (ws-create.sh appends it to the repo's), so ask
    # jj which workspace lives here. ws-remove.sh refusing to delete uncommitted work is the
    # guard; its --force is the user's call and is never passed from here.
    name=''
    while IFS= read -r candidate; do
      [ -n "$candidate" ] || continue
      if [ "$(env -C "$path" jj workspace root --name "$candidate" 2>/dev/null)" = "$(cd "$path" && pwd -P)" ]; then
        name=$candidate
        break
      fi
    done < <(env -C "$path" jj workspace list --no-pager -T 'name ++ "\n"')
    [ -n "$name" ] || {
      printf 'no jj workspace is rooted at %s\n' "$path" >&2
      exit 1
    }
    env -C "$path" "$here/ws-remove.sh" "$name" >&2
    return
  fi

  root=$(git_root "$path") || exit 0
  branch=$(git -C "$path" rev-parse --abbrev-ref HEAD 2>/dev/null || true)
  # No --force: a worktree with uncommitted files is refused, mirroring the jj side.
  git -C "$root" worktree remove "$path" >&2
  case "$branch" in
    worktree-*) git -C "$root" branch -d "$branch" >&2 2>&1 || true ;;
  esac
}

case "$event" in
  WorktreeCreate) create ;;
  WorktreeRemove) remove ;;
  *) ;;
esac
