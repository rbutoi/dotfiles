#!/usr/bin/env bash
# SessionStart hook: when a Claude session starts inside a SECONDARY jj workspace, tell it so.
#
# Why this exists: the jj-workspace skill is invoked in the session that CREATES a workspace, but
# the work happens in a session started inside it (SKILL.md §2 — that session has the workspace as
# its own cwd, so nothing needs a `cd` prefix). That session loads the repo's own CLAUDE.md /
# AGENTS.md, because the workspace is the same repo content, but NOT the skill. These few rules are
# the part it would otherwise be missing, and the commit cadence is the one that actually bites.
#
# Detection: `.jj/repo` is a DIRECTORY in a main workspace and a FILE (holding a path to the main
# repo) in a secondary one. That is an on-disk detail rather than documented API, but jj offers no
# first-class "am I in a secondary workspace" query — `jj root` and `jj workspace root` both return
# the workspace root in either case, and the workspace's own name is not stored under its `.jj` at
# all (it lives in the shared op store). Checked against colocated repos and subdirectories; see
# tests/ws-session-context.bats, which pins every one of those cases.
#
# Resolve the workspace root first: `.jj` exists only at the root, so a bare `[ -f .jj/repo ]` is a
# false negative for a session started in any subdirectory — which would make this look unreliable
# rather than broken.
#
# Silence is the normal outcome. Every session on this machine runs this hook, and only one started
# inside a secondary workspace prints anything, so bail as cheaply as possible in every other case.
set -euo pipefail

command -v jj >/dev/null 2>&1 || exit 0
root=$(jj workspace root 2>/dev/null) || exit 0
[ -f "$root/.jj/repo" ] || exit 0

# The pointer file is written relative to the workspace's own .jj/ (an absolute path also works,
# since this just cds to it). Losing it is not fatal — the rules matter more than the path.
main=''
if ptr=$(cat "$root/.jj/repo" 2>/dev/null) && [ -n "$ptr" ]; then
  main=$(cd "$root/.jj" 2>/dev/null && cd "$(dirname "$ptr")/.." 2>/dev/null && pwd -P) || main=''
fi

read -r -d '' msg <<EOF || true
You are working in a secondary jj workspace — a second working copy attached to the same repo,
created by the jj-workspace skill. It is not the main checkout${main:+ (that is $main)}.

- Commit as you go. After each self-contained change: \`jj commit -m 'area: what changed'\`. jj
  auto-snapshots the working copy into @, so there is no staging and nothing ever reads as
  uncommitted — a whole task otherwise piles into one nameless change, and splitting it afterwards
  is strictly more work than having committed twice.
- Do not integrate, rebase, or abandon anything on your own initiative. Merging this stack back
  rewrites history other checkouts are using; it is the user's call and is run from the main
  checkout, not here.
- Every commit here is visible to the other workspaces immediately — the repo store is shared, only
  the working copy is isolated. Never abandon, rebase, describe or edit another workspace's @, and
  do not move bookmarks it may be building on.
- For the full guidance (integrate and remove included), run \`/jj-workspace\` with no argument;
  that path creates nothing.
EOF

# Hand-rolled JSON so the hook has no dependency beyond coreutils: escape backslashes and quotes,
# then fold the real newlines into \n. jq would be tidier but is not guaranteed present, and a hook
# that dies on a missing binary fails at every session start.
esc=$(printf '%s' "$msg" |
  sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' |
  awk 'BEGIN { ORS = "" } { print sep $0; sep = "\\n" }')

printf '{"hookSpecificOutput":{"hookEventName":"SessionStart","additionalContext":"%s"}}\n' "$esc"
