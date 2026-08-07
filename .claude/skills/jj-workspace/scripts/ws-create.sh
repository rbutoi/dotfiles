#!/usr/bin/env bash
# Create a jj workspace (a second working copy on the same repo) and report the handles
# needed to merge it back later. Run from anywhere inside the repo.
#
#   ws-create.sh <name> [--base <rev>] [--path <dir>]
#
# Defaults: base = @- (the newest real commit, since the tip @ is conventionally empty),
# path = a SIBLING of the repo root named <repo-dir>-<name> (never nested inside the repo,
# where formatters/watchers/test runners would scan it).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd -P)
. "$here/_common.sh"

name=''
base='@-'
path=''
while [ $# -gt 0 ]; do
  case "$1" in
    --base)
      base="${2:-}"
      shift 2
      ;;
    --path)
      path="${2:-}"
      shift 2
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    -*) die "unknown option: $1" ;;
    *)
      [ -z "$name" ] || die "unexpected argument: $1"
      name="$1"
      shift
      ;;
  esac
done

[ -n "$name" ] || die "a workspace name is required (see --help)"
# The name becomes a directory name and a jj workspace id, so keep it boring.
[[ $name =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]] ||
  die "workspace name '$name' must be alphanumeric plus . _ - (and start alphanumeric)"

root=$(require_repo)

# One `jj workspace add` per name; a collision would otherwise fail after the path checks
# with a message about the wrong thing.
! jj workspace root --name "$name" >/dev/null 2>&1 ||
  die "a workspace named '$name' already exists (jj workspace list)"

# This is the ONLY place the sibling naming convention is defined. ws-remove.sh asks jj for
# the path rather than re-deriving it, so a workspace made with --path is still removable.
[ -n "$path" ] || path="${root%/*}/${root##*/}-$name"
[ ! -e "$path" ] || die "path already exists: $path"
# Canonicalize via the parent (which must exist anyway) so the nesting check below compares
# physical paths. Without this, /tmp/x vs /private/tmp/x on macOS silently defeats it — and a
# nested workspace is exactly what we're trying to prevent.
parent=$(cd "${path%/*}" 2>/dev/null && pwd -P) ||
  die "parent directory does not exist: ${path%/*}"
path="$parent/${path##*/}"
case "$path" in
  "$root" | "$root"/*) die "refusing to nest a workspace inside the repo ($path); use a sibling directory" ;;
esac

# The default base `@-` is ambiguous when @ is an empty merge (see is_empty_merge in _common.sh
# for why, and for how you end up there). Sealing is what makes the tip nameable again: `jj new`
# leaves the merge behind as a real commit, the tree is identical, and `@-` is then exactly the
# commit we wanted. It also restores the "tip @ is empty" shape the rest of this file assumes.
# Before resolving, so the resolution below sees the fixed graph.
if [ "$base" = '@-' ] && is_empty_merge '@'; then
  warn "@ is an empty merge, so '@-' would mean both of its parents; sealing it into a commit first (undo with \`jj undo\`)"
  run_cmd jj new
fi

# `one()` rather than a hand-rolled resolve: a revset matching 0 or 2+ commits would otherwise
# make `jj workspace add` create the directory and *then* fail about something else, and one()
# additionally re-runs jj to distinguish "no such revision" from "prefix is ambiguous".
base_id=$(one "$base")

# Two things worth saying out loud rather than silently baking in:
if has "$base_id & empty()"; then
  warn "base $base is an empty commit — check 'jj log' that this is the base you meant"
fi
if has '@ & ~empty()'; then
  warn "the current workspace has uncommitted changes in @; they are NOT included (base is $base)"
fi

run_cmd jj workspace add --name "$name" -r "$base" "$path"

# The workspace's initial (empty) change BECOMES the first commit when you `jj commit`, so
# this is the stack root — the stable handle for the later rebase, since change ids survive
# rebases and commit hashes don't. ws-merge.sh can re-derive it, but printing it here means
# there is something to pass to --root if the stack ever grows a second root.
stack_root=$(jjq "${name}@" 'change_id.shortest()')

printf '\n=== workspace ready ===\n'
printf 'name:        %s\n' "$name"
printf 'path:        %s\n' "$path"
printf 'base:        %s (%s)\n' "$base" "$base_id"
printf 'stack root:  %s\n' "$stack_root"

# Ignored files are not materialized, so a fresh workspace has no dependencies installed.
# Detect, suggest, do NOT run: the right command is project-specific and may need a flag.
printf '\ndependencies (NOT installed — ignored files are not copied):\n'
found=''
while IFS= read -r lock; do
  case "${lock##*/}" in
    pnpm-lock.yaml) cmd='pnpm install' ;;
    uv.lock) cmd='uv sync' ;;
    Cargo.lock) cmd='cargo fetch' ;;
    go.sum) cmd='go mod download' ;;
    *) continue ;;
  esac
  printf '  %s -> (cd %s; %s)\n' "$lock" "${lock%/*}" "$cmd"
  found=1
  # Nothing ignored is materialized yet, so node_modules/target/.venv cannot exist — .jj is
  # the only directory here big enough to be worth pruning.
done < <(find "$path" -maxdepth 3 -name .jj -prune -o \
  -type f \( -name pnpm-lock.yaml -o -name uv.lock -o -name Cargo.lock -o -name go.sum \) -print)
[ -n "$found" ] ||
  printf '  no pnpm/uv/cargo/go lockfile found — check the project'\''s own setup steps\n'

printf '\nwork in it by prefixing every command (the shell cwd resets between tool calls):\n'
printf '  cd %s; jj st\n' "$path"
# Integrating is the user's call, so print the command rather than implying it's a step to take
# once the work looks done. The name is what identifies the stack (ws-merge.sh derives the root
# from it), and a merge commit lands as the *integration* workspace's @, so the line runs from
# here — not from inside the feature workspace (that's the --rebase case).
printf '\nwhen the USER asks you to integrate it (never on your own initiative):\n'
printf '  cd %s; %s/ws-merge.sh %s -m %s\n' "$root" "$here" "$name" "'Merge $name'"
printf '  # dry run. Add --yes only when asked; --rebase linearizes instead, from inside %s\n' "$path"
printf 'then remove the workspace:\n'
printf '  %s/ws-remove.sh %s\n' "$here" "$name"
