#!/usr/bin/env bash
# Remove a jj workspace: stop tracking it, then delete its directory. Refuses when that
# would lose work, because the second half is an `rm -rf`.
#
#   ws-remove.sh <name> [--force]
#
# Refuses when the workspace still has UNCOMMITTED changes, which the rm would destroy;
# --force overrides. Committed work is never at risk: `jj workspace forget` leaves every
# real commit visible in the repo and only auto-abandons the trailing empty working-copy
# commit — so an unmerged stack is reported as a note, not a blocker.
set -euo pipefail
. "$(dirname "$0")/_common.sh"

name=''
force=''
while [ $# -gt 0 ]; do
  case "$1" in
    --force)
      force=1
      shift
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

# Free checks first, before anything shells out to jj.
[ -n "$name" ] || die "a workspace name is required (see --help)"
[ "$name" != 'default' ] || die "refusing to remove the 'default' workspace"
require_repo >/dev/null

# Ask jj where the workspace is instead of reconstructing ws-create.sh's naming convention.
# jj tracks each workspace's real root and returns it already canonical, so a workspace made
# with `ws-create.sh --path` is still removable by name, and the convention has exactly one
# definition. jj's own error covers both "no such workspace" and "directory already gone".
if ! path=$(jj workspace root --name "$name" 2>&1); then
  printf '%s\n' "$path" >&2
  die "cannot locate workspace '$name' (jj workspace list). If its directory is already gone: jj workspace forget $name"
fi

# rm -rf of the directory you're standing in leaves the shell somewhere that no longer exists.
# Comparing physical paths also catches being in a *subdirectory* of the doomed workspace.
case "$(pwd -P)/" in
  "$path"/*) die "run this from outside the workspace you're removing (you are in $(pwd -P))" ;;
esac

# Guard: the working-copy commit still holds changes nobody described or committed.
#
# Snapshot FIRST. jj only records a working copy when a command runs with that workspace as
# its cwd, so `<name>@` can still read as empty long after files were edited — which made an
# earlier version of this script cheerfully rm -rf real uncommitted work. Running the probe
# itself from inside the workspace snapshots and answers in one command.
if [ -z "$force" ]; then
  # A STALE working copy has to be caught separately, because jj refuses to answer anything
  # about it — and `|| dirty=''` below would read that refusal as "clean" and delete the
  # uncommitted work anyway. It's a routine state, not an exotic one: integrating a
  # single-commit stack rebases it, and a rebase run from anywhere but that workspace leaves it
  # stale. Sync it and the dirty check below becomes meaningful again.
  if ! probe=$(cd "$path" && jj status --no-pager 2>&1); then
    case "$probe" in
      *stale*) die "workspace '$name' has a stale working copy, so its uncommitted changes can't be checked. Sync it first (cd $path; jj workspace update-stale), or --force to delete regardless." ;;
      *) die "could not read workspace '$name': ${probe%%$'\n'*}" ;;
    esac
  fi
  dirty=$(cd "$path" && jjq '@ & ~empty()' '1') || dirty=''
  if [ -n "$dirty" ]; then
    printf 'uncommitted changes in workspace %s:\n' "$name" >&2
    (cd "$path" && jj status --no-pager) >&2 || true
    die "refusing to delete. Commit them, or pass --force to discard."
  fi
fi

# Report (don't block) commits reachable only from this workspace. They SURVIVE removal —
# `forget` keeps them visible — but afterwards nothing points at them, which is easy to
# forget about. Note this can't be phrased as "refuse until merged": after a linearizing
# rebase the stack is a *descendant* of the other workspace's tip, never an ancestor of its
# @, so a reachability test would keep failing long after the merge was done.
unmerged=$(jjq "::${name}@ ~ ::(bookmarks() | (working_copies() ~ ${name}@)) ~ empty()" \
  'change_id.shortest() ++ " " ++ description.first_line() ++ "\n"' || true)
if [ -n "$unmerged" ]; then
  printf 'note: these commits will only exist as a side stack after removal:\n%s\n' "$unmerged"
  printf '      they are kept, not deleted — reach them by change id, or merge with ws-merge.sh\n'
fi

run_cmd jj workspace forget "$name"
run_cmd rm -rf "$path"
printf 'forgot workspace %s and deleted %s\n' "$name" "$path"
printf 'To undo: jj op undo (restores the tracking; the directory stays deleted)\n'
