#!/usr/bin/env bash
# Remove a jj workspace: stop tracking it, then delete its directory. Refuses when that
# would lose work, because the second half is an `rm -rf`.
#
#   ws-remove.sh <name> [--force]
#
# Refuses when the workspace still has UNCOMMITTED changes, which the rm would destroy;
# --force overrides, and supplying it is the user's call to make, not an agent's. Committed
# work is never at risk: `jj workspace forget` leaves every real commit visible in the repo
# and only auto-abandons the trailing empty working-copy commit — so an unmerged stack is
# reported as a note, not a blocker.
#
# Runs fine from INSIDE the workspace it deletes — finishing one is the normal reason you're in
# there. It steps out to another workspace of the repo first (`default`, normally) and prints the
# `cd` you need afterwards, since your shell is left in a directory that no longer exists.
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

# Standing inside the workspace being deleted is the normal way to finish one — you were working
# in it — so step out rather than refusing. This used to be a hard error telling you to re-run from
# elsewhere, which is a rule the script can follow on its own.
#
# What it can't do is fix the SHELL that invoked it: cd here is this process's, so the caller is
# left in a deleted directory whatever we do, and the only honest answer is to say so and print the
# path (see the note at the end). What the cd does buy is that everything *after* it — the forget,
# the rm, any jj command — runs from a directory that still exists. Comparing physical paths also
# catches being in a *subdirectory* of the doomed workspace.
#
# Where it steps TO has to be another workspace of this repo, not just any surviving directory:
# every jj command below finds the repo through the cwd, so `/` or `$TMPDIR` would trade the
# "directory is gone" failure for a "not inside a jj repo" one. Asks jj for the path rather than
# assuming ws-create.sh's sibling convention, which a `--path` workspace doesn't follow.
inside=''
case "$(pwd -P)/" in
  "$path"/*)
    # `default` is the main checkout and the obvious landing place; the loop covers a repo whose
    # workspaces were renamed. If there is genuinely nowhere else to stand, the old refusal is
    # still the right answer — there is no cd that would help.
    outside=$(jj workspace root --name default 2>/dev/null) || {
      outside=''
      while IFS= read -r ws; do
        [ -n "$ws" ] && [ "$ws" != "$name" ] || continue
        outside=$(jj workspace root --name "$ws" 2>/dev/null) && break
        outside=''
      done < <(jj workspace list --no-pager -T 'name ++ "\n"')
    }
    [ -n "$outside" ] ||
      die "run this from outside the workspace you're removing (you are in $(pwd -P)): '$name' is the only workspace left, so there is nowhere in this repo to step to"
    inside=$(pwd -P)
    cd "$outside" || die "cannot step out of $inside into $outside"
    printf 'you are inside %s, so this ran from %s instead.\n' "$name" "$outside"
    ;;
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
      # Short form of the rule spelled out at the dirty refusal below: --force here deletes work
      # nobody has even been able to *list*, so syncing is the answer and the flag is not yours.
      *stale*) die "workspace '$name' has a stale working copy, so its uncommitted changes can't be checked. Sync it first (cd $path; jj workspace update-stale), or --force to delete regardless — the user's call, not the agent's." ;;
      *) die "could not read workspace '$name': ${probe%%$'\n'*}" ;;
    esac
  fi
  dirty=$(cd "$path" && jjq '@ & ~empty()' '1') || dirty=''
  if [ -n "$dirty" ]; then
    printf 'uncommitted changes in workspace %s:\n' "$name" >&2
    (cd "$path" && jj status --no-pager) >&2 || true
    # The rule at the point it comes up, as in ws-merge.sh's dry-run footer: this is the one
    # irreversible thing in the skill, and the flag that does it is right there in the refusal.
    die "refusing to delete. Commit them, or pass --force to discard.
       --force is the user's decision, not the agent's: it destroys work they have not seen.
       Report what is uncommitted and let them choose — committing it loses nothing."
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
# The one thing the cd above could not fix: the caller's shell is still in the deleted directory,
# and only the caller can leave it. Said last, where it is the next thing to act on — and with the
# path spelled out, because that shell can no longer tab-complete anything.
[ -z "$inside" ] ||
  printf 'your shell is still in %s, which no longer exists:\n  cd %s\n' "$inside" "$outside"
printf 'To undo: jj undo (restores the tracking; the directory stays deleted)\n'
