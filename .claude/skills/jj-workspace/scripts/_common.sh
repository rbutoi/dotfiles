#!/usr/bin/env bash
# Shared helpers for the ws-* scripts. Source it, don't run it:
#
#   . "$(dirname "$0")/_common.sh"
#
# Everything here is used by at least two of the three scripts. The jj wrappers exist so
# the flags (--no-pager --no-graph) and the "a bad revset reads as no match" decision live
# in one place instead of being re-spelled at every call site.

die() {
  printf 'error: %s\n' "$1" >&2
  exit 1
}
warn() { printf 'warning: %s\n' "$1" >&2; }

# Echo a mutating command, then run it. Only the ones that change something go through this —
# the read-only queries behind every decision would bury the two or three lines that matter.
#
# The point is repro: what these scripts do to history is a handful of ordinary jj commands, and
# printing them means an unexpected result can be re-run, adapted, or reasoned about by hand
# instead of read backwards out of the graph. So the printed form has to be pasteable, which
# means quoting arguments that need it — `-m Merge feat` is a different command from
# `-m 'Merge feat'`, and the unquoted echo of an `rm -rf` path with a space is worse than that.
run_cmd() {
  local a shown=''
  for a; do
    case "$a" in
      '' | *[!A-Za-z0-9_/.:@=,+-]*) a="'${a//\'/\'\\\'\'}'" ;;
    esac
    shown="${shown:+$shown }$a"
  done
  printf '+ %s\n' "$shown"
  "$@"
}

# Print the script's own header comment as its help text. Derived from the file's structure
# (drop the shebang, stop at the first non-comment line, strip the '# ') rather than a
# hard-coded line range, which silently over- or under-prints as soon as the comment is edited.
usage() { sed -e '1d' -e '/^[^#]/,$d' -e 's/^#\{1,\} \{0,1\}//' "$0"; }

# Require a jj repo and print the current workspace's root.
require_repo() { jj workspace root 2>/dev/null || die "not inside a jj repo (no .jj found)"; }

# Query the repo: revset, template -> stdout.
jjq() { jj log --no-pager --no-graph -r "$1" -T "$2" 2>/dev/null; }

# Change ids are printed as `change_id.shortest()` — the shortest currently-unique prefix, which
# is the part `jj log` highlights. So an id from these scripts is the one you can see in the
# graph and retype, rather than `short()`'s fixed 12 characters that nobody reads out.
#
# The trade-off, since these ids get recorded and passed back in later: a prefix that is unique
# today can stop being unique as the repo grows. It fails safe — jj answers `Change ID prefix
# `k` is ambiguous` instead of resolving the wrong change — and `one()` passes that message
# through, so the fix (re-read the id from `jj log`) is obvious rather than mysterious.

# Does the revset match anything? Tests for non-empty rather than == '1', because the
# template emits one '1' per match and a 2-match revset must still count as a match.
has() { [ -n "$(jjq "$1" '1')" ]; }

# The single change id a revset matches, or die. Worth pre-checking: `jj workspace add` and
# `jj rebase` given an ambiguous revset fail late, with an error about something else.
one() {
  local out
  if ! out=$(jjq "$1" 'change_id.shortest() ++ "\n"'); then
    # Re-run to show jj's own words. It distinguishes "no such revision" from "prefix is
    # ambiguous", and the latter is the one failure the short prefixes above can produce —
    # flattening both into one generic line would hide exactly the case worth naming. The
    # success path still discards stderr, so a stray warning can't be mistaken for output.
    printf 'error: could not resolve revision "%s"\n' "$1" >&2
    jj log --no-pager --no-graph -r "$1" -T '""' 2>&1 | head -3 >&2
    exit 1
  fi
  # A value from $() contains a newline only if it had 2+ lines — that is the whole test.
  [ -n "$out" ] && [[ $out != *$'\n'* ]] ||
    die "'$1' must resolve to exactly one commit, got: ${out//$'\n'/ }"
  printf '%s' "$out"
}
