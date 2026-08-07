#!/usr/bin/env bash
# Integrate a workspace's stack. Nothing moves between directories — the commits are already
# in the shared repo; this only changes how they're joined to the other line of history.
#
#   ws-merge.sh [<workspace>] [--onto <rev>] [-m <msg>] [--yes]   # picks a shape
#   ws-merge.sh [<workspace>] --merge|--rebase ...                # forces one
#   ws-merge.sh --root <change-id> ...                            # a stack that isn't a workspace's
#
# WHICH STACK, if you name neither a workspace nor --root: the one this workspace has been
# building, when run from inside it; otherwise the one sibling workspace with committed work on
# <onto>. Several candidates is a question only you can settle, so it lists their names instead
# of picking. Either way the stack it chose is printed before anything happens.
#
# By default it chooses by size: a stack of MORE THAN ONE commit gets a merge commit, a stack
# of exactly one is rebased straight on. Wrapping a single change in a two-parent merge adds a
# node that carries no information; several commits are a branch worth keeping visible as one.
#
# MERGE keeps the parallel shape: `jj new <onto> <stack-tip>` makes a real two-parent commit.
#   Run it from the INTEGRATION workspace (normally the main checkout) — that workspace's tree
#   becomes the merged one, which is where you resolve conflicts and run tests. A clean merge is
#   then sealed with `jj new`, so @ ends up empty ABOVE the merge rather than on it: same tree,
#   and `@-` keeps meaning one commit instead of both parents.
# REBASE replays the stack on top of <onto> for linear history. Best run from INSIDE the feature
#   workspace, so its own working copy follows the move; from elsewhere that workspace goes
#   stale and this prints the command to sync it.
#   It then moves THIS workspace's trailing empty @ on top of the integrated tip. An @ sitting on
#   <onto> was continuing that line of history, and the stack lands *beside* it — so leaving it
#   put forks the mainline instead of extending it. --no-advance skips that; an @ with
#   uncommitted changes is never rewritten, only reported, and neither is another workspace's.
#
# Default destination is `default@-` — the main workspace's newest *committed* revision. When
# that workspace is standing on an EMPTY merge — the state a merge here leaves behind — `@-`
# means both parents, and the destination is the merge itself; see resolve_onto.
# Prints the plan and stops; pass --yes to execute.
set -euo pipefail
. "$(dirname "$0")/_common.sh"

mode=auto
root=''
onto='default@-'
msg=''
yes=''
no_advance=''
ws_arg=''
mode_arg=''
while [ $# -gt 0 ]; do
  case "$1" in
    --root)
      root="${2:-}"
      shift 2
      ;;
    --onto)
      onto="${2:-}"
      shift 2
      ;;
    -m | --message)
      msg="${2:-}"
      shift 2
      ;;
    --merge | --rebase)
      mode="${1#--}"
      mode_arg="$1" # kept verbatim: the re-run line should say what was asked for, not what auto picked
      shift
      ;;
    --yes)
      yes=1
      shift
      ;;
    --no-advance)
      no_advance=1
      shift
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    -*) die "unknown option: $1" ;;
    *)
      [ -z "$ws_arg" ] || die "unexpected argument: $1 (one workspace name at a time)"
      ws_arg="$1"
      shift
      ;;
  esac
done

here_root=$(require_repo)

# `--onto` needs its own error path rather than plain `one()`: the default `default@-` turns
# AMBIGUOUS the moment that workspace is standing on a merge commit, because `@-` then means
# both of its parents. That reads as a nonsense error unless it's explained.
#
# Explained isn't enough, though — standing on a merge is precisely the state THIS script's
# merge mode leaves behind, so whoever integrated last is sitting on the join and every
# subsequent run hits it. When that @ is EMPTY the ambiguity has an answer: the merge itself.
# It carries no work of its own, only the join, and it is the single commit that has both
# parents' lines in it — landing on either parent alone would drop the other side from the
# result. A NON-empty @ there is somebody's conflict resolution or WIP, and then there is
# genuinely nothing to pick, so that still fails.
resolve_onto() {
  local ids target
  ids=$(jjq "$1" 'change_id.shortest() ++ "\n"') || die "could not resolve --onto: $1"
  [ -n "$ids" ] || die "--onto \"$1\" matched no commit"
  if [[ $ids == *$'\n'* ]]; then
    # `${1%@-}@` turns `default@-` into `default@`, and a bare `@-` into `@` — the commit whose
    # two parents are what the revset just matched.
    case "$1" in
      *@-)
        target="${1%@-}@"
        if has "$target & merges() & empty()"; then
          printf 'note: "%s" means both parents — %s is an empty merge commit.\n' "$1" "$target" >&2
          printf '      Using that merge itself as the destination: it holds no work of its own, and\n' >&2
          printf '      building on either parent alone would drop the other side of the join.\n' >&2
          jjq "$target" 'change_id.shortest()'
          return 0
        fi
        ;;
    esac
    printf 'error: --onto "%s" matches: %s\n' "$1" "${ids//$'\n'/ }" >&2
    case "$1" in
      *@-) printf 'hint: that workspace is sitting ON a merge commit that still holds changes, so "@-"\n      means both parents. Commit it there (`jj new` leaves it behind as a real commit),\n      or pass --onto <change-id>.\n' >&2 ;;
    esac
    exit 1
  fi
  printf '%s' "$ids"
}
# The names of the workspaces whose @ sits inside a revset. Asks jj rather than walking
# workspace by workspace, and keeps the template quoting (a revset embedded in a template
# string) in one place.
ws_at() { jj workspace list --no-pager -T 'if(target.contained_in("'"$1"'"), name ++ "\n", "")'; }

# Move workspace $1's trailing empty @ on top of $2 — by running jj IN THAT WORKSPACE'S OWN
# DIRECTORY. That detail is the whole point rather than a nicety: `jj rebase -r <ws>@` issued
# from here rewrites the commit but cannot touch the other checkout, which leaves it stale — its
# files still the pre-integration tree, so a dev server watching them keeps serving the old
# build until somebody runs update-stale by hand. Run from inside, jj updates the files as part
# of the move, and the integration shows up where it's being watched.
#
# Only an EMPTY, CHILDLESS @ is moved: that is a marker for where its owner will work next, not
# work itself, so there is nothing there to lose. Anything else is reported for them to move —
# this script still never rewrites somebody's uncommitted state.
advance_ws() {
  local ws=$1 tip=$2 path
  path=$(jj workspace root --name "$ws" 2>/dev/null) || return 0
  # Another workspace's edits are not in the graph until something runs jj THERE: jj snapshots
  # the working copy it is standing in, and only that one. So asking "is their @ empty?" from
  # here answers about the last time they ran a command — which is precisely backwards for the
  # guard below, since a file they saved a minute ago still reads as an empty marker and gets
  # moved. Any command in their directory takes the snapshot, so ask the question of the truth.
  env -C "$path" jj status >/dev/null 2>&1 ||
    env -C "$path" jj workspace update-stale >/dev/null 2>&1 || true
  if has "children(${ws}@)"; then
    warn "workspace $ws: its @ has children of its own, so it was left where it is"
    return 0
  fi
  if has "${ws}@ & ~empty()"; then
    printf 'workspace %s has uncommitted changes, so its @ was left where it is. To move it:\n  cd %s; jj rebase -r @ -d %s\n' \
      "$ws" "$path" "$tip"
    return 0
  fi
  # An @ that is an ANCESTOR of the tip can't be rebased onto its own descendant — and shouldn't
  # be. That's the empty-merge destination: part of the history now, not a spare tip. A fresh
  # empty @ above it is what continues the line there.
  if has "${ws}@ & ::${tip}"; then
    run_cmd env -C "$path" jj new "$tip" ||
      { warn "workspace $ws: could not advance its @ — cd $path; jj new $tip"; return 0; }
  else
    run_cmd env -C "$path" jj rebase -r @ -d "$tip" ||
      { warn "workspace $ws: could not advance its @ — cd $path; jj rebase -r @ -d $tip"; return 0; }
  fi
  printf 'workspace %s now sits on %s, and its checkout was updated with it — anything watching those files reloads.\n' \
    "$ws" "$tip"
}

# The destination is resolved FIRST because the stack is derived relative to it: the question
# "which work isn't integrated yet" has no meaning until you know what it's being integrated into.
onto_id=$(resolve_onto "$onto")

# Every workspace's @ is somebody's *uncommitted* working copy. Never a destination, and never
# a merge parent — and you can't tell from the id alone that that's what you picked.
#
# The exception is the empty merge resolve_onto just fell back to. The rule exists to protect
# uncommitted content, and an empty commit has none; the same reasoning holds when someone
# names such an @ outright, so this tests the commit rather than how we got to it.
owner=$(ws_at "$onto_id")
if [ -n "$owner" ]; then
  owner="${owner%%$'\n'*}"
  has "$onto_id & merges() & empty()" ||
    die "--onto resolves to the working-copy commit of workspace '$owner'. Wait for it to be committed, then use '${owner}@-'."
  # Allowed, but not silently: that @ is never rewritten, it just stops being the tip once the
  # integration lands as its child. Whoever is in there catches up with `jj new`.
  [ "$(jj workspace root --name "$owner" 2>/dev/null)" = "$here_root" ] ||
    warn "destination is workspace '$owner's own @ (an empty merge). It isn't touched, but it stops being the tip — that workspace continues the line with \`jj new\`."
fi

# --- which stack ---
#
# `<onto>..<rev>` is everything on rev's line that <onto> doesn't already have, so its root is
# where that line left the mainline. That root is the handle worth having: change ids survive the
# rebase that is about to rewrite everything above it. One derivation, three ways to say which rev.
line_from() { printf 'roots(%s..%s)' "$onto_id" "$1"; }
# ...and "does that line hold anything worth integrating". Committed changes only: a workspace's
# @ is uncommitted state, so counting it would make every dirty working copy look like a stack —
# including this one, which would then be integrated into itself.
has_work() { has "(${onto_id}..${1}) ~ empty() ~ working_copies()"; }

[ -z "$root" ] || [ -z "$ws_arg" ] || die "name a workspace or pass --root, not both"
ws_name='' # the stack's workspace, where one is known: some advice only makes sense with a name
if [ -n "$ws_arg" ]; then
  jj workspace root --name "$ws_arg" >/dev/null 2>&1 ||
    die "no workspace named '$ws_arg' (jj workspace list)"
  root=$(line_from "${ws_arg}@")
  from="workspace '$ws_arg'"
  ws_name="$ws_arg"
elif [ -n "$root" ]; then
  from='--root'
elif has_work '@'; then
  # Run from inside the feature workspace: the stack is the line under our own feet.
  root=$(line_from '@')
  from='this workspace'
else
  # Run from the integration checkout — our own line off <onto> holds nothing, so the stack is a
  # sibling's. One candidate is an answer. Several is a question, and the names are what it takes
  # to answer it; guessing there would integrate somebody's unrelated branch.
  cands=''
  while IFS= read -r ws; do
    [ -n "$ws" ] || continue
    [ "$(jj workspace root --name "$ws" 2>/dev/null)" != "$here_root" ] || continue
    has_work "${ws}@" || continue
    cands="${cands:+$cands }$ws"
  done < <(jj workspace list --no-pager -T 'name ++ "\n"')
  case "$cands" in
    '') die "no workspace has committed work on $onto_id — nothing to integrate (pass --root for a stack that isn't a workspace's)" ;;
    *' '*) die "several workspaces have work on $onto_id: $cands
       name the one you mean ($0 <workspace>), or pass --root <change-id>" ;;
  esac
  root=$(line_from "${cands}@")
  from="workspace '$cands'"
  ws_name="$cands"
fi
root_id=$(one "$root")
printf 'stack: %s (%s), onto %s\n' "$root_id" "$from" "$onto_id"

# Which OTHER workspace was continuing the destination's line — the one whose @ this integration
# overtakes, and whose checkout therefore has to end up on the result or go on showing the tree
# from before? Two ways to be that workspace, both by construction rather than by guesswork:
#   `--onto <ws>@-` names it — that revset IS the parent of <ws>'s @, so that @ is the tip being
#   overtaken; and the empty-merge fallback resolves the destination to a workspace's @ outright.
# A workspace whose @ merely sits under <onto> is only *based* there — which every freshly
# created one is, since ws-create.sh bases on @- — and moving those would be wrong, so a bare
# `children($onto_id)` set would be mostly noise.
beside=''
case "$onto" in ?*@-) beside="${onto%@-}" ;; esac
[ -z "$owner" ] || beside="$owner"
[ -z "$beside" ] || has "(${beside}@ & ($onto_id | children($onto_id))) ~ descendants($root_id)" || beside=''
[ -z "$beside" ] || [ "$(jj workspace root --name "$beside" 2>/dev/null)" != "$here_root" ] || beside=''
[ -z "$no_advance" ] || beside=''

# The dry run has to end in a line worth pasting, so echo back the invocation actually made
# rather than the fully-resolved one. Every id it would spell out is already printed above, and
# re-running is meant to be one keystroke's difference — that is the whole point of the defaults.
rerun="$0"
[ -z "$ws_arg" ] || rerun="$rerun $ws_arg"
[ "$from" != '--root' ] || rerun="$rerun --root $root_id"
[ "$onto" = 'default@-' ] || rerun="$rerun --onto $onto"
[ -z "$mode_arg" ] || rerun="$rerun $mode_arg"
[ -z "$no_advance" ] || rerun="$rerun --no-advance"

# Measure the stack before choosing a shape. Also the merge path's own inputs, so it happens
# once, here, rather than twice.
if [ "$mode" != rebase ]; then
  # The merge parent is the newest *committed* commit of the stack — not the trailing empty @ the
  # workspace leaves at its tip, and not any workspace @ holding WIP.
  tip_set=$(jjq "heads(${root_id}:: ~ empty() ~ working_copies())" 'change_id.shortest() ++ "\n"' || true)
  if [ -z "$tip_set" ]; then
    # The same empty set, two situations. Once the root descends from the destination the graph
    # can no longer tell "never committed anything" from "all of it already landed" — integrating
    # a workspace leaves it holding exactly what a fresh one has, one empty @ on the destination.
    # So say what is actually known, and never tell someone whose work is already in to commit it.
    if has "$root_id & descendants($onto_id)"; then
      die "nothing to integrate at $root_id: no committed change here that $onto_id doesn't already have (nothing committed yet, or all of it has landed).${ws_name:+
       If that workspace is finished: ws-remove.sh $ws_name}"
    fi
    die "nothing committed in the stack rooted at $root_id yet (only empty changes) — commit first"
  fi
  [[ $tip_set != *$'\n'* ]] ||
    die "that stack has multiple heads: ${tip_set//$'\n'/ } — integrate one at a time with an explicit --root"
  tip_id="$tip_set"

  # NB the quoting: a jj template is an expression, so a bare 1\n is a parse error (silently
  # giving an empty result, hence a "0 commits" message) — the string needs its own quotes.
  count=$(jjq "${root_id}::${tip_id} ~ empty()" '"1\n"' | grep -c . || true)

  if [ "$mode" = auto ]; then
    if [ "$count" -gt 1 ]; then
      mode=merge
      printf 'auto: %s commits -> merge commit (pass --rebase to linearize instead)\n' "$count"
    else
      mode=rebase
      printf 'auto: a single commit -> rebasing it straight on, no merge node (--merge forces one)\n'
      [ -z "$msg" ] || warn "-m is ignored when rebasing a single commit"
    fi
  fi
fi

if [ "$mode" = rebase ]; then
  # A rebase is two moves, not one: the stack goes onto <onto>, and any @ that was *sitting* on
  # <onto> — continuing that line of history — has to follow, or the mainline forks beside the
  # work instead of extending it. Both are decided here, before anything moves, while
  # `children()` still describes the old shape.
  joined=''
  ! has "$root_id & descendants($onto_id)" || joined=1

  # Only our own @ is ours to move. Every other workspace's @ is its uncommitted state, so those
  # are reported for their owner to move (see SKILL.md: never rebase another workspace's @).
  advance=''
  if [ -z "$no_advance" ] && has "(@ & children($onto_id)) ~ descendants($root_id)"; then
    if has 'children(@)'; then
      warn "@ is not a trailing tip (it has children of its own) — leaving it where it is"
    elif has '@ & ~empty()'; then
      advance=report # rewriting work in progress is not this script's call to make
    else
      advance=move
    fi
  fi
  if [ -n "$joined" ] && [ -z "$advance" ] && [ -z "$beside" ]; then
    printf 'nothing to do: %s already descends from %s\n' "$root_id" "$onto_id"
    exit 0
  fi

  # One graph, so the stack's relationship to its destination — and to the @ beside it — is
  # visible rather than implied.
  printf '=== plan (rebase / linear) ===\n'
  if [ -n "$joined" ]; then
    printf '%s already descends from %s; only the trailing @ is out of place.\n' "$root_id" "$onto_id"
  else
    printf 'move stack %s (and its descendants) onto %s\n' "$root_id" "$onto_id"
  fi
  case "$advance" in
    move) printf 'then move this workspace'\''s empty @ on top of the integrated tip\n' ;;
    report) printf 'this workspace'\''s @ has uncommitted changes: NOT moved, only reported\n' ;;
  esac
  [ -z "$beside" ] || printf 'then move workspace %s'\''s empty @ on top too, refreshing its checkout (an @ with changes there is only reported)\n' "$beside"
  printf '\n'
  jj log --no-pager -r "${root_id}:: | $onto_id | @" || true
  if [ -z "$yes" ]; then
    printf '\nnot executed. Re-run with --yes:\n  %s --yes\n' "$rerun"
    exit 0
  fi

  # Whose working copies sit inside the stack? The rebase rewrites those commits underneath
  # them, which leaves that workspace stale — except the one we're standing in, which jj
  # updates as it goes. Captured before the move, and reported with the path so the fix is a
  # copy-paste rather than a hunt.
  affected=$(ws_at "descendants($root_id)")

  if [ -z "$joined" ]; then
    run_cmd jj rebase -s "$root_id" -d "$onto_id"
    printf '\nrebased.\n'
  fi

  # The tip to build on, by the same rule the merge path picks a merge parent: the newest
  # *committed* change of the stack — never the trailing empty @ a workspace leaves behind, and
  # never another workspace's WIP. Read after the move, since that's what it has to describe.
  new_tip=$(jjq "heads(descendants($root_id) ~ empty() ~ working_copies())" 'change_id.shortest() ++ "\n"' || true)
  # As in _common.sh's one(): $() strips trailing newlines, so an embedded one means 2+ heads —
  # and then there is no single tip to build on, only a choice for a human to make.
  if [ -z "$new_tip" ] || [[ $new_tip == *$'\n'* ]]; then
    tips="${new_tip//$'\n'/ }"
    [ -z "$advance$beside" ] ||
      warn "the stack has no single committed tip (${tips:-none}) — every @ stays where it is"
    advance=''
    beside=''
  fi

  case "$advance" in
    move)
      run_cmd jj rebase -r @ -d "$new_tip"
      printf 'this workspace'\''s empty @ now sits on %s, so the next commit here continues the line.\n' "$new_tip"
      ;;
    report)
      printf '\nthis workspace'\''s @ has uncommitted changes, so it was left beside the stack.\n'
      printf 'To continue on top of the integrated tip:\n  jj rebase -r @ -d %s\n' "$new_tip"
      ;;
  esac
  printf '\nTo undo the whole operation: jj undo\n'

  [ -n "$joined" ] || while IFS= read -r ws; do
    [ -n "$ws" ] || continue
    ws_path=$(jj workspace root --name "$ws" 2>/dev/null) || continue
    [ "$ws_path" != "$here_root" ] || continue
    # Its @ was rewritten underneath it, so its files are still the pre-rebase tree and anything
    # watching them is serving a build of history that no longer exists. Syncing a working copy
    # to a commit that already moved is not a rewrite and loses nothing, so it's ours to run
    # rather than to suggest — which is what the whole point of the rebase reaching disk needs.
    run_cmd env -C "$ws_path" jj workspace update-stale ||
      warn "workspace $ws is stale and could not be synced: cd $ws_path; jj workspace update-stale"
  done <<<"$affected"

  # The mainline @ when the rebase was run from the feature workspace — the usual --rebase flow.
  # It was continuing <onto> and the stack has now landed on top of it, so it is beside the work
  # rather than above it, and its checkout shows the tree from before the integration.
  [ -z "$beside" ] || advance_ws "$beside" "$new_tip"
  exit 0
fi

# --- merge mode ---

# Already joined? For a merge that means the stack is reachable from the destination.
if has "$root_id & ::$onto_id"; then
  printf 'nothing to do: %s is already an ancestor of %s\n' "$root_id" "$onto_id"
  exit 0
fi

[ -n "$msg" ] || msg="Merge $count commit(s) from $root_id"

# The merge lands as *this* workspace's @, so running it in the feature workspace puts it
# somewhere you're about to delete rather than on your integration checkout.
if has "@ & descendants($root_id)"; then
  warn "you are inside the stack being merged — the merge would become THIS workspace's @."
  warn "run it from the integration workspace (the main checkout) instead."
fi

printf '=== plan (merge commit) ===\nmerge stack tip %s into %s\n' "$tip_id" "$onto_id"
[ -z "$beside" ] || printf 'then seal it with `jj new` here and move workspace %s'\''s empty @ on top, refreshing its checkout\n' "$beside"
printf '\n'
jj log --no-pager -r "${root_id}::${tip_id}" || true
printf '\nmessage: %s\n' "$msg"
if [ -z "$yes" ]; then
  printf '\nnot executed. Re-run with --yes:\n  %s -m %s --yes\n' "$rerun" "'$msg'"
  exit 0
fi

run_cmd jj new "$onto_id" "$tip_id" -m "$msg"

# A conflicting merge SUCCEEDS in jj — the conflict is recorded in the commit and the exit
# code is 0. Checking for it is the only way to know; never infer it from the exit status.
if has '@ & conflicts()'; then
  printf '\n!!! the merge has CONFLICTS (jj recorded them; the command still succeeded) !!!\n'
  jj status --no-pager 2>&1 | sed -n '/conflict/,$p' || true
  printf '\nResolve them in this working copy — the markers are jj'"'"'s own format, not git'"'"'s\n'
  printf '(`%%%%%%%%` shows the diff one side applied, `+++++++` the other side'"'"'s content).\n'
  printf 'Edit the files, then re-run the project gate. `jj undo` backs the merge out entirely.\n'
  # The clean path seals the merge with `jj new` (see below) and this one cannot — the resolution
  # has to happen in @, which is the merge. So say what the last step is: left as-is, @ stays ON a
  # merge, which is what makes `@-` mean both parents and sends the next command here down the
  # disambiguation path for no reason.
  printf 'Then `jj new` to leave the resolved merge behind as a commit — otherwise `@-` here means\nboth of its parents, and the next ws-create/ws-merge has to work around that.\n'
  exit 1
fi

printf '\nclean merge. Shape now:\n\n'
jj log --no-pager -r "@ | parents(@)" || true

# `jj new` — leave the merge behind as a real commit instead of as this workspace's @.
#
# This is the state the rest of the file spends so much effort coping with, so it is worth not
# creating it: an @ sitting ON a merge makes `@-` mean BOTH parents, and every later ws-create
# or ws-merge in this workspace then has to disambiguate a state that never needed to exist.
# The second reason is plainer — a merge is history, not somebody's working copy, and anything
# hung off it afterwards (another workspace's @, the next stack) would otherwise be hung off
# uncommitted state.
#
# It costs nothing. The tree is identical, so the project gate still runs on exactly the merged
# content, one commit further up; and a fix for a semantic conflict (both sides fine apart,
# broken together — the only way a *clean* merge fails a gate) lands as its own commit above the
# merge, which is a truer description of it than an amended merge. --no-advance opts out, same
# as for the @ moves: it is the same "leave a trailing empty @" decision.
merge_id=$(jjq '@' 'change_id.shortest()')
if [ -z "$no_advance" ]; then
  run_cmd jj new
  printf 'sealed the merge as %s; this workspace continues on a fresh empty @ above it.\n' "$merge_id"
fi

# The other workspace, if one was continuing this line: now safe to hang off the merge, since
# the step above turned it into a commit rather than a working copy.
[ -z "$beside" ] || advance_ws "$beside" "$merge_id"

if [ -z "$no_advance" ]; then
  printf '\nRun the project gate here — same tree as the merge. To undo the whole operation: jj undo\n'
else
  printf '\nRun the project gate here, then `jj new` to leave a fresh empty @ on top (--no-advance\nskipped that, so `@-` here means both parents until you do).\nTo undo: jj undo\n'
fi
