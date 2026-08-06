#!/usr/bin/env bats

load 'helper'

@test "an unresolvable root fails loudly" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  run env -C "$ws" "$scripts/ws-merge.sh" --root feat_bogus_id
  [ "$status" -ne 0 ]
  [[ "$output" == *resolve* ]]
}

# --- which stack: named, inferred, or refused ---

@test "run from inside the feature workspace, the stack needs no naming" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$ws" "$scripts/ws-merge.sh"
  [ "$status" -eq 0 ]
  [[ "$output" == *'stack:'*'this workspace'* ]]
  [[ "$output" == *plan* ]]
}

@test "run from the integration checkout, the one sibling holding work is found" {
  # A workspace with nothing committed is not a candidate — otherwise every freshly created one
  # would make the repo permanently ambiguous.
  repo=$(new_repo)
  new_ws "$repo" feat 1 >/dev/null
  new_ws "$repo" idle 0 >/dev/null
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$repo" "$scripts/ws-merge.sh"
  [ "$status" -eq 0 ]
  [[ "$output" == *"workspace 'feat'"* ]]
}

@test "a dirty integration @ is not mistaken for a stack to integrate into itself" {
  # Uncommitted changes are not a stack. Counting them would make the script pick its own @ the
  # moment the main checkout is dirty, which is most of the time.
  repo=$(new_repo)
  new_ws "$repo" feat 1 >/dev/null
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  ( cd "$repo" && printf 'wip\n' >wip.txt )
  run env -C "$repo" "$scripts/ws-merge.sh"
  [ "$status" -eq 0 ]
  [[ "$output" == *"workspace 'feat'"* ]]
}

@test "several candidate stacks are listed by name, never guessed between" {
  repo=$(new_repo)
  new_ws "$repo" alpha 1 >/dev/null
  new_ws "$repo" beta 1 >/dev/null
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$repo" "$scripts/ws-merge.sh"
  [ "$status" -ne 0 ]
  [[ "$output" == *'several workspaces'* ]]
  [[ "$output" == *alpha* && "$output" == *beta* ]]
}

@test "no stack anywhere says so instead of resolving to something arbitrary" {
  repo=$(new_repo)
  new_ws "$repo" idle 0 >/dev/null
  run env -C "$repo" "$scripts/ws-merge.sh"
  [ "$status" -ne 0 ]
  [[ "$output" == *'nothing to integrate'* ]]
}

@test "a workspace name picks its stack out of several" {
  repo=$(new_repo)
  new_ws "$repo" alpha 1 >/dev/null
  new_ws "$repo" beta 1 >/dev/null
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$repo" "$scripts/ws-merge.sh" beta
  [ "$status" -eq 0 ]
  [[ "$output" == *"workspace 'beta'"* ]]
  [[ "$output" != *"workspace 'alpha'"* ]]
}

@test "an unknown workspace name is refused" {
  repo=$(new_repo)
  new_ws "$repo" feat 1 >/dev/null
  run env -C "$repo" "$scripts/ws-merge.sh" nosuch
  [ "$status" -ne 0 ]
  [[ "$output" == *'no workspace named'* ]]
}

@test "a name and --root together are refused rather than silently ranked" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  run env -C "$repo" "$scripts/ws-merge.sh" feat --root "$(env -C "$ws" jj log --no-pager \
    --no-graph -r "$STACK_ROOT" -T 'change_id.short()')"
  [ "$status" -ne 0 ]
  [[ "$output" == *'not both'* ]]
}

@test "the re-run line repeats the invocation, not the ids it resolved" {
  # The whole point of inferring is that confirming costs one flag; echoing --root/--onto back
  # would hand the user the verbose form they just avoided typing.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$ws" "$scripts/ws-merge.sh"
  [[ "$output" == *'--yes'* ]]
  [[ "$output" != *'--root'* ]]
  [[ "$output" != *'--onto'* ]]
}

@test "unknown options are rejected" {
  repo=$(new_repo)
  run env -C "$repo" "$scripts/ws-merge.sh" --badflag
  [ "$status" -ne 0 ]
  [[ "$output" == *'unknown option'* ]]
}

@test "refuses a destination that is some workspace's live working copy" {
  # Every workspace's @ is somebody's uncommitted state. Building on it means depending on
  # something still being edited, and the change id alone doesn't reveal that.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  run env -C "$ws" "$scripts/ws-merge.sh" --root "$STACK_ROOT" --onto 'default@'
  [ "$status" -ne 0 ]
  [[ "$output" == *'working-copy commit of workspace'* ]]
}

@test "the plan is a dry run and changes nothing" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  # The mainline has to move, or there is genuinely nothing to integrate: a stack branched off
  # the tip already descends from it, and the script correctly says so instead of planning.
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  before=$(history_of "$repo")
  run env -C "$ws" "$scripts/ws-merge.sh" --root "$STACK_ROOT"
  [ "$status" -eq 0 ]
  [[ "$output" == *plan* ]]
  [[ "$output" == *'not executed'* ]]
  [ "$(history_of "$repo")" = "$before" ]
}

# --- the shape is chosen by stack size ---

@test "one commit is rebased straight on, with no merge node" {
  # A two-parent merge whose side is a single change records nothing the commit doesn't
  # already say, so it would be a node carrying no information.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$ws" "$scripts/ws-merge.sh" --root "$STACK_ROOT"
  [[ "$output" == *'single commit'* ]]
  [[ "$output" == *rebase* ]]
  [[ "$output" != *'merge stack tip'* ]]
}

@test "-m is called out as moot when the stack is rebased" {
  # Accepting it silently would imply the message landed somewhere.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  run env -C "$ws" "$scripts/ws-merge.sh" --root "$STACK_ROOT" -m 'Merge feat'
  [[ "$output" == *'-m is ignored'* ]]
}

@test "--merge forces the node on a single commit" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  run env -C "$ws" "$scripts/ws-merge.sh" --root "$STACK_ROOT" --merge
  [[ "$output" == *'merge stack tip'* ]]
}

@test "more than one commit gets a merge commit" {
  # Several commits are a branch, and worth keeping visible as one.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 2)
  run env -C "$ws" "$scripts/ws-merge.sh" --root "$STACK_ROOT"
  [[ "$output" == *'2 commits'* ]]
  [[ "$output" == *'merge stack tip'* ]]
}

@test "--rebase forces linear history on a multi-commit stack" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 3)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$ws" "$scripts/ws-merge.sh" --root "$STACK_ROOT" --rebase
  [[ "$output" == *rebase* ]]
  [[ "$output" != *'merge stack tip'* ]]
}

@test "a stack with nothing committed says so instead of merging an empty change" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 0)
  run env -C "$ws" "$scripts/ws-merge.sh" --root '@'
  [ "$status" -ne 0 ]
  [[ "$output" == *'nothing to integrate'* ]]
}

@test "a diverged stack of only empty changes is told to commit" {
  # The other side of that message: based off an older commit, so it is genuinely un-integrated
  # work-that-isn't-there-yet rather than work that already landed.
  repo=$(new_repo)
  env -C "$repo" "$scripts/ws-create.sh" feat --base 'description(substring:"first")' >/dev/null 2>&1
  run env -C "$repo-feat" "$scripts/ws-merge.sh" --root '@'
  [ "$status" -ne 0 ]
  [[ "$output" == *'commit first'* ]]
}

@test "a spent workspace is told it is spent, not told to commit" {
  # After integrating, a workspace is left holding one empty @ on the destination — the exact
  # shape a fresh one has. "commit first" would be the wrong instruction for work already in.
  repo=$(new_repo)
  new_ws "$repo" feat 1 >/dev/null
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  env -C "$repo" "$scripts/ws-merge.sh" feat --yes >/dev/null 2>&1
  run env -C "$repo" "$scripts/ws-merge.sh" feat
  [ "$status" -ne 0 ]
  [[ "$output" == *'nothing to integrate'* ]]
  [[ "$output" == *'ws-remove.sh feat'* ]]
  [[ "$output" != *'commit first'* ]]
}

# --- executing, in both shapes ---

@test "--yes on a single commit produces a linear history, no merge" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$ws" "$scripts/ws-merge.sh" --root "$STACK_ROOT" --yes
  [ "$status" -eq 0 ]
  [[ "$output" == *rebased* ]]
  # The moved commit now descends from the mainline tip, and nothing has two parents.
  merges=$(env -C "$repo" jj log --no-pager --no-graph -r 'all() & merges()' -T '"x"')
  [ -z "$merges" ]
  [ -n "$(env -C "$repo" jj log --no-pager --no-graph \
    -r 'description(substring:"ws work 1") & descendants(description(substring:"main work"))' -T '"x"')" ]
}

# --- the trailing @ follows the stack ---

@test "the integration workspace's empty @ is moved on top of the rebased stack" {
  # Otherwise the mainline forks: the stack lands on `onto` beside the @ that was continuing it,
  # so the next commit made here would branch off the pre-integration tip.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  root=$(env -C "$ws" jj log --no-pager --no-graph -r "$STACK_ROOT" -T 'change_id.short()')
  run env -C "$repo" "$scripts/ws-merge.sh" --root "$root" --yes
  [ "$status" -eq 0 ]
  [ -n "$(env -C "$repo" jj log --no-pager --no-graph \
    -r '@ & descendants(description(substring:"ws work 1"))' -T '"x"')" ]
}

@test "--no-advance leaves the @ where it is" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  root=$(env -C "$ws" jj log --no-pager --no-graph -r "$STACK_ROOT" -T 'change_id.short()')
  run env -C "$repo" "$scripts/ws-merge.sh" --root "$root" --no-advance --yes
  [ "$status" -eq 0 ]
  [ -z "$(env -C "$repo" jj log --no-pager --no-graph \
    -r '@ & descendants(description(substring:"ws work 1"))' -T '"x"')" ]
}

@test "an @ with uncommitted changes is reported, never rewritten" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  ( cd "$repo" && printf 'wip\n' >wip.txt )
  root=$(env -C "$ws" jj log --no-pager --no-graph -r "$STACK_ROOT" -T 'change_id.short()')
  run env -C "$repo" "$scripts/ws-merge.sh" --root "$root" --yes
  [ "$status" -eq 0 ]
  [[ "$output" == *'jj rebase -r @ -d'* ]]
  [ -z "$(env -C "$repo" jj log --no-pager --no-graph \
    -r '@ & descendants(description(substring:"ws work 1"))' -T '"x"')" ]
}

@test "a rebase from inside the feature workspace reports the mainline @ left beside it" {
  # Another workspace's @ is its uncommitted state, so this hands over the command instead.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$ws" "$scripts/ws-merge.sh" --root "$STACK_ROOT" --yes
  [ "$status" -eq 0 ]
  [[ "$output" == *'workspace default left its @ beside the stack'* ]]
  [[ "$output" == *'jj rebase -r @ -d'* ]]
}

@test "an unrelated sibling workspace is not nagged about its base" {
  # Every fresh workspace's @ is a child of default@- — ws-create.sh bases on @- — so a
  # "children(onto)" hint would tell every sibling to rebase onto this stack. Only the workspace
  # --onto was derived from was actually continuing that line.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  new_ws "$repo" other 0 >/dev/null
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  run env -C "$ws" "$scripts/ws-merge.sh" --root "$STACK_ROOT" --yes
  [ "$status" -eq 0 ]
  [[ "$output" == *'workspace default left its @'* ]]
  [[ "$output" != *'workspace other left its @'* ]]
}

@test "an already-joined stack still advances an @ stranded beside it" {
  # The state a plain rebase leaves behind: history is integrated, but the tip @ never moved.
  # "Nothing to do" would be a wrong answer — the two lines are still forked.
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  root=$(env -C "$ws" jj log --no-pager --no-graph -r "$STACK_ROOT" -T 'change_id.short()')
  env -C "$repo" "$scripts/ws-merge.sh" --root "$root" --no-advance --yes >/dev/null 2>&1
  run env -C "$repo" "$scripts/ws-merge.sh" --root "$root" --yes
  [ "$status" -eq 0 ]
  [[ "$output" != *'nothing to do'* ]]
  [ -n "$(env -C "$repo" jj log --no-pager --no-graph \
    -r '@ & descendants(description(substring:"ws work 1"))' -T '"x"')" ]
}

@test "a fully joined stack with @ already on top is a no-op" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 1)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  root=$(env -C "$ws" jj log --no-pager --no-graph -r "$STACK_ROOT" -T 'change_id.short()')
  env -C "$repo" "$scripts/ws-merge.sh" --root "$root" --yes >/dev/null 2>&1
  before=$(history_of "$repo")
  run env -C "$repo" "$scripts/ws-merge.sh" --root "$root" --yes
  [ "$status" -eq 0 ]
  [[ "$output" == *'nothing to do'* ]]
  [ "$(history_of "$repo")" = "$before" ]
}

@test "--yes on two commits produces a real two-parent merge" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 2)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  # Run from the integration workspace: the merge lands as *its* @, which is where conflicts
  # would be resolved and the gate run.
  run env -C "$repo" "$scripts/ws-merge.sh" --root "$(env -C "$ws" jj log --no-pager --no-graph \
    -r "$STACK_ROOT" -T 'change_id.short()')" -m 'Merge feat' --yes
  [ "$status" -eq 0 ]
  [[ "$output" == *'clean merge'* ]]
  parents=$(env -C "$repo" jj log --no-pager --no-graph -r 'parents(description(substring:"Merge feat"))' \
    -T 'description.first_line() ++ "\n"')
  [[ "$parents" == *'main work'* ]]
  [[ "$parents" == *'ws work 2'* ]]
}

@test "a conflicting merge is reported even though jj exits 0" {
  # The one that would otherwise slip through: jj records the conflict *in the commit* and
  # succeeds, so an exit-status check would call this a clean integration.
  repo=$(new_repo)
  env -C "$repo" "$scripts/ws-create.sh" feat >/dev/null 2>&1
  ws="$repo-feat"
  ( cd "$ws" && printf 'FEAT\n' >b.txt && jj commit -m 'feat edit' >/dev/null 2>&1 )
  ( cd "$repo" && printf 'MAIN\n' >b.txt && jj commit -m 'main edit' >/dev/null 2>&1 )
  root=$(env -C "$ws" jj log --no-pager --no-graph -r "$STACK_ROOT" -T 'change_id.short()')
  run env -C "$repo" "$scripts/ws-merge.sh" --root "$root" --merge -m 'Merge feat' --yes
  [ "$status" -ne 0 ]
  [[ "$output" == *CONFLICT* ]]
}

@test "re-running after integration says there is nothing to do" {
  repo=$(new_repo)
  ws=$(new_ws "$repo" feat 2)
  ( cd "$repo" && printf 'm\n' >m.txt && jj commit -m 'main work' >/dev/null 2>&1 )
  root=$(env -C "$ws" jj log --no-pager --no-graph -r "$STACK_ROOT" -T 'change_id.short()')
  env -C "$repo" "$scripts/ws-merge.sh" --root "$root" -m 'Merge feat' --yes >/dev/null 2>&1
  # Standing on the merge makes `default@-` mean both parents, so the default destination is
  # ambiguous until you step off it — the script has to explain that rather than just failing.
  run env -C "$repo" "$scripts/ws-merge.sh" --root "$root"
  [[ "$output" == *'nothing to do'* || "$output" == *'sitting ON a merge commit'* ]]
}
