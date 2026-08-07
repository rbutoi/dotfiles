---
name: jj-workspace
description: Create and work in an isolated jj workspace (jj's equivalent of a git worktree) so a task can proceed without disturbing the main working copy — typically because another agent, a long build, or a dev server holds it. Use when asked to "make a jj workspace/worktree", to work on something "in parallel", or when a task needs its own checkout of a jj repo. Takes the workspace name as its argument.
---

# jj workspace (parallel checkout)

A jj workspace is jj's version of a git worktree: **its own working copy on disk, attached to the
same repo**. Use one when the main working copy is occupied and shouldn't be touched.

The mechanics live in three scripts under `~/.claude/skills/jj-workspace/scripts/`. They validate,
refuse rather than guess, and print the handles needed for the next step. Everything below is the
part they can't decide for you.

Every command that *changes* something is echoed before it runs — `+ jj rebase -s x -d y`,
`+ rm -rf /path` — so what they did to the repo is a short list of ordinary commands you can re-run,
adapt, or quote back, rather than something to reconstruct from the resulting graph. The read-only
queries behind each decision stay quiet. Relay those `+` lines when reporting what happened.

## The argument is the workspace name

`/jj-workspace <name>` — a short kebab-case slug for the work (`filetree-width`, `flaky-e2e`). If no
name was given, derive one from the task and say which you picked; only ask if the task is too vague
to name.

## Mental model: what is and isn't isolated

**Isolated:** the working copy (files on disk) and its `@` commit. Two workspaces can't be editing
the same files.

**Shared:** the repo store, the op log, and therefore *every commit and change ID*. A workspace is
not a private branch:

- Your commits appear in the other workspace's `jj log` the moment you make them. Expected, not a bug.
- `jj workspace list` shows each workspace's `@`. In revsets, `<name>@` is that working-copy commit
  and `<name>@-` its parent — so `default@-` is the main workspace's last *committed* revision.
- **Never** `jj abandon`, `jj rebase`, `jj describe`, or `jj edit` another workspace's `@` or commits,
  and don't move bookmarks someone else may be building on. That's the one way to actually break them.
  The single exception `ws-merge.sh` takes is an *empty, childless* `@` after an integration, and it
  takes it by running jj **in that workspace's own directory** — see step 3.
- A workspace's `@` in the repo only reflects the last jj command run **with that workspace as cwd**.
  Reading `<name>@` from elsewhere can show a stale, empty-looking working copy that in fact has
  uncommitted edits. Snapshot first (`cd <ws>; jj status`) before believing it.

## 1. Create

```fish
~/.claude/skills/jj-workspace/scripts/ws-create.sh <name>
```

It bases on `@-` — the newest real commit, since the tip `@` is conventionally empty. If `@` is an
*empty merge* — the state you're in right after integrating anything — `@-` would mean both of its
parents, so it seals the merge with `jj new` first and bases on that. The tree is unchanged; the
merge just becomes a real commit instead of a working copy, which is where the new stack belongs.
The judgement
call is whether that's the base you want: pass `--base <rev>` to start from somewhere else, and read
the warnings it prints (an empty base, or uncommitted work in `@` that won't come along). It puts the
workspace in a sibling directory and refuses to nest one inside the repo, where formatters, watchers
and test runners would scan it. `--help` covers the remaining flags.

**Note the stack root it prints.** That initial empty change *becomes* your first commit, so it's
the stack root — the stable handle for the work, since change IDs survive rebases and commit hashes
don't. Day to day you won't need it: `ws-merge.sh` derives it from the workspace name. It's the
fallback for the cases a name can't express — a stack whose workspace is already gone, or one that
grew a second root.

Do **not** use `EnterWorktree` for this. It creates or enters a *git* worktree, which jj doesn't
track — even in a colocated repo, a jj workspace never appears in `git worktree list`.

## 2. Work in it

The shell cwd resets between tool calls, so **prefix every command** with the workspace path:

```fish
cd /path/to/<repo-dir>-<name>; jj st
cd /path/to/<repo-dir>-<name>/subproject; pnpm check
```

jj resolves the workspace from cwd, so a command run from the main directory operates on the *main*
workspace. `jj -R` points at the repo, not the workspace — it won't help.

Ignored files aren't materialized, so a fresh workspace has **no `node_modules`, build output, or
`.venv`**. `ws-create.sh` detects lockfiles and prints the install command rather than guessing; run
it before anything else. Run the project's gate in the workspace, not the main dir.

Commit per logical change and leave one empty `@` at the tip, as usual.

## 3. Integrate — never on your own initiative

**Integrating is the user's call, every time.** It rewrites shared history in a repo other people and
agents are using, so it is not yours to decide is due. Finishing the work is not authorisation. A
green test run is not authorisation. Neither is "they asked me to build it".

What you may do unasked: run the dry run, show the plan, and stop. What needs the user asking for
*that* merge, in the conversation you're in: passing `--yes`. Same rule for `ws-remove.sh --force`
and any `jj abandon` — propose, don't perform.

**First, check the other side didn't build the same thing.** Read their commits before integrating —
`jj log`, then `jj diff -r <rev> --stat` on anything that sounds related. Parallel agents handed
related briefs converge more than you'd expect: two independent takes on one feature will land on
the same file names, and rebasing then produces a duplicate implementation plus a conflict in every
shared file. When it happens, don't merge both — compare them, keep the better one whole, and abandon
the other. Say plainly which won, including when it's not yours; look specifically for what the
other version caught that yours didn't.

Otherwise: nothing moves between directories — the commits are already in the shared repo. All that's
left is deciding how the two lines of history join.

**The shape is chosen by size, and the script announces which it picked:**

| Stack | What happens | Why |
| ----- | ------------ | --- |
| More than one commit | Merge commit (`jj new <onto> <tip>`) | Several commits are a branch, and worth keeping visible as one |
| Exactly one commit | Rebased straight on | A two-parent node whose side is a single change records nothing the commit doesn't already say |

`--merge` and `--rebase` force either shape. `-m` sets the merge message and is called out as ignored
if the stack turns out to be rebased.

```fish
cd /path/to/repo                     # the integration checkout, NOT the feature workspace
~/.claude/skills/jj-workspace/scripts/ws-merge.sh <name> -m "Merge <name>"
```

**Almost everything is inferred; say as little as possible.** The workspace name identifies the
stack — the script derives its root itself. Drop even that and it takes the stack this workspace is
building (when run from inside one), or the single sibling workspace with committed work on the
destination; several candidates gets you their names, not a guess. `--root <change-id>` is for a
stack no workspace names any more. It prints the stack it chose, and the shape it chose, before
touching anything, and its `--yes` line repeats what you typed rather than the ids it resolved.

**Run it from the integration workspace** (normally the main checkout). That's the right cwd for a
merge, which lands in *that* workspace — its tree is the merged one, which is where the gate runs
— and it's the safer default for a rebase too: the feature workspace goes stale, and the script
re-syncs it. Getting it the other way round misfiles the merge
commit into a workspace you're about to delete. (If you know it's a rebase, running from inside the
feature workspace avoids the staleness entirely, since jj updates that working copy as it goes.)

**A clean merge is then sealed with `jj new`,** so `@` ends up empty *above* the merge rather than
on it. That's deliberate and it's free — same tree, so the gate still runs on the merged content —
and it's what stops `@-` meaning both parents for everything you run here next. A **conflicting**
merge can't be sealed (the resolution has to happen in `@`, which is the merge), so the script says
to `jj new` once you've resolved and gated. `--no-advance` opts out of the seal.

**A rebase is two moves, and the second one is easy to miss.** The stack goes *onto* the
destination — which means it lands *beside* the trailing empty `@` that was sitting on it, not under
it. Leave that `@` there and the mainline has forked: the integrated commit and your next commit are
siblings. So the script also moves the `@` of the workspace it runs in on top of the integrated tip,
and re-running it on an already-joined stack fixes exactly that stranded-`@` state rather than
reporting "nothing to do". `--no-advance` opts out.

**It moves the other workspace's `@` too — and that is about files, not history.** The workspace
that was continuing the destination's line (the one `--onto` was derived from: `default@-` →
`default`) ends up beside the work, so its *checkout* still holds the pre-integration tree and any
dev server watching it goes on serving the old build. So the script moves that `@` as well, by
running jj **in that workspace's own directory** — which is the whole trick: a `jj rebase -r
default@` issued from here rewrites the commit but cannot touch the other checkout, leaving it
stale. In merge mode the merge is *this* workspace's `@`, i.e. uncommitted state, so it seals it
with `jj new` first rather than hanging another checkout off a working copy still being edited.
Other siblings are left alone deliberately: every fresh workspace is based on `default@-` too, and
none of them was continuing that line. Workspaces the rebase moved *under* get
`jj workspace update-stale` run for them, same reasoning — a sync is not a rewrite.

Two `@`s it still won't rewrite, printing a `jj rebase -r @ -d <tip>` line instead:

- **one with uncommitted changes** — that's work in progress, and moving it can conflict. It
  snapshots the other workspace first (`jj status` in its directory) before believing it's empty,
  for the staleness reason above;
- **one with children of its own** — not a trailing tip, so moving it would drag them along.

Either way the first run only prints a plan. Hand the echoed `--yes` line to the user; don't run it
yourself (see above).

Destination defaults to `default@-` — the mainline's newest committed revision, and what the stack
inference above measures "not integrated yet" against; override with `--onto`. It refuses either
operation onto a workspace's `@` — that's somebody's uncommitted working copy — unless that `@` is
an empty merge, which holds no work to lose (below). It no-ops with a clear message when
the stack is already joined. `jj undo` reverses the whole thing.

Three jj behaviours that don't match git intuitions, all of which the script handles but you'll see:

- **A conflicting merge succeeds and exits 0.** jj records the conflict *in the commit* instead of
  failing, so the exit status tells you nothing — the script checks the `conflicts()` revset and says
  so loudly. Resolve by editing the files (markers are jj's own format: `%%%%%%%` is the diff one
  side applied, `+++++++` the other side's content), then any jj command re-snapshots and clears it.
- **A clean merge commit shows as `(empty)`.** That's correct for a merge, not a failure. A merge you
  resolved conflicts in is non-empty, because the resolution is its own change.
- **Standing on a merge makes `@-` ambiguous** — it means both parents. The scripts no longer put
  you there (see the seal above), but a merge made by hand, or with `--no-advance`, or before that
  sealing existed, still does — and when the merge is *empty* it has an answer: the merge
  itself, which holds no work of its own and is the only commit with both parents' lines in it
  (landing on either parent alone would drop the other side). `ws-merge.sh` resolves to it,
  `ws-create.sh` seals it into a commit first. A merge that still holds changes is somebody's
  resolution or WIP, so that one is refused with the reason.

**When another agent holds the main workspace, don't merge for them.** You can't tell whether they're
finished — there's no way to ask an agent in another directory. Finish your side, then hand the human
the exact `ws-merge.sh` line with the real IDs filled in, plus the removal command.

## 4. Remove (once merged, or abandoned)

```fish
~/.claude/skills/jj-workspace/scripts/ws-remove.sh <name>
```

Run it from outside the workspace. It refuses while uncommitted changes exist (`--force` discards
them) — the only genuinely lossy case, because **committed work always survives removal**. An
unmerged stack is therefore a note, not a blocker: it stays reachable by change ID.

## When the scripts aren't the answer

They cover the common shape (one sibling workspace, join onto another's tip, delete). For
anything else — merging into a bookmark, adopting a workspace someone else made, a stack that needs
splitting — drive jj directly:

```fish
jj workspace add --name <name> -r <rev> <path>
jj rebase -s <root-change-id> -d <dest>
jj workspace forget <name>          # then rm -rf the directory yourself
jj workspace update-stale           # in a workspace whose commits were rebased under it
```

The scripts are short and their errors say what they checked; read the relevant one rather than
working around a refusal you don't understand.

**If you change one, run the tests:** `bats tests/` (bats-core, installed globally via mise). They
drive a real throwaway jj repo per test — no mocks, since everything worth testing here is an
interaction with jj — and assert on the refusals, both shapes of integration, and the conflict path.
Two traps they encode, both of which produced tests that passed while checking nothing:

- `description("x")` defaults to an **exact** match and jj stores a trailing newline, so the
  unqualified form silently matches nothing. Use `description(substring:"x")`.
- A stack branched off the mainline tip already descends from it, so integrating without first
  moving the mainline is correctly a no-op — a fixture that skips that tests the wrong branch.
