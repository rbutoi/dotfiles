---
name: jj-workspace
description: Create and work in an isolated jj workspace (jj's equivalent of a git worktree) so a task can proceed without disturbing the main working copy — typically because another agent, a long build, or a dev server holds it. Invoke as /jj-workspace <name>; the workspace is created before Claude reads anything. With no name it creates nothing and just loads the guidance.
disable-model-invocation: true
allowed-tools: Bash(${CLAUDE_SKILL_DIR}/scripts/ws-create.sh *)
---

!`${CLAUDE_SKILL_DIR}/scripts/ws-create.sh $ARGUMENTS 2>&1`

# jj workspace (parallel checkout)

**The block above is `ws-create.sh`'s real output — it already ran.** Claude Code executes it while
expanding this file, before any of this text reaches the model, so creating the workspace is not a
step to decide on or a command to re-run. Read it for the path, the base, and the stack root; those
are yours to *use*, not to recite. If it reports an error instead (no name given, name taken,
ambiguous base), nothing was created; say what it said, in one line.

## Reporting: one line, then the work

This is `claude --worktree <name>`'s equivalent, and it should read like it — a workspace is *setup*,
not an accomplishment. Acknowledge it in **one line, plus the launch command** the script printed,
and stop:

```
Workspace `linear` at /Users/radu/dev/radu_materia_utils-linear (base b9fd7d67). Start a session in it:

    cd /Users/radu/dev/radu_materia_utils-linear; pnpm install; claude
```

That launch line is the point of the whole thing (§2) and the one piece of the output worth passing
on. Not the sections, the `+` echoes, the file count, the stack root, the `cd` prefix — that is all
above in context already. Not the dependency-install line as a chore for them either: if the task
needs the deps, run the install yourself, first thing. Don't ask what they'd like to build. **Do**
surface a printed warning (empty base, uncommitted `@` left behind) or a base that looks wrong —
those change what they do next. The rest of this file is reference for the later phases; read it
when you reach them, not out loud.

A jj workspace is jj's version of a git worktree: **its own working copy on disk, attached to the
same repo**. Use one when the main working copy is occupied and shouldn't be touched. The scripts
under `scripts/` validate, refuse rather than guess, and print the handles for the next step; they
echo every command that *changes* something (`+ jj rebase -s x -d y`) and stay quiet about read-only
queries, so quote those `+` lines when asked what a script did — not by default. Everything here is
what they can't decide for you; why the skill is *shaped* this way is [DESIGN.md](DESIGN.md).

## The argument is the workspace name

`/jj-workspace <name>` — a short kebab-case slug (`filetree-width`, `flaky-e2e`). Anything after it
goes to the script too, so `/jj-workspace flaky-e2e --base xyz` works. With **no argument** it
creates nothing, which is how you load this file on purpose for the integrate and remove steps.

Claude can't derive the name (nothing to derive from yet when the script runs) and can't invoke this
skill on its own. If the user meant to create one, propose a slug and ask them to re-run
`/jj-workspace <slug>` rather than reaching for `ws-create.sh` yourself — running it by hand works,
but then the transcript has two mechanisms for one thing.

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
  takes it by running jj **in that workspace's own directory**.
- A workspace's `@` in the repo only reflects the last jj command run **with that workspace as cwd**.
  Reading `<name>@` from elsewhere can show a stale, empty-looking working copy that in fact has
  uncommitted edits. Snapshot first (`cd <ws>; jj status`) before believing it.

**Also shared, and this one is easy to forget: the machine.** Fixed ports, dev servers, caches,
`dist/`. A test harness that reuses "the server already on port N" will happily attach to a *another
workspace's* server and report on their tree — green over your broken change, or, worse, golden
screenshots regenerated from their build, which is committable and looks plausible. Before trusting
a run that starts a server, check that the port belongs to this checkout.

## 1. Create — already done, above

It bases on `@-`, the newest real commit, since the tip `@` is conventionally empty. Your check is
after the fact: read the `base:` line and any warning (empty base, uncommitted work in `@` that
won't come along), and if the base is wrong, say so rather than carrying on. Fixing it costs nothing
yet — `ws-remove.sh <name>`, then `/jj-workspace <name> --base <rev>`. `--help` covers the flags.

The **stack root** it prints is the stable handle for the work — that initial empty change becomes
your first commit, and change IDs survive rebases where hashes don't. You rarely need it, since
`ws-merge.sh` derives it from the workspace name; it's the fallback for a stack whose workspace is
gone, or one that grew a second root.

Do **not** use `EnterWorktree`: it makes a *git* worktree, which jj doesn't track.

## 2. Work in it — from a session started inside it

**Hand the user the launch line and let them drive the work from there.** A session started in the
workspace has it as its own working directory: `cd` sticks, no command needs a prefix, and the
tooling behaves as if the workspace were the repo.

```fish
cd /path/to/<repo-dir>-<name>; claude
```

That session gets the repo's own `CLAUDE.md` / `AGENTS.md`, and the essentials from this file via
the `SessionStart` hook in `scripts/ws-session-context.sh`; `/jj-workspace` bare loads the rest.
Creating the workspace is therefore best done at the *start* of a task, when there's little context
to carry across.

**If you stay in this session** — a one-line fix, a quick look — prefix *every* command. The `cd`
does not carry to the next call, because the workspace is deliberately outside the session's allowed
directories, and jj resolves the workspace from cwd, so an unprefixed command operates on the *main*
workspace (`jj -R` points at the repo, not the workspace — it won't help):

```fish
cd /path/to/<repo-dir>-<name>; jj st
cd /path/to/<repo-dir>-<name>/subproject; pnpm check
```

Ignored files aren't materialized, so a fresh workspace has **no `node_modules`, build output, or
`.venv`**. `ws-create.sh` prints the install command it detected; run it yourself, silently, before
anything else, and run the project's gate here rather than in the main dir.

### Commit as you go — this is the one that gets skipped

**After each self-contained unit of work, commit.** A fix, a refactor, one step of a feature, a
newly-green test run:

```fish
cd /path/to/<repo-dir>-<name>; jj commit -m 'area: what changed'
```

`jj commit` describes the current `@` and opens a fresh empty one above it, so the tip stays empty and
the next unit starts clean. (`jj describe -m '…'` instead names `@` up front and lets a later bare
`jj commit` close it.)

**Why the reflex doesn't fire here:** jj auto-snapshots the working copy into `@` on every command.
No staging, no dirty tree, no `git status` growing longer, no step that fails because you forgot — so
the cue that normally provokes a commit never arrives. Three unrelated changes pile into one nameless
`@` and everything keeps working, until the stack that should have been four reviewable commits is
one and the fix is `jj split` after the fact.

So: part of finishing a unit, not a checkpoint to be prompted for. Committing in your own workspace
rewrites nothing shared, needs no permission, and `jj undo` reverses it — the opposite of §3, where
*integrating* is the user's call. Commit before switching topic. `jj log` is the check: more than one
unit's worth in `@`, or an `@` still undescribed after real edits, is the smell.

## 3. Integrate — never on your own initiative

**Integrating is the user's call, every time.** It rewrites shared history in a repo other people and
agents are using. Finishing the work is not authorisation. A green test run is not authorisation.
Neither is "they asked me to build it". What you may do unasked: run the dry run, show the plan, stop.
What needs the user asking for *that* merge, in the conversation you're in: passing `--yes`. Same
rule for `ws-remove.sh --force` and any `jj abandon` — propose, don't perform.

**First, check the other side didn't build the same thing.** `jj log`, then `jj diff -r <rev> --stat`
on anything that sounds related. Parallel agents on related briefs converge more than you'd expect,
and rebasing then yields a duplicate implementation plus a conflict in every shared file. Don't merge
both — compare, keep the better one whole, abandon the other, and say plainly which won, including
when it isn't yours.

**Then run `/simplify` over the stack, before the merge.** The workspace is the last moment it's
cheap: the commits are still yours alone, so a cleanup is an ordinary edit rather than a follow-up
apologising for the commit before it. (Style only — `/code-review` is the one that looks for bugs.)
Its fixes land in the working copy, so commit them like any other unit before merging.

Nothing moves between directories — the commits are already in the shared repo. All that's left is
how the two lines of history join, and the script picks that by size, announcing which it chose:

| Stack | What happens | Why |
| ----- | ------------ | --- |
| More than one commit | Merge commit (`jj new <onto> <tip>`) | Several commits are a branch, worth keeping visible as one |
| Exactly one commit | Rebased straight on | A two-parent node whose side is one change records nothing the commit doesn't |

`--merge` / `--rebase` force either shape; `-m` sets the merge message.

```fish
cd /path/to/repo                     # the integration checkout, NOT the feature workspace
~/.claude/skills/jj-workspace/scripts/ws-merge.sh <name> -m "Merge <name>"
```

**Say as little as possible; the rest is inferred.** The name identifies the stack and the script
derives its root, the destination (`default@-`, override with `--onto`), and the shape. Drop the name
too and it takes the stack this workspace is building; several candidates gets you their names, not a
guess. `--root <change-id>` is for a stack no workspace names any more.

**Run it from the integration checkout** — that's where a merge lands, and where you'd gate the
merged tree. (For a known rebase, running from *inside* the feature workspace is better still: jj
updates that working copy as it goes, so it can't go stale.) The first run only prints a plan.

It then advances the working copies so no checkout is left beside the work or standing on a merge,
skipping any `@` that has uncommitted changes or children of its own and printing a `jj rebase` line
for that instead. It says which; [DESIGN.md](DESIGN.md) says why. Two jj behaviours it papers over
but you'll still see: a **conflicting merge exits 0** (jj records conflicts in the commit, so the
script checks the `conflicts()` revset), and a clean **merge commit shows as `(empty)`**, which is
correct. `jj undo` reverses the whole operation.

Afterwards the destination has moved under you: **re-run the project's install if the other side
touched a lockfile**, then the gate.

**When another agent holds the main workspace, don't merge for them.** You can't tell whether they're
finished. Finish your side, then hand the human the exact `ws-merge.sh` line with the real IDs, plus
the removal command.

## 4. Remove (once merged, or abandoned)

```fish
~/.claude/skills/jj-workspace/scripts/ws-remove.sh <name>
```

Runs from anywhere, **including inside the workspace it deletes** — it steps out to the main
checkout itself, then prints the `cd` your shell needs, which is the one thing it can't fix for you.
It refuses while uncommitted changes exist (`--force` discards them) — the only genuinely lossy
case, because **committed work always survives removal**. An unmerged stack is therefore a note, not
a blocker: it stays reachable by change ID.

## When the scripts aren't the answer

They cover the common shape (one sibling workspace, join onto another's tip, delete). For anything
else — merging into a bookmark, adopting a workspace someone else made, a stack that needs splitting
— drive jj directly:

```fish
jj workspace add --name <name> -r <rev> <path>
jj rebase -s <root-change-id> -d <dest>
jj workspace forget <name>          # then rm -rf the directory yourself
jj workspace update-stale           # in a workspace whose commits were rebased under it
```

The scripts are short and their errors say what they checked; read the relevant one rather than
working around a refusal you don't understand. If you change one, [DESIGN.md](DESIGN.md) says how
they're tested.
