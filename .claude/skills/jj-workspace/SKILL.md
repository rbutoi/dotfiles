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

The workspace is always `<main-checkout>-<name>`, beside the main checkout. Creating one from
inside another workspace gives a second sibling (`app-bar` next to `app-foo`), never `app-foo-bar`.

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
yet — remove it (§4), then `/jj-workspace <name> --base <rev>`. `--help` covers the flags.

The **stack root** it prints is the stable handle for the work — that initial empty change becomes
your first commit, and change IDs survive rebases where hashes don't. It's what `jj rebase -s`
takes when you integrate (§3).

`EnterWorktree`, `claude --worktree` and worktree-isolated subagents make a jj workspace too, **when
`scripts/ws-worktree-hook.sh` is registered** as the `WorktreeCreate` and `WorktreeRemove` hook (its
header has the settings entry; `~/.claude/settings.json` isn't tracked by this repo). Without it they
make a *git* worktree, which jj doesn't track — so a workspace missing from `jj workspace list` after
`EnterWorktree` means the hook isn't registered. Through the hook the session moves into the
workspace itself, so no second session has to be started.

`ExitWorktree` with `remove` needs `discard_changes: true` in a jj repo: the tool's own
uncommitted-work check reads git and can't see a jj workspace, so it refuses without the flag. That
flag is not what protects your work — the hook itself refuses to delete a workspace with
uncommitted changes, and committed work always survives removal. Check `jj st` in it first anyway.

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
Neither is "they asked me to build it". What you may do unasked: work out the plan (the `jj log` below), show it, stop.
What needs the user asking for *that* merge, in the conversation you're in: running it. Same rule
for discarding uncommitted work and any `jj abandon` — propose, don't perform.

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
how the two lines of history join, and that is plain jj, run from the integration checkout (where a
merge lands, and where you'd gate the merged tree):

| Stack | Command | Why |
| ----- | ------- | --- |
| More than one commit | `jj new <onto> <name>@- -m 'Merge <name>'` | Several commits are a branch, worth keeping visible as one |
| Exactly one commit | `jj rebase -s <stack-root> -d <onto>` | A two-parent node whose side is one change records nothing the commit doesn't |

`<onto>` is normally `default@-`, the main workspace's last committed revision. Show the user the
plan — `jj log -r '<onto> | <name>@-'` — before running anything.

Two jj behaviours look like failure and aren't: a **conflicting merge exits 0** (jj records conflicts
in the commit, so check `jj log -r 'conflicts()'`), and a clean **merge commit shows as `(empty)`**,
which is correct. `jj undo` reverses the whole operation.

Then tidy the working copies. A rebase lands the stack *beside* the trailing empty `@` that was
sitting there, not under it, so `jj rebase -r @ -d <tip>` in that workspace — otherwise the mainline
has forked and your next commit is a sibling of the integrated one. Skip any `@` with uncommitted
changes or children of its own. A workspace whose commits were rebased under it is stale: `jj
workspace update-stale` in it. Run every one of these **in that workspace's own directory**; from
elsewhere the rewrite happens but the other checkout isn't touched.

Afterwards the destination has moved under you: **re-run the project's install if the other side
touched a lockfile**, then the gate.

**When another agent holds the main workspace, don't merge for them.** You can't tell whether they're
finished. Finish your side, then hand the human the exact commands with the real IDs, plus how to
remove the workspace.

## 4. Remove (once merged, or abandoned)

```fish
cd /path/to/<repo-dir>-<name>; jj st     # uncommitted changes are the one lossy case
cd /path/to/<repo-dir>; jj workspace forget <name>; rm -rf /path/to/<repo-dir>-<name>
```

**Committed work always survives removal:** `forget` leaves every commit in the repo, reachable by
change ID, and `jj undo` restores the tracking (the directory stays deleted). An unmerged stack is a
note, not a blocker. `cd` out of the directory first, or your shell is left in one that no longer
exists. `ExitWorktree` with `remove` does this for a workspace the hook made, and refuses while
uncommitted changes exist.

## When the script isn't the answer

`ws-create.sh` covers the common shape (one sibling workspace off the newest commit). For anything
else — a different path, merging into a bookmark, adopting a workspace someone else made, a stack
that needs splitting — drive jj directly:

```fish
jj workspace add --name <name> -r <rev> <path>
jj rebase -s <root-change-id> -d <dest>
jj workspace forget <name>          # then rm -rf the directory yourself
jj workspace update-stale           # in a workspace whose commits were rebased under it
```

The script is short and its errors say what it checked; read it rather than working around a refusal
you don't understand. If you change it, [DESIGN.md](DESIGN.md) says how it's tested.
