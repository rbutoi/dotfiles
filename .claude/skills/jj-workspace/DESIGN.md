# jj-workspace — why the skill is shaped this way

Everything here is true and none of it is actionable while you're doing a task, which is why it
isn't in [SKILL.md](SKILL.md): that file is loaded in full on every invocation, so anything a
working agent won't act on is a tax on the ones that do. Read this when you're **editing** the
skill, or when a behaviour surprises you and you want the reason.

## The create runs during expansion

SKILL.md's one `!`command`` line runs before any of its text reaches the model. That's what makes
"the workspace already exists" true rather than an instruction, and it's why the file opens by
saying so — otherwise the model's first move is to run a create that already ran.

Three details are load-bearing and all fail *silently* if undone, so `tests/skill-md.bats` pins
them:

- **`$ARGUMENTS`, never `$0`.** An indexed placeholder with no matching argument is left verbatim, so
  a bare `$0` reaches bash as `$0` and expands to the shell's name — `/jj-workspace` with no argument
  would create a workspace called `bash`. Unsubstituted `$ARGUMENTS` is simply empty, and the script
  refuses.
- **`2>&1`.** Every refusal goes to stderr, and only stdout replaces the placeholder. Without the
  merge, a failed create injects an empty block, which reads as success.
- **The injected path matches the `allowed-tools` glob.** If they drift, the skill still works but
  stops to ask for permission — which defeats running it during expansion.

## Why the model can't invoke it, and what that costs

`disable-model-invocation: true`, because invoking has a side effect: a directory appears on disk.
Auto-invocation would create one because a task merely *sounded* parallel. The price is that the
integrate and remove guidance is unreachable unless the user invokes the skill, so it can't be
consulted mid-task the way a reference normally would. Hence the no-argument path: it creates
nothing and just loads the file. That path has to stay harmless, because there is no way to load
the guidance *and* skip the script.

## Why `cd` can't stick in the creating session

Claude resets the shell cwd after every Bash call unless the directory is in the session's allowed
set, and a workspace is deliberately a *sibling* of the repo, so it isn't. `/add-dir <path>` fixes
it, but only the user can type that: there is no `SlashCommand` tool, the skill's `!` line is a
subprocess that can't reach the in-memory set, and command hooks can only return
`additionalContext` / `permissionDecision` / `updatedInput` — not the `addDirectories` permission
update that would do it.

`EnterWorktree` *does* perform exactly that update, which is why `claude --worktree` feels seamless.
It's a built-in, and it refuses a jj workspace because the path must appear in `git worktree list` —
which a jj workspace never does, even in a colocated repo. So this isn't a bug to work around: start
the session in the workspace, or prefix every command.

## The SessionStart hook

`scripts/ws-session-context.sh` covers the gap for a session started *inside* a workspace: that
session loads the repo's `CLAUDE.md`/`AGENTS.md` (same repo content) but not this skill. The hook
detects a secondary workspace and injects the four rules that bite — commit cadence first.

Detection is `.jj/repo` being a *file* (a pointer to the main repo) rather than a directory, after
resolving the workspace root, since `.jj` exists only at the root and a session may start in a
subdirectory. That's an on-disk detail rather than documented API, but jj offers no first-class "am
I in a secondary workspace" query, and the workspace's own name isn't stored under its `.jj` at all
(it lives in the shared op store). `tests/ws-session-context.bats` pins the cases, colocated repos
and subdirectories included.

**The hook is registered in `~/.claude/settings.json`, which this repo does not track**, so on a new
machine the skill arrives and the hook doesn't. If a workspace session seems unaware it's in one,
check that registration first.

## What `ws-merge.sh` does to working copies, and why

Integration is about two things at once — history, and the *files* other checkouts are looking at.

**Sealing a clean merge with `jj new`.** It leaves `@` empty *above* the merge rather than on it.
Free (same tree, so a gate still runs on the merged content), and it stops `@-` meaning both parents
for everything run next. A conflicting merge can't be sealed — the resolution has to happen in `@`,
which *is* the merge — so the script says to `jj new` once you've resolved and gated.

**A rebase is two moves, and the second is easy to miss.** The stack goes *onto* the destination,
which lands it *beside* the trailing empty `@` that was sitting there, not under it. Leave that `@`
and the mainline has forked: the integrated commit and your next commit are siblings. So the script
advances the `@` of the workspace it runs in, and re-running on an already-joined stack fixes exactly
that stranded state rather than reporting "nothing to do".

**Moving the other workspace's `@` is about files, not history.** The workspace continuing the
destination's line (the one `--onto` came from: `default@-` → `default`) ends up beside the work, so
its checkout still holds the pre-integration tree and any dev server watching it serves the old
build. The script moves it by running jj **in that workspace's own directory** — that's the whole
trick, since a `jj rebase -r default@` issued from elsewhere rewrites the commit but cannot touch
the other checkout, leaving it stale. In merge mode it seals first, rather than hanging another
checkout off a working copy still being edited. Other siblings are left alone deliberately: every
fresh workspace is based on `default@-` too, and none of them was continuing that line. Workspaces
the rebase moved *under* get `jj workspace update-stale` — a sync is not a rewrite.

Two `@`s it won't rewrite, printing a `jj rebase -r @ -d <tip>` line instead: one with uncommitted
changes (work in progress; moving it can conflict), and one with children of its own (not a trailing
tip, so moving it would drag them along). It snapshots the other workspace first — `jj status` in
its directory — before believing an `@` is empty, for the staleness reason above.

## jj behaviours that don't match git intuitions

- **A conflicting merge succeeds and exits 0.** jj records the conflict *in the commit*, so exit
  status tells you nothing; the scripts check the `conflicts()` revset. Conflict markers are jj's own
  format: `%%%%%%%` is the diff one side applied, `+++++++` the other side's content. Any jj command
  re-snapshots and clears the conflict once the files are edited.
- **A clean merge commit shows as `(empty)`.** Correct for a merge. One you resolved conflicts in is
  non-empty, because the resolution is its own change.
- **Standing on a merge makes `@-` ambiguous** — it means both parents. When the merge is *empty* it
  has an answer: the merge itself, which holds no work of its own and is the only commit carrying
  both parents' lines (landing on either parent alone would drop the other side). `ws-merge.sh`
  resolves to it; `ws-create.sh` seals it into a commit first. A merge that still holds changes is
  somebody's resolution or WIP, and is refused with the reason.

## Tests

`bats tests/` (bats-core, installed globally via mise). They drive a real throwaway jj repo per test
— no mocks, since everything worth testing here is an interaction with jj — and assert on the
refusals, both shapes of integration, and the conflict path. Two traps they encode, both of which
once produced tests that passed while checking nothing:

- `description("x")` defaults to an **exact** match and jj stores a trailing newline, so the
  unqualified form silently matches nothing. Use `description(substring:"x")`.
- A stack branched off the mainline tip already descends from it, so integrating without first
  moving the mainline is correctly a no-op — a fixture that skips that tests the wrong branch.
