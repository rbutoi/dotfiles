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
Left alone it makes a git worktree, and its `path` form refuses a jj workspace because the path must
appear in `git worktree list` — which a jj workspace never does, even in a colocated repo. With
`ws-worktree-hook.sh` registered (next section) the creation path is ours instead, and the session
moves into the workspace with its directory allowed. Without the hook: start the session in the
workspace, or prefix every command.

## The WorktreeCreate / WorktreeRemove hook

`scripts/ws-worktree-hook.sh` is what makes the built-ins create jj workspaces. It is a settings
command hook rather than a plugin hooks module (a "mod"), for two reasons that both fail silently
if forgotten: the engine bypasses user-tier module hooks on these events, and `--worktree` creates
its worktree before plugin modules load.

A configured hook *replaces* Claude's worktree handling in every repo, and printing nothing is an
error, not a fall-through. So outside jj the script has to make the git worktree itself
(`.claude/worktrees/<name>`, branch `worktree-<name>`, from origin's default branch per
`worktree.baseRef`). That is a reimplementation of the built-in's core only: copying local
settings and `worktree.symlinkDirectories` are not reproduced, so a repo that leans on those is
worse off with the hook registered.

Re-entering a name resumes its worktree. A path occupied by something that is *not* a worktree —
debris from a tool whose cwd was a removed worktree, such as a package install — is cleared so the
name stays usable: a tree of empty directories is removed, and anything holding a file is moved to
`.git/orphaned-worktrees/`, where nothing scans or commits it, so no file is ever deleted. Those
directories are never pruned automatically. A failed creation leaves no branch behind, because
`git worktree add -b` makes the branch before the rest of the work can fail.

Inside jj it creates through `ws-create.sh`, so naming and base have one definition, and removes
with `jj workspace forget` plus `rm -rf` after refusing a workspace with uncommitted changes or the
main checkout. `ExitWorktree`'s own dirty check is git-based and refuses a jj workspace unless
`discard_changes: true` is passed; the real guard is the hook's own.

Like the SessionStart hook, **the registration lives in `~/.claude/settings.json`, which this repo
does not track**, so on a new machine the script arrives and the hook doesn't.

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

## jj behaviours that don't match git intuitions

- **A conflicting merge succeeds and exits 0.** jj records the conflict *in the commit*, so exit
  status tells you nothing; check the `conflicts()` revset. Conflict markers are jj's own
  format: `%%%%%%%` is the diff one side applied, `+++++++` the other side's content. Any jj command
  re-snapshots and clears the conflict once the files are edited.
- **A clean merge commit shows as `(empty)`.** Correct for a merge. One you resolved conflicts in is
  non-empty, because the resolution is its own change.
- **Standing on a merge makes `@-` ambiguous** — it means both parents. When the merge is *empty* it
  has an answer: the merge itself, which holds no work of its own and is the only commit carrying
  both parents' lines (landing on either parent alone would drop the other side). `ws-create.sh`
  seals it into a commit first. A merge that still holds changes is
  somebody's resolution or WIP, and is refused with the reason.

## Tests

`bats tests/` (bats-core, installed globally via mise). They drive a real throwaway jj repo per test
— no mocks, since everything worth testing here is an interaction with jj — and assert on the
refusals, paths and output of `ws-create.sh` and the worktree hook, for jj and plain git alike. One
trap they encode once produced a test that passed while checking nothing:

- `description("x")` defaults to an **exact** match and jj stores a trailing newline, so the
  unqualified form silently matches nothing. Use `description(substring:"x")`.
