# worktree-hook

`worktree-hook.py` makes `claude --worktree <name>`, `EnterWorktree` and worktree-isolated subagents
create **jj workspaces** in a jj repo, and git worktrees everywhere else. It is a Claude Code
`WorktreeCreate` / `WorktreeRemove` command hook: JSON on stdin, and `WorktreeCreate` prints the new
path on stdout. Standard library only, and 3.9-compatible, because a hook runs with whichever
`python3` is first on PATH, which can be the system one.

## Registering

In `~/.claude/settings.json`, one entry per event:

```json
"hooks": {
  "WorktreeCreate": [{"hooks": [{"type": "command", "command": "$HOME/.claude/worktree-hook/worktree-hook.py"}]}],
  "WorktreeRemove": [{"hooks": [{"type": "command", "command": "$HOME/.claude/worktree-hook/worktree-hook.py"}]}],
  "SessionStart":   [{"hooks": [{"type": "command", "command": "$HOME/.claude/worktree-hook/worktree-hook.py nudge", "timeout": 10}]}]
}
```

The `SessionStart` entry is optional: it is the nudge about finished workspaces (below).

## Why a settings hook, and why it covers git too

- **Not a plugin.** The engine bypasses user-tier plugin hooks on these events, and `--worktree`
  creates its worktree before plugin modules load.
- **A configured hook replaces Claude's own worktree handling in every repo**, and printing nothing
  is an error, not a fall-through. So outside jj the hook makes the git worktree itself
  (`.claude/worktrees/<name>`, branch `worktree-<name>`, from origin's default branch per
  `worktree.baseRef`). That reimplements the built-in's core only: copying local settings and
  `worktree.symlinkDirectories` are not reproduced.
- **Trust.** Claude won't run a settings hook until the workspace trust dialog was accepted in the
  directory it launches from, and a parent directory doesn't count. A repo never opened
  interactively fails `--worktree` with "Workspace trust not yet accepted"; run `claude` there once.
  A workspace the hook creates inherits the session's trust, so it needs nothing.

## Behaviour

- **jj.** The workspace is `<main-checkout>-<name>`, a flat sibling: created from inside another
  workspace it is still `app-bar` beside `app-foo`, never `app-foo-bar`. It bases on `@-` (an empty
  merge `@` is sealed into a commit first, since `@-` would otherwise mean both parents). Names are
  made safe for a directory and a workspace id (a `/` becomes `-`). The name `default` is refused: it
  is the main checkout, never a worktree to hand back.
- **git.** A `/` in the name becomes `+`, as in Claude's own, so a worktree is never nested inside
  another; git judges what remains as a branch name. A new worktree starts from origin's default
  branch, freshly fetched, and falls back to the local `HEAD` when there is no origin, it can't be
  reached, or `worktree.baseRef` is `head`.
- **Re-entering a name resumes it**, whatever branch the worktree is on. A worktree deleted by hand
  has its stale registration pruned. A path occupied by something that is *not* a worktree (debris
  from a tool whose cwd was a removed worktree, such as a package install) is cleared: a tree of
  empty directories is removed, anything holding a file is moved to `.git/orphaned-worktrees/` (with a
  warning on stderr), so no file is deleted. A failed creation never leaves its branch behind, because `git worktree add -b`
  makes the branch before the rest can fail.
- **Removal refuses to lose work.** jj: uncommitted changes, a stale working copy and the main
  checkout are refused; committed work survives `forget`. Removing a path that no longer exists, or
  that isn't a repo, succeeds quietly. git: no `--force`, and a worktree that is not on a
  `worktree-*` branch the hook named (it was moved onto a feature branch, or detached) is left in place
  with a message saying how to remove it by hand, since that is where the real work lives. The branch
  it did name is deleted with `-d`, which keeps one with unmerged commits. `ExitWorktree`'s own check is git-based and refuses a
  jj workspace unless `discard_changes: true` is passed; the hook's guard is the real one.

## Finished workspaces

Nothing removes a jj workspace when its work is done, so they pile up (`land`, below, is the way to
end one deliberately). Claude only checks git worktrees it made itself when a session ends, and a hook
cannot tell it a workspace is clean, so this is a separate command:

- `worktree-hook.py prune [--dry-run] [--yes]` lists this repo's finished workspaces and the reason
  for each, then removes them on `y` (or all of them with `--yes`; with no terminal and no `--yes` it
  only lists). An unknown option is refused, so a mistyped `--dryrun --yes` cannot become "remove
  everything". Just before each removal it asks again whether that workspace is still finished,
  because removing one changes what the others hang on: two workspaces that hold the same unmerged
  commit each look finished while the other exists, and only one of them may go.
- `worktree-hook.py nudge` is a SessionStart hook: one line to the user, `jj: 2 finished workspaces
  (a, b) can be removed: … prune`, only when there is something to say, and it never fails a session.

**What "finished" means** is that nothing in the workspace exists nowhere else. Emptiness is neither
necessary nor sufficient: an empty `@` can sit on unmerged commits, and a merged workspace's `@` can
be a real commit. Concretely, the workspace's non-empty commits that are not reachable from trunk,
any bookmark, or another workspace's working copy must be none, or:

- **Already in the main line:** none left at all (a fast-forward, rebase or merge).
- **Already in trunk by content:** merging each remaining commit into trunk changes nothing
  (`git merge-tree --write-tree`), as after a squash or rebase merge, where the commits are new and
  ancestry can't tell. A merge that conflicts counts as not contained, so doubt keeps the workspace.

A workspace is also left alone if it changed in the last 15 minutes (`WORKTREE_HOOK_IDLE_SECONDS`), so
one you just made, which has nothing unique either, is never offered, or if any process is running in
it (and if that can't be told, it counts as in use).

**Stale workspaces are judged too**, from the working copy the repo records for them, because
integrating a workspace is what makes it stale, so finished ones usually are. That reading can miss
edits made since, so it only suggests. Before removing, `prune` runs `jj workspace update-stale` in
it (a no-op on a current one), which saves edits jj never snapshotted in a commit of their own, a
stray head nothing points at. If a new one appears, `prune` keeps the workspace and says where the
edits went, instead of removing the directory and stranding them.

Deleting the directory is instant from the caller's side: it is renamed out of the way and `rm -rf`
finishes in the background, because a workspace with `node_modules` can take tens of seconds to delete.
If that background delete is killed, a `.<name>.removing-<pid>` directory is left beside the repo.

Removal itself (`ExitWorktree`) asks a narrower question than `prune`: whether `@` would be lost,
since committed work stays reachable after `forget`. A non-empty `@` that is already in the main line,
or already in trunk, loses nothing, so it may go; a stale workspace is refused with jj's own hint.
`prune` asks whether anything is left to come back for, which is stricter.

Not covered: git worktrees (Claude cleans those itself), a workspace whose directory is already gone,
and telling a merged PR from GitHub, which would need the network. The content check needs git 2.38
or later (`merge-tree --write-tree`); on an older one nothing counts as contained, so squash-merged
workspaces are simply never offered.

## Landing a workspace

`worktree-hook.py land [<workspace>]` is how parallel work ends: it folds a workspace into `default`
and removes it, so no workspace is left behind to go stale. With no name it lands the one you are in.

Every command that rewrites commits runs from inside `default`: a rewrite is what makes a workspace
stale, and the only working copy that may move is the one the command runs in. The workspace's
commits go onto `default`'s parent and `default`'s `@` moves on top of them; then the workspace is
forgotten and its directory deleted as `prune` does. If the work is already under `default`, the
workspace is just removed.

What it refuses, before changing anything:

- an `@` with uncommitted changes, in the workspace or in `default` (commit or abandon them first);
- a workspace whose own commits already hold conflicts;
- a `default` whose `@-` is a merge, when there is work to carry.

A landing that conflicts with what `default` has gained is undone (`jj undo`) and the workspace left
exactly as it was: rebase it in the workspace, resolve the conflicts there, and land again.

Unlike `prune`, it does not check for processes running in the workspace. Landing is deliberate and
usually run from inside the workspace being removed, so the shell's own directory is gone afterwards;
it prints where `default` is.

## Tests

`bats tests/` (bats-core, installed via mise). They drive real throwaway jj and git repos, one per
test. Git's `fsmonitor`, if enabled globally, spawns a detached per-repo daemon that inherits bats'
output pipe and hangs the run, so the suite turns it off.
