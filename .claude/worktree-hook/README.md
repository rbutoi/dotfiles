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
  "WorktreeRemove": [{"hooks": [{"type": "command", "command": "$HOME/.claude/worktree-hook/worktree-hook.py"}]}]
}
```

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
  that isn't a repo, succeeds quietly. git: no `--force`, and only a branch the
  hook or Claude named is deleted, with `-d`. `ExitWorktree`'s own check is git-based and refuses a
  jj workspace unless `discard_changes: true` is passed; the hook's guard is the real one.

## Tests

`bats tests/` (bats-core, installed via mise). They drive real throwaway jj and git repos, one per
test. Git's `fsmonitor`, if enabled globally, spawns a detached per-repo daemon that inherits bats'
output pipe and hangs the run, so the suite turns it off.
