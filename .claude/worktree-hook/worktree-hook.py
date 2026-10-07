#!/usr/bin/env python3
"""Claude Code WorktreeCreate / WorktreeRemove hook: jj workspaces for its worktree features.

Makes `claude --worktree <name>`, EnterWorktree and worktree-isolated subagents create jj workspaces
in a jj repo, and git worktrees elsewhere. Reads the hook's JSON on stdin; WorktreeCreate prints the
new path on stdout. README.md has the registration entry, why a configured hook has to cover git too,
and why this is a settings hook rather than a plugin. Standard library only and 3.9-compatible: a
hook runs with whatever python3 is first on PATH, which can be the system one.
"""
import json
import os
import re
import shutil
import subprocess
import sys
import time

# Matches Claude's own worktree branches, so the hook's and the built-in's are interchangeable.
BRANCH_PREFIX = "worktree-"


class Fail(Exception):
    """A failure to report as `error: <message>` and exit 1."""


def die(message):
    raise Fail(message)


def warn(message):
    print("warning: " + message, file=sys.stderr)


def run(argv, cwd=None):
    """Run a command and capture its output. A missing binary or directory reads as a failed command
    (127), so "is this a jj repo?" is a question with a plain answer."""
    try:
        return subprocess.run(argv, cwd=cwd, capture_output=True, encoding="utf-8", errors="replace")
    except OSError as e:
        return subprocess.CompletedProcess(argv, 127, "", str(e))


def git(root, *args):
    return run(["git", "-C", root, *args])


def jj(cwd, *args):
    return run(["jj", "--no-pager", *args], cwd=cwd)


def out(proc):
    """A command's stdout, stripped, or None if it failed."""
    return proc.stdout.strip() if proc.returncode == 0 else None


def must(proc, what):
    """`proc`, or a Fail carrying its stderr."""
    if proc.returncode:
        die(proc.stderr.strip() or what + " failed")
    return proc


# ---- jj -------------------------------------------------------------------------------------


def jj_root(cwd, name=None):
    """The root of the current workspace (or of the one called `name`), or None outside a jj repo."""
    return out(jj(cwd, "workspace", "root", *(["--name", name] if name else [])))


def jj_has(cwd, revset):
    return bool(out(jj(cwd, "log", "--no-graph", "-r", revset, "-T", "1")))


def jj_name(raw):
    """A workspace name becomes a directory and a workspace id, so keep it boring."""
    return re.sub(r"^[^A-Za-z0-9]+", "", re.sub(r"[^A-Za-z0-9._-]", "-", raw))


def create_jj(cwd, root, raw_name):
    """Create the workspace for `raw_name` as a flat sibling of the main checkout, or re-enter it."""
    name = jj_name(raw_name)
    if not name:
        die("'%s' cannot be used as a workspace name" % raw_name)
    # Re-entering a workspace that exists is a resume, as in git. `default` is the main checkout,
    # never a worktree to hand back.
    existing = jj_root(cwd, name)
    if existing and name == "default":
        die("a workspace named 'default' already exists: it is the main checkout")
    if existing:
        return existing

    # Sibling names hang off the MAIN checkout, not off whichever workspace this ran in: from
    # `app-foo`, a new `bar` would otherwise be `app-foo-bar` instead of a flat `app-bar`. `default`
    # is the main checkout; a repo whose workspaces were renamed has none, so the current one anchors.
    path = "%s-%s" % (jj_root(cwd, "default") or root, name)
    if os.path.exists(path):
        die("path already exists: " + path)

    # `@-` is ambiguous when @ is an empty merge: it means both parents, and building on either
    # drops the other side of the join. The merge itself holds no work and carries both lines, so
    # sealing it (`jj new` leaves it behind as a real commit, same tree) makes `@-` nameable again.
    if jj_has(cwd, "@ & merges() & empty()"):
        must(jj(cwd, "new"), "jj new")
    # Exactly one commit, or `jj workspace add -r` would quietly make a merge its working copy.
    parents = (out(jj(cwd, "log", "--no-graph", "-r", "@-", "-T", 'change_id.shortest() ++ "\\n"')) or "").split()
    if len(parents) != 1:
        die("'@-' must resolve to exactly one commit, got: %s" % (" ".join(parents) or "nothing"))
    must(jj(cwd, "workspace", "add", "--name", name, "-r", parents[0], path), "jj workspace add")
    return path


def unreadable(path, proc):
    """Why jj can't read the workspace at `path`: in practice it is stale (its commits were rebased
    from another workspace), and the fix is in jj's own hint."""
    return "could not read the jj workspace at %s (stale? run `jj workspace update-stale` in it): %s" % (
        path, (proc.stderr.strip().splitlines() or [""])[0])


def remove_jj(path):
    """Forget and delete the jj workspace rooted at `path`, unless that would lose work."""
    # The workspace's name isn't its directory's (the sibling name is the repo's plus the name's), so
    # ask jj which workspace lives here.
    physical = os.path.realpath(path)
    listed = jj(path, "workspace", "list", "-T", 'name ++ "\\n"')
    if listed.returncode:
        die(unreadable(path, listed))
    name = next((n for n in listed.stdout.splitlines() if jj_root(path, n) == physical), None)
    if name is None:
        die("no jj workspace is rooted at " + path)
    if name == "default":
        die("refusing to remove the main checkout (%s)" % path)
    # Uncommitted changes are the one lossy case: `forget` leaves every commit in the repo, so
    # committed work survives the delete. jj only records a working copy when a command runs with
    # that workspace as its cwd, so ask from inside it — that snapshots first, and a stale working
    # copy (its commits were rebased underneath it) fails here instead of reading as clean.
    dirty = jj(path, "log", "--no-graph", "-r", "@ & ~empty()", "-T", "1")
    if dirty.returncode:
        die(unreadable(path, dirty))
    if dirty.stdout.strip():
        die("refusing to delete workspace '%s': it has uncommitted changes (%s). "
            "Commit or abandon them first." % (name, path))
    must(jj(path, "workspace", "forget", name), "jj workspace forget")
    shutil.rmtree(path)


# ---- git ------------------------------------------------------------------------------------


def git_name(raw):
    """The name also becomes a branch and ONE directory under .claude/worktrees, so a '/' is
    flattened to '+' as Claude does (a nested path would land inside another worktree), and leading
    punctuation goes, which also stops `..` climbing out. git itself judges what is left."""
    flat = re.sub(r"[^A-Za-z0-9._+-]", "-", raw.replace("/", "+"))
    return re.sub(r"^[^A-Za-z0-9]+", "", flat)


def git_common_dir(path):
    return out(git(path, "rev-parse", "--path-format=absolute", "--git-common-dir"))


def git_root(path):
    """The repo's main worktree root, also when standing in a linked worktree."""
    common = git_common_dir(path)
    if common is None:
        return None
    if common.endswith("/.git"):
        return common.removesuffix("/.git")
    return out(git(path, "rev-parse", "--show-toplevel"))


def registered_worktrees(root):
    listing = git(root, "worktree", "list", "--porcelain").stdout
    return {line.removeprefix("worktree ") for line in listing.splitlines() if line.startswith("worktree ")}


def has_non_dir(path):
    """Whether anything under `path` is not a directory. os.walk reports a symlink to a directory
    among the directories, so those are checked for separately: a link is something to keep."""
    if not os.path.isdir(path) or os.path.islink(path):
        return True
    for dirpath, dirnames, filenames in os.walk(path):
        if filenames or any(os.path.islink(os.path.join(dirpath, d)) for d in dirnames):
            return True
    return False


def free_path(root, path, registered):
    """Make `path` free to `git worktree add` into. Two things can occupy it without being a
    worktree."""
    # A worktree deleted by hand leaves its registration behind, and git then refuses both the path
    # and the branch until it is pruned. Prune only for that case: it also forgets every other
    # missing worktree (an unmounted drive's, say), which is not ours to touch otherwise. (`add -f`
    # would avoid the prune, but also overrides "branch is checked out elsewhere".)
    if not os.path.lexists(path):
        if path in registered:
            git(root, "worktree", "prune")
        return
    # Debris: a tool whose cwd was a removed worktree (a package install, say) recreated the
    # directory, and git refuses to add over it. A tree of empty directories is removed; anything
    # holding a file is moved into the repo's .git, where nothing scans or commits it and no file is
    # lost. The pid keeps two runs within the same second from landing in one directory.
    if not has_non_dir(path):
        shutil.rmtree(path)
        return
    aside = os.path.join(git_common_dir(root), "orphaned-worktrees",
                         "%s-%d-%d" % (os.path.basename(path), time.time(), os.getpid()))
    os.makedirs(os.path.dirname(aside), exist_ok=True)
    shutil.move(path, aside)
    warn("moved the leftover directory at %s to %s" % (path, aside))


def resolve_base(root):
    """Where a new git worktree starts: origin's default branch, freshly fetched, unless settings say
    `worktree.baseRef: head`. No origin, or one that can't be reached, means the local HEAD."""
    try:
        with open(os.path.expanduser("~/.claude/settings.json")) as f:
            if json.load(f).get("worktree", {}).get("baseRef") == "head":
                return "HEAD"
    except (OSError, ValueError, AttributeError):
        pass
    # `git remote add` (unlike clone) leaves origin/HEAD unset, hence the guesses after it.
    default = out(git(root, "symbolic-ref", "--short", "refs/remotes/origin/HEAD"))
    for ref in (default, "origin/main", "origin/master"):
        if ref and git(root, "show-ref", "--verify", "--quiet", "refs/remotes/" + ref).returncode == 0:
            fetched = git(root, "fetch", "--quiet", "origin", ref.removeprefix("origin/"))
            return ref if fetched.returncode == 0 else "HEAD"
    return "HEAD"


def create_git(cwd, raw_name):
    """Create the git worktree for `raw_name` under .claude/worktrees, or re-enter it."""
    root = git_root(cwd) or die("not a jj or git repository: " + cwd)
    name = git_name(raw_name)
    branch = BRANCH_PREFIX + name
    path = os.path.join(root, ".claude", "worktrees", name)
    registered = registered_worktrees(root)

    # `--worktree <name>` re-enters a worktree that is already there — whatever branch it is on —
    # rather than failing because the name is taken. `worktree list` prints physical paths.
    if name and os.path.isdir(path) and os.path.realpath(path) in registered:
        return path
    if not name or git(root, "check-ref-format", "refs/heads/" + branch).returncode:
        die("'%s' cannot be used as a worktree name" % raw_name)
    free_path(root, path, registered)

    # A surviving branch keeps its commits: check it out again, since `-b` refuses to reuse a name.
    reuse = git(root, "show-ref", "--verify", "--quiet", "refs/heads/" + branch).returncode == 0
    args = ["worktree", "add", path, branch] if reuse else ["worktree", "add", "-b", branch, path, resolve_base(root)]
    added = git(root, *args)
    if added.returncode and not reuse:
        # `add -b` makes the branch before anything else can fail (an unwritable parent, a full
        # disk), so a failure must not strand a branch with no worktree for the next attempt.
        git(root, "branch", "-D", branch)
    must(added, "git worktree add")
    return path


def remove_git(path):
    root = git_root(path)
    if root is None:
        return
    branch = out(git(path, "rev-parse", "--abbrev-ref", "HEAD")) or ""
    # No --force: a worktree with uncommitted files is refused, mirroring the jj side.
    must(git(root, "worktree", "remove", path), "git worktree remove")
    # Only a branch this hook (or Claude) named is deleted, and `-d` keeps one with unmerged commits.
    if branch.startswith(BRANCH_PREFIX):
        git(root, "branch", "-d", branch)


# ---- hook -----------------------------------------------------------------------------------


def hook(event):
    kind = event.get("hook_event_name")
    cwd = event.get("cwd") or ""
    if kind == "WorktreeCreate":
        raw_name = event.get("name") or ""
        root = jj_root(cwd)
        print(create_jj(cwd, root, raw_name) if root else create_git(cwd, raw_name))
    elif kind == "WorktreeRemove":
        path = event.get("worktree_path") or ""
        if not os.path.isdir(path):
            return
        if jj_root(path):
            remove_jj(path)
        else:
            remove_git(path)
    else:
        die("unsupported hook event: %r" % kind)


def main():
    try:
        hook(json.load(sys.stdin))
    except Fail as e:
        print("error: %s" % e, file=sys.stderr)
        return 1
    except Exception as e:  # a malformed payload or a filesystem error: one message format for Claude
        print("error: unexpected %s: %s" % (type(e).__name__, e), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
