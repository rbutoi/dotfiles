#!/usr/bin/env python3
"""Claude Code WorktreeCreate / WorktreeRemove hook: jj workspaces for its worktree features.

Makes `claude --worktree <name>`, EnterWorktree and worktree-isolated subagents create jj workspaces
in a jj repo, and git worktrees elsewhere. Reads the hook's JSON on stdin; WorktreeCreate prints the
new path on stdout. README.md has the registration entry, why a configured hook has to cover git too,
and why this is a settings hook rather than a plugin. Standard library only and 3.9-compatible: a
hook runs with whatever python3 is first on PATH, which can be the system one.

Two more commands, for jj workspaces that are finished but still on disk (README.md, "Finished
workspaces"): `prune [--dry-run] [--yes]` lists and removes them, and `nudge` is a SessionStart hook
that says in one line when there are any.
"""
import functools
import json
import os
import re
import shutil
import subprocess
import sys
import time

# Matches Claude's own worktree branches, so the hook's and the built-in's are interchangeable.
BRANCH_PREFIX = "worktree-"

# `prune` and the session-start nudge leave a workspace alone this long after its last change, so one
# just made (which has nothing unique either) is never offered for removal. Seconds; tests set it to 0.
IDLE_SECONDS = int(os.environ.get("WORKTREE_HOOK_IDLE_SECONDS", "900"))


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


def jj_log(cwd, revset, template):
    """`jj log` for a revset, stripped, or None if jj fails: in a stale workspace, say."""
    return out(jj(cwd, "log", "--no-graph", "-r", revset, "-T", template))


def jj_has(cwd, revset):
    return bool(jj_log(cwd, revset, "1"))


def jj_ids(cwd, revset):
    ids = jj_log(cwd, revset, 'commit_id ++ "\n"')
    return None if ids is None else ids.split()


def anchors(at):
    """What keeps a commit alive without workspace `at`: trunk, any bookmark, and every other
    workspace's working copy. Two questions are asked of it and differ on purpose. Removing a
    workspace asks only whether its `@` would be lost, since committed work stays reachable after
    `forget`. `prune` asks whether anything is left to come back for, which is stricter."""
    return "trunk() | bookmarks() | remote_bookmarks() | (working_copies() ~ %s)" % at


def unique_revset(at):
    """The non-empty commits of workspace `at` that exist nowhere else."""
    return "heads((::%s ~ ::(%s)) & ~empty())" % (at, anchors(at))


def as_workspace(cwd, name, path, query):
    """`query(dir, at)` asked from inside workspace `name`, which snapshots it so edits since count.
    A stale workspace can't be asked, but the repo still records its working copy, so ask from `cwd`
    about `name@`. That can miss edits made since, so it may suggest a removal but never justifies
    one: `prune` brings the workspace up to date and asks from inside again before it removes."""
    result = query(path, "@")
    return result if result is not None else query(cwd, name + "@")


@functools.lru_cache(maxsize=None)
def trunk_context(cwd):
    """(git dir, trunk commit, trunk's tree), or None when there is no trunk (it then resolves to the
    root commit) or no git store to ask."""
    trunk = jj_ids(cwd, "trunk() ~ root()")
    gitdir = out(jj(cwd, "git", "root"))
    if not trunk or not gitdir:
        return None
    tree = out(run(["git", "--git-dir", gitdir, "rev-parse", trunk[0] + "^{tree}"]))
    return (gitdir, trunk[0], tree) if tree else None


def in_trunk_by_content(cwd, ids):
    """Whether merging each commit into trunk changes nothing: its work is already there under other
    commit ids, as after a squash or rebase merge, where ancestry can't tell. A merge that conflicts
    counts as not contained, so doubt keeps the workspace."""
    context = trunk_context(cwd)
    return context is not None and all(
        out(run(["git", "--git-dir", context[0], "merge-tree", "--write-tree", context[1], i])) == context[2]
        for i in ids)


def finished(cwd, name, path):
    """Why workspace `name` can go without losing anything, or None if that can't be shown. Emptiness
    proves nothing: an empty `@` can sit on unmerged commits, and a merged workspace's `@` can be a
    real commit. What matters is whether any non-empty commit of it exists nowhere else."""
    unique = as_workspace(cwd, name, path, lambda d, at: jj_ids(d, unique_revset(at)))
    if unique is None:
        return None
    if not unique:
        return "already in the main line"
    if in_trunk_by_content(cwd, unique):
        return "already in trunk (same content)"
    return None


def workspaces(cwd):
    """(name, root, last change as recorded) for each workspace of the repo at `cwd`, or None if jj
    can't list them. One call, so a recent change rules a workspace out without asking it anything:
    the recorded time can only be older than the real one."""
    template = 'name ++ "\t" ++ root ++ "\t" ++ target.committer().timestamp().format("%s") ++ "\n"'
    listing = out(jj(cwd, "workspace", "list", "-T", template))
    if listing is None:
        return None
    return [(n, r, int(t)) for n, r, t in (line.split("\t") for line in listing.splitlines())]


def busy_dirs():
    """The working directory of every running process, or None if lsof can't say."""
    listing = run(["lsof", "-d", "cwd", "-Fn"]).stdout
    return [line[1:] for line in listing.splitlines() if line.startswith("n")] or None


def in_use(path, busy):
    """Whether a running process is inside `path`. Unknown use counts as in use: removing a directory
    under a live session is the one thing worse than leaving a finished workspace around."""
    real = os.path.realpath(path)
    return busy is None or any(d == real or d.startswith(real + os.sep) for d in busy)


def finished_workspaces(cwd):
    """(name, path, reason) for each secondary workspace of the repo at `cwd` that is finished, has
    not changed within IDLE_SECONDS and is not in use by any process."""
    found = []
    busy, looked = None, False
    for name, path, changed in workspaces(cwd) or []:
        if name == "default" or time.time() - changed < IDLE_SECONDS:
            continue
        reason = finished(cwd, name, path)
        if not reason:
            continue
        if not looked:  # a scan of every process, so only once something looks finished
            busy, looked = busy_dirs(), True
        if not in_use(path, busy):
            found.append((name, path, reason))
    return found


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


def discard(path):
    """Delete a directory without making anyone wait for it: rename it out of the way, which is
    instant, and let `rm -rf` finish in the background. A workspace with node_modules can take tens of
    seconds to delete, and nothing needs its files again."""
    parent, base = os.path.split(path.rstrip(os.sep))
    trash = os.path.join(parent, ".%s.removing-%d" % (base, os.getpid()))
    os.rename(path, trash)
    subprocess.Popen(["rm", "-rf", trash], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                     start_new_session=True)


def unreadable(path, proc):
    """Why jj can't read the workspace at `path`: in practice it is stale (its commits were rebased
    from another workspace), and the fix is in jj's own hint."""
    return "could not read the jj workspace at %s (stale? run `jj workspace update-stale` in it): %s" % (
        path, (proc.stderr.strip().splitlines() or [""])[0])


def remove_jj(path):
    """Forget and delete the jj workspace rooted at `path`, unless that would lose work."""
    # The workspace's name isn't its directory's (the sibling name is the repo's plus the name's), so
    # find it by root.
    rows = workspaces(path)
    if rows is None:
        die(unreadable(path, jj(path, "status")))
    physical = os.path.realpath(path)
    name = next((n for n, root, _ in rows if root == physical), None)
    if name is None:
        die("no jj workspace is rooted at " + path)
    if name == "default":
        die("refusing to remove the main checkout (%s)" % path)
    # Uncommitted changes are the one lossy case: `forget` leaves every commit in the repo, so
    # committed work survives the delete. jj only records a working copy when a command runs with
    # that workspace as its cwd, so ask from inside it, which snapshots first, and a stale one fails
    # here instead of reading as clean. `@` counts only if it exists nowhere else: a non-empty `@`
    # already in the main line, or whose work is already in trunk, loses nothing.
    dirty = jj_ids(path, "(@ & ~empty()) ~ ::(%s)" % anchors("@"))
    if dirty is None:
        die(unreadable(path, jj(path, "status")))
    if dirty and not in_trunk_by_content(path, dirty):
        die("refusing to delete workspace '%s': it has uncommitted changes (%s). "
            "Commit or abandon them first." % (name, path))
    must(jj(path, "workspace", "forget", name), "jj workspace forget")
    discard(path)


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
    # A worktree moved onto a branch this hook did not name (a feature branch, or a detached HEAD)
    # holds work someone comes back to, even when it is all committed and pushed, so it stays.
    if not branch.startswith(BRANCH_PREFIX):
        die("left in place: it is on %s, not a branch this hook created; remove it yourself with "
            "`git worktree remove %s` when it is finished" % (branch or "no branch", path))
    # No --force: a worktree with uncommitted files is refused, mirroring the jj side.
    must(git(root, "worktree", "remove", path), "git worktree remove")
    # `-d` keeps the branch if it has unmerged commits.
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


def update_stale(cwd, path):
    """Bring the workspace at `path` up to date, or fail if that saved edits jj never snapshotted. jj
    keeps those in a commit of their own, a stray head that nothing else points at, so removing the
    directory afterwards would strand them unannounced."""
    before = set(jj_ids(cwd, "heads(all())") or [])
    must(jj(path, "workspace", "update-stale"), "jj workspace update-stale")
    saved = [i for i in jj_ids(cwd, "heads(all()) & ~empty()") or [] if i not in before]
    if saved:
        die("its unsnapshotted edits were saved as %s (`jj log -r %s`); look them over, then run prune again"
            % (", ".join(i[:12] for i in saved), saved[0][:12]))


def still_finished(cwd, name, path):
    """Asked again just before removal: removing one workspace changes what the others hang on (two
    that hold the same unmerged commit each look finished while the other exists), and a session may
    have started since. Only this workspace is looked at, not the whole repo."""
    return bool(finished(cwd, name, path)) and not in_use(path, busy_dirs())


def prune(args):
    """List the finished jj workspaces of this repo and remove them on request."""
    unknown = [a for a in args if a not in ("--dry-run", "--yes")]
    if unknown:  # a mistyped safety flag must not become "remove everything"
        die("unknown option %s (prune takes --dry-run and --yes)" % unknown[0])
    dry, yes = "--dry-run" in args, "--yes" in args
    cwd = os.getcwd()
    if not jj_root(cwd):
        die("not inside a jj repo")
    found = finished_workspaces(cwd)
    if not found:
        print("no finished workspaces")
        return
    width = max(len(name) for name, _, _ in found)
    print("%d finished workspace%s:" % (len(found), "" if len(found) == 1 else "s"))
    for name, path, reason in found:
        print("  %-*s  %s  (%s)" % (width, name, path.replace(os.path.expanduser("~"), "~"), reason))
    if dry:
        return
    each = False
    if not yes:
        if not sys.stdin.isatty():
            print("not removing anything: there is no terminal to ask on (pass --yes to remove them all)")
            return
        answer = input("Remove %s? [y/N/s(elect)] " % ("it" if len(found) == 1 else "them all")).strip().lower()
        if answer not in ("y", "s"):
            return
        each = answer == "s"
    for name, path, _ in found:
        if each and input("  remove %s? [y/N] " % name).strip().lower() != "y":
            continue
        try:
            if not still_finished(cwd, name, path):
                die("it is no longer finished")
            update_stale(cwd, path)  # a no-op on a current workspace
            remove_jj(path)
            print("  removed " + name)
        except Fail as e:
            print("  kept %s: %s" % (name, e))


def nudge():
    """SessionStart: one line to the user, only when there is something to say, and never a failure."""
    try:
        cwd = json.load(sys.stdin).get("cwd") or os.getcwd()
        found = finished_workspaces(cwd)
        if found:
            script = os.path.abspath(__file__).replace(os.path.expanduser("~"), "~")
            print(json.dumps({"systemMessage": "jj: %d finished workspace%s (%s) can be removed: %s prune" % (
                len(found), "" if len(found) == 1 else "s", ", ".join(f[0] for f in found), script)}))
    except Exception:  # a nudge must never cost anyone a session
        pass


def main(args):
    try:
        if not args:  # a WorktreeCreate / WorktreeRemove hook
            hook(json.load(sys.stdin))
        elif args[0] == "prune":
            prune(args[1:])
        elif args[0] == "nudge":
            nudge()
        else:
            die("usage: worktree-hook.py [prune [--dry-run] [--yes] | nudge]; with no arguments it "
                "is the worktree hook and reads its JSON on stdin")
    except Fail as e:
        print("error: %s" % e, file=sys.stderr)
        return 1
    except Exception as e:  # a malformed payload or a filesystem error: one message format for Claude
        print("error: unexpected %s: %s" % (type(e).__name__, e), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
