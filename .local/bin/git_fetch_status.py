#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""
Git / jj Fetch Status
=====================
Find every git or jj (Jujutsu) repo under a root directory (default ~/dev),
fetch each one, and print a succinct report of how out of date each repo is
relative to its upstream.

By default only the immediate children of the root are inspected (depth 1).
Use --depth to descend further when your repos live in nested folders.

Usage:
    git_fetch_status.py                    # scan ~/dev, depth 1
    git_fetch_status.py ~/code             # scan a different root
    git_fetch_status.py --depth 3          # search up to 3 levels deep
    git_fetch_status.py --depth 0          # unlimited depth
    git_fetch_status.py --no-fetch         # report only, skip the network
    git_fetch_status.py --jobs 16          # parallel fetch workers

"Out of date" is the number of commits behind/ahead plus how long the repo has
been stale (the age of the oldest commit you're missing). For git this is
measured against the current branch's upstream; for jj it's the working copy
(@) against trunk(). Colocated git+jj repos are reported as jj.
"""

import argparse
import concurrent.futures
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

JJ = shutil.which("jj")

# ---- terminal colors (only when writing to a tty) --------------------------

_TTY = sys.stdout.isatty()


def _c(code: str) -> str:
    return code if _TTY else ""


RESET = _c("\033[0m")
BOLD = _c("\033[1m")
DIM = _c("\033[2m")
RED = _c("\033[31m")
GREEN = _c("\033[32m")
YELLOW = _c("\033[33m")
CYAN = _c("\033[36m")


def git(repo: Path, *args: str, timeout: int = 120) -> subprocess.CompletedProcess:
    """Run a git command inside `repo`, capturing output."""
    return subprocess.run(
        ["git", "-C", str(repo), *args],
        capture_output=True,
        text=True,
        timeout=timeout,
    )


def jj(repo: Path, *args: str, timeout: int = 120) -> subprocess.CompletedProcess:
    """Run a jj command against `repo`, capturing output. Never snapshots."""
    return subprocess.run(
        ["jj", "-R", str(repo), "--no-pager", "--color", "never",
         "--ignore-working-copy", *args],
        capture_output=True,
        text=True,
        timeout=timeout,
    )


def find_repos(root: Path, max_depth: int) -> list[Path]:
    """
    Find git and jj repos under `root`.

    A directory counts as a repo if it contains .git or .jj. max_depth is
    relative to root: depth 1 = direct children of root. max_depth == 0 means
    unlimited. Repos are not descended into.
    """
    repos: list[Path] = []

    def walk(directory: Path, depth: int) -> None:
        try:
            entries = sorted(
                (e for e in os.scandir(directory) if e.is_dir(follow_symlinks=False)),
                key=lambda e: e.name,
            )
        except (PermissionError, FileNotFoundError):
            return
        for entry in entries:
            path = Path(entry.path)
            if (path / ".git").exists() or (path / ".jj").is_dir():
                repos.append(path)
                continue  # don't descend into a repo
            if max_depth == 0 or depth < max_depth:
                walk(path, depth + 1)

    walk(root, 1)
    return repos


@dataclass
class Status:
    repo: Path
    vcs: str = "git"  # "git" or "jj"
    branch: str = ""
    ahead: int = 0
    behind: int = 0
    stale_for: str = ""  # age of the oldest missing commit
    base: str = ""  # ref we compared against, shown when it's a fallback
    base_fallback: bool = False  # True when base is the default branch, not the upstream
    note: str = ""  # "up to date", "no upstream", "fetch failed", ...
    ok: bool = True


def inspect(repo: Path, do_fetch: bool) -> Status:
    """Dispatch to the jj or git inspector. Colocated repos are treated as jj."""
    if (repo / ".jj").is_dir():
        return inspect_jj(repo, do_fetch)
    return inspect_git(repo, do_fetch)


def default_remote_ref(repo: Path) -> str | None:
    """
    The remote's default branch, e.g. "origin/main" — used as a comparison base
    when the current branch has no upstream of its own.
    """
    remote_out = git(repo, "remote")
    remotes = remote_out.stdout.split() if remote_out.returncode == 0 else []
    if not remotes:
        return None
    remote = "origin" if "origin" in remotes else remotes[0]

    # origin/HEAD, if the remote's default branch is recorded locally
    head = git(repo, "symbolic-ref", "--quiet", "--short", f"refs/remotes/{remote}/HEAD")
    if head.returncode == 0 and head.stdout.strip():
        return head.stdout.strip()

    # otherwise guess the conventional names
    for name in ("main", "master"):
        if git(repo, "rev-parse", "--verify", "--quiet", f"{remote}/{name}").returncode == 0:
            return f"{remote}/{name}"
    return None


def inspect_git(repo: Path, do_fetch: bool) -> Status:
    st = Status(repo=repo, vcs="git")

    # current branch (or detached HEAD)
    r = git(repo, "symbolic-ref", "--quiet", "--short", "HEAD")
    if r.returncode == 0:
        st.branch = r.stdout.strip()
    else:
        head = git(repo, "rev-parse", "--short", "HEAD")
        st.branch = f"detached@{head.stdout.strip()}" if head.returncode == 0 else "?"

    remotes = git(repo, "remote")
    if remotes.returncode == 0 and not remotes.stdout.strip():
        st.note = "no remote"
        return st

    if do_fetch:
        r = git(repo, "fetch", "--quiet", "--prune")
        if r.returncode != 0:
            st.note = "fetch failed"
            st.ok = False
            # keep going: we can still report cached upstream state below

    # compare against the branch's own upstream, or fall back to the remote's
    # default branch (origin/main) when the branch has no upstream of its own
    up = git(repo, "rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}")
    if up.returncode == 0:
        base = up.stdout.strip()
    else:
        base = default_remote_ref(repo)
        if base is None:
            if not st.note:
                st.note = "no upstream"
            return st
        st.base = base
        st.base_fallback = True

    # ahead/behind counts
    counts = git(repo, "rev-list", "--left-right", "--count", f"HEAD...{base}")
    if counts.returncode == 0:
        parts = counts.stdout.split()
        if len(parts) == 2:
            st.ahead, st.behind = int(parts[0]), int(parts[1])

    if st.behind:
        # age of the OLDEST commit we're missing == how long we've been stale
        oldest = git(
            repo,
            "log",
            "--reverse",
            "--format=%cr",
            f"HEAD..{base}",
        )
        if oldest.returncode == 0 and oldest.stdout:
            # %cr yields e.g. "10 days ago"; drop the "ago" so it reads "stale 10 days"
            st.stale_for = oldest.stdout.splitlines()[0].strip().removesuffix(" ago")

    if not st.note and st.behind == 0 and st.ahead == 0:
        st.note = "up to date"
    return st


def _jj_count(repo: Path, revset: str) -> int | None:
    """Count commits in a jj revset, or None if the revset can't be evaluated."""
    r = jj(repo, "log", "--no-graph", "-r", revset, "-T", '"x\\n"')
    if r.returncode != 0:
        return None
    return sum(1 for line in r.stdout.splitlines() if line.strip())


def inspect_jj(repo: Path, do_fetch: bool) -> Status:
    st = Status(repo=repo, vcs="jj")
    if JJ is None:
        st.note = "jj not installed"
        st.ok = False
        return st

    # label: local bookmark(s) at the working copy, else the change id
    bm = jj(repo, "log", "--no-graph", "-r", "@", "-T", "bookmarks")
    label = bm.stdout.strip() if bm.returncode == 0 else ""
    if not label:
        cid = jj(repo, "log", "--no-graph", "-r", "@", "-T", "change_id.short(8)")
        label = "@" + cid.stdout.strip() if cid.returncode == 0 else "@?"
    st.branch = label

    remotes = jj(repo, "git", "remote", "list")
    if remotes.returncode == 0 and not remotes.stdout.strip():
        st.note = "no remote"
        return st

    if do_fetch:
        r = jj(repo, "git", "fetch")
        if r.returncode != 0:
            st.note = "fetch failed"
            st.ok = False
            # keep going: still report cached trunk state below

    # how far the working copy trails trunk() (the remote main/master head)
    behind = _jj_count(repo, "::trunk() ~ ::@")
    if behind is None:
        if not st.note:
            st.note = "no trunk"
        return st
    st.behind = behind
    # local commits not yet on trunk, ignoring the (often empty) working copy
    st.ahead = _jj_count(repo, "(::@ ~ ::trunk()) ~ empty()") or 0

    if st.behind:
        oldest = jj(
            repo, "log", "--no-graph", "--reversed",
            "-r", "::trunk() ~ ::@",
            "-T", 'committer.timestamp().ago() ++ "\\n"',
        )
        if oldest.returncode == 0 and oldest.stdout:
            st.stale_for = oldest.stdout.splitlines()[0].strip().removesuffix(" ago")

    if not st.note and st.behind == 0 and st.ahead == 0:
        st.note = "up to date"
    return st


def format_status(st: Status) -> str:
    if st.note in ("fetch failed", "jj not installed"):
        return f"{RED}{st.note}{RESET}"
    if st.note in ("no upstream", "no trunk", "no remote"):
        return f"{DIM}{st.note}{RESET}"
    if st.behind == 0 and st.ahead == 0:
        return f"{GREEN}up to date{RESET}"

    bits = []
    if st.behind:
        bits.append(f"{RED}↓{st.behind}{RESET}")
    if st.ahead:
        bits.append(f"{YELLOW}↑{st.ahead}{RESET}")
    line = " ".join(bits)
    if st.stale_for:
        line += f"  {DIM}stale {st.stale_for}{RESET}"
    if st.base_fallback and st.base:
        line += f"  {DIM}vs {st.base}{RESET}"
    return line


def main() -> int:
    ap = argparse.ArgumentParser(
        description="git fetch every repo under a root and report how out of date each is.",
    )
    ap.add_argument(
        "root",
        nargs="?",
        default=str(Path.home() / "dev"),
        help="root directory to scan (default: ~/dev)",
    )
    ap.add_argument(
        "-d",
        "--depth",
        type=int,
        default=1,
        help="max search depth relative to root; 1 = direct children (default), 0 = unlimited",
    )
    ap.add_argument(
        "--no-fetch",
        action="store_true",
        help="skip `git fetch`; report against already-fetched state",
    )
    ap.add_argument(
        "-j",
        "--jobs",
        type=int,
        default=8,
        help="number of parallel fetch workers (default: 8)",
    )
    args = ap.parse_args()

    root = Path(args.root).expanduser()
    if not root.is_dir():
        print(f"{RED}error:{RESET} {root} is not a directory", file=sys.stderr)
        return 2

    repos = find_repos(root, args.depth)
    if not repos:
        print(f"no git repos found under {root} (depth {args.depth or '∞'})")
        return 0

    results: list[Status] = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=max(1, args.jobs)) as pool:
        futures = {
            pool.submit(inspect, repo, not args.no_fetch): repo for repo in repos
        }
        for fut in concurrent.futures.as_completed(futures):
            try:
                results.append(fut.result())
            except Exception as exc:  # noqa: BLE001 - surface, don't crash the batch
                st = Status(repo=futures[fut], note="error", ok=False)
                st.branch = str(exc).splitlines()[0][:40]
                results.append(st)

    results.sort(key=lambda s: str(s.repo).lower())  # alphabetical by path

    behind = sum(1 for s in results if s.behind)
    failed = sum(1 for s in results if not s.ok)
    summary = f"{BOLD}{len(results)} repos{RESET}{DIM} — {behind} behind{RESET}"
    if failed:
        summary += f"{DIM},{RESET} {RED}{failed} failed{RESET}"
    print(summary)

    names = [str(s.repo.relative_to(root)) for s in results]
    branches = [s.branch for s in results]
    name_w = max((len(n) for n in names), default=4)
    branch_w = max((len(b) for b in branches), default=6)

    for st, name, branch in zip(results, names, branches):
        status = format_status(st)
        name_col = f"{CYAN}{name}{RESET}".ljust(name_w + len(CYAN) + len(RESET))
        vcs_col = f"{DIM}{st.vcs:<3}{RESET}"
        branch_col = f"{DIM}{branch}{RESET}"
        pad = " " * (branch_w - len(branch))
        print(f"  {name_col}  {vcs_col}  {branch_col}{pad}  {status}")

    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
