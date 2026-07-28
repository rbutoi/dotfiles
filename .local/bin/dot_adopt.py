#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""
dot adopt — bring an existing ~/ file or directory under dotfiles management
============================================================================
Move a real file/directory that currently lives under $HOME into the dotfiles
repo (an environment dir under ~/.dots), then run `dot update` so the original
location becomes a symlink into the repo. This is the "I just made a new config
in ~/.config, now track it" workflow, done safely.

Given e.g. a fresh ~/.config/gitu/, this:
  1. validates the move is safe (many checks — see below),
  2. moves ~/.config/gitu  ->  ~/.dots/dotfiles/.config/gitu,
  3. runs `python3 ~/.dots/bin/dot update --skip-pull`,
  4. verifies ~/.config/gitu is now a symlink resolving into the repo,
  5. stages the new files in the repo (git add) unless --no-stage.

Usage:
    dot_adopt.py ~/.config/gitu                 # adopt one dir
    dot_adopt.py ~/.config/foo ~/.config/bar    # adopt several at once
    dot_adopt.py -n ~/.config/gitu              # dry run: show the plan only
    dot_adopt.py -y ~/.config/gitu              # don't prompt for confirmation
    dot_adopt.py --env private-dots ~/.config/x # adopt into the private env
    dot_adopt.py --no-stage ~/.config/gitu      # skip the `git add` afterwards

It refuses (touching nothing) when a move would be unsafe:
  - the path is already a symlink into the repo (already adopted — no-op),
  - the path already lives in the repo via a symlinked parent (just commit it),
  - it is a symlink pointing *outside* the repo (remove it yourself first),
  - the repo already has a file at that relative path (would clobber),
  - the same relative path exists in another environment (`dot` would error),
  - the name matches ~/.dots/.dotignore (`dot` would skip it, orphaning it).

Nothing is ever deleted: the original bytes are *moved* into the repo, so even
if `dot update` were to fail, your content is safe at the reported repo path
and re-running `dotup` will finish the job.
"""

from __future__ import annotations

import argparse
import errno
import os
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path

HOME = Path(os.path.abspath(os.path.expanduser("~")))

# environment dirs under base-dir that are not dotfile environments
NON_ENV_DIRS = {".git", "bin"}

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


def info(msg: str) -> None:
    print(msg)


def warn(msg: str) -> None:
    print(f"{YELLOW}warning:{RESET} {msg}", file=sys.stderr)


def err(msg: str) -> None:
    print(f"{RED}error:{RESET} {msg}", file=sys.stderr)


class AdoptError(Exception):
    """A validation failure for a single item — reported, nothing is moved."""


# ---- path helpers ----------------------------------------------------------


def norm(p: os.PathLike | str) -> Path:
    """Absolute, lexically-normalized path that does NOT resolve symlinks.

    We deliberately avoid Path.resolve()/os.path.realpath() here because we
    need to reason about symlinks in the path itself (is the leaf a link? is a
    parent a link into the repo?). abspath only collapses '.'/'..' and joins cwd.
    """
    return Path(os.path.abspath(os.path.expanduser(str(p))))


def real(p: os.PathLike | str) -> Path:
    """Fully symlink-resolved path (for equivalence checks)."""
    return Path(os.path.realpath(str(p)))


def within(child: Path, parent: Path) -> bool:
    """True if `child` is `parent` or nested under it (lexical, normalized)."""
    try:
        child.relative_to(parent)
        return True
    except ValueError:
        return False


# ---- .dotignore (mirror of dot/core.create_tree_from_filesystem) -----------


def load_dotignore_re(base_dir: Path) -> re.Pattern | None:
    """Compile ~/.dots/.dotignore into the same regex `dot` uses on filenames."""
    cfg = base_dir / ".dotignore"
    if not cfg.is_file():
        return None
    patterns = []
    for line in cfg.read_text().splitlines():
        name = line.rstrip()
        if name and not name.startswith("#"):
            patterns.append(name)
    if not patterns:
        return None
    return re.compile("(" + "|".join(patterns) + ")$", re.I)


def dir_has_linkable_file(path: Path, ignore_re: re.Pattern | None) -> bool:
    """True if walking `path` yields at least one file `dot` would link.

    `dot` skips the .git dir and any filename matching .dotignore. If a
    directory contains only ignored files, `dot` creates no symlink for it, so
    adopting it would move the content out of $HOME with nothing linked back.
    """
    for root, dirs, files in os.walk(path):
        dirs[:] = [d for d in dirs if d != ".git"]
        for f in files:
            if ignore_re is None or ignore_re.match(f) is None:
                return True
    return False


# ---- the plan --------------------------------------------------------------


@dataclass
class Item:
    target: Path  # absolute path under $HOME (leaf NOT symlink-resolved)
    relpath: Path  # target relative to $HOME
    source: Path  # base_dir/env/relpath — where it will live in the repo
    skip: bool = False  # already managed; no move needed
    note: str = ""  # human explanation, shown in the plan


@dataclass
class Config:
    base_dir: Path
    env: str
    env_dir: Path
    all_envs: list[str]
    ignore_re: re.Pattern | None = None
    dot_script: Path = field(init=False)

    def __post_init__(self) -> None:
        self.dot_script = self.base_dir / "bin" / "dot"


def classify(raw: str, cfg: Config) -> Item:
    """Validate a single argument and return an Item (possibly skip=True).

    Raises AdoptError if adopting `raw` would be unsafe or impossible.
    """
    target = norm(raw)

    if not os.path.lexists(target):
        raise AdoptError(f"{target}: does not exist")

    if target == HOME:
        raise AdoptError("refusing to adopt $HOME itself")

    if not within(target, HOME):
        raise AdoptError(f"{target}: is not under {HOME}")

    if within(target, cfg.base_dir):
        raise AdoptError(f"{target}: is already inside the dotfiles repo {cfg.base_dir}")

    relpath = target.relative_to(HOME)
    source = cfg.env_dir / relpath

    # (a) the leaf itself is a symlink
    if target.is_symlink():
        tgt = real(target)
        if within(tgt, cfg.base_dir):
            return Item(target, relpath, real_source_for(target, cfg),
                        skip=True, note="already a symlink into the repo — nothing to do")
        raise AdoptError(
            f"{target}: is a symlink to {tgt} (outside the repo); "
            "remove it yourself if you really mean to replace it")

    # (b) a parent is a symlink into the repo → already lives in the repo
    linked_parent = managed_via_parent(target, cfg.base_dir)
    if linked_parent is not None:
        return Item(target, relpath, real(target), skip=True,
                    note=f"already inside the repo via symlinked parent {linked_parent} "
                         f"(lives at {real(target)}); just commit it")

    # (c) repo already has something at this relative path
    if os.path.lexists(source):
        raise AdoptError(
            f"{relpath}: already exists in the repo at {source}; "
            "refusing to overwrite — reconcile them yourself")

    # (d) same relpath present in another environment → `dot` would error
    for other in cfg.all_envs:
        if other == cfg.env:
            continue
        rival = cfg.base_dir / other / relpath
        if os.path.lexists(rival):
            raise AdoptError(
                f"{relpath}: also exists in environment '{other}' ({rival}); "
                "`dot` cannot link a path that lives in two environments")

    # (e) .dotignore would make `dot` skip it → it'd be orphaned
    if target.is_dir():
        if not dir_has_linkable_file(target, cfg.ignore_re):
            raise AdoptError(
                f"{relpath}: every file under it matches ~/.dots/.dotignore, so "
                "`dot` would create no symlink — it would be orphaned")
    else:
        if cfg.ignore_re is not None and cfg.ignore_re.match(target.name) is not None:
            raise AdoptError(
                f"{relpath}: name matches ~/.dots/.dotignore, so `dot` would not "
                "link it — it would be orphaned. Rename it or edit .dotignore.")

    return Item(target, relpath, source)


def real_source_for(target: Path, cfg: Config) -> Path:
    """Best-effort repo path a symlinked target points at (for reporting)."""
    return real(target)


def managed_via_parent(target: Path, base_dir: Path) -> Path | None:
    """Return the nearest ancestor of `target` that is a symlink into the repo.

    If such an ancestor exists, `target` physically already lives inside the
    repo (it was written through the symlinked directory), so no move is needed.
    """
    for anc in target.parents:
        if anc == HOME or not within(anc, HOME):
            break
        if anc.is_symlink() and within(real(anc), base_dir):
            return anc
    return None


# ---- the move --------------------------------------------------------------


def move(src: Path, dst: Path) -> None:
    """Move src -> dst, creating parent dirs. Atomic rename when on one device."""
    dst.parent.mkdir(parents=True, exist_ok=True)
    try:
        os.rename(src, dst)
    except OSError as e:
        if e.errno == errno.EXDEV:  # cross-device: fall back to copy+remove
            shutil.move(str(src), str(dst))
        else:
            raise


def run_dot_update(cfg: Config) -> int:
    """Invoke the repo's own `dot update --skip-pull`, streaming its output."""
    cmd = ["python3", str(cfg.dot_script), "update", "--skip-pull"]
    info(f"{DIM}$ {' '.join(cmd)}{RESET}")
    return subprocess.run(cmd, cwd=str(cfg.base_dir)).returncode


def verify(item: Item) -> bool:
    """Confirm the original location now resolves, via a symlink, into the repo.

    `dot` may link at a higher directory than `item.source` (it links the
    highest single-environment dir), so we check symlink-resolution equivalence
    rather than the exact link location.
    """
    return os.path.lexists(item.target) and real(item.target) == real(item.source)


def git_add(cfg: Config, paths: list[Path]) -> None:
    if not (cfg.env_dir / ".git").exists():
        warn(f"{cfg.env_dir} is not a git repo; skipping `git add`")
        return
    rel = [str(p.relative_to(cfg.env_dir)) for p in paths]
    subprocess.run(["git", "-C", str(cfg.env_dir), "add", "--", *rel])
    info(f"{DIM}staged in {cfg.env}: {', '.join(rel)}{RESET}")


# ---- driver ----------------------------------------------------------------


def build_config(args: argparse.Namespace) -> Config:
    base_dir = norm(args.base_dir)
    if not base_dir.is_dir():
        raise AdoptError(f"base-dir {base_dir} does not exist")

    all_envs = sorted(
        p.name for p in base_dir.iterdir()
        if p.is_dir() and p.name not in NON_ENV_DIRS
    )
    if args.env not in all_envs:
        raise AdoptError(
            f"environment '{args.env}' not found under {base_dir}. "
            f"Available: {', '.join(all_envs) or '(none)'}")

    env_dir = base_dir / args.env
    dot_script = base_dir / "bin" / "dot"
    if not dot_script.is_file():
        raise AdoptError(f"cannot find the dot tool at {dot_script}")

    return Config(
        base_dir=base_dir,
        env=args.env,
        env_dir=env_dir,
        all_envs=all_envs,
        ignore_re=load_dotignore_re(base_dir),
    )


def confirm(prompt: str) -> bool:
    if not sys.stdin.isatty():
        return False
    try:
        return input(f"{prompt} [y/N] ").strip().lower() in ("y", "yes")
    except (EOFError, KeyboardInterrupt):
        print()
        return False


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Move a ~/ file or dir into the dotfiles repo and re-link it via `dot`.",
    )
    ap.add_argument("paths", nargs="+", help="file(s)/dir(s) under $HOME to adopt")
    ap.add_argument("--env", default="dotfiles",
                    help="target environment under base-dir (default: dotfiles)")
    ap.add_argument("--base-dir", default=str(HOME / ".dots"),
                    help="dotfiles base dir (default: ~/.dots)")
    ap.add_argument("-n", "--dry-run", action="store_true",
                    help="show the plan and exit without moving anything")
    ap.add_argument("-y", "--yes", action="store_true",
                    help="do not prompt for confirmation")
    ap.add_argument("--no-stage", action="store_true",
                    help="do not `git add` the adopted files afterwards")
    args = ap.parse_args()

    try:
        cfg = build_config(args)
    except AdoptError as e:
        err(str(e))
        return 2

    # ---- validate everything up front; move nothing until all pass ----
    items: list[Item] = []
    seen: dict[Path, str] = {}
    failed = False
    for raw in args.paths:
        try:
            item = classify(raw, cfg)
        except AdoptError as e:
            err(str(e))
            failed = True
            continue
        if item.relpath in seen:
            err(f"{item.relpath}: given more than once")
            failed = True
            continue
        seen[item.relpath] = raw
        items.append(item)

    if failed:
        err("aborting; nothing was moved")
        return 1

    # guard against adopting both a directory and something nested inside it
    for a in items:
        for b in items:
            if a is not b and within(b.target, a.target):
                err(f"{b.relpath} is nested inside {a.relpath}; adopt only the parent")
                return 1

    to_move = [i for i in items if not i.skip]
    skipped = [i for i in items if i.skip]

    for i in skipped:
        info(f"{YELLOW}skip{RESET} {i.target}  {DIM}({i.note}){RESET}")

    if not to_move:
        info("nothing to move.")
        return 0

    # ---- show the plan ----
    info(f"\n{BOLD}Plan{RESET} (env: {CYAN}{cfg.env}{RESET})")
    width = max(len(str(i.target)) for i in to_move)
    for i in to_move:
        kind = "dir " if i.target.is_dir() else "file"
        info(f"  {DIM}move {kind}{RESET} {str(i.target).ljust(width)}  ->  {i.source}")
    info(f"  {DIM}then{RESET} python3 {cfg.dot_script} update --skip-pull\n")

    if args.dry_run:
        info(f"{DIM}dry run — nothing changed.{RESET}")
        return 0

    if not args.yes and not confirm("Proceed?"):
        info("aborted.")
        return 1

    # ---- move (re-checking source is still absent, guarding TOCTOU) ----
    moved: list[Item] = []
    for i in to_move:
        if os.path.lexists(i.source):
            err(f"{i.source} appeared unexpectedly; stopping. "
                f"Moved so far: {', '.join(str(m.target) for m in moved) or 'none'}")
            break
        try:
            move(i.target, i.source)
            moved.append(i)
            info(f"{GREEN}moved{RESET} {i.target}  ->  {i.source}")
        except OSError as e:
            err(f"failed to move {i.target}: {e}")
            break

    if not moved:
        return 1

    # ---- re-link via dot ----
    rc = run_dot_update(cfg)
    if rc != 0:
        err(f"`dot update` exited {rc}. Your content is safe in the repo at:")
        for i in moved:
            err(f"    {i.source}")
        err("Re-run `dotup` to finish creating the symlinks.")
        return 1

    # ---- verify ----
    ok = True
    for i in moved:
        if verify(i):
            info(f"{GREEN}linked{RESET} {i.target} {DIM}->{RESET} {real(i.target)}")
        else:
            ok = False
            err(f"{i.target} is not a symlink into the repo after `dot update`. "
                f"Content is at {i.source}; investigate before committing.")

    if not ok:
        return 1

    # ---- stage ----
    if not args.no_stage:
        git_add(cfg, [i.source for i in moved])

    info(f"\n{GREEN}done{RESET} — {len(moved)} adopted into {cfg.env}. "
         f"Review with `git -C {cfg.env_dir} status`, then commit when ready.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
