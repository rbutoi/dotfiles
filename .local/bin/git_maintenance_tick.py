#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""
Git Maintenance Tick
====================
Keep a configurable set of git repos fetched and packed in the background, using
git's own `git maintenance` engine, on per-repo intervals that relax outside
business hours.

A scheduler wakes this every ~15 minutes (launchd on macOS, a systemd --user
timer on Linux) and *this* script decides what is actually due, so all the
interval logic lives in one testable place and both platforms behave alike.

Three tiers run per repo, each with its own last-run stamp:

    fetch    per-repo interval, business hours vs off hours
             `git fetch --all --prune`, which advances refs/remotes/* so that
             `git status`, shell prompts and ahead/behind counts tell the truth
             with no network wait, then `maintenance run --task=commit-graph`
    daily    `maintenance run --task=loose-objects --task=incremental-repack`
    weekly   `maintenance run --task=pack-refs`

Note this deliberately does *not* use `git maintenance run --schedule=...`, the
`prefetch` task, or `git maintenance register`/`start`:

  - `start` installs three fixed buckets (hourly/daily/weekly) shared by every
    repo, which cannot express per-repo or business-hours intervals.
  - `register` writes machine-specific absolute paths into the global config and
    sets maintenance.strategy=incremental, which also swaps `gc --auto` for
    `incremental-repack --auto` in that repo.
  - `prefetch` fetches into refs/prefetch/* and leaves refs/remotes/* alone, so
    `git status` stays stale until you fetch for real. We fetch for real
    instead, which makes prefetch redundant network traffic.

`run --task=<task>` bypasses maintenance.strategy entirely, so none of this
needs any git config, global or per-repo.

Config, later file winning (the .local one is machine-specific, usually untracked):

    ${XDG_CONFIG_HOME:-~/.config}/git-maintenance/config.toml
    ${XDG_CONFIG_HOME:-~/.config}/git-maintenance/config.local.toml

Usage:
    git_maintenance_tick.py                 # a tick: run what's due; silent if nothing is
    git_maintenance_tick.py --status        # table: interval in effect, last run, next due
    git_maintenance_tick.py --dry-run       # report what would run, run nothing
    git_maintenance_tick.py --force         # ignore the schedule, run every tier now
    git_maintenance_tick.py --repo ~/dev/x  # limit to one repo
    git_maintenance_tick.py --at 2026-08-04T22:00   # evaluate against a simulated clock
    git_maintenance_tick.py -j 4            # parallel workers (default 6)

State (last-run stamps, failure counts, last error) lives in
${XDG_STATE_HOME:-~/.local/state}/git-maintenance/state.json.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import fcntl
import glob
import json
import os
import re
import subprocess
import sys
import tomllib
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path

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

# ---- constants -------------------------------------------------------------

TIERS = ("fetch", "daily", "weekly")

# Kept in sync with the schedulers that call us:
#   Library/LaunchAgents/dev.radu.git-maintenance.plist  StartInterval 900
#   .config/systemd/user/git-maintenance.timer           OnCalendar=*:0/15
# Only used to compute the due-ness grace below; override with [defaults] tick.
DEFAULT_TICK = "15m"

# A tier is due when it is within half a tick of its interval, so that e.g. a 1h
# interval sampled every 15m lands near 1h instead of drifting out to 1h15.
GRACE_FRACTION = 0.5

# Consecutive-failure backoff is capped at this multiple of the interval, so a
# laptop that has been offline for a week retries every 8h rather than hourly.
MAX_BACKOFF = 8

FETCH_TIMEOUT = 300
MAINTENANCE_TIMEOUT = 900

DAY_NAMES = {"mon": 0, "tue": 1, "wed": 2, "thu": 3, "fri": 4, "sat": 5, "sun": 6}

GLOB_CHARS = "*?["


def xdg(var: str, default: str) -> Path:
    value = os.environ.get(var)
    return Path(value) if value else Path.home() / default


CONFIG_DIR = xdg("XDG_CONFIG_HOME", ".config") / "git-maintenance"
STATE_DIR = xdg("XDG_STATE_HOME", ".local/state") / "git-maintenance"

# ---- small helpers ---------------------------------------------------------


def git(
    repo: Path, *args: str, timeout: int = 120
) -> subprocess.CompletedProcess:
    """Run a git command inside `repo`, capturing output."""
    return subprocess.run(
        ["git", "-C", str(repo), *args],
        capture_output=True,
        text=True,
        timeout=timeout,
    )


def short(path: Path) -> str:
    """Render a path with $HOME collapsed to ~, for display."""
    try:
        return "~/" + str(path.relative_to(Path.home()))
    except ValueError:
        return str(path)


_INTERVAL_RE = re.compile(r"^\s*(\d+(?:\.\d+)?)\s*([smhdw])\s*$", re.I)
_UNIT_SECONDS = {"s": 1, "m": 60, "h": 3600, "d": 86400, "w": 604800}


def parse_interval(value: str | int | float, what: str) -> int:
    """Parse "30m" / "6h" / "7d" into seconds. Bare numbers are minutes."""
    if isinstance(value, (int, float)):
        return int(value * 60)
    m = _INTERVAL_RE.match(str(value))
    if not m:
        raise ValueError(
            f"{what}: cannot parse interval {value!r} "
            f"(expected e.g. '30m', '6h', '7d')"
        )
    return int(float(m.group(1)) * _UNIT_SECONDS[m.group(2).lower()])


def fmt_duration(seconds: float) -> str:
    """Compact human duration: 45s, 30m, 1.5h, 7d."""
    seconds = abs(seconds)
    if seconds < 90:
        return f"{seconds:.0f}s"
    if seconds < 3600:
        return f"{seconds / 60:.0f}m"
    if seconds < 48 * 3600:
        hours = seconds / 3600
        return f"{hours:.0f}h" if abs(hours - round(hours)) < 0.05 else f"{hours:.1f}h"
    return f"{seconds / 86400:.0f}d"


# ---- config ----------------------------------------------------------------


@dataclass
class BusinessHours:
    days: set[int] = field(default_factory=lambda: {0, 1, 2, 3, 4})
    start: int = 9 * 60  # minutes since midnight
    end: int = 18 * 60

    def covers(self, when: datetime) -> bool:
        minutes = when.hour * 60 + when.minute
        if self.start <= self.end:
            in_window = self.start <= minutes < self.end
        else:  # window wraps past midnight
            in_window = minutes >= self.start or minutes < self.end
        return when.weekday() in self.days and in_window

    def describe(self) -> str:
        names = [n for n, i in sorted(DAY_NAMES.items(), key=lambda kv: kv[1]) if i in self.days]
        return f"{','.join(names)} {self.start // 60:02d}:{self.start % 60:02d}-{self.end // 60:02d}:{self.end % 60:02d}"


@dataclass
class Rule:
    """One [[repo]] entry: a literal path or a glob, plus its intervals."""

    pattern: str
    business: int
    off: int
    daily: int
    weekly: int
    order: int  # position across the merged config files; later wins ties
    source: str

    @property
    def is_glob(self) -> bool:
        return any(ch in self.pattern for ch in GLOB_CHARS)

    def fetch_interval(self, business: bool) -> int:
        return self.business if business else self.off

    def interval(self, tier: str, business: bool) -> int:
        return self.fetch_interval(business) if tier == "fetch" else getattr(self, tier)

    @property
    def specificity(self) -> tuple[int, int, int]:
        """Literal paths beat globs; longer patterns beat shorter; later beats earlier."""
        return (0 if self.is_glob else 1, len(self.pattern), self.order)


@dataclass
class Config:
    rules: list[Rule]
    business_hours: BusinessHours
    tick: int
    sources: list[Path]


def parse_days(value) -> set[int]:
    """Accept ["mon","tue"] or "mon-fri" or "mon,wed,fri"."""
    if isinstance(value, str):
        names: list[str] = []
        for chunk in value.split(","):
            chunk = chunk.strip().lower()
            if "-" in chunk:
                first, last = (c.strip() for c in chunk.split("-", 1))
                if first not in DAY_NAMES or last not in DAY_NAMES:
                    raise ValueError(f"business_hours.days: unknown day range {chunk!r}")
                lo, hi = DAY_NAMES[first], DAY_NAMES[last]
                span = range(lo, hi + 1) if lo <= hi else [*range(lo, 7), *range(0, hi + 1)]
                names += [d for d, i in DAY_NAMES.items() if i in span]
            else:
                names.append(chunk)
        value = names
    days = set()
    for name in value:
        key = str(name).strip().lower()[:3]
        if key not in DAY_NAMES:
            raise ValueError(f"business_hours.days: unknown day {name!r}")
        days.add(DAY_NAMES[key])
    return days


def parse_hhmm(value: str, what: str) -> int:
    m = re.match(r"^\s*(\d{1,2}):(\d{2})\s*$", str(value))
    if not m:
        raise ValueError(f"{what}: expected HH:MM, got {value!r}")
    hour, minute = int(m.group(1)), int(m.group(2))
    if not (0 <= hour <= 24 and 0 <= minute < 60):
        raise ValueError(f"{what}: {value!r} is not a valid time")
    return hour * 60 + minute


def load_config(paths: list[Path]) -> Config:
    """
    Merge config files in order. [defaults] and [business_hours] keys are
    overridden by later files; [[repo]] entries accumulate (a later entry wins
    only if it is at least as specific — see Rule.specificity).
    """
    defaults = {"business": "1h", "off": "6h", "daily": "24h", "weekly": "7d"}
    tick = DEFAULT_TICK
    hours_raw: dict = {}
    entries: list[tuple[dict, str]] = []
    found: list[Path] = []

    for path in paths:
        if not path.is_file():
            continue
        found.append(path)
        try:
            data = tomllib.loads(path.read_text())
        except tomllib.TOMLDecodeError as exc:
            raise ValueError(f"{path}: {exc}") from exc
        section = data.get("defaults", {})
        tick = section.pop("tick", tick)
        defaults.update(section)
        hours_raw.update(data.get("business_hours", {}))
        for entry in data.get("repo", []):
            entries.append((entry, str(path)))

    if not found:
        raise FileNotFoundError(
            "no config found; expected one of:\n  " + "\n  ".join(str(p) for p in paths)
        )

    hours = BusinessHours()
    if "days" in hours_raw:
        hours.days = parse_days(hours_raw["days"])
    if "start" in hours_raw:
        hours.start = parse_hhmm(hours_raw["start"], "business_hours.start")
    if "end" in hours_raw:
        hours.end = parse_hhmm(hours_raw["end"], "business_hours.end")

    rules: list[Rule] = []
    for order, (entry, source) in enumerate(entries):
        if "path" not in entry:
            raise ValueError(f"{source}: a [[repo]] entry is missing `path`")
        pattern = str(entry["path"])
        rules.append(
            Rule(
                pattern=pattern,
                business=parse_interval(
                    entry.get("business", defaults["business"]), f"{pattern}.business"
                ),
                off=parse_interval(entry.get("off", defaults["off"]), f"{pattern}.off"),
                daily=parse_interval(
                    entry.get("daily", defaults["daily"]), f"{pattern}.daily"
                ),
                weekly=parse_interval(
                    entry.get("weekly", defaults["weekly"]), f"{pattern}.weekly"
                ),
                order=order,
                source=source,
            )
        )

    return Config(
        rules=rules,
        business_hours=hours,
        tick=parse_interval(tick, "defaults.tick"),
        sources=found,
    )


# ---- repo discovery --------------------------------------------------------


def is_git_repo(path: Path) -> bool:
    """True for a normal worktree (.git dir or gitfile) or a bare repo."""
    if (path / ".git").exists():
        return True
    return (path / "HEAD").is_file() and (path / "objects").is_dir()


def resolve_repos(config: Config) -> tuple[dict[Path, Rule], list[Path]]:
    """
    Expand every rule's pattern and pick the most specific rule per repo.
    Returns (repo -> rule, jj-only dirs that were skipped).
    """
    best: dict[Path, Rule] = {}
    skipped_jj: list[Path] = []

    for rule in config.rules:
        pattern = os.path.expanduser(rule.pattern)
        matches = (
            sorted(glob.glob(pattern)) if rule.is_glob else [pattern]
        )
        for match in matches:
            path = Path(match)
            if not path.is_dir():
                continue
            path = path.resolve()
            if not is_git_repo(path):
                # A jj repo without a colocated .git can't be fetched with git.
                if (path / ".jj").is_dir() and path not in skipped_jj:
                    skipped_jj.append(path)
                continue
            if path not in best or rule.specificity > best[path].specificity:
                best[path] = rule

    return best, skipped_jj


# ---- state -----------------------------------------------------------------


@dataclass
class TierState:
    last: float | None = None  # last success
    attempt: float | None = None  # last attempt, success or not
    fail: int = 0
    error: str = ""

    @classmethod
    def from_json(cls, data: dict) -> TierState:
        return cls(
            last=data.get("last"),
            attempt=data.get("attempt"),
            fail=int(data.get("fail", 0)),
            error=data.get("error", ""),
        )

    def to_json(self) -> dict:
        out: dict = {}
        if self.last is not None:
            out["last"] = round(self.last, 3)
        if self.attempt is not None:
            out["attempt"] = round(self.attempt, 3)
        if self.fail:
            out["fail"] = self.fail
        if self.error:
            out["error"] = self.error
        return out


class State:
    """Per-repo, per-tier stamps, persisted as JSON under XDG_STATE_HOME."""

    def __init__(self, path: Path):
        self.path = path
        self.repos: dict[str, dict[str, TierState]] = {}

    def load(self) -> State:
        if self.path.is_file():
            try:
                raw = json.loads(self.path.read_text())
            except (json.JSONDecodeError, OSError):
                raw = {}
            for repo, tiers in (raw.get("repos") or {}).items():
                self.repos[repo] = {
                    tier: TierState.from_json(tiers.get(tier, {})) for tier in TIERS
                }
        return self

    def get(self, repo: Path, tier: str) -> TierState:
        return self.repos.setdefault(
            str(repo), {t: TierState() for t in TIERS}
        ).setdefault(tier, TierState())

    def save(self) -> None:
        payload = {
            "version": 1,
            "repos": {
                repo: {
                    tier: state.to_json()
                    for tier, state in tiers.items()
                    if state.to_json()
                }
                for repo, tiers in sorted(self.repos.items())
            },
        }
        self.path.parent.mkdir(parents=True, exist_ok=True)
        tmp = self.path.with_suffix(".json.tmp")
        tmp.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")
        os.replace(tmp, self.path)  # atomic: never a half-written state file


# ---- scheduling ------------------------------------------------------------


def effective_interval(base: int, fail: int) -> int:
    """Exponential backoff on consecutive failures, capped at MAX_BACKOFF."""
    return base * min(2**fail, MAX_BACKOFF) if fail else base


def next_due_at(state: TierState, base: int) -> float | None:
    """Epoch time this tier next becomes due, or None if it never ran."""
    interval = effective_interval(base, state.fail)
    reference = state.attempt if state.fail else state.last
    return None if reference is None else reference + interval


def is_due(state: TierState, base: int, now: float, grace: int) -> bool:
    due_at = next_due_at(state, base)
    return True if due_at is None else now >= due_at - grace


# ---- doing the work --------------------------------------------------------


@dataclass
class Result:
    repo: Path
    ran: list[str] = field(default_factory=list)
    failed: list[str] = field(default_factory=list)
    updates: list[str] = field(default_factory=list)  # ref-update lines from fetch
    warnings: list[str] = field(default_factory=list)
    errors: dict[str, str] = field(default_factory=dict)  # tier -> message

    @property
    def did_anything(self) -> bool:
        return bool(self.ran or self.failed)


def first_error_line(proc: subprocess.CompletedProcess) -> str:
    for line in (proc.stderr or proc.stdout or "").splitlines():
        line = line.strip()
        if line:
            return line[:200]
    return f"exit {proc.returncode}"


def run_fetch(repo: Path, result: Result) -> bool:
    """
    Real fetch, so refs/remotes/* actually advances.

    maintenance.auto=false suppresses the implicit post-fetch `gc --auto`: the
    daily and weekly tiers own housekeeping here. --no-progress keeps stderr to
    just the ref-update table, which is worth logging.
    """
    try:
        proc = git(
            repo,
            "-c",
            "maintenance.auto=false",
            "fetch",
            "--all",
            "--prune",
            "--no-progress",
            "--no-recurse-submodules",
            timeout=FETCH_TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        result.errors["fetch"] = f"fetch timed out after {FETCH_TIMEOUT}s"
        return False

    if proc.returncode != 0:
        result.errors["fetch"] = first_error_line(proc)
        return False

    for line in proc.stderr.splitlines():
        line = line.strip()
        # keep the "abc1234..def5678  main -> origin/main" style lines, dropping
        # the "From <url>" / "Fetching <remote>" headers around them
        if line and not line.startswith(("From ", "Fetching ")):
            result.updates.append(line)
    return True


def run_tasks(repo: Path, tier: str, tasks: list[str], result: Result) -> bool:
    """Run `git maintenance run --task=...`. Housekeeping failures only warn."""
    args = ["maintenance", "run", *(f"--task={t}" for t in tasks)]
    try:
        proc = git(repo, *args, timeout=MAINTENANCE_TIMEOUT)
    except subprocess.TimeoutExpired:
        result.warnings.append(f"{tier}: timed out after {MAINTENANCE_TIMEOUT}s")
        return False
    if proc.returncode != 0:
        # e.g. incremental-repack on a repo with no pack files yet
        result.warnings.append(f"{tier}: {first_error_line(proc)}")
        return False
    return True


def process(repo: Path, due: list[str], now: float) -> Result:
    """Run every due tier for one repo. Never raises."""
    result = Result(repo=repo)

    if "fetch" in due:
        if run_fetch(repo, result):
            result.ran.append("fetch")
            # keep the commit-graph current so ahead/behind stays cheap
            run_tasks(repo, "commit-graph", ["commit-graph"], result)
        else:
            result.failed.append("fetch")

    if "daily" in due:
        run_tasks(repo, "daily", ["loose-objects", "incremental-repack"], result)
        result.ran.append("daily")

    if "weekly" in due:
        run_tasks(repo, "weekly", ["pack-refs"], result)
        result.ran.append("weekly")

    return result


# ---- reporting -------------------------------------------------------------


def print_status(
    repos: dict[Path, Rule],
    state: State,
    config: Config,
    now: float,
    when: datetime,
    business: bool,
) -> None:
    tier_name = "business" if business else "off-hours"
    print(
        f"{BOLD}{len(repos)} repos{RESET}{DIM} — {tier_name} intervals in effect at "
        f"{when:%a %Y-%m-%d %H:%M}; business hours are {config.business_hours.describe()}"
        f"{RESET}"
    )
    print(f"{DIM}config: {', '.join(short(p) for p in config.sources)}{RESET}")

    rows = []
    for repo, rule in sorted(repos.items(), key=lambda kv: str(kv[0]).lower()):
        fetch = state.get(repo, "fetch")
        interval = rule.fetch_interval(business)
        due_at = next_due_at(fetch, interval)

        if fetch.last is None:
            last = f"{YELLOW}never{RESET}"
        else:
            last = f"{fmt_duration(now - fetch.last)} ago"

        if due_at is None or now >= due_at - int(config.tick * GRACE_FRACTION):
            nxt = f"{GREEN}now{RESET}"
        else:
            nxt = f"in {fmt_duration(due_at - now)}"

        note = ""
        if fetch.fail:
            note = (
                f"{RED}{fetch.fail} fail{'s' if fetch.fail > 1 else ''}{RESET}"
                f"{DIM} {fetch.error}{RESET}"
            )
        rows.append((short(repo), fmt_duration(interval), last, nxt, note))

    name_w = max((len(r[0]) for r in rows), default=4)
    for name, interval, last, nxt, note in rows:
        line = (
            f"  {CYAN}{name:<{name_w}}{RESET}  {interval:>4}  "
            f"{DIM}last{RESET} {last:<20}  {DIM}next{RESET} {nxt}"
        )
        if note:
            line += f"  {note}"
        print(line)


def print_plan(due_map: dict[Path, list[str]], business: bool) -> None:
    tier_name = "business" if business else "off-hours"
    if not due_map:
        print(f"nothing due ({tier_name} intervals)")
        return
    print(f"{BOLD}{len(due_map)} repos due{RESET}{DIM} ({tier_name} intervals){RESET}")
    for repo, tiers in sorted(due_map.items(), key=lambda kv: str(kv[0]).lower()):
        print(f"  {CYAN}{short(repo)}{RESET}  {DIM}{' '.join(tiers)}{RESET}")


def classify(line: str) -> str:
    """Bucket one `git fetch` ref-update line for the summary."""
    if "[deleted]" in line:
        return "deleted"
    if "[rejected]" in line:
        return "rejected"
    if "[new tag]" in line:
        return "new tag"
    if "[new branch]" in line or "[new ref]" in line:
        return "new"
    if "(forced update)" in line:
        return "forced"
    return "updated"


def summarize_updates(lines: list[str]) -> str:
    """
    "56 refs (1 updated, 22 new, 14 deleted, 19 forced)".

    Busy monorepos push a hundred bot branches per hour; logging each line would
    grow the (unrotated) launchd log without telling us anything. Counts by
    category say as much in one line — use -v for the full list.
    """
    counts: dict[str, int] = {}
    for line in lines:
        kind = classify(line)
        counts[kind] = counts.get(kind, 0) + 1
    order = ["updated", "new", "new tag", "deleted", "forced", "rejected"]
    parts = [f"{counts[k]} {k}" for k in order if k in counts]
    return f"{len(lines)} ref{'s' if len(lines) != 1 else ''} ({', '.join(parts)})"


def report(results: list[Result], when: datetime, verbose: bool = False) -> None:
    """One summary line plus detail for repos that did work — silent otherwise."""
    worked = [r for r in results if r.did_anything]
    if not worked:
        return

    failed = [r for r in results if r.failed]
    summary = (
        f"{BOLD}{when:%Y-%m-%d %H:%M}{RESET} "
        f"{len(worked)} repo{'s' if len(worked) != 1 else ''} maintained"
    )
    if failed:
        summary += f", {RED}{len(failed)} fetch failed{RESET}"
    print(summary)

    for result in sorted(worked, key=lambda r: str(r.repo).lower()):
        tiers = " ".join(result.ran) or "-"
        head = f"  {CYAN}{short(result.repo)}{RESET}  {DIM}{tiers}{RESET}"
        if result.updates:
            head += f"  {GREEN}{summarize_updates(result.updates)}{RESET}"
        print(head)
        # Rejected refs are actionable, so they always show; the rest only with -v.
        for line in result.updates:
            if verbose or classify(line) == "rejected":
                print(f"      {DIM}{line}{RESET}")
        for tier, message in result.errors.items():
            print(f"      {RED}{tier} failed:{RESET} {message}")
        for warning in result.warnings:
            print(f"      {YELLOW}warn:{RESET} {warning}")


# ---- main ------------------------------------------------------------------


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Fetch and maintain configured git repos on per-repo, "
        "business-hours-aware intervals.",
    )
    ap.add_argument("--status", action="store_true", help="print the schedule and exit")
    ap.add_argument(
        "-n", "--dry-run", action="store_true", help="report what would run, run nothing"
    )
    ap.add_argument(
        "--force", action="store_true", help="ignore the schedule; run every tier now"
    )
    ap.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="list every ref update instead of summarising by category",
    )
    ap.add_argument(
        "--repo",
        action="append",
        metavar="PATH",
        help="limit to this repo (repeatable)",
    )
    ap.add_argument(
        "--at",
        metavar="ISO8601",
        help="evaluate against a simulated local clock, e.g. 2026-08-04T22:00",
    )
    ap.add_argument(
        "-j", "--jobs", type=int, default=6, help="parallel workers (default: 6)"
    )
    ap.add_argument(
        "--config",
        metavar="PATH",
        action="append",
        type=Path,
        help="config file to use instead of the default pair (repeatable)",
    )
    args = ap.parse_args()

    paths = args.config or [
        CONFIG_DIR / "config.toml",
        CONFIG_DIR / "config.local.toml",
    ]
    try:
        config = load_config(paths)
    except (FileNotFoundError, ValueError) as exc:
        print(f"{RED}error:{RESET} {exc}", file=sys.stderr)
        return 2

    if args.at:
        try:
            when = datetime.fromisoformat(args.at)
        except ValueError:
            print(
                f"{RED}error:{RESET} --at: cannot parse {args.at!r} as ISO 8601",
                file=sys.stderr,
            )
            return 2
    else:
        when = datetime.now()
    now = when.timestamp()

    repos, skipped_jj = resolve_repos(config)

    if args.repo:
        wanted = {Path(os.path.expanduser(r)).resolve() for r in args.repo}
        unknown = wanted - set(repos)
        repos = {p: r for p, r in repos.items() if p in wanted}
        for path in sorted(unknown):
            print(
                f"{YELLOW}warning:{RESET} {short(path)} is not a configured git repo",
                file=sys.stderr,
            )

    if not repos:
        print(
            f"{YELLOW}warning:{RESET} no git repos matched "
            f"{len(config.rules)} config entr{'y' if len(config.rules) == 1 else 'ies'}",
            file=sys.stderr,
        )
        return 0

    business = config.business_hours.covers(when)
    grace = int(config.tick * GRACE_FRACTION)

    state = State(STATE_DIR / "state.json")

    if args.status:
        state.load()
        print_status(repos, state, config, now, when, business)
        for path in skipped_jj:
            print(f"{DIM}  skipped (jj, no colocated git): {short(path)}{RESET}")
        return 0

    # One tick at a time: overlapping runs would fight over both this state file
    # and git's own per-repo maintenance lock.
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    lock_path = STATE_DIR / "tick.lock"
    lock = open(lock_path, "w")
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError:
        if _TTY:
            print(f"{YELLOW}another tick is already running{RESET}", file=sys.stderr)
        return 0

    try:
        state.load()

        due_map: dict[Path, list[str]] = {}
        for repo, rule in repos.items():
            due = [
                tier
                for tier in TIERS
                if args.force
                or is_due(state.get(repo, tier), rule.interval(tier, business), now, grace)
            ]
            if due:
                due_map[repo] = due

        if args.dry_run:
            print_plan(due_map, business)
            return 0

        if not due_map:
            return 0

        results: list[Result] = []
        with concurrent.futures.ThreadPoolExecutor(
            max_workers=max(1, args.jobs)
        ) as pool:
            futures = {
                pool.submit(process, repo, tiers, now): repo
                for repo, tiers in due_map.items()
            }
            for future in concurrent.futures.as_completed(futures):
                repo = futures[future]
                try:
                    results.append(future.result())
                except Exception as exc:  # noqa: BLE001 - never lose the batch
                    result = Result(repo=repo)
                    result.failed.append("fetch")
                    result.errors["fetch"] = str(exc).splitlines()[0][:200]
                    results.append(result)

        # State is only mutated here, on the main thread, after every worker is done.
        for result in results:
            for tier in result.ran:
                tier_state = state.get(result.repo, tier)
                tier_state.last = now
                tier_state.attempt = now
                tier_state.fail = 0
                tier_state.error = ""
            for tier in result.failed:
                tier_state = state.get(result.repo, tier)
                tier_state.attempt = now
                tier_state.fail += 1
                tier_state.error = result.errors.get(tier, "")
        state.save()

        report(results, when, verbose=args.verbose)
        return 1 if any(r.failed for r in results) else 0
    finally:
        fcntl.flock(lock, fcntl.LOCK_UN)
        lock.close()


if __name__ == "__main__":
    sys.exit(main())
