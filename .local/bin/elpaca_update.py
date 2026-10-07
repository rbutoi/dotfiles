#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""
Elpaca Update
=============
Update every Elpaca package in a separate, headless Emacs, then tell the
Emacs you're actually using that it can restart onto the new code.

Running `elpaca-pull-all' inside the live Emacs freezes it for the minute or
two the fetches and rebuilds take, and triggering it from a timer interrupts
whatever you're doing. Here the update happens in `emacs --batch' (see
lisp/elpaca-batch-update.el for how a batch process gets the package list
from the real config), and the live Emacs is only touched at the end,
through its emacsclient server.

Usage:
    elpaca_update.py                  # update; if anything changed, ask the live Emacs to restart
    elpaca_update.py --restart now    # ... restart it without asking
    elpaca_update.py --restart never  # ... leave it alone, just print the summary
    elpaca_update.py --no-notify      # don't contact a running Emacs at all
    elpaca_update.py --emacs-dir DIR  # operate on another config dir (testing); implies --no-notify

The batch process's full output is kept in <emacs-dir>/var/elpaca-update.log.
Exit status: 0 on success (even if nothing changed), 1 if some package failed
or Emacs could not be started, 2 if the update timed out.

Why a restart rather than hot-reloading: the live Emacs has many packages
deferred (`use-package-always-defer'), so once their build directories are
replaced it would lazy-load new code beside already-loaded old code. A
restart is the only way back to a consistent state. `restart-emacs' still
goes through the usual save-buffers prompts, and the "ask" mode defers to you
first because the prompt lands in whatever frame is selected.

Scheduling is deliberately not built in: run it by hand, or from a
LaunchAgent; the notify step works the same either way as long as the agent
shares your TMPDIR (that's where Emacs puts the emacsclient socket).
"""

import argparse
import json
import shutil
import subprocess
import sys
from pathlib import Path

DEFAULT_EMACS_DIR = Path.home() / ".config" / "emacs"
EMACS_FALLBACK = "/opt/homebrew/bin/emacs"


def find_emacs() -> str:
    return shutil.which("emacs") or EMACS_FALLBACK


def run_batch(emacs_dir: Path, log: Path) -> tuple[int, list[list[str]]]:
    """Run the batch update; return (exit status, parsed stdout records)."""
    entry = emacs_dir / "lisp" / "elpaca-batch-update.el"
    cmd = [
        find_emacs(), "--batch", "--init-directory", str(emacs_dir),
        "-l", str(entry), "-f", "elpaca-batch-update",
    ]
    log.parent.mkdir(parents=True, exist_ok=True)
    with log.open("w") as logf:
        try:
            proc = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=logf, text=True)
        except FileNotFoundError as e:
            print(f"elpaca_update: cannot start Emacs: {e}", file=sys.stderr)
            return 1, []
    # stdout is also kept in the log so it holds the whole story.
    with log.open("a") as logf:
        logf.write("\n--- stdout ---\n" + proc.stdout)
    records = [line.split("\t") for line in proc.stdout.splitlines() if line]
    return proc.returncode, records


def elisp_string(s: str) -> str:
    # JSON string escapes (\" \\ \n \uXXXX) are all valid Elisp string syntax.
    return json.dumps(s)


def notify_emacs(mode: str, summary: str) -> None:
    """Ask the live Emacs to restart. The eval returns immediately; the prompt
    runs from a 0s timer, because y-or-n-p inside the eval would hold the
    emacsclient connection (and this script) open until someone answers."""
    msg = elisp_string(summary)
    if mode == "now":
        form = f"(progn (run-at-time 0 nil (lambda () (message {msg}) (restart-emacs))) nil)"
    else:
        form = (
            "(progn (run-at-time 0 nil (lambda () "
            f"(when (y-or-n-p {elisp_string(summary + ' Restart Emacs now? ')}) (restart-emacs)))) nil)"
        )
    client = shutil.which("emacsclient")
    if client is None:
        print("elpaca_update: emacsclient not found; not signalling Emacs")
        return
    r = subprocess.run([client, "--eval", form], capture_output=True, text=True)
    if r.returncode != 0:
        print("elpaca_update: no running Emacs server to signal "
              f"({r.stderr.strip() or 'emacsclient failed'})")
    else:
        print("elpaca_update: signalled the running Emacs")


def main() -> int:
    ap = argparse.ArgumentParser(description="Update Elpaca packages in a separate Emacs process.")
    ap.add_argument("--restart", choices=["ask", "now", "never"], default="ask",
                    help="what to do with the running Emacs after an update (default: ask)")
    ap.add_argument("--no-notify", action="store_true", help="don't contact a running Emacs")
    ap.add_argument("--emacs-dir", type=Path, default=DEFAULT_EMACS_DIR,
                    help="Emacs config directory (default: ~/.config/emacs)")
    args = ap.parse_args()

    emacs_dir = args.emacs_dir.expanduser().resolve()
    # Never signal the live Emacs about an update that happened somewhere else.
    notify = not args.no_notify and args.restart != "never" and emacs_dir == DEFAULT_EMACS_DIR.resolve()
    log = emacs_dir / "var" / "elpaca-update.log"

    print(f"elpaca_update: updating packages in {emacs_dir} (log: {log})")
    status, records = run_batch(emacs_dir, log)

    updated = [r for r in records if r[0] == "updated" and len(r) == 5]
    failed = [(r[1], r[2] if len(r) > 2 else "") for r in records if r[0] == "failed" and len(r) >= 2]

    for _, pkg, old, new, count in updated:
        print(f"  {pkg}: {old}..{new} ({count} commits)")
    for pkg, reason in failed:
        print(f"  FAILED {pkg}: {reason}")
    if status == 2:
        print(f"elpaca_update: timed out; see {log}")
    elif status != 0 and not failed:
        print(f"elpaca_update: Emacs exited with status {status}; see {log}")

    if updated:
        summary = f"Elpaca updated {len(updated)} package{'s' if len(updated) != 1 else ''}."
        print(summary)
        if notify:
            notify_emacs(args.restart, summary)
    elif status == 0:
        print("elpaca_update: everything already up to date")

    return status


if __name__ == "__main__":
    sys.exit(main())
