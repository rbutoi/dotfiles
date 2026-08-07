# AGENTS.md

This file provides guidance to coding agents (Claude Code and any other tool following
the AGENTS.md convention) when working with code in this repository. `CLAUDE.md` is a
symlink to this file — edit this one.

## What this repo is

Personal dotfiles, linked into `$HOME` by [a fork of
`dotfiler`](https://github.com/svetlyak40wt/dotfiler) kept at `~/dev/dotfiler`. Two
*environments* overlay onto `$HOME`: this repo (`~/dev/dotfiles`, public) and
`~/dev/private-dots` (private sibling). Paths mirror `$HOME`, so `.config/git/config`
here is symlinked to `~/.config/git/config`.

Which repos are environments is declared in `~/.config/dotfiler/config.toml` — itself
tracked here at `.config/dotfiler/config.toml`, so the configuration is self-hosting —
plus `conf.d/*.toml` contributed by the private repo. This is why all three repos are
ordinary repos in `~/dev` rather than living inside the tool's checkout.

Nothing is built, compiled, or deployed. **The repo _is_ the live config** — editing a
tracked file takes effect immediately, with no install step. Only *adding, renaming, or
deleting* files needs a relink (`dotup`).

Current daily driver is macOS with fish; a large part of the tree is dormant Linux-era
config (see below).

## Commands

```fish
dotup                          # relink after add/rename/delete (dot update --skip-pull)
dot update --dry -v            # preview; -v is needed to see already-linked
dot status                     # per-environment git status
dot envs                       # resolved environments; --json for tooling
dot prune --dry                # links we own that the envs no longer offer

dot_adopt.py ~/.config/foo     # move an existing ~/ path into the repo, relink, stage
dot_adopt.py -n ~/.config/foo  # dry run; --env private-dots for the private repo

update_cached_confs.fish       # regenerate cached `brew shellenv` / `zoxide init` in fish conf.d

git_maintenance_tick.py --status       # background fetch/maintenance: what's scheduled, what's stale
git_maintenance_tick.py --dry-run
git_maintenance_tick.py --force --repo ~/dev/x
```

`dot` is `~/dev/dotfiler/bin/dot`; the tests for it are `uv run --with pytest pytest`
in that repo.

There is no CI, test suite, or pre-commit config here — check changed files by hand
with the tools mise already installs:

| Language | Check |
| --- | --- |
| fish | `fish -n file.fish`, format with `fish_indent -w` |
| bash/sh | `shellcheck path` |
| Python (`.local/bin/*`) | `ty check path` — they're PEP 723 `uv run --script` files, run them directly |
| Emacs Lisp | no batch harness; reload in a running Emacs |

## Live vs. dormant

- **Active:** `.config/{fish,git,git-maintenance,mise,emacs,zed,tmux,skhd,ghostty,gitu}`,
  `.local/bin/`, `Library/LaunchAgents/`, `.zshrc`, `.bashrc`.
- **Dormant:** the Linux desktop stack — `sway`, `i3blocks`, `polybar`, `waybar`, `rofi`,
  `wofi`, `xmonad`, `dunst`/`mako`, `pacman*`, `.config/systemd/`, `.mail/`, and most of
  `bin/`. Nearly all of it was last touched in a bulk 2023 import. Leave it alone unless
  the task is explicitly about it; don't sweep it into unrelated refactors.
- `.local/bin/` is where new tooling goes (macOS-era, `#!/usr/bin/env -S uv run --script`).
  `bin/` is the older Linux script collection, on `$PATH` via `~/bin`.

## Conventions that matter

**`.zshrc` and `.bashrc` are deliberately minimal, for agent safety.** fish is the real
shell; zsh/bash exist because other tooling — including agent Bash tools — spawns them
and sources these files. Anything clever there silently rewrites what an agent sees and
runs. So: no aliases or functions shadowing standard commands (`diff`, `ls`, `brew` must
be the real ones), nothing hitting the network, nothing printing at startup. The former
522- and 242-line versions are preserved *unsourced* at `.config/zsh/zshrc.legacy.zsh`
and `.config/bash/bashrc.legacy.bash`; port wanted bits to fish rather than restoring them.

**New shell code goes in fish.** `.config/fish/config.fish` for env/abbrs/aliases,
`functions/<name>.fish` for one autoloaded function per file (with `--description`),
`conf.d/` for early setup. Committed scripts with a bash/sh shebang stay POSIX/bash.

**Some config dirs are real directories in `$HOME`, not whole-dir symlinks.**
`.config/fish/conf.d`, `.config/fish/functions`, `.config/emacs`, and `.config/zed` are
written into by fisher, Emacs, and Zed, so `dot` links tracked files *individually* and
generated files sit beside them untracked. Don't "fix" this into a directory symlink —
that's what the `mkdir -p` loop in README.md exists to prevent.

**`.dotignore` says what must never be linked into `$HOME`.** Per-repo, merged over
dotfiler's built-in defaults; each line is an anchored case-insensitive regex matched
against the basename (a regex, not a glob). `AGENTS.md` and `CLAUDE.md` are there because
a `CLAUDE.md` at the `$HOME` root would be read as project instructions by any agent
session started there. Adding a pattern does not unlink anything already linked — the
file still exists here, so nothing dangles — so follow it with `dot prune`.

**Machine- and work-specific values live in `private-dots` and are included, never merged
into tracked files:** `.config/git/config` includes `~/.config/git/config.local`;
git-maintenance merges `config.local.toml` over its `config.toml`; Emacs `early-init.el`
does `(load "early-init-work.el" :noerror)`. Follow that pattern for anything host- or
employer-specific.

**Two package managers, split by role:** mise (`.config/mise/config.toml`) owns CLI tool
versions; the Homebrew `.config/brewfile/Brewfile` owns GUI apps and system packages. A
tool referenced from fish config should be declared in one of them.

**Emacs:** elpaca + use-package with `use-package-always-defer t` and
`always-ensure t`; `init.el` is just an ordered list of `require`s for `lisp/init-*.el`
modules; no-littering redirects state into `var/` (which `.ignore` excludes from ripgrep).
`lisp/custom.el` is Emacs-written but tracked — `lisp/update_custom.el.py` regenerates its
footer mapping `custom-safe-themes` SHAs to the elpaca source files they came from.

**Commit messages:** `area: lowercase summary` (`fish:`, `git:`, `shells:`, `emacs:`),
occasionally conventional (`fix(scc-commits):`, `perf(...)`).

**Comments carry the reasoning.** Configs and the `.local/bin` scripts document *why* a
choice was made — which upstream bug forced it, which obvious alternative was rejected and
what broke (see the `git_maintenance_tick.py` docstring, `.config/git/config`'s notes on
delta and difftastic, `prs-restale.fish` on OSC-8 links). Preserve and extend those notes
when you touch the code; new `.local/bin` scripts are expected to open with the same kind
of design docstring.

**Careful with stray files in the repo root:** `dot` will happily symlink one over its
real `$HOME` counterpart. `.gitignore` already blocks known offenders (`.zcompdump*`,
`.config/git-maintenance/config.local.toml`) — read its comments before adding files there.
