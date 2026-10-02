# -*- sh-basic-offset:2 -*-
#
# zsh is NOT my shell — fish is (.config/fish/). This file exists only because
# other things still spawn zsh; notably Claude Code's Bash tool runs /bin/zsh
# non-interactively, and non-interactive zsh sources .zshenv but skips .zshrc
# entirely. So anything PATH-related that non-interactive tooling needs to see
# has to live here, not in .zshrc.
#
# Same rules as .zshrc: deliberately boring, no aliases or functions shadowing
# standard commands, nothing that hits the network or prints at startup.

# fish's mise-activate hook rewrites PATH dynamically on every prompt, but that
# hook only runs in the interactive fish shell that spawned this process tree —
# a plain `zsh -c '...'` (or Claude Code's Bash tool) never triggers it, so it's
# stuck with whatever PATH looked like at the moment this process was forked.
# The shims dir resolves the right version per-invocation instead of relying on
# a shell hook, so put it on PATH here to make mise-managed tools visible to
# non-interactive shells too.
case ":$PATH:" in
  *":$HOME/.local/share/mise/shims:"*) ;;
  *) PATH="$HOME/.local/share/mise/shims:$PATH" ;;
esac

# my own scripts
case ":$PATH:" in
  *":$HOME/.local/bin:"*) ;;
  *) PATH="$HOME/.local/bin:$PATH" ;;
esac
