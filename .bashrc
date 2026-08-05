# .bashrc
# -*- sh-basic-offset:2 -*-
#
# bash is NOT my shell — fish is (.config/fish/). This file is kept only
# because tooling still spawns bash, and anything clever here silently
# rewrites what LLM agents see and run. Same reasoning as .zshrc, which got
# the same treatment.
#
# So: deliberately boring. Nothing hitting the network or printing at startup,
# and no aliases or functions shadowing standard commands — `diff`, `ls`, `df`
# here are the real ones.
#
# The previous 242-line config is preserved verbatim, unsourced, at
# .config/bash/bashrc.legacy.bash. It wants a real cleanup rather than a
# straight restore: `alias diff=delta` replaced diff wholesale, `alias s=sudo`
# plus `alias lsof='s lsof'` and `alias ss='s ss'` silently escalated
# privileges, `man` and `fd` were wrapped, and it sourced three files that no
# longer exist (~/.config/path.sh, ~/.bashrc_specific{,_mac}). Port what's
# still wanted to fish; don't re-source it here.

# if not running interactively, do nothing
[ -z "$PS1" ] && return

# history
HISTFILE=~/.bash_history
HISTSIZE=1000000
HISTFILESIZE=$HISTSIZE
HISTCONTROL=ignoreboth
shopt -s histappend

# my own scripts. the rest of PATH is inherited — mise is activated by fish in
# the parent process, never here.
case ":$PATH:" in
  *":$HOME/.local/bin:"*) ;;
  *) PATH="$HOME/.local/bin:$PATH" ;;
esac
