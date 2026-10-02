# -*- sh-basic-offset:2 -*-
#
# zsh is NOT my shell — fish is (.config/fish/). This file exists only because
# other things still spawn zsh interactively (a manual `zsh` in a terminal).
# Claude Code's Bash tool runs /bin/zsh non-interactively, which sources
# .zshenv but never this file — PATH additions meant for agent tooling belong
# there, not here.
#
# So: deliberately boring. No plugin manager, no prompt framework, nothing that
# hits the network or prints at startup, and above all no aliases or functions
# shadowing standard commands — `diff`, `ls`, `brew` here are the real ones.
#
# The previous 522-line config is preserved verbatim, unsourced, at
# .config/zsh/zshrc.legacy.zsh. It is thick with cruft and wants a real cleanup
# rather than a straight restore: zinit git-clones itself on first run, p10k
# instant prompt writes to the terminal before anything else, an osc7 chpwd hook
# emits escape sequences on every cd, `alias brew=newbrew` triggers a Brewfile
# dump on install, `diff` is piped through delta. Port what's still wanted to
# fish; don't re-source it here.

# history
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY HIST_IGNORE_DUPS HIST_IGNORE_SPACE SHARE_HISTORY

# allow `# comments` on an interactive command line
setopt interactive_comments

# PATH is handled in .zshenv (sourced here too, but also by non-interactive
# shells that skip this file) — nothing to add for interactive-only use yet.
