# Personal Zsh configuration file. It is strongly recommended to keep all
# shell customization and configuration (including exported environment
# variables such as PATH) in this file or in files source by it.
#
# Documentation: https://github.com/romkatv/zsh4humans/blob/v5/README.md.

# Periodic auto-update on Zsh startup: 'ask' or 'no'.
# You can manually run `z4h update` to update everything.
zstyle ':z4h:' auto-update      'yes'
# Ask whether to auto-update this often; has no effect if auto-update is 'no'.
zstyle ':z4h:' auto-update-days '28'

# Automaticaly wrap TTY with a transparent tmux ('integrated'), or start a
# full-fledged tmux ('system'), or disable features that require tmux ('no').
# zstyle ':z4h:' start-tmux       'integrated'
zstyle ':z4h:' start-tmux       'no'
# Move prompt to the bottom when zsh starts up so that it's always in the
# same position. Has no effect if start-tmux is 'no'.
zstyle ':z4h:' prompt-at-bottom 'no'

# Keyboard type: 'mac' or 'pc'.
zstyle ':z4h:bindkey' keyboard  'pc'

# Right-arrow key accepts one character ('partial-accept') from
# command autosuggestions or the whole thing ('accept')?
zstyle ':z4h:autosuggestions' forward-char 'accept'

# Recursively traverse directories when TAB-completing files.
zstyle ':z4h:fzf-complete' recurse-dirs 'no'

# Enable ('yes') or disable ('no') automatic teleportation of z4h over
# ssh when connecting to these hosts.
# zstyle ':z4h:ssh:example-hostname1'   enable 'yes'
# zstyle ':z4h:ssh:*.example-hostname2' enable 'no'
# The default value if none of the overrides above match the hostname.
# zstyle ':z4h:ssh:*'                   enable 'no'

# Send these files over to the remote host when connecting over ssh to the
# enabled hosts.
zstyle ':z4h:ssh:*' send-extra-files '~/.nanorc' '~/.env.zsh'

# Clone additional Git repositories from GitHub.
#
# This doesn't do anything apart from cloning the repository and keeping it
# up-to-date. Cloned files can be used after `z4h init`. This is just an
# example. If you don't plan to use Oh My Zsh, delete this line.
# z4h install ohmyzsh/ohmyzsh || return

# Install or update core components (fzf, zsh-autosuggestions, etc.) and
# initialize Zsh. After this point console I/O is unavailable until Zsh
# is fully initialized. Everything that requires user interaction or can
# perform network I/O must be done above. Everything else is best done below.
z4h init || return

# Extend PATH.
path=(~/bin $path)

# Export environment variables.
export GPG_TTY=$TTY

# Source additional local files if they exist.
z4h source ~/.env.zsh

# Use additional Git repositories pulled in with `z4h install`.
#
# This is just an example that you should delete. It does nothing useful.
# z4h source $Z4H/ohmyzsh/ohmyzsh/lib/diagnostics.zsh
# z4h source $Z4H/ohmyzsh/ohmyzsh/plugins/emoji-clock/emoji-clock.plugin.zsh
# fpath+=($Z4H/ohmyzsh/ohmyzsh/plugins/supervisor)

# Define key bindings.
z4h bindkey z4h-backward-kill-word  Ctrl+Backspace Ctrl+H
z4h bindkey z4h-backward-kill-zword Ctrl+Alt+Backspace

z4h bindkey undo Ctrl+/  # undo the last command line change
z4h bindkey redo Alt+/   # redo the last undone command line change

z4h bindkey z4h-cd-back    Alt+Left   # cd into the previous directory
z4h bindkey z4h-cd-forward Alt+Right  # cd into the next directory
z4h bindkey z4h-cd-up      Alt+Up     # cd into the parent directory
z4h bindkey z4h-cd-down    Alt+Down   # cd into a child directory

# Autoload functions.
autoload -Uz zmv

# Define functions and completions.
function md() { [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1" }
compdef _directories md

# Define named directories: ~w <=> Windows home directory on WSL.
[[ -z $z4h_win_home ]] || hash -d w=$z4h_win_home

# # Define aliases.
# alias tree='tree -a -I .git'

# # Add flags to existing aliases.
# alias ls="${aliases[ls]:-ls} -A"

# Set shell options: http://zsh.sourceforge.net/Doc/Release/Options.html.
# setopt glob_dots     # no special treatment for file names with a leading dot
# setopt no_auto_menu  # require an extra TAB press to open the completion menu

[[ -e ~/.config/path.sh ]] && source ~/.config/path.sh

# my bash stuff haphazardly

# . ~/.cache/complete-alias/complete_alias

#######################################
# User specific aliases and functions #
#######################################

upto () {
  if [ -z "$1" ]; then
    return
  fi
  local upto=$1
  cd "${PWD/\/$upto\/*//$upto}"
}

_upto() {
  local cur=${COMP_WORDS[COMP_CWORD]}
  local d=${PWD//\//\ }
  COMPREPLY=( $( compgen -W "$d" -- "$cur" ) )
}
complete -F _upto upto

pomo() {
  arg1=$1
  shift
  args="$*"

  min=${arg1:?Example: pomo 15 Take a break}
  sec=$((min * 60))
  msg="${args:?Example: pomo 15 Take a break}"

  while true; do
    sleep "${sec:?}" && echo "${msg:?}" && notify-send -u critical -t 0 "${msg:?}"
  done
}

which_countries_connected_today() {
  set -x
  journalctl -b _COMM=sshd |
    rg --color never -o '\d+\.\d+\.\d+\.\d+' |
    perl -ne 'chomp; my $geoip = qx(geoiplookup $_); print "$geoip" ' |
    wc_occurrences
  set +x
}

pause_torrents () {
  hours="${1:-1}"
  set -x
  logger stopping rtorrent for $hours hour && systemctl --user stop rtorrent.service && (echo "systemctl --user start rtorrent.service" | at now + $hours hour)
  set +x
}

# !!!
# mcd           () { mkdir -p "$@" && cd "$@"                                                                     ; }
fork          () { (setsid "$@" &)                                                                              ; }
all_atq       () { atq | perl -ne 'print "\n"; /^([\d]+).*/ && print $_, qx(at -c $1 | tail -2 | head -1)'      ; }
escape        () { python3 -c 'import json, sys; print(json.dumps(sys.stdin.read()))'                           ; }
unescape      () { python3 -c "import sys; print(sys.stdin.read().encode('utf-8').decode('unicode_escape'))"    ; }
pip3_upgrade  () { pip3 install -U "$(pip3 list --outdated | awk 'NR>2 {print $1}')"                            ; }
wc_occurrences() { python3 -c 'import collections, sys, pprint; pprint.pprint(collections.Counter(sys.stdin));' ; }
find_pi       () { sudo nmap -sn 192.168.0.0/24 | awk '/^Nmap/{ip=$NF}/B8:27:EB/{print ip}'                     ; }
fix_swaysock  () { export SWAYSOCK=/run/user/$(id -u)/sway-ipc.$(id -u).$(pgrep -x sway).sock                   ; }
fix_i3sock    () { export   I3SOCK=/run/user/$(id -u)/i3/ipc-socket.$(pgrep -x i3)                              ; }
alias alert='tput bel; notify-send -u normal -t 60000 -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# git
git_grep_blame() {
  git grep -n "$@" | perl -F':' -anpe '$_=`git blame -L$F[1],+1 $F[0]`'
}
git_dangling()   {
  # show git dangling commits sorted by timestamp
  git fsck --lost-found 2>/dev/null | grep "dangling commit" | choose 2 |
    xargs git show --no-patch --pretty=format:"%ad %h by %an, %s" --date=iso |
    sort -r | fzf --preview 'git show --color $(echo {} | choose 3)'
}

# need TERM hack outside tmux since emacs won't have truecolor otherwise.
# setting that to be the default TERM results in no truecolor through mosh
ec()  {
  if [ -z "$TMUX" ]; then
    TERM=foot-direct emacsclient "$@"
  else
    emacsclient "$@"
  fi
}
ew()  { ec -a= -nw "$@";  }; e() { ew "$@"; } # inline console editor
en()  { ec -a= -n  "$@";  }                   # open in existing editor
ewc() { ec -a= -nc "$@";  }                   # new graphical editor
# export EDITOR="TERM=foot-direct emacsclient -t"
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=zile
export BROWSER=xdg-open
export CLICOLOR=1

man() {
  if ! pgrep emacs >/dev/null; then
    command man "$@"
    return
  fi
  # check if manpage exists, let stderr output
  if ! command man -w "$@" > /dev/null; then
    return
  fi
  if [ $# -eq 0 ]; then
    cmd="(call-interactively 'man)"
  else
    cmd="(man \"$@\")"
  fi
  if [ -n "$INSIDE_EMACS" ]; then
    emacsclient -n --eval "$cmd"
  else
    emacsclient -t --eval "(progn $cmd (other-window 1) (delete-other-windows))"
  fi
}

alias l='ls -F'
alias ll='l -lA -h'
alias pg="pgrep"
alias chmox="chmod +x"
alias -- -="cd -"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias cdg='cd "$(git rev-parse --show-toplevel)"'
alias M='$(history -p \!\!) | less -'
export LESS=-RMiSeF
alias xo="xdg-open"
alias xc="xclip -selection clipboard"
if ! command -v tree >/dev/null; then
  alias tree="ls -R | grep \":$\" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
  tree2() {
    find . -type d "$@" | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/"
  }
fi
alias dv="dirs -v"
alias s='sudo'
alias ss='s ss' # these silently degrade without sudo
alias lsof='s lsof'
alias .~='exec zsh'
alias tm='tmux new -A -s auto'
alias tenv='eval $(tmux showenv -s)'
alias xa='xargs'
alias ssha='ssh -t a tmux new -ADs auto'
alias mosha='mosh -p 55880 a /home/radu/bin/continuetmux'
alias stow='stow -v' # nice to see the actions taken by default
alias fd="fd --one-file-system"
alias type='whence -f' # am too used to bash

########################
# external shell tools #
########################

if command -v rg >/dev/null; then
  export RIPGREP_CONFIG_PATH=~/.config/ripgreprc
  alias g=rg
fi

if command -v procs >/dev/null; then
  alias psg=procs
fi

alias less="TERM=screen-256color less"
if command -v bat >/dev/null; then
  # change TERM for proper italics support in bat and less
  alias bat="TERM=screen-256color bat --italic-text=always --wrap=never"
  alias xargs="TERM=screen-256color xargs" # if it calls bat/less
  alias c=bat
  alias m=bat # used to be `most` for a long time
  export PAGER="bat --plain"
  export BAT_THEME=gruvbox-dark
  export BAT_STYLE=changes,header,rule,numbers,snip
fi

unalias diff
if command -v delta >/dev/null; then
  diff() { /usr/bin/diff -u "$@" | delta --side-by-side; }
else
  alias diff="diff --color=auto"
fi

# !!!!!!
# [ -f ~/.fzf.bash ] && . ~/.fzf.bash
# [ -d /usr/share/fzf ] && . /usr/share/fzf/completion.bash && \
#   . /usr/share/fzf/key-bindings.bash
export FZF_DEFAULT_OPTS="--bind=ctrl-v:page-down,alt-v:page-up
  --color=fg:#ebdbb2,bg:#282828,hl:#fabd2f,fg+:#ebdbb2,bg+:#3c3836,hl+:#fabd2f
  --color=info:#83a598,prompt:#bdae93,spinner:#fabd2f,pointer:#83a598,marker:#fe8019,header:#665c54"
export FZF_DEFAULT_COMMAND='fd --hidden'
export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fzfp () { eval $FZF_DEFAULT_COMMAND -tf -tl . "$@" | fzf --preview 'bat --style=numbers --color=always {}' }

# # enable programmable completion features
# # worth mentioning: https://github.com/cykerway/complete-alias
# # Use bash-completion, if available
# [[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
#     . /usr/share/bash-completion/bash_completion
# !!!!!!!! not zsh

if command -v exa >/dev/null; then
  alias l='exa';              complete -F _complete_alias l
  alias ll='exa --color always -aagl --git'
fi

if [[ -e ~/.config/broot/launcher/bash/br ]]; then
  . ~/.config/broot/launcher/bash/br
   cdb() { br --only-folders --cmd "$*:cd"; }
    lb() { br --sizes --dates --permissions; }
  treb() { br --height $((LINES - 2)) --cmd :pt "$@"; }
fi

command -v duf  >/dev/null && alias df=duf
command -v navi >/dev/null && eval "$(navi widget zsh)" ### !!!!!!

alias dig="dig +nostats +nocomments +nocmd"  # make dig quiet by default

###

autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

set_win_title() {
  bpwd="$(basename "$PWD")"
  printf "${1:-\033]0;%s\007}" "${bpwd/#$USER/~}@${HOST/butoi-/}"
}
set_win_title_tmux() { set_win_title "\033k%s\033"; }
if [ -z "$TMUX" ]; then
  if [ -z "$INSIDE_EMACS" ]; then
    set_win_title
    add-zsh-hook -Uz precmd set_win_title
  fi
else
  set_win_title_tmux
  add-zsh-hook -Uz precmd set_win_title_tmux
fi

fpath=(~/.local/share/zsh/site-functions ~/.config/zsh $fpath)

# ! topgrade every week
if [[ $(($(</tmp/last_topgrade) + 604800)) -lt $(date +%s) ]] 2>/dev/null &&
     load_below; then
  echo -e "\033[36mTopgrade...\033[0m"
  date +%s >/tmp/last_topgrade
  if [[ -n "$TMUX" ]]; then
    tmux new-window 'echo -e "\033[36mTopgrade...\033[0m"; echo acquiring sudo timeout; sudo true; topgrade; echo waiting for input...; read'
  else
    # the built-in topgrade --tmux doesn't dedup calls
    tmux new -As topgrade -- 'echo acquiring sudo timeout; sudo true; topgrade; echo waiting for input...; read'
  fi
fi

# z4h breaks ssh config by making contol master too long?: https://github.com/romkatv/zsh4humans/issues/106
# zstyle ':z4h:ssh:*' ssh-command command ssh
unset -f ssh

[[ -e ~/.zshrc_specific ]] && source ~/.zshrc_specific
[[ -e ~/oss/zsh-prompt-benchmark/zsh-prompt-benchmark.plugin.zsh ]] && \
  .   ~/oss/zsh-prompt-benchmark/zsh-prompt-benchmark.plugin.zsh


# BEGIN_KITTY_SHELL_INTEGRATION
if test -e "$HOME/oss/kitty/shell-integration/kitty.zsh"; then source "$HOME/oss/kitty/shell-integration/kitty.zsh"; fi
# END_KITTY_SHELL_INTEGRATION
