# .bashrc
# -*- sh-basic-offset:2 -*-

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#########################
# Bash/terminal options #
#########################

ulimit -c unlimited
# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob
shopt -s extglob
shopt -s checkwinsize
# https://unix.stackexchange.com/questions/332791/how-to-permanently-disable-ctrl-s-in-terminal
stty -ixon
# Avoid duplicates and leading spaces
export HISTCONTROL=ignoreboth:erasedups
# Append to the Bash history file, rather than overwriting it
shopt -s histappend
export HISTFILESIZE=
export HISTSIZE=
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
# disable history expansion
set +H

# prompt + title
STARSHIP=starship
[ -x ~/bin/starship ] && STARSHIP=~/bin/starship
eval "$($STARSHIP init bash)"
function set_win_title()      { bpwd=$(basename $PWD); echo -ne "\033]0;${HOSTNAME/butoi-/}:${bpwd/#$USER/\~}\007"; }
function set_win_title_tmux() { bpwd=$(basename $PWD); echo -ne   "\033k${HOSTNAME/butoi-/}:${bpwd/#$USER/\~}\033"; }
if [ -z "$TMUX" ]; then
  starship_precmd_user_func=set_win_title
else
  starship_precmd_user_func=set_win_title_tmux
fi

#######################################
# User specific aliases and functions #
#######################################

mcd() {
  mkdir -p "$@"
  cd "$@"
}

fcd() {
  cd $(find . -name "$@" -type d)
}

upto ()
{
  if [ -z "$1" ]; then
    return
  fi
  local upto=$1
  cd "${PWD/\/$upto\/*//$upto}"
}

_upto()
{
  local cur=${COMP_WORDS[COMP_CWORD]}
  local d=${PWD//\//\ }
  COMPREPLY=( $( compgen -W "$d" -- "$cur" ) )
}
complete -F _upto upto

fork() { (setsid "$@" &); }

all_atq () {
  atq | perl -ne 'print "\n"; /^([\d]+).*/ && print $_, qx(at -c $1 | tail -2 | head -1)'
}

escape() {
  python3 -c 'import json, sys; print(json.dumps(sys.stdin.read()))'
}

unescape() {
  python3 -c "import sys; print(sys.stdin.read().encode('utf-8').decode('unicode_escape'))"
}

pip3_upgrade() {
  pip3 install -U "$(pip3 list --outdated | awk 'NR>2 {print $1}')"
}

# editor
ew() {
  emacsclient -a= -nw "$@"
}
en() {
  emacsclient -a= -n "$@"
}
enc() {
  emacsclient -a= -nc "$@"
}
e() { ew "$@"; }
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=zile

export RIPGREP_CONFIG_PATH=~/.config/ripgreprc
export CLICOLOR=1

alias g="grep --color=always -i"
alias pg="pgrep"
psg() {
  ps aux | grep "$@" | grep -iv 'grep\|shell-history'
}
alias chmox="chmod +x"
alias -- -="cd -"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
export PAGER=less
alias m=less
alias M='$(history -p \!\!) | less -'
export LESS=-RMiSeF
alias xo="xdg-open"
alias xc="xclip -selection clipboard"
if ! command -v tree >/dev/null 2>&1; then
  alias tree="ls -R | grep \":$\" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
  tree2() {
    find . -type d "$@" | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/"
  }
fi
alias c="bat"
alias dv="dirs -v"
alias s='sudo'
complete -F _complete_alias s
alias .~='. ~/.bashrc'
alias tm='tmux new -A -s auto'
alias tenv='eval $(tmux showenv -s)'
alias xa='xargs'
alias ssha='ssh -t a tmux new -ADs auto'
alias mosha='mosh -p 22688 a /home/radu/bin/continuetmux'
alias stow='stow -v' # nice to see the actions taken by default
alias count_word_occurrences="python3 -c 'import collections, sys, pprint; pprint.pprint(collections.Counter(sys.stdin));'"
alias fd="fd --one-file-system"
alias diff="diff --color=auto"

find_pi() {
  sudo nmap -sP 192.168.0.0/24 | awk '/^Nmap/{ip=$NF}/B8:27:EB/{print ip}'
}

fix_swaysock() {
  export SWAYSOCK=$(sway --get-socketpath)
}

alias alert='tput bel; notify-send -u normal -t 60000 -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

if [ -f /usr/share/fzf/completion.bash ]; then
  source /usr/share/fzf/completion.bash
  source /usr/share/fzf/key-bindings.bash
fi
export FZF_DEFAULT_OPTS="--bind=ctrl-v:page-down,alt-v:page-up"
export FZF_DEFAULT_COMMAND='fd --hidden'
alias fzfp="fd -tf | fzf --preview 'bat --style=numbers --color=always {}'"

git-grep-blame() {
  git grep -n "$@" | perl -F':' -anpe '$_=`git blame -L$F[1],+1 $F[0]`'
}

function pomo() {
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

# https://the.exa.website
# needs to be after PATH setting
if command -v exa >/dev/null 2>&1; then
  alias l='exa'
  alias ll='exa -aagl'
else
  alias l='ls -F'
  alias ll='l -lA -h'
fi

############
# epilogue #
############

# enable programmable completion features
# worth mentioning: https://github.com/cykerway/complete-alias
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
  . /etc/bash_completion
fi

command -v pipx >/dev/null && \
  eval "$(register-python-argcomplete pipx)"

if [ -f ~/.bashrc_specific_mac ] && [ $(uname) == "Darwin" ]; then
  . ~/.bashrc_specific_mac
fi

if [ -f ~/.bashrc_specific ]; then
  . ~/.bashrc_specific
fi

# to not be annoying if previous fails
true
