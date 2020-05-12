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
# Avoid duplicates
export HISTCONTROL=ignoredups:erasedups
# Append to the Bash history file, rather than overwriting it
shopt -s histappend
export HISTFILESIZE=
export HISTSIZE=
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
if [ $HOSTNAME != "Radu-Arch" ]; then
  # Force prompt to write history after every command.
  # http://superuser.com/questions/20900/bash-history-loss
  append_history() { history -a; }
  export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"
  precmd_functions+=(append_history)
fi
# disable history expansion
set +H

# prompt. prefer ~/bin version which has personal patchess
[ -x ~/bin/starship ] && STARSHIP=~/bin/starship || STARSHIP=starship
command -v $STARSHIP >/dev/null 2>&1 && eval "$($STARSHIP init bash)"

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
  python3 -c "import sys; print(sys.stdin.read().decode('unicode_escape'))"
}

pip3_upgrade() {
  pip3 install -U "$(pip3 list --outdated | awk 'NR>2 {print $1}')"
}

# editor
ew() {
  [ ! -e "$@" ] && return 1
  emacsclient -a= -nw "$@"
}
en() {
  [ ! -e "$@" ] && return 1
  emacsclient -a= -n "$@"
}
enc() {
  [ ! -e "$@" ] && return 1
  emacsclient -a= -nc "$@"
}
e() { ew "$@"; }
export EDITOR=emacsclient
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
m() {
  [ -n "$@" ] && bat "$@" || bat -
}
alias M='$(history -p \!\!) | bat -'
export LESS=-RMiSeF
alias xo="xdg-open"
alias xc="xclip -selection clipboard"
if ! command -v tree >/dev/null 2>&1; then
  alias tree="ls -R | grep \":$\" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
  tree2() {
    find . -type d "$@" | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/"
  }
fi
[ `uname` == "Linux" ] && alias rmdir="rmdir -p --ignore-fail-on-non-empty"
alias c="bat"
alias dv="dirs -v"
alias s='sudo'
complete -F _complete_alias s
alias .~='. ~/.bashrc'
alias tm='tmux a -d || tmux'
alias xa='xargs'

find_pi() {
  sudo nmap -sP 192.168.0.0/24 | awk '/^Nmap/{ip=$NF}/B8:27:EB/{print ip}'
}

alias alert='tput bel; notify-send -u normal -t 60000 -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

source-if-e () {
  [ -f "$@" ] && . "$@"
}
if [ "$TERM" != "dumb" ]; then
  source-if-e /usr/share/fzf/key-bindings.bash # Arch
  source-if-e /usr/share/fzf/completion.bash
  source-if-e /usr/share/doc/fzf/examples/key-bindings.bash # Debian
  source-if-e /usr/share/doc/fzf/examples/completion.bash
  source-if-e /usr/local/opt/fzf/shell/key-bindings.bash # Mac homebrew
  source-if-e /usr/local/opt/fzf/shell/completion.bash
  export FZF_DEFAULT_OPTS="--bind=ctrl-v:page-down,alt-v:page-up"
  export FZF_DEFAULT_COMMAND='fd --hidden'
fi

# enable programmable completion features
# worth mentioning: https://github.com/cykerway/complete-alias
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
  . /etc/bash_completion
fi

if [ -f ~/.bashrc_specific_mac ] && [ $(uname) == "Darwin" ]; then
  . ~/.bashrc_specific_mac
fi

if [ -f ~/.bashrc_specific ]; then
  . ~/.bashrc_specific
fi

# https://the.exa.website
# needs to be after PATH setting
if command -v exa >/dev/null 2>&1; then
  alias l='exa'
  alias ll='l -la'
else
  alias l='ls -F'
  [ `uname` == "Linux" ] && alias l='ls -F --color=auto --group-directories-first'
  alias ll='l -lA -h'
fi

# to not be annoying if previous fails
true
