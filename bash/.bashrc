# .bashrc
# -*- sh-basic-offset:4 -*-

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

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

# Force prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss
append_history() { history -a; }
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"
precmd_functions+=(append_history)

if [ "$TERM" != "dumb" ]; then
    RED='\[\e[0;31m\]'
    GREEN='\[\e[0;32m\]'
    RESET='\[\e[0m\]'
    export PS1="${RED}${BOLD}\${?##0}${RESET}${GREEN}\w${RESET} "
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
  python3 -c "import sys; print(sys.stdin.read().decode('unicode_escape'))"
}

# editor
ew() {
    [ ! -e "$@" ] && return 1
    emacsclient -nw "$@"
}
en() {
    [ ! -e "$@" ] && return 1
    emacsclient -n "$@"
}
e() { ew "$@"; }
export EDITOR="emacsclient -nw -a="
export ALTERNATE_EDITOR=zile


export RIPGREP_CONFIG_PATH=~/.config/ripgreprc
export CLICOLOR=1

alias g="grep --color=always -i"
alias pg="ps aux | grep -v grep | g"
alias chmox="chmod +x"
alias -- -="cd -"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias m="less"
alias M='$(history -p \!\!) | less'
export LESS=-RMiSe
alias xo="xdg-open"
alias xc="xclip -selection clipboard"
alias tree="ls -R | grep \":$\" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
[ `uname` == "Linux" ] && alias rmdir="rmdir -p --ignore-fail-on-non-empty"
tree2() {
    find . -type d "$@" | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/"
}
alias c="cat"
alias dv="dirs -v"
alias s='sudo'
complete -F _complete_alias s
alias .~='. ~/.bashrc'
alias tm='tmux a -d || tmux'
alias xa='xargs'

find_pi() {
    sudo nmap -sP 192.168.0.0/24 | awk '/^Nmap/{ip=$NF}/B8:27:EB/{print ip}'
}

alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

if [ "$TERM" != "dumb" ]; then
    [ -f ~/.fzf.bash ] && source ~/.fzf.bash
    [ -f /usr/share/fzf/key-bindings.bash ] && . /usr/share/fzf/key-bindings.bash
    [ -f /usr/share/fzf/completion.bash ] && . /usr/share/fzf/completion.bash
    export FZF_DEFAULT_OPTS="-e --bind=ctrl-v:page-down,alt-v:page-up"
fi

# https://the.exa.website
if command -v exa >/dev/null 2>&1; then
    alias l='exa'
    alias ll='l -la'
else
  alias l='ls -F'
  [ `uname` == "Linux" ] && alias l='ls -F --color=auto --group-directories-first'
  alias ll='l -lA -h'
fi

# enable programmable completion features
# worth mentioning: https://github.com/cykerway/complete-alias
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# https://github.com/jml/undistract-me
if [ -f /usr/share/undistract-me/long-running.bash ]; then
    source /usr/share/undistract-me/long-running.bash
    notify_when_long_running_commands_finish_install
fi

# Specific
[ -f ~/.bashrc_specific ] && source ~/.bashrc_specific

# to not be annoying if previous fails
true
