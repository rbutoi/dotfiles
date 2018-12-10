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
precmd_functions+=(append_history)

# Tell window name information. It's pretty useless though
# case "$TERM" in
# screen*)
#     tell_screen_info() { echo -ne "\033]0;${PWD##*/}@$HOSTNAME\007"; }
#     precmd_functions+=(tell_screen_info)
#     ;;
# *)
#     ;;
# esac

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

e() {
    [ ! -e "$@" ] && return 1
    emacsclient "$@"
}
ew() {
    [ ! -e "$@" ] && return 1
    emacsclient -nw "$@"
}
en() {
    [ ! -e "$@" ] && return 1
    emacsclient -n "$@"
}
# export EDITOR='emacsclient -nw'
export EDITOR=~/bin/my_editor.sh
export ALTERNATE_EDITOR=zile

# https://github.com/cykerway/complete-alias
[ -f /etc/bash_completion ] && . ~/bin/complete-alias/completions/bash_completion.sh

export CLICOLOR=1
alias l='ls -F'
[ `uname` == "Linux" ] && alias l='ls -F --color=auto --group-directories-first' 
alias ll='l -lA -h'
alias n="nano"
alias z="zile"
alias sz="sudo zile"
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
export LESS=-RMiS
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
alias .~='. ~/.bashrc'
complete -F _complete_alias s

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

# enable programmable completion features
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# https://github.com/jml/undistract-me
# if [ -f /usr/share/undistract-me/long-running.bash ]; then
#     source /usr/share/undistract-me/long-running.bash
#     notify_when_long_running_commands_finish_install
# fi

# Specific
[ -f ~/.bashrc_specific ] && source ~/.bashrc_specific

# to not be annoying if previous fails
true
