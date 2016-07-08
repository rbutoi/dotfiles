# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
mcd() {
    mkdir -p "$@"
    cd "$@"
}
export -f mcd

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

ulimit -c unlimited

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

shopt -s extglob

shopt -s checkwinsize

# Append to the Bash history file, rather than overwriting it
shopt -s histappend

export HISTFILESIZE=
export HISTSIZE=

# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
# Force prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss

case "$TERM" in
screen*)
    PROMPT_COMMAND='echo -ne "\033]0;${PWD##*/}\007"; '
    export PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
    ;;
*)
    ;;
esac

# magic to check if we have colour
colour=0
if tput Co > /dev/null 2>&1
then
    test "`tput Co`" -gt 2 && colour=1
elif tput colors > /dev/null 2>&1
then
    test "`tput colors`" -gt 2 && colour=1
fi

if [ $colour -eq 1  ]; then
    RED="\[$(tput setaf 1)\]"
    GREEN="\[$(tput setaf 2)\]"
    BLUE="\[$(tput setaf 4)\]"
    RESET="\[$(tput sgr0)\]"

    export PS1="${GREEN}\W ${BLUE}\$${RESET} "
else
    export PS1="\W \$ "
fi

# Aliases
e() {
    emacsclient "$@"
}
ew() {
    emacsclient -nw "$@"
}
en() {
    emacsclient -n "$@"
}
export EDITOR="emacsclient -nw"
export ALTERNATE_EDITOR=zile

export CLICOLOR=1
alias l='ls -F'
[ `uname` == "Linux" ] && alias l='ls -F --color=auto --group-directories-first' 
alias ll='l -lA -h'
alias n="nano"
alias g="grep --color=always -i"
alias p="ps aux|grep `whoami`"
m() {
    if [ -f /usr/bin/less ]; then
        less "$@"
    else
        more "$@"
    fi
}
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

case "$TERM" in
screen*)
    if [ -z "$A4_CHROOT" ]; then
        [ -f ~/.fzf.bash ] && source ~/.fzf.bash
    else
        export FZF_TMUX=0
        [ -f ~/.fzf.bash_chroot ] && source ~/.fzf.bash_chroot
    fi
    export FZF_DEFAULT_OPTS="-e --bind=ctrl-v:page-down,alt-v:page-up"
    ;;
*)
    ;;
esac

# Specifics
[ -f ~/.bashrc_arista ] && source ~/.bashrc_arista
