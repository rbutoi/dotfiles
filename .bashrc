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
PROMPT_COMMAND='echo -ne "\033]0;${PWD##*/}\007"'
export PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

export EDITOR="emacsclient -s /home/rbutoi/.emacs.d/server/server -nw"
export ALTERNATE_EDITOR=zile
e() {
    emacsclient -s /home/rbutoi/.emacs.d/server/server -n "$@" >/dev/null 2>&1
    which tmux >/dev/null && tmux select-pane -t :0.0 \; select-window -t 0
}
alias l='ls -F --color=auto --group-directories-first'
alias ll='l -lA -h'
alias n="nano"
alias g="grep --color=always -i"
m() {
    [ -f /usr/bin/less ] && less -R -M -i -S "$@" || more "$@"
}
alias xo="xdg-open"
alias xc="xclip -selection clipboard"
alias tree="ls -R | grep \":$\" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
alias rmdir="rmdir -p --ignore-fail-on-non-empty"
tree2() {
    find . -type d "$@" | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/"
}
alias c="cat"
alias dv="dirs -v"

export TMPDIR=$HOME/.tmux-session

if [ -f /lib64/ld-linux-x86-64.so.2 ]; then
    [ -f ~/.fzf.bash ] && source ~/.fzf.bash
else
    # [ -f ~/.fzf.bash_chroot ] && source ~/.fzf.bash_chroot
    :
fi
export FZF_DEFAULT_OPTS="-e --bind=ctrl-v:page-down,alt-v:page-up"

### Specifics
[ -f ~/.bashrc_arista ] && source ~/.bashrc_arista

