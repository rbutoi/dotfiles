# .bashrc
# -*- sh-basic-offset:2 -*-

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# set $PATH if not already
[ -f ~/.config/path.sh ] && . ~/.config/path.sh

#########################
# Bash/terminal options #
#########################

ulimit -c unlimited
# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob
shopt -s extglob
shopt -s checkwinsize
stty -ixon # https://unix.stackexchange.com/questions/332791/how-to-permanently-disable-ctrl-s-in-terminal
# Avoid duplicates and leading spaces
export HISTCONTROL=ignoreboth:erasedups
# Append to the Bash history file, rather than overwriting it
shopt -s histappend
export HISTFILESIZE=
export HISTSIZE=
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
set +H # disable history expansion

# prompt + title
eval "$(starship init bash)"
set_win_title() {
  bpwd="$(basename "$PWD")"
  printf "${1:-\033]0;%s\007}" "${bpwd/#$USER/\~}@${HOSTNAME/butoi-/}"
}
set_win_title_tmux() { set_win_title "\033k%s\033"; }
if [ -z "$TMUX" ]; then
  if [ -z "$INSIDE_EMACS" ]; then
    starship_precmd_user_func=set_win_title
  fi
else
  starship_precmd_user_func=set_win_title_tmux
fi
export starship_precmd_user_func

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

mcd           () { mkdir -p "$@" && cd "$@"                                                                     ; }
fork          () { (setsid "$@" &)                                                                              ; }
all_atq       () { atq | perl -ne 'print "\n"; /^([\d]+).*/ && print $_, qx(at -c $1 | tail -2 | head -1)'      ; }
escape        () { python3 -c 'import json, sys; print(json.dumps(sys.stdin.read()))'                           ; }
unescape      () { python3 -c "import sys; print(sys.stdin.read().encode('utf-8').decode('unicode_escape'))"    ; }
pip3_upgrade  () { pip3 install -U "$(pip3 list --outdated | awk 'NR>2 {print $1}')"                            ; }
wc_occurrences() { python3 -c 'import collections, sys, pprint; pprint.pprint(collections.Counter(sys.stdin));' ; }
find_pi       () { sudo nmap -sn 192.168.0.0/24 | awk '/^Nmap/{ip=$NF}/B8:27:EB/{print ip}'                     ; }
fix_swaysock  () { export SWAYSOCK=/run/user/$(id -u)/sway-ipc.$(id -u).$(pgrep -x sway).sock                   ; }
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

# editor
ew() { emacsclient -a= -nw "$@"; }; e() { ew "$@"; }; en() { emacsclient -a= -n  "$@"; }; enc() { emacsclient -a= -nc "$@"; }
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=zile

# man in browser/emacs
man() {
  if ! pidof -q emacs; then
    command man "$@"
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
export BROWSER=xdg-open
export CLICOLOR=1

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
if ! command -v tree >/dev/null; then
  alias tree="ls -R | grep \":$\" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
  tree2() {
    find . -type d "$@" | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/"
  }
fi
alias dv="dirs -v"
alias s='sudo'; complete -F _complete_alias s
alias ss='s ss' # these silently degrade without sudo
alias lsof='s lsof'
alias .~='. ~/.bashrc'
alias tm='tmux new -A -s auto'
alias tenv='eval $(tmux showenv -s)'
alias xa='xargs'
alias ssha='ssh -t a tmux new -ADs auto'
alias mosha='mosh -p 22688 a /home/radu/bin/continuetmux'
alias stow='stow -v' # nice to see the actions taken by default
alias fd="fd --one-file-system"
alias diff="diff --color=auto"

########################
# external shell tools #
########################

if command -v rg >/dev/null; then
  export RIPGREP_CONFIG_PATH=~/.config/ripgreprc
  alias g="rg"
fi

if command -v procs >/dev/null; then
  alias psg=procs
fi

if command -v bat >/dev/null; then
  alias bat="bat --style=changes,header,rule,numbers,snip --wrap=never"
  alias c=bat
  alias m=bat # used to be `most` for a long time
  export PAGER="bat --plain"
  export BAT_THEME=gruvbox-dark
fi

if command -v delta >/dev/null; then
  alias diff=delta
fi

[ -f ~/.fzf.bash ] && . ~/.fzf.bash
[ -d /usr/share/fzf ] && . /usr/share/fzf/completion.bash && \
  . /usr/share/fzf/key-bindings.bash
export FZF_DEFAULT_OPTS="--bind=ctrl-v:page-down,alt-v:page-up
  --color=fg:#ebdbb2,bg:#282828,hl:#fabd2f,fg+:#ebdbb2,bg+:#3c3836,hl+:#fabd2f
  --color=info:#83a598,prompt:#bdae93,spinner:#fabd2f,pointer:#83a598,marker:#fe8019,header:#665c54"
export FZF_DEFAULT_COMMAND='fd --hidden'
export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
alias fzfp="$FZF_DEFAULT_COMMAND -tf | fzf --preview 'bat --style=numbers --color=always {}'"

# enable programmable completion features
# worth mentioning: https://github.com/cykerway/complete-alias
# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion

if command -v exa >/dev/null; then
  alias l='exa';              complete -F _complete_alias l
  alias ll='exa --color always -aagl --git'; complete -F _complete_alias ll
fi

if [ -d ~/.config/broot ]; then
  . ~/.config/broot/launcher/bash/br
   cdb() { br --only-folders --cmd "$*:cd"; }
    lb() { br --sizes --dates --permissions; }
  treb() { br --height $((LINES - 2)) --cmd :pt "$@"; }
fi

command -v duf  >/dev/null && alias df=duf
command -v navi >/dev/null && eval "$(navi widget bash)"

## other hosts

if [ -f ~/.bashrc_specific_mac ] && [ "$(uname)" = "Darwin" ]; then
  . ~/.bashrc_specific_mac
fi

if [ -f ~/.bashrc_specific ]; then
  . ~/.bashrc_specific
fi

# to not be annoying if previous fails
true
