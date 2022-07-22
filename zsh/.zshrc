### .zshrc

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk

source_if() { [[ -e "$@" ]] && source "$@" }
fpath=(~/.local/share/zsh/site-functions ~/.config/zsh $fpath)

##################
# Shell features #
##################

## completion
zstyle ':completion:*' menu yes select
zstyle ':completion:*' use-cache on
zstyle ':completion:*' format '[%d]'

# TODO: doesn't work?
# also: try wizard % autoload -Uz compinstall && compinstall
# zstyle ':completion:*' cache-path "$HOME/.cache/zsh/.zcompcache"

zstyle ':completion:*' users root $USER             #fix lag in google3
autoload -Uz compinit bashcompinit
compinit
bashcompinit

## history
HISTFILE=~/.zsh_history
HISTSIZE=1000000000     # unlimited
SAVEHIST=$HISTSIZE
setopt append_history share_history hist_ignore_space hist_ignore_dups

## prompt
zinit light romkatv/powerlevel10k
source_if ~/.p10k.zsh  # `p10k configure` or edit this file
zinit light romkatv/zsh-prompt-benchmark

## fzf
source_if /usr/share/doc/fzf/examples/key-bindings.zsh
source_if /usr/share/doc/fzf/examples/completion.zsh
zinit light Aloxaf/fzf-tab
zstyle ':fzf-tab:*' prefix ''

# disable sort when completing options of any command
zstyle ':completion:complete:*:options' sort false

zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'  # cd preview

# give a preview of commandline arguments when completing `kill`
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap
compdef pkill=kill
compdef killall=kill

# show systemd unit status
zstyle ':fzf-tab:complete:systemctl-*:*' fzf-preview 'SYSTEMD_COLORS=1 systemctl status $word'

zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
  fzf-preview 'echo ${(P)word}'  # env var

# git
zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
  'git diff $word | delta'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
  'git log --color=always $word'
zstyle ':fzf-tab:complete:git-help:*' fzf-preview \
  'git help $word | bat -plman --color=always'
zstyle ':fzf-tab:complete:git-show:*' fzf-preview \
  'case "$group" in
  "commit tag") git show --color=always $word ;;
  *) git show --color=always $word | delta ;;
  esac'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
  'case "$group" in
  "modified file") git diff $word | delta ;;
  "recent commit object name") git show --color=always $word | delta ;;
  *) git log --color=always $word ;;
  esac'

zstyle ':fzf-tab:complete:tldr:argument-1' fzf-preview 'tldr --color always $word'  # tldr

# fzf-tab preview files, images
zstyle ':fzf-tab:complete:*:*' fzf-preview 'less ${(Q)realpath}'
export LESSOPEN='|~/bin/lessfilter %s'

## autosuggestions
zinit light zsh-users/zsh-autosuggestions
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
bindkey '^[m' autosuggest-accept

## line editing: akin to subword-mode in emacs
autoload -U select-word-style
select-word-style bash

## syntax highlighting
zinit light zdharma-continuum/fast-syntax-highlighting

## editing command line with emacs
autoload -U edit-command-line
zle -N edit-command-line  # Emacs style
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Shell options: http://zsh.sourceforge.net/Doc/Release/Options.html.

#######################################
# User specific aliases and functions #
#######################################

## path!
source_if ~/.config/path.sh

# Autoload functions.
autoload -Uz zmv

# Define functions and completions.
function md() { [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1" }
compdef _directories md

# # Add flags to existing aliases.
# alias ls="${aliases[ls]:-ls} -A"

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
compdef _upto upto

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

ew()  { emacsclient -a= -nw "$@";  }; e() { ew "$@"; } # inline console editor
en()  { emacsclient -a= -n  "$@";  }                   # open in existing editor
ewc() { emacsclient -a= -nc "$@";  }                   # new graphical editor
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=zile
export BROWSER=xdg-open
export CLICOLOR=1

emacs_systemd_restart() {
  set -x
  fix_i3sock
  systemctl --user restart emacs.service ||
    (pkill -9 emacs && systemctl --user restart emacs.service) &&
      i3-msg 'exec emacsclient -c'
  set +x
}

eman() {
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

###########
# aliases #
###########

alias l='ls -F'
ll() { l -lA -h "$@" }
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
if ! (( $+commands[tree] )); then
  alias tree="ls -R | grep \":$\" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
  tree2() {
    find . -type d "$@" | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/"
  }
fi
alias dv="dirs -v"
alias s='sudo'
alias ss='s ss'         # these silently degrade without sudo
alias lsof='s lsof'
alias .~='exec zsh'
alias tm='tmux new -A -s auto'
alias tenv='eval $(tmux showenv -s)'
alias xa='xargs'
alias ssha='ssh -t a tmux new -ADs auto'
alias mosha='mosh -p 55880 a /home/radu/bin/continuetmux'
alias stow='stow -v'    # nice to see the actions taken by default
alias fd="fd --one-file-system"
alias type='whence -f'  # am too used to bash

########################
# external shell tools #
########################

# BEGIN_KITTY_SHELL_INTEGRATION
if test -e "$HOME/oss/kitty/shell-integration/kitty.zsh"; then source "$HOME/oss/kitty/shell-integration/kitty.zsh"; fi
# END_KITTY_SHELL_INTEGRATION

if (( $+commands[rg] )); then
  export RIPGREP_CONFIG_PATH=~/.config/ripgreprc
  alias g=rg
fi

alias less="TERM=screen-256color less"
if (( $+commands[bat] )); then
  # change TERM for proper italics support in bat and less
  alias bat="TERM=screen-256color bat --italic-text=always --wrap=never"
  alias xargs="TERM=screen-256color xargs" # if it calls bat/less
  alias c=bat
  alias m=bat # used to be `most` for a long time
  export PAGER="bat --plain"
  export BAT_THEME=gruvbox-dark
  export BAT_STYLE=changes,header,rule,numbers,snip
fi

if (( $+commands[delta] )); then
  diff() { /usr/bin/diff -u "$@" | delta --side-by-side; }
else
  alias diff="diff --color=auto"
fi

export FZF_DEFAULT_OPTS="--bind=ctrl-v:page-down,alt-v:page-up
  --color=fg:#ebdbb2,bg:#282828,hl:#fabd2f,fg+:#ebdbb2,bg+:#3c3836,hl+:#fabd2f
  --color=info:#83a598,prompt:#bdae93,spinner:#fabd2f,pointer:#83a598,marker:#fe8019,header:#665c54"
export FZF_DEFAULT_COMMAND='fd --hidden'
export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fzfp () { fzf --preview 'bat --style=numbers --color=always {}' }

if (( $+commands[exa] )); then
  alias l='exa --group-directories-first'
  ll()   { exa -l --git "$@"}
  alias la='l -aa'
  alias lla='ll -aa'
fi

if [[ -e ~/.config/broot/launcher/bash/br ]]; then
  . ~/.config/broot/launcher/bash/br
   cdb() { br --only-folders --cmd "$*:cd"; }
    lb() { br --sizes --dates --permissions; }
  treb() { br --height $((LINES - 2)) --cmd :pt "$@"; }
fi

(( $+commands[duf] ))  && alias df=duf
(( $+commands[navi] )) && eval "$(navi widget zsh)" ### !!!!!!
(( $+commands[procs] )) && alias psg=procs

alias dig="dig +nostats +nocomments +nocmd"  # make dig quiet by default

## topgrade every week
if [[ $(($(<~/.cache/last_topgrade) + 604800)) -lt $(date +%s) ]] 2>/dev/null &&
     load_below; then
  echo -e "\033[36mLast topgrade was "$(date -d@$(<~/.cache/last_topgrade))", running now...\033[0m"
  date +%s >~/.cache/last_topgrade
  if [[ -n "$TMUX" ]]; then
    tmux new-window 'echo -e "\033[36mTopgrade...\033[0m"; topgrade; echo press enter to exit...; read'
  else
    # the built-in topgrade --tmux doesn't dedup calls
    tmux new -As topgrade -- 'topgrade; echo press enter to exit...; read'
  fi
fi

#################
# host-specific #
#################
source_if ~/.zshrc_specific
