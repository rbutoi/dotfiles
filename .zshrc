# -*- sh-basic-offset:2 -*-

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

################################################################
#                        Shell features                        #
# options: http://zsh.sourceforge.net/Doc/Release/Options.html #
################################################################

## completion
zstyle ':completion:*' menu yes select
zstyle ':completion:*' use-cache on
zstyle ':completion:*' format '[%d]'

# TODO: doesn't work?
# also: try wizard % autoload -Uz compinstall && compinstall
# zstyle ':completion:*' cache-path "$HOME/.cache/zsh/.zcompcache"

if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

autoload -Uz compinit bashcompinit
compinit
bashcompinit

complete -o nospace -C /opt/homebrew/bin/terraform terraform

HISTFILE=~/.zsh_history # history
HISTSIZE=1000000000     # unlimited
SAVEHIST=$HISTSIZE
# `fc -R` to read history (from other running shells) now. otherwise history is
# preserved per-shell
setopt inc_append_history hist_ignore_space hist_ignore_dups

zinit light romkatv/powerlevel10k # prompt
source_if ~/.p10k.zsh             # `p10k configure` or edit this file
zinit light romkatv/zsh-prompt-benchmark

##
## fzf
##

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh # no mas?
(( $+commands[fzf] )) && source <(fzf --zsh)

export FZF_DEFAULT_OPTS="--bind=ctrl-v:page-down,alt-v:page-up
  --color=fg:#ebdbb2,bg:#282828,hl:#fabd2f,fg+:#ebdbb2,bg+:#3c3836,hl+:#fabd2f
  --color=info:#83a598,prompt:#bdae93,spinner:#fabd2f,pointer:#83a598,marker:#fe8019,header:#665c54"
export FZF_DEFAULT_COMMAND='fd --hidden'
export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
MY_FZF_BIND_FLAGS="--header 'C-o: e {}, C-M-o: en {}' --bind \
  'ctrl-o:execute(emacsclient -nw {1}),ctrl-alt-i:execute(emacsclient -n {1})'"
fzfp()  {
    eval "fzf --preview 'if [[ -f {} ]] && [[ \$(stat -c%s {}) -lt 2000000 ]];
            then clp {}; else eza -laa {}; fi' $MY_FZF_BIND_FLAGS $@" }
enfz()  { en $(fzfp "$@")  }
econf() { enfz . ~/.config }
rg_fzfp() {  # from https://jeskin.net/blog/grep-fzf-clp/
  rg --column "$@" 2>/dev/null | fzf --delimiter=':' -n 2.. --preview-window '+{2}-/2' \
    --preview 'clp -h {2} {1}' --header 'C-o: e {}, C-M-o: en {}' --bind \
    'ctrl-o:execute(emacsclient -nw {1}),ctrl-alt-i:execute(emacsclient -n {1})'
}
apt_search_fzf()  { aptitude search "$@" | rg -v '^v|:i386' | fzf | choose 1; }
apt_install_fzf() { sudo apt install $(apt_search_fzf "$@"); }

zinit light Aloxaf/fzf-tab
zstyle ':fzf-tab:*' prefix ''
# completions and bindings are sourced in .zshrc_specific

# disable sort when completing options of any command
zstyle ':completion:complete:*:options' sort false

zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath' # cd preview

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
##
## end fzf
## TODO: try fzy for some things? or replace with skim?

zinit light zsh-users/zsh-autosuggestions # autosuggestions
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
bindkey '^[m' autosuggest-accept

autoload -U select-word-style # line editing: akin to subword-mode in emacs
select-word-style bash

zinit light zdharma-continuum/fast-syntax-highlighting # syntax highlighting

autoload -U edit-command-line   # editing command line with emacs
zle -N edit-command-line        # emacs style
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

bindkey '^[k' backward-kill-line # not emacs but useful on CLI

setopt auto_pushd auto_cd       # cd tweaks from zshoptions(1)

#######################################
# User specific aliases and functions #
#######################################

source_if ~/.config/path.sh

autoload -Uz zmv

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

fork          () { (setsid "$@" &)                                                                                        ; }
all_atq       () { atq | perl -ne 'print "\n"; /^([\d]+).*/ && print $_, qx(at -c $1 | tail -6 | grep -Ev "echo|exit|}")' ; }
escape        () { python3 -c 'import json, sys; print(json.dumps(sys.stdin.read()))'                                     ; }
unescape      () { python3 -c "import sys; print(sys.stdin.read().encode('utf-8').decode('unicode_escape'))"              ; }
wc_occurrences() { python3 -c 'import collections, sys, pprint; pprint.pprint(collections.Counter(sys.stdin));'           ; }
find_pi       () { sudo nmap -sn 192.168.0.0/24 | awk '/^Nmap/{ip=$NF}/B8:27:EB/{print ip}'                               ; }
alias alert='tput bel; notify-send -u normal -t 60000 -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"; xdg-open "https://www.youtube.com/watch?v=n2_X4VTCoEo"'

alias find_by_date='find . -printf "%T@ %Tc %p\n" | sort -n' # camera

# FILES=(DSCFnnnn.JPG ...)
# pushd /media/disk/DCIM/111_FUJI/ &&
#     rsync --archive --partial --info=progress2 $FILES \
#     /mnt/the_drive/Photos/BLAH/ &&
#     popd

check_missing_camera_files() {
  comm -23                                               \
    <(fd . /media/disk/DCIM      -tf -x basename | sort) \
    <(fd . /mnt/the_drive/Photos -tf -x basename | sort | grep -Ev 'xmp$')
}

fix_i3_or_swaysock() {
  pgrep -x i3 >/dev/null && fix_i3sock || fix_swaysock
}
fix_swaysock () {
  uid=$(id -u)
  export SWAYSOCK=$(ls /run/user/${uid}/sway-ipc.${uid}.*.sock)
  export I3SOCK=$SWAYSOCK  # allow i3 passthrough
}
fix_i3sock () {
  export I3SOCK=/run/user/$(id -u)/i3/ipc-socket.$(pgrep -x i3)
}

sshcd  () { ssh -t $1 "cd \"$2\"; exec \$SHELL -l"; }
sshcd. () { sshcd $1 $(pwd) }

##
## emacs
##
_e() {                          # emacs with stdin
  if [[ -p /dev/stdin ]]; then  # or [[ ! -t 0 ]]
    # tempfile="$(mktemp emacs-stdin-$USER.XXXXXXX --tmpdir)"
    tempfile=/tmp/emacs-stdin-$USER
    cat - > "$tempfile"
    # if stdin, definitely want a local terminal client
    emacsclient -a= --tty                  \
      --eval "(find-file \"$tempfile\")"   \
      --eval '(set-visited-file-name nil)' \
      --eval '(rename-buffer "*stdin*" t)'
  else
    emacsclient -a= "$@"
  fi
}
ew() { _e "$@" -nw;  }             # inline console editor
en() { _e "$@" -n ;  }             # open in existing editor
ec() { _e "$@" -nc;  }             # new graphical editor
alias e=ew

export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR=zile
export BROWSER=xdg-open
export CLICOLOR=1

emacs_systemd_restart() {
  fix_i3_or_swaysock
  systemctl --user restart emacs.service ||
    (pkill -9 emacs && systemctl --user restart emacs.service) &&
      i3-msg -- exec emacsclient -c >/dev/null
}

eman() {                        # (e)macs man
  # no emacs, fallback to man
  pgrep -i emacs >/dev/null || return man "$@"

  # no manpage, print standard man error
  man -w "$@" > /dev/null || return

  if [ $# -eq 0 ]; then
    # `eman` sans arguments starts the client in completion mode
    cmd="(call-interactively 'man)"
  else
    cmd="(man \"$@\")"
  fi

  if [ -n "$INSIDE_EMACS" ]; then
    emacsclient -n  --suppress-output --eval "$cmd"
  else
    emacsclient -nw --suppress-output --eval \
      "(progn $cmd (other-window 1) (delete-other-windows))"
  fi
}
compdef eman=man

###########
# aliases #
###########

l() { ls -F "$@" }
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
export LESS=-RMiF
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
alias ssha='ssh -t a -- tmux new -As auto'
mosh () {
  if [[ "$@" =~ '^a$' ]]; then
    command mosh -p 61736 "$@"
  else
    command mosh "$@"
  fi
}
alias mosha='mosh -p 61736 a -- tmux new -As auto'
alias fd="fd --one-file-system"
alias type='whence -f'  # am too used to bash
nms() { notmuch search "$@" | cut -c24-; }

gh_merge_approved() {
  gh search prs --review approved --author=@me is:open --json url | xargs -n1 gh pr merge --squash
}

gh_approve_and_merge() {
  gh pr review --approve "$@" && gh pr merge --squash "$@"
}

gh_stars() {
  # 269 character width
  echo 'my stars: updated | starred | created'
  gh api --paginate user/starred                                 \
     -H "Accept: application/vnd.github.v3.star+json" --template \
     '{{range .}}{{printf "%-60s" (.repo.html_url) | color "yellow"}} {{printf "⭐%-6.0f" (.repo.stargazers_count) | color "white"}} {{printf "[%10.10s] %-120.120s %16s up %16s ⭐\t from %15s\n" (.repo.language) (.repo.description) (timeago .repo.updated_at) (timeago .starred_at) (timeago .repo.created_at)}}{{end}}'
}

########################
# external shell tools #
########################

if [ -z $CARGO_PATH_SET ]; then
  export PATH=~/.cargo/bin:$PATH
  export CARGO_PATH_SET=1
fi

if [[ -d /home/linuxbrew && -z "$HOMEBREW_PREFIX" ]]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
  (( $+commands[clp] )) || (brew tap jpe90/clp && brew install jpe90/clp/clp)
fi

newbrew() {
  # Include all commands that should do a brew dump
  local dump_commands=('install' 'uninstall')
  local main_command="${1}"

  brew ${@}

  for command in "${dump_commands[@]}"; do
    [[ "${command}" == "${main_command}" ]] &&
      (brew bundle dump --file=${HOME}/.config/Brewfile --force 2>&1 |
         grep -v renamed &)
  done
}
alias brew=newbrew

zinit ice as"program" pick"bin/git-fuzzy"
zinit light bigH/git-fuzzy      # or github.com/wfxr/forgit ?

# BEGIN_KITTY_SHELL_INTEGRATION
if test -e "$HOME/oss/kitty/shell-integration/kitty.zsh"; then source "$HOME/oss/kitty/shell-integration/kitty.zsh"; fi
# END_KITTY_SHELL_INTEGRATION

if (( $+commands[zoxide] )); then
  eval "$(zoxide init zsh)"
fi

if (( $+commands[rg] )); then
  export RIPGREP_CONFIG_PATH=~/.config/ripgreprc
  alias g='rg 2>/dev/null'
  alias gw='g -w'
fi

alias less="TERM=screen-256color less"
if (( $+commands[bat] )); then
  # change TERM for proper italics support in bat and less
  alias bat="TERM=screen-256color bat --italic-text=always"
  alias xargs="TERM=screen-256color xargs" # if it calls bat/less
  alias c=bat
  alias m=less # used to be `most` for a long time
  export BAT_THEME=gruvbox-dark
  export BAT_STYLE=changes,header,rule,numbers,snip
fi

if (( $+commands[delta] )); then
  diff()  { /usr/bin/diff -u "$@" | delta; }
  diffs() { /usr/bin/diff -u "$@" | delta --side-by-side; }
  bdiff() { /usr/bin/diff --color=auto "$@" }
else
  alias diff="diff --color=auto"
fi

if (( $+commands[eza] )); then
  l()  { eza --group-directories-first "$@" }
  ll() { eza --group-directories-first --time-style=+'%a %e %b %H:%M' -g -l --git "$@" }
  alias la='l -aa'
  alias lla='ll -aa'
fi

if [[ -e ~/.config/broot/launcher/bash/br ]]; then
  . ~/.config/broot/launcher/bash/br
   cdb() { br --only-folders --cmd "$*:cd"; }
    lb() { br --sizes --dates --permissions; }
  treb() { br --height $((LINES - 2)) --cmd :pt "$@"; }
fi

(( $+commands[duf] ))   && alias df=duf
(( $+commands[navi] ))  && eval "$(navi widget zsh)" ### !!!!!!
(( $+commands[procs] )) && alias psg="procs --pager disable"

alias dig="dig +nostats +nocomments +nocmd"  # make dig quiet by default

# https://codeberg.org/dnkl/foot/
# set cwd with C-S-n: https://codeberg.org/dnkl/foot/wiki#user-content-spawning-new-terminal-instances-in-the-current-working-directory
function osc7 {
    local LC_ALL=C
    export LC_ALL

    setopt localoptions extendedglob
    input=( ${(s::)PWD} )
    uri=${(j::)input/(#b)([^A-Za-z0-9_.\!~*\'\(\)-\/])/%${(l:2::0:)$(([##16]#match))}}
    print -n "\e]7;file://${HOSTNAME}${uri}\e\\"
}
add-zsh-hook -Uz chpwd osc7

#################
# host-specific #
#################
source_if ~/.zshrc_specific
source_if ~/.config/zsh/zshrc_specific

#  LocalWords:  shellenv

# Local Variables:
# eval: (column-enforce-mode)
# End:

# hmm extras

pupdate() { case ":${PATH:=$1}:" in *:"$1":*) ;; *) PATH="$1:$PATH" ;; esac; }

pupdate ~/.local/bin

export PYTHON_HISTORY="${XDG_STATE_HOME:-$HOME/.local/state}"/python_history
