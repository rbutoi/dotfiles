# ref: https://fishshell.com/docs/current/language.html#configuration

## paths
set -gx GOPATH ~/.local/go
fish_add_path -g ~/.local/bin
fish_add_path -g ~/.local/go/bin
fish_add_path -g ~/.cargo/bin

status is-interactive || exit

# TODO: organize into files once I find a natural order to this.
# also find a way for these not to be indented?

#################
# fish settings #
#################
alias .~='source ~/.config/fish/config.fish'

set -g fish_greeting # none
set -gx EDITOR emacsclient -t

bind alt-k backward-kill-line # better than default C-u

# fzf
set -gx FZF_DEFAULT_OPTS "--bind 'ctrl-v:page-down,alt-v:page-up'"

# https://github.com/PatrickF1/fzf.fish
set fzf_history_time_format %a %b %d %H:%M:%S
fzf_configure_bindings --directory=\ct --variables=\e\cv --history=

# atuin
if type -q atuin
    atuin init fish | source
end

#######################
# aliases (broad-ish) #
#######################
set -gx LESS -im

alias s sudo
alias chmox 'chmod +x'
abbr -- - cd -
abbr tm tmux new -A -s auto

function md
    mkdir -p $argv[1] && cd $argv[1]
end

##################
# external tools #
##################

## emacs
function _e --wraps emacsclient # emacs with stdin
    if test -p /dev/stdin
        # or [[ ! -t 0 ]]
        # tempfile="$(mktemp emacs-stdin-$USER.XXXXXXX --tmpdir)"
        set tempfile '/tmp/emacs-stdin-'"$USER"
        cat - >"$tempfile"
        # if stdin, definitely want a local terminal client
        emacsclient -a= --tty --eval '(find-file "'"$tempfile"'")' --eval '(set-visited-file-name nil)' --eval '(rename-buffer "*stdin*" t)'
    else
        emacsclient -a= $argv
    end
end
function _e_gui --wraps emacsclient
    _e $argv
    set -l a '/Applications/Emacs.app'
    test -e "$a" && open $a
end
# TODO: only emacs has termcap issues?? *ERROR*: Terminal type xterm-kitty is not defined
alias e 'TERM=xterm-256color _e -nw' # inline console editor
alias en '_e_gui -n' # open in existing editor
alias ec '_e_gui -nc' # new graphical editor

if type -q eza
    set -gx EZA_ICONS_AUTO
    alias ls=eza
    alias l='ls --group-directories-first'
    alias ll="l --time-style=+'%a %e %b %H:%M' -g -l --git"
    alias la='l -a'
    alias lla='ll -a'
    alias llr='ll --time-style=relative'
end

if type -q bat
    alias c=bat
    set -gx MANPAGER "sh -c 'col -bx | bat -l man -p'"
    set -gx BAT_STYLE changes,header,header-filename,header-filesize,numbers,snip
end

type -q procs && alias psg=procs

if type -q rg
    alias g=rg
    set -gx RIPGREP_CONFIG_PATH ~/.config/ripgreprc
end

function brew --wraps brew # dumps package list to source control
    command brew $argv

    for c in {,un}install
        if test $c = $argv[1]
            fish -c "command brew bundle dump --file=$HOME/.config/Brewfile --force --describe" &
            break
        end
    end
end

if string match -q "*homebrew*" "$SHELL" # check for macOS by proxy
    set -gx aichat_config_dir ~/.config/aichat
end
