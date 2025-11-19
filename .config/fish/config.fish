# ref: https://fishshell.com/docs/current/language.html#configuration

## paths
fish_add_path -g ~/.local/bin
fish_add_path -g ~/.cargo/bin
set -gx GOPATH ~/.local/go
fish_add_path -g ~/.local/go/bin

# Added by Antigravity
fish_add_path /Users/radu/.antigravity/antigravity/bin

status is-interactive || exit

# TODO: organize into files once I find a natural order to this.

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
fzf_configure_bindings --directory=\ct --variables=\e\cv --history=\e\cr

# atuin
if type -q atuin
    atuin init fish --disable-up-arrow | source
end

#######################
# aliases (broad-ish) #
#######################
set -gx LESS -Rim

alias s sudo
alias chmox 'chmod +x'
abbr -- - cd -
abbr tm tmux new -A -s auto

function md
    mkdir -p $argv[1] && cd $argv[1]
end

function waitpid
    tail --pid=$argv[1] -f /dev/null
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
        emacsclient -a= --tty \
            --eval '(find-file "'"$tempfile"'")' \
            --eval '(set-visited-file-name nil)' \
            --eval '(rename-buffer "*stdin*" t)'
    else
        emacsclient -a= $argv
    end
end
# TODO: only emacs has termcap issues?? *ERROR*: Terminal type xterm-kitty is not defined
alias e 'TERM=xterm-256color _e -nw' # inline console editor
alias en '_e -n' # open in existing editor
alias ec '_e -nc' # new graphical editor

if type -q eza
    set -gx EZA_ICONS_AUTO
    alias ls='eza --group-directories-first'
    alias l='ls --git-ignore'
    alias ll="ls --time-style=+'%a %e %b %H:%M' -g -l --git"
    alias la='ls -a'
    alias lla='ll -a'
    alias llr='ll --time-style=relative'
end

if type -q bat
    alias c=bat
    set -gx MANPAGER "sh -c 'sed -u -e \"s/\\x1B\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'"
    set -gx BAT_STYLE changes,header,header-filename,header-filesize,numbers,snip
end

if type -q procs
    alias psg=procs # historically psg="ps aux | grep"
    alias psgl='procs --use-config large'
end

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

type -q paru && alias p=paru
