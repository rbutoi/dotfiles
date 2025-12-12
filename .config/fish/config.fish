#########
# paths #
#########

fish_add_path -g ~/.local/bin

fish_add_path -g ~/.cargo/bin

set -gx GOPATH ~/.local/go
fish_add_path -g $GOPATH/bin

# Added by Antigravity
fish_add_path /Users/radu/.antigravity/antigravity/bin

###

status is-interactive || exit

############
# shell UX #
############

set -g fish_greeting ''

set -Ux hydro_color_git green

bind alt-k backward-kill-line # standard readline, better than default C-u

set -gx FZF_DEFAULT_OPTS "--bind 'ctrl-v:page-down,alt-v:page-up'"
set fzf_history_time_format %a %b %d %H:%M:%S
fzf_configure_bindings --directory=\ct --variables=\e\cv
bind \e\cr history-pager # keep original history search

# Atuin (Shell History). TODO: remove?
if type -q atuin
    atuin init fish --disable-up-arrow | source
end

set -gx EDITOR emacsclient -t
set -gx LESS -Rim

###########
# aliases #
###########

abbr -- - cd -
abbr s sudo
abbr chmox 'chmod +x'
alias .~ 'source ~/.config/fish/config.fish'
abbr tm 'tmux new -A -s auto'

# alias e 'TERM=xterm-256color _e -nw' # console
alias e '_e -nw' # console
alias ec '_e -nc' # graphical
alias en '_e -n' # existing

function md --description "Make directory and enter it"
    mkdir -p $argv[1] && cd $argv[1]
end

function waitpid --description "Wait for a specific PID to finish"
    # TODO: macos
    gtail --pid=$argv[1] -f /dev/null
end

#####################
# modern UNIX tools #
#####################

alias ls 'eza --group-directories-first'
alias l 'ls --git-ignore'
alias ll "ls --time-style=+'%a %e %b %H:%M' -g -l --git"
alias la 'ls -a'
alias lla 'll -a'
alias llr 'll --time-style=relative'
set -gx EZA_ICONS_AUTO

alias c bat
set -gx BAT_STYLE changes,header,header-filename,header-filesize,numbers,snip
# Use bat as MANPAGER (strips formatting codes then colorizes)
set -gx MANPAGER "sh -c 'sed -u -e \"s/\\x1B\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'"

alias psg procs
alias psgl 'procs --use-config large'

# alias g rg
set -gx RIPGREP_CONFIG_PATH ~/.config/ripgreprc

###

# TODO: OS specific?
type -q paru && alias p paru

# Detect macOS via Homebrew variable presence or uname
if string match -q "*homebrew*" "$SHELL"; or test (uname) = Darwin
    set -gx aichat_config_dir ~/.config/aichat
end
