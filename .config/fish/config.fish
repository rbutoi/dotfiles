# ref: https://fishshell.com/docs/current/language.html#configuration

## paths
fish_add_path -g ~/.local/bin
set -gx GOPATH ~/.local/go
fish_add_path -g ~/.local/go/bin

status is-interactive || exit

# TODO: organize into files once I find a natural order to this.
# also find a way for these not to be indented?

#################
# fish settings #
#################
alias .~='source ~/.config/fish/config.fish'

set -g fish_greeting # none
set -gx EDITOR emacsclient -t

bind \ek backward-kill-line # better than default C-u

#######################
# aliases (broad-ish) #
#######################
alias s sudo
alias chmox 'chmod +x'
abbr -- - cd -
abbr tm tmux new -A -s auto

function md
    mkdir -p $argv[1] && cd $argv[1]
end

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

alias e '_e -nw' # inline console editor
alias en '_e_gui -n' # open in existing editor
alias ec '_e_gui -nc' # new graphical editor

###

if type -q eza
    alias ls=eza
    alias l='ls --group-directories-first'
    alias ll="l --time-style=+'%a %e %b %H:%M' -g -l --git"
    alias la='l -a'
    alias lla='ll -a'
    alias llr='ll --time-style=relative'
end

if type -q bat
    alias c=bat
end

type -q procs && alias psg=procs

function brew --wraps brew # dumps package list to source control
    command brew $argv

    for c in {,un}install
        if test $c = $argv[1]
            fish -c "command brew bundle dump --file=$HOME/.config/Brewfile --force 2>&1 | grep -v renamed" &
            break
        end
    end
end
