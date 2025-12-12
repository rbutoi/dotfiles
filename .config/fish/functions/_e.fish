function _e --wraps emacsclient --description "Emacsclient wrapper handling stdin and tty"
    if test -p /dev/stdin
        set tempfile '/tmp/emacs-stdin-'"$USER"
        cat - >"$tempfile"

        emacsclient -a= --tty \
            --eval '(find-file "'"$tempfile"'")' \
            --eval '(set-visited-file-name nil)' \
            --eval '(rename-buffer "*stdin*" t)'
    else
        emacsclient -a= $argv
    end
end
