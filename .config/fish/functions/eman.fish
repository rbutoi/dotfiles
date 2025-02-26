function eman --wraps=man
    pgrep -i emacs >/dev/null || return man "$argv"

    if [ (count $argv) -eq 0 ]
        # `eman` sans arguments starts the client in completion mode
        set cmd '(call-interactively \'man)'
    else
        man -w "$argv" >/dev/null || return
        set cmd "(man \"$argv\")"
    end

    if [ -n "$INSIDE_EMACS" ]
        emacsclient -n --suppress-output --eval "$cmd"
    else
        emacsclient -nw --suppress-output --eval '(progn '"$cmd"' (delete-other-windows))'
    end
end
