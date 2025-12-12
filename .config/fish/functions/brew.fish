function brew --wraps brew --description "Brew wrapper that auto-dumps Brewfile on changes"
    command brew $argv

    for c in install uninstall reinstall
        if contains -- $c $argv
            # don't block the shell
            fish -c "command brew bundle dump --file=$HOME/.config/Brewfile --force --describe" &
            break
        end
    end
end
