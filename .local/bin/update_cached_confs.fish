#!/usr/bin/env fish

cd ~/.config/fish/conf.d

brew shellenv >/tmp/new_brew.fish
zoxide init fish >/tmp/new_zoxide.fish

function check_changed
    diff -u \
        (perl -ne 'print if $found; $found = 1 if /## cached output/' \
        $argv[1] | psub) $argv[2]
end

if ! check_changed brew.fish /tmp/new_brew.fish
    echo 'test -d /Users || exit 1        # fast way of checking OS

## cached output of `brew shellenv`' >brew.fish
    cat /tmp/new_brew.fish >>brew.fish
end

if ! check_changed zoxide.fish /tmp/new_zoxide.fish
    echo '## cached output of `zoxide init fish`' >zoxide.fish
    cat /tmp/new_zoxide.fish >>zoxide.fish
end

rm /tmp/new_{brew,zoxide}.fish
