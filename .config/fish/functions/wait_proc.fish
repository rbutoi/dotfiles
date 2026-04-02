function wait_proc --description 'Wait for any running processes matching a pattern to finish'
    argparse 'm/message=' -- $argv
    or return 1

    set -l pattern $argv[1]
    if test -z "$pattern"
        echo "Usage: wait_proc <pgrep-pattern> [-m message]" >&2
        return 1
    end

    set -l label (test -n "$_flag_message"; and echo $_flag_message; or echo $pattern)

    pgrep -qf $pattern 2>/dev/null
    or return 0

    set -l frames '⠋' '⠙' '⠹' '⠸' '⠼' '⠴' '⠦' '⠧' '⠇' '⠏'
    set -l i 1

    while pgrep -qf $pattern 2>/dev/null
        printf '\r\033[K%s waiting for %s…' $frames[$i] $label
        set i (math "($i % "(count $frames)") + 1")
        sleep 0.5
    end

    printf '\r\033[K✓ %s finished\n' $label
end
