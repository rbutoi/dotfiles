# ref: https://fishshell.com/docs/current/language.html#configuration

if status is-interactive
  # TODO: organize into files once I find a natural order to this.
  # also find a way for these not to be indented?

  #################
  # fish settings #
  #################
  set -g fish_greeting            # none

  set -gx EDITOR emacsclient -t

  #######################
  # aliases (broad-ish) #
  #######################
  alias tm='tmux new -A -s auto'

  function brew --wraps brew    # dumps package list to source control
    command brew $argv

    for c in {,un}install
      if test $c = $argv[1]
        begin
          fish -c "command brew bundle dump --file=$HOME/.config/Brewfile --force 2>&1 | grep -v renamed" &
          break
        end
      end
    end
  end

end
