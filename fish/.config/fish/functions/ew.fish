function ew
  [ ! -e "$argv" ] && return 1
  emacsclient -a= -nw "$argv"
end
