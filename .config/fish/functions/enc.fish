function enc
  [ ! -e "$argv" ] && return 1
  emacsclient -a= -nc "$argv"
end
