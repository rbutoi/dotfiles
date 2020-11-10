function en
  [ ! -e "$argv" ] && return 1
  emacsclient -a= -n "$argv"
end
