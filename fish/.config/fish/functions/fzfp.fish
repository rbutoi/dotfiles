function fzfp
  fd -tf | fzf --preview bat --style $argv;
end