function all_atq
  atq | perl -ne 'print "\n"; /^([\d]+).*/ && print $_, qx(at -c $1 | tail -2 | head -1)'
end
