function unescape
  python3 -c "import sys; print(sys.stdin.read().decode('unicode_escape'))"
end
