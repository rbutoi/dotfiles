function escape
  python3 -c 'import json, sys; print(json.dumps(sys.stdin.read()))'
end
