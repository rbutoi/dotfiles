#!/usr/bin/env bats
# Each script's --help is derived from its own header comment rather than a hand-kept line
# range. The range version silently over-printed into the code the first time the comment grew,
# which is what these pin.

load 'helper'

for_each_script() { printf 'ws-create.sh\nws-merge.sh\nws-remove.sh\n'; }

@test "--help stops before the code" {
  while IFS= read -r s; do
    run "$scripts/$s" --help
    [ "$status" -eq 0 ]
    [[ "$output" != *'set -euo pipefail'* ]] || {
      echo "$s leaked its implementation into --help"
      return 1
    }
  done < <(for_each_script)
}

@test "--help strips the comment markers" {
  while IFS= read -r s; do
    run "$scripts/$s" --help
    # Checked per line, not as a substring: a usage line may legitimately carry a trailing
    # "# what this form does" annotation.
    [[ "$output" != '#'* && "$output" != *$'\n#'* ]] || {
      echo "$s left a line beginning with #"
      return 1
    }
  done < <(for_each_script)
}

@test "--help names the command it documents" {
  while IFS= read -r s; do
    run "$scripts/$s" --help
    [[ "$output" == *"$s"* ]] || {
      echo "$s --help never mentions $s"
      return 1
    }
  done < <(for_each_script)
}
