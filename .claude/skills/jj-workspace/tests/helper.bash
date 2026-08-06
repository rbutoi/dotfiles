# Shared setup for the ws-* bats suites.
#
# Everything worth testing here is an interaction with jj — which revset a guard actually
# matches, whether a refusal fires, what `jj workspace forget` leaves behind. Mocking jj would
# only test the scripts against themselves, so these drive the real thing.
#
# Each test gets its own repo under $BATS_TEST_TMPDIR, which bats removes afterwards. That
# matters more than it looks: ws-create derives the workspace path as a *sibling* of the repo,
# so putting the repo one level inside the temp dir keeps the workspace in there too. No state
# outside $TMPDIR, no network, and no ordering between tests.

scripts="$(cd "$BATS_TEST_DIRNAME/../scripts" && pwd -P)"

# A fresh repo with two commits and the conventional trailing empty @.
new_repo() {
  local d="$BATS_TEST_TMPDIR/repo"
  mkdir -p "$d"
  (
    cd "$d" &&
      jj git init >/dev/null 2>&1 &&
      printf 'a\n' >a.txt && jj commit -m 'first' >/dev/null 2>&1 &&
      printf 'b\n' >b.txt && jj commit -m 'second' >/dev/null 2>&1
  ) || return 1
  printf '%s' "$d"
}

# new_ws <repo> <name> [commits] -> echoes the workspace path.
# The commit count is the interesting knob: it's what ws-merge.sh's shape rule reads.
new_ws() {
  local repo=$1 name=$2 n=${3:-1} i
  env -C "$repo" "$scripts/ws-create.sh" "$name" >/dev/null 2>&1 || return 1
  local ws="$repo-$name"
  for ((i = 1; i <= n; i++)); do
    (
      cd "$ws" && printf 'w%s\n' "$i" >"w$i.txt" &&
        jj commit -m "ws work $i" >/dev/null 2>&1
    ) || return 1
  done
  printf '%s' "$ws"
}

# The stack root as the scripts' callers would name it, from inside the workspace.
STACK_ROOT='roots(default@-..@)'

# Every commit id in the repo — a cheap fingerprint for "did that command change history?".
history_of() { env -C "$1" jj log --no-pager --no-graph -r 'all()' -T 'change_id.short()'; }
