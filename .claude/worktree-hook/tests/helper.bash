# Shared setup for the hook's bats suite.
#
# Everything worth testing is an interaction with jj or git — whether a refusal fires, what
# `jj workspace forget` leaves behind. Mocking them would only test the hook against itself, so
# these drive the real tools.
#
# Each test gets its own repo under $BATS_TEST_TMPDIR, which bats removes afterwards. That matters
# more than it looks: the hook derives a jj workspace's path as a *sibling* of the repo, so
# putting the repo one level inside the temp dir keeps the workspace in there too. No state
# outside $TMPDIR, no network, and no ordering between tests.

scripts="$(cd "$BATS_TEST_DIRNAME/.." && pwd -P)"
hook="$scripts/worktree-hook.py"

# The hook gets its own HOME (where it reads settings.json); fixtures keep the real one.
setup() {
  home="$BATS_TEST_TMPDIR/home"
  mkdir -p "$home"
}

# create_in <cwd> <name> / remove_at <path>: the hook as Claude calls it, JSON on stdin.
create_in() { jq -n --arg cwd "$1" --arg name "$2" '{hook_event_name:"WorktreeCreate",cwd:$cwd,name:$name}' | env HOME="$home" "$hook"; }
remove_at() { jq -n --arg p "$1" '{hook_event_name:"WorktreeRemove",worktree_path:$p}' | env HOME="$home" "$hook"; }

# Physical path: git and jj report /private/var where the fixture says /var (macOS tmp).
phys() { (cd "$1" && pwd -P); }

# Where the hook puts the git worktree <name> of <repo>.
git_wt() { printf '%s/.claude/worktrees/%s' "$(phys "$1")" "$2"; }

# The branch checked out in a worktree, and its commit.
branch_of() { git -C "$1" rev-parse --abbrev-ref HEAD; }
head_of() { git -C "$1" rev-parse HEAD; }

# Is there a jj workspace called <name> in <repo>? (Prints the name if so.)
ws_named() { env -C "$1" jj workspace list --no-pager -T "if(name == \"$2\", name)"; }

# plant_debris <path> [content]: a directory holding a file, as a package install leaves behind.
plant_debris() {
  mkdir -p "$1/common/temp"
  printf '%s\n' "${2:-x}" >"$1/common/temp/f.txt"
}

# git with identity and signing pinned: the machine-wide config signs commits, which would block
# on a key prompt here.
gc() { git -c user.name=t -c user.email=t@t -c commit.gpgsign=false "$@"; }

# A fresh jj repo with two commits and the conventional trailing empty @. The optional argument
# names its directory under $BATS_TEST_TMPDIR (a name with a space is a useful one to try).
new_repo() {
  local d="$BATS_TEST_TMPDIR/${1:-repo}"
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
# The commit count says how much committed work the workspace already holds.
new_ws() {
  local repo=$1 name=$2 n=${3:-1} i ws
  ws=$(create_in "$repo" "$name") || return 1
  for ((i = 1; i <= n; i++)); do
    (
      cd "$ws" && printf 'w%s\n' "$i" >"w$i.txt" &&
        jj commit -m "ws work $i" >/dev/null 2>&1
    ) || return 1
  done
  printf '%s' "$ws"
}

# new_git_repo [dir]: a repo with one empty commit.
new_git_repo() {
  local d="$BATS_TEST_TMPDIR/${1:-gitrepo}"
  mkdir -p "$d"
  (cd "$d" && git init -q && gc commit -q --allow-empty -m init) || return 1
  printf '%s' "$d"
}

# A directory that is not a repo of any kind.
new_plain_dir() { mkdir -p "$BATS_TEST_TMPDIR/plain" && printf '%s' "$BATS_TEST_TMPDIR/plain"; }

# `prune` and the nudge as the user and SessionStart run them.
prune_in() { (cd "$1" && env HOME="$home" "$hook" prune "${@:2}"); }
nudge_in() { jq -n --arg cwd "$1" '{cwd:$cwd}' | env HOME="$home" "$hook" nudge; }

# `land` as the user runs it: <dir> <workspace?>.
land_in() { (cd "$1" && env HOME="$home" "$hook" land "${@:2}"); }

# jj reads <dir> without complaint, which it does not for a stale workspace.
assert_not_stale() {
  run env -C "$1" jj --no-pager st
  [ "$status" -eq 0 ]
}

# The descriptions of default's parent and grandparent, nearest first.
below_main() { env -C "$1" jj log --no-pager --no-graph -r 'default@- | default@--' -T 'description.first_line() ++ "\n"'; }

# Put work into the main line the way a rebase integration does: main's working copy is rebased on
# top of `<rev>`.
integrate() { env -C "$1" jj rebase -s 'default@' -d "$2" >/dev/null 2>&1; }

# new_ws <repo> <name> [commits], then integrated into the main line: a finished workspace.
finished_ws() {
  local ws
  ws=$(new_ws "$1" "$2" "${3:-1}") || return 1
  integrate "$1" "$2@-"
  printf '%s' "$ws"
}

# Rebase a workspace's stack from the main checkout, which leaves the workspace stale.
make_stale() { env -C "$1" jj rebase -s "roots(default@-..$2@)" -d 'default@--' >/dev/null 2>&1; }

# A jj repo whose trunk is a real remote's main: `trunk()` resolves to main@origin on its own, so no
# config is written (jj keeps a repo's own config outside the repo, under the real HOME).
new_repo_with_trunk() {
  local repo origin="$BATS_TEST_TMPDIR/origin.git"
  repo=$(new_repo) || return 1
  git init -q --bare -b main "$origin"
  env -C "$repo" jj git remote add origin "$origin" >/dev/null 2>&1
  env -C "$repo" jj bookmark create main -r 'default@-' >/dev/null 2>&1
  env -C "$repo" jj git push --bookmark main >/dev/null 2>&1
  printf '%s' "$repo"
}
