#!/usr/bin/env bats
#
# SKILL.md's frontmatter and its one injected command are executable configuration, not prose:
# Claude Code runs that line during expansion. Both of the mistakes guarded here fail *silently* —
# no error, just a wrong result nobody attributes to this file — which is why they're pinned.

load 'helper'

skill="$BATS_TEST_DIRNAME/../SKILL.md"

# The one `!`command`` line Claude Code runs while expanding SKILL.md. Read via the same grep in
# every test below, so a second injected line would break them loudly rather than be half-checked.
injected_line() { grep -F '!`' "$skill"; }

@test "the injected create command passes \$ARGUMENTS, never \$0" {
  # An indexed placeholder with no matching argument is left verbatim, so a bare `$0` reaches bash
  # as `$0` and expands to the shell's name: `/jj-workspace` with no argument would create a
  # workspace called `bash`. Unsubstituted `$ARGUMENTS` is an unset variable, so it's empty and the
  # script refuses instead. Proven, not assumed:
  run bash -c 'f() { printf "%s" "$1"; }; f $0'
  [ "$output" = bash ]
  run bash -c 'f() { printf "argc=%s" "$#"; }; f $ARGUMENTS'
  [ "$output" = argc=0 ]

  line=$(injected_line)
  [[ "$line" == *'$ARGUMENTS'* ]]
  [[ "$line" != *'$0'* ]]
}

@test "the injected create command merges stderr into stdout" {
  # Every refusal in these scripts goes to stderr, and only "output" is documented as replacing the
  # placeholder. Without 2>&1 a failed create injects an empty block, which reads as a success.
  line=$(injected_line)
  [[ "$line" == *'2>&1'* ]]
}

@test "the injected path matches the allowed-tools rule, so it never prompts" {
  # The permission grant is a glob over the command string; if the two drift apart the skill still
  # works but stops to ask, which defeats running it during expansion.
  rule=$(grep '^allowed-tools:' "$skill")
  [[ "$rule" == *'Bash(${CLAUDE_SKILL_DIR}/scripts/ws-create.sh *)'* ]]
  line=$(injected_line)
  [[ "$line" == *'${CLAUDE_SKILL_DIR}/scripts/ws-create.sh '* ]]
}

@test "model invocation is disabled, because invoking has a side effect" {
  # Expansion creates a directory on disk. Auto-invocation would do that because a task merely
  # sounded parallel.
  grep -q '^disable-model-invocation: true$' "$skill"
}

@test "the script the skill injects exists and is executable" {
  [ -x "$scripts/ws-create.sh" ]
}
