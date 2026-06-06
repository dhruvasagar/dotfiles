#!/usr/bin/env bash
# PostToolUse hook: Run rubocop --autocorrect on edited Ruby files.
# OPT-IN: Only runs when .ruby-upgrade-toolkit-rubocop file exists in project root.
# To enable: touch .ruby-upgrade-toolkit-rubocop
# To disable: rm .ruby-upgrade-toolkit-rubocop
set -euo pipefail

project_dir="${CLAUDE_PROJECT_DIR:-$(pwd)}"

# Opt-in check — do nothing if flag file is absent
if [[ ! -f "$project_dir/.ruby-upgrade-toolkit-rubocop" ]]; then
  exit 0
fi

input=$(cat)
file_path=$(echo "$input" | jq -r '.tool_input.file_path // empty' 2>/dev/null)

# Only run on Ruby files
if [[ -z "$file_path" ]]; then
  exit 0
fi

if [[ "$file_path" != *.rb ]]; then
  exit 0
fi

# Don't run on spec/test files (avoid autocorrecting test-specific patterns)
normalized="${file_path#"$project_dir/"}"
if [[ "$normalized" == spec/* ]] || [[ "$normalized" == test/* ]]; then
  exit 0
fi

# Ensure rubocop is available
if ! command -v bundle &>/dev/null; then
  exit 0
fi

if ! (cd "$project_dir" && bundle exec rubocop --version &>/dev/null 2>&1); then
  exit 0
fi

# Run safe autocorrect (only Style and Layout cops, no Metrics or Lint)
result=$(cd "$project_dir" && bundle exec rubocop --autocorrect \
  --only "Style,Layout" \
  --format quiet \
  "$file_path" 2>&1) || true

# Only report if corrections were made
if echo "$result" | grep -q "corrected"; then
  corrected=$(echo "$result" | grep -oE '[0-9]+ offenses? corrected' | head -1)
  echo "Rubocop: $corrected in $(basename "$file_path")"
fi

exit 0
