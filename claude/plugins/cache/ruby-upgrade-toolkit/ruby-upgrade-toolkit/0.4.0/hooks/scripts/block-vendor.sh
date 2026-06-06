#!/usr/bin/env bash
# PreToolUse hook: Block writes to vendor/ directory.
# During a Rails upgrade, vendor/ should never be modified directly —
# use bundle update instead.
set -euo pipefail

input=$(cat)
file_path=$(echo "$input" | jq -r '.tool_input.file_path // empty' 2>/dev/null)

# Only applies to Write and Edit tools with a file_path
if [[ -z "$file_path" ]]; then
  exit 0
fi

# Normalize path — resolve relative to project dir if needed
project_dir="${CLAUDE_PROJECT_DIR:-$(pwd)}"
normalized="${file_path#"$project_dir/"}"

if [[ "$normalized" == vendor/* ]] || [[ "$normalized" == */vendor/* ]]; then
  cat <<EOF
{
  "hookSpecificOutput": {
    "permissionDecision": "deny"
  },
  "systemMessage": "BLOCKED: Attempted to modify vendor/ directory. During a Rails upgrade, never edit vendored gems directly. Use 'bundle update <gem>' to update dependencies, then commit the updated Gemfile.lock. File attempted: $file_path"
}
EOF
  exit 0
fi

exit 0
