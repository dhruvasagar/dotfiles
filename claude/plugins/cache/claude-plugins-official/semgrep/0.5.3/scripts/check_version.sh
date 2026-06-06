#!/bin/bash

# Path to required version file
REQUIRED_VERSION_FILE="${CLAUDE_PLUGIN_ROOT}/semgrep-version"
REQUIRED_VERSION=$(cat "$REQUIRED_VERSION_FILE" 2>/dev/null || echo "unknown")

# Get installed Semgrep version
INSTALLED_VERSION=$(semgrep --version 2>/dev/null | head -n1 | awk '{print $1}')

if [ -z "$INSTALLED_VERSION" ]; then
    echo "⚠️  Semgrep not found. Please install Semgrep to use this plugin." >&2
    echo "   Visit: https://github.com/semgrep/mcp-marketplace" >&2
    exit 1
fi

# Simple version comparison (works for semantic versions)
if [ "$(printf '%s\n' "$REQUIRED_VERSION" "$INSTALLED_VERSION" | sort -V | head -n1)" != "$REQUIRED_VERSION" ]; then
    echo "⚠️  Semgrep version mismatch!" >&2
    echo "   Required: >= $REQUIRED_VERSION" >&2
    echo "   Installed: $INSTALLED_VERSION" >&2
    echo "   Please update Semgrep to use this plugin!" >&2
    exit 1
fi

echo "✓ Semgrep $INSTALLED_VERSION (compatible)"