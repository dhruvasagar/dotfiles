---
name: Upgrade Status
description: Use when the user runs /ruby-upgrade-toolkit:status or asks for the current upgrade status, upgrade progress report, whether the upgrade is complete, or a summary of what's done and what's remaining. No arguments — detects everything from the project state. Produces a RED/YELLOW/GREEN health report.
argument-hint: "(no arguments)"
allowed-tools: Read, Bash, Glob, Grep
version: 0.2.0
---

# Upgrade Status

Generate a health dashboard for the current state of the upgrade. Run this after each fix phase to confirm readiness before proceeding.

## Step 1: Detect Versions

```bash
ruby -v 2>/dev/null || echo "Ruby: unknown"
cat .ruby-version 2>/dev/null
grep "^ruby " Gemfile 2>/dev/null
bundle exec rails -v 2>/dev/null || echo "Rails: not present"
grep "load_defaults" config/application.rb 2>/dev/null || true
git branch --show-current 2>/dev/null
```

## Step 2: Test Suite

```bash
if [[ -d "spec" ]]; then
  bundle exec rspec --no-color --format progress 2>&1 | tail -10
else
  bundle exec rails test 2>&1 | tail -10 2>/dev/null || echo "No test suite found"
fi
```

## Step 3: Deprecation Warning Count

```bash
if [[ -d "spec" ]]; then
  DEPR=$(RAILS_ENV=test bundle exec rspec --no-color 2>&1 | grep -c "DEPRECATION" || true)
else
  DEPR=$(RAILS_ENV=test bundle exec rails test 2>&1 | grep -c "DEPRECATION" || true)
fi
echo "Deprecation warnings: $DEPR"
```

## Step 4: Ruby Warning Count

```bash
RUBY_WARN=$(RUBYOPT="-W:deprecated" bundle exec ruby -e "require 'bundler/setup'" 2>&1 | grep -c "warning:" || echo 0)
echo "Ruby warnings: $RUBY_WARN"
```

## Step 5: RuboCop Status

```bash
bundle exec rubocop --parallel --format json 2>/dev/null | python3 -c "
import sys, json
data = json.load(sys.stdin)
offenses = sum(len(f['offenses']) for f in data.get('files', []))
print(f'RuboCop offenses: {offenses}')
" 2>/dev/null || bundle exec rubocop --parallel 2>&1 | tail -3
```

## Step 6: Gem Compatibility Signal

```bash
bundle outdated 2>/dev/null | grep -cE "^\s*\*" || echo "0 outdated gems"
```

## Step 7: Zeitwerk (Rails only)

```bash
if [[ -f "config/application.rb" ]]; then
  bundle exec rails zeitwerk:check 2>&1 | grep -E "error|OK|expected" | head -5
fi
```

## Step 8: Render the Report

```
# Upgrade Status Report
Generated: [datetime]
Branch: [branch name]

## Versions
| | Current | Target |
|-|---------|--------|
| Ruby | [current] | [from .ruby-version or user context] |
| Rails | [current] | [from Gemfile or user context] |
| load_defaults | [value] | — |

## Test Suite
- Status: [PASSING / FAILING / NOT FOUND]
- [N] examples, [F] failures, [P] pending

## Warnings
- Deprecation warnings: [N]
- Ruby warnings: [N]
- RuboCop offenses: [N]

## Gem Health
- Outdated gems: [N]

## Zeitwerk (Rails only)
- [OK / N errors]

## Overall Readiness: [RED / YELLOW / GREEN]

GREEN  — Tests passing, 0 deprecation warnings, 0 RuboCop offenses
YELLOW — Tests passing but warnings or offenses remain
RED    — Test failures present — do not proceed to next phase

## Suggested Next Step
[Most actionable next step based on the report]
[Command to run]
```
