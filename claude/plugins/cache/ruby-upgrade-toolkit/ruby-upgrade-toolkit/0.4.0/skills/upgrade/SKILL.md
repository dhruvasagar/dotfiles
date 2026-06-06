---
name: Upgrade Orchestrator
description: Use when the user runs /ruby-upgrade-toolkit:upgrade or wants a fully automated Ruby/Rails upgrade pipeline. Orchestrates plan → audit → phased fixes → verification end-to-end with a live task list. Accepts ruby:X.Y.Z and optional rails:X.Y arguments. Pauses on failure and lets the user investigate before continuing.
argument-hint: "ruby:X.Y.Z [rails:X.Y]"
allowed-tools: Read, Edit, Bash, Glob, Grep, TodoWrite
version: 0.1.0
---

# Upgrade Orchestrator

Run a fully automated, phased Ruby (and optionally Rails) upgrade from start to finish. Maintain a live task list so the user can see exactly what is done, what is in progress, and what is coming next. Pause and surface any failure clearly before asking whether to continue, retry, or abort.

## Argument Parsing

Extract from the user's arguments:
- `ruby:X.Y.Z` — required target Ruby version
- `rails:X.Y` — optional target Rails version (omit sections if not provided)

## Step 1: Detect Current Versions

```bash
ruby -v 2>/dev/null || true
cat .ruby-version 2>/dev/null
grep "^ruby " Gemfile 2>/dev/null
grep -A2 "RUBY VERSION" Gemfile.lock 2>/dev/null
bundle exec rails -v 2>/dev/null || true
grep "gem ['\"]rails['\"]" Gemfile 2>/dev/null
grep "^    rails " Gemfile.lock 2>/dev/null | head -1
git branch --show-current 2>/dev/null
```

Read `Gemfile` and `Gemfile.lock`. Record:
- `CURRENT_RUBY` — current active Ruby version
- `TARGET_RUBY` — from arguments
- `CURRENT_RAILS` — if Rails project (else "none")
- `TARGET_RAILS` — from arguments (else "none")

**Early exit:** If `CURRENT_RUBY == TARGET_RUBY` and (`TARGET_RAILS` is none OR `CURRENT_RAILS == TARGET_RAILS`), print:

```
Already at target versions (Ruby TARGET_RUBY / Rails TARGET_RAILS).
Nothing to do. Run /ruby-upgrade-toolkit:status to confirm readiness.
```

Then stop.

## Step 2: Validate Compatibility

If `rails:` argument given, verify TARGET_RUBY is compatible with TARGET_RAILS:

| Target Ruby | Min Rails | Recommended |
|-------------|-----------|-------------|
| 2.7         | 5.2       | 6.0–6.1     |
| 3.0         | 6.1       | 7.0         |
| 3.1         | 7.0       | 7.0–7.1     |
| 3.2         | 7.0.4     | 7.1         |
| 3.3         | 7.1       | 7.1–7.2     |
| 3.4         | 7.2       | 7.2–8.0     |

If incompatible, stop immediately and tell the user which combinations are valid.

## Step 3: Determine Upgrade Path

If upgrading Ruby across more than one minor version (e.g. 2.7 → 3.3), list each intermediate step:
- 2.7 → 3.0 → 3.1 → 3.2 → 3.3

Each intermediate Ruby version becomes its own phase. The test suite must be green before moving to the next intermediate step.

If upgrading Rails across more than one minor version (e.g. 6.1 → 8.0):
- 6.1 → 7.0 → 7.1 → 8.0

Each intermediate Rails version becomes its own phase.

## Step 4: Build the Task List

Using `TodoWrite`, create the **full concrete task list** before any work begins. Generate one task per sub-phase, substituting the real version numbers from the upgrade path computed in Step 3.

**Example: Ruby 2.7 → 3.3, Rails 6.1 → 8.0 (generates this exact list):**

```
Phase 0 — Prerequisites: test baseline + upgrade branch
Phase 1a — Ruby 3.0.7: activate + version pins + gem updates
Phase 1b — Ruby 3.0.7: code fixes (keyword args, YAML, stdlib)
Phase 1c — Ruby 3.0.7: verify (RSpec + RuboCop + GREEN)
Phase 2a — Ruby 3.1.6: activate + version pins + gem updates
Phase 2b — Ruby 3.1.6: code fixes
Phase 2c — Ruby 3.1.6: verify (RSpec + RuboCop + GREEN)
Phase 3a — Ruby 3.2.4: activate + version pins + gem updates
Phase 3b — Ruby 3.2.4: code fixes
Phase 3c — Ruby 3.2.4: verify (RSpec + RuboCop + GREEN)
Phase 4a — Ruby 3.3.1: activate + version pins + gem updates
Phase 4b — Ruby 3.3.1: code fixes (it-parameter check)
Phase 4c — Ruby 3.3.1: verify (RSpec + RuboCop + GREEN)
Phase 5a — Rails 7.0: gem updates + bin/rails app:update
Phase 5b — Rails 7.0: deprecation fixes + framework defaults
Phase 5c — Rails 7.0: verify (RSpec + RuboCop + GREEN)
Phase 6a — Rails 7.1: gem updates + bin/rails app:update
Phase 6b — Rails 7.1: deprecation fixes + framework defaults
Phase 6c — Rails 7.1: verify (RSpec + RuboCop + GREEN)
Phase 7a — Rails 8.0: gem updates + bin/rails app:update
Phase 7b — Rails 8.0: deprecation fixes + framework defaults
Phase 7c — Rails 8.0: verify (RSpec + RuboCop + GREEN)
Phase 8 — Final verification: full suite + deprecation count + manual checklist
```

**Single-step examples:**
- Ruby 3.2 → 3.3 only → phases 0, 1a/1b/1c, 2 (final)
- Rails 7.0 → 8.0 only → phases 0, 1a/1b/1c (7.1), 2a/2b/2c (8.0), 3 (final)

Omit Rails phases entirely if no `rails:` argument was given. Number phases sequentially based on the actual path — do not use placeholder letters like "X.Y".

Print a summary banner before starting:

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Ruby Upgrade Orchestrator
Ruby:  CURRENT_RUBY → TARGET_RUBY
Rails: CURRENT_RAILS → TARGET_RAILS  (or "Ruby only")
Path:  [list intermediate steps if multi-step]
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Task list created. Starting Phase 0.
```

## Step 5: Phase 0 — Prerequisites

Mark "Phase 0" as in progress in TodoWrite.

### 5a. Test suite baseline

```bash
if [[ -d "spec" ]]; then
  bundle exec rspec --no-color --format progress 2>&1 | tail -10
else
  bundle exec rails test 2>&1 | tail -10 2>/dev/null || echo "No test suite found"
fi
```

Record `BASELINE_FAILURES`. If > 0, report them clearly:

```
⚠️  Pre-existing failures detected: N failures before upgrade begins.
    These are NOT caused by the upgrade and will not be fixed automatically.
    They will be tracked separately throughout the process.
```

Do not abort — continue with the baseline recorded.

### 5b. Intermediate Ruby versions installed (multi-step only)

If the upgrade path crosses more than one Ruby minor version, verify each intermediate version is already installed before starting any work:

```bash
# For each intermediate Ruby version in the path:
rbenv versions 2>/dev/null || rvm list 2>/dev/null
```

If any intermediate Ruby version is missing, stop immediately and list what needs to be installed:

```
⛔ Missing Ruby versions required for this upgrade path.
   Please install them before starting:

     rbenv install 3.0.7
     rbenv install 3.1.6
     rbenv install 3.2.4
     rbenv install 3.3.1

   Then re-run /ruby-upgrade-toolkit:upgrade ruby:TARGET_RUBY [rails:TARGET_RAILS]
```

Do not proceed until all required versions are present.

### 5c. Upgrade branch

```bash
git status --short
git branch --show-current
```

If already on an upgrade branch (e.g. `upgrade/ruby-*`), continue. Otherwise, suggest:

```
Recommended: create a dedicated branch before starting.
  git checkout -b upgrade/ruby-TARGET_RUBY

Proceed on current branch? [yes / no — I'll create the branch first]
```

Wait for confirmation before proceeding.

### 5d. RuboCop baseline

```bash
bundle exec rubocop --parallel --format json 2>/dev/null | python3 -c "
import sys, json
data = json.load(sys.stdin)
offenses = sum(len(f['offenses']) for f in data.get('files', []))
print(f'RuboCop baseline offenses: {offenses}')
" 2>/dev/null || bundle exec rubocop --parallel 2>&1 | tail -3
```

Record `BASELINE_RUBOCOP`.

Mark "Phase 0" complete in TodoWrite. Print:
```
✓ Phase 0 complete — baseline recorded (N RSpec failures, N RuboCop offenses)
```

---

## Step 6: Phase 1 — Ruby Upgrade (repeat for each intermediate version)

For each intermediate Ruby version (or just the target if single-step):

### 6a. Activate Ruby + Version Pins

Mark the current Ruby phase's "activate + version pins + gem updates" task as in progress.

Print:
```
━━━ Phase Xa: Ruby X.Y.Z — activate + version pins ━━━
```

**First: confirm the target Ruby is active.** Before touching any files, run:

```bash
ruby -v
```

If the active Ruby is NOT this phase's target version, stop and tell the user:

```
⛔ Ruby X.Y.Z is not active. Please activate it:
     rbenv local X.Y.Z
   or:
     rvm use X.Y.Z
   Then reply "continue" to resume from here.
```

Wait for "continue" before proceeding. Do not update any files until the correct Ruby is confirmed active.

**Once active, update version pins:**

1. Update `.ruby-version` to the exact target Ruby (e.g. `3.3.1`).
2. Update the `ruby` directive in `Gemfile` to `"~> X.Y"` (minor pin).
3. If `.tool-versions` exists, update the Ruby line.

### 6b. Gem Updates

Print:
```
━━━ Phase 1a (continued): gem updates ━━━
```

```bash
bundle install 2>&1
```

Fix gem incompatibilities iteratively using the error patterns from the fix skill (Step 3 in fix/SKILL.md). Update one gem at a time. For each error:
- "requires Ruby version" → `bundle update <gem>`
- "Could not find compatible versions" → `bundle update <conflicting_gem>`
- native extension failure → `gem pristine <gem>` or reinstall

Print each gem update as it happens:
```
  → Updating nokogiri to 1.16.0 (Ruby 3.2 requirement)
  → bundle install ... OK
```

Do not bulk-update all gems. Update one at a time until `bundle install` exits 0.

Mark "Phase 1a" complete in TodoWrite.

### 6c. Code Fixes

Mark "Phase 1b — Ruby X.Y.Z: code fixes" as in progress.

Print:
```
━━━ Phase 1b: Ruby code fixes ━━━
```

Apply all Ruby version-specific code fixes from the fix skill (Step 4 in fix/SKILL.md). For each category, print what is being checked and what was found:

```
  Checking: keyword argument separation (2.7→3.0)
  Found: 12 potential sites — scanning...
  Fixed: app/services/payment_service.rb:47 (Pattern A)
  Fixed: lib/importers/csv_importer.rb:23 (Pattern B)
  ...

  Checking: YAML.load → YAML.safe_load
  Found: 3 occurrences — fixing...
  Fixed: config/initializers/settings.rb:8

  Checking: `it` block parameter conflict (→3.4)
  Found: 0 occurrences
```

After each file fix, run that file's specs:
```bash
bundle exec rspec spec/path/to/file_spec.rb --no-color 2>&1 | tail -5
```

If the per-file spec fails after a fix, report it immediately:
```
⚠️  Fix introduced a failure in spec/path/to/file_spec.rb
    Error: [error message]
    Attempting to correct...
```

Retry with a corrected fix. If still failing after one retry, pause (see Failure Protocol below).

Mark "Phase 1b" complete in TodoWrite.

### 6d. Verify Ruby Phase

Mark "Phase 1c — Ruby X.Y.Z: verify" as in progress.

Print:
```
━━━ Phase 1c: verification ━━━
```

Run the full verification suite:

```bash
# 1. Confirm Ruby version
ruby -v

# 2. Full RSpec suite
bundle exec rspec --no-color --format progress 2>&1 | tail -15

# 3. RuboCop
bundle exec rubocop --parallel 2>&1 | tail -5

# 4. Deprecation warnings
RAILS_ENV=test bundle exec rspec --no-color 2>&1 | grep -c "DEPRECATION" 2>/dev/null || echo "0"
```

Compute readiness:
- GREEN: tests passing (only pre-existing failures, if any), RuboCop clean
- YELLOW: tests passing but deprecation warnings or RuboCop offenses remain
- RED: new test failures present (above BASELINE_FAILURES)

Print the result:
```
  Ruby: X.Y.Z ✓
  RSpec: N examples, N failures (N pre-existing, 0 new) ✓
  RuboCop: N offenses ✓
  Status: GREEN ✓
```

If RED → invoke Failure Protocol (see below).
If YELLOW → log the warnings, continue to next phase, note in final summary.

Mark "Phase 1c" complete in TodoWrite.

---

## Step 7: Phase 2 — Rails Upgrade (skip entirely if no `rails:` argument)

For each intermediate Rails version (or just the target if single-step):

### 7a. Rails Gem Update + app:update

Mark "Phase 2a — Rails X.Y: gem updates + app:update" as in progress.

Print:
```
━━━ Phase 2a: Rails gem updates ━━━
```

1. Update `gem 'rails', '~> X.Y'` in Gemfile.
2. `bundle update rails 2>&1 | tail -10`
3. Fix any cascading gem conflicts (same pattern as Step 6b).
4. Run `THOR_MERGE=cat bundle exec rails app:update 2>&1`.

For app:update diffs, print what was generated and apply selective changes:
```
  Generated: config/initializers/new_framework_defaults_X_Y.rb
  Updated: config/environments/production.rb (3 changes)
  Skipped: config/application.rb (intentional customization detected)
```

Mark "Phase 2a" complete in TodoWrite.

### 7b. Deprecation Fixes + Framework Defaults

Mark "Phase 2b — Rails X.Y: deprecation fixes + framework defaults" as in progress.

Print:
```
━━━ Phase 2b: Rails deprecation fixes ━━━
```

Apply the Rails deprecation fix table from fix/SKILL.md (Step 5d). For each pattern found, print:
```
  Fixing: update_attributes → update (N occurrences across N files)
  Fixing: before_filter → before_action (N occurrences)
  Fixing: redirect_to :back → redirect_back (N occurrences)
  ...
```

Update `config.load_defaults` in `config/application.rb`.

Update `.rubocop.yml` `AllCops.TargetRubyVersion` to match TARGET_RUBY minor.

For open redirect candidates, print each one and pause for user decision (A/B/C) as specified in fix/SKILL.md Step 5d.

For `has_and_belongs_to_many`, print the migration plan and pause for confirmation before touching anything.

Mark "Phase 2b" complete in TodoWrite.

### 7c. Verify Rails Phase

Mark "Phase 2c — Rails X.Y: verify" as in progress.

Print:
```
━━━ Phase 2c: verification ━━━
```

```bash
bundle exec rspec --no-color --format progress 2>&1 | tail -15
bundle exec rubocop --parallel 2>&1 | tail -5
bundle exec rails runner "puts Rails.version" 2>&1
RAILS_ENV=test bundle exec rspec --no-color 2>&1 | grep -c "DEPRECATION" 2>/dev/null || echo "0"
bundle exec rails zeitwerk:check 2>&1 | head -5
```

Print the result:
```
  Rails: X.Y ✓
  RSpec: N examples, N failures (N pre-existing, 0 new) ✓
  RuboCop: N offenses ✓
  Zeitwerk: OK ✓
  Status: GREEN ✓
```

If RED → invoke Failure Protocol.

Mark "Phase 2c" complete in TodoWrite.

---

## Step 8: Phase 3 — Final Verification

Mark "Phase 3 — Final verification" as in progress.

Print:
```
━━━ Phase 3: final verification ━━━
```

```bash
# Full suite
bundle exec rspec --no-color --format progress 2>&1 | tail -15

# Zero deprecations
DEPRECATIONS=$(RAILS_ENV=test bundle exec rspec --no-color 2>&1 | grep -c "DEPRECATION" || echo 0)
echo "Deprecation warnings: $DEPRECATIONS"

# Zero RuboCop offenses
bundle exec rubocop --parallel 2>&1 | tail -5

# CI/CD files still referencing old Ruby
grep -rn "ruby-${CURRENT_RUBY}" .github/ .circleci/ Jenkinsfile .gitlab-ci.yml 2>/dev/null | head -10

# Dockerfiles
grep -rn "ruby:${CURRENT_RUBY}" Dockerfile* docker-compose* 2>/dev/null | head -10
```

Mark "Phase 3" complete in TodoWrite.

---

## Step 9: Final Summary

Print the complete summary:

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Upgrade Complete
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Ruby:  CURRENT_RUBY → TARGET_RUBY ✓
Rails: CURRENT_RAILS → TARGET_RAILS ✓  (or "not upgraded")

RSpec
  Before: N failures (N pre-existing)
  After:  N failures (all pre-existing — 0 new)

RuboCop
  Before: N offenses
  After:  N offenses

Gems updated: [list]
Files changed: N

━━━ Manual steps still required ━━━
• CI/CD files still referencing Ruby CURRENT_RUBY:
    [list paths]
• Dockerfiles still using old base image:
    [list paths]
• Pre-existing RSpec failures (fix independently): N
• Complex patterns deferred for user review:
    [list any HABTM, open redirect, or other deferred items]

Run /ruby-upgrade-toolkit:status at any time to recheck readiness.
```

---

## Failure Protocol

Invoke this whenever a phase verification returns RED (new failures above baseline), or a fix introduces a regression that cannot be auto-corrected in one retry.

Print:

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
⛔ UPGRADE PAUSED — Phase [name] did not pass verification
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Failures introduced by this phase: N (above the N pre-existing baseline)

[Print the full RSpec failure output for each new failure — file, line, error message, backtrace]

━━━ What you can do ━━━

  A) Investigate and fix  — resolve the failures yourself, then reply "continue"
                             to resume from the verification step of this phase.

  B) Retry this phase     — reply "retry phase [name]" to re-run just this phase's
                             fixes from the beginning (useful if a fix was incomplete).

  C) Abort                — reply "abort" to stop here. All changes made so far
                             are preserved — nothing is rolled back.

Waiting for your decision...
```

**When the user replies "continue":**
Re-run the verification step for the current phase. If now GREEN, mark the phase complete and proceed to the next phase.

**When the user replies "retry phase [name]":**
Re-run that phase's fix step (not the version pin step — only the code/gem fix step) from the beginning, then re-verify.

**When the user replies "abort":**
Print a summary of what was completed and what was not. Do not roll back any changes. Exit.
