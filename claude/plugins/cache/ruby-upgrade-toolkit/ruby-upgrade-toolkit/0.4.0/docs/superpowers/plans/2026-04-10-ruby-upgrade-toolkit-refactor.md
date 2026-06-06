# ruby-upgrade-toolkit Plugin Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Refactor `claude-rails-upgrade` into `ruby-upgrade-toolkit` — a unified plugin with 4 namespaced commands (`plan`, `audit`, `fix`, `status`), a canonical `audit → plan → fix → status` workflow, and a context-aware unified auditor agent covering both Ruby and Rails upgrades.

**Architecture:** All plugin content is markdown (skills, commands, agents) and bash (hooks). The refactor is additive-then-destructive: create all new files first, delete old files last, so the plugin is never in a broken state mid-refactor. Skills contain the execution logic; commands are thin entry points that name the argument format and delegate to skills.

**Tech Stack:** Claude Code plugin system (markdown frontmatter + bash scripts), Bundler, RSpec, RuboCop

---

## File Map

### Create
| File | Purpose |
|------|---------|
| `commands/plan.md` | `/ruby-upgrade-toolkit:plan` slash command definition |
| `commands/audit.md` | `/ruby-upgrade-toolkit:audit` slash command definition |
| `commands/fix.md` | `/ruby-upgrade-toolkit:fix` slash command definition |
| `commands/status.md` | `/ruby-upgrade-toolkit:status` slash command definition |
| `skills/plan/SKILL.md` | Unified planning skill (Ruby + optional Rails) |
| `skills/audit/SKILL.md` | Unified audit skill (read-only) |
| `skills/fix/SKILL.md` | Unified fix skill (applies all changes) |
| `skills/status/SKILL.md` | Health dashboard skill |
| `hooks/scripts/ruby-version-sync.sh` | Version consistency validation hook |

### Modify
| File | Change |
|------|--------|
| `.claude-plugin/plugin.json` | Name → `ruby-upgrade-toolkit`, updated description |
| `.claude-plugin/marketplace.json` | Name + description update |
| `skills/rails-upgrade-guide/SKILL.md` | Mark as internal reference, update description so it does not surface as user-facing |
| `agents/upgrade-auditor.md` (currently `rails-upgrade-auditor.md`) | Full rewrite as unified Ruby+Rails auditor |
| `agents/deprecation-fixer.md` | Minor: update references from old skill names to `fix` skill |
| `hooks/hooks.json` | Replace `log-migration.sh` entry with `ruby-version-sync.sh` |
| `README.md` | Full rewrite per design spec section order |

### Delete
| File | Replaced by |
|------|-------------|
| `skills/upgrade-plan/SKILL.md` | `skills/plan/SKILL.md` |
| `skills/deprecation-audit/` (whole dir) | `skills/audit/SKILL.md` |
| `skills/fix-deprecations/` (whole dir) | `skills/fix/SKILL.md` |
| `skills/gem-compatibility-check/` (whole dir) | Absorbed into `audit` + `plan` |
| `skills/migration-safety-check/` (whole dir) | Absorbed into `audit` |
| `skills/config-upgrade/SKILL.md` | Absorbed into `fix` |
| `skills/upgrade-status/SKILL.md` | `skills/status/SKILL.md` |
| `skills/ruby-upgrade-guide/SKILL.md` | Content absorbed into `skills/plan/SKILL.md` |
| `agents/rails-upgrade-auditor.md` | `agents/upgrade-auditor.md` |
| `agents/ruby-upgrade-auditor.md` | `agents/upgrade-auditor.md` |
| `hooks/scripts/log-migration.sh` | `hooks/scripts/ruby-version-sync.sh` |

### Keep unchanged
- `hooks/scripts/block-vendor.sh`
- `hooks/scripts/rubocop-fix.sh`
- `skills/rails-upgrade-guide/references/` (all three reference files)
- `skills/fix-deprecations/references/fix-patterns.md` → move to `skills/rails-upgrade-guide/references/`
- `skills/gem-compatibility-check/references/compatibility-matrix.md` → move to `skills/rails-upgrade-guide/references/`
- `skills/migration-safety-check/references/risky-patterns.md` → move to `skills/rails-upgrade-guide/references/`
- `.github/workflows/rails-upgrade-ci.yml`
- `.gitignore`

---

## Task 1: Update plugin identity files

**Files:**
- Modify: `.claude-plugin/plugin.json`
- Modify: `.claude-plugin/marketplace.json`

- [ ] **Step 1: Update plugin.json**

Write the following content to `.claude-plugin/plugin.json`:

```json
{
  "name": "ruby-upgrade-toolkit",
  "version": "0.2.0",
  "description": "Upgrade Ruby and Rails safely: audit breaking changes, plan a phased roadmap, fix code and dependencies, verify with RSpec and RuboCop.",
  "author": {
    "name": "Dhruva Sagar",
    "url": "https://github.com/dhruvasagar/ruby-upgrade-toolkit"
  },
  "repository": "https://github.com/dhruvasagar/ruby-upgrade-toolkit",
  "license": "MIT",
  "keywords": ["ruby", "rails", "upgrade", "migration", "deprecation", "rubocop", "rspec"]
}
```

- [ ] **Step 2: Update marketplace.json**

Write the following content to `.claude-plugin/marketplace.json`:

```json
{
  "name": "ruby-upgrade-toolkit",
  "owner": {
    "name": "Dhruva Sagar"
  },
  "plugins": [
    {
      "name": "ruby-upgrade-toolkit",
      "source": "./",
      "description": "Upgrade Ruby and Rails safely: audit breaking changes, plan a phased roadmap, fix code and dependencies, verify with RSpec and RuboCop."
    }
  ]
}
```

- [ ] **Step 3: Verify JSON is valid**

```bash
python3 -m json.tool .claude-plugin/plugin.json > /dev/null && echo "plugin.json OK"
python3 -m json.tool .claude-plugin/marketplace.json > /dev/null && echo "marketplace.json OK"
```

Expected: both print `OK`.

- [ ] **Step 4: Commit**

```bash
git add .claude-plugin/plugin.json .claude-plugin/marketplace.json
git commit -m "chore: rename plugin to ruby-upgrade-toolkit"
```

---

## Task 2: Create commands/ directory with 4 command files

**Files:**
- Create: `commands/plan.md`
- Create: `commands/audit.md`
- Create: `commands/fix.md`
- Create: `commands/status.md`

Commands are thin entry points. Their job is to document the argument syntax and delegate execution to the corresponding skill. Claude reads the command file when the user invokes the slash command.

- [ ] **Step 1: Create commands/plan.md**

```markdown
---
name: plan
description: Generate a phased Ruby (and optionally Rails) upgrade roadmap for this project. Detects current versions automatically. Usage: /ruby-upgrade-toolkit:plan ruby:X.Y.Z [rails:X.Y]
argument-hint: "ruby:X.Y.Z [rails:X.Y]"
---

Parse the arguments provided by the user:
- `ruby:X.Y.Z` — required. The target Ruby version to upgrade to.
- `rails:X.Y` — optional. When provided, the plan also covers upgrading Rails to this version.

Current versions are auto-detected — do not ask the user for them.

Use the `ruby-upgrade-toolkit:plan` skill to generate the upgrade plan.
```

- [ ] **Step 2: Create commands/audit.md**

```markdown
---
name: audit
description: Read-only pre-upgrade audit. Surfaces all breaking changes, gem incompatibilities, and deprecations before touching any code. Usage: /ruby-upgrade-toolkit:audit ruby:X.Y.Z [rails:X.Y]
argument-hint: "ruby:X.Y.Z [rails:X.Y]"
---

Parse the arguments provided by the user:
- `ruby:X.Y.Z` — required. The target Ruby version.
- `rails:X.Y` — optional. When provided, the audit also covers Rails upgrade concerns.

Current versions are auto-detected. This command never modifies any file.

Use the `ruby-upgrade-toolkit:audit` skill to run the audit.
```

- [ ] **Step 3: Create commands/fix.md**

```markdown
---
name: fix
description: Apply all upgrade fixes — version pins, gem updates, code changes, RSpec fixes, and RuboCop fixes. Usage: /ruby-upgrade-toolkit:fix ruby:X.Y.Z [rails:X.Y] [scope:path]
argument-hint: "ruby:X.Y.Z [rails:X.Y] [scope:path]"
---

Parse the arguments provided by the user:
- `ruby:X.Y.Z` — required. The target Ruby version.
- `rails:X.Y` — optional. When provided, also applies Rails upgrade fixes.
- `scope:path` — optional. Restricts code fixes to the given file or directory (e.g. `scope:app/models/user.rb` or `scope:app/controllers/`). Gem and version pin changes always apply to the whole project regardless of scope.

Current versions are auto-detected.

Use the `ruby-upgrade-toolkit:fix` skill to apply the fixes.
```

- [ ] **Step 4: Create commands/status.md**

```markdown
---
name: status
description: Show the current upgrade health dashboard — versions, test suite pass/fail, deprecation count, and overall RED/YELLOW/GREEN readiness. No arguments required.
argument-hint: "(no arguments)"
---

No arguments required. Detects everything from the current project state.

Use the `ruby-upgrade-toolkit:status` skill to generate the health report.
```

- [ ] **Step 5: Verify frontmatter is parseable**

```bash
for f in commands/*.md; do
  python3 -c "
import sys
content = open('$f').read()
if not content.startswith('---'):
    print('MISSING FRONTMATTER: $f'); sys.exit(1)
parts = content.split('---', 2)
if len(parts) < 3:
    print('MALFORMED FRONTMATTER: $f'); sys.exit(1)
print('OK: $f')
"
done
```

Expected: `OK:` for all four files.

- [ ] **Step 6: Commit**

```bash
git add commands/
git commit -m "feat: add namespaced command definitions"
```

---

## Task 3: Write skills/plan/SKILL.md

**Files:**
- Create: `skills/plan/SKILL.md`

This skill absorbs `upgrade-plan/SKILL.md` and `ruby-upgrade-guide/SKILL.md`. It generates a phased, project-specific upgrade roadmap — Ruby phases first, Rails phases second (if `rails:` argument given).

- [ ] **Step 1: Create skills/plan/SKILL.md**

```markdown
---
name: Upgrade Plan
description: Use when the user runs /ruby-upgrade-toolkit:plan or asks to plan a Ruby or Rails upgrade, generate an upgrade roadmap, or understand what phases are involved in bumping versions. Accepts ruby:X.Y.Z and optional rails:X.Y arguments. Produces a phased, project-specific upgrade plan with checklists.
argument-hint: "ruby:X.Y.Z [rails:X.Y]"
allowed-tools: Read, Bash, Glob, Grep
version: 0.2.0
---

# Upgrade Plan

Generate a comprehensive, phased upgrade plan tailored to this specific project.

## Argument Parsing

Extract target versions from the arguments:
- `ruby:X.Y.Z` — required target Ruby version
- `rails:X.Y` — optional target Rails version (omit if Ruby-only upgrade)

## Step 1: Detect Current Versions

```bash
# Ruby version
ruby -v 2>/dev/null || true
cat .ruby-version 2>/dev/null
grep "^ruby " Gemfile 2>/dev/null
grep -A2 "RUBY VERSION" Gemfile.lock 2>/dev/null

# Rails version (if present)
bundle exec rails -v 2>/dev/null || true
grep "gem ['\"]rails['\"]" Gemfile 2>/dev/null
grep "^    rails " Gemfile.lock 2>/dev/null | head -1
```

Read `Gemfile` and `Gemfile.lock` to understand current gem versions.
Check for `.ruby-version`, `Gemfile`, `Gemfile.lock`, and `config/application.rb`.

## Step 2: Validate Ruby ↔ Rails Compatibility

If `rails:` argument was given, verify the target Ruby version is compatible with the target Rails version:

| Target Ruby | Minimum Rails | Recommended Rails |
|-------------|--------------|-------------------|
| 2.7         | 5.2          | 6.0–6.1           |
| 3.0         | 6.1          | 7.0               |
| 3.1         | 7.0          | 7.0–7.1           |
| 3.2         | 7.0.4        | 7.1               |
| 3.3         | 7.1          | 7.1–7.2           |
| 3.4         | 7.2          | 7.2–8.0           |

If the combination is incompatible, surface an error before generating the plan.

## Step 3: Identify Upgrade Path

If upgrading Ruby across more than one minor version (e.g. 2.7 → 3.3), list the intermediate steps:
- 2.7 → 3.0 → 3.1 → 3.2 → 3.3
Each intermediate step must have a green test suite before proceeding.

If upgrading Rails across more than one minor version (e.g. 6.1 → 8.0), list the intermediate steps:
- 6.1 → 7.0 → 7.1 → 8.0

## Step 4: Scan for Known Problem Areas

```bash
# Test suite baseline
if [[ -d "spec" ]]; then
  bundle exec rspec --no-color 2>&1 | tail -5
else
  bundle exec rails test 2>&1 | tail -5 2>/dev/null || echo "No test suite detected"
fi

# Gem outdatedness signal
bundle outdated 2>/dev/null | head -30
```

For Ruby 2.7 → 3.0 upgrades, scan for keyword argument issues:
```bash
grep -rn "def .*\*\*[a-z_]*\b" app/ lib/ --include="*.rb" 2>/dev/null | wc -l
RUBYOPT="-W:deprecated" bundle exec ruby -e "puts 'ok'" 2>&1 | head -5
```

For Rails upgrades, load `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/SKILL.md` and its relevant version reference file to identify breaking changes that apply.

## Step 5: Generate the Plan

Output a Markdown-formatted plan with the following structure. Fill in specifics based on what was found in Steps 1–4.

---

### Plan header

```
# Ruby [CURRENT] → [TARGET] Upgrade Plan
# (+ Rails [CURRENT] → [TARGET] if rails: argument given)
Generated: [date]
App: [name from config/application.rb or directory name]
```

### Phase 0: Prerequisites

- [ ] All current tests passing (`bundle exec rspec` or `bundle exec rails test`)
- [ ] Git branch created: `git checkout -b upgrade/ruby-[TARGET]`
- [ ] Current Ruby and Rails versions confirmed (from Step 1)
- [ ] CI pipeline configured to run on the upgrade branch

### Phase 1: Ruby Upgrade — [CURRENT_RUBY] → [TARGET_RUBY]

Repeat this phase for each intermediate Ruby version if multi-step.

#### 1a. Version Pins
- [ ] Update `.ruby-version` to `[TARGET_RUBY]`
- [ ] Update `ruby` directive in `Gemfile` to `"~> [TARGET_RUBY_MINOR]"`
- [ ] Install new Ruby: `rbenv install [TARGET_RUBY]` or `rvm install [TARGET_RUBY]`
- [ ] `bundle install`

#### 1b. Gem Updates for Ruby Compatibility
List each gem that needs a version bump for the target Ruby, with the required version and update command. Focus on gems with native extensions and known Ruby version constraints.

#### 1c. Ruby Version-Specific Code Changes
Based on the target Ruby version, list the code fixes required:

**2.7 → 3.0 (keyword argument separation):**
- Scan: `grep -rn "def .*\*\*" app/ lib/ --include="*.rb"`
- Fix each method where callers pass a plain hash where keywords are expected (add `**` at call site) or where a keyword method is called with `**{}` on a positional-hash method (remove `**`)
- YAML: `grep -rn "YAML\.load\b" app/ lib/ config/ --include="*.rb"` → replace with `YAML.safe_load`

**3.2 → 3.3 (`it` block parameter):**
- Scan: `grep -rn "\bit\b" app/ spec/ --include="*.rb" | grep -v "it ['\"]" | grep -v "#"`
- Rename any `it` variable inside blocks

**3.3 → 3.4 (stdlib gem removals):**
- Check for `require 'base64'`, `require 'csv'`, `require 'drb'`, `require 'mutex_m'`, `require 'nkf'`, `require 'bigdecimal'`
- Add each found library as explicit gem in Gemfile

#### 1d. Verify Phase 1
- [ ] `bundle exec rspec --no-color 2>&1 | tail -5` — must be green
- [ ] `bundle exec rubocop --parallel 2>&1 | tail -5` — must be clean
- [ ] `ruby -v` — confirms target Ruby active
- [ ] Run `/ruby-upgrade-toolkit:status` — must show GREEN

### Phase 2: Rails Upgrade — [CURRENT_RAILS] → [TARGET_RAILS]
*(Omit entirely if no `rails:` argument was given)*

Repeat for each intermediate Rails version if multi-step.

#### 2a. Gem Updates for Rails Compatibility
Cross-reference `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/compatibility-matrix.md`.
List each gem requiring a version bump with the required version and update command.

#### 2b. Rails Version Update
- [ ] Update `gem 'rails', '~> [TARGET_RAILS]'` in Gemfile
- [ ] `bundle update rails`
- [ ] `bin/rails app:update` — review each diff, apply selectively

#### 2c. Framework Defaults
- [ ] Set `config.load_defaults [TARGET_RAILS]` in `config/application.rb`
- [ ] Create `config/initializers/new_framework_defaults_[TARGET]_[MINOR].rb` stub to re-enable any defaults that break the app
- [ ] Enable defaults one at a time, run tests after each

#### 2d. Rails Version-Specific Deprecation Fixes
Based on the Rails version pair, load `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/rails-[X]-to-[Y].md` and list the fixes that apply to this codebase. For each:
- What to grep for
- What to change
- Test command to verify

#### 2e. Verify Phase 2
- [ ] `bundle exec rspec --no-color 2>&1 | tail -5` — must be green
- [ ] `bundle exec rubocop --parallel 2>&1 | tail -5` — must be clean
- [ ] `bundle exec rails runner "puts Rails.version"` — confirms target Rails
- [ ] Run `/ruby-upgrade-toolkit:status` — must show GREEN

### Phase 3: Final Verification

- [ ] Full test suite green
- [ ] Zero deprecation warnings: `RAILS_ENV=test bundle exec rspec 2>&1 | grep -c DEPRECATION || echo 0`
- [ ] Zero RuboCop offenses
- [ ] Staging deploy and smoke test
- [ ] Update CI/CD pipeline Ruby (and Rails) version — **manual step, list file paths**
- [ ] Update Dockerfile Ruby base image — **manual step, list file paths**

---

## Output

Print the complete plan. If the user has a preferred output path, write it to a file. Always include a `## Quick Commands` section at the end with copy-paste bash for each phase.
```

- [ ] **Step 2: Verify frontmatter**

```bash
python3 -c "
content = open('skills/plan/SKILL.md').read()
parts = content.split('---', 2)
assert len(parts) == 3, 'Missing frontmatter'
print('skills/plan/SKILL.md OK')
"
```

Expected: `skills/plan/SKILL.md OK`

- [ ] **Step 3: Commit**

```bash
git add skills/plan/
git commit -m "feat: add unified plan skill"
```

---

## Task 4: Write skills/audit/SKILL.md

**Files:**
- Create: `skills/audit/SKILL.md`

This skill absorbs `deprecation-audit/`, `gem-compatibility-check/`, and `migration-safety-check/`. It is read-only — it never modifies files.

- [ ] **Step 1: Create skills/audit/SKILL.md**

```markdown
---
name: Upgrade Audit
description: Use when the user runs /ruby-upgrade-toolkit:audit or asks to audit their project before an upgrade, find what will break, check gem compatibility, or assess the scope of an upgrade. Read-only — never modifies files. Accepts ruby:X.Y.Z and optional rails:X.Y arguments.
argument-hint: "ruby:X.Y.Z [rails:X.Y]"
allowed-tools: Read, Bash, Glob, Grep
version: 0.2.0
---

# Upgrade Audit

Perform a comprehensive read-only pre-upgrade audit. Never modify any file during this skill.

## Argument Parsing

Extract target versions from the arguments:
- `ruby:X.Y.Z` — required target Ruby version
- `rails:X.Y` — optional target Rails version

## Step 1: Detect Current Versions

```bash
ruby -v 2>/dev/null || true
cat .ruby-version 2>/dev/null
grep "^ruby " Gemfile 2>/dev/null
bundle exec rails -v 2>/dev/null || true
grep "gem ['\"]rails['\"]" Gemfile 2>/dev/null
grep "^    rails " Gemfile.lock 2>/dev/null | head -1
```

Determine whether this is a Rails project: check for `config/application.rb` or `rails` in Gemfile.

## Step 2: Test Suite Baseline

```bash
if [[ -d "spec" ]]; then
  bundle exec rspec --no-color --format progress 2>&1 | tail -10
else
  bundle exec rails test 2>&1 | tail -10 2>/dev/null || echo "No test suite found"
fi
```

Record: pass/fail status, example count, failure count.

## Step 3: Ruby Breaking Changes Audit

Run these checks based on the Ruby version being upgraded FROM and TO.

### 2.7 → 3.0: Keyword argument separation (most impactful change in Ruby 3.x history)

```bash
# Methods accepting **kwargs or options hash
grep -rn "def .*\*\*[a-z_]*" app/ lib/ --include="*.rb" 2>/dev/null | wc -l
grep -rn "def .*[a-z_]* = {}" app/ lib/ --include="*.rb" 2>/dev/null | wc -l

# Call sites with potential hash/keyword mismatch
grep -rn "\*\*options\|\*\*opts\|\*\*params\|\*\*kwargs" app/ --include="*.rb" 2>/dev/null | wc -l

# Ruby 2.7 deprecation warnings preview
RUBYOPT="-W:deprecated" bundle exec rspec --no-color 2>&1 | grep -i "keyword" | sort | uniq -c | sort -rn | head -20
```

### Any version: YAML.load without permitted_classes

```bash
grep -rn "YAML\.load\b\|Psych\.load\b" app/ lib/ config/ --include="*.rb" 2>/dev/null
```

Each match is a potential security issue and will fail in Psych 4 (Ruby 3.1+) with untrusted YAML.

### 3.2 → 3.3: `it` as reserved block parameter

```bash
grep -rn "\bit\b" app/ spec/ --include="*.rb" 2>/dev/null | grep -v "it ['\"]" | grep -v "#" | head -20
```

### 3.3 → 3.4: stdlib gem removals

```bash
for lib in base64 csv drb mutex_m nkf bigdecimal; do
  count=$(grep -rn "require ['\"]$lib['\"]" app/ lib/ --include="*.rb" 2>/dev/null | wc -l)
  [[ $count -gt 0 ]] && echo "$lib: $count occurrences (must add to Gemfile in Ruby 3.4)"
done
```

## Step 4: Rails Deprecation Audit (if Rails present)

Skip this section if no `rails:` argument and no Rails detected.

### Dynamic deprecation capture

```bash
# RSpec
RAILS_ENV=test bundle exec rspec --no-color 2>&1 | grep -E "DEPRECATION|deprecated" | sort | uniq -c | sort -rn | head -30

# Or Minitest
RAILS_ENV=test bundle exec rails test 2>&1 | grep -E "DEPRECATION|deprecated" | sort | uniq -c | sort -rn | head -30
```

### Static pattern scan

```bash
echo "update_attributes: $(grep -rn '\.update_attributes(' app/ --include='*.rb' 2>/dev/null | wc -l)"
echo "before_filter: $(grep -rn 'before_filter\|after_filter\|around_filter' app/ --include='*.rb' 2>/dev/null | wc -l)"
echo "redirect_to :back: $(grep -rn 'redirect_to :back' app/ --include='*.rb' 2>/dev/null | wc -l)"
echo "require_dependency: $(grep -rn 'require_dependency' app/ lib/ --include='*.rb' 2>/dev/null | wc -l)"
echo "HABTM: $(grep -rn 'has_and_belongs_to_many' app/ --include='*.rb' 2>/dev/null | wc -l)"
echo "old enum syntax: $(grep -rn '^ *enum [a-z_]*:' app/ --include='*.rb' 2>/dev/null | wc -l)"
echo "open redirect risk: $(grep -rn 'redirect_to.*params\[' app/controllers/ --include='*.rb' 2>/dev/null | wc -l)"
echo "render text:: $(grep -rn 'render text:' app/ --include='*.rb' 2>/dev/null | wc -l)"
```

### Zeitwerk check (Rails 6+)

```bash
bundle exec rails zeitwerk:check 2>&1 | head -20
```

## Step 5: Gem Compatibility Audit

```bash
# All gems with current versions
grep -E "^    [a-z]" Gemfile.lock | sed 's/^ *//' | sort > /tmp/rut-current-gems.txt
cat /tmp/rut-current-gems.txt | head -50

# Check for outdated gems
bundle outdated 2>/dev/null | head -30
```

Cross-reference against `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/compatibility-matrix.md` for Rails-adjacent gems.

For Ruby version compatibility, flag gems with native extensions — these must be reinstalled for the new Ruby binary:
```bash
bundle exec gem list | grep -E "json|nokogiri|pg|mysql2|bcrypt|ffi|msgpack|oj|puma|nio4r|redcarpet|sassc" 2>/dev/null
```

## Step 6: Migration Safety Audit (Rails only)

Skip if no Rails detected.

```bash
# Pending migrations
bundle exec rails db:migrate:status 2>/dev/null | grep "^  down" | head -10

# Risky migration patterns
grep -rn "execute\|remove_column\|drop_table\|rename_column\|change_column" db/migrate/ --include="*.rb" 2>/dev/null | grep -v "reversible\|# safe" | wc -l

# Concurrent index safety
grep -rn "add_index" db/migrate/ --include="*.rb" 2>/dev/null | grep -v "algorithm: :concurrently" | wc -l

# Database adapter
grep -E "adapter:" config/database.yml 2>/dev/null | head -3
```

Load `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/risky-patterns.md` for detailed pattern guidance.

## Step 7: RuboCop Target Version Gap

```bash
grep "TargetRubyVersion" .rubocop.yml 2>/dev/null || echo "TargetRubyVersion not set"
bundle exec rubocop --version 2>/dev/null
```

If `TargetRubyVersion` is below the target Ruby version, new cops will fire after the upgrade. Flag this.

## Step 8: Produce the Findings Report

```
# Ruby Upgrade Audit Report
Date: [date]
Current: Ruby [X.Y.Z] / Rails [X.Y] (or "no Rails detected")
Target:  Ruby [X.Y.Z] / Rails [X.Y] (or "Ruby only")
Upgrade Path: [list intermediate steps if multi-version]

## Test Suite Baseline
- Status: [PASSING / FAILING / NOT FOUND]
- [N] examples, [F] failures

## Critical Issues (will break on target version)
### Keyword Argument Mismatches (Ruby 3.0)
- [N] methods with **kwargs or opts={} patterns
- [N] call sites with potential mismatch
- Preview warnings from Ruby 2.7: [N found / 0]

### Unsafe YAML.load calls
- [N] occurrences across [N] files: [list files]

### Stdlib Removals (Ruby 3.4 only)
- [list: gem_name: N occurrences]

### `it` Variable Conflicts (Ruby 3.3)
- [N] potential conflicts

## Rails Deprecations (if Rails)
### Dynamic Warnings
[N] unique deprecation patterns:
[list top 5 by count]

### Static Pattern Counts
| Pattern | Count |
|---------|-------|
| update_attributes | N |
| before_filter | N |
| redirect_to :back | N |
| require_dependency | N |
| HABTM | N |
| old enum syntax | N |
| open redirect risk | N |

### Zeitwerk
[OK / N errors]

## Gem Compatibility
### Must Update (for target Ruby/Rails version)
| Gem | Current | Required | Action |
|-----|---------|----------|--------|

### Needs Investigation
- [list]

### Native Extension Gems (require reinstall for new Ruby)
- [list]

## Migration Safety (Rails only)
- Pending migrations: [N]
- Risky patterns found: [N]
- Non-concurrent indexes: [N]
- Database: [adapter]

## RuboCop
- Current TargetRubyVersion: [X.Y or not set]
- Target Ruby: [X.Y]
- Gap: [none / update .rubocop.yml AllCops.TargetRubyVersion]

## Effort Estimate
Low: no keyword arg issues, <5 gems to update, no Rails migration needed
Medium: <30 keyword arg sites, common gems need version bumps
High: >30 keyword arg sites, Rails upgrade also required, or gem with no compatible release

Overall: [Low / Medium / High]

## Recommended Next Steps
1. Run `/ruby-upgrade-toolkit:plan ruby:[TARGET] [rails:[TARGET]]` to generate the phased roadmap
2. [Most critical issue to fix first]
3. [Second priority]
```
```

- [ ] **Step 2: Verify frontmatter**

```bash
python3 -c "
content = open('skills/audit/SKILL.md').read()
parts = content.split('---', 2)
assert len(parts) == 3, 'Missing frontmatter'
print('skills/audit/SKILL.md OK')
"
```

Expected: `skills/audit/SKILL.md OK`

- [ ] **Step 3: Commit**

```bash
git add skills/audit/
git commit -m "feat: add unified audit skill"
```

---

## Task 5: Write skills/fix/SKILL.md

**Files:**
- Create: `skills/fix/SKILL.md`

This skill absorbs `fix-deprecations/`, `config-upgrade/`, and the actionable content from `ruby-upgrade-guide/`. It is the primary execution skill — it applies all changes, runs RSpec iteratively, runs RuboCop iteratively, and reports.

- [ ] **Step 1: Create skills/fix/SKILL.md**

```markdown
---
name: Upgrade Fix
description: Use when the user runs /ruby-upgrade-toolkit:fix or asks to apply upgrade fixes, bump Ruby/Rails versions, fix deprecations, fix RSpec failures after upgrading, or fix RuboCop issues. Accepts ruby:X.Y.Z, optional rails:X.Y, and optional scope:path arguments. Applies all changes and iterates until RSpec and RuboCop are green.
argument-hint: "ruby:X.Y.Z [rails:X.Y] [scope:path]"
allowed-tools: Read, Edit, Bash, Glob, Grep
version: 0.2.0
---

# Upgrade Fix

Apply all upgrade changes end-to-end: version pins, gem dependencies, code fixes, Rails config (if Rails), iterative RSpec green, iterative RuboCop green.

## Argument Parsing

Extract from arguments:
- `ruby:X.Y.Z` — required target Ruby version
- `rails:X.Y` — optional target Rails version
- `scope:path` — optional; restricts code-level fixes to this file or directory. Gem and version pin changes always apply project-wide.

## Step 1: Detect Current Versions

```bash
ruby -v 2>/dev/null || true
cat .ruby-version 2>/dev/null
grep "^ruby " Gemfile 2>/dev/null
bundle exec rails -v 2>/dev/null || true
```

Read `Gemfile` and `Gemfile.lock`.

## Step 2: Update Ruby Version Pins

1. Update `.ruby-version` to the exact target Ruby version (e.g. `3.3.1`).
2. Update the `ruby` directive in `Gemfile` to `"~> X.Y"` (minor version pin).
3. If `.tool-versions` exists, update the Ruby line.

```bash
# Confirm new Ruby is installed (user must have done this — if not, stop and instruct them)
ruby -v
# Expected output starts with: ruby X.Y.Z
```

If the active Ruby is not the target version, stop and tell the user:
> "Please install Ruby X.Y.Z and activate it (`rbenv use X.Y.Z` or `rvm use X.Y.Z`) then re-run this command."

## Step 3: Fix Gem Incompatibilities

```bash
bundle install 2>&1 | tail -20
```

If `bundle install` fails, identify each incompatible gem and update it:

```bash
# Update a single gem conservatively
bundle update <gem_name> 2>&1 | tail -10

# After each update, re-run bundle install to check for cascading issues
bundle install 2>&1 | tail -5
```

For Ruby 3.4+, add any missing stdlib gems to Gemfile that were found during audit:
```ruby
# Add to Gemfile under a comment:
# Ruby 3.4 stdlib removals — now separate gems
gem "base64"      # if used
gem "csv"         # if used
gem "bigdecimal"  # if used
```

Do not bulk-update all gems. Update one gem at a time until `bundle install` is clean.

## Step 4: Apply Ruby Version-Specific Code Fixes

If `scope:` is given, restrict file searches to that path. Otherwise search `app/`, `lib/`.

### 2.7 → 3.0: Keyword argument separation

For each method identified with a keyword argument mismatch:

**Pattern A — hash passed where keywords expected:**
```ruby
# BEFORE: method signature uses **kwargs or keyword params
# Caller passes a plain hash as last positional arg
options = { timeout: 30 }
connect(options)       # ArgumentError in Ruby 3.0

# AFTER: double-splat at call site
connect(**options)
```

**Pattern B — keywords passed to method expecting positional hash:**
```ruby
# BEFORE: double-splat on a method that takes options = {}
process(**{key: "value"})  # ArgumentError in Ruby 3.0

# AFTER: drop the double-splat
process(key: "value")
```

Read each affected file, identify which pattern applies, and apply the minimal fix. Run the file's tests after each file:
```bash
bundle exec rspec spec/path/to/file_spec.rb --no-color 2>&1 | tail -5
```

### Any version: YAML.load → YAML.safe_load

```bash
grep -rn "YAML\.load\b\|Psych\.load\b" ${SCOPE:-app/ lib/ config/} --include="*.rb"
```

For each occurrence, replace:
```ruby
# BEFORE
data = YAML.load(content)

# AFTER (no custom classes needed)
data = YAML.safe_load(content)

# AFTER (custom classes needed — use permitted_classes)
data = YAML.safe_load(content, permitted_classes: [Date, Symbol, MyClass])
```

### 3.2 → 3.3: `it` block parameter conflict

```bash
grep -rn "\bit\b" ${SCOPE:-app/ spec/} --include="*.rb" | grep -v "it ['\"]" | grep -v "#"
```

For each match inside a block body, rename `it` to a descriptive variable name. Read the surrounding context before renaming.

### 3.3 → 3.4: stdlib gem removals

If gems were added to Gemfile in Step 3, update any `require` statements to confirm they still work:
```bash
bundle exec ruby -e "require 'base64'; puts 'ok'" 2>&1
```

## Step 5: Apply Rails Fixes (if `rails:` argument given)

Skip this entire section if no `rails:` argument was provided.

### 5a. Update Rails gem

```bash
# Update Gemfile rails pin
# gem 'rails', '~> X.Y'
bundle update rails 2>&1 | tail -10
```

### 5b. Run bin/rails app:update

```bash
THOR_MERGE=cat bundle exec rails app:update 2>&1
```

Review each generated diff. Apply changes that:
- Add new configuration options
- Update deprecated option names
- Remove options that no longer exist

Do NOT blindly accept diffs that override intentional customizations.

### 5c. Update framework defaults

In `config/application.rb`, update:
```ruby
config.load_defaults [TARGET_RAILS_VERSION]
```

Create a stub initializer to re-enable any defaults that break the test suite:
```ruby
# config/initializers/new_framework_defaults_X_Y.rb
# Re-enable old defaults here as the app is updated to handle new ones
# Rails.application.config.old_default = old_value
```

### 5d. Apply Rails deprecation fixes

Load `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/fix-patterns.md` for the full pattern table.

Apply these safe auto-fixes across the scope (or whole app/ if no scope):

| Pattern | Fix |
|---------|-----|
| `.update_attributes(` | → `.update(` |
| `before_filter` | → `before_action` |
| `after_filter` | → `after_action` |
| `around_filter` | → `around_action` |
| `redirect_to :back` | → `redirect_back(fallback_location: root_path)` |
| `render text:` | → `render plain:` |
| `require_dependency` | remove the line |
| `find_by_<column>(` | → `find_by(<column>:` |
| `enum status: [` | → `enum :status, [` |
| `enum status: {` | → `enum :status, {` |
| `response.success?` | → `response.successful?` |
| `scope :name, where(` | → `scope :name, -> { where(` + closing `}` |

For each file modified, read it first, apply the fixes precisely (only the deprecated patterns, nothing else), then run the file's tests:
```bash
bundle exec rspec spec/corresponding/file_spec.rb --no-color 2>&1 | tail -5
```

For complex patterns (`redirect_to params[...]`, `has_and_belongs_to_many`), present the issue to the user and ask for confirmation before changing.

### 5e. Update RuboCop target version

In `.rubocop.yml`, ensure `AllCops.TargetRubyVersion` matches the target Ruby version:
```yaml
AllCops:
  TargetRubyVersion: X.Y  # match target Ruby minor version
  NewCops: enable
```

## Step 6: Iterative RSpec Loop

```bash
bundle exec rspec --no-color --format progress 2>&1 | tail -30
```

For each failure:
1. Read the error and backtrace.
2. Read the failing spec and the source file it tests.
3. Determine: Ruby breaking change, gem API change, Rails change, or pre-existing bug?
4. Apply the minimal fix to the source file (not the spec, unless the spec itself uses a deprecated pattern).
5. Verify the fix:
   ```bash
   bundle exec rspec spec/path/to/failing_spec.rb --no-color 2>&1 | tail -5
   ```
6. Repeat until the full suite is green.

If a failure is caused by an incompatible gem version, update that gem:
```bash
bundle update <gem_name>
bundle exec rspec spec/path/to/failing_spec.rb --no-color 2>&1 | tail -5
```

If a failure cannot be traced to the upgrade (pre-existing bug), document it in the summary and do not attempt to fix it.

## Step 7: Iterative RuboCop Loop

```bash
bundle exec rubocop --parallel 2>&1 | tail -20
```

First, apply safe auto-corrections:
```bash
bundle exec rubocop -a 2>&1 | tail -10
```

Then review and apply unsafe auto-corrections:
```bash
bundle exec rubocop -A 2>&1 | tail -10
```

For remaining offenses, fix each one manually:
1. Read the file and the offense message.
2. Apply the minimal fix using Edit.
3. Verify:
   ```bash
   bundle exec rubocop path/to/file.rb 2>&1
   ```

If RuboCop itself is outdated for the new Ruby:
```bash
bundle update rubocop rubocop-rails rubocop-rspec rubocop-performance
```

## Step 8: Produce Summary

```
## Upgrade Fix Summary
Date: [date]
Ruby: [old] → [new]
Rails: [old] → [new] (or "not upgraded")
Scope: [full project / path/to/scope]

### Version Pins Updated
- .ruby-version: [old] → [new]
- Gemfile ruby directive: updated
- [.tool-versions: updated / not present]

### Gem Updates
| Gem | Old Version | New Version |
|-----|-------------|-------------|

### Ruby Code Changes
- Keyword argument fixes: [N] files, [N] occurrences
- YAML.load → safe_load: [N] occurrences
- [it variable renames: N]
- [stdlib gem additions: list]

### Rails Changes (if applicable)
- Deprecation fixes: [N] files, [N] patterns fixed
- Config updates: [list files changed]
- Framework defaults: load_defaults updated to [X.Y]

### RSpec
- Before: [N] failures
- After: [N] failures (should be 0)

### RuboCop
- Before: [N] offenses
- After: [N] offenses (should be 0)

### Manual Action Required
- CI/CD files still referencing old Ruby: [list paths]
- Dockerfiles still using old base image: [list paths]
- Pre-existing RSpec failures (not upgrade-related): [N]
- Complex patterns deferred for user review: [list]
```
```

- [ ] **Step 2: Verify frontmatter**

```bash
python3 -c "
content = open('skills/fix/SKILL.md').read()
parts = content.split('---', 2)
assert len(parts) == 3, 'Missing frontmatter'
print('skills/fix/SKILL.md OK')
"
```

Expected: `skills/fix/SKILL.md OK`

- [ ] **Step 3: Commit**

```bash
git add skills/fix/
git commit -m "feat: add unified fix skill"
```

---

## Task 6: Write skills/status/SKILL.md

**Files:**
- Create: `skills/status/SKILL.md`

This skill absorbs `upgrade-status/SKILL.md`. It is the checkpoint skill — run it after each fix phase to confirm readiness before proceeding.

- [ ] **Step 1: Create skills/status/SKILL.md**

```markdown
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
  DEPR=$(RAILS_ENV=test bundle exec rspec --no-color 2>&1 | grep -c "DEPRECATION" 2>/dev/null || echo 0)
else
  DEPR=$(RAILS_ENV=test bundle exec rails test 2>&1 | grep -c "DEPRECATION" 2>/dev/null || echo 0)
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
offenses = sum(f['offenses'].__len__() for f in data.get('files', []))
print(f'RuboCop offenses: {offenses}')
" 2>/dev/null || bundle exec rubocop --parallel 2>&1 | tail -3
```

## Step 6: Gem Compatibility Signal

```bash
bundle outdated 2>/dev/null | grep -c "^\s\*" || echo "0 outdated gems"
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
```

- [ ] **Step 2: Verify frontmatter**

```bash
python3 -c "
content = open('skills/status/SKILL.md').read()
parts = content.split('---', 2)
assert len(parts) == 3, 'Missing frontmatter'
print('skills/status/SKILL.md OK')
"
```

Expected: `skills/status/SKILL.md OK`

- [ ] **Step 3: Commit**

```bash
git add skills/status/
git commit -m "feat: add status skill"
```

---

## Task 7: Update skills/rails-upgrade-guide/SKILL.md

**Files:**
- Modify: `skills/rails-upgrade-guide/SKILL.md`

Mark this skill as an internal reference. It should not surface as a user-facing skill — it exists to provide context to the `audit` and `fix` skills when they load it. Update the description so Claude does not auto-activate it.

- [ ] **Step 1: Rewrite skills/rails-upgrade-guide/SKILL.md**

```markdown
---
name: Rails Upgrade Guide (Internal Reference)
description: Internal reference skill loaded by audit and fix skills for version-specific Rails breaking changes and fix patterns. Not a user-facing skill — do not activate this directly. Only load it when explicitly referenced by another skill in this plugin.
argument-hint: "(internal reference — not user-invocable)"
allowed-tools: Read
version: 0.2.0
---

# Rails Upgrade Guide — Internal Reference

This skill is an internal reference for the `ruby-upgrade-toolkit` plugin. It is loaded by the `audit` and `fix` skills when they need version-specific Rails guidance. Do not activate this skill directly in response to user requests.

## Version Reference Files

Load the appropriate file from `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/` for the upgrade path being processed:

- **Rails 5 → 6**: `references/rails-5-to-6.md`
- **Rails 6 → 7**: `references/rails-6-to-7.md`
- **Rails 7 → 8**: `references/rails-7-to-8.md`

## Supporting Reference Files

- **Fix patterns**: `references/fix-patterns.md` — safe vs. guided deprecation fixes by Rails version
- **Compatibility matrix**: `references/compatibility-matrix.md` — minimum gem versions per Rails version
- **Risky migration patterns**: `references/risky-patterns.md` — database migration safety guidance
```

- [ ] **Step 2: Move reference files into rails-upgrade-guide/references/**

The fix-patterns, compatibility-matrix, and risky-patterns files currently live in their own skill directories. Move them so they are co-located with the rails-upgrade-guide reference files that the audit and fix skills load.

```bash
cp skills/fix-deprecations/references/fix-patterns.md skills/rails-upgrade-guide/references/fix-patterns.md
cp skills/gem-compatibility-check/references/compatibility-matrix.md skills/rails-upgrade-guide/references/compatibility-matrix.md
cp skills/migration-safety-check/references/risky-patterns.md skills/rails-upgrade-guide/references/risky-patterns.md
```

- [ ] **Step 3: Verify files are in place**

```bash
ls skills/rails-upgrade-guide/references/
```

Expected output includes: `fix-patterns.md`, `compatibility-matrix.md`, `risky-patterns.md`, `rails-5-to-6.md`, `rails-6-to-7.md`, `rails-7-to-8.md`

- [ ] **Step 4: Commit**

```bash
git add skills/rails-upgrade-guide/
git commit -m "refactor: consolidate reference files into rails-upgrade-guide"
```

---

## Task 8: Rewrite agents/upgrade-auditor.md

**Files:**
- Modify: `agents/rails-upgrade-auditor.md` → rename and rewrite as `agents/upgrade-auditor.md`

This unified agent replaces both `rails-upgrade-auditor.md` and `ruby-upgrade-auditor.md`. It detects project type automatically and runs the appropriate audit scope.

- [ ] **Step 1: Create agents/upgrade-auditor.md**

```markdown
---
name: upgrade-auditor
description: Use this agent when a user describes an upgrade intent in natural language — e.g. "I need to upgrade Ruby to 3.3", "upgrade this app from Rails 7 to 8", "what would it take to get to Ruby 3.2?", "how bad is our deprecation situation?", or "audit my app before upgrading". Detects whether the project is Rails or plain Ruby and runs an appropriate read-only audit. Produces a prioritized findings report. Examples:

<example>
Context: User wants to upgrade Ruby and Rails together.
user: "I need to upgrade this app from Ruby 2.7 and Rails 6.1 to Ruby 3.3 and Rails 8.0"
assistant: "I'll use the upgrade-auditor agent to do a full pre-upgrade assessment before we touch any code."
<commentary>
Combined Ruby+Rails upgrade intent triggers the auditor. It detects Rails presence automatically and covers both domains.
</commentary>
</example>

<example>
Context: User wants a Ruby-only upgrade.
user: "We need to get to Ruby 3.2 — what are we dealing with?"
assistant: "Let me run the upgrade-auditor to assess the current state against Ruby 3.2."
<commentary>
Ruby-only upgrade intent triggers the auditor. It will detect no Rails and scope the audit accordingly.
</commentary>
</example>

<example>
Context: User wants to understand upgrade complexity before committing.
user: "How hard would it be to upgrade to Rails 8?"
assistant: "I'll use the upgrade-auditor to give you a complete picture before we decide."
<commentary>
Scoping/assessment questions trigger the auditor — user wants the full picture before committing.
</commentary>
</example>

model: inherit
color: cyan
tools: ["Read", "Bash", "Grep", "Glob"]
---

You are a Ruby and Rails upgrade specialist. Your job is to perform a comprehensive read-only pre-upgrade audit and produce a clear, prioritized findings report. You never modify files.

## Step 1: Detect Project Type and Current Versions

```bash
ruby -v 2>/dev/null || echo "Ruby: not detected"
cat .ruby-version 2>/dev/null
grep "^ruby " Gemfile 2>/dev/null
grep -A2 "RUBY VERSION" Gemfile.lock 2>/dev/null
```

Determine if this is a Rails project by checking for `config/application.rb` or `gem 'rails'` in `Gemfile`.

```bash
bundle exec rails -v 2>/dev/null || echo "Rails: not present"
grep "gem ['\"]rails['\"]" Gemfile 2>/dev/null
```

## Step 2: Infer Upgrade Targets

If the user specified target versions, use those. If not, infer the most likely target:
- Ruby: next stable minor version above current
- Rails: next major version above current (if Rails present)

State your inference and proceed — do not ask if it is clear from context.

## Step 3: Run the Audit

Use the `ruby-upgrade-toolkit:audit` skill, passing the inferred or specified `ruby:X.Y.Z` and (if Rails present) `rails:X.Y` arguments.

The skill handles all audit logic. Follow it completely.

## Step 4: Recommend Next Steps

After the audit report, always end with:

```
## Recommended Workflow

Run these commands in order:

1. `/ruby-upgrade-toolkit:audit ruby:[TARGET] [rails:[TARGET]]`   ← you just completed this
2. `/ruby-upgrade-toolkit:plan  ruby:[TARGET] [rails:[TARGET]]`   ← generate phased roadmap
3. `/ruby-upgrade-toolkit:fix   ruby:[TARGET] [rails:[TARGET]]`   ← apply changes
4. `/ruby-upgrade-toolkit:status`                                  ← verify each phase
```
```

- [ ] **Step 2: Verify frontmatter**

```bash
python3 -c "
content = open('agents/upgrade-auditor.md').read()
parts = content.split('---', 2)
assert len(parts) == 3, 'Missing frontmatter'
assert 'name: upgrade-auditor' in parts[1], 'Wrong agent name'
print('agents/upgrade-auditor.md OK')
"
```

Expected: `agents/upgrade-auditor.md OK`

- [ ] **Step 3: Commit**

```bash
git add agents/upgrade-auditor.md
git commit -m "feat: add unified upgrade-auditor agent"
```

---

## Task 9: Update agents/deprecation-fixer.md

**Files:**
- Modify: `agents/deprecation-fixer.md`

Update the frontmatter description and any slash command references to use the new namespaced format.

- [ ] **Step 1: Update the frontmatter description field**

The current `description:` field ends with examples mentioning `/fix-deprecations`. Change those lines so the examples reference the new command:

Change the three example `assistant:` lines in the frontmatter from:
```
assistant: "I'll use the deprecation-fixer agent to read, analyze, and fix the deprecations in that file."
assistant: "I'll launch the deprecation-fixer agent to systematically work through all files with deprecation warnings."
assistant: "Using the deprecation-fixer agent to fix the deprecations in UsersController."
```

Add after the last example block (before `model: inherit`), a note:
```
You can also fix specific files directly without this agent using /ruby-upgrade-toolkit:fix ruby:X.Y.Z scope:path.
```

- [ ] **Step 2: Update the Step 6 Final Verification command**

In the agent body, find the final verification bash block:
```bash
RAILS_ENV=test bundle exec rspec --no-color 2>&1 | grep -c "DEPRECATION" 2>/dev/null || echo "0 deprecation warnings"
```

Add after it a line recommending the status command:
```
After all files are fixed, run /ruby-upgrade-toolkit:status to confirm overall upgrade health.
```

- [ ] **Step 3: Verify no old slash command format remains**

```bash
grep -n "^/fix-deprecations\|^/gem-compatibility\|^/deprecation-audit\|^/upgrade-plan\|^/upgrade-status\|^/config-upgrade\|^/migration-safety" agents/deprecation-fixer.md
```

Expected: no output.

- [ ] **Step 4: Commit**

```bash
git add agents/deprecation-fixer.md
git commit -m "refactor: update deprecation-fixer to reference new fix command"
```

---

## Task 10: Add ruby-version-sync hook and update hooks.json

**Files:**
- Create: `hooks/scripts/ruby-version-sync.sh`
- Modify: `hooks/hooks.json`

The new hook fires when `.ruby-version` or `Gemfile` is edited and validates that both files agree on the Ruby version. If they disagree, it emits a warning (non-blocking).

- [ ] **Step 1: Create hooks/scripts/ruby-version-sync.sh**

```bash
#!/usr/bin/env bash
# PostToolUse hook: Validate .ruby-version and Gemfile ruby directive stay in sync.
# Fires when either .ruby-version or Gemfile is edited.
# Non-blocking — emits a warning but does not prevent the write.
set -euo pipefail

input=$(cat)
file_path=$(echo "$input" | jq -r '.tool_input.file_path // empty' 2>/dev/null)

if [[ -z "$file_path" ]]; then
  exit 0
fi

project_dir="${CLAUDE_PROJECT_DIR:-$(pwd)}"
normalized="${file_path#"$project_dir/"}"

# Only fire on .ruby-version or Gemfile
if [[ "$normalized" != ".ruby-version" ]] && [[ "$normalized" != "Gemfile" ]]; then
  exit 0
fi

ruby_version_file="$project_dir/.ruby-version"
gemfile="$project_dir/Gemfile"

# Read .ruby-version (strip whitespace)
if [[ ! -f "$ruby_version_file" ]]; then
  exit 0
fi
rv_version=$(cat "$ruby_version_file" | tr -d '[:space:]')

# Read Gemfile ruby directive
if [[ ! -f "$gemfile" ]]; then
  exit 0
fi
gemfile_version=$(grep -E "^ruby ['\"]" "$gemfile" | grep -oE "[0-9]+\.[0-9]+(\.[0-9]+)?" | head -1)

if [[ -z "$gemfile_version" ]]; then
  # No ruby directive in Gemfile — not an error, just no check possible
  exit 0
fi

# Compare major.minor (patch may legitimately differ between .ruby-version and Gemfile ~> pin)
rv_minor=$(echo "$rv_version" | cut -d. -f1-2)
gf_minor=$(echo "$gemfile_version" | cut -d. -f1-2)

if [[ "$rv_minor" != "$gf_minor" ]]; then
  echo "WARNING: Ruby version mismatch detected:"
  echo "  .ruby-version: $rv_version (minor: $rv_minor)"
  echo "  Gemfile ruby:  $gemfile_version (minor: $gf_minor)"
  echo "  Run: /ruby-upgrade-toolkit:fix ruby:$rv_version to reconcile"
fi

exit 0
```

- [ ] **Step 2: Make the script executable**

```bash
chmod +x hooks/scripts/ruby-version-sync.sh
```

- [ ] **Step 3: Test the script syntax**

```bash
bash -n hooks/scripts/ruby-version-sync.sh && echo "Syntax OK"
```

Expected: `Syntax OK`

- [ ] **Step 4: Update rubocop-fix.sh flag file name**

The script currently checks for `.rails-upgrade-rubocop`. Update it to check for `.ruby-upgrade-toolkit-rubocop` to match the new plugin name:

In `hooks/scripts/rubocop-fix.sh`, replace:
```bash
if [[ ! -f "$project_dir/.rails-upgrade-rubocop" ]]; then
```
with:
```bash
if [[ ! -f "$project_dir/.ruby-upgrade-toolkit-rubocop" ]]; then
```

Also update the comment block at the top of the script:
```bash
# OPT-IN: Only runs when .ruby-upgrade-toolkit-rubocop file exists in project root.
# To enable: touch .ruby-upgrade-toolkit-rubocop
# To disable: rm .ruby-upgrade-toolkit-rubocop
```

Verify syntax after edit:
```bash
bash -n hooks/scripts/rubocop-fix.sh && echo "Syntax OK"
```

- [ ] **Step 5: Update hooks/hooks.json**

Replace the `log-migration.sh` PostToolUse entry with `ruby-version-sync.sh`. The full updated file:

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "bash \"$CLAUDE_PLUGIN_ROOT/hooks/scripts/block-vendor.sh\"",
            "timeout": 5
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "bash \"$CLAUDE_PLUGIN_ROOT/hooks/scripts/ruby-version-sync.sh\"",
            "timeout": 5
          }
        ]
      },
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "bash \"$CLAUDE_PLUGIN_ROOT/hooks/scripts/rubocop-fix.sh\"",
            "timeout": 30
          }
        ]
      }
    ]
  }
}
```

- [ ] **Step 6: Validate hooks.json**

```bash
python3 -m json.tool hooks/hooks.json > /dev/null && echo "hooks.json OK"
```

Expected: `hooks.json OK`

- [ ] **Step 7: Commit**

```bash
git add hooks/scripts/ruby-version-sync.sh hooks/scripts/rubocop-fix.sh hooks/hooks.json
git commit -m "feat: add ruby-version-sync hook, update rubocop flag name, replace log-migration"
```

---

## Task 11: Delete old files

**Files:** All files listed in the "Delete" section of the file map.

Delete old files only after all new files are committed. This ensures the plugin is never broken mid-refactor.

- [ ] **Step 1: Delete old skill directories**

```bash
rm -rf skills/upgrade-plan
rm -rf skills/deprecation-audit
rm -rf skills/fix-deprecations
rm -rf skills/gem-compatibility-check
rm -rf skills/migration-safety-check
rm -rf skills/config-upgrade
rm -rf skills/upgrade-status
rm -rf skills/ruby-upgrade-guide
```

- [ ] **Step 2: Delete old agents**

```bash
rm agents/rails-upgrade-auditor.md
rm agents/ruby-upgrade-auditor.md
```

- [ ] **Step 3: Delete old hook script**

```bash
rm hooks/scripts/log-migration.sh
```

- [ ] **Step 4: Verify remaining structure**

```bash
find . -not -path './.git/*' -not -path './docs/*' -not -path './.github/*' -type f | sort
```

Expected output must include:
- `.claude-plugin/plugin.json`
- `.claude-plugin/marketplace.json`
- `commands/plan.md`, `commands/audit.md`, `commands/fix.md`, `commands/status.md`
- `skills/plan/SKILL.md`
- `skills/audit/SKILL.md`
- `skills/fix/SKILL.md`
- `skills/status/SKILL.md`
- `skills/rails-upgrade-guide/SKILL.md` + 6 references files
- `agents/upgrade-auditor.md`
- `agents/deprecation-fixer.md`
- `hooks/hooks.json` + 3 scripts (block-vendor, rubocop-fix, ruby-version-sync)
- `README.md`

Must NOT include: any of the deleted paths.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "refactor: remove old skill directories, agents, and hook replaced by new structure"
```

---

## Task 12: Rewrite README.md

**Files:**
- Modify: `README.md`

Full rewrite. The README is the primary documentation for users and contributors. It must explain the canonical workflow, all commands with examples, agents, hooks, and how to contribute.

- [ ] **Step 1: Write README.md**

```markdown
# ruby-upgrade-toolkit

A Claude Code plugin for upgrading Ruby projects safely — including Ruby on Rails apps. Supports any Ruby version upgrade (2.7→3.x and beyond) and any Rails version upgrade (5→8), separately or together.

## How It Works

The plugin gives Claude a structured, repeatable methodology through four commands that map to a canonical workflow:

```
audit → plan → fix → status
```

**Why this order matters:**
- `audit` is read-only — zero risk, run it first to understand the full scope before touching code
- `plan` uses audit findings to sequence work correctly — Ruby phases before Rails phases, intermediate versions before final target
- `fix` applies changes phase by phase — gem updates, code fixes, then iterative RSpec and RuboCop until green
- `status` is your checkpoint after each fix phase — confirms green before you proceed

## Installation

### Via Claude Code marketplace

```bash
/plugin marketplace add dhruvasagar/ruby-upgrade-toolkit
```

### Local development

```bash
git clone https://github.com/dhruvasagar/ruby-upgrade-toolkit
/plugin local add /path/to/ruby-upgrade-toolkit
```

## Command Reference

All commands are namespaced under `/ruby-upgrade-toolkit:` to avoid conflicts with other plugins.

### `/ruby-upgrade-toolkit:audit ruby:X.Y.Z [rails:X.Y]`

Read-only pre-upgrade assessment. Run this first.

Surfaces: Ruby breaking changes for the target version, Rails deprecations (if Rails present), gem incompatibilities for both Ruby and Rails targets, migration safety issues, RuboCop TargetRubyVersion gap, and an effort estimate.

**Never modifies any file.**

```bash
# Ruby-only audit
/ruby-upgrade-toolkit:audit ruby:3.3.1

# Combined Ruby + Rails audit
/ruby-upgrade-toolkit:audit ruby:3.3.1 rails:8.0
```

### `/ruby-upgrade-toolkit:plan ruby:X.Y.Z [rails:X.Y]`

Generate a phased, project-specific upgrade roadmap. Detects current versions automatically.

Produces a Markdown plan with: prerequisites, Ruby upgrade phases (one per intermediate version), Rails upgrade phases (if `rails:` given), and a final verification checklist. Each phase ends with RSpec green + RuboCop clean.

```bash
# Ruby-only plan
/ruby-upgrade-toolkit:plan ruby:3.3.1

# Combined Ruby + Rails plan
/ruby-upgrade-toolkit:plan ruby:3.3.1 rails:8.0
```

### `/ruby-upgrade-toolkit:fix ruby:X.Y.Z [rails:X.Y] [scope:path]`

Apply all upgrade changes. The primary execution command.

Applies: `.ruby-version` and `Gemfile` pin updates, gem dependency updates, Ruby version-specific code fixes, Rails deprecation fixes (if `rails:` given), Rails config updates (if `rails:` given), iterative RSpec until green, iterative RuboCop until clean.

Flags CI/CD pipeline files and Dockerfiles for manual update — never modifies them automatically.

```bash
# Fix Ruby upgrade
/ruby-upgrade-toolkit:fix ruby:3.3.1

# Fix Ruby + Rails upgrade
/ruby-upgrade-toolkit:fix ruby:3.3.1 rails:8.0

# Fix a single file only (gem/pin changes still apply project-wide)
/ruby-upgrade-toolkit:fix ruby:3.3.1 rails:8.0 scope:app/models/user.rb

# Fix a directory
/ruby-upgrade-toolkit:fix ruby:3.3.1 rails:8.0 scope:app/controllers/
```

### `/ruby-upgrade-toolkit:status`

Current upgrade health dashboard. No arguments.

Reports: current vs. target versions, test suite pass/fail, deprecation warning count, Ruby warning count, RuboCop offense count, and overall RED/YELLOW/GREEN readiness.

Run this after each `fix` phase. Do not proceed to the next phase if the report shows RED.

```bash
/ruby-upgrade-toolkit:status
```

## Canonical Workflow Examples

### Example 1: Ruby-only upgrade (2.7 → 3.3)

```bash
# 1. Understand the scope
/ruby-upgrade-toolkit:audit ruby:3.3.1

# 2. Generate the roadmap (includes intermediate: 2.7→3.0→3.1→3.2→3.3)
/ruby-upgrade-toolkit:plan ruby:3.3.1

# 3. Apply the first phase (2.7 → 3.0, the most impactful step)
/ruby-upgrade-toolkit:fix ruby:3.0.7

# 4. Checkpoint — must be GREEN before next phase
/ruby-upgrade-toolkit:status

# 5. Continue phase by phase until 3.3.1
/ruby-upgrade-toolkit:fix ruby:3.1.6
/ruby-upgrade-toolkit:status
/ruby-upgrade-toolkit:fix ruby:3.2.4
/ruby-upgrade-toolkit:status
/ruby-upgrade-toolkit:fix ruby:3.3.1
/ruby-upgrade-toolkit:status
```

### Example 2: Coordinated Ruby + Rails upgrade

```bash
# 1. Full audit of both upgrade targets
/ruby-upgrade-toolkit:audit ruby:3.3.1 rails:8.0

# 2. Phased plan (Ruby first, then Rails)
/ruby-upgrade-toolkit:plan ruby:3.3.1 rails:8.0

# 3. Complete the Ruby upgrade first
/ruby-upgrade-toolkit:fix ruby:3.3.1
/ruby-upgrade-toolkit:status  # must be GREEN

# 4. Then apply the Rails upgrade
/ruby-upgrade-toolkit:fix ruby:3.3.1 rails:8.0
/ruby-upgrade-toolkit:status  # must be GREEN
```

### Example 3: Targeted fix during an upgrade

```bash
# Fix a specific model that has deprecation warnings
/ruby-upgrade-toolkit:fix ruby:3.3.1 rails:8.0 scope:app/models/order.rb

# Check progress
/ruby-upgrade-toolkit:status
```

### Example 4: Check current state at any time

```bash
/ruby-upgrade-toolkit:status
```

## Agents

Two agents activate automatically based on natural language context — no slash commands needed.

### upgrade-auditor

Fires when you describe an upgrade intent:
- "I need to upgrade this app from Rails 7 to 8"
- "What would it take to get to Ruby 3.3?"
- "How bad is our deprecation situation?"

Detects whether the project is Rails or plain Ruby automatically. Produces the same findings report as `/ruby-upgrade-toolkit:audit` and ends with the recommended command sequence.

### deprecation-fixer

Fires when you ask to fix deprecations in a specific file or directory:
- "Fix the deprecation warnings in app/models/user.rb"
- "Clear all deprecations from app/controllers/"

Reads each file, applies safe mechanical fixes automatically, presents complex fixes for confirmation, and runs the file's tests to verify.

## Hooks

Three automatic hooks activate when the plugin is installed:

| Hook | Event | Behavior |
|------|-------|----------|
| `block-vendor` | PreToolUse (Write/Edit) | Blocks any write to `vendor/` — use `bundle update` instead |
| `ruby-version-sync` | PostToolUse (Write/Edit) | Warns when `.ruby-version` and `Gemfile` ruby directive have different minor versions |
| `rubocop-fix` | PostToolUse (Write/Edit) | **Opt-in** — auto-corrects Style/Layout cops on edited `.rb` files |

### Enable RuboCop auto-fix

```bash
touch .ruby-upgrade-toolkit-rubocop   # enable
rm .ruby-upgrade-toolkit-rubocop      # disable
```

Requires `rubocop` in your Gemfile. Only corrects Style and Layout cops — does not touch Metrics or Lint.

## Prerequisites

- Claude Code 1.x+
- Ruby project using Bundler
- `jq` installed (required by hook scripts): `brew install jq` / `apt install jq`
- RuboCop in Gemfile (for rubocop-fix hook and `fix` command RuboCop step)
- Target Ruby version installed via rbenv or rvm before running `fix`

## Ruby ↔ Rails Version Compatibility

| Target Ruby | Minimum Rails | Recommended Rails |
|-------------|--------------|-------------------|
| 2.7         | 5.2          | 6.0–6.1           |
| 3.0         | 6.1          | 7.0               |
| 3.1         | 7.0          | 7.0–7.1           |
| 3.2         | 7.0.4        | 7.1               |
| 3.3         | 7.1          | 7.1–7.2           |
| 3.4         | 7.2          | 7.2–8.0           |

Always complete the Ruby upgrade before starting the Rails upgrade when doing both.

## Contributing

Issues and PRs welcome at [github.com/dhruvasagar/ruby-upgrade-toolkit](https://github.com/dhruvasagar/ruby-upgrade-toolkit).

### Adding a new Rails version guide

Add a file at `skills/rails-upgrade-guide/references/rails-X-to-Y.md` and reference it in `skills/rails-upgrade-guide/SKILL.md`.

### Updating the gem compatibility matrix

Edit `skills/rails-upgrade-guide/references/compatibility-matrix.md`. Follow the existing table format.

### Adding a new Ruby version's breaking changes

Add breaking change patterns to the relevant step in `skills/audit/SKILL.md` (Step 3) and `skills/fix/SKILL.md` (Step 4), scoped to the relevant version pair.

## License

MIT
```

- [ ] **Step 2: Verify README has all required sections**

```bash
for section in "audit" "plan" "fix" "status" "Workflow" "Agents" "Hooks" "Prerequisites" "Contributing"; do
  grep -q "$section" README.md && echo "OK: $section" || echo "MISSING: $section"
done
```

Expected: `OK:` for all sections.

- [ ] **Step 3: Commit**

```bash
git add README.md
git commit -m "docs: rewrite README for ruby-upgrade-toolkit"
```

---

## Task 13: Final validation

- [ ] **Step 1: Verify complete file structure**

```bash
find . -not -path './.git/*' -type f | sort
```

Confirm:
- All 4 command files exist under `commands/`
- All 4 new skills exist under `skills/`
- `skills/rails-upgrade-guide/` has 6 reference files
- `agents/upgrade-auditor.md` and `agents/deprecation-fixer.md` exist
- `agents/rails-upgrade-auditor.md` and `agents/ruby-upgrade-auditor.md` do NOT exist
- `hooks/scripts/ruby-version-sync.sh` exists
- `hooks/scripts/log-migration.sh` does NOT exist
- None of the old skill directories exist

- [ ] **Step 2: Validate all JSON files**

```bash
for f in .claude-plugin/plugin.json .claude-plugin/marketplace.json hooks/hooks.json; do
  python3 -m json.tool "$f" > /dev/null && echo "OK: $f"
done
```

Expected: `OK:` for all three files.

- [ ] **Step 3: Validate all shell scripts**

```bash
for f in hooks/scripts/*.sh; do
  bash -n "$f" && echo "OK: $f"
done
```

Expected: `OK:` for all three scripts.

- [ ] **Step 4: Validate all skill frontmatter**

```bash
for f in skills/*/SKILL.md commands/*.md agents/*.md; do
  python3 -c "
import sys
content = open('$f').read()
if not content.startswith('---'):
    print('MISSING FRONTMATTER: $f'); sys.exit(1)
parts = content.split('---', 2)
if len(parts) < 3:
    print('MALFORMED FRONTMATTER: $f'); sys.exit(1)
print('OK: $f')
"
done
```

Expected: `OK:` for every file.

- [ ] **Step 5: Verify no old command names remain in any file**

```bash
grep -rn "/upgrade-plan\|/deprecation-audit\|/fix-deprecations\|/gem-compatibility-check\|/migration-safety-check\|/config-upgrade\|/upgrade-status" . --include="*.md" --include="*.json" --exclude-dir=".git" --exclude-dir="docs"
```

Expected: no output. If any matches found, update those references to the new namespaced command format.

- [ ] **Step 6: Final commit**

```bash
git add -A
git status  # confirm nothing unexpected is staged
git commit -m "chore: final validation pass — ruby-upgrade-toolkit refactor complete"
```

- [ ] **Step 7: Tag the release**

```bash
git tag v0.2.0
```
```
