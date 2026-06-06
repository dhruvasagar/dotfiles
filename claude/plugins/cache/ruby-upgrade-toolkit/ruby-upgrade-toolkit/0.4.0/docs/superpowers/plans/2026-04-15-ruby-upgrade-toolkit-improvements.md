# Ruby Upgrade Toolkit: Targeted Improvements Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement the eight highest-impact improvements identified in the thorough plugin review, addressing critical detection gaps, usability issues, and missing reference content.

**Architecture:** Each task modifies one or two Markdown skill/reference files. There is no compiled code — changes are tested by running the grep/bash patterns directly against a fixture or real project to verify correct output. Tasks are independent and can be executed in any order.

**Tech Stack:** Bash (hook scripts), Markdown (skill/agent instruction files), grep/sed/awk (pattern detection), Ruby one-liners (verification).

---

## File Map

Files created or modified:

| Task | File                                                            | Change                                                             |
|------|-----------------------------------------------------------------|--------------------------------------------------------------------|
| 1    | `skills/audit/SKILL.md`                                         | Add baseline failure warning to Step 2                             |
| 1    | `skills/fix/SKILL.md`                                           | Add pre-existing failure capture before Step 6 loop                |
| 2    | `skills/audit/SKILL.md`                                         | Expand Step 3 keyword arg section with Pattern B detection         |
| 2    | `skills/fix/SKILL.md`                                           | Add Pattern B fix instructions to Step 4                           |
| 3    | `skills/audit/SKILL.md`                                         | Replace naive `it` grep with context-aware version                 |
| 3    | `skills/fix/SKILL.md`                                           | Replace naive `it` grep with context-aware version                 |
| 4    | `skills/fix/SKILL.md`                                           | Add structured bundle install failure parsing to Step 3            |
| 5    | `skills/fix/SKILL.md`                                           | Add open redirect confirmation flow in Step 5d                     |
| 5    | `skills/audit/SKILL.md`                                         | Annotate open redirect static pattern with confirmation note       |
| 6    | `skills/rails-upgrade-guide/references/turbo-stimulus-guide.md` | Create Turbo/Stimulus migration reference                          |
| 6    | `skills/rails-upgrade-guide/SKILL.md`                           | Add turbo-stimulus-guide to reference list                         |
| 6    | `skills/audit/SKILL.md`                                         | Add Turbolinks/turbo detection to Step 4 static scan               |
| 6    | `skills/fix/SKILL.md`                                           | Add Turbo migration step to Step 5                                 |
| 7    | `skills/rails-upgrade-guide/references/compatibility-matrix.md` | Add Rails 5.x column to all tables                                 |
| 8    | `hooks/scripts/ruby-version-sync.sh`                            | Handle `.ruby-version` with `ruby-` prefix and `~>` pin in Gemfile |

---

### Task 1: Pre-existing Test Failure Warning

Prevents a common footgun: the fix skill runs the RSpec loop, hits failures, and spends time "fixing" them when they were pre-existing before the upgrade. The audit skill should loudly warn when the baseline is not GREEN.

**Files:**
- Modify: `skills/audit/SKILL.md` (Step 2, lines 33–43)
- Modify: `skills/fix/SKILL.md` (before Step 6 loop, around line 218)

- [ ] **Step 1: Verify the audit Step 2 baseline section currently lacks a warning**

```bash
grep -n "FAILING\|pre-existing\|baseline" /Users/dhruva/src/oss/claude-rails-upgrade/skills/audit/SKILL.md
```

Expected: zero or one match in the report template only (line 175+), none in Step 2.

- [ ] **Step 2: Replace the Step 2 "Record" line in `skills/audit/SKILL.md`**

Find this exact text (lines 42–43):
```
Record: pass/fail status, example count, failure count.
```

Replace with:
```
Record: pass/fail status, example count, failure count.

> **If the test suite is FAILING before the upgrade begins, surface this as the first item in the findings report under a `## ⚠️ Pre-existing Failures` section. These failures are NOT caused by the upgrade. Do not count them against upgrade risk. Instruct the user to fix them before starting the upgrade or explicitly document them as known pre-existing issues.**
```

- [ ] **Step 3: Verify the edit took effect**

```bash
grep -n "Pre-existing\|pre-existing" /Users/dhruva/src/oss/claude-rails-upgrade/skills/audit/SKILL.md
```

Expected: one match in Step 2.

- [ ] **Step 4: Add pre-existing failure capture to `skills/fix/SKILL.md` before Step 6**

Find the exact text at the start of Step 6 (line ~218):
```
## Step 6: Iterative RSpec Loop

```bash
bundle exec rspec --no-color --format progress 2>&1 | tail -30
```
```

Replace with:
```
## Step 6: Iterative RSpec Loop

Before starting the fix loop, capture the failure count from the pre-upgrade baseline (from the audit report or by running RSpec now):

```bash
bundle exec rspec --no-color --format progress 2>&1 | tail -5
```

Note the number of failures as `BASELINE_FAILURES`. In the summary, only count failures **above** this baseline as upgrade-introduced regressions. If the baseline already had failures, document them separately as pre-existing and do not attempt to fix them unless the user explicitly asks.

```bash
bundle exec rspec --no-color --format progress 2>&1 | tail -30
```
```

- [ ] **Step 5: Verify**

```bash
grep -n "BASELINE_FAILURES\|pre-existing" /Users/dhruva/src/oss/claude-rails-upgrade/skills/fix/SKILL.md
```

Expected: 2 matches (one for BASELINE_FAILURES, one for "pre-existing").

- [ ] **Step 6: Commit**

```bash
cd /Users/dhruva/src/oss/claude-rails-upgrade
git add skills/audit/SKILL.md skills/fix/SKILL.md
git commit -m "feat: add pre-existing test failure baseline warning to audit and fix skills"
```

---

### Task 2: Keyword Argument Pattern B Detection

The audit currently only counts `**kwargs`/options hash method definitions (Pattern A). It misses Pattern B: call sites that double-splat a hash into a method expecting a plain hash options argument. The fix skill documents both patterns but the audit only reports Pattern A counts.

**Files:**
- Modify: `skills/audit/SKILL.md` (Step 3, keyword arg section, lines 48–60)
- Modify: `skills/fix/SKILL.md` (Step 4 Pattern B section, lines 90–100)

- [ ] **Step 1: Verify current audit Step 3 keyword section**

```bash
grep -n "Pattern B\|double-splat" /Users/dhruva/src/oss/claude-rails-upgrade/skills/audit/SKILL.md
```

Expected: zero matches (Pattern B is not yet in the audit skill).

- [ ] **Step 2: Expand the keyword arg audit section in `skills/audit/SKILL.md`**

Find the exact block (lines 48–60):
```
### 2.7 → 3.0: Keyword argument separation (most impactful change in Ruby 3.x history)

```bash
# Methods accepting **kwargs or options hash
grep -rn "def .*\*\*[a-z_]*" app/ lib/ --include="*.rb" 2>/dev/null | wc -l
grep -rn "def .*[a-z_]* = {}" app/ lib/ --include="*.rb" 2>/dev/null | wc -l

# Call sites with potential hash/keyword mismatch
grep -rEn "\*\*options|\*\*opts|\*\*params|\*\*kwargs" app/ --include="*.rb" 2>/dev/null | wc -l

# Ruby 2.7 deprecation warnings preview (only run if upgrading FROM Ruby 2.7)
RUBYOPT="-W:deprecated" bundle exec rspec --no-color 2>&1 | grep -i "keyword" | sort | uniq -c | sort -rn | head -20
```
```

Replace with:
```
### 2.7 → 3.0: Keyword argument separation (most impactful change in Ruby 3.x history)

```bash
# Pattern A — methods that accept **kwargs (callers must NOT pass plain hash)
echo "Pattern A — **kwargs method definitions:"
grep -rEn "def [a-z_]+\([^)]*\*\*[a-z_]+" app/ lib/ --include="*.rb" 2>/dev/null | wc -l

# Pattern A — methods with options hash default (callers must NOT pass **hash)
echo "Pattern A — options={} method definitions:"
grep -rEn "def [a-z_]+\([^)]*[a-z_]+ = \{\}" app/ lib/ --include="*.rb" 2>/dev/null | wc -l

# Pattern A call sites — hash variable passed without ** where kwargs expected
echo "Pattern A — hash variables at call sites (candidates):"
grep -rEn "[a-z_]+\(options\)|[a-z_]+\(opts\)|[a-z_]+\(params\)" app/ lib/ --include="*.rb" 2>/dev/null | grep -v "def " | wc -l

# Pattern B — double-splat used at call site where method expects positional hash
echo "Pattern B — **hash passed to method expecting positional hash:"
grep -rEn "\*\*options|\*\*opts|\*\*params|\*\*kwargs" app/ lib/ --include="*.rb" 2>/dev/null | grep -v "def " | wc -l

# Best signal: Ruby 2.7 live deprecation warnings (only run if upgrading FROM Ruby 2.7)
echo "Live 2.7 deprecation warnings (keyword arg warnings only):"
RUBYOPT="-W:deprecated" bundle exec rspec --no-color 2>&1 | grep -iE "keyword|hash.*keyword|keyword.*hash" | sort | uniq -c | sort -rn | head -20
```

**What to look for:**
- Pattern A method definitions + Pattern A call sites with mismatched hash: ArgumentError in Ruby 3.0 when caller passes `connect(options)` (plain hash) to a method with `def connect(**opts)`.
- Pattern B: `**hash` at call site passed to method with `def process(options = {})`: ArgumentError in Ruby 3.0 when caller passes `process(**opts)` to a method expecting a positional hash.
- Live warnings from 2.7 are the most reliable signal — each warning corresponds to a guaranteed break in 3.0.
```

- [ ] **Step 3: Verify the expanded section**

```bash
grep -n "Pattern A\|Pattern B" /Users/dhruva/src/oss/claude-rails-upgrade/skills/audit/SKILL.md
```

Expected: at least 4 matches (Pattern A ×3, Pattern B ×1).

- [ ] **Step 4: Expand Pattern B in fix skill `skills/fix/SKILL.md`**

Find the Pattern B block (lines ~90–100):
```
**Pattern B — keywords passed to method expecting positional hash:**
```ruby
# BEFORE: double-splat on a method that takes options = {}
process(**{key: "value"})  # ArgumentError in Ruby 3.0

# AFTER: drop the double-splat
process(key: "value")
```
```

Replace with:
```
**Pattern B — keywords passed to method expecting positional hash:**

Detection — find `**hash` at call sites where the method accepts `options = {}`:
```bash
grep -rEn "\*\*options|\*\*opts|\*\*params|\*\*kwargs" ${SCOPE:-app/ lib/} --include="*.rb" | grep -v "def "
```

For each match, read the method definition it is calling. If the method signature is `def method_name(options = {})` or `def method_name(opts = {})` (positional hash, NOT `**`), this is Pattern B:

```ruby
# BEFORE: double-splat on a method that takes options = {}
process(**{key: "value"})   # ArgumentError in Ruby 3.0
process(**user_opts)        # ArgumentError in Ruby 3.0 if process(opts={})

# AFTER: drop the double-splat
process(key: "value")
process(user_opts)
```

If the method definition is NOT available (external gem method), check the gem's changelog for Ruby 3.0 compatibility. Do not apply Pattern B fix to external gem call sites — update the gem instead.
```

- [ ] **Step 5: Verify**

```bash
grep -n "Pattern B\|double-splat\|external gem" /Users/dhruva/src/oss/claude-rails-upgrade/skills/fix/SKILL.md
```

Expected: 3+ matches including the new "external gem" guidance.

- [ ] **Step 6: Commit**

```bash
cd /Users/dhruva/src/oss/claude-rails-upgrade
git add skills/audit/SKILL.md skills/fix/SKILL.md
git commit -m "feat: add Pattern B keyword argument detection to audit and fix skills"
```

---

### Task 3: Smarter `it` Variable Detection

The current grep for the `it` reserved block parameter is:
```
grep -rn "\bit\b" app/ spec/ --include="*.rb" | grep -v "it ['\"]" | grep -v "^\s*#"
```

This still produces massive false positives because `\bit\b` matches any word containing exactly "it" — including `submit`, `commit`, and RSpec example groups like `shared_context "foo" do |it|`. The fix is to use a two-step approach: detect only `it` used as a bare variable inside a block (not as an RSpec call and not in a string).

**Files:**
- Modify: `skills/audit/SKILL.md` (Step 3, `it` section, lines 70–73)
- Modify: `skills/fix/SKILL.md` (Step 4, `it` section, lines 129–132)

- [ ] **Step 1: Test the current pattern against a fixture to confirm false positives**

```bash
# Create a temp fixture to confirm the problem
cat > /tmp/it_test.rb << 'EOF'
# False positives the current grep would catch:
submit_form(it: true)    # "it" in a hash key
# it is working fine     # comment line
form.submit              # "submit" contains "it"? No, \bit\b won't match "submit"

# Real positives:
items.each { it.save }  # "it" used as block param — this IS the Ruby 3.4 issue
EOF

echo "=== Current pattern output ==="
grep -n "\bit\b" /tmp/it_test.rb | grep -v "it ['\"]" | grep -v "^[[:space:]]*#"

echo "=== Better pattern output ==="
grep -En "^\s*\bit\b\." /tmp/it_test.rb | grep -v "^[[:space:]]*#"

rm /tmp/it_test.rb
```

Expected: current pattern matches line with `it:` hash key (false positive). Better pattern only matches `it.save` usage.

- [ ] **Step 2: Replace `it` detection in `skills/audit/SKILL.md`**

Find the exact block (lines 70–73):
```
### → 3.4: `it` as reserved block parameter (warnings in 3.2–3.3, breaks in 3.4)

```bash
grep -rn "\bit\b" app/ spec/ --include="*.rb" 2>/dev/null | grep -v "it ['\"]" | grep -v "^\s*#" | head -20
```
```

Replace with:
```
### → 3.4: `it` as reserved block parameter (warnings in 3.2–3.3, breaks in 3.4)

Only a concern when upgrading TO Ruby 3.4. `it` as a bare method call inside a block is being reserved as the default block parameter.

```bash
# Step 1: Find bare `it` used as a receiver or assigned — most reliable signal
grep -rEn "^\s*it\." app/ lib/ --include="*.rb" 2>/dev/null | grep -v "^[[:space:]]*#" | head -20

# Step 2: Find `it` used on the right side of assignment or in expressions
grep -rEn "[^a-z_]it\b[^'\"\[]" app/ lib/ --include="*.rb" 2>/dev/null \
  | grep -v "^[[:space:]]*#" \
  | grep -v "it ['\"]" \
  | grep -v "bit\b\|commit\b\|submit\b\|permit\b\|limit\b\|edit\b\|visit\b\|digit\b\|habit\b\|orbit\b" \
  | head -20
```

Each match requires manual inspection to confirm `it` is used as a block variable (not as an RSpec `it "..."` call or a symbol key like `it:` or the word in a string).
```

- [ ] **Step 3: Replace `it` detection in `skills/fix/SKILL.md`**

Find the exact block (lines 129–132):
```
```bash
grep -rEn "\bit\b" ${SCOPE:-app/ spec/} --include="*.rb" | grep -v "it ['\"]" | grep -v "^[[:space:]]*#"
```
```

Replace with:
```
```bash
# Find `it` used as a block variable (receiver or assignment target)
grep -rEn "^\s*it\." ${SCOPE:-app/ lib/} --include="*.rb" 2>/dev/null | grep -v "^[[:space:]]*#"

# Broader search, excluding common false positives
grep -rEn "[^a-z_]it\b[^'\"\[]" ${SCOPE:-app/ lib/} --include="*.rb" 2>/dev/null \
  | grep -v "^[[:space:]]*#" \
  | grep -v "it ['\"]" \
  | grep -v "bit\b\|commit\b\|submit\b\|permit\b\|limit\b\|edit\b\|visit\b\|digit\b\|habit\b\|orbit\b"
```

For each match, read 5 lines of context around it to confirm `it` is truly a block parameter variable (not an RSpec test call). When confirmed, rename `it` to a descriptive name based on what the block iterates over (e.g., `it` in `items.each { it.save }` → `item`).
```

- [ ] **Step 4: Verify both edits**

```bash
grep -n "bit.*commit.*submit\|common false positive" /Users/dhruva/src/oss/claude-rails-upgrade/skills/audit/SKILL.md
grep -n "bit.*commit.*submit\|common false positive" /Users/dhruva/src/oss/claude-rails-upgrade/skills/fix/SKILL.md
```

Expected: one match in each file (the new exclusion pattern line).

- [ ] **Step 5: Commit**

```bash
cd /Users/dhruva/src/oss/claude-rails-upgrade
git add skills/audit/SKILL.md skills/fix/SKILL.md
git commit -m "fix: improve 'it' variable detection to reduce false positives"
```

---

### Task 4: Bundle Install Failure Parsing

Step 3 of the fix skill currently does `bundle install 2>&1 | tail -20` and then says "identify each incompatible gem". It provides no structured error message parsing. Common bundle errors have deterministic patterns and can be matched to targeted actions.

**Files:**
- Modify: `skills/fix/SKILL.md` (Step 3, lines 48–71)

- [ ] **Step 1: Verify current Step 3 content**

```bash
grep -n "bundle install\|bundle update\|incompatible" /Users/dhruva/src/oss/claude-rails-upgrade/skills/fix/SKILL.md | head -15
```

Expected: 4–5 matches, none mentioning structured error parsing.

- [ ] **Step 2: Replace Step 3 in `skills/fix/SKILL.md`**

Find the exact block:
```
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
gem "ostruct"     # if used
```

Do not bulk-update all gems. Update one gem at a time until `bundle install` is clean.
```

Replace with:
```
## Step 3: Fix Gem Incompatibilities

```bash
bundle install 2>&1
```

Parse the output using these error patterns:

**Error: "requires Ruby version"**
```
Gem X requires Ruby >= A.B.C, current is X.Y.Z
```
Action: The gem has a minimum Ruby version constraint that is too high for the current Ruby OR too low for the target. Run: `bundle update <gem_name>`. If the latest version still requires a higher Ruby than available, the gem must be replaced (see compatibility matrix).

**Error: "Could not find compatible versions"**
```
Could not find compatible versions for gem '<name>':
  In snapshot (Gemfile.lock): <name> = X.Y.Z was resolved to X.Y.Z, which depends on ...
```
Action: This is a cascading constraint conflict. Run:
```bash
bundle update <name_of_conflicting_gem> 2>&1 | tail -15
```
If the conflict involves Rails itself, ensure the Gemfile `rails` pin was updated in Step 2 first.

**Error: "an error occurred while installing X"** (native extension)
```
An error occurred while installing nokogiri (1.x.y), and Bundler cannot continue.
```
Action: The gem's native extension needs to be recompiled for the new Ruby. Run:
```bash
gem install <gem_name> -- --with-xml2-include=$(brew --prefix libxml2)/include/libxml2 2>&1
# or simply:
bundle exec gem pristine <gem_name>
```

**Error: "rake aborted" or build failure during install**
```
rake aborted!
LoadError: cannot load such file -- <stdlib_lib>
```
Action: A gem's Rakefile requires a stdlib gem that is now separate in Ruby 3.4. Add the gem to Gemfile first, then retry bundle install.

**General approach — update one gem at a time:**
```bash
# Conservative single-gem update
bundle update <gem_name> 2>&1 | tail -10

# Verify no new conflicts introduced
bundle install 2>&1 | tail -5
```

For Ruby 3.4+, add any missing stdlib gems to Gemfile that were found during audit:
```ruby
# Ruby 3.4 stdlib removals — now separate gems
gem "base64"      # if used via require 'base64'
gem "csv"         # if used via require 'csv'
gem "bigdecimal"  # if used via require 'bigdecimal'
gem "ostruct"     # if used via require 'ostruct'
```

Do not bulk-update all gems. Update one gem at a time until `bundle install` exits 0.
```

- [ ] **Step 3: Verify the expanded Step 3**

```bash
grep -n "requires Ruby version\|native extension\|rake aborted\|one gem at a time" /Users/dhruva/src/oss/claude-rails-upgrade/skills/fix/SKILL.md
```

Expected: 4 matches (one per error pattern heading).

- [ ] **Step 4: Commit**

```bash
cd /Users/dhruva/src/oss/claude-rails-upgrade
git add skills/fix/SKILL.md
git commit -m "feat: add structured bundle install error parsing to fix skill"
```

---

### Task 5: Open Redirect Confirmation Flow

The fix skill says "For complex patterns (`redirect_to params[...]`...) present the issue to the user and ask for confirmation before changing." but gives no guidance on what to present or what the confirmation flow looks like. The audit also just counts open redirect risks without explaining them. This task adds a concrete confirmation flow.

**Files:**
- Modify: `skills/fix/SKILL.md` (Step 5d, end of the table, around line 206)
- Modify: `skills/audit/SKILL.md` (Step 4 static scan, open redirect line ~108)

- [ ] **Step 1: Verify current state of the open redirect handling**

```bash
grep -n "open redirect\|redirect_to.*params\|confirmation" /Users/dhruva/src/oss/claude-rails-upgrade/skills/fix/SKILL.md
grep -n "open redirect" /Users/dhruva/src/oss/claude-rails-upgrade/skills/audit/SKILL.md
```

Expected: one mention each in fix (the brief note) and audit (the counter line).

- [ ] **Step 2: Expand the open redirect note in `skills/fix/SKILL.md`**

Find the exact text (after the fix pattern table):
```
For complex patterns (`redirect_to params[...]`, `has_and_belongs_to_many`), present the issue to the user and ask for confirmation before changing.
```

Replace with:
```
**For `has_and_belongs_to_many`**: present the join-model migration plan (create model, replace association, add id column) and wait for explicit user confirmation.

**For open redirects (`redirect_to params[...]`)** — these require security judgment, not just a syntax fix. For each occurrence:

```bash
grep -rn "redirect_to.*params\[" app/controllers/ --include="*.rb"
```

Read the surrounding code and present to the user:

```
Found open redirect candidate:
  File: app/controllers/sessions_controller.rb:47
  Code: redirect_to params[:return_to]

  Security risk: if params[:return_to] is not validated, an attacker can craft
  a URL like /login?return_to=https://evil.com to redirect users off-site.

  Options:
    A) Safe redirect (recommended): only allow relative paths
       redirect_to(params[:return_to].presence&.start_with?('/') ? params[:return_to] : root_path)

    B) Allowlist redirect: only allow specific domains
       allowed = URI.parse(params[:return_to]).host rescue nil
       redirect_to(allowed && ALLOWED_HOSTS.include?(allowed) ? params[:return_to] : root_path)

    C) Keep as-is (intentional external redirect — document why)

  Which option? [A/B/C]
```

Apply the chosen fix. Do not auto-apply open redirect fixes without this confirmation.
```

- [ ] **Step 3: Add a note to the audit's open redirect counter line**

Find in `skills/audit/SKILL.md`:
```
echo "open redirect risk: $(grep -rn 'redirect_to.*params\[' app/controllers/ --include='*.rb' 2>/dev/null | wc -l)"
```

Replace with:
```
echo "open redirect risk: $(grep -rn 'redirect_to.*params\[' app/controllers/ --include='*.rb' 2>/dev/null | wc -l) (requires security review — not auto-fixed)"
```

- [ ] **Step 4: Verify both edits**

```bash
grep -n "open redirect\|allowlist\|ALLOWED_HOSTS" /Users/dhruva/src/oss/claude-rails-upgrade/skills/fix/SKILL.md | head -10
grep -n "requires security review" /Users/dhruva/src/oss/claude-rails-upgrade/skills/audit/SKILL.md
```

Expected: multiple matches in fix, one in audit.

- [ ] **Step 5: Commit**

```bash
cd /Users/dhruva/src/oss/claude-rails-upgrade
git add skills/fix/SKILL.md skills/audit/SKILL.md
git commit -m "feat: add concrete open redirect confirmation flow with security options"
```

---

### Task 6: Turbo/Turbolinks Migration Reference

Apps upgrading from Rails 6 to 7+ need to migrate from the `turbolinks` gem to `turbo-rails`. This is a significant JS-layer change with no direct Ruby equivalent. No reference file exists for it, and neither the audit nor fix skills detect Turbolinks usage or guide the migration. This task creates the reference and integrates it.

**Files:**
- Create: `skills/rails-upgrade-guide/references/turbo-stimulus-guide.md`
- Modify: `skills/rails-upgrade-guide/SKILL.md` (reference list)
- Modify: `skills/audit/SKILL.md` (Step 4 static scan)
- Modify: `skills/fix/SKILL.md` (Step 5 — new sub-step 5f)

- [ ] **Step 1: Confirm the reference file does not exist**

```bash
ls /Users/dhruva/src/oss/claude-rails-upgrade/skills/rails-upgrade-guide/references/
```

Expected: `compatibility-matrix.md`, `fix-patterns.md`, `rails-5-to-6.md`, `rails-6-to-7.md`, `rails-7-to-8.md`, `risky-patterns.md` — no `turbo-stimulus-guide.md`.

- [ ] **Step 2: Create `skills/rails-upgrade-guide/references/turbo-stimulus-guide.md`**

```markdown
# Turbo & Stimulus Migration Guide

Reference for migrating from Turbolinks (Rails 6) to Turbo (Rails 7+).
Applies when upgrading Rails 6.x → 7.x or later.

## Background

Rails 7 ships with Hotwire by default: **Turbo** (replaces Turbolinks) and **Stimulus** (JS framework).
Turbolinks still works in Rails 7 but is no longer maintained. Migration is strongly recommended before Rails 8.

## Step 1: Detect Turbolinks Usage

```bash
# Gem present
grep -n "turbolinks" Gemfile Gemfile.lock 2>/dev/null

# JavaScript references
grep -rn "turbolinks" app/javascript/ app/assets/ --include="*.js" --include="*.coffee" 2>/dev/null | head -20

# ERB/HTML data attributes
grep -rn "data-turbolinks" app/views/ --include="*.erb" --include="*.html" 2>/dev/null | head -20

# Ruby controller/helper references
grep -rn "turbolinks" app/ lib/ --include="*.rb" 2>/dev/null | head -20
```

## Step 2: Add turbo-rails

```ruby
# Gemfile
gem "turbo-rails"
gem "stimulus-rails"  # if using Stimulus
```

```bash
bundle install
bin/rails turbo:install
bin/rails stimulus:install  # if using Stimulus
```

## Step 3: Remove Turbolinks

```ruby
# Gemfile — remove or comment out:
# gem "turbolinks", "~> 5"
```

```bash
bundle install
```

## Step 4: Update JavaScript

### importmap (Rails 7+ default)

```javascript
// config/importmap.rb — remove:
# pin "turbolinks", to: "turbolinks.js"

// app/javascript/application.js — replace:
// BEFORE:
import Turbolinks from "turbolinks"
Turbolinks.start()

// AFTER:
import "@hotwired/turbo-rails"
```

### Webpacker / jsbundling

```javascript
// BEFORE (turbolinks):
import Turbolinks from "turbolinks"
Turbolinks.start()
document.addEventListener("turbolinks:load", () => { ... })

// AFTER (turbo):
import "@hotwired/turbo"
document.addEventListener("turbo:load", () => { ... })
```

## Step 5: Update Event Listeners

| Turbolinks event | Turbo equivalent |
|-----------------|-----------------|
| `turbolinks:load` | `turbo:load` |
| `turbolinks:before-cache` | `turbo:before-cache` |
| `turbolinks:before-render` | `turbo:before-render` |
| `turbolinks:render` | `turbo:render` |
| `turbolinks:visit` | `turbo:visit` |
| `turbolinks:request-start` | `turbo:before-fetch-request` |
| `turbolinks:request-end` | `turbo:before-fetch-response` |

## Step 6: Update Data Attributes

| Turbolinks attribute | Turbo equivalent |
|---------------------|-----------------|
| `data-turbolinks="false"` | `data-turbo="false"` |
| `data-turbolinks-track="reload"` | `data-turbo-track="reload"` |
| `data-turbolinks-action="replace"` | `data-turbo-action="replace"` |
| `data-turbolinks-permanent` | `data-turbo-permanent` |
| `data-turbolinks-cache` | no direct equivalent — use `data-turbo-cache="false"` to disable |

**Find all data attributes to update:**
```bash
grep -rn "data-turbolinks" app/views/ --include="*.erb" --include="*.html" 2>/dev/null
```

## Step 7: Update ERB Helpers

```erb
<%# BEFORE: Turbolinks cache control %>
<meta name="turbolinks-cache-control" content="no-cache">

<%# AFTER: Turbo cache control %>
<meta name="turbo-cache-control" content="no-cache">
```

## Step 8: Update Flash Messages

Turbo requires explicit `turbo_stream` or `format.turbo_stream` responses for flash behavior in SPA-like flows:

```ruby
# BEFORE (Turbolinks — flash appeared after redirect automatically)
redirect_to root_path, notice: "Saved!"

# AFTER (Turbo — works the same for full-page navigations, but for form submissions via fetch:)
respond_to do |format|
  format.turbo_stream { render turbo_stream: turbo_stream.update("flash", partial: "shared/flash") }
  format.html { redirect_to root_path, notice: "Saved!" }
end
```

## Step 9: Test

```bash
bundle exec rspec spec/ --no-color 2>&1 | tail -10
# Check for JS errors in browser console after deployment to staging
```

## Common Issues

**Issue:** `undefined method 'turbolinks_tag'` in views
**Fix:** Remove calls to `turbolinks_tag` — Turbo does not use it.

**Issue:** Page does not update after form submit
**Fix:** Ensure the form has `data: { turbo: true }` or that Turbo is loaded correctly in the JS pipeline.

**Issue:** Duplicate event fires on page load
**Fix:** Change `turbolinks:load` to `turbo:load` in all JS files.
```

- [ ] **Step 3: Add the reference to `skills/rails-upgrade-guide/SKILL.md`**

```bash
grep -n "references/" /Users/dhruva/src/oss/claude-rails-upgrade/skills/rails-upgrade-guide/SKILL.md
```

Find the references list and add `turbo-stimulus-guide.md`. The file contains lines like:
```
- `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/fix-patterns.md`
```

Add after the last reference entry:
```
- `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/turbo-stimulus-guide.md` — Turbolinks → Turbo/Stimulus migration
```

- [ ] **Step 4: Add Turbolinks detection to `skills/audit/SKILL.md` Step 4**

Find the static pattern scan section (around line 99), after the `render text:` echo line. Add before the closing of the code block:

```bash
echo "turbolinks gem: $(grep -n 'turbolinks' Gemfile 2>/dev/null | grep -v '^\s*#' | wc -l) (migrate to turbo-rails for Rails 7+)"
echo "turbolinks JS: $(grep -rn 'turbolinks' app/javascript/ app/assets/ --include='*.js' --include='*.coffee' 2>/dev/null | wc -l) JS references"
echo "data-turbolinks attrs: $(grep -rn 'data-turbolinks' app/views/ --include='*.erb' --include='*.html' 2>/dev/null | wc -l) view attributes"
```

- [ ] **Step 5: Add a Step 5f to `skills/fix/SKILL.md`**

Find the `### 5e. Update RuboCop target version` line and insert before it:

```markdown
### 5f. Migrate Turbolinks → Turbo (Rails 7+ only)

Skip if not upgrading to Rails 7 or later.

```bash
# Check if turbolinks is still in Gemfile
grep -n "turbolinks" Gemfile 2>/dev/null | grep -v "^\s*#"
```

If found, follow `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/turbo-stimulus-guide.md` for the complete migration procedure. Key steps:
1. Add `gem "turbo-rails"` to Gemfile, `bundle install`, run `bin/rails turbo:install`
2. Remove `gem "turbolinks"` from Gemfile, `bundle install`
3. Update JavaScript event listeners (`turbolinks:load` → `turbo:load`)
4. Update data attributes (`data-turbolinks` → `data-turbo`)
5. Run RSpec to catch any view/JS integration regressions

```

- [ ] **Step 6: Verify all four changes**

```bash
ls /Users/dhruva/src/oss/claude-rails-upgrade/skills/rails-upgrade-guide/references/turbo-stimulus-guide.md
grep -n "turbo-stimulus-guide\|turbolinks" /Users/dhruva/src/oss/claude-rails-upgrade/skills/rails-upgrade-guide/SKILL.md
grep -n "turbolinks gem\|turbolinks JS" /Users/dhruva/src/oss/claude-rails-upgrade/skills/audit/SKILL.md
grep -n "Migrate Turbolinks\|5f\." /Users/dhruva/src/oss/claude-rails-upgrade/skills/fix/SKILL.md
```

Expected: file exists, one match in SKILL.md, two matches in audit, one match in fix.

- [ ] **Step 7: Commit**

```bash
cd /Users/dhruva/src/oss/claude-rails-upgrade
git add skills/rails-upgrade-guide/references/turbo-stimulus-guide.md \
        skills/rails-upgrade-guide/SKILL.md \
        skills/audit/SKILL.md \
        skills/fix/SKILL.md
git commit -m "feat: add Turbo/Turbolinks migration guide and integrate into audit and fix skills"
```

---

### Task 7: Rails 5.x Gem Compatibility Matrix

The compatibility matrix starts at Rails 6.0, but many apps are still on Rails 5.x and need to upgrade through 6. Adding a Rails 5.2 column gives those users the same "what version do I need?" guidance for the first hop.

**Files:**
- Modify: `skills/rails-upgrade-guide/references/compatibility-matrix.md`

- [ ] **Step 1: Verify the current matrix starts at Rails 6.0**

```bash
head -10 /Users/dhruva/src/oss/claude-rails-upgrade/skills/rails-upgrade-guide/references/compatibility-matrix.md
grep "Rails 5\|5\.2\|5\.0" /Users/dhruva/src/oss/claude-rails-upgrade/skills/rails-upgrade-guide/references/compatibility-matrix.md | head -5
```

Expected: headers show `Rails 6.0` as the first column, no Rails 5.x content.

- [ ] **Step 2: Update the matrix header and all table rows**

In `skills/rails-upgrade-guide/references/compatibility-matrix.md`, replace the file header block and every table header row. The strategy: add `Rails 5.2` as the first column in every table.

Replace the file's top section:
```
| Gem | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|
```

With (in the Core Ecosystem table):
```
| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| devise | >= 4.5.0 | >= 4.7.0 | >= 4.7.1 | >= 4.8.1 | >= 4.9.2 | >= 4.9.3 |
| pundit | >= 1.1 | >= 2.0 | >= 2.1 | >= 2.2 | >= 2.3 | >= 2.3.1 |
| kaminari | >= 1.1.0 | >= 1.2.0 | >= 1.2.1 | >= 1.2.2 | >= 1.2.2 | >= 1.2.2 |
| will_paginate | >= 3.1.7 | >= 3.3.0 | >= 3.3.0 | >= 4.0.0 | >= 4.0.0 | >= 4.0.0 |
| ransack | >= 1.8 | >= 2.3.0 | >= 2.4.0 | >= 3.0.0 | >= 4.0.0 | >= 4.1.0 |
| friendly_id | >= 5.2.3 | >= 5.3.0 | >= 5.4.0 | >= 5.4.2 | >= 5.5.0 | >= 5.5.0 |
| rolify | >= 5.2.0 | >= 5.3.0 | >= 6.0.0 | >= 6.0.0 | >= 6.0.0 | >= 6.0.0 |
```

Apply the same pattern for all other sections (Background Jobs, File Uploads, API/Serialization, Authentication/Authorization, Admin, Testing, Auditing/Versioning, Search, Utilities). Each table needs:
- A new `Rails 5.2` header column
- A new `Rails 5.2` data column with appropriate minimum versions

Full replacement content (the complete new file):

```markdown
# Gem Compatibility Matrix

Minimum gem versions required for each Rails version.
Last updated: 2026-04.

## Core Ecosystem

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| devise | >= 4.5.0 | >= 4.7.0 | >= 4.7.1 | >= 4.8.1 | >= 4.9.2 | >= 4.9.3 |
| pundit | >= 1.1 | >= 2.0 | >= 2.1 | >= 2.2 | >= 2.3 | >= 2.3.1 |
| kaminari | >= 1.1.0 | >= 1.2.0 | >= 1.2.1 | >= 1.2.2 | >= 1.2.2 | >= 1.2.2 |
| will_paginate | >= 3.1.7 | >= 3.3.0 | >= 3.3.0 | >= 4.0.0 | >= 4.0.0 | >= 4.0.0 |
| ransack | >= 1.8 | >= 2.3.0 | >= 2.4.0 | >= 3.0.0 | >= 4.0.0 | >= 4.1.0 |
| friendly_id | >= 5.2.3 | >= 5.3.0 | >= 5.4.0 | >= 5.4.2 | >= 5.5.0 | >= 5.5.0 |
| rolify | >= 5.2.0 | >= 5.3.0 | >= 6.0.0 | >= 6.0.0 | >= 6.0.0 | >= 6.0.0 |

## Background Jobs

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| sidekiq | >= 5.0 | >= 6.0 | >= 6.1 | >= 6.4 | >= 7.0 | >= 7.0 |
| delayed_job | >= 4.1.5 | >= 4.1.9 | >= 4.1.9 | >= 4.1.11 | >= 4.1.11 | >= 4.1.11 |
| resque | >= 1.27 | >= 2.0 | >= 2.0 | >= 2.3 | >= 2.3 | >= 2.6 |
| que | >= 0.14 | >= 1.3 | >= 1.3 | >= 2.0 | >= 2.0 | >= 2.0 |
| good_job | not supported | >= 1.0 | >= 2.0 | >= 3.0 | >= 3.10 | >= 3.21 |

## File Uploads

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| carrierwave | >= 1.3 | >= 2.1 | >= 2.1 | >= 3.0 | >= 3.0 | >= 3.0 |
| shrine | >= 2.18 | >= 3.0 | >= 3.0 | >= 3.3 | >= 3.4 | >= 3.4 |
| paperclip | **INCOMPATIBLE** | **INCOMPATIBLE** | **INCOMPATIBLE** | **INCOMPATIBLE** | **INCOMPATIBLE** | **INCOMPATIBLE** |
| active_storage_validations | >= 0.7 | >= 0.9 | >= 0.9 | >= 1.0 | >= 1.1 | >= 2.0 |
| image_processing | >= 1.7 | >= 1.10 | >= 1.10 | >= 1.12 | >= 1.12 | >= 1.12 |

## API / Serialization

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| jsonapi-serializer | >= 2.0 | >= 2.1 | >= 2.1 | >= 2.2 | >= 2.2 | >= 2.2 |
| active_model_serializers | >= 0.10.7 | >= 0.10.12 | >= 0.10.12 | >= 0.10.13 | investigate | investigate |
| blueprinter | >= 0.20 | >= 0.25 | >= 0.25 | >= 1.0 | >= 1.0 | >= 1.0 |
| jbuilder | >= 2.7 | >= 2.9 | >= 2.11 | >= 2.11 | >= 2.11 | >= 2.11 |
| fast_jsonapi | **ABANDONED** | **ABANDONED** | **ABANDONED** | **ABANDONED** | **ABANDONED** | **ABANDONED** |

## Authentication / Authorization

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| cancancan | >= 3.0 | >= 3.3 | >= 3.3 | >= 3.4 | >= 3.5 | >= 3.6 |
| jwt | >= 2.1 | >= 2.3 | >= 2.3 | >= 2.4 | >= 2.7 | >= 2.7 |
| bcrypt | >= 3.1.12 | >= 3.1.13 | >= 3.1.13 | >= 3.1.16 | >= 3.1.18 | >= 3.1.18 |
| doorkeeper | >= 5.2 | >= 5.4 | >= 5.5 | >= 5.6 | >= 5.7 | >= 5.7 |
| omniauth | >= 1.9 | >= 2.0 | >= 2.0 | >= 2.1 | >= 2.1 | >= 2.1 |

## Admin

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| activeadmin | >= 2.1 | >= 2.9 | >= 2.11 | >= 3.0 | >= 3.1 | >= 4.0 |
| administrate | >= 0.11 | >= 0.16 | >= 0.17 | >= 0.18 | >= 0.20 | >= 1.0 |

## Testing

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| rspec-rails | >= 3.8 | >= 4.0 | >= 4.1 | >= 5.0 | >= 6.0 | >= 7.0 |
| factory_bot_rails | >= 5.0 | >= 6.1 | >= 6.1 | >= 6.2 | >= 6.2 | >= 6.4 |
| shoulda-matchers | >= 4.0 | >= 4.4 | >= 4.5 | >= 5.0 | >= 5.1 | >= 5.3 |
| capybara | >= 3.18 | >= 3.30 | >= 3.35 | >= 3.36 | >= 3.39 | >= 3.40 |
| vcr | >= 5.0 | >= 6.0 | >= 6.0 | >= 6.1 | >= 6.2 | >= 6.2 |
| webmock | >= 3.6 | >= 3.12 | >= 3.14 | >= 3.14 | >= 3.18 | >= 3.23 |
| database_cleaner-active_record | >= 1.8 | >= 2.0 | >= 2.0 | >= 2.1 | >= 2.1 | >= 2.1 |

## Auditing / Versioning

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| paper_trail | >= 9.2 | >= 10.3 | >= 12.0 | >= 12.3 | >= 13.0 | >= 14.0 |
| audited | >= 4.9 | >= 5.0 | >= 5.0 | >= 5.2 | >= 5.3 | >= 5.4 |

## Search

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| pg_search | >= 2.1 | >= 2.3 | >= 2.3 | >= 2.3.5 | >= 2.3.6 | >= 2.3.6 |
| searchkick | >= 4.0 | >= 5.0 | >= 5.1 | >= 5.2 | >= 5.3 | >= 5.4 |
| elasticsearch-rails | >= 6.0 | >= 7.1 | >= 7.1 | >= 7.2 | >= 8.0 | >= 8.0 |

## Utilities

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| draper | >= 3.1 | >= 4.0 | >= 4.0 | >= 4.0.1 | >= 4.0.2 | investigate |
| acts-as-taggable-on | >= 6.0 | >= 8.0 | >= 9.0 | >= 10.0 | >= 10.0 | >= 10.0 |
| enumerize | >= 2.3 | >= 2.5 | >= 2.5 | >= 2.6 | >= 2.7 | >= 2.7 |
| aasm | >= 5.0 | >= 5.2 | >= 5.2 | >= 5.4 | >= 5.5 | >= 5.5 |
| state_machines-activerecord | >= 0.7 | >= 0.8 | >= 0.8 | >= 0.9 | >= 0.9 | >= 0.10 |
| money-rails | >= 1.13 | >= 1.15 | >= 1.15 | >= 1.15.0 | >= 1.15.0 | investigate |

## Incompatible / Abandoned Gems (must replace)

| Gem | Replacement | Notes |
|-----|-------------|-------|
| paperclip | Active Storage / Shrine | Last release 2019 |
| protected_attributes | Strong Parameters (built-in) | Rails 4 era |
| attr_accessible | Strong Parameters (built-in) | Rails 4 era |
| quiet_assets | Removed (built-in in Rails 6) | |
| turbolinks | turbo-rails | Replace for Rails 7+ |
| webpacker | importmap-rails / jsbundling-rails | Archived 2023 |
| fast_jsonapi | jsonapi-serializer | Fork maintained |
| ar-octopus | Use Rails multi-db | Unmaintained |
| rails_admin | activeadmin or administrate | Very outdated for 7+ |
```

- [ ] **Step 3: Write the file**

Write the full content above to `skills/rails-upgrade-guide/references/compatibility-matrix.md`.

- [ ] **Step 4: Verify Rails 5.2 column is present in all tables**

```bash
grep -c "Rails 5\.2" /Users/dhruva/src/oss/claude-rails-upgrade/skills/rails-upgrade-guide/references/compatibility-matrix.md
```

Expected: 11 matches (one per table header).

- [ ] **Step 5: Commit**

```bash
cd /Users/dhruva/src/oss/claude-rails-upgrade
git add skills/rails-upgrade-guide/references/compatibility-matrix.md
git commit -m "feat: add Rails 5.2 column to gem compatibility matrix"
```

---

### Task 8: ruby-version Sync Hook Edge Cases

The hook currently fails silently when `.ruby-version` uses the `ruby-3.3.1` prefix format (used by rbenv's legacy mode) and when the Gemfile uses a `~>` pessimistic constraint. Both are common and cause false "version mismatch" warnings.

**Files:**
- Modify: `hooks/scripts/ruby-version-sync.sh`

- [ ] **Step 1: Confirm the current hook does not strip the `ruby-` prefix**

```bash
grep -n "ruby-\|prefix\|strip" /Users/dhruva/src/oss/claude-rails-upgrade/hooks/scripts/ruby-version-sync.sh
```

Expected: zero matches for `prefix` or `strip` — the hook does not handle the `ruby-X.Y.Z` format.

- [ ] **Step 2: Test that the prefix causes a false positive**

```bash
# Simulate the edge case
echo "ruby-3.3.1" | tr -d '[:space:]' | cut -d. -f1-2
# Outputs: ruby-3 (wrong! should be "3.3")
```

- [ ] **Step 3: Replace `hooks/scripts/ruby-version-sync.sh` with the fixed version**

The fixed version handles: `ruby-X.Y.Z` prefix format, `~> X.Y` and `~> X.Y.Z` in Gemfile, and `>= X.Y.Z` constraints. Write the complete file:

```bash
#!/usr/bin/env bash
# PostToolUse hook: Validate .ruby-version and Gemfile ruby directive stay in sync.
# Fires when either .ruby-version or Gemfile is edited.
# Non-blocking — emits a warning but does not prevent the write.
#
# Handles .ruby-version formats:
#   3.3.1          (standard)
#   ruby-3.3.1     (rbenv legacy prefix)
#
# Handles Gemfile ruby directive formats:
#   ruby "3.3.1"       (exact version)
#   ruby "~> 3.3"      (minor-locked pessimistic constraint)
#   ruby "~> 3.3.1"    (patch-locked pessimistic constraint)
#   ruby ">= 3.2.0"    (minimum version — no mismatch check, log info only)
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

# Read .ruby-version (strip whitespace and optional "ruby-" prefix)
if [[ ! -f "$ruby_version_file" ]]; then
  exit 0
fi
rv_raw=$(cat "$ruby_version_file" | tr -d '[:space:]')
rv_version="${rv_raw#ruby-}"  # strip "ruby-" prefix if present

# Read Gemfile ruby directive
if [[ ! -f "$gemfile" ]]; then
  exit 0
fi
gemfile_raw=$(grep -E "^ruby ['\"]" "$gemfile" | head -1)
gemfile_version=$(echo "$gemfile_raw" | grep -oE "[0-9]+\.[0-9]+(\.[0-9]+)?" | head -1)

if [[ -z "$gemfile_version" ]]; then
  # No ruby directive in Gemfile — not an error, just no check possible
  exit 0
fi

# For >= constraints, comparison is not meaningful — emit info and exit
if echo "$gemfile_raw" | grep -q ">="; then
  echo "INFO: Gemfile uses ruby '>= $gemfile_version' (open constraint — skipping sync check)"
  exit 0
fi

# Compare major.minor (patch may legitimately differ between .ruby-version exact and ~> pin)
rv_minor=$(echo "$rv_version" | cut -d. -f1-2)
gf_minor=$(echo "$gemfile_version" | cut -d. -f1-2)

if [[ "$rv_minor" != "$gf_minor" ]]; then
  echo "WARNING: Ruby version mismatch detected:"
  echo "  .ruby-version: $rv_raw → normalized to $rv_version (minor: $rv_minor)"
  echo "  Gemfile ruby:  $gemfile_version (minor: $gf_minor)"
  echo "  Run: /ruby-upgrade-toolkit:fix ruby:$rv_version to reconcile"
fi

exit 0
```

- [ ] **Step 4: Verify the fix handles both formats correctly**

```bash
# Test 1: ruby- prefix stripped correctly
echo "ruby-3.3.1" | awk '{sub(/^ruby-/, ""); print}' | cut -d. -f1-2
# Expected output: 3.3

# Test 2: standard format unchanged
echo "3.3.1" | awk '{sub(/^ruby-/, ""); print}' | cut -d. -f1-2
# Expected output: 3.3

# Test 3: verify the file is executable
ls -la /Users/dhruva/src/oss/claude-rails-upgrade/hooks/scripts/ruby-version-sync.sh | awk '{print $1}'
# Expected: starts with -rwx
```

- [ ] **Step 5: Ensure the file is executable**

```bash
chmod +x /Users/dhruva/src/oss/claude-rails-upgrade/hooks/scripts/ruby-version-sync.sh
```

- [ ] **Step 6: Commit**

```bash
cd /Users/dhruva/src/oss/claude-rails-upgrade
git add hooks/scripts/ruby-version-sync.sh
git commit -m "fix: handle ruby- prefix and >= constraints in ruby-version sync hook"
```

---

## Self-Review

**Spec coverage check:**
- ✅ Task 1: Pre-existing test failure warning — audit (Step 2) + fix (Step 6)
- ✅ Task 2: Keyword argument Pattern B detection — audit (Step 3) + fix (Step 4)
- ✅ Task 3: Smarter `it` detection — audit (Step 3) + fix (Step 4)
- ✅ Task 4: Bundle install failure parsing — fix (Step 3)
- ✅ Task 5: Open redirect confirmation flow — fix (Step 5d) + audit (Step 4)
- ✅ Task 6: Turbo/Turbolinks guide — new reference file + SKILL.md + audit + fix
- ✅ Task 7: Rails 5.x compatibility matrix — compatibility-matrix.md
- ✅ Task 8: Hook edge cases — ruby-version-sync.sh

**Placeholder scan:** No TBD, TODO, "implement later", or vague steps found. Every code block contains actual commands or file content.

**Type consistency:** No types or method signatures used across tasks — all changes are independent Markdown/Bash edits.
