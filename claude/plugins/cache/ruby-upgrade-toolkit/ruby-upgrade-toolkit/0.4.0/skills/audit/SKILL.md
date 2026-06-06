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

> **If the test suite is FAILING before the upgrade begins, surface this as the first item in the findings report under a `## ⚠️ Pre-existing Failures` section. These failures are NOT caused by the upgrade. Do not count them against the Effort Estimate. Instruct the user to fix them before starting the upgrade or explicitly document them as known pre-existing issues.**

## Step 3: Ruby Breaking Changes Audit

Run these checks based on the Ruby version being upgraded FROM and TO.

### 2.7 → 3.0: Keyword argument separation (most impactful change in Ruby 3.x history)

```bash
# Pattern A — methods that accept **kwargs (callers must NOT pass plain hash)
echo "Pattern A — **kwargs method definitions:"
grep -rEn "def [a-z_]+\([^)]*\*\*[a-z_]+" app/ lib/ --include="*.rb" 2>/dev/null | wc -l

# Pattern A — methods with options hash default (callers must NOT pass **hash)
echo "Pattern A — options={} method definitions:"
grep -rEn "def [a-z_]+\([^)]*[a-z_]+ = \{\}" app/ lib/ --include="*.rb" 2>/dev/null | wc -l

# Pattern A call sites — hash variable passed without ** where kwargs expected
# NOTE: high noise — Rails params calls inflate this count; use live warnings below as the authoritative signal
echo "Pattern A — hash variables at call sites (approximate, do not use for Effort Estimate):"
grep -rEn "[a-z_]+\(options\)|[a-z_]+\(opts\)" app/ lib/ --include="*.rb" 2>/dev/null | grep -v "def " | wc -l

# Pattern B — double-splat used at call site where method expects positional hash
echo "Pattern B — **hash passed to method expecting positional hash:"
grep -rEn "\*\*options|\*\*opts|\*\*kwargs" app/ lib/ --include="*.rb" 2>/dev/null | grep -v "def " | wc -l

# Best signal: Ruby 2.7 live deprecation warnings (only run if upgrading FROM Ruby 2.7)
echo "Live 2.7 deprecation warnings (keyword arg warnings only):"
RUBYOPT="-W:deprecated" bundle exec rspec --no-color 2>&1 | grep -iE "keyword|hash.*keyword|keyword.*hash" | sort | uniq -c | sort -rn | head -20
```

**What to look for:**
- Pattern A method definitions + Pattern A call sites with mismatched hash: ArgumentError in Ruby 3.0 when caller passes `connect(options)` (plain hash) to a method with `def connect(**opts)`.
- Pattern B: `**hash` at call site passed to method with `def process(options = {})`: ArgumentError in Ruby 3.0 when caller passes `process(**opts)` to a method expecting a positional hash.
- Live warnings from 2.7 are the most reliable signal — each warning corresponds to a guaranteed break in 3.0.

### Any version: YAML.load without permitted_classes

```bash
grep -rn "YAML\.load\b\|Psych\.load\b" app/ lib/ config/ --include="*.rb" 2>/dev/null
```

Each match is a potential security issue and will fail in Psych 4 (Ruby 3.1+) with untrusted YAML.

### → 3.4: `it` as reserved block parameter (warnings in 3.2–3.3, breaks in 3.4)

Only a concern when upgrading TO Ruby 3.4. `it` as a bare method call inside a block is being reserved as the default block parameter.

```bash
# Step 1: Find bare `it` used as a receiver or assigned — most reliable signal
grep -rEn "^\s*it\." app/ lib/ --include="*.rb" 2>/dev/null | grep -v "^[[:space:]]*#" | head -20

# Step 2: Find bare `it` in expressions (Ruby one-liner — correct word boundaries, portable)
ruby -r find -e '
  Find.find("app", "lib") do |f|
    next unless f.end_with?(".rb") && File.file?(f)
    File.readlines(f).each_with_index do |line, i|
      next if line =~ /^\s*#/
      next if line =~ /\bit\s+['"'"'\"]/
      next if line =~ /\b(bit|commit|submit|permit|limit|edit|visit|digit|habit|orbit)\b/
      puts "#{f}:#{i+1}:#{line.chomp}" if line =~ /(?<![a-z_0-9])it(?![a-z_0-9?!])/
    end
  end
' 2>/dev/null | head -20
```

Each match requires manual inspection to confirm `it` is used as a block variable (not as an RSpec `it "..."` call or a symbol key like `it:` or the word in a string).

### 3.3 → 3.4: stdlib gem removals

```bash
for lib in base64 csv drb mutex_m nkf bigdecimal ostruct; do
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
echo "before_filter: $(grep -rEn 'before_filter|after_filter|around_filter' app/ --include='*.rb' 2>/dev/null | wc -l)"
echo "redirect_to :back: $(grep -rn 'redirect_to :back' app/ --include='*.rb' 2>/dev/null | wc -l)"
echo "require_dependency: $(grep -rn 'require_dependency' app/ lib/ --include='*.rb' 2>/dev/null | wc -l)"
echo "HABTM: $(grep -rn 'has_and_belongs_to_many' app/ --include='*.rb' 2>/dev/null | wc -l)"
echo "old enum syntax: $(grep -rEn '^ *enum [a-z_]+:' app/ --include='*.rb' 2>/dev/null | wc -l)"
echo "open redirect risk: $(grep -rn 'redirect_to.*params\[' app/controllers/ --include='*.rb' 2>/dev/null | wc -l) (requires security review — not auto-fixed)"
echo "render text:: $(grep -rn 'render text:' app/ --include='*.rb' 2>/dev/null | wc -l)"
echo "turbolinks gem: $(grep -n 'turbolinks' Gemfile 2>/dev/null | grep -v '^\s*#' | wc -l) (migrate to turbo-rails for Rails 7+)"
echo "turbolinks JS refs: $(grep -rn 'turbolinks' app/javascript/ app/assets/ --include='*.js' --include='*.coffee' 2>/dev/null | wc -l)"
echo "data-turbolinks attrs: $(grep -rn 'data-turbolinks' app/views/ --include='*.erb' --include='*.html' 2>/dev/null | wc -l)"
```

### Zeitwerk check (Rails 6+)

```bash
bundle exec rails zeitwerk:check 2>&1 | head -20
```

## Step 5: Gem Compatibility Audit

```bash
# All gems with current versions (match entries with a version in parens to avoid PLATFORMS section)
grep -E "^    [a-z][a-z0-9_-]+ \(" Gemfile.lock | sed 's/^ *//' | sort | head -50

# Check for outdated gems
bundle outdated 2>/dev/null | head -30
```

If `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/compatibility-matrix.md` exists, read it for Rails-adjacent gem compatibility guidance.

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
grep -rEn "execute|remove_column|drop_table|rename_column|change_column" db/migrate/ --include="*.rb" 2>/dev/null | grep -Ev "reversible|# safe" | wc -l

# Concurrent index safety
grep -rn "add_index" db/migrate/ --include="*.rb" 2>/dev/null | grep -v "algorithm: :concurrently" | wc -l

# Database adapter
grep -E "adapter:" config/database.yml 2>/dev/null | head -3
```

If `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/risky-patterns.md` exists, read it for detailed risky pattern guidance.

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

## ⚠️ Pre-existing Failures (include only if baseline was FAILING)
- [N] failures existed before the upgrade — not caused by this upgrade
- User should fix these independently or document them as known issues

## Test Suite Baseline
- Status: [PASSING / FAILING / NOT FOUND]
- [N] examples, [F] failures

## Critical Issues (will break on target version)
### Keyword Argument Mismatches (Ruby 3.0)
- Pattern A — **kwargs definitions: [N]
- Pattern A — opts={} definitions: [N]
- Pattern A — call site candidates (approximate): [N]
- Pattern B — **hash at call site: [N]
- Live 2.7 warnings (authoritative): [N found / 0]

### Unsafe YAML.load calls
- [N] occurrences across [N] files: [list files]

### Stdlib Removals (Ruby 3.4 only)
- [list: gem_name: N occurrences]

### `it` Variable Conflicts (Ruby 3.4)
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
| turbolinks gem | N |
| turbolinks JS refs | N |
| data-turbolinks attrs | N |

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
