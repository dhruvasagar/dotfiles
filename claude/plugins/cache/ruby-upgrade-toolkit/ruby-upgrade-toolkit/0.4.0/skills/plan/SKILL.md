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
