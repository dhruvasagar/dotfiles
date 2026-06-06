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
gem pristine <gem_name>
# if not cached, reinstall:
gem install <gem_name>
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

Detection — find `**hash` at call sites where the method accepts `options = {}`:
```bash
grep -rEn "\*\*options|\*\*opts|\*\*params|\*\*kwargs" ${SCOPE:-app/ lib/} --include="*.rb" | grep -v "def "
```

For each match, read the method definition it is calling. If the method signature is `def method_name(options = {})` or `def method_name(opts = {})` (positional hash, NOT `**`), this is Pattern B:

```ruby
# BEFORE: double-splat on a method that takes options = {}
process(**{key: "value"})   # ArgumentError in Ruby 3.0
process(**user_opts)        # ArgumentError in Ruby 3.0 if process(opts={})

# AFTER: wrap literal hash in braces (positional), keep variable as-is
process({key: "value"})     # passes hash as positional argument
process(user_opts)          # remove ** — pass hash directly as positional arg
```

If the method definition is NOT available (external gem method), check the gem's changelog for Ruby 3.0 compatibility. Do not apply Pattern B fix to external gem call sites — update the gem instead.

Read each affected file, identify which pattern applies, and apply the minimal fix. Run the file's tests after each file:
```bash
bundle exec rspec spec/path/to/file_spec.rb --no-color 2>&1 | tail -5
```

### Any version: YAML.load → YAML.safe_load

```bash
grep -rEn "YAML\.load\b|Psych\.load\b" ${SCOPE:-app/ lib/ config/} --include="*.rb"
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

### → 3.4: `it` block parameter conflict (warnings in 3.2–3.3, breaks in 3.4)

Only run this section when upgrading to Ruby 3.4.

```bash
# Find `it` used as a block variable (receiver or assignment target)
grep -rEn "^\s*it\." ${SCOPE:-app/ lib/} --include="*.rb" 2>/dev/null | grep -v "^[[:space:]]*#"

# Broader search using Ruby for correct word-boundary detection (grep -P not available on macOS)
export SCOPE_DIR="${SCOPE:-app lib}"
ruby -r find -e '
  Find.find(*ENV["SCOPE_DIR"].split) do |f|
    next unless f.end_with?(".rb") && File.file?(f)
    File.readlines(f).each_with_index do |line, i|
      next if line =~ /^\s*#/
      next if line =~ /\bit\s+['"'"'\"]/
      next if line =~ /\b(bit|commit|submit|permit|limit|edit|visit|digit|habit|orbit)\b/
      puts "#{f}:#{i+1}:#{line.chomp}" if line =~ /(?<![a-z_0-9])it(?![a-z_0-9?!])/
    end
  end
' 2>/dev/null
```

For each match, read 5 lines of context around it to confirm `it` is truly a block parameter variable (not an RSpec test call). When confirmed, rename `it` to a descriptive name based on what the block iterates over (e.g., `it` in `items.each { it.save }` → `item`).

### 3.3 → 3.4: stdlib gem removals

If gems were added to Gemfile in Step 3, verify they load correctly:
```bash
bundle exec ruby -e "require 'base64'; puts 'ok'" 2>&1
```

## Step 5: Apply Rails Fixes (if `rails:` argument given)

Skip this entire section if no `rails:` argument was provided.

### 5a. Update Rails gem

Update the `rails` pin in `Gemfile` to `'~> X.Y'`, then:
```bash
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

If `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/fix-patterns.md` exists, read it for the full pattern table.

Apply these safe auto-fixes across the scope (or whole `app/` if no scope):

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
       # Note: must exclude '//' prefix — browsers treat '//evil.com' as protocol-relative
       path = params[:return_to].presence
       redirect_to(path&.start_with?('/') && !path.start_with?('//') ? path : root_path)

    B) Allowlist redirect: only allow specific domains
       # Define ALLOWED_HOSTS in config/application.rb or ApplicationController:
       #   ALLOWED_HOSTS = %w[app.example.com staging.example.com].freeze
       allowed_host = URI.parse(params[:return_to].to_s).host rescue nil
       redirect_to(allowed_host && ALLOWED_HOSTS.include?(allowed_host) ? params[:return_to] : root_path)

    C) Keep as-is (intentional external redirect — document why)

  Which option? [A/B/C]
```

Apply the chosen fix. Do not auto-apply open redirect fixes without this confirmation.

### 5e. Migrate Turbolinks → Turbo (Rails 7+ only)

Skip if not upgrading to Rails 7 or later.

```bash
grep -n "turbolinks" Gemfile 2>/dev/null | grep -v "^\s*#"
```

If found, follow `$CLAUDE_PLUGIN_ROOT/skills/rails-upgrade-guide/references/turbo-stimulus-guide.md` for the complete migration procedure. Key steps:
1. Add `gem "turbo-rails"` to Gemfile, `bundle install`, run `bin/rails turbo:install`
2. Remove `gem "turbolinks"` from Gemfile, `bundle install`
3. Update JavaScript event listeners (`turbolinks:load` → `turbo:load`)
4. Update data attributes (`data-turbolinks` → `data-turbo`)
5. Run RSpec to catch any view/JS integration regressions

### 5f. Update RuboCop target version

In `.rubocop.yml`, ensure `AllCops.TargetRubyVersion` matches the target Ruby minor version:
```yaml
AllCops:
  TargetRubyVersion: X.Y
  NewCops: enable
```

## Step 6: Iterative RSpec Loop

Before starting the fix loop, establish the baseline failure count. Use the failure count from the audit report if one was produced in this session. Otherwise capture it now (before any fixes have been applied):

```bash
BASELINE_FAILURES=$(bundle exec rspec --no-color --format progress 2>&1 | grep -oE "[0-9]+ failure" | grep -oE "[0-9]+" | head -1); echo "Baseline failures: ${BASELINE_FAILURES:-0}"
```

Keep `BASELINE_FAILURES` in context throughout Step 6. In the Step 8 summary, report only failures **above** this baseline as upgrade-introduced regressions. If the baseline already had failures, document them separately as pre-existing and do not attempt to fix them unless the user explicitly asks.

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
