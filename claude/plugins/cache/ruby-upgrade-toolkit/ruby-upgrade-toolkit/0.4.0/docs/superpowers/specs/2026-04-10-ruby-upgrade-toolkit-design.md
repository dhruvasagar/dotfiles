# ruby-upgrade-toolkit: Plugin Refactor Design

**Date:** 2026-04-10
**Status:** Approved
**Scope:** Full refactor of `claude-rails-upgrade` into `ruby-upgrade-toolkit`

---

## Problem Statement

The original `claude-rails-upgrade` plugin was Rails-centric by name and structure, despite Ruby version upgrades being an equally important — and often prerequisite — concern. The 7-command surface was granular but disconnected, with no explicit slash command files (so no namespacing), duplicate auditor agents, a Rails-specific hook that didn't apply to pure Ruby projects, and a naming convention that excluded the broader use case.

---

## Goals

- Support Ruby version upgrades, Rails version upgrades, and coordinated Ruby+Rails upgrades from a single plugin
- Collapse 7 granular commands into 4 high-level commands with a clear canonical workflow
- Namespace all slash commands under the plugin name to avoid collisions with other plugins
- Replace two separate auditor agents with one context-aware agent
- Replace a Rails-specific hook with a Ruby-centric one that's relevant to both use cases
- Produce thorough documentation with a clear usage sequence and concrete examples

---

## Non-Goals

- Supporting non-Bundler Ruby projects
- Supporting non-RSpec/Minitest test frameworks
- Automatically pushing changes or creating PRs

---

## Plugin Identity

| Field              | Value                                                                                                                                  |
|--------------------|----------------------------------------------------------------------------------------------------------------------------------------|
| Name               | `ruby-upgrade-toolkit`                                                                                                                 |
| Marketplace source | `./`                                                                                                                                   |
| Description        | Upgrade Ruby and Rails safely: audit breaking changes, plan a phased roadmap, fix code and dependencies, verify with RSpec and RuboCop |

---

## Directory Structure

```
ruby-upgrade-toolkit/
├── .claude-plugin/
│   ├── plugin.json
│   └── marketplace.json
├── commands/                        ← NEW
│   ├── plan.md
│   ├── audit.md
│   ├── fix.md
│   └── status.md
├── skills/
│   ├── plan/
│   │   └── SKILL.md                ← replaces: upgrade-plan + ruby-upgrade-guide
│   ├── audit/
│   │   └── SKILL.md                ← replaces: deprecation-audit
│   │                                           + gem-compatibility-check
│   │                                           + migration-safety-check
│   ├── fix/
│   │   └── SKILL.md                ← replaces: fix-deprecations + config-upgrade
│   ├── status/
│   │   └── SKILL.md                ← replaces: upgrade-status
│   └── rails-upgrade-guide/        ← kept as internal reference only
│       ├── SKILL.md
│       └── references/
│           ├── rails-5-to-6.md
│           ├── rails-6-to-7.md
│           └── rails-7-to-8.md
├── agents/
│   ├── upgrade-auditor.md          ← replaces: rails-upgrade-auditor
│   │                                           + ruby-upgrade-auditor
│   └── deprecation-fixer.md        ← kept, scoped to fix skill logic
├── hooks/
│   ├── hooks.json
│   └── scripts/
│       ├── block-vendor.sh         ← kept unchanged
│       ├── rubocop-fix.sh          ← kept unchanged
│       └── ruby-version-sync.sh   ← NEW, replaces log-migration.sh
└── README.md                       ← fully rewritten
```

### Files deleted

- `skills/upgrade-plan/`
- `skills/deprecation-audit/`
- `skills/fix-deprecations/`
- `skills/gem-compatibility-check/`
- `skills/migration-safety-check/`
- `skills/config-upgrade/`
- `skills/upgrade-status/`
- `skills/ruby-upgrade-guide/` (content absorbed into `skills/plan/`)
- `agents/rails-upgrade-auditor.md`
- `agents/ruby-upgrade-auditor.md`
- `hooks/scripts/log-migration.sh`

---

## Command Design

### Argument syntax

All commands follow `key:value` pairs. Current versions are always auto-detected from `.ruby-version`, `Gemfile`, and `Gemfile.lock`. Only target versions are specified.

```
/ruby-upgrade-toolkit:audit ruby:3.3.10
/ruby-upgrade-toolkit:audit ruby:3.3.10 rails:8.0
/ruby-upgrade-toolkit:plan  ruby:3.3.10 rails:8.0
/ruby-upgrade-toolkit:fix   ruby:3.3.10 rails:8.0
/ruby-upgrade-toolkit:fix   ruby:3.3.10 rails:8.0 scope:app/models/
/ruby-upgrade-toolkit:status
```

| Argument     | Required on            | Description                                                   |
|--------------|------------------------|---------------------------------------------------------------|
| `ruby:X.Y.Z` | `audit`, `plan`, `fix` | Target Ruby version                                           |
| `rails:X.Y`  | optional on all        | Target Rails version; when present, Rails upgrade is included |
| `scope:path` | optional on `fix`      | Narrow fix to a file or directory                             |

### Command responsibilities

**`/ruby-upgrade-toolkit:audit`**
Read-only. Run this first to understand the full scope of work before touching any code.
- Detects current Ruby and Rails versions
- Identifies Ruby version-specific breaking changes that apply to the codebase
- Captures Rails deprecation warnings from the test suite (if Rails present)
- Checks gem compatibility against target Ruby and Rails versions
- Checks migration safety (if Rails present)
- Reports RuboCop `TargetRubyVersion` gap
- Produces a prioritized findings report with effort estimate
- Never modifies any file

**`/ruby-upgrade-toolkit:plan`**
Generates a phased, ordered upgrade roadmap tailored to the specific project.
- Reads audit findings (or runs a lightweight version of audit if not yet run)
- Phases Ruby upgrade steps first, Rails steps second
- Each phase ends with: RSpec green + RuboCop clean + `status` GREEN
- Flags CI/CD and Docker files that need manual updates
- Outputs the plan as a Markdown document

**`/ruby-upgrade-toolkit:fix`**
Applies changes iteratively. The core execution skill.
- Updates `.ruby-version`, `Gemfile` Ruby pin
- Updates gem dependencies for Ruby and Rails version compatibility
- Applies Ruby version-specific code fixes (keyword args, YAML, stdlib removals, etc.)
- Applies Rails deprecation fixes (enum syntax, filter→action, redirect patterns, etc.)
- Updates Rails config files and framework defaults
- Runs RSpec iteratively until green
- Runs RuboCop (`-a` then `-A`) iteratively until clean
- After each phase, emits a summary and recommends running `status`
- Flags CI/CD and Docker files for manual update (never modifies them)

**`/ruby-upgrade-toolkit:status`**
Current health dashboard. Run after each fix phase to verify before proceeding.
- Reports current Ruby and Rails versions vs. targets
- Test suite pass/fail with counts
- Deprecation warning count
- Gem compatibility signal
- Overall readiness: RED / YELLOW / GREEN
- Suggested next step

---

## Canonical Workflow

```
audit → plan → fix (loop) → status (checkpoint after each phase)
```

### Why this order

1. **audit first** — read-only, zero risk. Surfaces all breaking changes before any code is touched. Prevents surprises mid-fix.
2. **plan second** — uses audit findings to sequence work correctly. A Ruby 2.7→3.0 keyword argument fix must happen before the Rails 7→8 enum fix; the plan enforces this order.
3. **fix in phases** — applies changes one phase at a time. Each phase must be green before the next begins. Never attempt a bulk fix across Ruby and Rails simultaneously.
4. **status as checkpoint** — confirms each phase is clean. RED after a fix phase means do not proceed — diagnose and re-run fix.

### Example: coordinated Ruby + Rails upgrade

```
# Step 1: understand what you're dealing with
/ruby-upgrade-toolkit:audit ruby:3.3.10 rails:8.0

# Step 2: generate the phased plan
/ruby-upgrade-toolkit:plan ruby:3.3.10 rails:8.0

# Step 3a: apply Ruby upgrade fixes first
/ruby-upgrade-toolkit:fix ruby:3.3.10

# Step 3b: checkpoint
/ruby-upgrade-toolkit:status

# Step 3c: apply Rails upgrade fixes
/ruby-upgrade-toolkit:fix ruby:3.3.10 rails:8.0

# Step 3d: final checkpoint
/ruby-upgrade-toolkit:status
```

### Example: Ruby-only upgrade (no Rails)

```
/ruby-upgrade-toolkit:audit ruby:3.2.0
/ruby-upgrade-toolkit:plan  ruby:3.2.0
/ruby-upgrade-toolkit:fix   ruby:3.2.0
/ruby-upgrade-toolkit:status
```

### Example: targeted fix on a single file

```
/ruby-upgrade-toolkit:fix ruby:3.3.10 rails:8.0 scope:app/models/user.rb
```

### Example: mid-upgrade status check

```
/ruby-upgrade-toolkit:status
```
Run at any time during the upgrade to see current health without making changes.

---

## Agent Design

### `upgrade-auditor`

Replaces both `rails-upgrade-auditor` and `ruby-upgrade-auditor`.

**Trigger:** User describes upgrade intent in natural language — e.g., "I need to upgrade Ruby to 3.3", "upgrade this app from Rails 7 to 8", "what would it take to get to Ruby 3.2 and Rails 8?"

**Behavior:**
- Detects whether the project has Rails (checks for `config/application.rb`, Rails in `Gemfile`)
- Runs audit logic appropriate to the context: Ruby-only audit for plain Ruby projects, full Ruby+Rails audit for Rails apps
- Produces the same structured findings report as the `audit` skill
- Read-only — never modifies files

**Model:** inherit
**Color:** cyan

### `deprecation-fixer`

Kept as-is. Fires when user asks to fix deprecations in a specific file or directory. Runs a scoped version of `fix` skill logic rather than duplicating it.

---

## Hook Design

### `block-vendor.sh` (PreToolUse: Write|Edit) — kept

Blocks any write to `vendor/`. Applies to both Ruby and Rails projects.

### `rubocop-fix.sh` (PostToolUse: Write|Edit) — kept

Opt-in. Auto-corrects Style/Layout cops on edited `.rb` files.
Enable: `touch .ruby-upgrade-toolkit-rubocop`
Disable: `rm .ruby-upgrade-toolkit-rubocop`
**Migration note:** replaces `.rails-upgrade-rubocop` flag from the old plugin name. Existing users must re-create the flag file.

### `ruby-version-sync.sh` (PostToolUse: Write|Edit) — NEW

Replaces `log-migration.sh`. Fires when `.ruby-version` or `Gemfile` is edited.
Checks that the Ruby version in `.ruby-version` matches the `ruby` directive in `Gemfile`.
If they differ, emits a warning message but does not block the write.

---

## Documentation Plan

`README.md` sections:

1. **Overview** — what the plugin does, who it's for, Ruby vs Ruby+Rails support
2. **Prerequisites** — Ruby + rbenv/rvm, Bundler, RuboCop in Gemfile, `jq` for hooks
3. **Installation** — marketplace command, local dev path
4. **Workflow** — the four-step sequence with rationale for the order
5. **Command reference** — each command with argument table and 3–4 examples
6. **Agents** — when they fire, what they produce
7. **Hooks** — automatic vs opt-in, how to enable/disable rubocop hook
8. **Contributing** — how to extend for new version pairs

---

## Implementation Order

1. Update `plugin.json` and `marketplace.json`
2. Create `commands/` directory with 4 command files
3. Write `skills/plan/SKILL.md`
4. Write `skills/audit/SKILL.md`
5. Write `skills/fix/SKILL.md`
6. Write `skills/status/SKILL.md`
7. Update `skills/rails-upgrade-guide/SKILL.md` (mark as internal reference)
8. Write `agents/upgrade-auditor.md` (unified)
9. Update `agents/deprecation-fixer.md` (minor: reference fix skill)
10. Write `hooks/scripts/ruby-version-sync.sh`
11. Update `hooks/hooks.json`
12. Delete old skill directories, old agents, `log-migration.sh`
13. Rewrite `README.md`
