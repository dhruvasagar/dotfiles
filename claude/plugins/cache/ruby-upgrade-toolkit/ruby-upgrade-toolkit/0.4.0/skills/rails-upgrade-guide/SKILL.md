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
- **Turbo/Stimulus migration**: `references/turbo-stimulus-guide.md` — Turbolinks → Turbo migration for Rails 7+
