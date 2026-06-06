---
description: Generate a phased Ruby (and optionally Rails) upgrade roadmap for this project. Detects current versions automatically. Usage: /ruby-upgrade-toolkit:plan ruby:X.Y.Z [rails:X.Y]
argument-hint: "ruby:X.Y.Z [rails:X.Y]"
---

Parse the arguments provided by the user:
- `ruby:X.Y.Z` — required. The target Ruby version to upgrade to.
- `rails:X.Y` — optional. When provided, the plan also covers upgrading Rails to this version.

Current versions are auto-detected — do not ask the user for them.

Read `$CLAUDE_PLUGIN_ROOT/skills/plan/SKILL.md` and follow its instructions completely, passing the parsed arguments.
