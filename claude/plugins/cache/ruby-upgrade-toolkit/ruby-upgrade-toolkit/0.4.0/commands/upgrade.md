---
description: Full automated Ruby/Rails upgrade pipeline — audit, phased fixes, and verification with a live task list. Pauses on failure so you can investigate before continuing. Usage: /ruby-upgrade-toolkit:upgrade ruby:X.Y.Z [rails:X.Y]
argument-hint: "ruby:X.Y.Z [rails:X.Y]"
---

Parse the arguments provided by the user:
- `ruby:X.Y.Z` — required. The target Ruby version.
- `rails:X.Y` — optional. When provided, also orchestrates the Rails upgrade.

Current versions are auto-detected.

Read `$CLAUDE_PLUGIN_ROOT/skills/upgrade/SKILL.md` and follow its instructions completely, passing the parsed arguments.
