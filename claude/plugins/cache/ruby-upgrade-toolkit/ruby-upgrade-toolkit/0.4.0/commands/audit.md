---
description: Read-only pre-upgrade audit. Surfaces all breaking changes, gem incompatibilities, and deprecations before touching any code. Usage: /ruby-upgrade-toolkit:audit ruby:X.Y.Z [rails:X.Y]
argument-hint: "ruby:X.Y.Z [rails:X.Y]"
---

Parse the arguments provided by the user:
- `ruby:X.Y.Z` — required. The target Ruby version.
- `rails:X.Y` — optional. When provided, the audit also covers Rails upgrade concerns.

Current versions are auto-detected. This command never modifies any file.

Read `$CLAUDE_PLUGIN_ROOT/skills/audit/SKILL.md` and follow its instructions completely, passing the parsed arguments.
