---
name: Always tag new plugin versions
description: When bumping the ruby-upgrade-toolkit plugin version, always create a matching git tag (vX.Y.Z) as part of the same release workflow.
type: feedback
originSessionId: ea941ffe-a3c7-4353-810a-b00db7570631
---
Whenever the version in `.claude-plugin/plugin.json` is bumped for a release, immediately create a matching annotated git tag in the format `vX.Y.Z` on the release commit.

**Why:** The plugin's auto-update mechanism and marketplace discovery rely on tags. A release without its tag is effectively invisible to users running `/plugin install` and breaks `git describe`-based version tracking. The user noticed v0.5.0–v0.8.1 shipped without tags and wants this made standard practice going forward.

**How to apply:** After any commit that changes the `version` field of `.claude-plugin/plugin.json`, run `git tag vX.Y.Z <commit-sha>` (annotated tag preferred: `git tag -a vX.Y.Z -m "Release vX.Y.Z"`). Confirm with the user before pushing the tag to origin — creating the tag is local-only and safe; pushing is the publish step and should be explicit.
