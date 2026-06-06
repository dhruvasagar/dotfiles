# Turbo & Stimulus Migration Guide

Reference for migrating from Turbolinks (Rails 6) to Turbo (Rails 7+).
Applies when upgrading Rails 6.x → 7.x or later.

## Background

Rails 7 ships with Hotwire by default: **Turbo** (replaces Turbolinks) and **Stimulus** (JS framework).
Turbolinks still works in Rails 7 but is no longer maintained. Migration is strongly recommended before Rails 8.

## Step 1: Detect Turbolinks Usage

```bash
# Gem present
grep -n "turbolinks" Gemfile Gemfile.lock 2>/dev/null | grep -v "^\s*#"

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
```

Detect your JS pipeline, then run the appropriate installer:

```bash
# importmap (Rails 7 default — check if config/importmap.rb exists)
bin/rails turbo:install
bin/rails stimulus:install  # if using Stimulus

# jsbundling (esbuild/rollup/webpack — check for package.json with build script)
bin/rails turbo:install:node
bin/rails stimulus:install  # if using Stimulus

# Webpacker (legacy — check for config/webpacker.yml)
yarn add @hotwired/turbo-rails
# Then manually import in app/javascript/application.js — see Step 4
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
| `turbolinks:request-end` | `turbo:before-fetch-response` (fires before processing; no exact 1:1 equivalent — use `turbo:submit-end` for form responses or inspect `event.detail.fetchResponse` in `turbo:before-fetch-response`) |

## Step 6: Update Data Attributes

| Turbolinks attribute | Turbo equivalent |
|---------------------|-----------------|
| `data-turbolinks="false"` | `data-turbo="false"` |
| `data-turbolinks-track="reload"` | `data-turbo-track="reload"` |
| `data-turbolinks-action="replace"` | `data-turbo-action="replace"` |
| `data-turbolinks-permanent` | `data-turbo-permanent` |

**Find all data attributes to update:**
```bash
grep -rn "data-turbolinks" app/views/ --include="*.erb" --include="*.html" 2>/dev/null
```

## Step 7: Update ERB Cache Control Meta Tag

```erb
<%# BEFORE %>
<meta name="turbolinks-cache-control" content="no-cache">

<%# AFTER %>
<meta name="turbo-cache-control" content="no-cache">
```

## Step 8: Test

```bash
bundle exec rspec spec/ --no-color 2>&1 | tail -10
```

Check browser console for JS errors after deploying to staging.

## Common Issues

**`undefined method 'turbolinks_tag'`**: Remove calls to `turbolinks_tag` — Turbo does not use it.

**Page does not update after form submit**: Ensure `data: { turbo: true }` or that Turbo is loaded in the JS pipeline.

**Duplicate event fires on page load**: Change `turbolinks:load` to `turbo:load` in all JS files.
