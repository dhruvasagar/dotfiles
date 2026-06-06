# Rails 7 → 8 Breaking Changes & Migration Guide

## Minimum Ruby Version

Rails 8.0 requires Ruby **3.2+**.

## Key New Features (understand before upgrading)

- **Solid Cache** — DB-backed cache store (replaces Redis/Memcached for many apps)
- **Solid Queue** — DB-backed job queue (replaces Sidekiq/Delayed Job for many apps)
- **Solid Cable** — DB-backed Action Cable adapter
- **Propshaft** — new asset pipeline (simpler than Sprockets)
- **Kamal** — Docker-based deploy tool bundled with `rails new`
- **Authentication generator** — `bin/rails generate authentication`
- **Rate limiting** — `rate_limit` in controllers

## Breaking Changes

### Ruby Version

Upgrading Ruby 2.7/3.0/3.1 → 3.2 alongside Rails is the most common blocker:

- Keyword argument separation (fully enforced since Ruby 3.0) — all `**kwargs` issues should already be fixed
- `Proc.new` without block removed in Ruby 3.0
- Pattern matching stable in 3.2
- `it` as default block parameter (Ruby 3.4) — avoid naming variables `it`

### Active Record

- `Rails.application.config.active_record.sqlite3_adapter_strict_strings_by_default` — default true for new apps
- `has_and_belongs_to_many` — still works but emits deprecation; migrate to `has_many :through`
- `where` with array condition now uses `IN (?)` consistently — verify any raw SQL fragments
- `enum` definition syntax changed:
  ```ruby
  # Old (still works with warning)
  enum status: [:draft, :published]
  
  # New (preferred)
  enum :status, [:draft, :published]
  # or
  enum :status, draft: 0, published: 1
  ```
- `ActiveRecord::Base.logger` assignment deprecated at instance level

### Action Pack / Controllers

- `protect_from_forgery` — `prepend: false` is now the default (hooks run before other callbacks)
- `cookies.signed` and `cookies.encrypted` — key rotation API changed
- `ActionDispatch::Request#raw_post` — encoding handling tightened

### Asset Pipeline (Sprockets → Propshaft)

Rails 8 ships with **Propshaft** as default. Existing apps still use Sprockets unless migrated.

**Propshaft differences:**
- No manifest.js — asset paths are content-hashed automatically
- No `//= require` directives — use import maps or bundler
- `asset_path` helpers still work
- `image-url()` in CSS must use standard `url()` with Propshaft

**Migration path (optional — Sprockets still supported):**
```ruby
# Gemfile: replace sprockets-rails with propshaft
gem 'propshaft'
# Remove: gem 'sprockets-rails'
```

### Action View

- `link_to` with block — HTML escaping now stricter for user-provided content
- `turbo_frame_tag` — Turbo 8 changes default morphing behavior

### Active Job / Queuing

- `perform_later` — `ActiveJob::EnqueueError` now raised on enqueue failure (was silent)
- Solid Queue changes `queue_adapter` default for new apps — existing apps unaffected unless explicitly switching

### Action Mailer

- `preview_interceptors` — API unified with `interceptors`

### Active Storage

- Service configuration — `config/storage.yml` format unchanged but service class names normalized
- Variant processing — `image_processing` gem now required for variants (was implicit)

### Railties / Generators

- `bin/rails g scaffold` — generates authentication-aware scaffolds if auth is configured
- `config/environments/` — some deprecated options emit warnings or are removed:
  - `config.serve_static_files` removed (was `config.public_file_server.enabled`)
  - `config.static_cache_control` removed

## Configuration Changes

```ruby
# config/application.rb
config.load_defaults 8.0

# New defaults enabled by load_defaults 8.0:
# - config.action_dispatch.default_headers changes (X-XSS-Protection removed)
# - config.active_support.to_time_preserves_timezone = :zone
# - config.active_record.automatically_invert_plural_associations = true  ← Can break existing associations
# - config.action_controller.allow_deprecated_parameters_hash_equality = false
```

**`automatically_invert_plural_associations`** — Rails will now auto-detect the inverse association for `has_many`. This can change eager loading behavior. Test all `has_many` / `belongs_to` pairs.

## Solid Suite (Optional but Recommended)

```bash
# Add to Gemfile
gem "solid_cache"
gem "solid_queue"
gem "solid_cable"

# Install
bin/rails solid_cache:install
bin/rails solid_queue:install
bin/rails solid_cable:install
```

These require a database (can be separate SQLite file for SQLite apps, or same DB). They replace Redis/Sidekiq for apps that don't need high throughput.

## Gem Compatibility Notes

| Gem | Required version for Rails 8 |
|-----|------------------------------|
| devise | >= 4.9.3 |
| pundit | >= 2.3 |
| kaminari | >= 1.2.2 |
| sidekiq | >= 7.0 (or replace with Solid Queue) |
| carrierwave | >= 3.0 |
| ransack | >= 4.0 |
| paper_trail | >= 14.0 |
| turbo-rails | >= 2.0 |
| stimulus-rails | >= 1.3 |
| importmap-rails | >= 2.0 |

## Upgrade Commands

```bash
# 1. Update Ruby to 3.2+ first
ruby -v  # must be >= 3.2.0

# 2. Update Gemfile
gem 'rails', '~> 8.0'

# 3. Bundle
bundle update rails

# 4. Run app:update
bin/rails app:update

# 5. Check for enum deprecations
grep -rn "enum [a-z_]*:" app/models/

# 6. Check has_and_belongs_to_many
grep -rn "has_and_belongs_to_many" app/models/

# 7. Run tests
bundle exec rspec
```
