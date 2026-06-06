# Rails 6 → 7 Breaking Changes & Migration Guide

## Minimum Ruby Version

Rails 7.0 requires Ruby **2.7+**. Rails 7.1 requires Ruby **2.7+** (3.0+ recommended).

## Key New Features (understand before upgrading)

- **Import maps** — replaces Webpacker as default (no Node.js required)
- **Hotwire** (Turbo + Stimulus) — replaces Turbolinks + UJS
- **Active Record encryption** — `encrypts :column_name`
- **Strict loading** improvements
- **Zeitwerk** fully enforced (no Classic mode fallback)
- **Rails 7.1**: `generates_token_for`, `normalizes`, async queries, `ActiveRecord::Base.generate_unique_token`

## Breaking Changes

### JavaScript / Turbolinks → Turbo

The most common pain point for Rails 6 → 7.

- `Turbolinks.visit` → `Turbo.visit`
- `turbolinks:load` event → `turbo:load`
- UJS (`rails-ujs`) → `@hotwired/turbo-rails`
- `data-remote="true"` → `data-turbo-frame` or Turbo Streams
- jQuery UJS (`jquery_ujs`) must be replaced

**Identify usages:**
```bash
grep -r "turbolinks\|data-remote\|rails-ujs\|jquery_ujs" app/javascript app/views
```

### Active Record

- `where.not` with multiple conditions now generates `NOT (a AND b)` instead of `NOT a AND NOT b` — **query behavior change, verify results**
- `has_many :through` no longer accepts `source_type` with polymorphic associations without explicit configuration
- `ActiveRecord::Base.logger` — changing in multi-db setups
- `update_counters` skips validations — now explicit
- Eager loading with `includes` + `references` — stricter SQL generation

### Action Controller

- `force_ssl` config option removed — use `config.assume_ssl` and `ActionDispatch::SSL` middleware
- `protect_from_forgery` — token form changed; verify CSRF handling in JS-heavy apps
- `redirect_to` with `status:` — `302` now uses `GET` method (was preserving method) — aligns with HTTP spec

### Active Storage

- Service URLs now expire — cached URLs in external systems will break
- `has_one_attached :file, dependent: :purge_later` is new default
- Direct upload presigned URL format changed

### Action Mailer

- `deliver_later` — now enqueued with `ActionMailer::MailDeliveryJob` (same as 6.1)
- `mail(to:)` — keyword argument enforcement

### Active Job

- `retry_on` / `discard_on` — exception matching is now more strict
- Serialization of `GlobalID` objects — verify custom serializers

### Action View

- `content_tag` and `tag` helpers — void elements no longer accept blocks (raises error)
- `form_with` — `local: true` is now the default (was `false` in 6.0)

### Rails 7.1 Specific

- `ActiveRecord::Base.generate_unique_token` — new, replaces `SecureRandom.uuid` patterns
- `config.active_record.query_log_tags_enabled` — default true (adds SQL comments)
- Template rendering — stricter HTML safety enforcement in some edge cases

## Configuration Changes

```ruby
# config/application.rb
config.load_defaults 7.0  # or 7.1

# New defaults enabled by load_defaults 7.0:
# - config.action_controller.raise_on_open_redirects = true  ← HIGH IMPACT
# - config.action_view.button_to_generates_button_tag = true
# - config.action_view.apply_stylesheet_media_default = false
# - config.active_support.executor_around_test_case = true
# - config.active_record.verify_foreign_keys_for_fixtures = true
# - config.active_record.partial_inserts = false  ← Can break INSERT queries

# config.action_controller.raise_on_open_redirects = true
# CRITICAL: Any redirect_to with user-controlled input will now raise
# Fix: Use allow_other_host: true explicitly where needed, or validate URLs
```

**`raise_on_open_redirects` is the #1 upgrade blocker for Rails 6 → 7.** Search for all `redirect_to` calls with external URL logic.

## Open Redirect Audit

```bash
# Find potentially dangerous redirect_to calls
grep -rn "redirect_to.*params\|redirect_to.*request\|redirect_to.*session" app/controllers/
```

## Gem Compatibility Notes

| Gem | Required version for Rails 7 |
|-----|------------------------------|
| devise | >= 4.8.1 |
| pundit | >= 2.2 |
| kaminari | >= 1.2.2 |
| sidekiq | >= 6.5 |
| carrierwave | >= 3.0 |
| active_storage_validations | >= 1.0 |
| ransack | >= 3.0 |
| friendly_id | >= 5.4.2 |
| paper_trail | >= 12.0 |
| draper | >= 4.0 |
| rolify | >= 6.0 |

## Upgrade Commands

```bash
# 1. Update Gemfile
gem 'rails', '~> 7.1'

# 2. Bundle
bundle update rails

# 3. Run app:update
bin/rails app:update

# 4. Audit open redirects
grep -rn "redirect_to.*params\|redirect_to.*request" app/controllers/

# 5. Migrate Turbolinks → Turbo if using JS
# 6. Run tests
bundle exec rspec
```
