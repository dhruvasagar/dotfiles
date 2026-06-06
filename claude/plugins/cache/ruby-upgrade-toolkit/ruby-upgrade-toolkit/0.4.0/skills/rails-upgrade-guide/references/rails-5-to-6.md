# Rails 5 → 6 Breaking Changes & Migration Guide

## Minimum Ruby Version

Rails 6.0 requires Ruby **2.5+**. Rails 6.1 requires Ruby **2.5+** (2.7+ recommended).

## Key New Features (understand before upgrading)

- **Multiple databases** — `connects_to`, `connected_to` API
- **Action Mailbox** — inbound email routing
- **Action Text** — rich text with Trix
- **Webpacker** replaces Sprockets as default JS bundler (Sprockets still available)
- **Zeitwerk** autoloader replaces Classic autoloader

## Breaking Changes

### Autoloader: Classic → Zeitwerk

The most impactful change. Zeitwerk enforces that file names match constant names exactly.

**Identify broken files:**
```bash
bundle exec rails zeitwerk:check
```

**Common fixes:**
- `lib/my_module/foo_bar.rb` must define `MyModule::FooBar` (not `MyModule::Foo_bar`)
- Acronyms need explicit inflection rules:
  ```ruby
  # config/initializers/inflections.rb
  ActiveSupport::Inflector.inflections(:en) do |inflect|
    inflect.acronym 'API'
  end
  ```
- `require_dependency` calls → remove (Zeitwerk handles this)
- `autoload` calls → remove (Zeitwerk handles this)

### Action Cable

- `ActionCable.startDebugging()` / `stopDebugging()` removed — use browser devtools
- Channel callbacks now use keyword arguments

### Active Record

- `update_attributes` → `update` (was deprecated in 5.x, removed in 6.0)
- `save(validate: false)` — still works but review usage
- `default_scope` with `where` now merges with `all` correctly — verify expected query behavior
- `ActiveRecord::Base.allow_concurrency` removed

### Active Storage

- `has_one_attached` / `has_many_attached` — variants API changed slightly
- Direct uploads require `ActiveStorage::DirectUploadsController` route

### Action View

- `render partial:` no longer accepts a string that matches a partial without leading underscore
- `image_tag` now generates width/height attributes by default (can disable)

### Action Mailer

- Delivery jobs now use `ActionMailer::MailDeliveryJob` (was `ActionMailer::DeliveryJob`)
- Update any custom queue configuration targeting the old job class name

### Routing

- `redirect` with a hash: `redirect(path: '/new')` syntax changed — use `redirect('/new')`
- `ActionDispatch::Http::UploadedFile` — interface stable but verify file upload handling

### Test Framework

- `ActionDispatch::IntegrationTest` — `assigns` and `assert_template` removed (use `assert_select`, check `response.body`)
- Install `rails-controller-testing` gem if you relied on `assigns`

### JavaScript / Asset Pipeline

- Webpacker is now the default for JS — existing Sprockets JS still works
- `//= require` in `.js` files no longer processed by Webpack
- Review `app/javascript/` vs `app/assets/javascripts/` split

## Configuration Changes

```ruby
# config/application.rb — add after upgrading
config.load_defaults 6.0  # or 6.1

# New defaults enabled by config.load_defaults 6.0:
# - config.action_view.default_enforce_utf8 = false
# - config.action_dispatch.use_cookies_with_metadata = true
# - config.action_mailer.delivery_job = "ActionMailer::MailDeliveryJob"
# - config.active_record.collection_cache_versioning = true
```

## Gem Compatibility Notes

| Gem | Required version for Rails 6 |
|-----|------------------------------|
| devise | >= 4.7 |
| pundit | >= 2.1 |
| kaminari | >= 1.2 |
| sidekiq | >= 6.0 |
| carrierwave | >= 2.1 |
| paperclip | **INCOMPATIBLE** — migrate to Active Storage |
| delayed_job | >= 4.1.9 |
| ransack | >= 2.3 |

## Upgrade Commands

```bash
# 1. Update Gemfile
gem 'rails', '~> 6.1'

# 2. Bundle
bundle update rails

# 3. Run app:update (review each diff carefully)
bin/rails app:update

# 4. Check Zeitwerk compliance
bin/rails zeitwerk:check

# 5. Run tests
bundle exec rspec  # or bundle exec rails test
```
