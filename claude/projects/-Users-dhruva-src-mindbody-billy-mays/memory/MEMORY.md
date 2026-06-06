# Billy Mays Project Memory

## Project Overview
Rails 6.1 LTS app being upgraded from Ruby 2.7 to Ruby 3.3.10. Branch: `development-arcus`.

## Key Files Modified (Ruby 3.3.10 + Rails 6.1 Upgrade)

### Gem Fixes
- **`mindbody_internal_apis_gem` client.rb**: Fixed Ruby 3.x keyword arg compatibility in `register_client_methods`.
  - File: `/Users/dhruva/.asdf/installs/ruby/3.3.10/lib/ruby/gems/3.3.0/bundler/gems/Mindbody.RubyGems.mindbody_internal_apis_gem-8822f6d03c26/lib/mindbody_internal_apis/client.rb`
  - Changed `define_singleton_method(method_name) do |options = {}|` → `|**options|` and `api_client_klass.new(options)` → `api_client_klass.new(**options)`
  - This fixes `ArgumentError: wrong number of arguments (given 1, expected 0)` when calling `V1.network`, `V6.sites(...)`, `V6.classes(...)`

### App Files Modified
- **`Dockerfile`**: Bundler version, Chrome install, Debian Bookworm apt sources (`/etc/apt/sources.list.d/debian.sources`), `netcat-openbsd`
- **`Gemfile`**: sentry-raven → sentry-ruby + sentry-rails, spring upgrade, rubocop upgrades
- **`bin/rails`**: Added `ENV['OBJC_DISABLE_INITIALIZE_FORK_SAFETY'] = 'YES'` before Spring loads (macOS fork safety)
- **`config/routes.rb`**: Removed `Sidekiq::Web.set :session_secret` (Sidekiq 7 removed Sinatra API)
- **`config/initializers/sentry.rb`**: New file for Sentry configuration
- **`config/initializers/middleware_stack_patch.rb`**: Deleted — was determined to be unnecessary
- **`config/initializers/lograge.rb`**: Wrapped `CustomLogrageFormatter` in `unless defined?` guard
- **`config/initializers/datadog_tracer.rb`**: Added `unless defined?` guard for `DD_EXCLUDED_PROGRAM_NAMES`
- **`config/environments/test.rb`**: `cache_classes = false` (Spring), `perform_caching = false`, removed Sidekiq `namespace`/`size` options, cache_store = `:memory_store`
- **`config/environments/development.rb`**: Removed `maintain_test_schema!`, Sidekiq fixes, `:redis_cache_store` instead of `:redis_store`, fixed `frederick_url`
- **`config/environments/production.rb`**: `:redis_cache_store` instead of `:redis_store`, `Sidekiq.default_configuration[:concurrency]`
- **`app/models/video.rb`**: `Raven.capture_message` → `Sentry.capture_message`
- **`app/controllers/concerns/v2/api_controller_concern.rb`**: `Raven.capture_exception` → `Sentry.capture_exception` (2 occurrences)
- **`app/controllers/promotions_controller.rb`**: `Raven.capture_exception` → `Sentry.capture_exception` (2 occurrences)
- **`app/controllers/deep_links_controller.rb`**: `Raven.capture_exception` → `Sentry.capture_exception`

## Key Patterns/Issues Discovered
- **Ruby 3.x keyword arg migration**: Hash `{}` passed as positional to keyword-only `initialize` fails. Fix: use `**options` splat.
- **Debian Bookworm**: Uses `/etc/apt/sources.list.d/debian.sources` (DEB822 format), not `/etc/apt/sources.list`
- **Sidekiq 7**: Removed `namespace:`, `size:` from redis config; `Sidekiq.options` → `Sidekiq.default_configuration`
- **Spring + macOS**: `OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES` must be set in `bin/rails` BEFORE Spring forks
- **`:redis_store` → `:redis_cache_store`**: For Rails native cache versioning support
- **`cache_classes = false`**: Required for Spring; use `unless defined?` guards for constants
- **Sentry migration**: All `Raven.*` → `Sentry.*` (capture_exception, capture_message)

## Test Running
```bash
bundle exec rspec spec/requests/api/v2/videos_controller_requests_spec.rb
bundle exec cucumber
```

## Cucumber: Migrated from VCR to WebMock (DONE)
- **Why**: VCR cassettes are thread-local (`Thread.current[:_vcr_cassette_stack]`). With `allow_http_connections_when_no_cassette = true`, Puma's server thread saw an empty cassette stack and made real connections. Patching `current_cassette` was insufficient because VCR's WebMock integration checks `cassettes.empty?`, not `current_cassette`.
- **Solution**: Replaced VCR cucumber integration with direct WebMock stubs (global across threads).
- **Files**:
  - Deleted: `features/support/vcr.rb`
  - Created: `features/support/webmock_stubs.rb` — sets `WebMock.disable_net_connect!(allow_localhost: true)` globally; `Before` hook registers all stubs
  - Created: `features/fixtures/mindbody_api/*.json` — 10 fixture files for API responses
  - Updated: `features/interstitial_page.feature` — removed `@vcr` tag (kept `@javascript`)
- **VCR still used in RSpec** (`spec/support/vcr.rb` + 113 cassettes) — no change needed there
- **Key gotcha**: Rack/WebMock parses `ids%5B%5D=49` (Faraday array params) as `{"ids" => ["49"]}`, NOT `{"ids[]" => "49"}`. Use `hash_including("ids" => ["49"])` in stubs. Same for `siteIds%5B%5D=312` → `{"siteIds" => ["312"]}`.
- **`webmock/cucumber`** (`env.rb` already loads it): calls `WebMock.enable!` at load time, `WebMock.reset!` after each scenario. Our `Before` hook re-registers stubs after each reset.
