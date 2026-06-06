# Deprecation Fix Patterns Reference

## Rails 5.x → 6.x Fixes

| Deprecated | Replacement | Auto-safe? |
|-----------|-------------|------------|
| `update_attributes(attrs)` | `update(attrs)` | Yes |
| `before_filter` / `after_filter` | `before_action` / `after_action` | Yes |
| `redirect_to :back` | `redirect_back(fallback_location: root_path)` | Yes |
| `require_dependency` | Remove (Zeitwerk auto-loads) | Yes |
| `render text:` | `render plain:` | Yes |
| `ActionController::Parameters#merge!` | `#merge` (returns new object) | Review |
| `assert_template` | `assert_select` or check `response.body` | Manual |
| `assigns(:var)` in controller tests | Use request specs / check response | Manual |
| `config.serve_static_files` | `config.public_file_server.enabled` | Yes (config file) |
| `find_by_<column>` dynamic finders | `find_by(column: value)` | Yes |
| `scope :name, where(...)` | `scope :name, -> { where(...) }` | Yes |

## Rails 6.x → 7.x Fixes

| Deprecated | Replacement | Auto-safe? |
|-----------|-------------|------------|
| `redirect_to params[:url]` | Add `allow_other_host: true` or validate URL | Review |
| `form_with local: false` | `form_with` (default is now local: true) | Review |
| `ActionController::Base.force_ssl` | `config.force_ssl = true` in environment file | Yes |
| `config.force_ssl` (old form) | `config.assume_ssl = true` | Yes |
| `ActiveRecord::Base.logger =` | Use `ActiveRecord::Base.connection.logger` | Review |
| `before_action :verify_authenticity_token` order | Review `protect_from_forgery prepend:` | Review |
| Turbolinks JS events | Turbo events (see turbo-rails migration guide) | Manual |

## Rails 7.x → 8.x Fixes

| Deprecated | Replacement | Auto-safe? |
|-----------|-------------|------------|
| `enum status: [...]` | `enum :status, [...]` | Yes |
| `enum status: { a: 0 }` | `enum :status, { a: 0 }` | Yes |
| `has_and_belongs_to_many` | `has_many :through` with join model | Manual |
| `ActiveRecord::Base.logger` (instance assignment) | Use initializer | Review |
| `response.success?` | `response.successful?` | Yes |
| `config.active_record.sqlite3_adapter_strict_strings_by_default` (old default) | No action needed (new default is stricter) | N/A |

## Universal Patterns (any version)

### `render text:` → `render plain:`
```ruby
# Before
render text: "Hello"
render text: "Error", status: 422

# After
render plain: "Hello"
render plain: "Error", status: 422
```

### `scope` without lambda
```ruby
# Before (emits warning, freezes at load time)
scope :active, where(active: true)

# After
scope :active, -> { where(active: true) }
```

### Dynamic finder methods
```ruby
# Before
User.find_by_email("alice@example.com")
User.find_all_by_role("admin")

# After
User.find_by(email: "alice@example.com")
User.where(role: "admin")
```

### `assert_template` and `assigns` (controller tests)

These were extracted to `rails-controller-testing` gem in Rails 5 and fully removed in Rails 6+.

**Migration**: Move logic to request specs (RSpec) or integration tests:
```ruby
# Before (controller spec)
expect(assigns(:user)).to eq(user)
assert_template :show

# After (request spec)
get user_path(user)
expect(response).to have_http_status(:ok)
expect(response.body).to include(user.name)
```

### `config.serve_static_files`
```ruby
# config/environments/production.rb

# Before
config.serve_static_files = ENV['RAILS_SERVE_STATIC_FILES'].present?

# After
config.public_file_server.enabled = ENV['RAILS_SERVE_STATIC_FILES'].present?
```
