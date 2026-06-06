# Gem Compatibility Matrix

Minimum gem versions required for each Rails version.
Last updated: 2026-04.

## Core Ecosystem

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| devise | >= 4.5.0 | >= 4.7.0 | >= 4.7.1 | >= 4.8.1 | >= 4.9.2 | >= 4.9.3 |
| pundit | >= 1.1 | >= 2.0 | >= 2.1 | >= 2.2 | >= 2.3 | >= 2.3.1 |
| kaminari | >= 1.1.0 | >= 1.2.0 | >= 1.2.1 | >= 1.2.2 | >= 1.2.2 | >= 1.2.2 |
| will_paginate | >= 3.1.7 | >= 3.3.0 | >= 3.3.0 | >= 4.0.0 | >= 4.0.0 | >= 4.0.0 |
| ransack | >= 1.8 | >= 2.3.0 | >= 2.4.0 | >= 3.0.0 | >= 4.0.0 | >= 4.1.0 |
| friendly_id | >= 5.2.3 | >= 5.3.0 | >= 5.4.0 | >= 5.4.2 | >= 5.5.0 | >= 5.5.0 |
| rolify | >= 5.2.0 | >= 5.3.0 | >= 6.0.0 | >= 6.0.0 | >= 6.0.0 | >= 6.0.0 |

## Background Jobs

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| sidekiq | >= 5.0 | >= 6.0 | >= 6.1 | >= 6.4 | >= 7.0 | >= 7.0 |
| delayed_job | >= 4.1.5 | >= 4.1.9 | >= 4.1.9 | >= 4.1.11 | >= 4.1.11 | >= 4.1.11 |
| resque | >= 1.27 | >= 2.0 | >= 2.0 | >= 2.3 | >= 2.3 | >= 2.6 |
| que | >= 0.14 | >= 1.3 | >= 1.3 | >= 2.0 | >= 2.0 | >= 2.0 |
| good_job | not supported | >= 1.0 | >= 2.0 | >= 3.0 | >= 3.10 | >= 3.21 |

## File Uploads

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| carrierwave | >= 1.3 | >= 2.1 | >= 2.1 | >= 3.0 | >= 3.0 | >= 3.0 |
| shrine | >= 2.18 | >= 3.0 | >= 3.0 | >= 3.3 | >= 3.4 | >= 3.4 |
| paperclip | **INCOMPATIBLE** | **INCOMPATIBLE** | **INCOMPATIBLE** | **INCOMPATIBLE** | **INCOMPATIBLE** | **INCOMPATIBLE** |
| active_storage_validations | >= 0.7 | >= 0.9 | >= 0.9 | >= 1.0 | >= 1.1 | >= 2.0 |
| image_processing | >= 1.7 | >= 1.10 | >= 1.10 | >= 1.12 | >= 1.12 | >= 1.12 |

## API / Serialization

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| jsonapi-serializer | >= 2.0 | >= 2.1 | >= 2.1 | >= 2.2 | >= 2.2 | >= 2.2 |
| active_model_serializers | >= 0.10.7 | >= 0.10.12 | >= 0.10.12 | >= 0.10.13 | investigate | investigate |
| blueprinter | >= 0.20 | >= 0.25 | >= 0.25 | >= 1.0 | >= 1.0 | >= 1.0 |
| jbuilder | >= 2.7 | >= 2.9 | >= 2.11 | >= 2.11 | >= 2.11 | >= 2.11 |
| fast_jsonapi | **ABANDONED** | **ABANDONED** | **ABANDONED** | **ABANDONED** | **ABANDONED** | **ABANDONED** |

## Authentication / Authorization

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| cancancan | >= 3.0 | >= 3.3 | >= 3.3 | >= 3.4 | >= 3.5 | >= 3.6 |
| jwt | >= 2.1 | >= 2.3 | >= 2.3 | >= 2.4 | >= 2.7 | >= 2.7 |
| bcrypt | >= 3.1.12 | >= 3.1.13 | >= 3.1.13 | >= 3.1.16 | >= 3.1.18 | >= 3.1.18 |
| doorkeeper | >= 5.2 | >= 5.4 | >= 5.5 | >= 5.6 | >= 5.7 | >= 5.7 |
| omniauth | >= 1.9 | >= 2.0 | >= 2.0 | >= 2.1 | >= 2.1 | >= 2.1 |

## Admin

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| activeadmin | >= 2.1 | >= 2.9 | >= 2.11 | >= 3.0 | >= 3.1 | >= 4.0 |
| administrate | >= 0.11 | >= 0.16 | >= 0.17 | >= 0.18 | >= 0.20 | >= 1.0 |

## Testing

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| rspec-rails | >= 3.8 | >= 4.0 | >= 4.1 | >= 5.0 | >= 6.0 | >= 7.0 |
| factory_bot_rails | >= 5.0 | >= 6.1 | >= 6.1 | >= 6.2 | >= 6.2 | >= 6.4 |
| shoulda-matchers | >= 4.0 | >= 4.4 | >= 4.5 | >= 5.0 | >= 5.1 | >= 5.3 |
| capybara | >= 3.18 | >= 3.30 | >= 3.35 | >= 3.36 | >= 3.39 | >= 3.40 |
| vcr | >= 5.0 | >= 6.0 | >= 6.0 | >= 6.1 | >= 6.2 | >= 6.2 |
| webmock | >= 3.6 | >= 3.12 | >= 3.14 | >= 3.14 | >= 3.18 | >= 3.23 |
| database_cleaner-active_record | >= 1.8 | >= 2.0 | >= 2.0 | >= 2.1 | >= 2.1 | >= 2.1 |

## Auditing / Versioning

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| paper_trail | >= 9.2 | >= 10.3 | >= 12.0 | >= 12.3 | >= 13.0 | >= 14.0 |
| audited | >= 4.9 | >= 5.0 | >= 5.0 | >= 5.2 | >= 5.3 | >= 5.4 |

## Search

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| pg_search | >= 2.1 | >= 2.3 | >= 2.3 | >= 2.3.5 | >= 2.3.6 | >= 2.3.6 |
| searchkick | >= 4.0 | >= 5.0 | >= 5.1 | >= 5.2 | >= 5.3 | >= 5.4 |
| elasticsearch-rails | >= 6.0 | >= 7.1 | >= 7.1 | >= 7.2 | >= 8.0 | >= 8.0 |

## Utilities

| Gem | Rails 5.2 | Rails 6.0 | Rails 6.1 | Rails 7.0 | Rails 7.1 | Rails 8.0 |
|-----|-----------|-----------|-----------|-----------|-----------|-----------|
| draper | >= 3.1 | >= 4.0 | >= 4.0 | >= 4.0.1 | >= 4.0.2 | investigate |
| acts-as-taggable-on | >= 6.0 | >= 8.0 | >= 9.0 | >= 10.0 | >= 10.0 | >= 10.0 |
| enumerize | >= 2.3 | >= 2.5 | >= 2.5 | >= 2.6 | >= 2.7 | >= 2.7 |
| aasm | >= 5.0 | >= 5.2 | >= 5.2 | >= 5.4 | >= 5.5 | >= 5.5 |
| state_machines-activerecord | >= 0.7 | >= 0.8 | >= 0.8 | >= 0.9 | >= 0.9 | >= 0.10 |
| money-rails | >= 1.13 | >= 1.15 | >= 1.15 | >= 1.15.0 | >= 1.15.0 | investigate |

## Incompatible / Abandoned Gems (must replace)

| Gem | Replacement | Notes |
|-----|-------------|-------|
| paperclip | Active Storage / Shrine | Last release 2019 |
| protected_attributes | Strong Parameters (built-in) | Rails 4 era |
| attr_accessible | Strong Parameters (built-in) | Rails 4 era |
| quiet_assets | Removed (built-in in Rails 6) | |
| turbolinks | turbo-rails | Replace for Rails 7+ |
| webpacker | importmap-rails / jsbundling-rails | Archived 2023 |
| fast_jsonapi | jsonapi-serializer | Fork maintained |
| ar-octopus | Use Rails multi-db | Unmaintained |
| rails_admin | activeadmin or administrate | Very outdated for 7+ |
