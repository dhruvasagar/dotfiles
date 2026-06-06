# Risky Migration Patterns Reference

A quick-reference card for migration safety review.

## PostgreSQL Lock Table

| Operation | Lock Type | Blocks | Safe Alternative |
|-----------|-----------|--------|-----------------|
| `add_column` with default | `AccessExclusiveLock` | All R/W | `add_column` (no default) + batch update |
| `add_column` Rails 6.1+ with default | No rewrite | Safe | Direct (Rails 6.1+ uses volatile default) |
| `remove_column` | `AccessExclusiveLock` | All R/W | Ignore column in code first, then remove |
| `rename_column` | `AccessExclusiveLock` | All R/W | 4-step: add, backfill, code swap, remove |
| `change_column` (type) | `AccessExclusiveLock` | All R/W | 4-step same as rename |
| `add_index` (default) | `ShareLock` | Writes | `algorithm: :concurrently` + `disable_ddl_transaction!` |
| `add_index :concurrently` | None | None | Recommended for all production indexes |
| `add_foreign_key` | `AccessShareLock` + validation scan | Reads | `validate: false`, then `validate_foreign_key` |
| `create_table` | None on new table | None | Safe |
| `drop_table` | `AccessExclusiveLock` | All R/W | Remove code references first |
| `change_column_null false` | Full table scan | Reads | Batch validate before constraint |

## Safe `add_column` with Default (Rails 6.1+)

Rails 6.1+ on PostgreSQL does NOT rewrite the table for columns with volatile defaults:

```ruby
# SAFE in Rails 6.1+ on PostgreSQL
def change
  add_column :users, :active, :boolean, default: true, null: false
end
```

For Rails < 6.1:

```ruby
# SAFE on all versions (3 steps)
def up
  add_column :users, :active, :boolean, null: true
  User.in_batches.update_all(active: true)
  change_column_null :users, :active, false
  change_column_default :users, :active, true
end
```

## Safe Index Creation (PostgreSQL)

```ruby
class AddIndexToUsersEmail < ActiveRecord::Migration[7.0]
  disable_ddl_transaction!

  def change
    add_index :users, :email, algorithm: :concurrently
  end
end
```

## Safe Foreign Key Addition (PostgreSQL)

```ruby
# Migration 1: Add without validation (instant)
def up
  add_foreign_key :orders, :users, validate: false
end

# Migration 2 (separate deploy): Validate (concurrent, no lock)
def up
  validate_foreign_key :orders, :users
end
```

## Safe Column Removal (Zero-Downtime)

**Step 1 (code deploy)**: Tell Rails to ignore the column:
```ruby
class User < ApplicationRecord
  self.ignored_columns += [:old_column_name]
end
```

**Step 2 (after deploy)**: Remove the column:
```ruby
def change
  remove_column :users, :old_column_name, :string
end
```

**Step 3 (cleanup)**: Remove `ignored_columns` line.

## Safe Column Rename (Zero-Downtime)

1. Add new column (no default)
2. Write to both columns in application code
3. Backfill new column from old column (batch job or migration)
4. Switch reads to new column
5. Stop writing to old column
6. Remove old column (using ignore pattern above)

## Dangerous Raw SQL Patterns

```ruby
# These bypass Rails safety checks — always review
execute "ALTER TABLE ..."
execute "UPDATE ... WHERE ..."  # large table without batching = lock

# Safe batch update pattern
User.in_batches(of: 1000) do |batch|
  batch.update_all(column: value)
end
```

## Irreversibility Checklist

A migration is irreversible if it:
- Drops a table or column without storing the schema
- Removes data
- Uses `execute` with no inverse
- Changes a column type in a lossy way

Always test: `bundle exec rails db:migrate:down VERSION=...` in development.
