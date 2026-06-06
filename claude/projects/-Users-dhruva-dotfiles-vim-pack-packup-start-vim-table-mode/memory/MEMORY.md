# vim-table-mode Lua Port

## Project: /Users/dhruva/dotfiles/vim/pack/packup/start/vim-table-mode

## Status: Lua port implemented (2026-03-04)

## Key files created/modified:
- `autoload/tablemode/table.vim` - Fixed broken Realign (now delegates to Lua)
- `lua/tablemode/utils.lua` - Rewritten (separator_count uses manual scan)
- `lua/tablemode/align.lua` - Rewritten (split_with_sep, Align algorithm fixed)
- `lua/tablemode/table.lua` - Rewritten (added Realign, IsBorderLine)
- `lua/tablemode/spreadsheet.lua` - Created (ported all VimScript spreadsheet fns)
- `lua/tablemode/spreadsheet/cell.lua` - Rewritten (GetCells, SetCell, Motion)
- `lua/tablemode/spreadsheet/cell_extra.lua` - Delegates to spreadsheet.lua
- `lua/tablemode/spreadsheet/cell_motion.lua` - Delegates to cell.lua Motion
- `lua/tablemode/spreadsheet/formula.lua` - Fixed (loadstring→load, formula parsing)
- `lua/tablemode/init.lua` - Created (setup, Enable/Disable/Toggle, TableizeRange)

## Critical implementation details:

### Align algorithm:
- split_with_sep(str, sep) splits including separators as elements
- Elements: [prefix, sep, cell, sep, cell, ..., sep, suffix]
- Strip: element[1] strip trailing only; all others strip both sides
- Pad each element to max width; add trailing space to all except element[1]
- Join all and strip trailing → produces `| cell | cell |` format

### separator_count:
- Manual scan respecting \ escapes (not VimScript pattern)
- `\sep` is skipped, unescaped sep is counted

### Realign:
- Walk outward from given line to find table extent (while IsTable)
- Collect data rows as {lnum, text} records
- Align.Align() pads all cells to same width per column
- Write back aligned rows, then regenerate border lines

### Module structure:
- All modules use `local M = {} ... return M` pattern
- Lazy requires with `require()` to avoid circular deps
- Config accessed via `vim.g.*` and `vim.b.*`

## Testing:
- Realign test passed: `|test11|test12|` → `| test11 | test12 |`
- Border regeneration works correctly
- All modules load without error

## VimScript compatibility:
- plugin/table-mode.vim unchanged (still sets defaults and Plug mappings)
- autoload/tablemode/*.vim still works; Realign now delegates to Lua
- Lua users can call `require('tablemode').setup(opts)`
