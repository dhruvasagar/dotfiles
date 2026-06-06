# FileTree Icons + OilBuffer Design

**Date:** 2026-05-06  
**Status:** Approved  

---

## Overview

Two parallel improvements:

1. **Icon/color beautification** — add nerd-font glyphs and per-type colors to both `FileTreeBuffer` and `OilBuffer` via a shared `icons` module. Fallback to color-only when nerd fonts are unavailable.
2. **OilBuffer** — a new flat-directory-listing buffer kind that is fully writable. `:w` diffs the buffer against its open-time snapshot and executes renames, deletes, and creates on disk. The existing `FileTreeBuffer` is unchanged in behavior; only its renderer gains icons.

---

## 1. Icon System (`icons.rs`)

### New file: `crates/lattice-ui-tui/src/icons.rs`

Single public function consumed by both renderers:

```rust
pub fn icon_for_entry(path: &Path, is_dir: bool, nerd_fonts: bool) -> (&'static str, Style)
```

Returns `(glyph, style)`. When `nerd_fonts` is false, glyph is `""` (empty — no glyph column rendered at all) and only the style carries color information.

### Glyph coverage (~50 types)

| Category | Glyph | Style |
|---|---|---|
| Directory (expanded) | `󰉋 ` | bold blue |
| Directory (collapsed) | `󰉋 ` | bold blue |
| `.rs` | `󱘗 ` | orange |
| `.toml` | ` ` | yellow |
| `.json` | ` ` | yellow |
| `.md` | `󰍔 ` | white |
| `.sh` / `.bash` / `.zsh` | ` ` | green |
| `.py` | ` ` | yellow |
| `.js` / `.ts` | ` ` | yellow |
| `.html` | ` ` | orange |
| `.css` | ` ` | blue |
| `.git*` | `󰒓 ` | dim |
| Hidden files (`.` prefix) | (extension glyph or ``) | dim dark-gray |
| Unknown | ` ` | terminal default |

### Theme additions (`theme.rs`)

```rust
pub file_tree_dir_style: Style,     // default: bold blue
pub file_tree_hidden_style: Style,  // default: dim dark-gray  
pub file_tree_file_style: Style,    // default: Color::Reset (terminal default)
```

### Config option

New boolean option `ui.nerd_fonts` (default: `true`) stored on `App`. Settable via `:set ui.nerd_fonts false` for terminals without a nerd-font patched font.

---

## 2. FileTreeBuffer Rendering Changes

### `file_tree.rs` — `render_to_buffer`

Each rendered line becomes `<indent><marker><icon><name>` where `<icon>` is the glyph from `icon_for_entry`. The rope content (used for motions) includes the glyph character inline — no structural change to how motions work.

`FileTreeBuffer` remains **read-only**. No changes to expand/collapse logic, entry tracking, or the `<CR>` handler.

### `render.rs` — `draw_file_tree_pane`

Switches from `Span::styled(whole_line, cursor_style)` to a multi-span `Line` per row:
- **Indent + marker span** — unstyled (or dir style for `▾`/`▸`)
- **Icon span** — colored per `icon_for_entry`
- **Name span** — dir style (bold blue) for directories, hidden style for dotfiles, file style otherwise

The cursor REVERSED modifier is applied at the `Line` level, composing with existing span styles.

---

## 3. OilBuffer

### New file: `crates/lattice-ui-tui/src/oil.rs`

```rust
pub struct OilEntry {
    pub name: String,
    pub is_dir: bool,
}

pub struct OilBuffer {
    pub id: BufferId,
    pub dir: PathBuf,
    snapshot: Vec<OilEntry>,  // state at open / last successful :w
    pub content: Buffer,      // editable rope — one name per line, sorted dirs-first
    pub cursor: Position,
    pub scroll: usize,
}
```

### `OilBuffer::open(dir: PathBuf) -> io::Result<Self>`

Reads directory entries (same sort order as `FileTreeBuffer`: dirs first, then alpha). Builds `snapshot` and renders the rope as one bare `<name>` per line — **no icon in the rope**. Icons are added by `draw_oil_pane` as leading renderer spans, never touching the rope content. This keeps the rope as pure editable filename text. The rope is immediately writable.

### `OilBuffer::navigate_into(&mut self, subdir: PathBuf) -> io::Result<()>`

Replaces `dir`, `snapshot`, and rope in-place — same buffer ID, same pane. `<CR>` on a directory entry calls this. `<CR>` on a file calls `do_edit(path)`.

### `OilBuffer::navigate_up(&mut self) -> io::Result<()>`

Sets `dir` to `dir.parent()` and reloads. The `-` key calls this.

### `OilBuffer::apply(&mut self) -> io::Result<()>`

Called by `:w`. Diff algorithm:

1. Parse current rope lines → `Vec<String>` of names (skip blank lines; rope contains bare names so no prefix stripping needed).
2. Compare against `snapshot` names:
   - Name in snapshot, not in current → **delete** (any number supported — remove as many lines as you like)
   - Name in current, not in snapshot → **create** (any number supported — add as many lines as you like)
   - Special case: if the totals are **exactly one delete + one create**, use `fs::rename` instead of separate delete+create, preserving file attributes and git history. With 2+ of either side, all ops execute as independent deletes and creates.
3. Execute in order: **renames → deletes → creates** (renames first avoids transient name collisions).
4. On any error: stop, echo the error, reload snapshot from disk state.
5. On success: refresh `snapshot` to the new disk state.

### `OilBuffer::is_dirty(&self) -> bool`

Returns true if the current rope content differs from a fresh render of the snapshot. Used for the `[+]` status line marker.

---

## 4. BufferKind & App Wiring

### `buffers.rs`

```rust
pub enum BufferKind {
    Document,
    Help,
    FileTree,
    Oil,       // NEW — writable
}
```

`is_read_only()` returns `false` for `Oil`. `label()` returns `"oil"`.

### `app.rs` changes

**`:edit <dir>` behavior change:** `do_edit` currently opens `FileTreeBuffer` when given a directory path. This call site switches to `do_open_oil(dir)` instead.

**New `do_open_oil(dir: PathBuf)`:** mirrors `do_open_file_tree` — de-dups by dir path, inserts `OilBuffer` into `BufferRegistry`, activates it in the current pane.

**`:w` dispatch:** when `active_buffer == BufferKind::Oil`, `:w` calls `oil.apply()` instead of the document write path.

**`run_oil_invocation`:** motions resolve against the oil rope (same `run_read_only_motion` path). Operators (insert, delete, change) are **permitted** — `is_read_only()` returns false, so the standard document operator path runs against the oil rope. The oil rope is just a normal editable buffer.

**`draw_oil_pane`:** same multi-span colored rendering as the updated tree pane. Status line shows `[oil] <dir>` with `[+]` when dirty.

### `effect.rs` / `ex_commands.rs`

```rust
Effect::OpenOil { dir: Option<PathBuf> }  // NEW
```

| Ex-command | Maps to |
|---|---|
| `:oil [dir]` | `Effect::OpenOil { dir }` |
| `:filetree [dir]` | `Effect::OpenFileTree { root }` (existing, renamed from current invocation) |

---

## 5. Keybindings

| Context | Key | Action |
|---|---|---|
| Normal mode (Document) | `-` | `:oil <parent-dir-of-current-file>` |
| File tree | `-` | `:oil <hovered-dir-or-parent-of-hovered-file>` |
| Oil buffer | `-` | `oil.navigate_up()` |
| Oil buffer | `<CR>` on dir | `oil.navigate_into(dir)` |
| Oil buffer | `<CR>` on file | `do_edit(path)` |

One consistent key, three context-sensitive behaviors.

---

## 6. Files Changed Summary

| File | Change |
|---|---|
| `lattice-ui-tui/src/icons.rs` | **NEW** — icon/color resolver |
| `lattice-ui-tui/src/oil.rs` | **NEW** — OilBuffer |
| `lattice-ui-tui/src/theme.rs` | Add 3 style fields |
| `lattice-ui-tui/src/buffers.rs` | Add `BufferKind::Oil` |
| `lattice-ui-tui/src/file_tree.rs` | `render_to_buffer` calls icons module |
| `lattice-ui-tui/src/render.rs` | Multi-span tree renderer + new `draw_oil_pane` |
| `lattice-ui-tui/src/app.rs` | `:edit <dir>` → oil, `do_open_oil`, `-` keybind, `:w` dispatch |
| `lattice-grammar/src/effect.rs` | Add `Effect::OpenOil` |
| `lattice-grammar/src/ex_commands.rs` | Wire `:oil` command |

---

## 7. Non-goals (explicitly out of scope)

- Cross-directory moves (cut line from one oil buffer, paste into another) — v2
- Hidden-marker ID system for unambiguous multi-rename tracking — v2
- Sprite atlas / GPU icon rendering — post-1.0 per §5.6.7
- Auto-detect nerd font availability — not reliably possible; `ui.nerd_fonts` config flag instead
