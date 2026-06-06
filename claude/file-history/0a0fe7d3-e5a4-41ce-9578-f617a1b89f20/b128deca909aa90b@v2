# FileTree Icons + OilBuffer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add nerd-font icons + per-type colors to the file tree, and introduce `OilBuffer` — a flat, writable directory listing where `:w` renames/deletes/creates files on disk.

**Architecture:** Shared `icons.rs` module drives rendering for both `FileTreeBuffer` (read-only, icons embedded in rope) and `OilBuffer` (writable, icons added as renderer spans only — rope stays pure filenames). `OilBuffer` is a new `BufferKind::Oil` variant wired into the existing registry/pane/input pipeline. `:edit <dir>` opens oil; `:filetree` opens the expandable tree. The `-` key is context-sensitive: opens oil for the parent dir from a document or tree, navigates up from inside oil.

**Tech Stack:** Rust, ratatui (`Style`, `Span`, `Line`), `ropey` (via `lattice_core::Buffer`), `std::fs` (rename/remove/create), `lattice_grammar` effects + ex-commands.

---

## File Map

| File | Action | Responsibility |
|---|---|---|
| `crates/lattice-ui-tui/src/icons.rs` | **Create** | `icon_for_entry()` — glyph + style per path/kind |
| `crates/lattice-ui-tui/src/oil.rs` | **Create** | `OilBuffer`, `OilEntry`, open/navigate/apply |
| `crates/lattice-ui-tui/src/theme.rs` | Modify | Add 3 style fields for dir/hidden/file |
| `crates/lattice-ui-tui/src/buffers.rs` | Modify | Add `BufferKind::Oil` |
| `crates/lattice-ui-tui/src/buffer_registry.rs` | Modify | Add `BufferData::Oil`, `oil()`/`oil_mut()` accessors |
| `crates/lattice-ui-tui/src/file_tree.rs` | Modify | `render_to_buffer` calls `icons::icon_for_entry` |
| `crates/lattice-ui-tui/src/render.rs` | Modify | Colored spans in `draw_file_tree_pane`; new `draw_oil_pane` |
| `crates/lattice-ui-tui/src/app.rs` | Modify | `do_open_oil`, `do_oil_follow`, `run_oil_invocation`, `-` keybind, `:w` dispatch, `do_edit` dir branch |
| `crates/lattice-ui-tui/src/input.rs` | Modify | Oil buffer-local keys; `-` in Normal for Doc/Tree |
| `crates/lattice-ui-tui/src/lib.rs` | Modify | Add `pub mod icons;` and `pub mod oil;` |
| `crates/lattice-grammar/src/effect.rs` | Modify | Add `Effect::OpenOil { dir }` |
| `crates/lattice-grammar/src/ex_commands.rs` | Modify | Add `:oil`; rename `ex:tree` → `ex:filetree` |

---

## Task 1: Icon module + Theme style fields

**Files:**
- Create: `crates/lattice-ui-tui/src/icons.rs`
- Modify: `crates/lattice-ui-tui/src/theme.rs`
- Modify: `crates/lattice-ui-tui/src/lib.rs`

- [ ] **Step 1.1: Write failing tests for `icon_for_entry`**

Add to the bottom of a new `crates/lattice-ui-tui/src/icons.rs`:

```rust
//! Icon and color resolver shared by FileTree and OilBuffer renderers.

use std::path::Path;
use ratatui::style::{Color, Modifier, Style};

use crate::theme::Theme;

/// Returns `(glyph, style)` for a directory or file entry.
/// When `nerd_fonts` is false the glyph is `""` (no column rendered)
/// and only the style carries visual differentiation.
pub fn icon_for_entry(path: &Path, is_dir: bool, nerd_fonts: bool, theme: &Theme) -> (&'static str, Style) {
    if is_dir {
        let glyph = if nerd_fonts { "󰉋 " } else { "" };
        return (glyph, theme.file_tree_dir_style);
    }
    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
    let ext  = path.extension().and_then(|e| e.to_str()).unwrap_or("");
    let is_hidden = name.starts_with('.');
    let (glyph, color) = if nerd_fonts {
        nerd_glyph_and_color(name, ext)
    } else {
        ("", ext_color(ext))
    };
    let base_style = Style::new().fg(color);
    let style = if is_hidden {
        theme.file_tree_hidden_style
    } else {
        base_style
    };
    (glyph, style)
}

fn nerd_glyph_and_color(name: &str, ext: &str) -> (&'static str, Color) {
    match ext {
        "rs"                            => ("󱘗 ", Color::from_u32(0xFF8C00)), // orange
        "toml"                          => (" ", Color::Yellow),
        "json" | "jsonc"                => (" ", Color::Yellow),
        "md" | "mdx"                    => ("󰍔 ", Color::White),
        "sh" | "bash" | "zsh" | "fish" => (" ", Color::Green),
        "py"                            => (" ", Color::Yellow),
        "js" | "mjs" | "cjs"           => (" ", Color::Yellow),
        "ts" | "tsx"                    => (" ", Color::Blue),
        "jsx"                           => (" ", Color::Cyan),
        "html" | "htm"                  => (" ", Color::from_u32(0xFF6600)),
        "css" | "scss" | "sass"         => (" ", Color::Magenta),
        "go"                            => (" ", Color::Cyan),
        "c" | "h"                       => (" ", Color::Blue),
        "cpp" | "cxx" | "cc" | "hpp"   => (" ", Color::Blue),
        "java"                          => (" ", Color::from_u32(0xFF8C00)),
        "kt" | "kts"                    => (" ", Color::Magenta),
        "swift"                         => (" ", Color::from_u32(0xFF5533)),
        "zig"                           => (" ", Color::Yellow),
        "lua"                           => (" ", Color::Blue),
        "vim"                           => (" ", Color::Green),
        "yaml" | "yml"                  => (" ", Color::Green),
        "xml"                           => ("󰗀 ", Color::from_u32(0xFF8C00)),
        "sql"                           => (" ", Color::Blue),
        "lock"                          => ("󰌾 ", Color::DarkGray),
        "gitignore" | "gitmodules"
        | "gitattributes"               => ("󰒓 ", Color::DarkGray),
        "dockerfile" | "containerfile"  => (" ", Color::Blue),
        _                               => (" ", Color::Reset),
    }
}

fn ext_color(ext: &str) -> Color {
    match ext {
        "rs"                            => Color::from_u32(0xFF8C00),
        "toml" | "json" | "jsonc"       => Color::Yellow,
        "sh" | "bash" | "zsh" | "fish"
        | "go" | "kt" | "kts"          => Color::Green,
        "py" | "js" | "mjs" | "cjs"    => Color::Yellow,
        "ts" | "tsx" | "c" | "h"
        | "cpp" | "cxx" | "hpp"        => Color::Blue,
        "md" | "mdx"                    => Color::White,
        _                              => Color::Reset,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::theme::Theme;
    use std::path::PathBuf;

    fn theme() -> Theme { Theme::default() }

    #[test]
    fn directory_returns_dir_style() {
        let (glyph, style) = icon_for_entry(&PathBuf::from("src"), true, true, &theme());
        assert_eq!(glyph, "󰉋 ");
        assert_eq!(style, theme().file_tree_dir_style);
    }

    #[test]
    fn nerd_fonts_false_returns_empty_glyph_for_dir() {
        let (glyph, _) = icon_for_entry(&PathBuf::from("src"), true, false, &theme());
        assert_eq!(glyph, "");
    }

    #[test]
    fn rust_file_gets_orange_glyph() {
        let (glyph, style) = icon_for_entry(&PathBuf::from("main.rs"), false, true, &theme());
        assert_eq!(glyph, "󱘗 ");
        assert_eq!(style.fg, Some(Color::from_u32(0xFF8C00)));
    }

    #[test]
    fn hidden_file_uses_hidden_style() {
        let (_, style) = icon_for_entry(&PathBuf::from(".gitignore"), false, true, &theme());
        assert_eq!(style, theme().file_tree_hidden_style);
    }

    #[test]
    fn unknown_ext_falls_back_to_default_file_glyph() {
        let (glyph, _) = icon_for_entry(&PathBuf::from("Makefile"), false, true, &theme());
        assert_eq!(glyph, " ");
    }
}
```

- [ ] **Step 1.2: Run tests to verify they fail**

```bash
cargo test -p lattice-ui-tui --lib icons 2>&1 | tail -20
```

Expected: compile error — `icons` module not declared in `lib.rs`.

- [ ] **Step 1.3: Add module declaration to `lib.rs`**

In `crates/lattice-ui-tui/src/lib.rs`, after `pub mod folds;`:

```rust
pub mod icons;
pub mod oil;     // add this now (oil.rs will be created in Task 3)
```

Create an empty `crates/lattice-ui-tui/src/oil.rs` placeholder so it compiles:

```rust
// placeholder — implemented in Task 3
```

- [ ] **Step 1.4: Add style fields to `Theme`**

In `crates/lattice-ui-tui/src/theme.rs`, add to the `Theme` struct after `pane_separator_horizontal`:

```rust
/// Style for directory entries in file-tree and oil buffers.
pub file_tree_dir_style: Style,
/// Style for hidden files (names starting with `.`).
pub file_tree_hidden_style: Style,
/// Base style for regular file entries (overridden per-extension by icons module).
pub file_tree_file_style: Style,
/// Whether to use nerd-font glyphs. When false, icons module returns empty glyphs.
pub nerd_fonts: bool,
```

In `Theme::default()`, add:

```rust
file_tree_dir_style: Style::new().fg(Color::Blue).add_modifier(Modifier::BOLD),
file_tree_hidden_style: Style::new().fg(Color::DarkGray).add_modifier(Modifier::DIM),
file_tree_file_style: Style::new(),
nerd_fonts: true,
```

- [ ] **Step 1.5: Run tests to verify they pass**

```bash
cargo test -p lattice-ui-tui --lib icons 2>&1 | tail -10
```

Expected: `test result: ok. 5 passed`

- [ ] **Step 1.6: Run full test suite to check no regressions**

```bash
cargo test -p lattice-ui-tui --lib 2>&1 | tail -5
```

Expected: all tests pass.

- [ ] **Step 1.7: Commit**

```bash
git add crates/lattice-ui-tui/src/icons.rs crates/lattice-ui-tui/src/oil.rs \
        crates/lattice-ui-tui/src/theme.rs crates/lattice-ui-tui/src/lib.rs
git commit -m "feat: add icons module and theme style fields for file-tree/oil"
```

---

## Task 2: FileTree renderer — icons + colored spans

**Files:**
- Modify: `crates/lattice-ui-tui/src/file_tree.rs`
- Modify: `crates/lattice-ui-tui/src/render.rs`

- [ ] **Step 2.1: Write failing test for icon-in-rope rendering**

In `crates/lattice-ui-tui/src/file_tree.rs`, update the existing `render_uses_indentation_and_marker` test and add a new one:

```rust
#[test]
fn render_embeds_nerd_icon_in_rope() {
    let dir = temp_dir();
    std::fs::write(dir.join("main.rs"), "x").unwrap();
    let tree = FileTreeBuffer::open(&dir, true).unwrap(); // nerd_fonts = true
    let body = tree.content.as_string();
    // Rust file must have the nerd-font glyph in the rope
    assert!(body.contains("󱘗 "), "expected rust glyph in rope, got: {body}");
    std::fs::remove_dir_all(dir).ok();
}

#[test]
fn render_no_icon_when_nerd_fonts_disabled() {
    let dir = temp_dir();
    std::fs::write(dir.join("main.rs"), "x").unwrap();
    let tree = FileTreeBuffer::open(&dir, false).unwrap(); // nerd_fonts = false
    let body = tree.content.as_string();
    assert!(!body.contains("󱘗 "), "unexpected glyph when nerd_fonts=false");
    std::fs::remove_dir_all(dir).ok();
}
```

- [ ] **Step 2.2: Run tests to see them fail**

```bash
cargo test -p lattice-ui-tui --lib file_tree 2>&1 | tail -15
```

Expected: compile error — `open` takes 1 arg, not 2.

- [ ] **Step 2.3: Update `FileTreeBuffer` to accept `nerd_fonts` and embed icons**

In `crates/lattice-ui-tui/src/file_tree.rs`:

Add `use crate::theme::Theme;` and update the struct:

```rust
#[derive(Debug)]
pub struct FileTreeBuffer {
    pub id: BufferId,
    pub root: PathBuf,
    pub entries: Vec<FileTreeEntry>,
    pub content: Buffer,
    pub cursor: Position,
    pub scroll: usize,
    pub nerd_fonts: bool,   // stored so re-renders stay consistent
}
```

Update `open` signature:

```rust
pub fn open(root: impl Into<PathBuf>, nerd_fonts: bool) -> std::io::Result<Self> {
    let root = root.into();
    let mut entries = Vec::new();
    entries.push(FileTreeEntry {
        path: root.clone(),
        depth: 0,
        kind: FileTreeEntryKind::Directory { expanded: true },
    });
    let children = read_dir_sorted(&root)?;
    for (path, is_dir) in children {
        entries.push(FileTreeEntry {
            path,
            depth: 1,
            kind: if is_dir {
                FileTreeEntryKind::Directory { expanded: false }
            } else {
                FileTreeEntryKind::File
            },
        });
    }
    let content = render_to_buffer(&entries, nerd_fonts);
    Ok(Self {
        id: BufferId::next(),
        root,
        entries,
        content,
        cursor: Position::ZERO,
        scroll: 0,
        nerd_fonts,
    })
}
```

Update `toggle_at` to pass `nerd_fonts` to re-render:

```rust
pub fn toggle_at(&mut self, index: usize) -> std::io::Result<()> {
    let Some(entry) = self.entries.get(index) else {
        return Ok(());
    };
    if let FileTreeEntryKind::Directory { expanded } = entry.kind {
        if expanded {
            self.collapse(index);
        } else {
            self.expand(index)?;
        }
        self.content = render_to_buffer(&self.entries, self.nerd_fonts);
    }
    Ok(())
}
```

Update `render_to_buffer` to accept `nerd_fonts` and call `icons::icon_for_entry`:

```rust
fn render_to_buffer(entries: &[FileTreeEntry], nerd_fonts: bool) -> Buffer {
    use crate::icons::icon_for_entry;
    use crate::theme::Theme;
    let theme = Theme::default(); // renderer reads live theme; this is used only for style defaults
    let mut text = String::new();
    for (i, entry) in entries.iter().enumerate() {
        let indent = "  ".repeat(entry.depth as usize);
        let marker = match entry.kind {
            FileTreeEntryKind::Directory { expanded: true }  => "▾ ",
            FileTreeEntryKind::Directory { expanded: false } => "▸ ",
            FileTreeEntryKind::File                          => "  ",
        };
        let is_dir = matches!(entry.kind, FileTreeEntryKind::Directory { .. });
        let (icon, _style) = icon_for_entry(&entry.path, is_dir, nerd_fonts, &theme);
        let name = if entry.depth == 0 {
            entry.path.display().to_string()
        } else {
            entry.path.file_name()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default()
        };
        text.push_str(&indent);
        text.push_str(marker);
        text.push_str(icon);
        text.push_str(&name);
        if i + 1 < entries.len() {
            text.push('\n');
        }
    }
    let mut buffer = Buffer::empty();
    if !text.is_empty() {
        let _ = buffer.apply_edit(&Edit::insert(Position::ZERO, text));
    }
    buffer
}
```

Fix existing tests to pass `nerd_fonts` arg (use `false` to keep the ASCII assertions valid):

```rust
// In each existing test that calls FileTreeBuffer::open, change:
let tree = FileTreeBuffer::open(&dir).unwrap();
// to:
let tree = FileTreeBuffer::open(&dir, false).unwrap();
```

- [ ] **Step 2.4: Fix all call sites of `FileTreeBuffer::open` in `app.rs`**

Search for `FileTreeBuffer::open` in `app.rs`:

```bash
grep -n "FileTreeBuffer::open" crates/lattice-ui-tui/src/app.rs
```

At each call site, add the `nerd_fonts` argument sourced from the theme:

```rust
// Before:
let tree = match FileTreeBuffer::open(&root) {
// After:
let tree = match FileTreeBuffer::open(&root, self.theme.nerd_fonts) {
```

- [ ] **Step 2.5: Run file_tree tests to verify they pass**

```bash
cargo test -p lattice-ui-tui --lib file_tree 2>&1 | tail -10
```

Expected: all pass including the two new icon tests.

- [ ] **Step 2.6: Update `draw_file_tree_pane` in `render.rs` to emit colored spans**

Find `fn draw_file_tree_pane` (around line 1344). Replace the `lines: Vec<Line>` construction:

```rust
// BEFORE (plain single-span per line):
let lines: Vec<Line> = tree
    .content
    .as_string()
    .split('\n')
    .skip(scroll)
    .take(viewport)
    .enumerate()
    .map(|(i, l)| {
        let line_idx = scroll + i;
        let style = if is_active && line_idx == cursor_line {
            TuiStyle::default().add_modifier(Modifier::REVERSED)
        } else {
            TuiStyle::default()
        };
        Line::from(Span::styled(l.to_string(), style))
    })
    .collect();

// AFTER (multi-span colored lines):
use crate::icons::icon_for_entry;
let nerd_fonts = app.theme.nerd_fonts;
let theme = &app.theme;
let raw_text = tree.content.as_string();
let lines: Vec<Line> = raw_text
    .split('\n')
    .enumerate()
    .zip(tree.entries.iter().chain(std::iter::repeat(&crate::file_tree::FileTreeEntry {
        path: std::path::PathBuf::new(),
        depth: 0,
        kind: crate::file_tree::FileTreeEntryKind::File,
    })))
    .skip(scroll)
    .take(viewport)
    .map(|((i, raw_line), entry)| {
        let line_idx = scroll + i;
        let is_cursor = is_active && line_idx == cursor_line;
        let is_dir = matches!(entry.kind, crate::file_tree::FileTreeEntryKind::Directory { .. });
        let (_glyph, entry_style) = icon_for_entry(&entry.path, is_dir, nerd_fonts, theme);
        let cursor_mod = if is_cursor { Modifier::REVERSED } else { Modifier::empty() };
        let span_style = entry_style.add_modifier(cursor_mod);
        Line::from(Span::styled(raw_line.to_string(), span_style))
    })
    .collect();
```

> Note: The rope already contains the glyph text (embedded in Task 2.3). The renderer applies color per-entry by walking `tree.entries` in parallel with rope lines. The entry count equals line count by construction.

- [ ] **Step 2.7: Run full test suite**

```bash
cargo test -p lattice-ui-tui --lib 2>&1 | tail -5
```

Expected: all pass.

- [ ] **Step 2.8: Commit**

```bash
git add crates/lattice-ui-tui/src/file_tree.rs crates/lattice-ui-tui/src/render.rs \
        crates/lattice-ui-tui/src/app.rs
git commit -m "feat: add nerd-font icons and colored spans to file-tree renderer"
```

---

## Task 3: OilBuffer data model

**Files:**
- Modify: `crates/lattice-ui-tui/src/oil.rs` (replace placeholder)

- [ ] **Step 3.1: Write failing tests**

Replace `crates/lattice-ui-tui/src/oil.rs` with:

```rust
//! Flat directory listing buffer — oil.nvim-style editable view.
//!
//! The rope contains one bare filename per line (no icons — icons are
//! added as renderer spans in `draw_oil_pane` so the rope stays pure
//! editable text). `apply()` diffs the rope against the open-time
//! snapshot and executes renames / deletes / creates on disk.

use std::path::{Path, PathBuf};

use lattice_core::Buffer;
use lattice_protocol::edit::Edit;
use lattice_protocol::position::Position;

use crate::buffers::BufferId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OilEntry {
    pub name: String,
    pub is_dir: bool,
}

#[derive(Debug)]
pub struct OilBuffer {
    pub id: BufferId,
    pub dir: PathBuf,
    /// State at open / last successful `:w`.
    snapshot: Vec<OilEntry>,
    /// Editable rope — one bare filename per line, dirs-first alpha order.
    pub content: Buffer,
    pub cursor: Position,
    pub scroll: usize,
}

impl OilBuffer {
    pub fn open(dir: impl Into<PathBuf>) -> std::io::Result<Self> {
        let dir = dir.into();
        let snapshot = read_dir_entries(&dir)?;
        let content = render_to_buffer(&snapshot);
        Ok(Self {
            id: BufferId::next(),
            dir,
            snapshot,
            content,
            cursor: Position::ZERO,
            scroll: 0,
        })
    }

    /// Replace the listing with `subdir`'s contents in-place.
    /// Called by `<CR>` on a directory entry.
    pub fn navigate_into(&mut self, subdir: impl Into<PathBuf>) -> std::io::Result<()> {
        self.dir = subdir.into();
        self.snapshot = read_dir_entries(&self.dir)?;
        self.content = render_to_buffer(&self.snapshot);
        self.cursor = Position::ZERO;
        self.scroll = 0;
        Ok(())
    }

    /// Replace the listing with the parent directory's contents.
    /// Called by `-`.
    pub fn navigate_up(&mut self) -> std::io::Result<()> {
        if let Some(parent) = self.dir.parent().map(Path::to_path_buf) {
            self.navigate_into(parent)?;
        }
        Ok(())
    }

    /// True when the current rope content differs from the snapshot render.
    pub fn is_dirty(&self) -> bool {
        self.content.as_string() != render_to_buffer(&self.snapshot).as_string()
    }

    /// Names from the snapshot (used by `apply` and tests).
    pub fn snapshot_names(&self) -> Vec<&str> {
        self.snapshot.iter().map(|e| e.name.as_str()).collect()
    }

    /// Entry at the cursor line (for `<CR>` dispatch).
    pub fn entry_at_cursor(&self) -> Option<&OilEntry> {
        self.snapshot.get(self.cursor.line as usize)
    }

    pub fn line_count(&self) -> u32 {
        self.content.line_count()
    }

    pub fn move_cursor(&mut self, dx: i32, dy: i32, viewport: usize) {
        let last_line = self.line_count().saturating_sub(1) as i32;
        let new_line = (self.cursor.line as i32 + dy).clamp(0, last_line) as u32;
        let line_len = line_byte_len(&self.content, new_line);
        let new_byte = (self.cursor.byte as i32 + dx).clamp(0, line_len as i32) as u32;
        self.cursor = Position::new(new_line, new_byte);
        self.adjust_scroll_to_cursor(viewport);
    }

    pub fn jump_cursor_to(&mut self, line: u32, viewport: usize) {
        let last_line = self.line_count().saturating_sub(1);
        let target = line.min(last_line);
        let line_len = line_byte_len(&self.content, target);
        self.cursor = Position::new(target, self.cursor.byte.min(line_len));
        self.adjust_scroll_to_cursor(viewport);
    }

    pub fn adjust_scroll_to_cursor(&mut self, viewport: usize) {
        if viewport == 0 { return; }
        let line = self.cursor.line as usize;
        if line < self.scroll {
            self.scroll = line;
        } else if line >= self.scroll + viewport {
            self.scroll = line + 1 - viewport;
        }
    }
}

/// Read a directory's entries sorted dirs-first then alpha.
fn read_dir_entries(dir: &Path) -> std::io::Result<Vec<OilEntry>> {
    let mut entries: Vec<OilEntry> = Vec::new();
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let is_dir = entry.file_type()?.is_dir();
        let name = entry.file_name().to_string_lossy().into_owned();
        entries.push(OilEntry { name, is_dir });
    }
    entries.sort_by(|a, b| match (a.is_dir, b.is_dir) {
        (true, false) => std::cmp::Ordering::Less,
        (false, true) => std::cmp::Ordering::Greater,
        _ => a.name.cmp(&b.name),
    });
    Ok(entries)
}

/// Render snapshot entries to a rope — one bare name per line, no icons.
fn render_to_buffer(entries: &[OilEntry]) -> Buffer {
    let text = entries
        .iter()
        .enumerate()
        .map(|(i, e)| {
            if i + 1 < entries.len() {
                format!("{}\n", e.name)
            } else {
                e.name.clone()
            }
        })
        .collect::<String>();
    let mut buf = Buffer::empty();
    if !text.is_empty() {
        let _ = buf.apply_edit(&Edit::insert(Position::ZERO, text));
    }
    buf
}

fn line_byte_len(buf: &Buffer, line: u32) -> u32 {
    buf.as_string()
        .split('\n')
        .nth(line as usize)
        .map(|l| l.len() as u32)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)]
    use super::*;

    fn temp_dir() -> PathBuf {
        use std::sync::atomic::{AtomicU64, Ordering};
        static N: AtomicU64 = AtomicU64::new(0);
        let dir = std::env::temp_dir()
            .join(format!("lattice-oil-{}-{}", std::process::id(), N.fetch_add(1, Ordering::Relaxed)));
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn open_lists_dirs_first_then_files_alpha() {
        let dir = temp_dir();
        std::fs::write(dir.join("z.txt"), "").unwrap();
        std::fs::write(dir.join("a.txt"), "").unwrap();
        std::fs::create_dir(dir.join("sub")).unwrap();
        let oil = OilBuffer::open(&dir).unwrap();
        assert_eq!(oil.snapshot_names(), vec!["sub", "a.txt", "z.txt"]);
        std::fs::remove_dir_all(dir).ok();
    }

    #[test]
    fn rope_contains_bare_names_only() {
        let dir = temp_dir();
        std::fs::write(dir.join("main.rs"), "").unwrap();
        let oil = OilBuffer::open(&dir).unwrap();
        let text = oil.content.as_string();
        assert_eq!(text.trim(), "main.rs");
        // No icon glyphs in rope
        assert!(!text.contains("󱘗"), "icon should not be in rope");
        std::fs::remove_dir_all(dir).ok();
    }

    #[test]
    fn navigate_into_replaces_listing() {
        let dir = temp_dir();
        let sub = dir.join("sub");
        std::fs::create_dir(&sub).unwrap();
        std::fs::write(sub.join("inner.rs"), "").unwrap();
        let mut oil = OilBuffer::open(&dir).unwrap();
        oil.navigate_into(&sub).unwrap();
        assert_eq!(oil.snapshot_names(), vec!["inner.rs"]);
        assert_eq!(oil.cursor, Position::ZERO);
        std::fs::remove_dir_all(dir).ok();
    }

    #[test]
    fn navigate_up_goes_to_parent() {
        let dir = temp_dir();
        let sub = dir.join("sub");
        std::fs::create_dir(&sub).unwrap();
        let mut oil = OilBuffer::open(&sub).unwrap();
        oil.navigate_up().unwrap();
        assert_eq!(oil.dir, dir);
        std::fs::remove_dir_all(dir).ok();
    }

    #[test]
    fn is_dirty_false_when_unchanged() {
        let dir = temp_dir();
        std::fs::write(dir.join("a.txt"), "").unwrap();
        let oil = OilBuffer::open(&dir).unwrap();
        assert!(!oil.is_dirty());
        std::fs::remove_dir_all(dir).ok();
    }
}
```

- [ ] **Step 3.2: Run tests to verify they fail**

```bash
cargo test -p lattice-ui-tui --lib oil 2>&1 | tail -15
```

Expected: compile error — `BufferId`, `Buffer`, etc. not yet referenced in oil.rs imports (verify the module compiles once `mod oil` is declared).

- [ ] **Step 3.3: Run tests until they pass**

```bash
cargo test -p lattice-ui-tui --lib oil 2>&1 | tail -10
```

Expected: `test result: ok. 5 passed`

- [ ] **Step 3.4: Run full suite for regressions**

```bash
cargo test -p lattice-ui-tui --lib 2>&1 | tail -5
```

- [ ] **Step 3.5: Commit**

```bash
git add crates/lattice-ui-tui/src/oil.rs
git commit -m "feat: add OilBuffer data model with open/navigate/is_dirty"
```

---

## Task 4: OilBuffer::apply — diff and execute filesystem operations

**Files:**
- Modify: `crates/lattice-ui-tui/src/oil.rs`

- [ ] **Step 4.1: Write failing tests for `apply`**

Add to the `tests` module in `oil.rs`:

```rust
    #[test]
    fn apply_renames_single_file() {
        let dir = temp_dir();
        std::fs::write(dir.join("old.txt"), "content").unwrap();
        let mut oil = OilBuffer::open(&dir).unwrap();
        // Edit rope: replace "old.txt" with "new.txt"
        let edit = lattice_protocol::edit::Edit::delete(
            lattice_protocol::position::Position::new(0, 0),
            lattice_protocol::position::Position::new(0, 7),
        );
        oil.content.apply_edit(&edit).unwrap();
        let ins = lattice_protocol::edit::Edit::insert(
            lattice_protocol::position::Position::new(0, 0),
            "new.txt".to_string(),
        );
        oil.content.apply_edit(&ins).unwrap();
        oil.apply().unwrap();
        assert!(dir.join("new.txt").exists(), "new.txt should exist after rename");
        assert!(!dir.join("old.txt").exists(), "old.txt should not exist after rename");
        std::fs::remove_dir_all(dir).ok();
    }

    #[test]
    fn apply_deletes_removed_lines() {
        let dir = temp_dir();
        std::fs::write(dir.join("keep.txt"), "").unwrap();
        std::fs::write(dir.join("gone.txt"), "").unwrap();
        let mut oil = OilBuffer::open(&dir).unwrap();
        // Remove the "gone.txt" line from the rope
        let text = oil.content.as_string();
        let new_text = text.lines()
            .filter(|l| *l != "gone.txt")
            .collect::<Vec<_>>()
            .join("\n");
        oil.content = Buffer::empty();
        if !new_text.is_empty() {
            oil.content.apply_edit(&Edit::insert(Position::ZERO, new_text)).unwrap();
        }
        oil.apply().unwrap();
        assert!(!dir.join("gone.txt").exists());
        assert!(dir.join("keep.txt").exists());
        std::fs::remove_dir_all(dir).ok();
    }

    #[test]
    fn apply_creates_new_file_lines() {
        let dir = temp_dir();
        std::fs::write(dir.join("existing.txt"), "").unwrap();
        let mut oil = OilBuffer::open(&dir).unwrap();
        // Append a new filename to the rope
        let append = Edit::insert(
            Position::new(oil.line_count(), 0),
            "\nnewfile.txt".to_string(),
        );
        oil.content.apply_edit(&append).unwrap();
        oil.apply().unwrap();
        assert!(dir.join("newfile.txt").exists());
        std::fs::remove_dir_all(dir).ok();
    }

    #[test]
    fn apply_handles_multiple_deletes_and_creates() {
        let dir = temp_dir();
        std::fs::write(dir.join("a.txt"), "").unwrap();
        std::fs::write(dir.join("b.txt"), "").unwrap();
        let mut oil = OilBuffer::open(&dir).unwrap();
        // Remove both existing, add two new
        oil.content = Buffer::empty();
        oil.content.apply_edit(&Edit::insert(Position::ZERO, "c.txt\nd.txt".to_string())).unwrap();
        oil.apply().unwrap();
        assert!(!dir.join("a.txt").exists());
        assert!(!dir.join("b.txt").exists());
        assert!(dir.join("c.txt").exists());
        assert!(dir.join("d.txt").exists());
        std::fs::remove_dir_all(dir).ok();
    }

    #[test]
    fn apply_refreshes_snapshot_on_success() {
        let dir = temp_dir();
        std::fs::write(dir.join("a.txt"), "").unwrap();
        let mut oil = OilBuffer::open(&dir).unwrap();
        // Edit rope: rename a.txt to b.txt
        oil.content = Buffer::empty();
        oil.content.apply_edit(&Edit::insert(Position::ZERO, "b.txt".to_string())).unwrap();
        oil.apply().unwrap();
        assert_eq!(oil.snapshot_names(), vec!["b.txt"]);
        assert!(!oil.is_dirty());
        std::fs::remove_dir_all(dir).ok();
    }
```

- [ ] **Step 4.2: Run tests to verify they fail**

```bash
cargo test -p lattice-ui-tui --lib oil::tests 2>&1 | tail -10
```

Expected: compile error — `apply` method not defined.

- [ ] **Step 4.3: Implement `OilBuffer::apply`**

Add `apply` to the `impl OilBuffer` block in `oil.rs`:

```rust
/// Diff rope against snapshot and execute filesystem operations.
/// Order: renames → deletes → creates.
/// On error: stops, returns the error (caller echoes it).
/// On success: refreshes snapshot to new disk state.
pub fn apply(&mut self) -> std::io::Result<()> {
    let current_names: Vec<String> = self
        .content
        .as_string()
        .lines()
        .map(|l| l.trim().to_owned())
        .filter(|l| !l.is_empty())
        .collect();

    let snap_names: std::collections::HashSet<&str> =
        self.snapshot.iter().map(|e| e.name.as_str()).collect();
    let curr_set: std::collections::HashSet<&str> =
        current_names.iter().map(|s| s.as_str()).collect();

    let deleted: Vec<&OilEntry> = self
        .snapshot
        .iter()
        .filter(|e| !curr_set.contains(e.name.as_str()))
        .collect();

    let created: Vec<&str> = current_names
        .iter()
        .map(|s| s.as_str())
        .filter(|name| !snap_names.contains(*name))
        .collect();

    // Rename heuristic: exactly 1 delete + 1 create → use fs::rename
    if deleted.len() == 1 && created.len() == 1 {
        let from = self.dir.join(&deleted[0].name);
        let to   = self.dir.join(created[0]);
        std::fs::rename(&from, &to)?;
    } else {
        // Execute renames first (none here), then deletes, then creates.
        for entry in &deleted {
            let path = self.dir.join(&entry.name);
            if entry.is_dir {
                std::fs::remove_dir_all(&path)?;
            } else {
                std::fs::remove_file(&path)?;
            }
        }
        for name in &created {
            // Names ending in '/' create directories; others create files.
            let clean = name.trim_end_matches('/');
            let path = self.dir.join(clean);
            if name.ends_with('/') {
                std::fs::create_dir_all(&path)?;
            } else {
                std::fs::File::create(&path)?;
            }
        }
    }

    // Refresh snapshot from disk.
    self.snapshot = read_dir_entries(&self.dir)?;
    self.content  = render_to_buffer(&self.snapshot);
    Ok(())
}
```

- [ ] **Step 4.4: Run apply tests**

```bash
cargo test -p lattice-ui-tui --lib oil 2>&1 | tail -10
```

Expected: all oil tests pass.

- [ ] **Step 4.5: Run full suite**

```bash
cargo test -p lattice-ui-tui --lib 2>&1 | tail -5
```

- [ ] **Step 4.6: Commit**

```bash
git add crates/lattice-ui-tui/src/oil.rs
git commit -m "feat: implement OilBuffer::apply with rename/delete/create diff"
```

---

## Task 5: BufferKind::Oil + BufferRegistry

**Files:**
- Modify: `crates/lattice-ui-tui/src/buffers.rs`
- Modify: `crates/lattice-ui-tui/src/buffer_registry.rs`

- [ ] **Step 5.1: Write failing tests**

Add to `buffers.rs` tests:

```rust
    #[test]
    fn oil_is_writable() {
        assert!(!BufferKind::Oil.is_read_only());
    }

    #[test]
    fn oil_label() {
        assert_eq!(BufferKind::Oil.label(), "oil");
    }
```

Add to `buffer_registry.rs` tests:

```rust
    #[test]
    fn oil_with_dir_finds_match() {
        let mut r = BufferRegistry::new();
        let id = BufferId::next();
        let dir = std::path::PathBuf::from("/some/dir");
        let oil = crate::oil::OilBuffer::open(&dir).unwrap_or_else(|_| {
            // temp dir for test
            let tmp = std::env::temp_dir().join(format!("oil-test-{}", id.0));
            std::fs::create_dir_all(&tmp).unwrap();
            crate::oil::OilBuffer::open(&tmp).unwrap()
        });
        r.insert(BufferEntry {
            id,
            flags: BufferFlags::default(),
            data: BufferData::Oil(oil),
        });
        assert_eq!(r.oil_ids_sorted(), vec![id]);
    }
```

- [ ] **Step 5.2: Run tests to verify failure**

```bash
cargo test -p lattice-ui-tui --lib buffers 2>&1 | tail -10
cargo test -p lattice-ui-tui --lib buffer_registry 2>&1 | tail -10
```

Expected: compile errors.

- [ ] **Step 5.3: Add `BufferKind::Oil` to `buffers.rs`**

```rust
pub enum BufferKind {
    #[default]
    Document,
    Help,
    FileTree,
    Oil,   // writable flat directory listing
}
```

Update `is_read_only`:

```rust
pub fn is_read_only(self) -> bool {
    matches!(self, BufferKind::Help | BufferKind::FileTree)
}
```

Update `label`:

```rust
pub fn label(self) -> &'static str {
    match self {
        BufferKind::Document => "document",
        BufferKind::Help     => "help",
        BufferKind::FileTree => "file-tree",
        BufferKind::Oil      => "oil",
    }
}
```

- [ ] **Step 5.4: Add `BufferData::Oil` and accessors to `buffer_registry.rs`**

Add import at top:
```rust
use crate::oil::OilBuffer;
```

Add to `BufferData` enum:
```rust
pub enum BufferData {
    Document(DocumentEntry),
    FileTree(FileTreeBuffer),
    Help(HelpBuffer),
    Oil(OilBuffer),      // NEW
}
```

Add to `BufferEntry::kind()`:
```rust
BufferData::Oil(_) => BufferKind::Oil,
```

Add accessor methods to `BufferEntry`:
```rust
pub fn oil(&self) -> Option<&OilBuffer> {
    match &self.data {
        BufferData::Oil(o) => Some(o),
        _ => None,
    }
}

pub fn oil_mut(&mut self) -> Option<&mut OilBuffer> {
    match &mut self.data {
        BufferData::Oil(o) => Some(o),
        _ => None,
    }
}
```

Add registry helpers:
```rust
pub fn oil_ids_sorted(&self) -> Vec<BufferId> {
    let mut ids: Vec<BufferId> = self
        .by_id
        .iter()
        .filter(|(_, e)| matches!(e.data, BufferData::Oil(_)))
        .map(|(id, _)| *id)
        .collect();
    ids.sort();
    ids
}

pub fn oil_with_dir(&self, dir: &std::path::Path) -> Option<BufferId> {
    for entry in self.by_id.values() {
        if let BufferData::Oil(o) = &entry.data
            && o.dir == dir
        {
            return Some(entry.id);
        }
    }
    None
}

pub fn oil(&self, id: BufferId) -> Option<&OilBuffer> {
    self.by_id.get(&id).and_then(BufferEntry::oil)
}

pub fn oil_mut(&mut self, id: BufferId) -> Option<&mut OilBuffer> {
    self.by_id.get_mut(&id).and_then(BufferEntry::oil_mut)
}
```

- [ ] **Step 5.5: Run tests**

```bash
cargo test -p lattice-ui-tui --lib 2>&1 | tail -5
```

Expected: all pass.

- [ ] **Step 5.6: Commit**

```bash
git add crates/lattice-ui-tui/src/buffers.rs crates/lattice-ui-tui/src/buffer_registry.rs
git commit -m "feat: add BufferKind::Oil and BufferRegistry oil accessors"
```

---

## Task 6: Effect::OpenOil + ex-commands (`:oil`, `:filetree`)

**Files:**
- Modify: `crates/lattice-grammar/src/effect.rs`
- Modify: `crates/lattice-grammar/src/ex_commands.rs`

- [ ] **Step 6.1: Add `Effect::OpenOil` to `effect.rs`**

Find the `OpenFileTree` variant (around line 178). Add immediately after `CloseFileTree`:

```rust
/// `:Oil [path]` -- open an oil buffer for `path` (flat editable listing).
/// Absent = current document's parent directory / cwd.
OpenOil {
    dir: Option<PathBuf>,
},
```

- [ ] **Step 6.2: Run grammar tests**

```bash
cargo test -p lattice-grammar --lib 2>&1 | tail -5
```

Expected: all pass (new variant is non-exhaustive only if used in match — app.rs will need updating in Task 7).

- [ ] **Step 6.3: Add `oil` field to `ExCommandIds` and register `:oil` command**

In `ex_commands.rs`, add to the `ExCommandIds` struct after `file_tree_close`:

```rust
pub oil: ExCommandId,
```

Find where `file_tree_close` is registered (around line 477). Add the `:oil` command immediately after:

```rust
let oil = registry.register_ex_command(
    "ex:oil",
    "Open an oil buffer (`:Oil [path]`). Absent = current dir.",
    ExCommandSpec {
        latency_class: LatencyClass::Display,
        accepts_bang: false,
        accepts_range: false,
        parse_args: Box::new(parse_optional_path),
        apply: Box::new(|ctx| {
            let dir = match &ctx.args {
                Args::String(p) if !p.is_empty() => Some(std::path::PathBuf::from(p.as_str())),
                _ => None,
            };
            Ok(Effect::OpenOil { dir })
        }),
        args_schema: vec![ArgSpec {
            name: "dir",
            kind: ArgKind::String,
            doc: "Directory to open. Absent = current document's parent.",
            prompt: "dir:",
            default: ArgDefault::None,
            completion: Some("gen:files"),
        }],
        surface_form: SurfaceForm::Keyword,
    },
);
```

- [ ] **Step 6.4: Rename `:tree` → `:filetree`**

Find the registration at line ~451:

```rust
let file_tree = registry.register_ex_command(
    "ex:tree",
```

Change to:

```rust
let file_tree = registry.register_ex_command(
    "ex:filetree",
    "Open a file-tree buffer (`:Filetree [path]`). Absent = current dir.",
```

- [ ] **Step 6.5: Add `oil` to the returned `ExCommandIds` struct literal**

Find the struct literal that returns from `populate()` (around line 1011). Add:

```rust
oil,
```

alongside the other fields.

- [ ] **Step 6.6: Run tests**

```bash
cargo test -p lattice-grammar --lib 2>&1 | tail -5
cargo test -p lattice-ui-tui --lib 2>&1 | tail -5
```

Expected: all pass (app.rs match on Effect may produce warnings — handled in Task 7).

- [ ] **Step 6.7: Commit**

```bash
git add crates/lattice-grammar/src/effect.rs crates/lattice-grammar/src/ex_commands.rs
git commit -m "feat: add Effect::OpenOil and :oil / :filetree ex-commands"
```

---

## Task 7: App wiring — do_open_oil, :edit dir branch, :w dispatch, oil invocation

**Files:**
- Modify: `crates/lattice-ui-tui/src/app.rs`

- [ ] **Step 7.1: Add `do_open_oil` method**

Find `fn do_open_file_tree` (around line 12176). Add a parallel method immediately before or after it:

```rust
fn do_open_oil(&mut self, dir: Option<std::path::PathBuf>) {
    let dir = match dir {
        Some(p) => p,
        None => match self.document.path().and_then(|p| p.parent().map(Into::into)) {
            Some(parent) => parent,
            None => match std::env::current_dir() {
                Ok(p) => p,
                Err(e) => {
                    self.set_message(EchoLevel::Error, format!("cwd error: {e}"));
                    return;
                }
            },
        },
    };
    if let Some(existing_id) = self.buffers.oil_with_dir(&dir) {
        self.activate_oil(existing_id);
        self.set_message(EchoLevel::Info, format!("oil: {} (already open)", dir.display()));
        return;
    }
    let oil = match crate::oil::OilBuffer::open(&dir) {
        Ok(o) => o,
        Err(e) => {
            self.set_message(EchoLevel::Error, format!("oil open error: {}: {e}", dir.display()));
            return;
        }
    };
    if matches!(self.active_buffer, BufferKind::Document) {
        let cur = self.cursor;
        self.push_position_history(cur, PositionSource::AutoJump);
    }
    let new_id = oil.id;
    self.buffers.insert(BufferEntry {
        id: new_id,
        flags: BufferFlags::default(),
        data: BufferData::Oil(oil),
    });
    self.snapshot_active_pane();
    self.snapshot_active_document();
    self.active_buffer = BufferKind::Oil;
    let pane = self.pane_tree.active_mut();
    pane.buffer = BufferKind::Oil;
    pane.buffer_id = new_id;
    pane.cursor = Position::ZERO;
    pane.scroll = 0;
    self.pending = Pending::None;
    self.set_message(EchoLevel::Info, format!("oil: {}", dir.display()));
}
```

- [ ] **Step 7.2: Add `activate_oil` method (mirrors `activate_file_tree`)**

Find `fn activate_file_tree` (around line 8958). Add immediately after:

```rust
pub fn activate_oil(&mut self, id: BufferId) {
    if self.buffers.oil(id).is_none() { return; }
    if id == self.active_pane_buffer_id() && matches!(self.active_buffer, BufferKind::Oil) {
        return;
    }
    let oil_cursor = self.buffers.oil(id).map(|o| o.cursor).unwrap_or(Position::ZERO);
    let oil_scroll = self.buffers.oil(id).map(|o| o.scroll).unwrap_or(0);
    self.active_buffer = BufferKind::Oil;
    let pane = self.pane_tree.active_mut();
    pane.buffer = BufferKind::Oil;
    pane.buffer_id = id;
    pane.cursor = oil_cursor;
    pane.scroll = oil_scroll;
    self.cursor = oil_cursor;
    self.scroll = oil_scroll as u32;
    self.pending = Pending::None;
}
```

- [ ] **Step 7.3: Change `do_edit` directory branch to open oil**

Find at line ~4884:

```rust
if let Ok(meta) = std::fs::metadata(&target)
    && meta.is_dir()
{
    self.do_open_file_tree(Some(target));
    return;
}
```

Change to:

```rust
if let Ok(meta) = std::fs::metadata(&target)
    && meta.is_dir()
{
    self.do_open_oil(Some(target));
    return;
}
```

- [ ] **Step 7.4: Wire `Effect::OpenOil` in `apply_effect`**

Find where `Effect::OpenFileTree` and `Effect::CloseFileTree` are handled (around line 11099). Add:

```rust
Effect::OpenOil { dir } => self.do_open_oil(dir),
```

- [ ] **Step 7.5: Update `do_write` to call `oil.apply()` when active buffer is Oil**

Find `fn do_write` (around line 10743). Add a guard at the top:

```rust
fn do_write(&mut self, path: Option<std::path::PathBuf>) {
    // Oil buffer: :w applies pending filesystem operations instead of file write.
    if matches!(self.active_buffer, BufferKind::Oil) {
        let oil_id = self.active_pane_buffer_id();
        if let Some(oil) = self.buffers.oil_mut(oil_id) {
            let dir_display = oil.dir.display().to_string();
            match oil.apply() {
                Ok(()) => self.set_message(EchoLevel::Info, format!("oil: applied changes in {dir_display}")),
                Err(e) => self.set_message(EchoLevel::Error, format!("oil apply error: {e}")),
            }
        }
        return;
    }
    // ... existing document write code unchanged ...
```

- [ ] **Step 7.6: Add `run_oil_invocation` and `do_oil_follow`**

Find `fn run_file_tree_invocation` (around line 10804). Add parallel methods:

```rust
fn run_oil_invocation(&mut self, inv: CommandInvocation) {
    // Oil is writable — operators and motions both run via the document path.
    // We delegate to `run_document_invocation` so dd/cc/etc. work normally
    // against the oil rope.
    self.run_document_invocation(inv);
}

fn do_oil_follow(&mut self) {
    let active_id = self.active_pane_buffer_id();
    let idx = self.cursor.line as usize;
    let Some(oil) = self.buffers.oil(active_id) else { return; };
    // entry_at_cursor reads snapshot order; cursor maps 1:1 to snapshot.
    let Some(entry) = oil.entry_at_cursor().cloned() else { return; };
    let dir = oil.dir.clone();
    if entry.is_dir {
        if let Some(oil) = self.buffers.oil_mut(active_id) {
            let sub = dir.join(&entry.name);
            if let Err(e) = oil.navigate_into(sub) {
                self.set_message(EchoLevel::Error, format!("oil navigate: {e}"));
            } else {
                self.cursor = Position::ZERO;
                self.scroll = 0;
            }
        }
    } else {
        let path = dir.join(&entry.name);
        self.do_edit(Some(path), false);
    }
}
```

- [ ] **Step 7.7: Wire Oil into the main invocation dispatcher**

Find:
```rust
if matches!(self.active_buffer, BufferKind::FileTree) {
    self.run_file_tree_invocation(inv);
    return;
}
```

Add immediately before it:

```rust
if matches!(self.active_buffer, BufferKind::Oil) {
    self.run_oil_invocation(inv);
    return;
}
```

- [ ] **Step 7.8: Wire `Action::FollowLink` for Oil in `apply`**

Find the `Action::FollowLink` dispatch (around line 3803):

```rust
Action::FollowLink => match self.active_buffer {
    BufferKind::FileTree => self.do_file_tree_follow(),
    ...
```

Add:

```rust
Action::FollowLink => match self.active_buffer {
    BufferKind::Oil      => self.do_oil_follow(),
    BufferKind::FileTree => self.do_file_tree_follow(),
    ...
```

- [ ] **Step 7.9: Update snapshot/scroll handling for Oil panes**

Find where `BufferKind::FileTree` is handled in `snapshot_active_pane` (around line 11899):

```rust
BufferKind::FileTree => {
    if let Some(t) = self.buffers.file_tree_mut(pane_id) {
        t.cursor = self.cursor;
        t.scroll = self.scroll as usize;
    }
}
```

Add an Oil arm immediately before:

```rust
BufferKind::Oil => {
    if let Some(o) = self.buffers.oil_mut(pane_id) {
        o.cursor = self.cursor;
        o.scroll = self.scroll as usize;
    }
}
```

- [ ] **Step 7.10: Update `active_text` for Oil (used by motions)**

Search for `active_text` in `app.rs`:

```bash
grep -n "fn active_text\|active_text()" crates/lattice-ui-tui/src/app.rs | head -10
```

Find where it returns text per buffer kind. Add an Oil arm that returns `oil.content.as_string()` following the `FileTree` pattern.

- [ ] **Step 7.11: Update `line_count` for Oil**

Search for `line_count` dispatch in `app.rs` and add the Oil arm:

```bash
grep -n "line_count\|FileTree.*line_count" crates/lattice-ui-tui/src/app.rs | head -10
```

Add `BufferKind::Oil => self.buffers.oil(id).map(|o| o.line_count()).unwrap_or(0)` following the FileTree pattern.

- [ ] **Step 7.12: Build to catch remaining exhaustive match gaps**

```bash
cargo build -p lattice-ui-tui 2>&1 | grep "error\[" | head -20
```

Fix any remaining `BufferKind::Oil` non-exhaustive matches by following the `FileTree` pattern in each match arm.

- [ ] **Step 7.13: Run full test suite**

```bash
cargo test -p lattice-ui-tui --lib 2>&1 | tail -5
```

- [ ] **Step 7.14: Commit**

```bash
git add crates/lattice-ui-tui/src/app.rs
git commit -m "feat: wire OilBuffer into App — do_open_oil, :edit dir, :w dispatch, oil follow"
```

---

## Task 8: Oil pane renderer (`draw_oil_pane`)

**Files:**
- Modify: `crates/lattice-ui-tui/src/render.rs`

- [ ] **Step 8.1: Add `draw_oil_pane` function**

Find `fn draw_file_tree_pane` in `render.rs`. Add the following function immediately after it:

```rust
fn draw_oil_pane(
    frame: &mut Frame,
    area: Rect,
    app: &App,
    pane: &crate::pane::PaneState,
    is_active: bool,
) {
    let Some(oil) = app.buffers.oil(pane.buffer_id) else { return; };
    let (cursor_line, scroll) = if is_active {
        (app.cursor.line as usize, app.scroll as usize)
    } else {
        (pane.cursor.line as usize, pane.scroll as usize)
    };
    let viewport = area.height as usize;
    let nerd_fonts = app.theme.nerd_fonts;
    let theme = &app.theme;
    let lines: Vec<Line> = oil
        .content
        .as_string()
        .split('\n')
        .enumerate()
        .zip(oil.snapshot_entries().iter().chain(std::iter::repeat(&crate::oil::OilEntry {
            name: String::new(),
            is_dir: false,
        })))
        .skip(scroll)
        .take(viewport)
        .map(|((i, name_str), entry)| {
            use crate::icons::icon_for_entry;
            let line_idx = scroll + i;
            let is_cursor = is_active && line_idx == cursor_line;
            let path = oil.dir.join(&entry.name);
            let (icon, entry_style) = icon_for_entry(&path, entry.is_dir, nerd_fonts, theme);
            let cursor_mod = if is_cursor { Modifier::REVERSED } else { Modifier::empty() };
            // Icon span + name span
            let icon_span = Span::styled(icon.to_string(), entry_style.add_modifier(cursor_mod));
            let name_span = Span::styled(name_str.to_string(), entry_style.add_modifier(cursor_mod));
            Line::from(vec![icon_span, name_span])
        })
        .collect();
    frame.render_widget(Paragraph::new(lines), area);
    if is_active && area.height > 0 && area.width > 0 {
        let row_off = (app.cursor.line as usize).saturating_sub(app.scroll as usize);
        let row_off = row_off.min(area.height.saturating_sub(1) as usize);
        let icon_width = if nerd_fonts { 2 } else { 0 };
        let col_off = (app.cursor.byte as usize + icon_width).min(area.width.saturating_sub(1) as usize);
        frame.set_cursor_position((area.x + col_off as u16, area.y + row_off as u16));
    }
}
```

- [ ] **Step 8.2: Add `snapshot_entries` helper to `OilBuffer`**

The renderer above calls `oil.snapshot_entries()`. Add to `impl OilBuffer` in `oil.rs`:

```rust
pub fn snapshot_entries(&self) -> &[OilEntry] {
    &self.snapshot
}
```

- [ ] **Step 8.3: Wire `draw_oil_pane` into the pane kind dispatch**

Find:
```rust
crate::buffers::BufferKind::FileTree => {
    draw_file_tree_pane(frame, content_rect, app, &pane, is_active);
}
```

Add immediately after:

```rust
crate::buffers::BufferKind::Oil => {
    draw_oil_pane(frame, content_rect, app, &pane, is_active);
}
```

- [ ] **Step 8.4: Update the status line for Oil panes**

Find the status line label match (around line 1170):

```rust
crate::buffers::BufferKind::FileTree => app
    .buffers
    .file_tree(pane.buffer_id)
    .map(|t| format!("[tree] {}", t.root.display()))
    .unwrap_or_else(|| "[tree]".to_string()),
```

Add:

```rust
crate::buffers::BufferKind::Oil => app
    .buffers
    .oil(pane.buffer_id)
    .map(|o| {
        let dirty = if o.is_dirty() { " [+]" } else { "" };
        format!("[oil] {}{dirty}", o.dir.display())
    })
    .unwrap_or_else(|| "[oil]".to_string()),
```

- [ ] **Step 8.5: Build and test**

```bash
cargo build -p lattice-ui-tui 2>&1 | grep "error" | head -10
cargo test -p lattice-ui-tui --lib 2>&1 | tail -5
```

- [ ] **Step 8.6: Commit**

```bash
git add crates/lattice-ui-tui/src/render.rs crates/lattice-ui-tui/src/oil.rs
git commit -m "feat: add draw_oil_pane renderer with icons and dirty status indicator"
```

---

## Task 9: Keybindings — `-` key and Oil input handling

**Files:**
- Modify: `crates/lattice-ui-tui/src/app.rs` (add `Action::OilNavigateUp`)
- Modify: `crates/lattice-ui-tui/src/input.rs`

- [ ] **Step 9.1: Add `Action::OilNavigateUp` to the `Action` enum**

In `app.rs`, find `Action::FollowLink` (around line 734). Add immediately after it:

```rust
/// `-` in any normal-mode context — context-sensitive:
/// • Document / FileTree → open oil for parent dir of current file / hovered entry
/// • Oil buffer → `oil.navigate_up()`
OilNavigateUp,
```

- [ ] **Step 9.2: Wire `Action::OilNavigateUp` in `apply`**

Find the `Action::FollowLink` dispatch. Add immediately after it:

```rust
Action::OilNavigateUp => self.do_oil_navigate_up(),
```

- [ ] **Step 9.3: Add `do_oil_navigate_up` method to `app.rs`**

```rust
fn do_oil_navigate_up(&mut self) {
    match self.active_buffer {
        BufferKind::Oil => {
            let id = self.active_pane_buffer_id();
            if let Some(oil) = self.buffers.oil_mut(id) {
                if let Err(e) = oil.navigate_up() {
                    self.set_message(EchoLevel::Error, format!("oil navigate up: {e}"));
                    return;
                }
                self.cursor = Position::ZERO;
                self.scroll = 0;
            }
        }
        BufferKind::FileTree => {
            // Open oil for the hovered entry's directory
            let id = self.active_pane_buffer_id();
            let dir = self
                .buffers
                .file_tree(id)
                .and_then(|t| t.entry_at_cursor())
                .map(|e| {
                    if matches!(e.kind, crate::file_tree::FileTreeEntryKind::Directory { .. }) {
                        e.path.clone()
                    } else {
                        e.path.parent().unwrap_or(&e.path).to_path_buf()
                    }
                });
            self.do_open_oil(dir);
        }
        _ => {
            // Document or other: open oil for parent of current file
            let dir = self
                .document
                .path()
                .and_then(|p| p.parent().map(Into::into));
            self.do_open_oil(dir);
        }
    }
}
```

- [ ] **Step 9.4: Add `-` key to the Normal mode translator and Oil buffer-local handler**

In `crates/lattice-ui-tui/src/input.rs`:

**Oil buffer-local block** — update the existing block that handles Help/FileTree (around line 140):

```rust
// BEFORE:
if matches!(ctx.active_buffer, BufferKind::Help | BufferKind::FileTree)
    && matches!(ctx.modal, ModalState::Normal)
    && matches!(ctx.pending, Pending::None)
{
    match event.code {
        KeyCode::Esc   => return Action::HelpDismiss,
        KeyCode::Char('q') if !ctx.recording_macro => return Action::HelpDismiss,
        KeyCode::Enter => return Action::FollowLink,
        _ => {}
    }
}

// AFTER (split into two blocks):
if matches!(ctx.active_buffer, BufferKind::Help | BufferKind::FileTree)
    && matches!(ctx.modal, ModalState::Normal)
    && matches!(ctx.pending, Pending::None)
{
    match event.code {
        KeyCode::Esc   => return Action::HelpDismiss,
        KeyCode::Char('q') if !ctx.recording_macro => return Action::HelpDismiss,
        KeyCode::Enter => return Action::FollowLink,
        KeyCode::Char('-') => return Action::OilNavigateUp,
        _ => {}
    }
}

if matches!(ctx.active_buffer, BufferKind::Oil)
    && matches!(ctx.modal, ModalState::Normal)
    && matches!(ctx.pending, Pending::None)
{
    match event.code {
        KeyCode::Enter    => return Action::FollowLink,
        KeyCode::Char('-') => return Action::OilNavigateUp,
        _ => {}
    }
}
```

**Normal mode global** — add `-` to `translate_normal` for Document context.

Find where `translate_normal` handles keys (it's a large match). Add before the final `_ =>` arm:

```rust
KeyCode::Char('-') => Action::OilNavigateUp,
```

- [ ] **Step 9.5: Build and fix any remaining exhaustive match warnings**

```bash
cargo build -p lattice-ui-tui 2>&1 | grep "error\|warning.*non-exhaustive" | head -20
```

Fix any `Action::OilNavigateUp` gaps in existing match arms by adding `Action::OilNavigateUp => {}` (no-op) where appropriate.

- [ ] **Step 9.6: Run full test suite**

```bash
cargo test -p lattice-ui-tui --lib 2>&1 | tail -5
cargo test -p lattice-grammar --lib 2>&1 | tail -5
```

Expected: all pass.

- [ ] **Step 9.7: Commit**

```bash
git add crates/lattice-ui-tui/src/app.rs crates/lattice-ui-tui/src/input.rs
git commit -m "feat: add - keybinding for context-sensitive oil navigation"
```

---

## Self-Review

**Spec coverage check:**

| Spec requirement | Task |
|---|---|
| Nerd Font icons + color by extension | Task 1 (icons.rs), Task 2 (tree renderer) |
| Fallback to color-only when `ui.nerd_fonts=false` | Task 1 (`icon_for_entry` nerd_fonts=false path) |
| `file_tree_dir_style`, `file_tree_hidden_style`, `file_tree_file_style` on Theme | Task 1 |
| FileTree stays read-only, icons only | Task 2 |
| OilBuffer: open, navigate_into, navigate_up, is_dirty | Task 3 |
| Oil rope has bare names only (no icons) | Task 3 (tested) |
| apply: rename heuristic (1:1 → fs::rename) | Task 4 |
| apply: multiple deletes + creates | Task 4 (tested) |
| apply: renames before deletes before creates | Task 4 |
| apply: snapshot refresh on success | Task 4 (tested) |
| `BufferKind::Oil` writable | Task 5 |
| `BufferRegistry` oil accessors | Task 5 |
| `Effect::OpenOil` | Task 6 |
| `:oil [dir]` ex-command | Task 6 |
| `:filetree [dir]` rename from `:tree` | Task 6 |
| `:edit <dir>` → oil | Task 7 |
| `:w` on oil → apply | Task 7 |
| `<CR>` in oil → follow (dir=navigate, file=open) | Task 7 |
| Oil renderer with icons as spans | Task 8 |
| `[oil] <dir> [+]` status line | Task 8 |
| `-` in Document/FileTree → oil parent | Task 9 |
| `-` in Oil → navigate_up | Task 9 |
| `<CR>` in FileTree → `-` also routes correctly | Task 9 |

All spec requirements covered. ✓
