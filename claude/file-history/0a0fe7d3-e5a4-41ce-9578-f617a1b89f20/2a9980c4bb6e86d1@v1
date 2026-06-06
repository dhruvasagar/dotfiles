//! Buffer-kind discriminator for the multi-buffer foundation
//! (DESIGN.md §5.9).
//!
//! Phase 1 wiring: every concrete buffer type the App can hold a
//! cursor in (today: a code [`Document`] and an optional
//! [`HelpBuffer`]) is tagged with a [`BufferKind`]. The App carries
//! one `active_buffer: BufferKind` which decides where keystrokes
//! land -- a `j` in Normal mode resolves the same `line_down`
//! motion against the active buffer, regardless of kind.
//!
//! [`BufferId`] is a stable, monotonically-allocated handle. v1 only
//! has at most one buffer per kind, but the id scaffolding lands now
//! so position-history entries (§5.1.1) can identify "which buffer"
//! once Phase B.1.c spawns multiple Document buffers.
//!
//! [`Document`]: lattice_core::Document
//! [`HelpBuffer`]: crate::help::HelpBuffer

use std::sync::atomic::{AtomicU32, Ordering};

/// Which kind of buffer the App's input pipeline currently routes
/// to. The chord grammar, motions, and position history are shared;
/// kind only matters at a few discrete decision points: which cursor
/// a motion mutates, whether mutating actions are accepted (most
/// non-document kinds are read-only), and which buffer-local
/// bindings apply (Help binds `<CR>` to follow-link, FileTree binds
/// `<CR>` to follow-entry, etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BufferKind {
    /// The user's edit-target -- one [`Document`] today.
    ///
    /// [`Document`]: lattice_core::Document
    #[default]
    Document,
    /// A `:describe-*` / `:apropos` / `:keymap` view. Read-only;
    /// motions / yank work, edits don't.
    Help,
    /// Filesystem hierarchy view (DESIGN.md §5.9 buffer-as-content).
    /// Rope-backed with one rendered line per visible entry; `<CR>`
    /// on a directory toggles expansion, on a file opens it as a
    /// new Document buffer.
    FileTree,
}

impl BufferKind {
    /// Whether mutating operators (delete, change, paste, insert)
    /// are accepted on this kind. Only [`BufferKind::Document`] is
    /// writable; help and panel kinds are read-only (yank still
    /// works -- it's not a mutation).
    pub fn is_read_only(self) -> bool {
        !matches!(self, BufferKind::Document)
    }

    /// Short label for echo-area diagnostics.
    pub fn label(self) -> &'static str {
        match self {
            BufferKind::Document => "document",
            BufferKind::Help => "help",
            BufferKind::FileTree => "file-tree",
        }
    }
}

/// Stable monotonic handle identifying one buffer instance. Two
/// buffers with the same [`BufferKind`] still have distinct ids.
/// The App allocates these via [`BufferId::next`] at buffer-
/// creation time and stores them on each buffer + on every
/// position-history entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BufferId(pub u32);

impl BufferId {
    /// Allocate a fresh id. Process-global, monotonic, never
    /// recycled (collision impossible inside one process lifetime
    /// short of 2^32 buffer creations).
    pub fn next() -> Self {
        static NEXT: AtomicU32 = AtomicU32::new(1);
        Self(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}

/// Vim-style per-buffer flags (DESIGN.md §5.9). The shape is fixed
/// now so additions don't churn every call site; v1 ships with
/// `listed` populated (`:bn` / `:bp` / `:ls` skip unlisted buffers
/// once the wiring lands) and `hidden` reserved for "keep loaded
/// without a window" semantics. Both default to "normal buffer"
/// (listed = true, hidden = false).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BufferFlags {
    /// Whether the buffer appears in `:bn` / `:bp` / `:ls`.
    /// Vim's "unlisted" buffer (`:setlocal nobuflisted`).
    pub listed: bool,
    /// Whether the buffer stays loaded even when no window shows
    /// it. Vim's `'hidden'` option per buffer. v1 doesn't gc on
    /// pane close, so this is informational; future cleanup
    /// passes will read it.
    pub hidden: bool,
}

impl Default for BufferFlags {
    fn default() -> Self {
        Self {
            listed: true,
            hidden: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn document_is_writable() {
        assert!(!BufferKind::Document.is_read_only());
    }

    #[test]
    fn help_is_read_only() {
        assert!(BufferKind::Help.is_read_only());
    }

    #[test]
    fn file_tree_is_read_only() {
        assert!(BufferKind::FileTree.is_read_only());
    }

    #[test]
    fn buffer_id_is_monotonic() {
        let a = BufferId::next();
        let b = BufferId::next();
        assert!(b.0 > a.0);
    }

    #[test]
    fn default_kind_is_document() {
        assert_eq!(BufferKind::default(), BufferKind::Document);
    }
}
