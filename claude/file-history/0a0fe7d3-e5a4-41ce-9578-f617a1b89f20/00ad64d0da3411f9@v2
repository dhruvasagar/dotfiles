//! Unified buffer registry (DESIGN.md §5.9).
//!
//! Every concrete buffer the App can route input through -- a code
//! [`Document`] today, a [`FileTreeBuffer`] tomorrow -- lives in
//! [`BufferRegistry`] keyed by [`BufferId`]. A single registry
//! gives `:bn` / `:bp` / `:ls` / `:bd` a consistent surface across
//! buffer kinds; multiple file trees coexist with multiple
//! documents under the same shape.
//!
//! Help buffers stay overlay-rendered for v1 (transient popup),
//! so they're not in the registry yet -- moving them in is a
//! follow-up that doesn't require structural change.
//!
//! Hot-path access: the *active* document's actor handle, syntax
//! state, and last-parsed-version live on [`crate::app::App`]
//! directly so motion / dispatch code stays unchanged. Switching
//! the active document snapshots those fields back into the
//! matching registry entry and loads from the destination's.
//!
//! [`Document`]: lattice_core::Document
//! [`FileTreeBuffer`]: crate::file_tree::FileTreeBuffer

use std::collections::HashMap;

use lattice_runtime::DocumentHandle;
use lattice_syntax::Syntax;

use crate::buffers::{BufferFlags, BufferId, BufferKind};
use crate::file_tree::FileTreeBuffer;
use crate::oil::OilBuffer;
use crate::help::HelpBuffer;

/// Per-document registry payload. Each entry carries the actor
/// handle plus per-document tree-sitter [`Syntax`] state, fold
/// list, and any other "lives with this buffer until it
/// closes" state.
///
/// **Active vs inactive split.** The currently-active buffer's
/// `syntax` / `folds` slots are conventionally `None` / empty
/// because the live state lives on `App.syntax` / `App.folds`
/// for hot-path access. Switching buffers via
/// `App::activate_document` snapshots the old buffer's live
/// state into its entry, then loads the destination's state
/// from its entry into the App's hot-path fields. The
/// `App::activate_buffer_state` hook then refreshes anything
/// that needs recomputing for the newly-active buffer (e.g.
/// fold recompute when switching into a buffer for the first
/// time).
#[derive(Debug)]
pub struct DocumentEntry {
    pub id: BufferId,
    pub handle: DocumentHandle,
    pub syntax: Option<Syntax>,
    pub last_parsed_text_version: u64,
    /// Per-buffer fold list. Empty means "not yet computed for
    /// this buffer." The activation hook recomputes against
    /// the current `App.foldmethod` on first activation;
    /// subsequent re-activations (switching back to this
    /// buffer) restore the user's open/closed state without
    /// re-walking the buffer.
    pub folds: Vec<crate::app::Fold>,
}

/// One slot in the registry. The kind-specific data lives in
/// [`BufferData`]; flags + id + label apply uniformly.
#[derive(Debug)]
pub struct BufferEntry {
    pub id: BufferId,
    pub flags: BufferFlags,
    pub data: BufferData,
}

impl BufferEntry {
    pub fn kind(&self) -> BufferKind {
        match &self.data {
            BufferData::Document(_) => BufferKind::Document,
            BufferData::FileTree(_) => BufferKind::FileTree,
            BufferData::Help(_) => BufferKind::Help,
            BufferData::Oil(_) => BufferKind::Oil,
        }
    }

    pub fn document(&self) -> Option<&DocumentEntry> {
        match &self.data {
            BufferData::Document(d) => Some(d),
            _ => None,
        }
    }

    pub fn document_mut(&mut self) -> Option<&mut DocumentEntry> {
        match &mut self.data {
            BufferData::Document(d) => Some(d),
            _ => None,
        }
    }

    pub fn file_tree(&self) -> Option<&FileTreeBuffer> {
        match &self.data {
            BufferData::FileTree(t) => Some(t),
            _ => None,
        }
    }

    pub fn file_tree_mut(&mut self) -> Option<&mut FileTreeBuffer> {
        match &mut self.data {
            BufferData::FileTree(t) => Some(t),
            _ => None,
        }
    }

    pub fn help(&self) -> Option<&HelpBuffer> {
        match &self.data {
            BufferData::Help(h) => Some(h),
            _ => None,
        }
    }

    pub fn help_mut(&mut self) -> Option<&mut HelpBuffer> {
        match &mut self.data {
            BufferData::Help(h) => Some(h),
            _ => None,
        }
    }

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
}

#[derive(Debug)]
pub enum BufferData {
    Document(DocumentEntry),
    FileTree(FileTreeBuffer),
    /// Help / log / picker-listing buffers placed into a pane
    /// (DESIGN.md §5.9, §5.11). The transient overlay path
    /// (`App.help_buffer`) remains for popup-style displays
    /// (hover, doc lookups, error toasts); persistent help views
    /// (`:lsp-log`, `:lsp-server-log`, `:lsp-trace-log`,
    /// `:describe-*`, `:diagnostics`) route here so they live in
    /// a real pane, can be split, switched, listed via `:ls`,
    /// and updated live when their backing source emits events.
    Help(HelpBuffer),
    /// Flat editable directory listing (oil.nvim-style).
    Oil(OilBuffer),
}

/// The App's buffer registry. Wraps a `HashMap<BufferId,
/// BufferEntry>` with helpers for the common access patterns
/// (look up by id, iterate by kind, sorted ids for `:bn`-style
/// cycling).
#[derive(Debug, Default)]
pub struct BufferRegistry {
    by_id: HashMap<BufferId, BufferEntry>,
}

impl BufferRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, entry: BufferEntry) {
        self.by_id.insert(entry.id, entry);
    }

    pub fn remove(&mut self, id: BufferId) -> Option<BufferEntry> {
        self.by_id.remove(&id)
    }

    pub fn get(&self, id: BufferId) -> Option<&BufferEntry> {
        self.by_id.get(&id)
    }

    pub fn get_mut(&mut self, id: BufferId) -> Option<&mut BufferEntry> {
        self.by_id.get_mut(&id)
    }

    pub fn contains(&self, id: BufferId) -> bool {
        self.by_id.contains_key(&id)
    }

    pub fn len(&self) -> usize {
        self.by_id.len()
    }

    pub fn is_empty(&self) -> bool {
        self.by_id.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &BufferEntry> {
        self.by_id.values()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut BufferEntry> {
        self.by_id.values_mut()
    }

    /// All ids in ascending order. Used by `:bn` / `:bp` for
    /// deterministic cycling order independent of HashMap
    /// hash-randomization.
    pub fn sorted_ids(&self) -> Vec<BufferId> {
        let mut ids: Vec<BufferId> = self.by_id.keys().copied().collect();
        ids.sort();
        ids
    }

    /// All listed ids in ascending order. `:bn` / `:bp` skip
    /// unlisted buffers (vim semantics); `:ls` shows them under a
    /// separate header (post-v1 polish).
    pub fn listed_ids_sorted(&self) -> Vec<BufferId> {
        let mut ids: Vec<BufferId> = self
            .by_id
            .iter()
            .filter(|(_, e)| e.flags.listed)
            .map(|(id, _)| *id)
            .collect();
        ids.sort();
        ids
    }

    /// Document buffers only, sorted by id.
    pub fn document_ids_sorted(&self) -> Vec<BufferId> {
        let mut ids: Vec<BufferId> = self
            .by_id
            .iter()
            .filter(|(_, e)| matches!(e.data, BufferData::Document(_)))
            .map(|(id, _)| *id)
            .collect();
        ids.sort();
        ids
    }

    /// File-tree buffers only, sorted by id.
    pub fn file_tree_ids_sorted(&self) -> Vec<BufferId> {
        let mut ids: Vec<BufferId> = self
            .by_id
            .iter()
            .filter(|(_, e)| matches!(e.data, BufferData::FileTree(_)))
            .map(|(id, _)| *id)
            .collect();
        ids.sort();
        ids
    }

    /// Help buffers only, sorted by id.
    pub fn help_ids_sorted(&self) -> Vec<BufferId> {
        let mut ids: Vec<BufferId> = self
            .by_id
            .iter()
            .filter(|(_, e)| matches!(e.data, BufferData::Help(_)))
            .map(|(id, _)| *id)
            .collect();
        ids.sort();
        ids
    }

    /// First help buffer with the given title, if any. Used by the
    /// `:lsp-log` / `:lsp-trace-log` openers so re-running the
    /// command surfaces the existing buffer rather than allocating
    /// a duplicate.
    pub fn help_with_title(&self, title: &str) -> Option<BufferId> {
        for entry in self.by_id.values() {
            if let BufferData::Help(h) = &entry.data
                && h.title == title
            {
                return Some(entry.id);
            }
        }
        None
    }

    /// First document buffer with the given path, if any. Used by
    /// `:e FILE` to detect "already open".
    pub fn document_with_path(&self, path: &std::path::Path) -> Option<BufferId> {
        for entry in self.by_id.values() {
            if let BufferData::Document(d) = &entry.data
                && d.handle.path() == Some(path.to_path_buf())
            {
                return Some(entry.id);
            }
        }
        None
    }

    /// First file-tree buffer with the given root, if any. Used
    /// by `:Tree path` to detect "already open".
    pub fn file_tree_with_root(&self, root: &std::path::Path) -> Option<BufferId> {
        for entry in self.by_id.values() {
            if let BufferData::FileTree(t) = &entry.data
                && t.root == root
            {
                return Some(entry.id);
            }
        }
        None
    }

    /// Convenience: borrow a document entry by id (returns `None`
    /// if absent OR if the entry is a different kind).
    pub fn document(&self, id: BufferId) -> Option<&DocumentEntry> {
        self.by_id.get(&id).and_then(BufferEntry::document)
    }

    pub fn document_mut(&mut self, id: BufferId) -> Option<&mut DocumentEntry> {
        self.by_id.get_mut(&id).and_then(BufferEntry::document_mut)
    }

    pub fn file_tree(&self, id: BufferId) -> Option<&FileTreeBuffer> {
        self.by_id.get(&id).and_then(BufferEntry::file_tree)
    }

    pub fn file_tree_mut(&mut self, id: BufferId) -> Option<&mut FileTreeBuffer> {
        self.by_id.get_mut(&id).and_then(BufferEntry::file_tree_mut)
    }

    pub fn help(&self, id: BufferId) -> Option<&HelpBuffer> {
        self.by_id.get(&id).and_then(BufferEntry::help)
    }

    pub fn help_mut(&mut self, id: BufferId) -> Option<&mut HelpBuffer> {
        self.by_id.get_mut(&id).and_then(BufferEntry::help_mut)
    }

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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fresh_registry_is_empty() {
        let r = BufferRegistry::new();
        assert!(r.is_empty());
        assert_eq!(r.len(), 0);
    }

    #[test]
    fn sorted_ids_returns_ascending_order() {
        let mut r = BufferRegistry::new();
        let id_a = BufferId::next();
        let id_b = BufferId::next();
        let id_c = BufferId::next();
        // Insert out of order.
        r.insert(BufferEntry {
            id: id_c,
            flags: BufferFlags::default(),
            data: BufferData::FileTree(FileTreeBuffer {
                id: id_c,
                root: std::path::PathBuf::from("/c"),
                entries: Vec::new(),
                content: lattice_core::Buffer::empty(),
                cursor: lattice_protocol::position::Position::ZERO,
                scroll: 0,
                nerd_fonts: false,
            }),
        });
        r.insert(BufferEntry {
            id: id_a,
            flags: BufferFlags::default(),
            data: BufferData::FileTree(FileTreeBuffer {
                id: id_a,
                root: std::path::PathBuf::from("/a"),
                entries: Vec::new(),
                content: lattice_core::Buffer::empty(),
                cursor: lattice_protocol::position::Position::ZERO,
                scroll: 0,
                nerd_fonts: false,
            }),
        });
        r.insert(BufferEntry {
            id: id_b,
            flags: BufferFlags::default(),
            data: BufferData::FileTree(FileTreeBuffer {
                id: id_b,
                root: std::path::PathBuf::from("/b"),
                entries: Vec::new(),
                content: lattice_core::Buffer::empty(),
                cursor: lattice_protocol::position::Position::ZERO,
                scroll: 0,
                nerd_fonts: false,
            }),
        });
        let sorted = r.sorted_ids();
        assert_eq!(sorted, vec![id_a, id_b, id_c]);
    }

    #[test]
    fn unlisted_buffers_skip_listed_ids() {
        let mut r = BufferRegistry::new();
        let id_a = BufferId::next();
        let id_b = BufferId::next();
        r.insert(BufferEntry {
            id: id_a,
            flags: BufferFlags {
                listed: true,
                hidden: false,
            },
            data: BufferData::FileTree(FileTreeBuffer {
                id: id_a,
                root: std::path::PathBuf::from("/a"),
                entries: Vec::new(),
                content: lattice_core::Buffer::empty(),
                cursor: lattice_protocol::position::Position::ZERO,
                scroll: 0,
                nerd_fonts: false,
            }),
        });
        r.insert(BufferEntry {
            id: id_b,
            flags: BufferFlags {
                listed: false,
                hidden: false,
            },
            data: BufferData::FileTree(FileTreeBuffer {
                id: id_b,
                root: std::path::PathBuf::from("/b"),
                entries: Vec::new(),
                content: lattice_core::Buffer::empty(),
                cursor: lattice_protocol::position::Position::ZERO,
                scroll: 0,
                nerd_fonts: false,
            }),
        });
        let listed = r.listed_ids_sorted();
        assert_eq!(listed, vec![id_a]);
        let all = r.sorted_ids();
        assert_eq!(all, vec![id_a, id_b]);
    }

    #[test]
    fn file_tree_with_root_finds_match() {
        let mut r = BufferRegistry::new();
        let id = BufferId::next();
        let path = std::path::PathBuf::from("/some/root");
        r.insert(BufferEntry {
            id,
            flags: BufferFlags::default(),
            data: BufferData::FileTree(FileTreeBuffer {
                id,
                root: path.clone(),
                entries: Vec::new(),
                content: lattice_core::Buffer::empty(),
                cursor: lattice_protocol::position::Position::ZERO,
                scroll: 0,
                nerd_fonts: false,
            }),
        });
        assert_eq!(r.file_tree_with_root(&path), Some(id));
        assert_eq!(
            r.file_tree_with_root(std::path::Path::new("/different")),
            None
        );
    }
}
