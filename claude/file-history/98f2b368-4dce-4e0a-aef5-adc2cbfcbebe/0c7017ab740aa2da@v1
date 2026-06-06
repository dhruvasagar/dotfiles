//! Small App-helper state types -- pure data, no renderer
//! coupling.
//!
//! Phase 5.2: extracted from `lattice-ui-tui::app` so the
//! eventual App migration carries fewer in-line type
//! definitions. Each struct here is a piece of state App holds
//! in a field (search line in progress, last search, unnamed
//! register, prev-pane snapshot). Renderer-agnostic by
//! construction.

use lattice_core::{BufferId, BufferKind, FoldMethod};
use lattice_grammar::{SearchDirection, VisualKind, YankKind};
use lattice_protocol::CancellationToken;
use lattice_protocol::position::{Position, Range as ProtoRange};

use crate::action::{Action, FindKind};

/// In-progress `/` or `?` state. The cursor at entry is preserved
/// so Esc can restore it.
#[derive(Debug, Clone)]
pub struct SearchLine {
    pub direction: SearchDirection,
    pub pattern: String,
    pub origin: Position,
}

/// Last completed search -- consulted by `n` and `N`.
#[derive(Debug, Clone)]
pub struct LastSearch {
    pub pattern: String,
    pub direction: SearchDirection,
}

/// The unnamed register's payload. v1 uses a single global slot;
/// the full vim register zoo (`"a-z`, `"+`, `"*`, etc.) lands
/// later.
#[derive(Debug, Clone)]
pub struct UnnamedRegister {
    pub content: String,
    pub kind: YankKind,
}

/// Snapshot of the active pane's state captured just before help
/// took it over. Used by `dismiss_popup` to restore the user to
/// the buffer + cursor + scroll they came from. The same struct
/// serves both display modes (in-pane and popup-overlay).
#[derive(Debug, Clone, Copy)]
pub struct PrevPaneState {
    pub buffer: BufferKind,
    pub buffer_id: BufferId,
    pub cursor: Position,
    pub scroll: u32,
}

/// Hot-path option cache. Mirrors the typed-options registry's
/// resolved values for the active buffer; reads on this struct
/// fire on every render tick, so the cache exists to skip a
/// HashMap lookup per option. Repopulated by
/// `App::rebuild_option_cache` after every `:set`.
#[derive(Debug, Clone, Copy)]
pub struct OptionCache {
    pub show_line_numbers: bool,
    pub relative_line_numbers: bool,
    pub wrap_lines: bool,
    pub ignorecase: bool,
    pub tabstop: u32,
    pub foldenable: bool,
    pub foldmethod: FoldMethod,
    pub scrolloff: u32,
    pub completion_auto_insert_single: bool,
    pub show_whitespace: bool,
    pub current_line_highlight: bool,
    pub whitespace_tab: Option<char>,
    pub whitespace_trailing: Option<char>,
    pub whitespace_leading: Option<char>,
    pub whitespace_space: Option<char>,
    pub whitespace_eol: Option<char>,
}

impl Default for OptionCache {
    fn default() -> Self {
        Self {
            show_line_numbers: true,
            relative_line_numbers: false,
            wrap_lines: false,
            ignorecase: false,
            tabstop: 8,
            foldenable: true,
            foldmethod: FoldMethod::Manual,
            scrolloff: 0,
            completion_auto_insert_single: true,
            show_whitespace: false,
            current_line_highlight: false,
            whitespace_tab: Some('→'),
            whitespace_trailing: Some('·'),
            whitespace_leading: Some('·'),
            whitespace_space: None,
            whitespace_eol: None,
        }
    }
}

/// Capture of the most recent find/till for `;`/`,` repeat.
#[derive(Debug, Clone, Copy)]
pub struct LastFind {
    pub kind: FindKind,
    pub target: char,
}

/// In-progress macro recording. `q<reg>` starts; `q` again
/// stops and persists into the register table.
#[derive(Debug, Clone)]
pub struct MacroRecording {
    pub register: char,
    pub actions: Vec<Action>,
}

/// One entry on the vim-style tag stack. Pushed by `gd` (and
/// the goto-* family) at the pre-jump cursor; popped by `<C-t>`
/// to walk back. Distinct from the jump list because the user's
/// mental model for `<C-t>` is "undo the drill-down chain", not
/// "step through every cursor jump in chronological order".
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TagStackEntry {
    pub buffer: BufferKind,
    pub buffer_id: BufferId,
    pub position: Position,
    pub label: String,
}

/// One entry in the unified position history (DESIGN.md §5.1.1).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PositionEntry {
    pub position: Position,
    pub source: PositionSource,
    pub buffer: BufferKind,
    pub buffer_id: BufferId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PositionSource {
    /// Pushed by "big motions" -- gg, G, search, *, #, %, mark jump.
    AutoJump,
    /// Reserved: `g<C-o>` style "I explicitly want to remember here"
    /// pushes (emacs `set-mark`). Not yet wired to a key.
    ExplicitMark,
    /// Reserved: pushed by plugins (LSP go-to-definition, fuzzy-finder
    /// hop, etc.). Treated like AutoJump for navigation.
    PluginPush,
    /// `mX` named mark. Walks via `g;` / `g,`.
    NamedMark(char),
}

impl PositionEntry {
    /// True for entries that the standard Ctrl-O / Ctrl-I jump-list
    /// walks consume.
    pub fn is_jump(&self) -> bool {
        matches!(
            self.source,
            PositionSource::AutoJump | PositionSource::PluginPush
        )
    }

    /// True for entries the `g;` / `g,` mark-history walks consume.
    pub fn is_named_mark(&self) -> bool {
        matches!(self.source, PositionSource::NamedMark(_))
    }
}

/// One replace-mode entry -- the byte that was at `at` before the
/// overwrite, so `<BS>` can restore it. `original = None` means
/// the overwrite extended the line (the position was past EOL);
/// `<BS>` deletes the inserted char rather than restoring a byte.
#[derive(Debug, Clone)]
pub struct ReplaceEntry {
    pub at: Position,
    pub original: Option<String>,
}

/// Most-recently-completed visual selection. Used by `gv` to
/// reselect.
#[derive(Debug, Clone, Copy)]
pub struct LastVisual {
    pub anchor: Position,
    pub head: Position,
    pub kind: VisualKind,
}

/// Snapshot of an in-progress `:s/pat/repl/...` preview.
/// Refreshed on every cmdline keystroke while the input parses as
/// a substitute; consumed by the renderer to overlay match ranges
/// (and the typed replacement, when present) on the target buffer.
#[derive(Debug, Clone)]
pub struct SubstitutePreview {
    /// Match ranges in the target line(s).
    pub matches: Vec<ProtoRange>,
    /// The user-typed replacement template, once the second `/`
    /// has been entered. None while the user is still inside the
    /// pattern field.
    pub replacement: Option<String>,
    /// Whether the user has explicitly typed flags including 'g'.
    pub global: bool,
}

/// In-flight blockwise-visual insert (`I` or `A`).
///
/// When the user enters `I` from blockwise visual, the typed
/// prefix is replicated to every line in the block at the same
/// column on Esc. We capture the rectangle's lines and the
/// per-line insert column at entry time, then replay the
/// recorded text to all lines except the top one (the top row
/// was edited live during the Insert session).
#[derive(Debug, Clone, Copy)]
pub struct PendingBlockInsert {
    pub start_line: u32,
    pub end_line: u32,
    pub insert_col: u32,
    pub live_edits: u32,
}

/// In-flight async picker init. The future from
/// `PickerSourceGenerator::init` is spawned on the LSP
/// runtime; its resolved batch lands here via `rx`. The
/// `cancel` token lets a subsequent `:picker <source>` drop
/// the predecessor before it completes.
pub struct PendingPickerInit {
    pub source_id: String,
    pub generator: std::sync::Arc<dyn lattice_picker::PickerSourceGenerator>,
    pub rx: tokio::sync::mpsc::UnboundedReceiver<
        lattice_picker::SourceResult<lattice_picker::CandidateBatch>,
    >,
    pub cancel: CancellationToken,
}

impl std::fmt::Debug for PendingPickerInit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PendingPickerInit")
            .field("source_id", &self.source_id)
            .finish_non_exhaustive()
    }
}

/// Live-picker query state. Installed when `open_picker`
/// resolves a source whose `spec().live` is true; survives
/// until the picker is dismissed.
///
/// Two phases:
///
/// 1. **Debouncing.** `debounce_until = Some(deadline)` while
///    a keystroke is pending. Every fresh keystroke
///    reschedules the deadline forward. Once `Instant::now()
///    >= deadline`, the main-loop drain fires
///    `PickerSourceGenerator::on_query_changed` and clears
///    `debounce_until` to None.
/// 2. **In-flight.** When `on_query_changed` returns a
///    `Future` or a `Stream`, the spawned task lands its
///    result on `inflight.rx`. The drain seats new raw
///    candidates (if the result is still relevant) or drops
///    the result (if the user has typed past the launched
///    query).
pub struct LivePickerQueryState {
    pub source_id: String,
    pub generator: std::sync::Arc<dyn lattice_picker::PickerSourceGenerator>,
    pub debounce_until: Option<std::time::Instant>,
    pub inflight: Option<InFlightLiveQuery>,
    /// Set by `open_picker` from the first positional arg
    /// when the source is live; consumed (taken) by
    /// `seat_picker_from_pairs` on the first seat so the
    /// picker prompt opens pre-populated with the user's
    /// `:picker grep <pattern>` argument. None for live
    /// pickers opened without an initial pattern.
    pub initial_query: Option<String>,
}

impl std::fmt::Debug for LivePickerQueryState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LivePickerQueryState")
            .field("source_id", &self.source_id)
            .field("debounce_until", &self.debounce_until)
            .field("inflight", &self.inflight)
            .field("initial_query", &self.initial_query)
            .finish_non_exhaustive()
    }
}

/// Spawned `on_query_changed` future / stream paired with
/// the query it was launched against. The drain compares
/// `launched_for_query` against the picker's live query;
/// if they differ, the user has kept typing and a newer fire
/// is already in flight (or coming via debounce) -- discard
/// the stale result.
pub struct InFlightLiveQuery {
    pub cancel: CancellationToken,
    pub rx: tokio::sync::mpsc::UnboundedReceiver<
        lattice_picker::SourceResult<lattice_picker::PickerInitResult>,
    >,
    pub launched_for_query: String,
}

impl std::fmt::Debug for InFlightLiveQuery {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InFlightLiveQuery")
            .field("launched_for_query", &self.launched_for_query)
            .finish_non_exhaustive()
    }
}

/// Debounce window before a live picker's query change
/// fires `on_query_changed`. Telescope uses ~150ms; chosen so
/// that burst keystrokes coalesce into one source call
/// without feeling laggy. Constant lives here (not in
/// `lattice-picker`) because debounce is host policy -- the
/// picker primitive is renderer- and timer-agnostic.
pub const LIVE_PICKER_DEBOUNCE: std::time::Duration =
    std::time::Duration::from_millis(150);
