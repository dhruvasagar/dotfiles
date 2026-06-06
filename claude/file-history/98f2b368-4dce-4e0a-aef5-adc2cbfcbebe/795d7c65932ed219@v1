//! The renderer-agnostic editor state.
//!
//! Phase 5.B.3 introduces [`Editor`] as the destination for
//! the per-cluster field migration from
//! `lattice-ui-tui::App`. See
//! [`docs/dev/architecture/phase-5b-app-design.md`] for the
//! Option-D → Option-E pivot that this struct realises:
//!
//! - The host owns the editor's state and logic in `Editor`.
//! - Each renderer crate composes `Editor` into its own
//!   concrete `App` wrapper alongside its renderer-specific
//!   caches (`theme`, `pane_render_registry`, ...).
//!
//! Subsequent slices (5.B.4 onwards) relocate field clusters
//! one at a time from `App` into `Editor`, moving the methods
//! that touch only those fields into `impl Editor` here. Each
//! per-cluster commit ships green: methods that still live in
//! `impl App` access migrated fields via `self.editor.foo`;
//! methods that have moved access them via `self.foo` (now an
//! inherent method on `Editor`).
//!
//! The empty-now/grows-later shape is intentional: it lets
//! the wrapper field `editor: Editor` get added to `App`
//! before any field actually moves, giving every subsequent
//! migration a target that already exists in the type
//! system.

use std::collections::HashMap;
use std::path::PathBuf;

use lattice_grammar::{CommandInvocation, Register};
use lattice_protocol::position::{Position, Range as ProtoRange};

use std::sync::Arc;

use lattice_config::{ConfigRegistry, OptionOverrideSet, ResolvedOptions};
use lattice_core::ui::popup::PopupPlacement;
use lattice_help::topics::HelpTopicRegistry;
use lattice_mode::{ActiveModes, BufferLocals, GuardStoreHandle, ModeRegistry, ServiceRegistry};
use lattice_lsp::cache::{
    CodeActionOutcome, CodeActionRow, CodeLensOutcome, CompletionItemRow, CompletionOutcome,
    CompletionResolveOutcome, DocumentColorOutcome, DocumentHighlightCache,
    DocumentHighlightOutcome, DocumentLinksOutcome, FoldingRangeOutcome, FormatOutcome,
    HoverOutcome, InlayHintOutcome, InsertCompletionLspOutcome, LspCodeLensCache,
    LspDocumentColorCache, LspDocumentLinksCache, LspFoldsCache, LspInlayHintCache, LspNavKind,
    LspPullDiagnosticsCache, LspSelectionChain, LspSemanticTokensCache, PullDiagnosticsOutcome,
    ReferencesOutcome, RenameOutcome, SelectionRangeOutcome, SemanticTokensOutcome,
    SignatureHelpOutcome, SymbolsOutcome,
};
use lattice_lsp::{DiagnosticsLayer, LspLogger, LspSupervisorHandle};
use lattice_protocol::CancellationToken;
use lattice_picker::{Picker, PickerMruIndex, PickerRegistry};
use lattice_grammar::ModalState;
use lattice_grammar::builtins::Builtins;
use lattice_grammar::CommandRegistry;
use lattice_protocol::Event;
use lattice_protocol::edit::EditDelta;
use lattice_runtime::{EventBus, MessagePushed, MessagesRing};
use lattice_syntax::{LangRegistry, StyledSpan, SyntaxHandle};

use crate::action::{Action, EchoMessage};
use crate::actions::ActionIds;
use crate::chord::KeyChord;
use crate::keymap_registry::{KeymapHandle, LayerId};
use crate::buffer_registry::BufferRegistry;
use crate::buffers::BufferId;
use crate::state::{
    LastFind, LastSearch, LastVisual, LivePickerQueryState, MacroRecording, OptionCache,
    PendingBlockInsert, PendingPickerInit, PositionEntry, PrevPaneState, ReplaceEntry, SearchLine,
    SubstitutePreview, TagStackEntry, UnnamedRegister,
};
use lattice_core::BufferKind;
use lattice_protocol::position::Position as ProtoPosition;
use crate::ui::theme::Theme as HostTheme;

/// Renderer-agnostic editor state.
///
/// The renderer-agnostic half of every editor App. Each
/// renderer's `App` struct composes one of these alongside
/// its renderer-specific caches. Host-level code (mode
/// lifecycle, dispatch, picker sources, LSP supervisor, ...)
/// takes `&mut Editor` directly; renderer-side code takes
/// `&mut App` and reaches the editor via `app.editor`.
///
/// **Field set grows per-cluster.** Each 5.B.x slice
/// migrates a logical cluster of fields here from
/// `lattice-ui-tui::App`. As clusters land, this struct
/// accumulates state; in parallel, `App`'s direct field set
/// shrinks. When the migration completes, every renderer-
/// agnostic field on App lives here, every renderer-agnostic
/// method on App lives in this crate's `impl Editor` blocks,
/// and App becomes a thin wrapper holding `editor: Editor`
/// plus renderer-specific caches only.
///
/// **Clusters landed so far:**
/// - 5.B.4 -- macro recording state (`macros`,
///   `macro_recording`, `last_played_macro`).
/// - 5.B.5 -- marks + registers (`marks`, `registers`,
///   `pending_register`, `unnamed_register`).
/// - 5.B.6 -- position history + tag stack
///   (`position_history`, `position_history_cursor`,
///   `recent_files`, `tag_stack`, `pending_tag_origin`).
/// - 5.B.7 -- search state (`search_line`, `last_search`,
///   `current_match`, `all_matches`, `substitute_preview`).
/// - 5.B.8 -- vim repeat + visual state (`pending_count`,
///   `op_count`, `visual_anchor`, `last_change`,
///   `last_visual`, `last_find`).
/// - 5.B.9 -- replace + insert state (`replace_history`,
///   `last_insert`, `recording_insert`,
///   `pending_block_insert`).
/// - 5.B.10 -- popup (subset) (`popup_buffer`,
///   `prev_pane_for_help`, `popup_placement`). Skipped:
///   `popup_back_stack` -- holds `PopupSnapshot` which still
///   lives in `lattice-ui-tui::app::popup`; follow-up slice
///   moves the snapshot type to host before migrating the
///   field.
/// - 5.B.11 -- cmdline + echo (`command_line`,
///   `last_message`, `messages`,
///   `pending_message_event_rx`, `pending_redraw`,
///   `command_history`, `command_history_cursor`,
///   `command_history_pending`, `auto_submit_after_chord`).
/// - 5.B.12 -- syntax (`lang_registry`, `syntax`,
///   `last_parsed_text_version`, `pending_syntax_edits`,
///   `last_synced_syntax_version`, `visible_highlights`,
///   `pane_highlights`). Skipped:
///   `visible_highlights_key` -- its type
///   `VisibleHighlightsKey` lives in
///   `lattice-ui-tui::app::highlights` with `pub(super)`
///   visibility; follow-up slice promotes it to host first.
/// - 5.B.13 -- picker (`picker`, `picker_registry`,
///   `picker_mru`, `picker_mru_path`,
///   `pending_picker_init`, `live_picker_query`,
///   `previewing`). Picker support types (`PendingPickerInit`,
///   `LivePickerQueryState`, `InFlightLiveQuery`,
///   `LIVE_PICKER_DEBOUNCE`) also moved from
///   `lattice-ui-tui::app` to `lattice_host::state`.
/// - 5.B.14 -- config + modes (`config`, `option_cache`,
///   `mode_registry`, `services`, `mode_guards`,
///   `active_modes`, `buffer_locals`, `resolved_options`,
///   `buffer_local_overrides`, `option_change_rx`,
///   `help_topics`, `host_theme`).
/// - 5.B.15 -- modal + dispatch (`modal`, `partial_chord`,
///   `registry`, `event_bus`, `builtins`, `action_ids`,
///   `keymap`, `completion_popup_layer`, `snippet_layer`).
/// - 5.B.16 -- active-pane state (subset) (`cursor`,
///   `scroll`, `should_quit`, `viewport_height`,
///   `terminal_width`, `active_buffer`,
///   `document_buffer_id`, `buffers`). Skipped:
///   `document`, `snapshot_cache`, `pane_tree` --
///   their types have no natural `Default` (actor handles
///   + tree-with-root invariants). Follow-up slice removes
///   `#[derive(Default)]` from `Editor` in favour of an
///   `Editor::new(...)` constructor so these can migrate.
/// - 5.B.17 -- LSP per-buffer caches (`lsp_progress`,
///   `lsp_selection_chain`, `lsp_selection_chain_index`,
///   `lsp_document_highlights`,
///   `last_document_highlight_issue_cursor`,
///   `lsp_folds_cache`, `lsp_inlay_hints_cache`,
///   `lsp_document_links_cache`, `lsp_code_lens_cache`,
///   `lsp_document_color_cache`,
///   `lsp_semantic_tokens_cache`,
///   `lsp_pull_diagnostics_cache`).
/// - 5.B.18 -- all remaining LSP fields: subsystem handles
///   (`lsp`, `lsp_diagnostics`, `lsp_logger`, plus
///   `lsp_log_event_rx`, `lsp_progress_event_rx`,
///   `lsp_config_tree`, `buffer_uris`), server-initiated
///   channels (`pending_apply_edit_rx`,
///   `pending_configuration_rx`, `pending_show_document_rx`,
///   `pending_show_message_request_rx`,
///   `lsp_pending_show_message_requests`,
///   `lsp_show_message_request_queue`,
///   `lsp_next_show_message_request_id`), and all per-
///   feature request channels (the `pending_*_rx` /
///   `pending_*_token` pairs for hover, definition,
///   references, symbols, format, signature-help,
///   completion, moniker, rename, code-action,
///   selection-range, document-highlight, folding-range,
///   document-links, code-lens, document-color, inlay-hint,
///   semantic-tokens, pull-diagnostics, plus the refresh
///   channels and the lifecycle / detach channels).
///   `LspSupervisorHandle` and `DiagnosticsLayer` gained
///   placeholder `Default` impls (dropped-receiver
///   channels; production overwrites in `boot.rs`).
///   `lsp_file_watcher` stays on App for now: its inner
///   type `LspFileWatcher` lives in `lattice-ui-tui::app::
///   lsp_watcher` -- migrates with a follow-up that moves
///   the watcher into a host module.
#[derive(Debug, Default)]
pub struct Editor {
    /// Completed macro recordings keyed by register name.
    /// Replays go through the dispatch layer's `PlayMacro`
    /// action handler. v1 records `Action` streams; insert-
    /// mode keystrokes ARE captured (every Action::Insert is
    /// recorded), but dot-repeat-style replay of insert content
    /// from `c`/`i`/`a` remains a §15 follow-up.
    pub macros: HashMap<char, Vec<Action>>,
    /// In-flight macro recording. `Some` while between
    /// `q<reg>` start and the matching `q` stop; pushed
    /// Actions append to `actions`.
    pub macro_recording: Option<MacroRecording>,
    /// The most recently played macro register, for `@@`
    /// repeat.
    pub last_played_macro: Option<char>,
    /// Unnamed register -- destination of `y` / `d` / `c`,
    /// source of `p` / `P`. `None` until something has been
    /// yanked.
    pub unnamed_register: Option<UnnamedRegister>,
    /// User-set marks. v1 stores them flat by name (a-z,
    /// A-Z, 0-9); uppercase / numbered global marks treat
    /// all marks as buffer-local since the v1 TUI runs
    /// against a single document.
    pub marks: HashMap<char, Position>,
    /// Named registers `"a-z`, `"A-Z`, numbered `"0-"9`,
    /// etc. Stores content + kind. `""` (the unnamed
    /// register) is [`Self::unnamed_register`]; this map
    /// covers everything else.
    pub registers: HashMap<Register, UnnamedRegister>,
    /// Register selected for the next operator / paste
    /// (`"a` prefix). Consumed-and-cleared by `run_invocation`
    /// (operators) and `do_paste` (paste). `None` means use
    /// unnamed.
    pub pending_register: Option<Register>,
    /// Unified position-history ring (DESIGN.md §5.1.1).
    /// Every entry is tagged by source so different keybindings
    /// walk filtered views of the same data (`Ctrl-O` / `Ctrl-I`
    /// walk `AutoJump` + `PluginPush`; `g;` / `g,` walk
    /// `NamedMark`).
    pub position_history: Vec<PositionEntry>,
    /// Cursor into [`Self::position_history`] -- the next entry
    /// the navigation action would visit.
    pub position_history_cursor: usize,
    /// MRU list of canonical paths the user has opened via
    /// `:edit` (or any path flowing through `do_edit`). Newest
    /// first; deduplicated; capped at `MAX_RECENT_FILES`. Source
    /// for the `:recent` picker.
    pub recent_files: Vec<PathBuf>,
    /// Vim-style tag stack (DESIGN.md §5.1.1 follow-up).
    /// Distinct from the jump list: each "drill-down" navigation
    /// (`gd` / `gD` / `gy` / `gI` and their multi-result picker
    /// accept variants) pushes one entry; `<C-t>` pops the most
    /// recent entry. `<C-o>` walks all jumps chronologically;
    /// `<C-t>` pops only the LIFO tag-style drill-downs.
    pub tag_stack: Vec<TagStackEntry>,
    /// Pre-jump origin captured when an LSP nav request fires;
    /// transferred to [`Self::tag_stack`] on the actual jump
    /// (single-result drain or multi-result picker accept).
    /// Cleared on picker dismiss / nav cancellation / drain
    /// with no results.
    pub pending_tag_origin: Option<TagStackEntry>,
    /// In-progress `/` or `?` search. `Some` only while
    /// `modal == ModalState::Search(_)`.
    pub search_line: Option<SearchLine>,
    /// Most recent submitted search; consulted by `n` / `N`.
    pub last_search: Option<LastSearch>,
    /// Range of the most recent search match, used to draw
    /// the primary highlight in the buffer view. Cleared on
    /// Esc and on cursor motion.
    pub current_match: Option<ProtoRange>,
    /// Every occurrence of the most recent search pattern,
    /// used to draw the secondary "hlsearch" overlay.
    /// Cleared on Esc; persists after submit until the next
    /// search.
    pub all_matches: Vec<ProtoRange>,
    /// In-progress substitute preview. Populated as the user
    /// types `:s/pat...`; the renderer overlays match ranges
    /// (and the typed replacement once the second `/` has
    /// been entered) so the user sees the substitution before
    /// pressing Enter. Cleared when the cmdline closes or the
    /// input no longer parses as a substitute (DESIGN.md
    /// §5.9.10).
    pub substitute_preview: Option<SubstitutePreview>,
    /// In-progress count prefix being typed (`3` of `3w`,
    /// `12` of `12dd`). 0 means "no count typed". The next
    /// `Action::Invoke` consumes this and resets it to 0.
    pub pending_count: u32,
    /// Count latched when an operator key was pressed (`2`
    /// of `2d3w`). Multiplied with the motion's count (`3`)
    /// to give the final count the operator dispatches with
    /// (`6`). 0 means "no operator count".
    pub op_count: u32,
    /// Anchor position when Visual mode was entered. `None`
    /// outside Visual; restored on Esc. The `head` of the
    /// selection follows the cursor; the anchor stays put so
    /// the selection extends or contracts as the user moves.
    pub visual_anchor: Option<Position>,
    /// Last operator-class invocation that mutated the
    /// buffer. `.` re-dispatches it from the current cursor.
    /// v1 records operator + motion / operator + range /
    /// Visual-mode operator; insert-mode text replay remains
    /// a §5.2.4 gap.
    pub last_change: Option<CommandInvocation>,
    /// Last Visual-mode selection extents, captured on exit
    /// so `gv` can re-enter Visual with the same anchor /
    /// head / kind.
    pub last_visual: Option<LastVisual>,
    /// Last f/F/t/T find on this buffer, for `;` / `,`.
    pub last_find: Option<LastFind>,
    /// Per-Replace-session log of overwritten bytes so
    /// backspace can restore the original (rather than
    /// deleting). Cleared on entry, pushed on each
    /// `OverwriteChar`, popped on `ReplaceUndoLast`.
    pub replace_history: Vec<ReplaceEntry>,
    /// Text inserted during the most recently completed
    /// Insert session. Captured on Esc out of Insert;
    /// replayed by dot-repeat after the operator part. `None`
    /// if the last change had no insert phase.
    pub last_insert: Option<String>,
    /// In-flight blockwise-visual `I` / `A` session. Captured
    /// at mode-entry time (block extents + per-line insert
    /// column); consumed when Insert exits, at which point
    /// the recorded text is replicated to every line in the
    /// block other than the top row (the top row's insert is
    /// the recording itself). `None` outside a block-visual
    /// insert.
    pub pending_block_insert: Option<PendingBlockInsert>,
    /// Text being captured during the *current* Insert
    /// session. Promoted into [`Self::last_insert`] when
    /// leaving Insert.
    pub recording_insert: Option<String>,
    /// Active popup buffer slot, if a popup overlay is open.
    /// The concrete content lives in the [`crate::buffer_registry::BufferRegistry`]
    /// keyed by this id, flagged
    /// `BufferFlags { listed: false, hidden: true }`.
    pub popup_buffer: Option<BufferId>,
    /// Pane state captured before activating help -- used by
    /// `dismiss_popup` to restore the user to whatever buffer
    /// / cursor / scroll they came from. Set by both display
    /// paths (in-pane activation and popup overlay); cleared
    /// by dismiss.
    pub prev_pane_for_help: Option<PrevPaneState>,
    /// Where the popup overlay sits on screen when one is
    /// open. Lives on the editor (not on the buffer) because
    /// the popup is a generic rectangular surface inside
    /// which any buffer kind renders -- placement is a
    /// property of the popup, not of whatever buffer happens
    /// to be its content.
    pub popup_placement: PopupPlacement,
    /// In-progress text in the `:` minibuffer. Populated
    /// only while `modal == ModalState::Command`.
    pub command_line: String,
    /// Most recent transient status / error message,
    /// displayed in the echo area until replaced.
    pub last_message: Option<EchoMessage>,
    /// Append-only chronological ring of every echo
    /// (`set_message` call). The `:messages` ex-command
    /// opens a `*messages*` buffer rendered from this ring
    /// (the emacs `*Messages*` analogue). Bounded by
    /// `MessagesRing::capacity`. Wrapped in `Arc<Mutex<>>`
    /// so the boot-installed `MessagesLayer` (a
    /// `tracing::Layer` running on whatever thread emitted
    /// the event) can push into the same ring the App reads
    /// on the main thread for backlog seeding.
    pub messages: std::sync::Arc<std::sync::Mutex<MessagesRing>>,
    /// Receiver for [`lattice_runtime::MessagePushed`] events
    /// published by `set_message`. The runtime's per-tick
    /// drain coalesces bursts and rebuilds the `*messages*`
    /// buffer view once per frame.
    pub pending_message_event_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<MessagePushed>>,
    /// Set by `Action::RedrawScreen` (`<C-l>`); the runtime
    /// clears this on its next frame after issuing a full
    /// terminal-clear so any leftover ANSI / stale glyph
    /// state gets repainted from scratch.
    pub pending_redraw: bool,
    /// Submitted `:` command history. Newest at the back.
    /// Bounded.
    pub command_history: Vec<String>,
    /// While in Command modal: index into
    /// [`Self::command_history`] of the entry currently
    /// shown (`None` = the user's in-progress text).
    pub command_history_cursor: Option<usize>,
    /// Snapshot of the user's typed `command_line` on the
    /// first Up so Down can return to it after walking
    /// through history.
    pub command_history_pending: Option<String>,
    /// One-shot "auto-submit on next chord" flag. Set when
    /// the user submitted a Chord-arg-required command with
    /// no value (`:describe-key<CR>`); the cmdline pre-fills
    /// with the command word + space, and the very next
    /// captured chord auto-fires `Action::CommandLineSubmit`
    /// without an explicit `<CR>`. Reset on cancel / submit.
    pub auto_submit_after_chord: bool,
    /// Tree-sitter language registry. Services the document
    /// buffer's `Syntax` and every `HelpBuffer` constructed
    /// by `:describe-*` / `:apropos` / `:keymap` (help
    /// bodies render with markdown highlighting + fenced-
    /// block injections sourced from this same registry).
    pub lang_registry: Arc<LangRegistry>,
    /// Per-document tree-sitter state. `None` when the
    /// document's language is `Plain` (no grammar bundled).
    /// Reparses run on a worker task; reads against the
    /// latest snapshot are wait-free via `ArcSwap`.
    pub syntax: Option<SyntaxHandle>,
    /// `text_version` last sent to the syntax handle's
    /// reparse channel. Used to skip republishing identical
    /// state when no text mutation has happened since the
    /// previous frame.
    pub last_parsed_text_version: u64,
    /// Tree-sitter-shaped edit deltas accumulated since the
    /// last `maybe_reparse_syntax` call. Pushed by
    /// `publish_document_changed` after each
    /// `Buffer::apply_edit`; drained by
    /// `maybe_reparse_syntax` and shipped to the syntax
    /// worker as `Vec<EditDelta>` for incremental reparse.
    pub pending_syntax_edits: Vec<EditDelta>,
    /// `text_version` the syntax worker's tree is known to
    /// be at. Sent as `from_version` on the next reparse
    /// request so the worker can verify edits apply to the
    /// correct tree baseline.
    pub last_synced_syntax_version: u64,
    /// Per-line `StyledSpan`s for the currently visible
    /// viewport, indexed from `[scroll, scroll +
    /// viewport_height)`. Recomputed each frame by
    /// `refresh_highlights` (called from the runtime before
    /// drawing).
    pub visible_highlights: Vec<Vec<StyledSpan>>,
    /// Per-frame snapshot of inactive panes' visible-window
    /// syntax highlights, keyed by pane index. Refreshed by
    /// `refresh_pane_highlights` before each draw so the
    /// renderer can read via `&App`. The active pane uses
    /// the live [`Self::visible_highlights`] field instead.
    pub pane_highlights: std::collections::HashMap<usize, Vec<Vec<StyledSpan>>>,
    /// Active picker overlay. `None` outside picker mode.
    pub picker: Option<Picker>,
    /// Picker source registry -- `:picker` source kinds.
    pub picker_registry: Arc<PickerRegistry>,
    /// Per-source MRU index that biases the picker's initial
    /// candidate ordering toward recently-accepted picks.
    pub picker_mru: PickerMruIndex,
    /// Optional on-disk persistence path for [`Self::picker_mru`].
    /// `None` for ephemeral / test installs.
    pub picker_mru_path: Option<PathBuf>,
    /// In-flight async picker init, if the active picker
    /// source's `init` returned a Future.
    pub pending_picker_init: Option<PendingPickerInit>,
    /// Live-picker query state -- present only when the
    /// active picker source has `spec().live == true`.
    pub live_picker_query: Option<LivePickerQueryState>,
    /// One-tick preview gate: set when the picker should
    /// render its candidate row preview into the auxiliary
    /// region. Cleared after the renderer reads it.
    pub previewing: bool,
    /// Shared typed-options registry (DESIGN.md §5.12).
    /// Every option's *current value* lives in here behind
    /// an `ArcSwap<T>`; `:set` parses against it; the
    /// customize buffer view (post-1.0) reads + writes
    /// through the same surface.
    pub config: Arc<ConfigRegistry>,
    /// Hot-path read cache for the option values.
    /// Repopulated by `rebuild_option_cache` after every
    /// `:set`. Accessor methods on App read this cached
    /// primitive directly (~1ns) instead of going through
    /// the registry's mutex + ArcSwap + downcast (~33ns).
    pub option_cache: OptionCache,
    /// Mode registry (M.1). Owns the catalogue of registered
    /// modes; activation / deactivation routes through here.
    pub mode_registry: Arc<ModeRegistry>,
    /// Typed service map subsystems hand off to modes so
    /// `Mode::on_activate` can pull subsystem handles via
    /// `ctx.service::<T>()`. Populated at boot; read-only
    /// after init.
    pub services: Arc<ServiceRegistry>,
    /// Per-`(buffer, mode)` Guard storage. Modes return an
    /// owned `Mode::Guard` from `on_activate`; the
    /// dispatcher stashes it here keyed by `(BufferId,
    /// ModeId)`. On deactivation the dispatcher drops the
    /// Guard, firing its `Drop` impl for synchronous
    /// cleanup. Wrapped in `Arc<Mutex<>>` because the
    /// spawned lifecycle task inserts from a worker thread.
    pub mode_guards: GuardStoreHandle,
    /// Per-buffer active modes (major + minors).
    pub active_modes: HashMap<BufferId, ActiveModes>,
    /// Per-buffer mode-owned local state. Modes populate
    /// locals via the `BufferLocal` typed-map during
    /// `on_activate`; the App routes `&mut BufferLocals`
    /// into the registry's activation methods.
    pub buffer_locals: HashMap<BufferId, BufferLocals>,
    /// Per-buffer mode-resolved options cache. Refreshed
    /// eagerly on mode toggle and option write.
    pub resolved_options: HashMap<BufferId, ResolvedOptions>,
    /// Buffer-local explicit overrides (`:setlocal foo=bar`)
    /// per buffer. Inputs to resolution; the resolver chains
    /// these with mode contributions before writing
    /// [`Self::resolved_options`].
    pub buffer_local_overrides: HashMap<BufferId, OptionOverrideSet>,
    /// Receiver for `OptionChanged` events published by the
    /// option-cascade pipeline. `Option` only because the
    /// field needs to be `take`-able so the drain method can
    /// borrow `&mut self` for cascade work while iterating
    /// the receiver. Always `Some` between calls.
    pub option_change_rx: Option<tokio::sync::mpsc::UnboundedReceiver<Event>>,
    /// Free-form help topic registry (DESIGN.md §5.11).
    /// `:help` reads from this; built-ins are sourced from
    /// `docs/user/*.md` at build time. Plugins / future LSP
    /// integrations register additional topics through the
    /// same registry.
    pub help_topics: Arc<HelpTopicRegistry>,
    /// Renderer-neutral canonical theme. `:set ui.*` writes
    /// this; the renderer's cached adapter (e.g.
    /// `lattice_ui_tui::App.theme`) is rebuilt from this on
    /// every successful cascade.
    pub host_theme: HostTheme,
    /// Buffer-level modal state machine (DESIGN.md §5.2).
    /// One of Normal / Insert / Visual / Op-pending /
    /// Command / Search / Replace.
    pub modal: ModalState,
    /// In-flight partial-chord stack from the trie. When the
    /// trie returns `LookupResult::Partial`, the dispatch
    /// layer appends the chord here; the next keystroke
    /// runs through the trie with this stack as prefix.
    /// Cleared on every non-`AbsorbPartialChord` action.
    pub partial_chord: Vec<KeyChord>,
    /// Grammar registry shared with the document actor by
    /// `Arc`. The actor calls `lattice_grammar::execute`
    /// with this registry from inside its own task. The App
    /// also reads it directly for the parser, completion
    /// pipeline, and introspection.
    pub registry: Arc<CommandRegistry>,
    /// In-process event bus (DESIGN.md §5.10). The App
    /// publishes editor lifecycle events
    /// (DocumentChanged, SelectionsChanged,
    /// ModalModeChanged, BeforeSave, DocumentSaved,
    /// BeforeQuit, OptionChanged) after observing the
    /// corresponding state transitions.
    pub event_bus: Arc<EventBus>,
    /// Built-in command-ids (`d`, `y`, `w`, `j`, …) -- the
    /// canonical `CommandId` values keymap registrations
    /// resolve against.
    pub builtins: Builtins,
    /// App-side typed action IDs (`CommandKind::Action`
    /// registrations). Each field is a `CommandId`
    /// resolving to an `ActionSpec` whose `apply` returns
    /// `Effect::AppAction(AppEffect::Foo)`.
    pub action_ids: ActionIds,
    /// Layered keymap registry (DESIGN.md §5.2.3).
    /// Populated at construction; the input dispatcher
    /// reads from it on every keystroke. Wait-free reads
    /// via internal `ArcSwap`; concurrent writes (mode
    /// push/pop, plugin registration, `:bind`) never stall
    /// the input path.
    pub keymap: KeymapHandle,
    /// `LayerId` of the active completion-popup minor-mode
    /// layer when the popup is open; `None` otherwise.
    /// Pushed / popped in lockstep with `insert_completion`.
    pub completion_popup_layer: Option<LayerId>,
    /// `LayerId` of the active-snippet minor-mode layer
    /// when a snippet is in flight; `None` otherwise. Same
    /// lockstep pattern as
    /// [`Self::completion_popup_layer`].
    pub snippet_layer: Option<LayerId>,
    /// Active buffer's cursor (DESIGN.md §5.1.1). Updated
    /// in lockstep with the active pane's stash so cross-
    /// pane jumps restore the right position.
    pub cursor: ProtoPosition,
    /// First visible line in the viewport (0-based).
    pub scroll: u32,
    /// Quit flag. The main loop reads this and tears down
    /// after the next paint. Set by `:q` / `:qa` / `Ctrl-C`
    /// / SIGINT.
    pub should_quit: bool,
    /// Last height we were drawn at; used by motion
    /// clamping and viewport scrolling. Updated by the
    /// renderer before each frame.
    pub viewport_height: u32,
    /// Last terminal width we were drawn at. Used by pane
    /// geometry (DESIGN.md §5.9 navigation needs to know
    /// which pane is horizontally adjacent). `None` until
    /// the renderer first records it.
    pub terminal_width: Option<u16>,
    /// Which buffer the input pipeline currently routes to.
    /// When a help overlay is open this is `Help`; otherwise
    /// `Document`. Denormalized from
    /// `pane_tree.active().buffer` -- updated in lockstep
    /// with the active pane.
    pub active_buffer: BufferKind,
    /// Stable id for the *active* document buffer. Mirrors
    /// the active pane's `buffer_id` whenever that pane
    /// holds a Document leaf.
    pub document_buffer_id: BufferId,
    /// Unified buffer registry (DESIGN.md §5.9). Holds
    /// every open buffer regardless of kind -- documents,
    /// file trees, future outline / diagnostics views.
    pub buffers: BufferRegistry,
    /// Accumulated `$/progress` state keyed by
    /// `(server_id, token)`. `Begin` inserts; `Report`
    /// updates; `End` removes. The modeline picks the most
    /// recent active entry to surface.
    pub lsp_progress: HashMap<(Arc<str>, String), lattice_lsp::LspProgressUpdate>,
    /// Cached `textDocument/selectionRange` chain for the
    /// smart-expansion operator.
    pub lsp_selection_chain: Option<LspSelectionChain>,
    /// Current step inside `lsp_selection_chain.ranges`.
    /// 0 = innermost; `chain.ranges.len() - 1` = outermost.
    pub lsp_selection_chain_index: usize,
    /// Cached `textDocument/documentHighlight` for the
    /// active buffer + symbol position.
    pub lsp_document_highlights: Option<DocumentHighlightCache>,
    /// Cursor position at which the most recent
    /// `documentHighlight` request was issued.
    pub last_document_highlight_issue_cursor: Option<ProtoPosition>,
    /// Per-buffer cache of the last
    /// `textDocument/foldingRange` response.
    pub lsp_folds_cache: HashMap<BufferId, LspFoldsCache>,
    /// Per-buffer `inlayHint` cache.
    pub lsp_inlay_hints_cache: HashMap<BufferId, LspInlayHintCache>,
    /// Per-buffer `documentLink` cache.
    pub lsp_document_links_cache: HashMap<BufferId, LspDocumentLinksCache>,
    /// Per-buffer code-lens cache.
    pub lsp_code_lens_cache: HashMap<BufferId, LspCodeLensCache>,
    /// Per-buffer `documentColor` cache.
    pub lsp_document_color_cache: HashMap<BufferId, LspDocumentColorCache>,
    /// Per-buffer semantic-tokens cache.
    pub lsp_semantic_tokens_cache: HashMap<BufferId, LspSemanticTokensCache>,
    /// Per-buffer pull-diagnostics cache (keyed
    /// `result_id`s for `Unchanged` short-circuit).
    pub lsp_pull_diagnostics_cache: HashMap<BufferId, LspPullDiagnosticsCache>,
    // ---- LSP subsystem handles + log/progress channels ----
    pub lsp: LspSupervisorHandle,
    pub lsp_diagnostics: DiagnosticsLayer,
    pub lsp_logger: LspLogger,
    pub lsp_log_event_rx: Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspLogPushed>>,
    pub lsp_progress_event_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspProgressUpdate>>,
    pub lsp_config_tree: toml::Table,
    pub buffer_uris: HashMap<BufferId, lattice_lsp::Uri>,
    // ---- LSP server-initiated channels ----
    pub pending_apply_edit_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::InboundApplyEdit>>,
    pub pending_configuration_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::InboundConfigurationRequest>>,
    pub pending_show_document_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::InboundShowDocument>>,
    pub pending_show_message_request_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::InboundShowMessageRequest>>,
    pub lsp_pending_show_message_requests:
        HashMap<u32, lattice_lsp::InboundShowMessageRequest>,
    pub lsp_show_message_request_queue: std::collections::VecDeque<u32>,
    pub lsp_next_show_message_request_id: u32,
    // ---- LSP per-feature request channels (rx + token pairs) ----
    pub pending_hover_rx: Option<tokio::sync::mpsc::UnboundedReceiver<HoverOutcome>>,
    pub pending_hover_token: Option<CancellationToken>,
    pub pending_definition_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<Vec<lsp_types::Location>>>,
    pub pending_definition_token: Option<CancellationToken>,
    pub pending_nav_kind: Option<LspNavKind>,
    pub pending_references_rx: Option<tokio::sync::mpsc::UnboundedReceiver<ReferencesOutcome>>,
    pub pending_references_token: Option<CancellationToken>,
    pub pending_symbols_rx: Option<tokio::sync::mpsc::UnboundedReceiver<SymbolsOutcome>>,
    pub pending_symbols_token: Option<CancellationToken>,
    pub pending_format_rx: Option<tokio::sync::mpsc::UnboundedReceiver<FormatOutcome>>,
    pub pending_format_token: Option<CancellationToken>,
    pub pending_signature_help_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<SignatureHelpOutcome>>,
    pub pending_signature_help_token: Option<CancellationToken>,
    pub pending_completion_rx: Option<tokio::sync::mpsc::UnboundedReceiver<CompletionOutcome>>,
    pub pending_completion_token: Option<CancellationToken>,
    pub pending_completion_items: Option<Vec<CompletionItemRow>>,
    pub pending_moniker_rx: Option<tokio::sync::mpsc::UnboundedReceiver<String>>,
    pub pending_rename_rx: Option<tokio::sync::mpsc::UnboundedReceiver<RenameOutcome>>,
    pub pending_rename_token: Option<CancellationToken>,
    pub pending_code_action_rx: Option<tokio::sync::mpsc::UnboundedReceiver<CodeActionOutcome>>,
    pub pending_code_action_token: Option<CancellationToken>,
    pub pending_code_action_items: Option<Vec<CodeActionRow>>,
    pub pending_code_action_handle: Option<lattice_lsp::ServerHandle>,
    pub pending_selection_range_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<SelectionRangeOutcome>>,
    pub pending_selection_range_token: Option<CancellationToken>,
    pub pending_document_highlight_token: Option<CancellationToken>,
    pub pending_document_highlight_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<DocumentHighlightOutcome>>,
    pub pending_folding_range_token: Option<CancellationToken>,
    pub pending_folding_range_rx: Option<tokio::sync::mpsc::UnboundedReceiver<FoldingRangeOutcome>>,
    pub pending_document_links_token: Option<CancellationToken>,
    pub pending_document_links_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<DocumentLinksOutcome>>,
    pub pending_code_lens_token: Option<CancellationToken>,
    pub pending_code_lens_rx: Option<tokio::sync::mpsc::UnboundedReceiver<CodeLensOutcome>>,
    pub pending_code_lens_refresh_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspCodeLensRefresh>>,
    pub pending_code_lens_items: Option<Vec<lsp_types::CodeLens>>,
    pub pending_code_lens_server: Option<Arc<str>>,
    pub pending_document_color_token: Option<CancellationToken>,
    pub pending_document_color_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<DocumentColorOutcome>>,
    pub pending_color_presentations: Option<Vec<lsp_types::ColorPresentation>>,
    pub pending_color_range: Option<lsp_types::Range>,
    pub pending_inlay_hint_token: Option<CancellationToken>,
    pub pending_inlay_hint_rx: Option<tokio::sync::mpsc::UnboundedReceiver<InlayHintOutcome>>,
    pub pending_semantic_tokens_token: Option<CancellationToken>,
    pub pending_semantic_tokens_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<SemanticTokensOutcome>>,
    pub pending_pull_diagnostics_token: Option<CancellationToken>,
    pub pending_pull_diagnostics_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<PullDiagnosticsOutcome>>,
    pub pending_diagnostic_refresh_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspDiagnosticRefresh>>,
    pub pending_inlay_hint_refresh_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspInlayHintRefresh>>,
    pub pending_semantic_tokens_refresh_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspSemanticTokensRefresh>>,
    pub pending_lsp_detach_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::events::LspBufferDetached>>,
    pub pending_mode_lifecycle_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_mode::ModeEvent>>,
    pub pending_insert_completion_lsp_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<InsertCompletionLspOutcome>>,
    pub pending_insert_completion_lsp_token: Option<CancellationToken>,
    pub pending_completion_resolve_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<CompletionResolveOutcome>>,
    pub pending_completion_resolve_token: Option<CancellationToken>,
}
