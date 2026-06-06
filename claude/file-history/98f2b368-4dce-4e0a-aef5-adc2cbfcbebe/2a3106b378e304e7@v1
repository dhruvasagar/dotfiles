//! Pure application state and transitions.
//!
//! The state machine is intentionally separated from the IO loop so it can be
//! unit-tested without spinning up a terminal. Each input keystroke becomes
//! an `Action`; `App::apply` consumes the action, dispatching motion / edit
//! work through `lattice_grammar::execute()` where appropriate.
//!
//! ## Module layout
//!
//! This file holds the `App` struct definition, the cross-feature data
//! types it carries (`Action`, the LSP outcome enums + structs,
//! `OptionCache`, `PositionEntry`, `LspNavKind`, `CompletionState`,
//! `Fold`, `EchoMessage`, `EchoLevel`, `SearchLine`, ...), the
//! cross-module free helpers (`line_byte_len`, `is_word_char_byte`,
//! `word_under_cursor`, etc.), and a `mod tests` block of cross-
//! feature integration tests. Per-feature App methods live in
//! `app/<feature>.rs` submodules -- see `docs/dev/notes/ui-tui-refactor.md`
//! for the full per-module catalog. The R.1.x slice sequence
//! (R.1.0 -- R.1.98) split the App's monolithic impl block apart.
//!
//! ## Where to look for App methods
//!
//! - `app/dispatch.rs` -- `apply` / `apply_effect` /
//!   `apply_app_effect` / `handle_edits` / `dispatch_blocking` /
//!   `run_*_invocation` / `execute_ex_line` / Effect classifiers.
//! - `app/lsp.rs` -- every `lattice-lsp` consumer (requests,
//!   drains, log buffers, hover, navigation, references,
//!   signature help, completion, rename, code action, format,
//!   symbols, trace, on-type formatting, trigger chars,
//!   workspace/applyEdit + workspace/configuration drains).
//! - `app/lifecycle.rs` -- `:e` / `:w` / `:q` / `:bn` / `:ls` /
//!   `<C-l>`, save family, help-buffer adoption, document swap,
//!   buffer-state hooks, pane snapshot, event publishers.
//! - `app/completion.rs` -- popup state machine, ranker, ghost
//!   text, snippet expansion, refilter, `EffectiveCompletionConfig`.
//! - `app/edit.rs` -- actor-bridge mutation wrappers,
//!   yank / paste / register store, Insert+Replace primitives,
//!   `:d`, block-insert.
//! - `app/motions.rs` -- bracket match, history walkers,
//!   mark jump, viewport / scroll, cursor clamp, viewport
//!   sizing, active-buffer accessors.
//! - `app/folds.rs` -- fold compute / open / close / auto-open.
//! - `app/search.rs` -- `/`, `?`, `:s`, `:%s`, find family.
//! - `app/options.rs` -- `:set`, typed options, customize,
//!   per-language overrides, typed-option getters.
//! - `app/cmdline.rs` -- `:` minibuffer + completion.
//! - `app/help.rs` -- `:help`, `:describe-*`, `:apropos`,
//!   `:keymap`, `do_help_follow_link`.
//! - `app/highlights.rs` -- tree-sitter highlight cache +
//!   per-frame refresh + post-edit shift.
//! - `app/visual.rs` -- charwise / linewise / blockwise
//!   selection state, `set_selections_blocking`.
//! - `app/picker.rs` -- picker state machine + candidate
//!   builders.
//! - `app/boot.rs` -- `App::new`, `build_lsp_subsystem`,
//!   `load_persistent_config`, `sync_keymap_overlays`,
//!   `sync_theme_from_config`.
//! - `app/file_tree.rs` -- file-tree buffer ops.
//! - `app/oil.rs` -- oil buffer ops.
//! - `app/macros.rs` -- `q` recording / `@` replay.
//! - `app/mode.rs` -- `modal_label` + `enter_mode` +
//!   `activate_major_for_buffer_kind`.
//! - `app/syntax.rs` -- `maybe_reparse_syntax`.
//! - `app/test_helpers.rs` -- shared test fixtures.

use lattice_core::Buffer;
#[cfg(test)]
use lattice_core::Document;
// SearchDirection + YankKind + ModalState: only referenced
// from `mod tests` after the migrations. Gate to test-only
// (or `pub use`) so the `#![deny(unused_imports)]` lint
// stays clean for production builds while submodule tests
// keep resolving them via `use super::*;`.
#[cfg(test)]
use lattice_grammar::SearchDirection;
#[cfg(test)]
use lattice_grammar::YankKind;
// Re-exported so submodule tests using `use super::*;` keep
// seeing the names after Phase 5.B.* migrations.
pub use lattice_grammar::ModalState;
pub use lattice_grammar::command::CommandInvocation;
use lattice_grammar::register::Register;
use lattice_protocol::position::Position;
#[cfg(test)]
use lattice_protocol::selection::{Selection, SelectionSet};
use lattice_runtime::{DocumentHandle, SnapshotCache};

use crate::buffer_registry::{BufferData, BufferEntry, BufferRegistry, DocumentEntry};
use crate::buffers::{BufferFlags, BufferId, BufferKind};

use crate::pane::PaneTree;
// Re-export PaneDirection so `super::*` in the tests module
// resolves it. The `Action::NavigatePane(PaneDirection)` variant
// now lives in `lattice_host::action::Action`; this re-export
// keeps the test paths compiling.
pub use lattice_grammar::PaneDirection;

// R.1.0 -- app/ submodule skeleton. Each submodule is a
// per-feature destination for the App's methods. R.1.0 only
// creates the empty modules with scoping doc comments;
// subsequent R.1.x slices move method blocks across without
// rethinking the structure. See docs/dev/architecture/keymap-architecture.md
// (or the dedicated R.1 doc) for the full feature -> module
// mapping.
mod boot;
mod cmdline;
mod completion;
mod dispatch;
mod display;
mod edit;
mod file_tree;
mod folds;
mod help;
mod highlights;
mod lifecycle;
mod lsp;
mod lsp_log_buffers;
mod lsp_watcher;
mod macros;
mod messages;
mod mode;
mod motions;
mod oil;
mod operators;
mod options;
pub(crate) mod picker;
mod popup;
mod search;
mod state;
mod syntax;
mod visual;

#[cfg(test)]
pub(crate) mod test_helpers;

// Slice 8.i.4.d: the `Pending` enum and `Action::SetPending`
// variant retired here. All multi-key Normal-mode chord state
// flows through `App::partial_chord` driven by
// `Action::AbsorbPartialChord` (slices 8.i.4.a/b) plus
// `AppEffect::AbsorbOperatorPrefix(_)` for operator prefixes
// (slice 8.i.4.c). `App::apply`'s pending field, the
// `Action::SetPending(_)` arm, and the "non-SetPending clears
// pending" guard are gone.

/// Vim's `H` / `M` / `L` cursor target within the visible
/// viewport. Slice 8.i.2.c hoisted the type into
/// `lattice_grammar::app_effect` so `AppEffect::JumpViewport`
/// can carry it; this is a re-export so existing
/// `crate::app::ViewportPos` callers stay compiling.
pub use lattice_grammar::ViewportPos;

/// Vim's `zz` / `zt` / `zb` post-scroll cursor target. Re-export
/// of the `lattice_grammar` definition (see `ViewportPos` above
/// for rationale).
pub use lattice_grammar::ScrollPos;

// Phase 5.2: `FindKind`, `EchoMessage`, `EchoLevel`, and the
// `Action` enum (below) moved to `lattice_host::action`. The
// re-exports preserve every existing `crate::app::Action` /
// `crate::app::EchoLevel` import in this crate's modules + tests
// + benches; no call-site changes needed.
pub use lattice_host::action::{Action, EchoLevel, EchoMessage, FindKind};

// `MessageRecord` / `MessagesRing` / `MessagePushed` are
// renderer-agnostic, live in [`lattice_runtime`], and re-
// exported below so existing call sites that imported them
// through `crate::app::*` keep working. The wire-typed
// `lattice_grammar::EchoLevel` rides through unchanged --
// `crate::app::EchoLevel` is the display-typed peer kept for
// renderer-side ergonomics; conversion lives at the
// `set_message` seam.
pub use lattice_runtime::{MessagePushed, MessageRecord, MessagesRing};

/// Convert the renderer's display-typed `EchoLevel` to the
/// wire-typed `lattice_grammar::EchoLevel` used by
/// `MessageRecord` (and every other host-side / plugin-side
/// subscriber on the typed bus). The variants are bit-
/// identical; the split between display-typed (ui-tui) and
/// wire-typed (grammar / runtime / plugin host) is the
/// existing convention that kept the renderer surface from
/// leaking into the grammar layer.
/// Convert the renderer's display-typed `EchoLevel` to the
/// wire-typed `lattice_grammar::EchoLevel`. Used by
/// `set_message` for direct-path record construction.
pub(crate) fn echo_level_to_wire(level: EchoLevel) -> lattice_grammar::EchoLevel {
    match level {
        EchoLevel::Trace => lattice_grammar::EchoLevel::Trace,
        EchoLevel::Debug => lattice_grammar::EchoLevel::Debug,
        EchoLevel::Info => lattice_grammar::EchoLevel::Info,
        EchoLevel::Warn => lattice_grammar::EchoLevel::Warn,
        EchoLevel::Error => lattice_grammar::EchoLevel::Error,
    }
}

/// Reverse of [`echo_level_to_wire`]: every renderer-side
/// reader of `MessageRecord` (e.g. the `*messages*` formatter)
/// projects the wire-typed level back to the renderer's
/// display-typed enum. Allowed-dead until the renderer grows
/// a reader site -- the formatter currently matches on the
/// wire-typed enum directly to skip the round-trip.
#[allow(dead_code)]
pub(crate) fn echo_level_from_wire(level: lattice_grammar::EchoLevel) -> EchoLevel {
    match level {
        lattice_grammar::EchoLevel::Trace => EchoLevel::Trace,
        lattice_grammar::EchoLevel::Debug => EchoLevel::Debug,
        lattice_grammar::EchoLevel::Info => EchoLevel::Info,
        lattice_grammar::EchoLevel::Warn => EchoLevel::Warn,
        lattice_grammar::EchoLevel::Error => EchoLevel::Error,
    }
}

/// Convert grammar's wire-typed [`lattice_grammar::EchoLevel`] (carried
/// by `Effect::Echo`) into the App's display-typed `EchoLevel`. Two types
/// because the App's is part of the public crate API; the grammar's is a
/// dispatch detail.
/// Resolve user-typed command text to a `CommandId`, accepting
/// either the canonical registry name (`ex:write`) or an alias
/// (`write`, `w`). Used by App handlers that take a command name
/// from user input -- mirrors the two-stage logic in
/// `excommand::parse_invocation`.
pub(super) fn resolve_command_name_or_alias(
    registry: &lattice_grammar::CommandRegistry,
    name: &str,
) -> Option<lattice_grammar::CommandId> {
    if let Some(id) = registry.id_by_name(name) {
        return Some(id);
    }
    let canonical = crate::excommand::aliases().get(name).copied()?;
    registry.id_by_name(canonical)
}


// Phase 5.2: `SearchLine`, `LastSearch`, `UnnamedRegister`,
// `PrevPaneState` moved to `lattice_host::state`. Re-exported
// below.
pub use lattice_host::state::{LastSearch, PrevPaneState, SearchLine, UnnamedRegister};

// Phase 5.B.13: `PendingPickerInit`, `LivePickerQueryState`,
// `InFlightLiveQuery`, `LIVE_PICKER_DEBOUNCE` moved to
// `lattice_host::state`. Re-exported below so existing
// `crate::app::*` references keep resolving.
pub use lattice_host::state::{
    InFlightLiveQuery, LIVE_PICKER_DEBOUNCE, LivePickerQueryState, PendingPickerInit,
};

// `UnnamedRegister` and `PrevPaneState` moved to
// `lattice_host::state` (re-export above).


// Phase 5.2: LSP cache + outcome types moved to
// `lattice_lsp::cache`. Re-exported here so existing
// `crate::app::HoverOutcome` etc. references continue to
// resolve unchanged across this crate + downstream consumers.
pub use lattice_lsp::cache::{
    CodeActionOutcome, CodeActionRow, CodeLensOutcome, CompletionItemRow, CompletionOutcome,
    CompletionResolveOutcome, DecodedSemanticToken, DocumentColorOutcome,
    DocumentHighlightCache, DocumentHighlightOutcome, DocumentLinksOutcome, FoldingRangeOutcome,
    FormatOutcome, HoverOutcome, InlayHintOutcome, InsertCompletionLspOutcome, LspCodeLensCache,
    LspDocumentColorCache, LspDocumentLinksCache, LspFoldsCache, LspInlayHintCache, LspNavKind,
    LspPullDiagnosticsCache, LspSelectionChain, LspSemanticTokensCache,
    PullDiagnosticsOutcome, ReferencesOutcome, RenameOutcome, SelectionRangeOutcome,
    SelectionRangeStep, SemanticTokensOutcome, SignatureHelpOutcome, SymbolRow, SymbolsOutcome,
    apply_semantic_token_edits, decode_semantic_tokens,
};

// Phase 5.B.3: composition pivot (see
// `docs/dev/architecture/phase-5b-app-design.md`). App is a
// concrete struct -- a thin renderer-specific wrapper around
// the renderer-agnostic `lattice_host::editor::Editor` plus
// the two TUI-shaped caches (`theme`, `pane_render_registry`).
// GPUI's analogue (future `lattice_ui_gpui::App`) takes the
// same shape with its own renderer-specific caches. No
// generics: every `impl App` inside this crate is a plain
// inherent impl, the orphan rule doesn't bite, and per-cluster
// field migrations from App → Editor can land green one at a
// time. The 5.B.2 `App<R: Renderer>` parametrization is
// reverted; the `lattice_host::Renderer` trait + `TuiRenderer`
// marker are retained as Phase 5.6's `lattice-render` work may
// reuse them.
pub struct App {
    /// Composition root for the renderer-agnostic editor state
    /// (Phase 5.B.3 -- see
    /// `docs/dev/architecture/phase-5b-app-design.md`). Empty
    /// at the start of the migration; grows as per-cluster
    /// commits relocate field clusters from `App` into
    /// `Editor`. Host-level call sites that need only
    /// renderer-agnostic state take `&mut Editor` directly;
    /// renderer-side code reaches it via `app.editor`.
    pub editor: lattice_host::editor::Editor,
    /// Handle to the per-document actor (DESIGN.md §5.2.1, §5.7).
    /// The actor owns the writable `Document` (from `lattice-core`);
    /// mutations route through it; reads load a versioned snapshot.
    /// Denormalized from `documents[active_document_id].handle` for
    /// hot-path access.
    pub document: DocumentHandle,
    /// Per-thread cached reader for [`Self::document`]'s published
    /// snapshot cell (DESIGN.md §5.6.8). The renderer's per-frame
    /// `snapshot_cache.load()` returns the current
    /// `Arc<DocumentSnapshot>` in ~300ps in steady state (no edit
    /// since last frame); ~16ns when the actor has just published.
    /// Rebuilt whenever [`Self::document`] is reassigned --
    /// `arc_swap::Cache` caches against a specific cell, so it must
    /// follow the active document's handle.
    pub snapshot_cache: SnapshotCache,
    /// Stable id for the *active* document buffer. Mirrors the
    /// active pane's `buffer_id` whenever that pane holds a
    /// Document leaf. Position-history entries (§5.1.1) and
    /// per-pane state record this id; switching the active
    /// document via `:bnext` / `:e FILE` rotates `Self::document` /
    /// `Self::syntax` etc. to the new active.
    // Phase 5.B.16: `document_buffer_id`, `buffers`,
    // `active_buffer`, `cursor`, `scroll`, `should_quit`,
    // `viewport_height`, `terminal_width` moved to
    // `editor.{document_buffer_id, buffers, active_buffer,
    // cursor, scroll, should_quit, viewport_height,
    // terminal_width}`.
    /// Pane tree (DESIGN.md §5.9). Held on App for now;
    /// migrates to `editor.pane_tree` in a follow-up slice
    /// after `Editor::new` replaces `Default` and
    /// `PaneTree` gets a non-panicking placeholder.
    pub pane_tree: PaneTree,
    // Phase 5.B.15: `modal` moved to `editor.editor.modal`.
    /// In-flight partial-chord stack from the trie (slice 8.i.4).
    /// When the trie returns `LookupResult::Partial`,
    /// `dispatch_normal` / `dispatch_insert` emit
    /// `Action::AbsorbPartialChord(c)` and `App::apply` appends
    /// `c` here. The next keystroke runs through the trie with
    /// this stack as prefix and resolves the full multi-key
    /// chord. Cleared on every non-`AbsorbPartialChord` action.
    /// Operator-prefix pushes (8 prefixes: `d`, `c`, `y`, `>`,
    /// `<`, `gU`, `gu`, `g~`) come from
    /// `AppEffect::AbsorbOperatorPrefix(_)` via
    /// `apply_app_effect`, which also latches `pending_count`
    /// into `op_count` atomically with the prefix push.
    // Phase 5.B.15: `partial_chord` moved to
    // `editor.editor.partial_chord`.
    /// Grammar registry shared with the document actor by `Arc`. The
    /// actor calls `lattice_grammar::execute` with this registry from
    /// inside its own task. The App also reads it directly for the
    /// parser, completion pipeline, and introspection -- all
    /// read-only operations.
    // Phase 5.B.15: `registry` moved to `editor.editor.registry`.
    /// In-process event bus (DESIGN.md §5.10). The App publishes
    /// editor lifecycle events (DocumentChanged, SelectionsChanged,
    /// ModalModeChanged, BeforeSave, DocumentSaved, BeforeQuit,
    /// OptionChanged) after observing the corresponding state
    /// transitions. The App itself subscribes to `OptionChanged`
    /// for the cascade hook (see [`Self::option_change_rx`]);
    /// other subscribers (plugins, autocmds) wire up the same way.
    // Phase 5.B.15: `event_bus` moved to
    // `editor.editor.event_bus`.
    /// Receiver for in-flight LSP hover responses (Phase 4.2.b).
    /// `K` fires a `textDocument/hover` request through the typed
    /// wrapper; the spawned task awaits the actor's response and
    /// pushes a [`HoverOutcome`] onto this channel. The main loop
    /// drains it before each draw via [`Self::drain_pending_hover`]
    /// and either feeds the body into the existing hover popup
    /// via the private `do_open_hover`, or echoes the no-result
    /// reason so the user knows their `K` press was received and
    /// processed (versus silently dropped).
    ///
    /// `Option` only because the field needs to be `take`-able so
    /// the drain method can borrow `&mut self` for the popup
    /// update; always `Some` between calls.
    pub pending_hover_rx: Option<tokio::sync::mpsc::UnboundedReceiver<HoverOutcome>>,
    /// Cancellation token of the most recent hover request. Flipped
    /// when the user re-fires `K`, moves the cursor, or changes
    /// mode -- so a slow server's response arrives marked stale and
    /// is dropped by the typed wrapper's relay. `None` when no
    /// hover is in flight.
    pub pending_hover_token: Option<lattice_protocol::CancellationToken>,
    /// Receiver for in-flight goto-definition responses (Phase
    /// 4.2.c). Shape mirrors [`Self::pending_hover_rx`] -- `gd`
    /// fires every attached server's `textDocument/definition`,
    /// the spawned task collects the merged + deduped location
    /// list, and pushes it onto this channel. Drained per frame
    /// in [`Self::drain_pending_definitions`]; single-result
    /// case jumps in-place, multi-result case echoes a count
    /// (picker buffer lands with 4.2.d).
    pub pending_definition_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<Vec<lsp_types::Location>>>,
    /// Cancellation token of the most recent goto-definition
    /// request. Flipped on a follow-up `gd` so a slow server's
    /// stale response can't drop a popup over a moved cursor.
    pub pending_definition_token: Option<lattice_protocol::CancellationToken>,
    /// Which navigation flavour the in-flight request is for. Used
    /// by [`Self::drain_pending_definitions`] to pick the right
    /// "no X found" echo and the pre-jump history-source tag.
    /// `None` when no nav request is in flight; matches the
    /// nullness of `pending_definition_token`.
    ///
    /// All four nav requests (definition / declaration /
    /// typeDefinition / implementation) share the same
    /// `pending_definition_*` slot because there's never a
    /// reason to have more than one in flight at once -- a
    /// follow-up nav cancels its predecessor.
    pub pending_nav_kind: Option<LspNavKind>,
    /// Receiver for in-flight references responses (Phase 4.2.d).
    /// References gets its own slot (separate from
    /// `pending_definition_*`) because the result handling differs
    /// -- references opens a buffer-backed list view rather than
    /// jumping. The two surfaces could coexist (a hover popup +
    /// a references list), so they don't fight over the slot.
    pub pending_references_rx: Option<tokio::sync::mpsc::UnboundedReceiver<ReferencesOutcome>>,
    /// Cancellation token for the most recent references request.
    /// Flipped on follow-up `gr` so a slow server's response
    /// can't open a list against a moved cursor.
    pub pending_references_token: Option<lattice_protocol::CancellationToken>,
    /// Receiver for in-flight document-symbol responses (Phase
    /// 4.2.e). Same shape as references -- the merged symbol
    /// outline arrives as a `Vec<SymbolRow>` ready to seed the
    /// picker.
    pub pending_symbols_rx: Option<tokio::sync::mpsc::UnboundedReceiver<SymbolsOutcome>>,
    pub pending_symbols_token: Option<lattice_protocol::CancellationToken>,
    /// Receiver for in-flight format responses (Phase 4.3).
    pub pending_format_rx: Option<tokio::sync::mpsc::UnboundedReceiver<FormatOutcome>>,
    pub pending_format_token: Option<lattice_protocol::CancellationToken>,
    /// Receiver for in-flight signature help responses (Phase
    /// 4.3). The drain feeds the markdown body into the same
    /// popup pipeline `K` uses, so the user can dismiss with
    /// `<Esc>` or move the cursor to clear.
    pub pending_signature_help_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<SignatureHelpOutcome>>,
    pub pending_signature_help_token: Option<lattice_protocol::CancellationToken>,
    /// Receiver for in-flight LSP completion responses (Phase
    /// 4.2.g). The accept path stitches the chosen item's
    /// insert_text into the buffer at the captured replace
    /// range.
    pub pending_completion_rx: Option<tokio::sync::mpsc::UnboundedReceiver<CompletionOutcome>>,
    pub pending_completion_token: Option<lattice_protocol::CancellationToken>,
    /// Captured at request fire so the accept path can splice
    /// the chosen item's text into the buffer at the right
    /// position. Cleared on dismiss / outcome consumption.
    pub pending_completion_items: Option<Vec<CompletionItemRow>>,
    /// 4.5.g: receiver for in-flight `:lsp-moniker` responses.
    /// Each request creates a fresh channel; the drain
    /// (`drain_pending_moniker`) reads at most one summary line
    /// per tick and surfaces it as a minibuffer echo.
    pub pending_moniker_rx: Option<tokio::sync::mpsc::UnboundedReceiver<String>>,
    /// Receiver for in-flight `:rename` responses (Phase 4.3).
    /// Drained per-frame; the `Edits` arm fans out across every
    /// affected URI applying TextEdits (one undo unit per file
    /// in v1; cross-file atomic application is queued).
    pub pending_rename_rx: Option<tokio::sync::mpsc::UnboundedReceiver<RenameOutcome>>,
    pub pending_rename_token: Option<lattice_protocol::CancellationToken>,
    /// Receiver for in-flight `:code-actions` responses (Phase
    /// 4.3). Drained per-frame; the Items arm pins items on
    /// the App and opens a picker.
    pub pending_code_action_rx: Option<tokio::sync::mpsc::UnboundedReceiver<CodeActionOutcome>>,
    pub pending_code_action_token: Option<lattice_protocol::CancellationToken>,
    /// Pinned code-action items, indexed by the picker's
    /// candidate `text` (`#<idx>`). Cleared on dismiss /
    /// outcome consumption / accept.
    pub pending_code_action_items: Option<Vec<CodeActionRow>>,
    /// The handle that produced the in-flight code-action
    /// request, kept alive so the resolve / executeCommand
    /// follow-up routes to the same server. v1 picks the first
    /// server that advertises the provider; the handle clone
    /// is cheap (Arc-backed).
    pub pending_code_action_handle: Option<lattice_lsp::ServerHandle>,
    /// Receiver end of the App's own subscription to
    /// `EventKind::OptionChanged` (DESIGN.md §5.10 + §5.12). The
    /// typed-options registry publishes through `event_bus` on
    /// every successful set; this channel queues those events for
    /// [`Self::drain_option_changes`] to consume on the App's main
    /// thread. Decouples cascade timing from the publish path
    /// (publishes can come from any thread -- plugin tasks, future
    /// LSP-driven config writes, the customize buffer) without
    /// risking re-entrancy on the registry mutex or the renderer.
    ///
    /// `Option` only because the field needs to be `take`-able so
    /// the drain method can borrow `&mut self` for cascade work
    /// while iterating the receiver. Always `Some` between calls.
    // Phase 5.B.14: `option_change_rx` moved to
    // `editor.editor.option_change_rx`.
    /// Shared language registry for tree-sitter highlighting. One
    /// `Arc<LangRegistry>` services the document buffer's `Syntax`
    /// AND every `HelpBuffer` constructed by `:describe-*` /
    /// `:apropos` / `:keymap`. Help bodies render with markdown
    /// highlighting (headings, fenced-block injections to the
    /// language tag) sourced from this same registry.
    // Phase 5.B.12: `lang_registry` moved to
    // `editor.editor.lang_registry`.
    // Phase 5.B.15: `builtins` moved to `editor.editor.builtins`.
    /// App-side typed action IDs (`CommandKind::Action`
    /// registrations from `crate::actions::populate`). Each
    /// field is a `CommandId` resolving to an `ActionSpec` whose
    /// `apply` returns `Effect::AppAction(AppEffect::Foo)`. Per-
    /// mode keymap modules consume this alongside `builtins` to
    /// build typed `CommandInvocation`s for chord bindings (slice
    /// 8.i; see `docs/dev/notes/8i-approach.md`).
    // Phase 5.B.15: `action_ids` moved to
    // `editor.editor.action_ids`.
    /// Layered keymap registry (DESIGN.md §5.2.3, audit slice 8.c).
    /// Populated at construction time; the input dispatcher reads
    /// from it on every keystroke. Wait-free reads via the
    /// internal `ArcSwap`; concurrent registration writes (mode
    /// push/pop, plugin registration, `:bind`) never stall the
    /// input path. Slices 8.d / 8.e / 8.f wire Replace, Visual,
    /// and Insert through this; Normal follows in 8.g.
    // Phase 5.B.15: `keymap` moved to `editor.editor.keymap`.
    /// `LayerId` of the active completion-popup minor-mode
    /// layer when the popup is open; `None` otherwise. Pushed /
    /// popped by [`Self::sync_keymap_overlays`] in lockstep with
    /// `self.insert_completion`. Slice 8.f.
    // Phase 5.B.15: `completion_popup_layer` moved to
    // `editor.editor.completion_popup_layer`.
    /// `LayerId` of the active-snippet minor-mode layer when a
    /// snippet is in flight; `None` otherwise. Same lockstep
    /// pattern as [`Self::completion_popup_layer`].
    // Phase 5.B.15: `snippet_layer` moved to
    // `editor.editor.snippet_layer`.
    // Phase 5.B.11: `command_line`, `last_message`,
    // `messages`, `pending_message_event_rx`, `pending_redraw`
    // moved to `editor.{command_line, last_message, messages,
    // pending_message_event_rx, pending_redraw}`.
    /// Per-document tree-sitter state. `None` when the document's language
    /// is `Plain` (no grammar bundled).
    ///
    /// Audit slice 3: this is now an async handle. Reparses run
    /// on a worker task (`tokio::task::spawn_blocking`) so the
    /// UI thread never parses; reads against the latest snapshot
    /// are wait-free via `ArcSwap`. The `Syntax` struct itself
    /// stays accessible for one-shot users (help-buffer
    /// markdown highlighting).
    // Phase 5.B.12: `syntax`, `last_parsed_text_version`,
    // `pending_syntax_edits`, `last_synced_syntax_version`,
    // `visible_highlights` moved to `editor.{syntax,
    // last_parsed_text_version, pending_syntax_edits,
    // last_synced_syntax_version, visible_highlights}`.
    /// Slice B.3: cache key validating the contents of
    /// `visible_highlights`. When `refresh_highlights` finds the
    /// freshly-computed key matches this stored key, the
    /// existing `visible_highlights` is still valid and the
    /// `highlight_lines` call is skipped entirely (~178µs at
    /// 24-line viewport on rust per BENCHMARKS.md →
    /// noise-floor on cache hit).
    ///
    /// Invariant: `Some` ⟹ `visible_highlights` was computed
    /// against this key's snapshot+state. `None` after construction,
    /// after the syntax handle is replaced, and after a state-
    /// change render where the key check passes the recompute
    /// branch (the new key is stored).
    ///
    /// Steady-state hit rate: ~100% (cursor blinking, no edit).
    /// Drops cleanly to 0% during edits/scroll/fold-toggle.
    visible_highlights_key: Option<highlights::VisibleHighlightsKey>,
    // Phase 5.B.7: `search_line`, `last_search`,
    // `current_match`, `all_matches`, `substitute_preview`
    // moved to `editor.search_line` / `editor.last_search`
    // / `editor.current_match` / `editor.all_matches` /
    // `editor.substitute_preview`.
    // Phase 5.B.5: `unnamed_register` moved to
    // `editor.unnamed_register`.
    // Phase 5.B.8: `pending_count`, `op_count`,
    // `visual_anchor`, `last_change`, `last_visual` moved
    // to `editor.{pending_count, op_count, visual_anchor,
    // last_change, last_visual}`.
    // Phase 5.B.5: `marks` moved to `editor.marks`.
    // Phase 5.B.9: `replace_history` moved to
    // `editor.replace_history`.
    // Phase 5.B.5: `registers` and `pending_register` moved
    // to `editor.registers` / `editor.pending_register`.
    /// Unified position-history ring (§5.1.1). Every entry is tagged
    /// by source, so different keybindings can iterate filtered views
    /// of the same data:
    ///
    /// - `Ctrl-O` / `Ctrl-I` (Tab) walk `AutoJump` and `PluginPush`.
    /// - `g;` / `g,` walk `NamedMark`.
    ///
    /// Pushed before "big jumps" (gg, G, search submit, n / N, *, #,
    /// %, mark jumps) with `AutoJump`, plus on every `mX` with
    /// `NamedMark(X)`. The cursor sits at one past the last navigated
    // Phase 5.B.6: `position_history`,
    // `position_history_cursor`, `recent_files`, `tag_stack`,
    // `pending_tag_origin` moved to
    // `editor.position_history` / `editor.position_history_cursor`
    // / `editor.recent_files` / `editor.tag_stack` /
    // `editor.pending_tag_origin`.
    // Phase 5.B.4: `macros`, `macro_recording`,
    // `last_played_macro` moved to `editor.macros` /
    // `editor.macro_recording` / `editor.last_played_macro`.
    // Phase 5.B.8: `last_find` moved to `editor.last_find`.
    /// Manual folds. v1 supports non-nested folds defined by line range.
    /// `closed=true` means the fold's interior is skipped during render.
    pub folds: Vec<Fold>,
    // Phase 5.B.9: `last_insert`, `pending_block_insert`,
    // `recording_insert` moved to `editor.{last_insert,
    // pending_block_insert, recording_insert}`.
    /// Shared typed-options registry (DESIGN.md §5.12). Every
    /// option's *current value* lives in here behind an
    /// `ArcSwap<T>`; `:set` parses against it; the customize
    /// buffer view (post-1.0) reads + writes through the same
    /// surface. Renderer-agnostic options self-register via
    /// the `linkme`-aggregated `OPTION_DECLS` slice; this
    /// renderer's own options register via the `linkme` slice in
    /// `crate::tui_options`.
    // Phase 5.B.14: `config` moved to `editor.editor.config`.
    /// Hot-path read cache for the option values. Populated at
    /// [`Self::new`] time; refreshed inside the
    /// `Event::OptionChanged` cascade so writes through any path
    /// (cmdline, plugins, the future customize buffer) propagate.
    /// Accessor methods on `App` (`foldmethod()` / `tabstop()` /
    /// `show_line_numbers()` / ...) read the cached primitive
    /// directly (~1ns field access) instead of going through the
    /// registry's mutex + ArcSwap + downcast (~33ns). The
    /// renderer hits these accessors per visible line, so the
    /// difference is measurable on the 60-line / 120-line frame
    /// benchmarks. Single source of truth stays in
    /// [`Self::config`]; this struct is a derived projection.
    // Phase 5.B.14: `option_cache` moved to
    // `editor.editor.option_cache`.
    // M.2.0c: TUI-specific options self-register via the
    // linkme slice. No `tui_options` field needed -- callers
    // read directly via `config.get_typed::<UiDimInactive>()`
    // etc. (see `sync_theme_from_config`).
    /// Mode registry (M.1). Owns the catalogue of registered
    /// modes; activation / deactivation routes through here.
    /// One process-shared registry; all Documents share the
    /// same mode definitions.
    // Phase 5.B.14: `mode_registry` moved to
    // `editor.editor.mode_registry`.
    /// Phase 3: typed service map subsystems hand off to modes
    /// so the mode's `Mode::on_activate` can pull subsystem
    /// handles via `ctx.service::<T>()`. Populated at boot (LSP
    /// supervisor handle, buffer-uri resolver). Read-only after
    /// init.
    ///
    /// M-async.1: `Arc<ServiceRegistry>` (not bare
    /// `ServiceRegistry`) because `ModeContext` owns its handles
    /// by Arc clone -- the dispatcher does `services.clone()`
    /// per activation to build the owned ctx.
    // Phase 5.B.14: `services` moved to `editor.editor.services`.
    /// M-async.1/2: per-`(buffer, mode)` Guard storage. Modes
    /// return an owned [`Mode::Guard`](lattice_mode::Mode::Guard)
    /// from `on_activate`; the dispatcher stashes it here keyed
    /// by `(BufferId, ModeId)`. On deactivation the dispatcher
    /// drops the Guard, firing its `Drop` impl for synchronous
    /// cleanup (unsubscribe, restore prior option, drop
    /// supervisor handle).
    ///
    /// M-async.2: wrapped in [`GuardStoreHandle`] (Arc<Mutex<>>)
    /// because the spawned lifecycle task locks + inserts on
    /// `on_activate` resolve from a tokio worker thread; the App
    /// thread locks + removes on deactivate.
    // Phase 5.B.14: `mode_guards` moved to
    // `editor.editor.mode_guards`.
    /// Mode-keyed pane render dispatch (M.4 follow-up). Populated
    /// at boot; lookup walks active minors then the major to find
    /// the per-buffer renderer + status formatter, with the
    /// document path as the fallback when no provider matches.
    /// Replaces the helper-side `match buffer.kind` in
    /// `draw_pane_content` and `pane_status_label`.
    pub pane_render_registry: crate::pane_render::PaneRenderRegistry,
    /// Per-buffer active modes (major + minors). M.1 wired the
    /// field on `Document` for the document buffer, but
    /// `Document` lives behind the actor's snapshot-cache, so
    /// for M.2.1 the App layer maintains a parallel
    /// per-buffer map keyed by `buffers::BufferId` -- this is
    /// the version `recompute_options_for_buffer` reads to
    /// pull mode contributions. `Document.modes` and this map
    /// converge in M.4 when `ActiveModes` joins
    /// `DocumentSnapshot`.
    // Phase 5.B.14: `active_modes` moved to
    // `editor.editor.active_modes`.
    /// Per-buffer mode-owned local state (M.3.2.a). Modes
    /// populate locals via the `BufferLocal` typed-map during
    /// `on_activate`; the App routes
    /// `&mut BufferLocals` into the registry's activation
    /// methods. M.3.2.b/c migrates existing per-variant data
    /// (`SyntaxHandle`, `Vec<Fold>`, etc.) into locals owned
    /// by their respective modes; until then this map exists
    /// to thread through the new activation API and to back
    /// `:describe-buffer`'s inspection (no entries until
    /// M.3.2.b).
    // Phase 5.B.14: `buffer_locals` moved to
    // `editor.editor.buffer_locals`.
    /// Per-buffer mode-resolved options cache (M.2.1, see
    /// `mode-architecture.md` §6.3 / §9.4 — note: the doc shows
    /// this on `Document`, but lattice-core cannot depend on
    /// lattice-config without a dep cycle, so the cache lives
    /// at the App layer keyed by `buffers::BufferId` (the App's
    /// per-buffer key, not the lower-level
    /// `lattice_protocol::BufferId`). Refreshed eagerly on mode
    /// toggle and option write per §6.3.1. Reads via type-keyed
    /// access against the cached snapshot are O(1).
    // Phase 5.B.14: `resolved_options` moved to
    // `editor.editor.resolved_options`.
    /// Buffer-local explicit overrides (`:setlocal foo=bar`)
    /// per buffer. Inputs to resolution; the resolver chains
    /// these with mode contributions before writing
    /// [`Self::resolved_options`]. Empty for buffers the user
    /// has never run `:setlocal` against.
    // Phase 5.B.14: `buffer_local_overrides` moved to
    // `editor.editor.buffer_local_overrides`.
    /// Free-form help topic registry (DESIGN.md §5.11). `:help`
    /// reads from this; built-ins are sourced from `docs/user/*.md`
    /// at build time. Plugins / future LSP integrations register
    /// additional topics through the same registry.
    // Phase 5.B.14: `help_topics` moved to
    // `editor.editor.help_topics`.
    /// UI styling knobs (DESIGN.md §5.6). Carries per-pane status
    /// line colors, the inactive-pane dim overlay, separator
    /// characters, etc. Customizable via `:set ui.*` options.
    ///
    /// Phase 5.3: the canonical state is [`Self::host_theme`]
    /// (renderer-neutral). This field is the cached ratatui-typed
    /// adapter that the TUI renderer reads on the hot path; it's
    /// rebuilt from `host_theme` on every successful
    /// `sync_theme_from_config`. The duplication is transitional;
    /// when GPUI lands and the TUI cache moves off `App`,
    /// `App.theme` collapses into `host_theme` (renamed) and each
    /// renderer maintains its own cached view.
    pub theme: crate::theme::Theme,
    /// Phase 5.3: renderer-neutral canonical theme. `:set ui.*`
    /// writes this; the cached TUI adapter [`Self::theme`] is
    /// rebuilt from it. Future renderers (GPUI) read from this
    /// field and maintain their own cached view -- the host owns
    /// the canonical neutral state.
    // Phase 5.B.14: `host_theme` moved to
    // `editor.editor.host_theme`.
    /// Per-frame snapshot of inactive panes' visible-window syntax
    /// highlights, keyed by pane index. Refreshed by
    /// [`Self::refresh_pane_highlights`] before each draw so the
    /// renderer can read via `&App`. The active pane uses the live
    /// [`Self::visible_highlights`] field instead.
    // Phase 5.B.12: `pane_highlights` moved to
    // `editor.editor.pane_highlights`.
    // Phase 5.B.11: `command_history`,
    // `command_history_cursor`, `command_history_pending`
    // moved to `editor.{command_history,
    // command_history_cursor, command_history_pending}`.
    /// Active popup overlay's buffer id (DESIGN.md §5.11; M.4).
    /// `Some(id)` while a `:describe-*` / `:apropos` / hover / etc.
    /// popup is open; the actual buffer lives in
    /// [`Self::buffers`] (the unified registry) with
    /// `BufferFlags { listed: false, hidden: true }`. Resolve the
    /// concrete handle through [`Self::popup_help`] /
    /// [`Self::popup_help_mut`] -- the slot itself is just a
    /// reference into the registry. Display strategy is governed
    /// by [`lattice_core::ui::display::BufferDisplayCategory`];
    /// callers route through the private `display_buffer` helper.
    // Phase 5.B.10: `popup_buffer` moved to
    // `editor.popup_buffer`.
    /// LIFO stack of snapshots taken every time the popup's content
    /// gets swapped in place by a help → help link follow (e.g.
    /// `:describe-buffer` → click `[text-mode](mode:text-mode)` →
    /// `:describe-mode text-mode`). One popup buffer is reused
    /// across the navigation so jump-list / marks / search /
    /// register state stay coherent; this stack records what was
    /// in the buffer before each swap so `<C-o>` from inside the
    /// popup can restore the prior frame without leaving Help.
    /// Drained on `dismiss_popup`; reset whenever a fresh top-
    /// level popup opens from outside Help.
    pub popup_back_stack: Vec<crate::app::popup::PopupSnapshot>,
    /// Pane state captured before activating help -- used by
    /// `dismiss_popup` to restore the user to whatever buffer +
    /// cursor + scroll they came from. Set by both display
    /// paths (in-pane via `activate_help_in_pane`, popup via
    /// `open_help_popup_overlay`); cleared by dismiss. v1 single-
    /// pane scope -- multi-pane help dismissal will key by pane
    /// id when that scenario surfaces.
    // Phase 5.B.10: `prev_pane_for_help` moved to
    // `editor.prev_pane_for_help`.
    /// Where the popup overlay sits on screen when one is open.
    /// Carried on App (not on the buffer) because the popup is a
    /// generic rectangular surface inside which any buffer kind
    /// renders -- placement is a property of the popup, not of
    /// whatever buffer happens to be its content. Cursor-anchored
    /// popups (hover / signature help) set this when they open;
    /// the default (centred) covers `:lsp-status`, `:describe-*`,
    /// `:apropos`, `:help`, `:keymap`, `:options`, `:ls`, ...
    // Phase 5.B.10: `popup_placement` moved to
    // `editor.popup_placement`.

    /// Pluggable completion pipeline (DESIGN.md §5.11.3). Owned by
    /// the App at v1 -- promotes to a sibling crate when plugins
    /// need cross-buffer access.
    pub completion_registry: lattice_completion::CompletionRegistry,
    /// Active completion popup. `Some` while the user has Tab-
    /// triggered completion in the `:` line.
    pub completion_state: Option<CompletionState>,
    /// Active **Insert-mode** completion popup (Phase 4.2.g).
    /// Distinct from `completion_state` (which drives the `:` line
    /// completion popup): this one floats over the buffer, shows
    /// candidates from sources (LSP / snippets / buffer-words /
    /// path / tree-sitter / plugin), and the host's keystroke
    /// dispatcher routes through a "completion-popup minor mode"
    /// keymap layer while it's `Some`. Behavioural spec lives in
    /// [`docs/dev/architecture/insert-completion.md`](../../docs/dev/architecture/insert-completion.md).
    pub insert_completion: Option<lattice_completion::InsertCompletionState>,
    /// Receiver for in-flight LSP insert-completion responses.
    /// Drained per-frame; the drain merges new items into
    /// `insert_completion.raw` and refilters.
    pub pending_insert_completion_lsp_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<InsertCompletionLspOutcome>>,
    /// Cancellation token for the most recent LSP insert-
    /// completion request. Flipped on every re-trigger
    /// (isIncomplete refresh, manual `<C-Space>` re-fire) so a
    /// slow server's stale response can't pollute the popup.
    pub pending_insert_completion_lsp_token: Option<lattice_protocol::CancellationToken>,
    /// Receiver for in-flight `completionItem/resolve` results
    /// (Phase 4.2.g.3). Populates the focused candidate's
    /// LspCompletionMeta `documentation` field + the docs
    /// popup body. Cancelled when selection changes / popup
    /// closes / fresher resolve fires.
    pub pending_completion_resolve_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<CompletionResolveOutcome>>,
    pub pending_completion_resolve_token: Option<lattice_protocol::CancellationToken>,
    /// Per-language snippet registry (Phase 4.2.g.4). Loaded
    /// at startup from bundled / user / project paths via
    /// `lattice-snippet::load`; the `gen:snippet` source
    /// consults it per-popup-trigger.
    /// CSM.5: held as `Arc<ArcSwap<...>>` so the mode-captured
    /// handle stays valid across `:reload-snippets`. Source reads
    /// load the current snapshot via `.load()` (wait-free); the
    /// reload path swaps the inner via `.store()` so the mode's
    /// next produce sees the fresh data.
    pub snippet_registry: std::sync::Arc<arc_swap::ArcSwap<lattice_snippet::SnippetRegistry>>,
    /// Sidecar metadata for snippet candidates in the active
    /// insert-completion popup. Indexed by the candidate's
    /// `CandidateData::Extension { payload }` (u32 LE) --
    /// same shape as `insert_completion_lsp_meta` for the
    /// LSP source.
    /// CSM.5: retired. Snippet candidates now carry their stable
    /// `name` in the `Extension::payload` field; the accept path
    /// re-resolves the body via `App.snippet_registry.by_name`.
    /// Field kept as an empty Vec for one slice so callers that
    /// haven't migrated still compile; field deletion in a
    /// follow-up cleanup slice.
    pub insert_completion_snippet_meta: Vec<SnippetCandidateMeta>,
    /// Per-session accept-count map for the insert-mode
    /// completion popup (Phase 4.2.g.5). Each accepted candidate
    /// bumps the counter for its `(text, kind)` pair; the ranker
    /// reads this map and adds a bounded bonus
    /// (`InsertRanker::FREQUENCY_BONUS_CAP`) so recently-accepted
    /// items bubble above tied peers next time. In-memory only
    /// in v1 -- cleared on process exit; persistence with a
    /// privacy story lands later (Phase 4.2.g.7 polish queue per
    /// `docs/dev/architecture/insert-completion.md` §11).
    pub completion_accept_freq:
        std::collections::HashMap<(String, lattice_completion::CandidateKind), u32>,
    /// TOML structural sections collected by the config loader at
    /// startup but not yet routed to their owners. Keyed by full
    /// dotted path (e.g. `"completion.per-language.markdown"`,
    /// `"plugin.rust-analyzer"`); value is the sub-table verbatim.
    /// Phase 4.2.g.5 (3b/3) drains the `completion.per-language.*`
    /// entries into `per_language_completion`; the plugin host
    /// (Phase 7) will drain `plugin.*`. v1's bucket means a user
    /// who writes `[plugin.X]` optimistically doesn't lose the
    /// content -- it's preserved here for the plugin host to pick
    /// up when it registers.
    pub pending_config_structural_sections: std::collections::BTreeMap<String, toml::Table>,
    /// Per-language insert-completion overrides (Phase 4.2.g.5
    /// (3b/3); spec at `docs/dev/architecture/insert-completion.md` §9). Seeded
    /// at App init from
    /// [`lattice_completion::per_language_defaults`] (markdown
    /// drops LSP, rust enables auto-fire, etc.); user TOML in
    /// `[completion.per-language.<lang>]` sections layers on top
    /// via [`Self::apply_per_language_toml_overrides`]. Read by
    /// the private `effective_completion_for` which walks
    /// per-language -> global option -> hardcoded fallback for
    /// each effective field.
    pub per_language_completion:
        std::collections::HashMap<String, lattice_completion::PerLanguageOverrides>,
    /// `true` while the active insert-completion popup is in
    /// path-completion mode (Phase 4.2.g.6 (2/2)). Set at
    /// popup-trigger time when the cursor sits inside a string
    /// literal AND `gen:path` is enabled for the active
    /// language; cleared on popup dismiss / accept. Drives
    /// path-aware anchor resolution in `do_completion_trigger`
    /// and source-set selection in
    /// `populate_insert_completion_sync` (path source only;
    /// other sync sources skip).
    pub completion_in_path_context: bool,
    /// Live snippet expansion. `Some` while a snippet is
    /// active and `<Tab>` / `<S-Tab>` navigate placeholders.
    /// Dropped on `$0` consumption / `<Esc>` / cursor moving
    /// outside the snippet's tabstop ranges.
    pub active_snippet: Option<lattice_snippet::ActiveSnippet>,
    /// Per-language directories from which snippet packs are
    /// loaded on startup / `:reload-snippets` (Phase 4.2.g.4).
    /// Each entry is a directory of `*.json` files in
    /// friendly-snippets format; the file's stem is the
    /// language id (e.g. `rust.json` -> language `"rust"`,
    /// `_global.json` -> the all-language `*` slot). Tests
    /// seed this with a tempdir; production reads from
    /// `~/.editor.config/lattice/snippets/` (wired in startup -- see
    /// `App::default_snippet_dirs`).
    pub snippet_dirs: Vec<std::path::PathBuf>,
    /// Active vertico-style picker (DESIGN.md §5.9.7, §5.9.10).
    /// `Some` while a picker is open over a buffer / LSP instance
    /// / future generator. Input routes here in
    /// [`crate::input::translate`] before falling through to the
    /// modal handlers; render takes precedence over completion +
    /// hover popups.
    // Phase 5.B.13: `picker` moved to `editor.editor.picker`.
    /// Metadata registry of every picker source the
    /// `:picker <source>` ex-command can dispatch to. Populated
    /// at boot with first-party source specs; feature crates
    /// (LSP, snippets) add their sources through dedicated
    /// `register_picker_sources` entry points. The plugin host
    /// will feed WIT-imported sources into the same registry
    /// (Phase 7+). See `docs/dev/architecture/picker.md`.
    ///
    /// Held as `Arc<...>` so the `gen:picker-sources` completion
    /// generator can capture a `Weak<PickerRegistry>` and walk
    /// the source list on every keystroke without keeping the
    /// registry alive past the App. Mirror of the
    /// `Arc<ModeRegistry>` pattern used by `gen:modes`.
    // Phase 5.B.13: `picker_registry` moved to
    // `editor.editor.picker_registry`.
    /// Picker MRU index. Loaded from
    /// `$XDG_CACHE_HOME/lattice/picker-mru.bincode` at boot
    /// (best-effort -- corruption / version-mismatch silently
    /// discards and starts fresh per `docs/dev/architecture/picker.md`
    /// § 9.3). `open_picker` reads via `frecency_bonus` on
    /// each candidate; `do_picker_accept` records on the
    /// chosen row and best-effort persists. Single-threaded
    /// host today; if plugin sources ever need shared access
    /// this becomes `Arc<RwLock<...>>` -- the trait surface
    /// already supports that move.
    // Phase 5.B.13: `picker_mru` moved to
    // `editor.editor.picker_mru`.
    /// Resolved persistence path for the MRU cache.
    /// `Some(path)` when `dirs::cache_dir()` returned a usable
    /// path at boot; `None` disables persistence (in-memory
    /// only -- sandboxed runs, headless test fixtures).
    // Phase 5.B.13: `picker_mru_path` moved to
    // `editor.editor.picker_mru_path`.
    /// `Some` while an async picker source's `init` future is
    /// in-flight. Holds the spawned-task channel handle + the
    /// generator + the source id; the main loop's
    /// `drain_pending_picker_init` pumps the channel and seats
    /// the picker once results arrive.
    ///
    /// A second `:picker <source>` invocation while one is
    /// pending cancels the predecessor (`cancel.cancel()`) and
    /// replaces this slot -- vim-style "do what I last said."
    // Phase 5.B.13: `pending_picker_init` moved to
    // `editor.editor.pending_picker_init`.
    /// `Some` while a live picker is open
    /// ([`PickerSourceSpec::live`] == true). Holds the
    /// generator + debounce deadline + any in-flight
    /// `on_query_changed` task. The main loop's
    /// `drain_pending_live_picker_query` fires the source's
    /// re-fetch when the debounce expires and seats the new
    /// batch when the future resolves.
    // Phase 5.B.13: `live_picker_query` moved to
    // `editor.editor.live_picker_query`.
    /// True while a buffer activation is in *preview* mode --
    /// driven by the picker's `select_next` / `select_prev`
    /// hooks. Activate paths gate position-history pushes on
    /// this flag so a hover-preview doesn't pollute the jump
    /// list. Cleared at the end of every preview tick.
    // Phase 5.B.13: `previewing` moved to
    // `editor.editor.previewing`.
    /// Receiver for [`lattice_lsp::LspLogPushed`] events (Phase
    /// 4; M.5.3.b moved the event type from `lattice-protocol`'s
    /// central enum to `lattice-lsp::events`). Drained once per
    /// main-loop tick by [`Self::drain_lsp_log_events`];
    /// matching log buffers in `BufferRegistry` are rebuilt from
    /// the logger snapshot so `*lsp*` / `*lsp:<server>*` /
    /// `*lsp:<server>:trace*` views update live without the
    /// user having to reopen them.
    // 5.B.18b: `lsp_log_event_rx`, `lsp_progress_event_rx`
    // moved to `editor.{lsp_log_event_rx,
    // lsp_progress_event_rx}`.
    /// Receiver for [`lattice_lsp::LspBufferDetached`] events
    /// published by `LspMode::on_deactivate`. Drained once per
    /// main-loop tick by [`Self::drain_lsp_detach_events`]; for
    /// each event the App calls [`Self::lsp_close_buffer`]
    /// (which sends `textDocument/didClose` per attached server
    /// and clears `buffer_uris`). The subscriber pattern keeps
    /// the wire-level close off the mode-activation path:
    /// `deactivate_mode_by_id` no longer special-cases
    /// `lsp-mode`; any future renderer hosting the App gets the
    /// same behaviour for free.
    pub pending_lsp_detach_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspBufferDetached>>,
    /// M-async.3: receiver for [`lattice_mode::ModeEvent`].
    /// The dispatcher publishes lifecycle events through the
    /// typed bus; the App's per-tick drain processes
    /// `ModeActivationFailed` for rollback (clearing
    /// `active_modes` + dropping any leaked Guards for failed
    /// modes). Other variants (`MajorEntered` etc.) are
    /// observable here too -- subscribers that need them
    /// (modeline refresh, telemetry) hook in by sharing this
    /// receiver via a fan-out later; for now the drain only
    /// acts on the failure variant.
    pub pending_mode_lifecycle_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_mode::ModeEvent>>,
    /// 4.4.g: receiver for `LspInlayHintRefresh` events. The
    /// actor publishes these when a server sends
    /// `workspace/inlayHint/refresh`; the App's per-tick drain
    /// clears `lsp_inlay_hints_cache` for buffers attached to
    /// the requesting server. The next render tick re-issues
    /// `inlayHint` and refills.
    pub pending_inlay_hint_refresh_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspInlayHintRefresh>>,
    /// 4.4.i: receiver for `LspSemanticTokensRefresh` events.
    /// Mirrors `pending_inlay_hint_refresh_rx`: actor publishes
    /// on `workspace/semanticTokens/refresh`; the App's per-tick
    /// drain drops `lsp_semantic_tokens_cache` entries (including
    /// the cached `result_id`) for buffers attached to the
    /// requesting server, forcing the next render tick to issue
    /// a fresh `semanticTokens/full` baseline rather than a
    /// delta against a now-stale id.
    pub pending_semantic_tokens_refresh_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspSemanticTokensRefresh>>,
    /// Accumulated `$/progress` state keyed by
    /// (server_id, token). `Begin` inserts; `Report` updates;
    /// `End` removes. The modeline picks the most recent
    /// active entry to surface.
    // Phase 5.B.17: `lsp_progress` moved to
    // `editor.lsp_progress`.
    /// 4.4.e: cached `textDocument/selectionRange` chain for
    /// the smart-expansion operator. Index 0 is the innermost
    /// range (closest to the cursor); each subsequent entry is
    /// one `parent` step outward. `cursor` and `buffer` are the
    /// anchor that captured this chain; we invalidate the cache
    /// when the cursor moves outside the innermost range or
    /// the active buffer changes.
    // Phase 5.B.17: `lsp_selection_chain` moved to
    // `editor.lsp_selection_chain`.
    /// 4.4.e: current step inside `lsp_selection_chain.ranges`.
    /// `0` = innermost; `chain.ranges.len() - 1` = outermost.
    // Phase 5.B.17: `lsp_selection_chain_index` moved to
    // `editor.lsp_selection_chain_index`.
    /// 4.4.e: receiver for the in-flight `selectionRange`
    /// response. Drained per frame.
    pub pending_selection_range_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<SelectionRangeOutcome>>,
    /// 4.4.e: cancellation token for the in-flight
    /// `selectionRange` request. Flipped when the user invokes
    /// `:lsp-expand-region` again (or moves the cursor) before
    /// the prior response lands; the response is then dropped.
    pub pending_selection_range_token: Option<lattice_protocol::CancellationToken>,
    /// 4.4.e: cached `textDocument/documentHighlight` for the
    /// active buffer + symbol position. Refreshed by the
    /// per-tick pump when the cursor moves to a different
    /// (line, byte). Renderer reads this to paint the soft
    /// overlay across same-symbol occurrences.
    // Phase 5.B.17: `lsp_document_highlights` moved to
    // `editor.lsp_document_highlights`.
    /// 4.4.e: cursor position at which the most recent
    /// `documentHighlight` request was issued. Used by the
    /// pump to decide whether to re-issue (cursor moved) vs.
    /// reuse the cache. Distinct from `cache.cursor` because
    /// the in-flight request may not have landed yet.
    // Phase 5.B.17: `last_document_highlight_issue_cursor`
    // moved to
    // `editor.last_document_highlight_issue_cursor`.
    /// 4.4.e: cancellation token + receiver for the in-flight
    /// `documentHighlight` request.
    pub pending_document_highlight_token: Option<lattice_protocol::CancellationToken>,
    pub pending_document_highlight_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<DocumentHighlightOutcome>>,
    /// 4.4.f: per-buffer cache of the last `textDocument/foldingRange`
    /// response. Keyed by `BufferId` because the foldmethod is a
    /// per-buffer setting; multiple open buffers can each track
    /// their own LSP fold list. Invalidated by the pump when
    /// the document version bumps.
    // Phase 5.B.17: `lsp_folds_cache` moved to
    // `editor.lsp_folds_cache`.
    /// 4.4.f: cancellation token + receiver for the in-flight
    /// `foldingRange` request. Single-flight: a new request
    /// cancels any predecessor.
    pub pending_folding_range_token: Option<lattice_protocol::CancellationToken>,
    pub pending_folding_range_rx: Option<tokio::sync::mpsc::UnboundedReceiver<FoldingRangeOutcome>>,
    /// 4.4.g: per-buffer `inlayHint` cache. Refilled by the
    /// per-tick pump when the document version changes; the
    /// renderer overlay splices each hint as virtual text.
    // Phase 5.B.17: `lsp_inlay_hints_cache` moved to
    // `editor.lsp_inlay_hints_cache`.
    /// 4.5.c: per-buffer `documentLink` cache. Refilled by
    /// the per-tick pump on document-version change; consumed
    /// by `gx` (Normal-mode keystroke) -- the first link whose
    /// range covers the cursor wins. The renderer overlay
    /// (underline link ranges) is queued; today the cache only
    /// drives navigation, not visuals.
    // Phase 5.B.17: `lsp_document_links_cache` moved to
    // `editor.lsp_document_links_cache`.
    /// 4.5.c: in-flight `documentLink` single-flight slot.
    pub pending_document_links_token: Option<lattice_protocol::CancellationToken>,
    pub pending_document_links_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<DocumentLinksOutcome>>,
    /// 4.5.d: per-buffer code-lens cache. Refilled by the
    /// per-tick pump on document-version change; consumed by
    /// `:lsp-code-lens` (opens a picker over the cached
    /// lenses). Cleared via `workspace/codeLens/refresh` so
    /// servers that recompute lenses out-of-band (test runs,
    /// debug session start) can force a refetch.
    // Phase 5.B.17: `lsp_code_lens_cache` moved to
    // `editor.lsp_code_lens_cache`.
    /// 4.5.d: in-flight `codeLens` single-flight slot.
    pub pending_code_lens_token: Option<lattice_protocol::CancellationToken>,
    pub pending_code_lens_rx: Option<tokio::sync::mpsc::UnboundedReceiver<CodeLensOutcome>>,
    /// 4.5.d: workspace/codeLens/refresh inbound stream. The
    /// actor publishes `LspCodeLensRefresh`; the drain pulls
    /// from this rx (subscribed via `event_bus.subscribe_typed`)
    /// and evicts per-buffer caches.
    pub pending_code_lens_refresh_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspCodeLensRefresh>>,
    /// 4.5.d: snapshot of the lenses the open picker is
    /// referencing. Cleared on picker dismiss / accept.
    /// `RoutingPayload::LspCodeLens { index }` indexes into
    /// this vec.
    pub pending_code_lens_items: Option<Vec<lsp_types::CodeLens>>,
    /// 4.5.d: server id the snapshot came from -- used by the
    /// accept handler to route `workspace/executeCommand` to
    /// the originating server (commands are server-specific).
    pub pending_code_lens_server: Option<std::sync::Arc<str>>,
    /// 4.5.e: per-buffer `documentColor` cache.
    // Phase 5.B.17: `lsp_document_color_cache` moved to
    // `editor.lsp_document_color_cache`.
    /// 4.5.e: in-flight `documentColor` single-flight slot.
    pub pending_document_color_token: Option<lattice_protocol::CancellationToken>,
    pub pending_document_color_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<DocumentColorOutcome>>,
    /// 4.5.e: snapshot of color presentations the open picker
    /// is referencing. Cleared on dismiss / accept.
    /// Routing payload index points into this vec.
    pub pending_color_presentations: Option<Vec<lsp_types::ColorPresentation>>,
    /// 4.5.e: original color literal's range -- needed by the
    /// accept handler so it can replace the literal even when
    /// the chosen `ColorPresentation` has no `text_edit` (the
    /// `label` then replaces at this range).
    pub pending_color_range: Option<lsp_types::Range>,
    /// 4.4.g: in-flight inlayHint single-flight slot.
    pub pending_inlay_hint_token: Option<lattice_protocol::CancellationToken>,
    pub pending_inlay_hint_rx: Option<tokio::sync::mpsc::UnboundedReceiver<InlayHintOutcome>>,
    /// 4.4.h: per-buffer semantic-tokens cache. Refilled when
    /// the document version changes; the renderer overlay
    /// repaints span ranges that fall under a token with a
    /// kind-driven style.
    // Phase 5.B.17: `lsp_semantic_tokens_cache` moved to
    // `editor.lsp_semantic_tokens_cache`.
    /// 4.4.h: in-flight semanticTokens/full single-flight slot.
    pub pending_semantic_tokens_token: Option<lattice_protocol::CancellationToken>,
    pub pending_semantic_tokens_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<SemanticTokensOutcome>>,
    /// 4.4.j: per-buffer pull-diagnostics cache. Keys the
    /// last `result_id` the server issued so the next pull
    /// can be answered as `Unchanged` cheaply. The pump
    /// re-issues on document-version change OR cache miss
    /// (e.g. after `workspace/diagnostic/refresh` evicts).
    // Phase 5.B.17: `lsp_pull_diagnostics_cache` moved to
    // `editor.lsp_pull_diagnostics_cache`.
    /// 4.4.j: in-flight `textDocument/diagnostic` single-flight
    /// slot. Each new request cancels its predecessor.
    pub pending_pull_diagnostics_token: Option<lattice_protocol::CancellationToken>,
    pub pending_pull_diagnostics_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<PullDiagnosticsOutcome>>,
    /// 4.4.j: receiver for `LspDiagnosticRefresh` events.
    /// Actor publishes when a server sends
    /// `workspace/diagnostic/refresh`; the App's per-tick
    /// drain evicts `lsp_pull_diagnostics_cache` entries for
    /// buffers attached to that server, and the next render
    /// tick re-pulls.
    pub pending_diagnostic_refresh_rx:
        Option<tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::LspDiagnosticRefresh>>,
    // 4.4.f: stash for `lsp-folding-mode` activation moved
    // into `BufferLocals` (owned by the mode via the
    // `PriorFoldmethod` typed local in
    // `lattice_lsp::folding_sync`). Modes own their own
    // lifecycle state; the App is just the orchestrator.
    // `completion.auto_insert_single` lives on the typed-options
    // registry (`self.editor.config` type-keyed by
    // `lattice_config::CompletionAutoInsertSingle`). Read via
    // [`Self::completion_auto_insert_single`].
    // Phase 5.B.11: `auto_submit_after_chord` moved to
    // `editor.editor.auto_submit_after_chord`.
    /// LSP subsystem handle (DESIGN.md §5.4, Phase 4.1.h +
    /// audit slice 1). Reads (`servers_for`, `running_actors`,
    /// `configs`, ...) are wait-free against an
    /// `ArcSwap<SupervisorSnapshot>`; writes (`open_buffer`,
    /// `attach_handle`, `close_buffer`, `flush*`, `shutdown`)
    /// route through the supervisor task's mailbox. The UI
    /// thread can call any read on the keystroke / render path
    /// without ever blocking, and the previous
    /// `Arc<tokio::sync::Mutex<LspSupervisor>>` -- which
    /// silently dropped work via `try_lock` whenever an async
    /// `:e <path>` held the mutex across the LSP `initialize`
    /// handshake -- is gone.
    // 5.B.18b: `lsp`, `lsp_diagnostics`, `lsp_logger`
    // moved to `editor.{lsp, lsp_diagnostics, lsp_logger}`.
    /// 4.4.l.2: file-watcher service backing
    /// `workspace/didChangeWatchedFiles`. Held on App for now;
    /// migrates when its inner type
    /// (`crate::app::lsp_watcher::LspFileWatcher`) relocates to
    /// a host module.
    pub lsp_file_watcher: Option<crate::app::lsp_watcher::LspFileWatcher>,
    // 5.B.18b: `pending_apply_edit_rx`,
    // `pending_configuration_rx`,
    // `pending_show_document_rx`,
    // `pending_show_message_request_rx` moved to
    // `editor.{pending_apply_edit_rx,
    // pending_configuration_rx, pending_show_document_rx,
    // pending_show_message_request_rx}`.
    // 5.B.18b: `lsp_pending_show_message_requests` moved to
    // `editor.lsp_pending_show_message_requests`.
    // 5.B.18b: `lsp_show_message_request_queue`,
    // `lsp_next_show_message_request_id` moved to
    // `editor.{lsp_show_message_request_queue,
    // lsp_next_show_message_request_id}`.
    // 5.B.18b: `lsp_config_tree` moved to
    // `editor.lsp_config_tree`.
    // 5.B.18b: `buffer_uris` moved to `editor.buffer_uris`.
}

/// One open completion popup (DESIGN.md §5.11.3 vertico-style
/// rendering). Built by `Action::CommandLineCompleteOrAdvance`
/// when the user presses Tab; consumed by accept / dismiss / scroll
/// actions.
#[derive(Debug, Clone)]
pub struct CompletionState {
    pub candidates: Vec<lattice_completion::RenderedCandidate>,
    pub selected: usize,
    /// Byte offset within `App.editor.command_line` where the prefix being
    /// completed begins. The accept-handler replaces
    /// `[replace_start, command_line.len())` with the chosen
    /// candidate's `text`.
    pub replace_start: usize,
    /// What the cmdline looked like at popup-open time (for
    /// debugging + future filter-as-you-type refinement).
    pub original_line: String,
}

// `Fold` + `FoldMethod` moved to `lattice_core::folding` for
// renderer-agnostic ownership. Both re-exported through
// `lattice_core`'s crate root + this re-export so existing
// `crate::app::Fold` / `crate::app::FoldMethod` call sites
// keep resolving unchanged.
pub use lattice_core::{Fold, FoldMethod};


/// CSM.8b: `LspCompletionMeta` + `LSP_COMPLETION_KIND_ID`
/// moved into `lattice-lsp::completion` so the type lives in
/// the crate that owns lsp-types. The host imports them via
/// this re-export -- candidate payloads carry the serde-
/// encoded form directly (`encode_meta` / `decode_meta`) so
/// the parallel `App.insert_completion_lsp_meta` sidecar is
/// gone; the candidate IS the metadata.
pub use lattice_lsp::completion::{
    LSP_COMPLETION_KIND_ID, LspCompletionMeta, decode_meta, encode_meta,
};
/// `Extension::kind_id` discriminant for snippet-sourced
/// candidates (Phase 4.2.g.4). Sidecar metadata lives in
/// `App.insert_completion_snippet_meta`.
pub const SNIPPET_COMPLETION_KIND_ID: u32 = 2;

/// Sidecar metadata for snippet candidates in the popup. The
/// host renders the snippet body on accept and starts an
/// `ActiveSnippet`; this struct carries the parsed body +
/// the display fields the popup row uses.
#[derive(Debug, Clone)]
pub struct SnippetCandidateMeta {
    pub name: String,
    pub prefix: String,
    pub description: Option<String>,
    pub body: lattice_snippet::SnippetBody,
}


// Phase 5.2: OptionCache, LastFind, MacroRecording, TagStackEntry,
// PositionEntry, PositionSource, ReplaceEntry, LastVisual,
// SubstitutePreview, PendingBlockInsert moved to lattice_host::state.
pub use lattice_host::state::{
    LastFind, LastVisual, MacroRecording, OptionCache, PendingBlockInsert, PositionEntry,
    PositionSource, ReplaceEntry, SubstitutePreview, TagStackEntry,
};


impl std::fmt::Debug for App {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("App")
            .field("cursor", &self.editor.cursor)
            .field("scroll", &self.editor.scroll)
            .field("should_quit", &self.editor.should_quit)
            .field("viewport_height", &self.editor.viewport_height)
            .field("modal", &self.editor.modal)
            .field("command_line", &self.editor.command_line)
            .field("last_message", &self.editor.last_message)
            .field("dirty", &self.document.dirty())
            .finish()
    }
}

impl App {
    /// Set the echo / status-line message. The renderer reads
    /// the most recent value into the bottom row each frame.
    ///
    /// Side effects:
    /// 1. Updates [`Self::last_message`] for the echo-area
    ///    paint (renderer reads this each frame).
    /// 2. Emits a `tracing::event!` at the matching level.
    ///    The boot-installed `MessagesLayer`
    ///    (`lattice_runtime::install_messages_subscriber`)
    ///    captures the event, pushes a [`MessageRecord`] to
    ///    [`MessagesRing`] (`self.messages`), and publishes
    ///    [`lattice_runtime::MessagePushed`] on the editor
    ///    event bus. The `*messages*` buffer's per-tick drain
    ///    ([`Self::drain_message_events`]) appends each record.
    ///
    /// **msg-mode.1 contract:** `set_message` writes directly
    /// to the ring + bus (the legacy path). The
    /// [`lattice_runtime::MessagesLayer`] installed at boot
    /// captures every *other* `tracing::*` event in the editor
    /// (LSP layer logs, plugin host, future telemetry) into
    /// the same ring + bus. Two paths, one destination: every
    /// record the user sees in `*messages*` comes from one of
    /// these sources, and both produce identical
    /// `MessageRecord` shapes.
    ///
    /// **Why not route `set_message` through `tracing::event!`
    /// directly:** `tracing::subscriber::set_global_default` is
    /// process-wide. The first App installed captures the
    /// global subscriber; subsequent Apps (in tests, or
    /// multi-window setups) can't replace it. Keeping
    /// `set_message` independent of the global subscriber
    /// preserves per-App isolation — each App's bus + ring are
    /// the only consumers of its own `set_message` calls.
    pub fn set_message(&mut self, level: EchoLevel, text: impl Into<String>) {
        let text: String = text.into();
        self.editor.last_message = Some(EchoMessage {
            text: text.clone(),
            level,
        });
        let record = MessageRecord {
            timestamp: std::time::SystemTime::now(),
            level: echo_level_to_wire(level),
            text,
        };
        if let Ok(mut ring) = self.editor.messages.lock() {
            ring.push(record.clone());
        }
        self.editor.event_bus.publish_typed(MessagePushed { record });
    }
}

pub(crate) fn line_byte_len(buf: &Buffer, line: u32) -> u32 {
    // §8.2 hot path: use ropey's O(log n) line API instead of
    // materialising the whole buffer.
    buf.line_byte_len(line)
}

pub(crate) fn last_addressable_line(buf: &Buffer) -> u32 {
    let lc = buf.line_count();
    if lc == 0 {
        return 0;
    }
    // ropey reports an extra empty line for any rope ending in
    // `\n`. Detect that by checking whether the last "line" the
    // rope reports is empty, without materialising the entire
    // buffer text.
    let last_idx = lc - 1;
    if buf.line_byte_len(last_idx) == 0 && lc >= 2 {
        last_idx - 1
    } else {
        last_idx
    }
}

pub(super) fn is_valid_mark_name(c: char) -> bool {
    c.is_ascii_alphabetic() || c.is_ascii_digit()
}

/// Render a register's content into a one-line preview (truncated and
/// with newlines escaped). Used by `:reg`.
pub(super) fn preview_register(s: &str) -> String {
    const MAX: usize = 40;
    let escaped: String = s
        .chars()
        .map(|c| if c == '\n' { '\u{21B5}' } else { c })
        .collect();
    if escaped.chars().count() <= MAX {
        escaped
    } else {
        let trimmed: String = escaped.chars().take(MAX).collect();
        format!("{trimmed}…")
    }
}

pub(super) fn is_word_char_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Cross-source visual dedup for the insert-completion popup
/// (Phase 4.2.g.7 polish). Keeps the FIRST occurrence of each
/// `raw.text`; subsequent rows with the same text drop out.
/// Called after the ranker has sorted descending by score, so
/// the surviving row is the highest-ranked entry per text --
/// the buffer-words copy of `outer` outranks the tree-sitter
/// copy at the spec's 100/80 priority split, so the popup row
/// for `outer` carries the buffer-words tag.
///
/// Selection / navigation / accept all index into the deduped
/// vec naturally; this is the only place we coalesce rows
/// across sources, and it runs before the popup paints.
pub(super) fn dedup_rendered_by_text(rendered: &mut Vec<lattice_completion::RenderedCandidate>) {
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
    rendered.retain(|cand| seen.insert(cand.raw.text.clone()));
}

/// True for bytes the path-completion source treats as part of
/// a filename / directory component. Wider than `is_word_char_byte`
/// so common filename characters (`.`, `-`, `~`) ride the same
/// segment; the trigger anchor breaks at `/` (dir boundary)
/// rather than at these characters.
pub(super) fn is_path_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || matches!(b, b'_' | b'-' | b'.' | b'~' | b'+' | b'@')
}

/// Expand `~/...` against `$HOME` and absolutise a relative
/// path against the process's current working directory.
///
/// Two failure modes this closes:
///
/// 1. `:e ~/foo.rs` -- without expansion, `Document::open`
///    forwards the literal `~` to `read_to_string` and fails
///    ENOENT. Cmdline completion (`gen:files`) already does
///    its own tilde expansion for `read_dir`, which is why
///    completion appears to work but submit fails.
///
/// 2. Oil opened at a single-component relative path (`src`,
///    `foo.rs`, `.`) navigating up. `Path::parent()` returns
///    `Some("")` for those, and `read_dir("")` is ENOENT.
///    Normalising before storing keeps `OilDir` absolute, so
///    `parent()` walks the filesystem correctly.
///
/// We deliberately do not call `canonicalize` -- it requires
/// the target to exist (`:e new.rs` against an unsaved path
/// has to work) and would surprise users by resolving symlinks
/// vim treats as opaque. Cwd-failure or HOME-missing falls
/// through to the input verbatim; the caller's open then
/// produces the same error it would have produced without
/// the normalise step.
pub(super) fn normalize_user_path(path: &std::path::Path) -> std::path::PathBuf {
    let expanded = expand_tilde(path);
    if expanded.is_absolute() {
        return expanded;
    }
    match std::env::current_dir() {
        Ok(cwd) => cwd.join(expanded),
        Err(_) => expanded,
    }
}

/// `~/rest` → `$HOME/rest`, `~` alone → `$HOME`. Anything else
/// passes through unchanged. Mirrors the helper in
/// `lattice-completion::builtins::generators` -- they stay
/// in lockstep because the cmdline-completion source and the
/// `:e` submit path both flow through user-typed strings.
fn expand_tilde(path: &std::path::Path) -> std::path::PathBuf {
    let Some(s) = path.to_str() else {
        return path.to_path_buf();
    };
    let Some(home) = std::env::var_os("HOME") else {
        return path.to_path_buf();
    };
    if s == "~" {
        return std::path::PathBuf::from(home);
    }
    if let Some(rest) = s.strip_prefix("~/") {
        return std::path::PathBuf::from(home).join(rest);
    }
    path.to_path_buf()
}

/// True if `line_idx` is empty or whitespace-only. Used by the
/// fold-aware j/k snap to swallow trailing blanks between sibling
/// folds (so `j` from a closed fold's heading lands on the next
/// sibling's heading, not on the blank between them).
pub(super) fn is_blank_line(buffer: &lattice_core::Buffer, line_idx: u32) -> bool {
    buffer
        .line(line_idx)
        .map(|s| s.trim().is_empty())
        .unwrap_or(true)
}

/// Phase 4.2 features (hover, definition, references, completion)
/// all need this; later we'll thread the per-server negotiated
/// `PositionEncodingKind` through here so utf-8 / utf-32 servers
/// don't pay the utf-16 conversion. For 4.2.b utf-16 is correct
/// for every server we care about today.
/// Render an LSP `SignatureHelp` response into a markdown body
/// the popup renderer can display. Picks the active signature
/// (server-supplied `active_signature` index, default 0) and
/// inlines the active parameter's documentation when present.
/// Returns the empty string when the response carries no
/// signatures.
pub(crate) fn signature_help_to_markdown(sh: &lsp_types::SignatureHelp) -> String {
    if sh.signatures.is_empty() {
        return String::new();
    }
    let active_sig_idx = sh.active_signature.unwrap_or(0) as usize;
    let sig = sh
        .signatures
        .get(active_sig_idx)
        .or_else(|| sh.signatures.first())
        .expect("non-empty checked above");
    let mut out = String::new();
    // Active signature's call form -- renders as a fenced code
    // block so the popup's markdown highlighter picks up
    // syntax highlighting (whatever language the server says
    // -- we don't know, so default to text).
    out.push_str("```text\n");
    out.push_str(&sig.label);
    out.push_str("\n```\n");
    // Parameter highlight: append a short note pointing at the
    // active parameter's name. The popup overlay doesn't yet
    // do per-character paragraph styling, so this is the
    // friendliest representation we have without bespoke
    // rendering.
    if let Some(active_param_idx) = sig.active_parameter.or(sh.active_parameter)
        && let Some(params) = sig.parameters.as_ref()
        && let Some(param) = params.get(active_param_idx as usize)
    {
        let label_str = match &param.label {
            lsp_types::ParameterLabel::Simple(s) => s.clone(),
            lsp_types::ParameterLabel::LabelOffsets(_) => String::new(),
        };
        if !label_str.is_empty() {
            out.push_str(&format!("\n**param:** `{label_str}`\n"));
        }
        if let Some(doc) = param.documentation.as_ref() {
            let doc_str = match doc {
                lsp_types::Documentation::String(s) => s.clone(),
                lsp_types::Documentation::MarkupContent(mc) => mc.value.clone(),
            };
            if !doc_str.is_empty() {
                out.push('\n');
                out.push_str(&doc_str);
                out.push('\n');
            }
        }
    }
    // Signature-level documentation when present.
    if let Some(doc) = sig.documentation.as_ref() {
        let doc_str = match doc {
            lsp_types::Documentation::String(s) => s.clone(),
            lsp_types::Documentation::MarkupContent(mc) => mc.value.clone(),
        };
        if !doc_str.is_empty() {
            out.push('\n');
            out.push_str(&doc_str);
            out.push('\n');
        }
    }
    out
}

/// Inverse of `app_to_lsp_position` -- LSP utf-16 character
/// column → utf-8 byte column on the given line. Used by
/// `apply_lsp_text_edits` to convert TextEdit ranges back to
/// the App's `Position` shape.
pub(crate) fn lsp_position_to_app_byte(buffer: &Buffer, line: u32, character: u32) -> u32 {
    let line_text = buffer.line(line).unwrap_or_default();
    lattice_lsp::position::utf16_column_to_utf8_byte(&line_text, character)
}

/// 4.4.e: does the App `(line, byte)` cursor sit inside the
/// half-open LSP `range`? We compare only by (line, character)
/// in LSP utf-16 space because the range itself is LSP-shape;
/// the App cursor is converted on the fly.
pub(crate) fn cursor_inside_range(cursor: Position, range: &lsp_types::Range) -> bool {
    let line = cursor.line;
    let col = cursor.byte;
    let start = (range.start.line, range.start.character);
    let end = (range.end.line, range.end.character);
    let here = (line, col);
    here >= start && here < end
}

/// 4.4.f: convert an LSP `FoldingRange` to our `Fold`. LSP
/// returns line-based extents; the identity hash combines
/// start_line + end_line + kind so closed-state survives
/// re-fetches that produce the same logical fold.
pub(crate) fn folding_range_to_fold(r: lsp_types::FoldingRange) -> Fold {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    r.start_line.hash(&mut hasher);
    r.end_line.hash(&mut hasher);
    // Spec omits a stable u32 for `kind`; hash the string form
    // when present so a fold's kind change still re-keys.
    if let Some(kind) = r.kind.as_ref() {
        std::mem::discriminant(kind).hash(&mut hasher);
    }
    Fold {
        start_line: r.start_line,
        end_line: r.end_line,
        closed: false,
        identity: Some(hasher.finish()),
    }
}

/// 4.4.e: flatten an LSP `SelectionRange` (linked list via
/// `parent`) into a `Vec<Range>` ordered innermost-first.
pub(crate) fn flatten_selection_range_chain(
    head: &lsp_types::SelectionRange,
) -> Vec<lsp_types::Range> {
    let mut out = Vec::new();
    let mut cur = Some(head);
    while let Some(node) = cur {
        out.push(node.range);
        cur = node.parent.as_deref();
    }
    out
}

pub(crate) fn app_to_lsp_position(buffer: &Buffer, p: Position) -> Option<lsp_types::Position> {
    let line_text = buffer.line(p.line)?;
    let character = lattice_lsp::position::utf8_byte_to_utf16_column(&line_text, p.byte);
    Some(lsp_types::Position {
        line: p.line,
        character,
    })
}

/// Flatten a `DocumentSymbolResponse` into our pre-rendered
/// `SymbolRow` shape. The legacy `Flat(Vec<SymbolInformation>)`
/// variant is one row per symbol with no nesting; the modern
/// `Nested(Vec<DocumentSymbol>)` variant carries
/// `children: Vec<DocumentSymbol>`, walked depth-first to
/// preserve outline ordering.
pub(crate) fn flatten_document_symbol_response(
    resp: lsp_types::DocumentSymbolResponse,
    path: &std::path::Path,
    out: &mut Vec<SymbolRow>,
) {
    match resp {
        lsp_types::DocumentSymbolResponse::Flat(syms) => {
            for sym in syms {
                if let Some(row) = symbol_information_to_row(&sym) {
                    out.push(row);
                }
            }
        }
        lsp_types::DocumentSymbolResponse::Nested(syms) => {
            fn walk(
                syms: Vec<lsp_types::DocumentSymbol>,
                path: &std::path::Path,
                depth: u32,
                out: &mut Vec<SymbolRow>,
            ) {
                for sym in syms {
                    out.push(SymbolRow {
                        name: sym.name.clone(),
                        kind_glyph: symbol_kind_glyph(sym.kind),
                        container: None,
                        depth,
                        path: path.to_path_buf(),
                        line: sym.selection_range.start.line,
                        col: sym.selection_range.start.character,
                    });
                    if let Some(children) = sym.children {
                        walk(children, path, depth + 1, out);
                    }
                }
            }
            walk(syms, path, 0, out);
        }
    }
}

/// Map a flat `SymbolInformation` (legacy outline + workspace
/// symbol shape) into our row type. Returns `None` when the
/// location's URI doesn't resolve to a path.
/// Convert a modern (LSP 3.17+) `WorkspaceSymbol` into a
/// `SymbolRow` (Phase 4.2 follow-up). When the symbol's
/// `location` came back as the `WorkspaceLocation` (URI-only)
/// variant, fires `workspaceSymbol/resolve` against the
/// originating server to upgrade to a real `Location` with
/// `range`. Returns `None` when:
/// - The URI doesn't map to a path.
/// - Resolve fails (server doesn't actually advertise it,
///   or returns a still-unresolved shape).
/// - Cancellation fires while we're awaiting resolve.
pub(crate) async fn workspace_symbol_to_row(
    handle: &lattice_lsp::ServerHandle,
    sym: lsp_types::WorkspaceSymbol,
    token: &lattice_protocol::CancellationToken,
) -> Option<SymbolRow> {
    use lsp_types::OneOf;
    let (path, line, col) = match &sym.location {
        OneOf::Left(loc) => (
            lattice_lsp::actor::uri_to_path(&loc.uri)?,
            loc.range.start.line,
            loc.range.start.character,
        ),
        OneOf::Right(wsl) => {
            let path = lattice_lsp::actor::uri_to_path(&wsl.uri)?;
            // Server's resolveProvider absent -> no point firing.
            // Fall back to (0, 0); the user can still navigate
            // to the file.
            if !handle.capabilities().workspace_symbol_resolve_provider() {
                (path, 0, 0)
            } else {
                match handle
                    .workspace_symbol_resolve(sym.clone(), token.clone())
                    .await
                {
                    Ok(resolved) => match resolved.location {
                        OneOf::Left(loc) => (
                            lattice_lsp::actor::uri_to_path(&loc.uri).unwrap_or(path),
                            loc.range.start.line,
                            loc.range.start.character,
                        ),
                        // Server replied without populating range
                        // -- spec violation, but defensive: fall
                        // back to (0, 0) instead of dropping.
                        OneOf::Right(_) => (path, 0, 0),
                    },
                    // Resolve failed -- log via the symbol's
                    // path-only fallback. Caller still gets a
                    // navigable row.
                    Err(_) => (path, 0, 0),
                }
            }
        }
    };
    Some(SymbolRow {
        name: sym.name,
        kind_glyph: symbol_kind_glyph(sym.kind),
        container: sym.container_name,
        depth: 0,
        path,
        line,
        col,
    })
}

pub(crate) fn symbol_information_to_row(sym: &lsp_types::SymbolInformation) -> Option<SymbolRow> {
    let path = lattice_lsp::actor::uri_to_path(&sym.location.uri)?;
    Some(SymbolRow {
        name: sym.name.clone(),
        kind_glyph: symbol_kind_glyph(sym.kind),
        container: sym.container_name.clone(),
        depth: 0,
        path,
        line: sym.location.range.start.line,
        col: sym.location.range.start.character,
    })
}

/// 4.5.a: render one `CallHierarchyItem` (the caller in an
/// `IncomingCall.from` or the callee in an
/// `OutgoingCall.to`) as a [`SymbolRow`] for the picker. The
/// item's `detail` (if any) plus the *originating* callable's
/// name ride in `container` -- e.g. for an incoming-calls
/// listing of `foo`, the rows say "bar.rs:42 fn quux  (in foo)".
/// Falls back to `(path, 0, 0)` if the URI doesn't parse;
/// keeping the row visible beats silently dropping it.
pub(crate) fn call_hierarchy_to_row(
    item: &lsp_types::CallHierarchyItem,
    related_to: &str,
) -> SymbolRow {
    let path = lattice_lsp::actor::uri_to_path(&item.uri).unwrap_or_default();
    SymbolRow {
        name: item.name.clone(),
        kind_glyph: symbol_kind_glyph(item.kind),
        container: Some(format!("in {related_to}")),
        depth: 0,
        path,
        line: item.selection_range.start.line,
        col: item.selection_range.start.character,
    }
}

/// 4.5.c: does the given `range` cover the LSP `position`?
/// Inclusive on both ends (matches VSCode's "click-through"
/// semantics on a link's rightmost char). Used by the `gx`
/// keystroke to find the first cached `documentLink` under
/// the cursor.
pub(crate) fn range_covers(range: lsp_types::Range, position: lsp_types::Position) -> bool {
    let after_start =
        (range.start.line, range.start.character) <= (position.line, position.character);
    let before_end = (position.line, position.character) <= (range.end.line, range.end.character);
    after_start && before_end
}

/// 4.5.b: render one `TypeHierarchyItem` (a supertype or
/// subtype of the cursor's type) as a [`SymbolRow`] for the
/// picker. Same projection as [`call_hierarchy_to_row`]
/// because the item shape is identical (name, kind, uri,
/// range, selection_range); the container hint differs
/// (`"super of foo"` / `"sub of foo"`).
pub(crate) fn type_hierarchy_to_row(
    item: &lsp_types::TypeHierarchyItem,
    related_to: &str,
) -> SymbolRow {
    let path = lattice_lsp::actor::uri_to_path(&item.uri).unwrap_or_default();
    SymbolRow {
        name: item.name.clone(),
        kind_glyph: symbol_kind_glyph(item.kind),
        container: Some(format!("of {related_to}")),
        depth: 0,
        path,
        line: item.selection_range.start.line,
        col: item.selection_range.start.character,
    }
}

/// Extract a placeholder string from a `PrepareRenameResponse`.
/// The spec gives three shapes: a Range (no placeholder, just
/// "you can rename here"), a Range+Placeholder (preferred), or
/// a DefaultBehavior signal (server defers to the editor's
/// word-under-cursor heuristic). We pull the placeholder when
/// present, else `None` so the App's caller can fall back to
/// the heuristic.
pub(crate) fn prepare_rename_placeholder(
    resp: &lsp_types::PrepareRenameResponse,
) -> Option<String> {
    match resp {
        lsp_types::PrepareRenameResponse::RangeWithPlaceholder { placeholder, .. } => {
            Some(placeholder.clone())
        }
        lsp_types::PrepareRenameResponse::Range(_) => None,
        lsp_types::PrepareRenameResponse::DefaultBehavior { .. } => None,
    }
}

/// Flatten a `WorkspaceEdit` into a per-file
/// `Vec<(Uri, Vec<TextEdit>)>`. Handles both the legacy `changes`
/// HashMap shape and the modern `document_changes` shape (which
/// also carries DocumentChangeOperation::Op create/rename/delete
/// -- those are skipped in v1; rename returns plain text edits
/// for ~100% of identifiers).
///
/// Empty Vec means "nothing to apply" (the App echoes `Empty`).
pub(crate) fn flatten_workspace_edit(
    we: lsp_types::WorkspaceEdit,
) -> Vec<(lsp_types::Uri, Vec<lsp_types::TextEdit>)> {
    let mut out: Vec<(lsp_types::Uri, Vec<lsp_types::TextEdit>)> = Vec::new();
    if let Some(changes) = we.changes {
        for (uri, edits) in changes {
            if !edits.is_empty() {
                out.push((uri, edits));
            }
        }
    }
    if let Some(doc_changes) = we.document_changes {
        match doc_changes {
            lsp_types::DocumentChanges::Edits(edits) => {
                for te_doc in edits {
                    let uri = te_doc.text_document.uri.clone();
                    let raw_edits: Vec<lsp_types::TextEdit> = te_doc
                        .edits
                        .into_iter()
                        .filter_map(|e| match e {
                            lsp_types::OneOf::Left(te) => Some(te),
                            // AnnotatedTextEdit -- strip the
                            // annotation; v1 doesn't surface
                            // change-annotations to the user.
                            lsp_types::OneOf::Right(ate) => Some(ate.text_edit),
                        })
                        .collect();
                    if !raw_edits.is_empty() {
                        out.push((uri, raw_edits));
                    }
                }
            }
            // create-file / rename-file / delete-file ops are
            // skipped in v1 -- the rename use case is identifier
            // rewrites, which servers return as plain text edits.
            lsp_types::DocumentChanges::Operations(_) => {}
        }
    }
    out
}

/// Single-character glyph for an LSP `CodeActionKind`. Maps
/// the standard kinds (quickfix, refactor*, source*) to a
/// short visual marker for the picker margin. Unknown / custom
/// kinds and bare `Command` payloads land on `?`.
pub(crate) fn code_action_kind_glyph(kind: Option<&lsp_types::CodeActionKind>) -> &'static str {
    use lsp_types::CodeActionKind as K;
    let Some(kind) = kind else {
        return "?";
    };
    if *kind == K::QUICKFIX {
        "🛠"
    } else if *kind == K::REFACTOR {
        "♻"
    } else if *kind == K::REFACTOR_EXTRACT {
        "↗"
    } else if *kind == K::REFACTOR_INLINE {
        "↘"
    } else if *kind == K::REFACTOR_REWRITE {
        "↺"
    } else if *kind == K::SOURCE {
        "★"
    } else if *kind == K::SOURCE_ORGANIZE_IMPORTS {
        "≡"
    } else if *kind == K::SOURCE_FIX_ALL {
        "✓"
    } else {
        "?"
    }
}

/// Single-character glyph for an LSP `CompletionItemKind`.
/// Same shape as `symbol_kind_glyph` but maps the completion-
/// item kind enum (which is wider -- snippets, keywords,
/// folders, etc.). Kept narrow on purpose; richer per-kind
/// styling lives in the renderer once buffer-level Insert
/// completion lands.
pub(crate) fn completion_kind_glyph(kind: Option<lsp_types::CompletionItemKind>) -> &'static str {
    use lsp_types::CompletionItemKind as K;
    match kind {
        Some(K::FUNCTION) | Some(K::METHOD) | Some(K::CONSTRUCTOR) => "ƒ",
        Some(K::VARIABLE) | Some(K::FIELD) | Some(K::PROPERTY) => "v",
        Some(K::CONSTANT) => "K",
        Some(K::CLASS) | Some(K::INTERFACE) => "🅒",
        Some(K::STRUCT) => "🅢",
        Some(K::ENUM) | Some(K::ENUM_MEMBER) => "🅔",
        Some(K::MODULE) => "📦",
        Some(K::FILE) | Some(K::FOLDER) => "📄",
        Some(K::SNIPPET) => "✂",
        Some(K::KEYWORD) => "K",
        Some(K::TEXT) => "≡",
        Some(K::REFERENCE) => "→",
        _ => "?",
    }
}

/// Single-character glyph for an LSP `SymbolKind`. Picked to
/// fit a fixed-width column in picker rows so the marginalia
/// column stays aligned. Falls back to `?` for kinds we don't
/// have a specific glyph for.
pub(crate) fn symbol_kind_glyph(kind: lsp_types::SymbolKind) -> &'static str {
    use lsp_types::SymbolKind as K;
    match kind {
        K::FILE => "📄",
        K::MODULE | K::NAMESPACE | K::PACKAGE => "📦",
        K::CLASS | K::INTERFACE => "🅒",
        K::METHOD | K::FUNCTION => "ƒ",
        K::CONSTRUCTOR => "🅒",
        K::PROPERTY | K::FIELD => "•",
        K::VARIABLE => "v",
        K::CONSTANT => "K",
        K::STRING | K::NUMBER | K::BOOLEAN | K::ARRAY | K::OBJECT => "≡",
        K::ENUM | K::ENUM_MEMBER => "🅔",
        K::STRUCT => "🅢",
        K::EVENT => "🅔",
        K::OPERATOR => "⊕",
        K::TYPE_PARAMETER => "T",
        _ => "?",
    }
}

/// Word (alphanumeric + `_` run) under `cursor` in `buffer`, or
/// `None` when the cursor isn't on a word byte. Mirrors vim's
/// `<cword>` for the simple case that `:references` needs to
/// label its results buffer ("References for \"foo\""). Walks
/// the line once at the cursor column; doesn't scan forward to
/// the next word like `do_search_word_under_cursor` does --
/// "no symbol under cursor" is preferable to a label that
/// jumps to a different identifier than the user pointed at.
pub(crate) fn word_under_cursor(buffer: &Buffer, cursor: Position) -> Option<String> {
    let line = buffer.line(cursor.line)?;
    let bytes = line.as_bytes();
    let byte_idx = cursor.byte as usize;
    if byte_idx >= bytes.len() || !is_word_char_byte(bytes[byte_idx]) {
        return None;
    }
    let mut start = byte_idx;
    while start > 0 && is_word_char_byte(bytes[start - 1]) {
        start -= 1;
    }
    let mut end = byte_idx;
    while end < bytes.len() && is_word_char_byte(bytes[end]) {
        end += 1;
    }
    if start == end {
        return None;
    }
    Some(String::from_utf8_lossy(&bytes[start..end]).into_owned())
}

/// Flatten an LSP `GotoDefinitionResponse` (Scalar / Array /
/// Link) into a uniform `Vec<Location>`. The `Link` shape carries
/// richer per-result info (origin selection range used to
/// highlight the symbol the user clicked); we drop it for now and
/// keep the target location only -- the App's jump path is
/// position-only. When 4.2.d's picker buffer lands the link
/// metadata (e.g., `target_selection_range` for narrower jump
/// destinations) becomes useful and this function gains a richer
/// sibling.
pub(crate) fn definition_response_to_locations(
    resp: lsp_types::GotoDefinitionResponse,
) -> Vec<lsp_types::Location> {
    match resp {
        lsp_types::GotoDefinitionResponse::Scalar(loc) => vec![loc],
        lsp_types::GotoDefinitionResponse::Array(locs) => locs,
        lsp_types::GotoDefinitionResponse::Link(links) => links
            .into_iter()
            .map(|l| lsp_types::Location {
                uri: l.target_uri,
                // `target_selection_range` is the narrower symbol
                // range; `target_range` is the enclosing block.
                // Picker UX usually wants the narrower one.
                range: l.target_selection_range,
            })
            .collect(),
    }
}

/// Render an LSP `HoverContents` payload to a markdown string the
/// renderer's [`crate::hover::HoverPopup`] pipeline can highlight
/// via the markdown grammar.
///
/// `MarkedString::String(s)` keeps `s` verbatim. `MarkedString::LanguageString
/// { language, value }` wraps `value` in a fenced code block tagged with
/// `language` so the markdown injection picks it up.
/// `MarkupContent` arrives pre-rendered as either markdown or plaintext
/// (we treat plaintext as already-good markdown). `Array` joins each
/// element with two newlines so blocks separate cleanly.
pub(crate) fn hover_contents_to_markdown(contents: &lsp_types::HoverContents) -> String {
    fn marked_to_markdown(m: &lsp_types::MarkedString) -> String {
        match m {
            lsp_types::MarkedString::String(s) => s.clone(),
            lsp_types::MarkedString::LanguageString(ls) => {
                format!("```{}\n{}\n```", ls.language, ls.value)
            }
        }
    }
    match contents {
        lsp_types::HoverContents::Scalar(m) => marked_to_markdown(m),
        lsp_types::HoverContents::Array(items) => items
            .iter()
            .map(marked_to_markdown)
            .collect::<Vec<_>>()
            .join("\n\n"),
        lsp_types::HoverContents::Markup(m) => m.value.clone(),
    }
}

pub(super) fn previous_position(buf: &Buffer, p: Position) -> Position {
    if p.byte > 0 {
        Position::new(p.line, p.byte - 1)
    } else if p.line > 0 {
        let prev_line = p.line - 1;
        Position::new(prev_line, line_byte_len(buf, prev_line))
    } else {
        p
    }
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used, clippy::panic)]
    use super::test_helpers::{
        app_with, attach_test_syntax, invoke_motion, submit_ex, write_temp_file,
    };
    use super::*;
    use crate::help::HelpContent;

    #[test]
    fn normalize_user_path_expands_tilde_slash_against_home() {
        // `~/foo.rs` → `$HOME/foo.rs`. Mutating env vars
        // requires `unsafe` post-edition-2024 and the workspace
        // bans `unsafe_code`, so anchor the assertion against
        // the runner's actual HOME instead. Skip silently if
        // HOME isn't set -- not a meaningful environment.
        let Some(home) = std::env::var_os("HOME") else {
            return;
        };
        let got = normalize_user_path(std::path::Path::new("~/projects/foo.rs"));
        let expected = std::path::PathBuf::from(&home).join("projects/foo.rs");
        assert_eq!(got, expected);
    }

    #[test]
    fn normalize_user_path_expands_bare_tilde_to_home() {
        let Some(home) = std::env::var_os("HOME") else {
            return;
        };
        let got = normalize_user_path(std::path::Path::new("~"));
        assert_eq!(got, std::path::PathBuf::from(home));
    }

    #[test]
    fn normalize_user_path_leaves_absolute_paths_unchanged() {
        // Absolute paths are pass-through -- the cwd-join branch
        // is skipped. Critical for the oil dedupe path
        // (`oil_with_dir`) where two callers compare against the
        // same stored absolute key.
        let got = normalize_user_path(std::path::Path::new("/tmp/abs/path.rs"));
        assert_eq!(got, std::path::PathBuf::from("/tmp/abs/path.rs"));
    }

    #[test]
    fn normalize_user_path_absolutises_relative_against_cwd() {
        // Single-component relative paths are the case that
        // broke oil's `-` (parent of `"foo.rs"` is `""`, and
        // `read_dir("")` is ENOENT). Post-fix the input is
        // already absolute by the time it reaches `read_dir`.
        let got = normalize_user_path(std::path::Path::new("foo.rs"));
        let expected = std::env::current_dir().unwrap().join("foo.rs");
        assert_eq!(got, expected);
    }

    #[test]
    fn normalize_user_path_preserves_dotdot_relative_against_cwd() {
        // `..` segments stay -- we deliberately don't
        // canonicalise (would require the path to exist). The
        // filesystem resolves `../foo.rs` correctly through
        // `read_to_string`.
        let got = normalize_user_path(std::path::Path::new("../foo.rs"));
        let expected = std::env::current_dir().unwrap().join("../foo.rs");
        assert_eq!(got, expected);
    }

    /// Sanity check: a bare motion drives the cursor through
    /// the full translate + apply path. If this fails, the
    /// harness itself is broken; every other `press_*` test
    /// is suspect.
    /// Sanity check: an operator + motion deletes the right
    /// span end-to-end. Exercises the `[d]` action-kind
    /// short-circuit (pushes `d` into `partial_chord`) plus
    /// the `[d, w]` resolution under the prefix.
    // -- count flow seam ----------------------------------------
    //
    // These pin the path translate -> attach_count -> dispatcher
    // multiplication. Slice 8.i.4.d documented the failure mode if
    // the action-kind short-circuit didn't bypass
    // `run_document_invocation`'s `pending_count = 0;`: the reset
    // would fire before `AbsorbOperatorPrefix` could latch op_count,
    // breaking `2dw`-style flows. These tests would catch a
    // regression of that fix.

    /// `3j` moves down 3 lines: pending_count fed straight to a
    /// motion (no operator latch path).
    /// `3dd` deletes 3 lines: count latches into op_count via
    /// the `d` action-kind dispatch; the second `d` resolves
    /// linewise with the count baked in by `attach_count`. Pins
    /// slice 8.i.4.f's removal of the dispatcher's redundant
    /// multiplication, AND slice 8.i.4.g's `dd`-consumes-newline
    /// fix (line count drops by 3 -- the buffer goes from 5
    /// lines to 2, with no leading empty line).
    /// `d2w` deletes 2 words: the `2` between operator and motion
    /// must reach the digit accumulator, not get eaten by the
    /// partial_chord lookup. Pins slice 8.i.4.f's hoist of digit
    /// handling above the partial_chord short-circuit.
    /// `2d2w` deletes 4 words: op_count=2 multiplies with
    /// motion_count=2. Pins both 8.i.4.f fixes end-to-end --
    /// digit-after-operator survives, and the dispatcher honours
    /// the input-side baked count without re-multiplying.
    /// After `3j`, a bare `j` moves only one line: pending_count
    /// must reset after the motion fires.
    // -- partial_chord state machine ----------------------------
    //
    // Pins the multi-keystroke prefix walk. `gg` is a Normal-mode
    // multi-key motion (no operator); `dd` is the operator
    // self-key linewise resolution; `df,` is a 3-keystroke chord
    // (operator -> find-char prefix -> captured delimiter).

    /// `gg` jumps to the first line: prefix `g` parks in
    /// partial_chord, second `g` resolves the terminal.
    /// `df,` deletes up to (exclusive of) the comma: 3-keystroke
    /// chord across the operator + find-char captured-delimiter
    /// sub-tree (slice 8.i.4.c). Lattice's `f`-motion is exclusive
    /// (see `df_deletes_through_target_char` near the dispatch
    /// tests), so the comma stays.
    // -- <C-w> sub-tree (action-kind short-circuit, 8.i.4.d) ----

    /// `<C-w>v` splits the active pane vertically. Exercises the
    /// `<C-w>` action-kind short-circuit + the AfterCtrlW layer.
    // -- mode transition seam -----------------------------------

    /// `i` enters Insert, typed chars land in the buffer, `<Esc>`
    /// returns to Normal. Pins the modal state machine across a
    /// mode round-trip in a single keystroke stream.
    /// Subscribe a channel sink to the App's event bus. Returns

    // ---- Slice B.2 part 2: edit-delta accumulation -------------
    //
    // Pin that EditDeltas accumulate on App.editor.pending_syntax_edits
    // across edits, drain on maybe_reparse_syntax, and the
    // version baseline tracks correctly. The actual incremental
    // reparse correctness is covered by lattice-syntax's parity
    // tests; these App-level tests pin the plumbing.

    // ---- Initial state ----

    #[test]
    fn new_app_starts_at_origin_in_normal_mode() {
        let a = app_with("abc", 10);
        assert_eq!(a.editor.cursor, Position::ZERO);
        assert_eq!(a.editor.scroll, 0);
        assert!(!a.editor.should_quit);
        assert_eq!(a.editor.modal, ModalState::Normal);
        assert!(a.editor.partial_chord.is_empty());
    }

    #[test]
    fn every_advertised_completion_source_resolves_at_boot() {
        // Every `ArgSpec::completion = Some(...)` declared in
        // `lattice_grammar::ex_commands` must resolve to a
        // generator registered at App boot. Otherwise typing
        // `<Tab>` on that arg silently produces no candidates --
        // the bug class that motivated this slice.
        let a = app_with("hi", 5);
        for name in a.editor.registry.names() {
            let id = a.editor.registry.id_by_name(name).unwrap();
            let Some(spec) = a.editor.registry.ex_command_spec(id) else {
                continue;
            };
            for arg in &spec.args_schema {
                if let Some(source) = arg.completion {
                    assert!(
                        a.completion_registry.generator_by_name(source).is_some(),
                        "{name}'s arg `{}` advertises completion source `{source}` \
                         but no generator with that name is registered at boot",
                        arg.name,
                    );
                }
            }
        }
    }

    #[test]
    fn modal_label_reports_state() {
        let mut a = app_with("", 10);
        assert_eq!(a.modal_label(), "NORMAL");
        a.apply(Action::EnterMode(ModalState::Insert));
        assert_eq!(a.modal_label(), "INSERT");
    }

    #[test]
    fn quit_sets_flag() {
        let mut a = app_with("abc", 10);
        a.apply(Action::Quit);
        assert!(a.editor.should_quit);
    }

    // ---- Motion via grammar engine ----

    // ---- Insert mode ----

    // ---- Operator + motion composition ----

    // ---- Undo / Redo ----

    // ---- Viewport scrolling ----

    // ---- Command-line minibuffer ----

    #[test]
    fn submitting_returns_to_normal_modal() {
        let mut a = app_with("hello", 10);
        a.apply(Action::EnterCommandLine);
        a.apply(Action::CommandLineAppend('q'));
        a.apply(Action::CommandLineSubmit);
        assert_eq!(a.editor.modal, ModalState::Normal);
    }

    #[test]
    fn echo_action_replaces_last_message() {
        let mut a = app_with("", 10);
        a.apply(Action::Echo(EchoMessage {
            text: "hi".into(),
            level: EchoLevel::Info,
        }));
        assert_eq!(a.editor.last_message.as_ref().unwrap().text, "hi");
        a.apply(Action::Echo(EchoMessage {
            text: "bye".into(),
            level: EchoLevel::Warn,
        }));
        assert_eq!(a.editor.last_message.as_ref().unwrap().text, "bye");
        assert_eq!(a.editor.last_message.as_ref().unwrap().level, EchoLevel::Warn);
    }
    // ---- change operator end-to-end ----

    // ---- Substitute (:s/foo/bar/[g]) ----

    // ---- Line join (J / gJ) ----

    // ---- WORD motions (W, B, E) end-to-end ----

    // ---- Position history (Ctrl-O / Ctrl-I) ----

    // ---- partial_chord lifecycle (regression) ----
    //
    // Slice 8.i.4 retired the legacy `Pending` enum; the regression
    // these tests pin is now expressed against `partial_chord`:
    // any non-`AbsorbPartialChord(_)` action must clear the
    // chord stack so a leftover prefix doesn't mis-route the
    // next keystroke.

    #[test]
    fn zz_clears_partial_chord_so_next_key_is_a_motion() {
        // Regression: previously `zz` left pending=AfterZ, so `j` after
        // `zz` was interpreted as `zj` (GotoNextFold) and emitted "no
        // more folds". Now expressed against partial_chord.
        let mut a = app_with("a\nb\nc\nd\ne", 10);
        a.apply(Action::AbsorbPartialChord(crate::chord::KeyChord::char(
            'z',
        )));
        a.apply(Action::ScrollCursorTo(ScrollPos::Center));
        assert!(a.editor.partial_chord.is_empty());
    }

    #[test]
    fn play_macro_clears_partial_chord() {
        let mut a = app_with("hello", 10);
        a.apply(Action::AbsorbPartialChord(crate::chord::KeyChord::char(
            '@',
        )));
        // No macro recorded; this errors but should still clear partial_chord.
        a.apply(Action::PlayMacro('z'));
        assert!(a.editor.partial_chord.is_empty());
    }

    // ---- §5.1.1 unified position history ----

    #[test]
    fn g_semicolon_walks_named_mark_history_backward() {
        let mut a = app_with("a\nb\nc\nd\ne", 10);
        // Set marks at three positions.
        a.editor.cursor = Position::new(1, 0);
        a.apply(Action::SetMark('a'));
        a.editor.cursor = Position::new(3, 0);
        a.apply(Action::SetMark('b'));
        a.editor.cursor = Position::new(4, 0);
        // g; lands on 'b' (most recent named mark).
        a.apply(Action::WalkMarkHistoryBack);
        assert_eq!(a.editor.cursor, Position::new(3, 0));
        // g; again -> 'a'.
        a.apply(Action::WalkMarkHistoryBack);
        assert_eq!(a.editor.cursor, Position::new(1, 0));
    }

    #[test]
    fn g_comma_walks_named_mark_history_forward() {
        let mut a = app_with("a\nb\nc\nd\ne", 10);
        a.editor.cursor = Position::new(1, 0);
        a.apply(Action::SetMark('a'));
        a.editor.cursor = Position::new(3, 0);
        a.apply(Action::SetMark('b'));
        a.editor.cursor = Position::new(4, 0);
        a.apply(Action::WalkMarkHistoryBack); // -> 'b'
        a.apply(Action::WalkMarkHistoryBack); // -> 'a'
        a.apply(Action::WalkMarkHistoryForward); // -> 'b'
        assert_eq!(a.editor.cursor, Position::new(3, 0));
    }

    #[test]
    fn g_semicolon_with_no_named_marks_emits_error() {
        let mut a = app_with("a\nb\nc", 10);
        a.editor.cursor = Position::new(2, 0);
        a.apply(invoke_motion(a.editor.builtins.goto_first_line)); // pushes AutoJump
        a.apply(Action::WalkMarkHistoryBack);
        let msg = a.editor.last_message.as_ref().unwrap();
        assert_eq!(msg.level, EchoLevel::Error);
        assert!(msg.text.contains("no marks"));
    }

    // ---- Multiple registers ----

    // ---- ~ toggle case at cursor ----

    // ---- Word-search (* / #) and matching-bracket (%) ----

    #[test]
    fn hash_finds_previous_occurrence_of_word_under_cursor() {
        let mut a = app_with("foo bar foo bar", 10);
        a.editor.cursor = Position::new(0, 8); // on 'f' of second "foo"
        a.apply(Action::SearchWordUnderCursor(SearchDirection::Backward));
        assert_eq!(a.editor.cursor, Position::ZERO);
    }

    // ---- Viewport motions ----

    // ---- Replace mode ----

    #[test]
    fn esc_exits_replace_to_normal_and_pulls_cursor_back() {
        let mut a = app_with("hello", 10);
        a.apply(Action::EnterMode(ModalState::Replace));
        a.apply(Action::OverwriteChar('H'));
        // Cursor at (0,1) after one overwrite.
        a.apply(Action::EnterMode(ModalState::Normal));
        // enter_mode pulls cursor back one byte on Normal entry.
        assert_eq!(a.editor.modal, ModalState::Normal);
        assert_eq!(a.editor.cursor, Position::new(0, 0));
    }

    // ---- Marks ----

    #[test]
    fn invalid_mark_name_emits_error() {
        let mut a = app_with("hello", 10);
        a.apply(Action::SetMark(' '));
        let msg = a.editor.last_message.as_ref().unwrap();
        assert_eq!(msg.level, EchoLevel::Error);
        assert!(a.editor.marks.is_empty());
    }

    #[test]
    fn marks_are_keyed_by_name() {
        let mut a = app_with("hello\nworld", 10);
        a.editor.cursor = Position::new(0, 1);
        a.apply(Action::SetMark('a'));
        a.editor.cursor = Position::new(1, 3);
        a.apply(Action::SetMark('b'));
        a.editor.cursor = Position::ZERO;
        a.apply(Action::JumpToMarkExact('a'));
        assert_eq!(a.editor.cursor, Position::new(0, 1));
        a.apply(Action::JumpToMarkExact('b'));
        assert_eq!(a.editor.cursor, Position::new(1, 3));
    }

    #[test]
    fn uppercase_mark_works_same_as_lowercase_in_v1() {
        // v1 makes no distinction between buffer-local (a-z) and global
        // (A-Z) marks since the TUI runs against a single document.
        let mut a = app_with("hello\nworld", 10);
        a.editor.cursor = Position::new(1, 2);
        a.apply(Action::SetMark('A'));
        a.editor.cursor = Position::ZERO;
        a.apply(Action::JumpToMarkExact('A'));
        assert_eq!(a.editor.cursor, Position::new(1, 2));
    }

    #[test]
    fn jumping_to_mark_with_invalid_name_is_error() {
        let mut a = app_with("hello", 10);
        a.apply(Action::JumpToMarkExact(' '));
        let msg = a.editor.last_message.as_ref().unwrap();
        assert_eq!(msg.level, EchoLevel::Error);
    }

    // ---- Dot-repeat ----

    #[test]
    fn motion_does_not_record_last_change() {
        let mut a = app_with("hello world", 10);
        a.apply(invoke_motion(a.editor.builtins.word_forward));
        assert!(a.editor.last_change.is_none());
    }

    // ---- count prefix end-to-end ----

    #[test]
    fn push_digit_accumulates_pending_count() {
        let mut a = app_with("abc", 10);
        a.apply(Action::PushDigit(1));
        a.apply(Action::PushDigit(2));
        a.apply(Action::PushDigit(3));
        assert_eq!(a.editor.pending_count, 123);
    }

    // Slice 8.i.4.f: the next batch of tests bake `Count(N)`
    // directly into the operator invocation -- mirroring what the
    // input-side `attach_count` produces when the keystroke
    // pipeline runs. The dispatcher's job is now to honour the
    // baked count and drain `pending_count` / `op_count` from
    // App state. The full keystroke -> count pipeline lives in
    // the `key_harness_*` press-harness tests.

    // ---- find / till motions end-to-end ----

    #[test]
    fn fz_jumps_to_z_on_current_line() {
        let mut a = app_with("hello, world", 10);
        let inv = CommandInvocation::of(a.editor.builtins.find_char_forward.0)
            .with_args(lattice_grammar::Args::Char('w'));
        a.apply(Action::Invoke(inv));
        assert_eq!(a.editor.cursor, Position::new(0, 7));
    }

    #[test]
    fn t_lands_one_byte_before_target() {
        let mut a = app_with("hello, world", 10);
        let inv = CommandInvocation::of(a.editor.builtins.till_char_forward.0)
            .with_args(lattice_grammar::Args::Char('w'));
        a.apply(Action::Invoke(inv));
        assert_eq!(a.editor.cursor, Position::new(0, 6));
    }

    #[test]
    fn df_deletes_through_target_char() {
        // From "hello, world" with cursor at 0, `df,` deletes "hello," and
        // leaves " world".
        let mut a = app_with("hello, world", 10);
        let inv = CommandInvocation::of(a.editor.builtins.delete.0).with_target(
            lattice_grammar::Target::Motion(
                a.editor.builtins.find_char_forward,
                lattice_grammar::Args::Char(','),
            ),
        );
        a.apply(Action::Invoke(inv));
        // dispatcher uses [start, end) range; find_char_forward returns the
        // position of the comma (byte 5), so [0, 5) = "hello" is deleted.
        // The trailing comma stays in place.
        assert_eq!(a.document.text(), ", world");
    }

    #[test]
    fn ct_with_change_enters_insert_mode() {
        let mut a = app_with("hello, world", 10);
        let inv = CommandInvocation::of(a.editor.builtins.change.0).with_target(
            lattice_grammar::Target::Motion(
                a.editor.builtins.till_char_forward,
                lattice_grammar::Args::Char(','),
            ),
        );
        a.apply(Action::Invoke(inv));
        assert_eq!(a.editor.modal, ModalState::Insert);
    }

    // ---- yank + paste end-to-end ----

    #[test]
    fn yw_populates_unnamed_register_charwise() {
        let mut a = app_with("hello world", 10);
        let inv = CommandInvocation::of(a.editor.builtins.yank.0).with_target(
            lattice_grammar::Target::Motion(a.editor.builtins.word_forward, lattice_grammar::Args::None),
        );
        a.apply(Action::Invoke(inv));
        let reg = a.editor.unnamed_register.as_ref().unwrap();
        assert_eq!(reg.content, "hello ");
        assert_eq!(reg.kind, YankKind::Charwise);
        // Buffer untouched by yank.
        assert_eq!(a.document.text(), "hello world");
    }

    #[test]
    fn goto_first_line_into_closed_fold_auto_opens() {
        let initial = "# H1\nbody\nbody2\n# H2\nafter\n";
        let mut a = app_with(initial, 10);
        a.set_foldmethod_for_test(FoldMethod::Markdown);
        a.recompute_folds();
        let idx = a
            .folds
            .iter()
            .position(|f| f.start_line == 0)
            .expect("H1 fold");
        a.folds[idx].closed = true;
        // Move cursor away first (so gg is a non-trivial jump).
        a.editor.cursor = Position::new(4, 0);
        let inv = CommandInvocation::of(a.editor.builtins.goto_first_line.0);
        a.apply(Action::Invoke(inv));
        let fold = a
            .folds
            .iter()
            .find(|f| f.start_line == 0)
            .expect("H1 fold still present");
        assert!(!fold.closed, "gg should auto-open the destination fold");
    }

    #[test]
    fn zi_toggles_foldenable_and_renders_folds_open() {
        let mut a = app_with("# H\nbody\n# H2\n", 10);
        a.set_foldmethod_for_test(FoldMethod::Markdown);
        a.recompute_folds();
        let idx = a
            .folds
            .iter()
            .position(|f| f.start_line == 0)
            .expect("H1 fold");
        a.folds[idx].closed = true;
        // Sanity: the fold is closed and visible to the renderer.
        assert!(a.line_inside_closed_fold(1));
        assert!(a.fold_start_at(0).is_some());
        // zi disables.
        a.apply(Action::ToggleFoldEnable);
        assert!(!a.foldenable());
        // With foldenable=false, the renderer sees no closed folds.
        assert!(!a.line_inside_closed_fold(1));
        assert!(a.fold_start_at(0).is_none());
        // zi again re-enables and the closed-state is preserved.
        a.apply(Action::ToggleFoldEnable);
        assert!(a.foldenable());
        assert!(a.line_inside_closed_fold(1));
        assert!(a.fold_start_at(0).is_some());
    }

    #[test]
    fn nofoldenable_disables_fold_aware_operators() {
        let initial = "# H1\nbody one\nbody two\n# H2\nafter\n";
        let mut a = app_with(initial, 10);
        a.set_foldmethod_for_test(FoldMethod::Markdown);
        a.recompute_folds();
        let idx = a
            .folds
            .iter()
            .position(|f| f.start_line == 0)
            .expect("H1 fold");
        a.folds[idx].closed = true;
        a.set_foldenable_for_test(false);
        a.editor.cursor = Position::new(0, 0);
        let inv = CommandInvocation::of(a.editor.builtins.delete.0)
            .with_range(lattice_grammar::Range::CurrentLine);
        a.apply(Action::Invoke(inv));
        // With foldenable=false, dd should affect just one line.
        let text = a.document.text();
        assert!(
            !text.contains("# H1"),
            "heading should be deleted: {text:?}"
        );
        assert!(
            text.contains("body one"),
            "body one should remain: {text:?}"
        );
    }

    #[test]
    fn linear_j_does_not_auto_open_fold() {
        // `docs/user/folding.md`: linear motions (j/k/h/l/w/b) do
        // NOT trigger auto-open. The cursor "skips" over closed
        // folds via `line_inside_closed_fold` filtering -- but the
        // fold itself stays closed. Here we simulate a synthetic
        // cursor move into the fold range to verify the rule.
        let initial = "# H1\nbody\nbody2\n# H2\nafter\n";
        let mut a = app_with(initial, 10);
        a.set_foldmethod_for_test(FoldMethod::Markdown);
        a.recompute_folds();
        let idx = a
            .folds
            .iter()
            .position(|f| f.start_line == 0)
            .expect("H1 fold");
        a.folds[idx].closed = true;
        // Direct cursor move (not via auto-open path).
        a.editor.cursor = Position::new(1, 0);
        let still_closed = a
            .folds
            .iter()
            .find(|f| f.start_line == 0)
            .expect("H1 fold still present");
        assert!(
            still_closed.closed,
            "merely setting cursor should not open folds"
        );
    }

    // ---- Bracketed-paste burst (Action::PasteText) ----

    // ---- Blockwise visual operators (DESIGN.md §15:18) ----

    /// Drive into Blockwise visual at `anchor`, then move the cursor to
    /// `head` so the rectangle is `[anchor, head]`. Returns the App
    /// ready for an operator dispatch.
    // ---- Help overlay (DESIGN.md §5.11) ----

    // ---- Command-line completion (DESIGN.md §5.11.3) ----

    #[test]
    fn accept_completion_replaces_prefix_with_chosen_text() {
        let mut a = app_in_command_mode("descri");
        a.apply(Action::CommandLineCompleteOrAdvance);
        // The accepted candidate uses the user-facing alias form,
        // not the canonical `ex:*` name. The first candidate (after
        // ranking) is one of the describe-* family.
        a.apply(Action::CommandLineAcceptCompletion);
        assert!(
            a.editor.command_line.starts_with("describe-") || a.editor.command_line == "apropos",
            "expected user-facing alias, got `{}`",
            a.editor.command_line
        );
        assert!(a.completion_state.is_none());
    }

    // ---- Chord-capture (DESIGN.md §B.1, ArgKind::Chord) ----

    // ---- Missing-arg chord prompt (DESIGN.md §B.1) ----

    #[test]
    fn cancel_clears_armed_chord_prompt() {
        let mut a = app_in_command_mode("describe-key");
        a.apply(Action::CommandLineSubmit);
        assert!(a.editor.auto_submit_after_chord);
        a.apply(Action::CommandLineCancel);
        assert!(!a.editor.auto_submit_after_chord);
    }

    #[test]
    fn arg_slot_completion_for_describe_command_shows_command_names() {
        // After "describe-command moti", the slot is arg 0 with
        // completion source "gen:commands" -- popup should list
        // motion:* commands.
        let mut a = app_in_command_mode("describe-command moti");
        a.apply(Action::CommandLineCompleteOrAdvance);
        let state = a.completion_state.as_ref().expect("popup");
        assert!(
            state
                .candidates
                .iter()
                .any(|c| c.raw.text.starts_with("motion:")),
            "expected motion:* candidates: {:?}",
            state
                .candidates
                .iter()
                .map(|c| &c.raw.text)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn accept_in_arg_slot_replaces_only_the_arg_prefix() {
        let mut a = app_in_command_mode("describe-command moti");
        a.apply(Action::CommandLineCompleteOrAdvance);
        a.apply(Action::CommandLineAcceptCompletion);
        // Should now be "describe-command motion:..." -- the
        // command word + space preserved; only `moti` replaced.
        assert!(a.editor.command_line.starts_with("describe-command motion:"));
    }

    // ---- Hybrid <C-h> (DESIGN.md §5.11.3 Q11) ----

    // ---- delete_trailing_word helper ----

    // ---- Alias preference for command candidates ----

    #[test]
    fn resolve_command_name_or_alias_handles_both_forms() {
        let mut registry = lattice_grammar::CommandRegistry::new();
        let _ = lattice_grammar::builtins::populate(&mut registry);
        let _ = lattice_grammar::ex_commands::populate(&mut registry);
        // Canonical hits.
        assert!(resolve_command_name_or_alias(&registry, "ex:write").is_some());
        assert!(resolve_command_name_or_alias(&registry, "ex:apropos").is_some());
        assert!(resolve_command_name_or_alias(&registry, "motion:line-down").is_some());
        // Alias hits.
        assert!(resolve_command_name_or_alias(&registry, "w").is_some());
        assert!(resolve_command_name_or_alias(&registry, "apropos").is_some());
        assert!(resolve_command_name_or_alias(&registry, "describe-command").is_some());
        // Misses.
        assert!(resolve_command_name_or_alias(&registry, "nope").is_none());
        assert!(resolve_command_name_or_alias(&registry, "").is_none());
    }

    #[test]
    fn parser_accepts_canonical_name_directly() {
        // Defensive: even if the user types the canonical name
        // (`:ex:describe-command`), the parser resolves it. The
        // assertion: no "unknown command" error message. Whatever
        // happens downstream (e.g. `:ex:write` errors on no file
        // name) is unrelated to parser resolution.
        let mut a = app_in_command_mode("ex:describe-command ex:write");
        a.apply(Action::CommandLineSubmit);
        // Should have opened the help buffer; no "unknown
        // command" error from the parser.
        assert!(
            a.editor.popup_buffer.is_some(),
            "help should open from canonical-name describe-command"
        );
    }

    // ---- completion.auto_insert_single (B + sub-decision (i)) ----
    //
    // Single-candidate auto-insert at popup-open: when `<Tab>` would
    // open a popup with exactly one candidate AND the option is on,
    // skip the popup and apply the candidate to the cmdline directly.
    // Today there's only one completion path (cmdline `:` Tab), so
    // this hook covers `gen:commands`, `gen:options`, and every other
    // arg-slot generator uniformly. When LSP / Insert-mode completion
    // lands (Phase 4.2, task #199), Phase 4.2 should reuse
    // `open_completion_popup` (or factor a shared helper) so this
    // option stays universal without a second knob.

    // ---- Pane tree (DESIGN.md §5.9, B.1.b) ----

    #[test]
    fn focused_hover_does_not_auto_dismiss_on_motion() {
        // State B: cursor is *inside* the popup; motions move the
        // popup's cursor, not the doc's. The State-A auto-dismiss
        // hook is gated on `prev_pane_for_help.is_none()` -- in
        // State B that field is Some, so motion doesn't drop the
        // popup.
        let mut a = app_with("fn main() {}\n", 5);
        a.do_open_hover("line 1\nline 2\nline 3");
        a.do_lsp_hover_request(); // -> State B
        assert!(matches!(a.editor.active_buffer, BufferKind::Help));
        // Move within popup.
        let inv = lattice_grammar::CommandInvocation::of(a.editor.builtins.line_down.0);
        a.apply(Action::Invoke(inv));
        assert!(a.editor.popup_buffer.is_some(), "popup persists in State B");
        assert_eq!(a.editor.cursor.line, 1);
    }

    // ---- Multiple Document buffers (DESIGN.md §5.9, B.1.c) ----

    #[test]
    fn bnext_cycles_through_open_buffers() {
        let path = write_temp_file("b", "one\n");
        let mut a = app_with("xx", 10);
        let first_id = a.editor.document_buffer_id;
        a.editor.command_line = format!("e {}", path.display());
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        let second_id = a.editor.document_buffer_id;
        assert_ne!(first_id, second_id);
        a.editor.command_line = "bn".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert_eq!(a.editor.document_buffer_id, first_id);
        a.editor.command_line = "bn".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert_eq!(a.editor.document_buffer_id, second_id);
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn editing_already_open_path_switches_back_to_it() {
        let path = write_temp_file("d", "alpha\n");
        let mut a = app_with("xx", 10);
        let initial_id = a.editor.document_buffer_id;
        a.editor.command_line = format!("e {}", path.display());
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        let new_id = a.editor.document_buffer_id;
        // Cycle back to first buffer.
        a.editor.command_line = "bn".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert_eq!(a.editor.document_buffer_id, initial_id);
        // Re-editing the new file's path should switch to its
        // existing buffer rather than spawning a third.
        a.editor.command_line = format!("e {}", path.display());
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert_eq!(a.editor.document_buffer_id, new_id);
        // Listed-count gates the "did we accidentally spawn a third?"
        // assertion; synthetic `*lsp*` doesn't count.
        assert_eq!(a.editor.buffers.listed_ids_sorted().len(), 2);
        let _ = std::fs::remove_file(path);
    }

    // ---- Buffer activation lifecycle ----
    //
    // Regression coverage for the `<C-l>`-needed bug: opening a
    // second file via `:e <path>` left the new buffer with empty
    // folds and stale highlight caches because no single hook ran
    // on activation. `App::activate_buffer_state` is now the one
    // place to add buffer-level state that needs to come up with
    // the buffer.

    #[test]
    fn activation_skips_fold_seed_for_manual_foldmethod() {
        // Manual foldmethod => activation must NOT auto-create folds
        // (the user's `zf` ranges are authoritative; auto-seeding
        // would surprise them).
        let path = write_temp_file("activate-manual", "a:\n    x\n    y\nb:\n    p\n    q\n");
        let mut a = app_with("xx", 10);
        a.set_foldmethod_for_test(FoldMethod::Manual);
        a.editor.command_line = format!("e {}", path.display());
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert!(
            a.folds.is_empty(),
            "manual foldmethod should not auto-seed folds: {:?}",
            a.folds
        );
        let _ = std::fs::remove_file(path);
    }

    // ---- File-tree buffer (DESIGN.md §5.9, B.1.d) ----

    // ---- Typed options registry (DESIGN.md §5.12, B.2) ----

    // ---- Hover popup (DESIGN.md §5.9.6, B.3) ----

    // ---- LSP hover (Phase 4.2.b) ----

    // ---- LSP goto-definition (Phase 4.2.c) ----

    /// 4.5.c: `range_covers` returns true for points strictly
    /// inside the range; inclusive on both ends to match the
    /// "click on the rightmost char of a link" UX.
    #[test]
    fn range_covers_inclusive_at_both_ends() {
        let r = lsp_types::Range {
            start: lsp_types::Position {
                line: 2,
                character: 4,
            },
            end: lsp_types::Position {
                line: 2,
                character: 10,
            },
        };
        // Inside.
        assert!(super::range_covers(
            r,
            lsp_types::Position {
                line: 2,
                character: 6
            }
        ));
        // Boundary (inclusive).
        assert!(super::range_covers(r, r.start));
        assert!(super::range_covers(r, r.end));
        // Before start -> miss.
        assert!(!super::range_covers(
            r,
            lsp_types::Position {
                line: 2,
                character: 3
            }
        ));
        // After end -> miss.
        assert!(!super::range_covers(
            r,
            lsp_types::Position {
                line: 2,
                character: 11
            }
        ));
        // Different line outside the range.
        assert!(!super::range_covers(
            r,
            lsp_types::Position {
                line: 1,
                character: 7
            }
        ));
        assert!(!super::range_covers(
            r,
            lsp_types::Position {
                line: 3,
                character: 0
            }
        ));
    }

    /// 4.5.c: `range_covers` works across line boundaries when
    /// the range spans multiple lines.
    #[test]
    fn range_covers_multi_line_range() {
        let r = lsp_types::Range {
            start: lsp_types::Position {
                line: 2,
                character: 4,
            },
            end: lsp_types::Position {
                line: 4,
                character: 8,
            },
        };
        // Mid-range second line: covered regardless of column.
        assert!(super::range_covers(
            r,
            lsp_types::Position {
                line: 3,
                character: 0
            }
        ));
        // First line before start column -> miss.
        assert!(!super::range_covers(
            r,
            lsp_types::Position {
                line: 2,
                character: 3
            }
        ));
        // Last line after end column -> miss.
        assert!(!super::range_covers(
            r,
            lsp_types::Position {
                line: 4,
                character: 9
            }
        ));
    }

    #[test]
    fn symbol_kind_glyph_distinct_for_common_kinds() {
        // We don't assert the *exact* glyph (those may evolve);
        // we just want the common kinds to map to *distinct*
        // glyphs so a glance distinguishes a fn from a struct.
        use lsp_types::SymbolKind as K;
        let f = super::symbol_kind_glyph(K::FUNCTION);
        let s = super::symbol_kind_glyph(K::STRUCT);
        let m = super::symbol_kind_glyph(K::MODULE);
        let v = super::symbol_kind_glyph(K::VARIABLE);
        assert_ne!(f, s);
        assert_ne!(f, m);
        assert_ne!(f, v);
    }

    #[test]
    fn prepare_rename_placeholder_extracted_from_range_with_placeholder() {
        let r = lsp_types::Range {
            start: lsp_types::Position {
                line: 0,
                character: 0,
            },
            end: lsp_types::Position {
                line: 0,
                character: 3,
            },
        };
        let resp = lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: r,
            placeholder: "foo".into(),
        };
        assert_eq!(
            super::prepare_rename_placeholder(&resp),
            Some("foo".to_string())
        );
        let resp = lsp_types::PrepareRenameResponse::Range(r);
        assert_eq!(super::prepare_rename_placeholder(&resp), None);
    }

    #[test]
    fn picker_dismiss_clears_pending_tag_origin() {
        let mut a = app_with("foo\n", 10);
        a.editor.pending_tag_origin = Some(super::TagStackEntry {
            buffer: a.editor.active_buffer,
            buffer_id: a.active_pane_buffer_id(),
            position: Position::new(0, 0),
            label: "foo".into(),
        });
        // Open + dismiss a picker. We don't need real candidates;
        // a non-Some picker dismiss already takes the picker
        // first. Simulate by setting picker Some.
        let mut p = lattice_picker::Picker::new(
            "test",
            lattice_picker::PickerSource::LspLocations,
            lattice_picker::PickerAction::JumpToLspLocation,
        );
        p.set_lsp_locations(Vec::new());
        a.editor.picker = Some(p);
        a.apply(Action::PickerDismiss);
        assert!(a.editor.pending_tag_origin.is_none());
    }

    #[test]
    fn diagnostics_picker_clears_stale_tag_origin() {
        // If a stale nav-intent origin was set (race scenario:
        // gd fired but drain hasn't run; user invokes
        // :diagnostics), opening the diagnostics picker MUST
        // clear the origin so a later JumpToLspLocation accept
        // doesn't push the wrong entry.
        let mut a = app_with("foo\n", 10);
        a.editor.pending_tag_origin = Some(super::TagStackEntry {
            buffer: a.editor.active_buffer,
            buffer_id: a.active_pane_buffer_id(),
            position: Position::new(0, 0),
            label: "stale".into(),
        });
        a.do_list_diagnostics();
        assert!(a.editor.pending_tag_origin.is_none());
    }

    // ---- :help (DESIGN.md §5.11) ----

    #[test]
    fn h_alias_resolves_to_help() {
        let mut a = app_with("xx", 10);
        a.editor.command_line = "h folding".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        let h = a.popup_help().expect("help open");
        assert_eq!(h.title, "help folding");
    }

    #[test]
    fn after_change_user_can_type_and_replacement_lands() {
        let mut a = app_with("hello world", 10);
        let inv = CommandInvocation::of(a.editor.builtins.change.0).with_target(
            lattice_grammar::Target::Motion(a.editor.builtins.word_forward, lattice_grammar::Args::None),
        );
        a.apply(Action::Invoke(inv));
        assert_eq!(a.editor.modal, ModalState::Insert);
        a.apply(Action::Insert("HEY ".into()));
        assert_eq!(a.document.text(), "HEY world");
    }

    // ---- LSP wiring tests (Phase 4.1.i) ---------------------

    #[test]
    fn pathless_document_does_not_register_buffer_uri() {
        // Path-less scratch document -> `App::new` publishes no
        // `Event::DocumentOpened` (well, *publishes one for
        // observability, but with `path: None`*) and registers
        // no `buffer_uris` entry. The attach driver ignores
        // path-less events.
        let app = App::new(Document::from_text("fn main() {}"));
        assert!(app.buffer_uri(app.editor.document_buffer_id).is_none());
    }

    // ---- LSP diagnostic navigation tests (Phase 4.1.d.iv) ----

    /// Helper: seed N diagnostics into the App's LSP layer at
    /// the given lines + map a fake URI to the active buffer.
    // ---- LSP introspection tests (Phase 4.1.g) ---------------

    #[test]
    fn k_chord_is_registered_in_keymap() {
        // `:describe-key K` walks the keymap registry; without an
        // entry there it reports "K is not bound" even though the
        // input translator dispatches K to LspHoverRequest. The
        // registry entry is the source of truth `:describe-key`
        // and `:apropos` consult.
        use crate::keymap::{BindingMode, default_keymap};
        let entries = default_keymap();
        let k = entries
            .iter()
            .find(|e| e.chord == "K" && e.mode == BindingMode::Normal);
        assert!(
            k.is_some(),
            "K should be registered as a Normal-mode binding"
        );
        let entry = k.unwrap();
        assert!(
            entry.doc.to_lowercase().contains("hover"),
            "doc should mention hover, got {:?}",
            entry.doc
        );
    }

    // ---- Edit-dispatch wiring tests (Phase 4.1.i.2) ----------

    /// Path-bearing initial documents publish
    /// `Event::DocumentOpened` from `App::new` and register the
    /// URI eagerly. The attach driver picks the event up off
    /// the bus and submits to the supervisor on the LSP runtime
    /// -- the UI thread never parks. We verify the eager URI
    /// registration here (`buffer_uris` is observable on the
    /// public App API); full driver -> supervisor behaviour
    /// (the publish path itself) is covered in
    /// `lattice_lsp::attach_driver::tests`.
    #[test]
    fn path_bearing_initial_document_registers_uri_eagerly() {
        use std::str::FromStr;
        // Build a Document with a fixed path. We can't use
        // `Document::open` here without I/O; the builder
        // surface (DocumentBuilder::with_path) lets us seed
        // the path directly.
        let path = std::path::PathBuf::from("/tmp/lattice-test/initial.rs");
        let doc = lattice_core::DocumentBuilder::default()
            .with_path(path.clone())
            .with_text("fn main() {}")
            .build();
        let app = App::new(doc);
        let expected = <lattice_lsp::Uri as FromStr>::from_str(
            lattice_lsp::actor::uri_from_path(&path).as_str(),
        )
        .unwrap();
        assert_eq!(
            app.buffer_uri(app.editor.document_buffer_id),
            Some(&expected),
            "path-bearing initial document must register URI eagerly"
        );
    }

    // ---- Snippet host integration (Phase 4.2.g.4) ----

    /// Test helper: attach a freshly-parsed `Syntax` for `lang`
    /// to `a`, wrapped in a [`SyntaxHandle`]. Mirrors the audit

    #[test]
    fn dedup_helper_keeps_first_occurrence_by_text() {
        // Direct unit test on the dedup helper. Ranker has
        // already sorted; we feed in a vec mimicking the
        // post-rank state (highest-ranked entry first per
        // text), confirm the deduped vec keeps the first
        // occurrence and preserves order otherwise.
        use lattice_completion::{
            CandidateKind, MatchScore, RawCandidate, RenderedCandidate, ScoredCandidate, SourceId,
        };
        let mk = |text: &str, source: &str, score: u32| {
            let raw =
                RawCandidate::plain(text, CandidateKind::Plain).with_source(SourceId::new(source));
            RenderedCandidate::from_scored(ScoredCandidate {
                raw,
                score: MatchScore(score),
                match_ranges: Vec::new(),
            })
        };
        let mut rendered = vec![
            mk("outer", "gen:buffer-words", 200),
            mk("alpha", "gen:buffer-words", 180),
            mk("outer", "gen:tree-sitter-symbol", 180),
            mk("beta", "gen:tree-sitter-symbol", 150),
            mk("alpha", "gen:tree-sitter-symbol", 150),
        ];
        super::dedup_rendered_by_text(&mut rendered);
        let texts: Vec<&str> = rendered.iter().map(|c| c.raw.text.as_str()).collect();
        let sources: Vec<&str> = rendered
            .iter()
            .map(|c| c.raw.source.as_ref().map(|s| s.as_str()).unwrap_or(""))
            .collect();
        assert_eq!(texts, vec!["outer", "alpha", "beta"]);
        // Each kept row carries the higher-ranked source's
        // tag (the first occurrence of each text).
        assert_eq!(
            sources,
            vec![
                "gen:buffer-words",
                "gen:buffer-words",
                "gen:tree-sitter-symbol",
            ],
        );
    }

    /// Inject an `InboundApplyEdit` into the App's drain
    /// receiver. Replaces whatever was there; tests start with
    /// an empty receiver so this is fine.

    fn install_lsp_candidate_with_commit_chars(
        a: &mut App,
        text: &str,
        commit_chars: Vec<char>,
        anchor: Position,
    ) {
        let cursor = a.editor.cursor;
        let snap = a.document.snapshot();
        let line = snap.buffer.line(cursor.line).unwrap_or_default();
        let query = line
            .get(anchor.byte as usize..cursor.byte as usize)
            .unwrap_or("")
            .to_string();
        let mut state = lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            anchor,
            cursor,
            query,
        );
        let meta = LspCompletionMeta {
            label: text.to_string(),
            insert_text: text.to_string(),
            filter_text: None,
            sort_text: None,
            detail: None,
            documentation: None,
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            deprecated: false,
            preselect: false,
            commit_characters: commit_chars,
            additional_text_edits: Vec::new(),
            command: None,
            insert_text_format: lsp_types::InsertTextFormat::PLAIN_TEXT,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: lsp_types::CompletionItem::new_simple(text.to_string(), String::new()),
            resolved: false,
        };
        let payload = lattice_lsp::completion::encode_meta(&meta);
        let mut raw =
            lattice_completion::RawCandidate::plain(text, lattice_completion::CandidateKind::Plain)
                .with_source(lattice_completion::SourceId::new(
                    lattice_completion::LSP_COMPLETION_SOURCE_ID,
                ));
        raw.data = lattice_completion::CandidateData::Extension {
            kind_id: LSP_COMPLETION_KIND_ID,
            payload,
        };
        state.raw.push(raw);
        a.refilter_insert_completion(&mut state);
        a.insert_completion = Some(state);
    }

    #[test]
    fn commit_char_in_lsp_item_accepts_then_inserts() {
        let mut a = app_with("foo", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::new(0, 3);
        install_lsp_candidate_with_commit_chars(&mut a, "foo", vec!['.', '('], Position::new(0, 0));
        a.do_completion_accept_then_insert('.');
        // Popup closed; accept replaced the partial with the
        // full LSP insert, then `.` was appended.
        assert!(a.insert_completion.is_none(), "popup closed on commit");
        assert_eq!(a.document.snapshot().buffer.as_string(), "foo.");
    }

    #[test]
    fn non_commit_char_is_plain_insert_popup_refilters() {
        let mut a = app_with("foo", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::new(0, 3);
        install_lsp_candidate_with_commit_chars(&mut a, "foo", vec!['.'], Position::new(0, 0));
        a.do_completion_accept_then_insert('a');
        // `a` isn't a commit char -> the focused candidate
        // wasn't accepted; `a` was inserted plainly. The
        // refresh hook closes the popup because the new
        // query "fooa" no longer matches the candidate
        // "foo" prefix-wise (matcher returns no rows).
        assert_eq!(a.document.snapshot().buffer.as_string(), "fooa");
    }

    #[test]
    fn extra_commit_chars_option_contributes_globally() {
        let mut a = app_with("foo", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::new(0, 3);
        // Server says no commit chars; the global option
        // adds `,`.
        install_lsp_candidate_with_commit_chars(&mut a, "foo", Vec::new(), Position::new(0, 0));
        a.do_set("completion.extra_commit_chars=,");
        a.do_completion_accept_then_insert(',');
        assert!(a.insert_completion.is_none());
        assert_eq!(a.document.snapshot().buffer.as_string(), "foo,");
    }

    #[test]
    fn sync_candidate_honors_extra_commit_chars_only() {
        // A buffer-words candidate has no per-item commit
        // list (sync sources don't carry one). The global
        // extras still apply.
        let mut a = app_with("alpha bravo ", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::new(0, 12);
        a.do_completion_trigger();
        // Server-supplied list is empty for sync candidates;
        // set the global extras to include `;`.
        a.do_set("completion.extra_commit_chars=;");
        // Focus the `alpha` candidate (insert at cursor).
        if let Some(state) = a.insert_completion.as_mut() {
            state.selected = state
                .rendered
                .iter()
                .position(|r| r.raw.text == "alpha")
                .expect("alpha");
        }
        a.do_completion_accept_then_insert(';');
        // Popup closed; `alpha` inserted then `;`.
        assert!(a.insert_completion.is_none());
        let text = a.document.snapshot().buffer.as_string();
        assert!(text.ends_with("alpha;"), "got `{text}`");
    }

    #[test]
    fn populate_insert_completion_sync_drops_disabled_source() {
        // Inject a per-language override that limits rust to
        // snippets only -> buffer-words emit is suppressed even
        // though the buffer is full of word-completion fodder.
        let mut a = app_with("foo bar baz qux quux ", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::new(0, 21);
        // Pretend the active language is rust by overriding
        // the `rust` slot. (Test buffer has no path so
        // active_language_id() returns ""; insert that as the
        // key directly to land the override.)
        a.per_language_completion.insert(
            String::new(),
            lattice_completion::PerLanguageOverrides {
                sources: Some(vec![lattice_completion::SourceId::new(
                    lattice_completion::SNIPPET_SOURCE_ID,
                )]),
                ..Default::default()
            },
        );
        a.do_completion_trigger();
        // Popup either closed (no candidates) or has only
        // snippet items. Buffer-words mustn't appear.
        if let Some(state) = a.insert_completion.as_ref() {
            for cand in &state.rendered {
                let src = cand.raw.source.as_ref().map(|s| s.as_str()).unwrap_or("");
                assert_ne!(
                    src,
                    lattice_completion::BufferWordsSource::ID,
                    "buffer-words filtered out",
                );
            }
        }
    }

    // ---- M.3.0: built-in major modes registered at boot ----

    // ---- M.3.1: ReadOnly option flows from major modes ----

    #[test]
    fn document_buffer_active_mode_is_text_mode() {
        // Plain document with no path ⇒ Lang::Plain ⇒ text-mode.
        let a = app_with("hi", 5);
        let buf = a.editor.document_buffer_id;
        let active = a.editor.active_modes.get(&buf).expect("active_modes populated");
        assert_eq!(active.major(), Some(lattice_mode::TextMode::mode_id()));
    }

    // ---- M.3.2.b.1: help-mode locals seeded at construction ----

    #[test]
    fn renderer_reads_help_data_through_buffer_locals() {
        // M.3.2.c.5: BufferLocals are the canonical owner of help
        // per-buffer state -- the HelpBuffer struct no longer
        // carries `links` / `anchors` / `highlights` fields. Open
        // a help buffer (which seeds two parsed links into
        // locals), then mutate the locals; readers must reflect
        // the mutation since there's no struct-field fallback.
        let mut a = app_with("hi", 5);
        let help = crate::help::HelpContent::from_lines(
            "test-render",
            vec!["[link-a](command:a) and [link-b](command:b)".into()],
        );
        let help_id = a.open_help_in_pane(help);

        let synthetic = crate::modes::HelpLinks(vec![crate::help::HelpLink {
            range: lattice_protocol::position::Range::new(
                lattice_protocol::position::Position::ZERO,
                lattice_protocol::position::Position::new(0, 5),
            ),
            target: crate::help::HelpLinkTarget::Unresolved("synthetic".into()),
        }]);
        a.editor.buffer_locals
            .get_mut(&help_id)
            .expect("locals seeded")
            .insert(synthetic);

        let _ = a.popup_help().expect("popup_buffer set");
        let locals = a
            .editor.buffer_locals
            .get(&help_id)
            .expect("locals seeded by open_help_in_pane");
        let from_locals = locals.get::<crate::modes::HelpLinks>().unwrap();
        assert_eq!(from_locals.0.len(), 1);
        assert_eq!(
            from_locals.0[0].target,
            crate::help::HelpLinkTarget::Unresolved("synthetic".into())
        );
    }

    // ---- M.3.2.c.2: file-tree-mode locals seeded + readers ----

    // ---- M.3.2.c.3: oil-mode locals seeded ----

    #[test]
    fn list_registers_with_no_state_says_so() {
        let mut a = app_with("hello", 10);
        submit_ex(&mut a, "reg");
        let msg = a.editor.last_message.as_ref().unwrap();
        assert!(msg.text.contains("no registers"));
    }

    #[test]
    fn list_registers_includes_unnamed_and_zero() {
        let mut a = app_with("hello world", 10);
        let inv = CommandInvocation::of(a.editor.builtins.yank.0).with_target(
            lattice_grammar::Target::Motion(a.editor.builtins.word_forward, lattice_grammar::Args::None),
        );
        a.apply(Action::Invoke(inv));
        submit_ex(&mut a, "reg");
        let msg = a.editor.last_message.as_ref().unwrap();
        assert!(msg.text.contains("\"\""));
        assert!(msg.text.contains("\"0"));
    }

    #[test]
    fn list_marks_with_no_marks_says_so() {
        let mut a = app_with("hello", 10);
        submit_ex(&mut a, "marks");
        let msg = a.editor.last_message.as_ref().unwrap();
        assert!(msg.text.contains("no marks"));
    }

    #[test]
    fn list_marks_shows_set_marks() {
        let mut a = app_with("hello\nworld", 10);
        a.editor.cursor = Position::new(1, 2);
        a.apply(Action::SetMark('a'));
        submit_ex(&mut a, "marks");
        let msg = a.editor.last_message.as_ref().unwrap();
        assert!(msg.text.contains('a'));
        // Line 2 (1-indexed for display) at byte 2.
        assert!(msg.text.contains("2:2"));
    }

    #[test]
    fn global_delete_matching_lines() {
        let mut a = app_with("foo\nbar\nfoo\nbaz", 10);
        submit_ex(&mut a, "g/foo/d");
        // Both "foo" lines deleted; "bar" and "baz" remain.
        assert_eq!(a.document.text(), "bar\nbaz");
    }

    #[test]
    fn vglobal_delete_non_matching_lines() {
        let mut a = app_with("foo\nbar\nfoo\nbaz", 10);
        submit_ex(&mut a, "v/foo/d");
        // Only "foo" lines remain.
        assert_eq!(a.document.text(), "foo\nfoo");
    }

    #[test]
    fn global_substitute_on_matching_lines() {
        let mut a = app_with("foo\nbaz\nfoo", 10);
        submit_ex(&mut a, "g/foo/s/foo/X/");
        // Both "foo" lines get substituted.
        assert_eq!(a.document.text(), "X\nbaz\nX");
    }

    #[test]
    fn global_no_matches_emits_error() {
        let mut a = app_with("hello\nworld", 10);
        submit_ex(&mut a, "g/xyz/d");
        let msg = a.editor.last_message.as_ref().unwrap();
        assert_eq!(msg.level, EchoLevel::Error);
    }

    #[test]
    fn capital_w_skips_punctuation() {
        let mut a = app_with("foo,bar baz", 10);
        a.apply(invoke_motion(a.editor.builtins.big_word_forward));
        assert_eq!(a.editor.cursor, Position::new(0, 8));
    }

    #[test]
    fn fold_action_clears_partial_chord() {
        let mut a = app_with("a\nb\nc", 10);
        a.apply(Action::AbsorbPartialChord(crate::chord::KeyChord::char(
            'z',
        )));
        a.apply(Action::OpenFoldAtCursor);
        assert!(a.editor.partial_chord.is_empty());
    }

    #[test]
    fn yank_with_named_register_stores_into_named_and_unnamed() {
        let mut a = app_with("hello world", 10);
        a.apply(Action::SelectRegister(Register::Named('a')));
        let inv = CommandInvocation::of(a.editor.builtins.yank.0).with_target(
            lattice_grammar::Target::Motion(a.editor.builtins.word_forward, lattice_grammar::Args::None),
        );
        a.apply(Action::Invoke(inv));
        // Named slot populated.
        let named = a.editor.registers.get(&Register::Named('a')).unwrap();
        assert_eq!(named.content, "hello ");
        // Unnamed also populated.
        assert_eq!(a.editor.unnamed_register.as_ref().unwrap().content, "hello ");
        // Pending register consumed.
        assert!(a.editor.pending_register.is_none());
    }

    #[test]
    fn yank_auto_populates_zero_register() {
        let mut a = app_with("hello world", 10);
        let inv = CommandInvocation::of(a.editor.builtins.yank.0).with_target(
            lattice_grammar::Target::Motion(a.editor.builtins.word_forward, lattice_grammar::Args::None),
        );
        a.apply(Action::Invoke(inv));
        let zero = a.editor.registers.get(&Register::Numbered(0)).unwrap();
        assert_eq!(zero.content, "hello ");
    }

    #[test]
    fn yank_does_not_record_last_change() {
        let mut a = app_with("hello world", 10);
        let inv = CommandInvocation::of(a.editor.builtins.yank.0).with_target(
            lattice_grammar::Target::Motion(a.editor.builtins.word_forward, lattice_grammar::Args::None),
        );
        a.apply(Action::Invoke(inv));
        // Yank doesn't mutate the buffer; dot-repeat shouldn't pick this up.
        assert!(a.editor.last_change.is_none());
    }

    #[test]
    fn change_records_last_change() {
        let mut a = app_with("hello world", 10);
        let inv = CommandInvocation::of(a.editor.builtins.change.0).with_target(
            lattice_grammar::Target::Motion(a.editor.builtins.word_forward, lattice_grammar::Args::None),
        );
        a.apply(Action::Invoke(inv));
        // change drops to Insert, but the change itself is recorded.
        assert!(a.editor.last_change.is_some());
    }

    #[test]
    fn capital_f_jumps_backward() {
        let mut a = app_with("hello, world", 10);
        a.editor.cursor = Position::new(0, 11); // on 'd'
        let inv = CommandInvocation::of(a.editor.builtins.find_char_backward.0)
            .with_args(lattice_grammar::Args::Char('h'));
        a.apply(Action::Invoke(inv));
        assert_eq!(a.editor.cursor, Position::ZERO);
    }

    fn app_in_command_mode(line: &str) -> App {
        let mut a = app_with("xx", 10);
        a.editor.modal = ModalState::Command;
        a.editor.command_line = line.into();
        a
    }

    #[test]
    fn dismiss_completion_keeps_command_line_intact() {
        let mut a = app_in_command_mode("descri");
        a.apply(Action::CommandLineCompleteOrAdvance);
        a.apply(Action::CommandLineDismissCompletion);
        assert_eq!(a.editor.command_line, "descri");
        assert!(a.completion_state.is_none());
    }

    #[test]
    fn clear_with_open_popup_widens_to_all_commands() {
        let mut a = app_in_command_mode("descri");
        a.apply(Action::CommandLineCompleteOrAdvance);
        let narrow_count = a.completion_state.as_ref().unwrap().candidates.len();
        a.apply(Action::CommandLineClear);
        assert!(a.completion_state.is_some());
        assert_eq!(a.editor.command_line, "");
        let widened = a.completion_state.as_ref().unwrap().candidates.len();
        assert!(widened >= narrow_count);
    }

    #[test]
    fn append_chord_concatenates_token() {
        let mut a = app_in_command_mode("describe-key ");
        a.apply(Action::CommandLineAppendChord("<C-c>".into()));
        assert_eq!(a.editor.command_line, "describe-key <C-c>");
    }

    #[test]
    fn append_chord_supports_multi_token_sequences() {
        // gg / <C-w>j -- multi-stroke chords. Each press appends
        // its own token.
        let mut a = app_in_command_mode("describe-key ");
        a.apply(Action::CommandLineAppendChord("g".into()));
        a.apply(Action::CommandLineAppendChord("g".into()));
        assert_eq!(a.editor.command_line, "describe-key gg");
    }

    #[test]
    fn close_last_pane_is_a_noop_with_warning() {
        let mut a = app_with("xx", 10);
        a.apply(Action::ClosePane);
        assert_eq!(a.pane_tree.len(), 1);
        let msg = a.editor.last_message.as_ref().expect("warn echo");
        assert!(msg.text.contains("only one pane"));
    }

    #[test]
    fn dismiss_focused_hover_restores_doc_cursor() {
        // Esc / q in State B routes to HelpDismiss, which restores
        // the pre-State-B cursor / scroll on the doc.
        let mut a = app_with("fn main() {}\nlet x = 1;\n", 5);
        a.editor.cursor = lattice_protocol::Position::new(1, 4);
        a.do_open_hover("hover body");
        a.do_lsp_hover_request(); // -> State B
        // Move inside the popup.
        let inv = lattice_grammar::CommandInvocation::of(a.editor.builtins.line_down.0);
        a.apply(Action::Invoke(inv));
        assert!(matches!(a.editor.active_buffer, BufferKind::Help));
        // Dismiss.
        a.apply(Action::HelpDismiss);
        assert!(a.editor.popup_buffer.is_none());
        assert!(matches!(a.editor.active_buffer, BufferKind::Document));
        assert_eq!(a.editor.cursor, lattice_protocol::Position::new(1, 4));
        assert!(a.editor.prev_pane_for_help.is_none());
    }

    #[test]
    fn opening_help_in_pane_keeps_document_syntax_live() {
        // Bug: opening `:lsp-log` (which routes through
        // `open_help_in_pane`) stashed the document's syntax onto
        // the registry entry, leaving `self.editor.syntax = None` for the
        // duration of the help session. The help buffer renders as
        // a popup overlay over the underlying document; the
        // document paint reads `self.editor.syntax`, so the document
        // appeared unhighlighted under the popup.
        //
        // Fix: `activate_help_in_pane` does NOT call
        // `snapshot_active_document`. Hot-path state stays live;
        // the round-trip back via `activate_document` early-returns
        // for the same-doc case and skips the restore (entry has
        // nothing to give).
        let mut a = app_with("fn main() {}\n", 10);
        a.editor.terminal_width = Some(80);
        attach_test_syntax(&mut a, lattice_syntax::Lang::Rust);
        assert!(a.editor.syntax.is_some(), "fixture syntax wired");
        // Open a help buffer in pane (mimics `:lsp-log rust`).
        let _help_id =
            a.open_help_in_pane(HelpContent::from_lines("lsp:rust", vec!["log line".into()]));
        assert!(matches!(a.editor.active_buffer, BufferKind::Help));
        // The document's syntax must remain on the hot path so the
        // pane underneath paints with highlights.
        assert!(
            a.editor.syntax.is_some(),
            "syntax must stay live during help-in-pane overlay"
        );
        // Round-trip back to the document.
        let doc_id = a.editor.buffers.document_ids_sorted().first().copied().unwrap();
        a.activate_document(doc_id);
        assert!(matches!(a.editor.active_buffer, BufferKind::Document));
        assert!(
            a.editor.syntax.is_some(),
            "syntax must survive the help-in-pane round trip"
        );
    }

    #[test]
    fn dismissing_tree_preserves_document_syntax_state() {
        // Regression: opening `:Tree` and pressing `q` to dismiss
        // it returned to the document with `self.editor.syntax = None`,
        // so the renderer fell back to plain text (no
        // colours). Cause: the on-tree-open snapshot moved syntax
        // into the document entry, then activate_document on
        // dismiss called snapshot_active_document again and
        // overwrote the entry's stashed syntax with None.
        let dir = std::env::temp_dir().join(format!("lattice-tree-syntax-{}", std::process::id()));
        std::fs::create_dir_all(&dir).ok();
        let mut a = app_with("fn main() {}\n", 10);
        a.editor.terminal_width = Some(80);
        // Wire up a Rust syntax instance so there's something to lose.
        attach_test_syntax(&mut a, lattice_syntax::Lang::Rust);
        // Open the tree, then dismiss.
        a.editor.command_line = format!("Filetree {}", dir.display());
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert!(matches!(
            a.editor.active_buffer,
            crate::buffers::BufferKind::FileTree
        ));
        // `:TreeClose` (the path `q` takes in the tree).
        a.editor.command_line = "FiletreeClose".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert!(matches!(
            a.editor.active_buffer,
            crate::buffers::BufferKind::Document
        ));
        assert!(
            a.editor.syntax.is_some(),
            "syntax must survive the tree round-trip"
        );
        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn close_tree_pane_keeps_tree_in_registry() {
        // Trees now live in the unified buffer registry; closing
        // the only pane that referenced one leaves the tree
        // accessible via `:bn` / `:bp` / `:b N`. Use `:bd` to
        // actually drop it.
        let dir = std::env::temp_dir().join(format!("lattice-tree-gc-{}", std::process::id()));
        std::fs::create_dir_all(&dir).ok();
        let mut a = app_with("xx", 10);
        a.editor.terminal_width = Some(80);
        a.apply(Action::SplitPaneVertical);
        a.apply(Action::NavigatePane(PaneDirection::Right));
        a.editor.command_line = format!("Filetree {}", dir.display());
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert_eq!(a.editor.buffers.file_tree_ids_sorted().len(), 1);
        a.apply(Action::ClosePane);
        // Tree stays in the registry post-close.
        assert_eq!(a.editor.buffers.file_tree_ids_sorted().len(), 1);
        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn bdelete_closes_active_buffer_and_picks_a_successor() {
        let path = write_temp_file("e", "alpha\n");
        let mut a = app_with("xx", 10);
        let initial_id = a.editor.document_buffer_id;
        a.editor.command_line = format!("e {}", path.display());
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        // Now active = new buffer; delete it. Successor should
        // be initial_id.
        a.editor.command_line = "bd".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert_eq!(a.editor.document_buffer_id, initial_id);
        // Listed-count: only the original document remains in the
        // user-facing cycle; the synthetic `*lsp*` is unlisted.
        assert_eq!(a.editor.buffers.listed_ids_sorted().len(), 1);
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn bdelete_only_buffer_is_rejected() {
        let mut a = app_with("xx", 10);
        a.editor.command_line = "bd".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        // The listed count gates the "only buffer" check.
        // Synthetic unlisted buffers (`*lsp*`, ...) don't count as
        // switch destinations, so the rejection still fires when
        // only one user-listed buffer remains.
        assert_eq!(a.editor.buffers.listed_ids_sorted().len(), 1);
        let msg = a.editor.last_message.as_ref().expect("error echo");
        assert!(msg.text.contains("only buffer"));
    }

    #[test]
    fn opening_new_file_seeds_folds_for_indent_foldmethod() {
        // foldmethod=indent on the initial buffer; then `:e <new>`
        // should populate folds for the new buffer without requiring
        // a manual `<C-l>` redraw.
        let path = write_temp_file(
            "activate-folds-indent",
            "a:\n    x\n    y\nb:\n    p\n    q\n",
        );
        let mut a = app_with("xx", 10);
        a.set_foldmethod_for_test(FoldMethod::Indent);
        a.editor.command_line = format!("e {}", path.display());
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        // The new buffer should have folds without `<C-l>`.
        assert!(
            !a.folds.is_empty(),
            "expected folds to be seeded on activation, got empty"
        );
        assert!(
            a.folds.iter().any(|f| f.start_line == 0),
            "expected a fold starting at line 0: {:?}",
            a.folds
        );
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn docs_toggle_pulls_body_from_cached_metadata_documentation() {
        let mut a = app_with("xx", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::ZERO;
        // Seed popup state with a single LSP candidate that
        // already has documentation cached.
        let mut state = lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            Position::ZERO,
            Position::ZERO,
            String::new(),
        );
        let meta = super::LspCompletionMeta {
            label: "foo".into(),
            insert_text: "foo".into(),
            filter_text: None,
            sort_text: None,
            detail: Some("fn foo() -> i32".into()),
            documentation: Some("Returns 42.".into()),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            deprecated: false,
            preselect: false,
            commit_characters: Vec::new(),
            additional_text_edits: Vec::new(),
            command: None,
            insert_text_format: lsp_types::InsertTextFormat::PLAIN_TEXT,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: lsp_types::CompletionItem::default(),
            resolved: true,
        };
        let mut raw = lattice_completion::RawCandidate::plain(
            "foo",
            lattice_completion::CandidateKind::Plain,
        );
        raw.display = "foo".into();
        raw.data = lattice_completion::CandidateData::Extension {
            kind_id: super::LSP_COMPLETION_KIND_ID,
            payload: lattice_lsp::completion::encode_meta(&meta),
        };
        let scored = lattice_completion::ScoredCandidate {
            raw,
            score: lattice_completion::MatchScore(100),
            match_ranges: Vec::new(),
        };
        state
            .rendered
            .push(lattice_completion::RenderedCandidate::from_scored(scored));
        // CSM.8b.5: candidate payload IS the meta -- no sidecar push.
        let _ = meta;
        a.insert_completion = Some(state);
        a.do_completion_toggle_docs();
        let body = a
            .insert_completion
            .as_ref()
            .and_then(|s| s.doc_popup.as_ref())
            .and_then(|d| d.body.clone())
            .expect("body populated");
        assert!(body.contains("fn foo() -> i32"));
        assert!(body.contains("Returns 42."));
    }

    #[test]
    fn docs_toggle_a_second_time_closes_popup() {
        let mut a = app_with("xx", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::ZERO;
        a.insert_completion = Some(lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            Position::ZERO,
            Position::ZERO,
            String::new(),
        ));
        a.do_completion_toggle_docs();
        // Even with no candidate, the popup opens with an
        // empty body slot. Toggling again closes it.
        let was_open = a
            .insert_completion
            .as_ref()
            .map(|s| s.doc_popup.is_some())
            .unwrap_or(false);
        assert!(was_open);
        a.do_completion_toggle_docs();
        let now_closed = a
            .insert_completion
            .as_ref()
            .map(|s| s.doc_popup.is_none())
            .unwrap_or(true);
        assert!(now_closed);
    }

    #[test]
    fn docs_scroll_clamps_at_zero_and_advances_by_eight() {
        let mut a = app_with("xx", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::ZERO;
        a.insert_completion = Some(lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            Position::ZERO,
            Position::ZERO,
            String::new(),
        ));
        a.do_completion_toggle_docs();
        // Default scroll is 0; up clamps at 0.
        assert_eq!(
            a.insert_completion
                .as_ref()
                .and_then(|s| s.doc_popup.as_ref())
                .map(|d| d.scroll),
            Some(0)
        );
        a.apply(Action::CompletionDocsScrollUp);
        assert_eq!(
            a.insert_completion
                .as_ref()
                .and_then(|s| s.doc_popup.as_ref())
                .map(|d| d.scroll),
            Some(0)
        );
        a.apply(Action::CompletionDocsScrollDown);
        assert_eq!(
            a.insert_completion
                .as_ref()
                .and_then(|s| s.doc_popup.as_ref())
                .map(|d| d.scroll),
            Some(8)
        );
        a.apply(Action::CompletionDocsScrollDown);
        assert_eq!(
            a.insert_completion
                .as_ref()
                .and_then(|s| s.doc_popup.as_ref())
                .map(|d| d.scroll),
            Some(16)
        );
        a.apply(Action::CompletionDocsScrollUp);
        assert_eq!(
            a.insert_completion
                .as_ref()
                .and_then(|s| s.doc_popup.as_ref())
                .map(|d| d.scroll),
            Some(8)
        );
    }

    #[test]
    fn follow_link_source_opens_file_at_line() {
        // `:describe-command :lsp-trace` (and similar) renders a
        // `[<source>](file:PATH:LINE)` link. Following it should
        // open the file via the multi-buffer machinery and
        // position the cursor at the requested line. Pre-fix this
        // arm just echoed "(file open arrives with multi-buffer)"
        // -- we already had multi-buffer; the placeholder was
        // stale.
        let path = std::env::temp_dir().join(format!("lattice-srclink-{}.rs", std::process::id()));
        std::fs::write(&path, "first\nsecond\nthird\nfourth\n").unwrap();
        let mut a = app_with("xx", 10);
        // Open a help buffer so the active modal/buffer state
        // matches what `FollowLink` expects.
        a.editor.command_line = "help".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        // Build a synthetic source link inside the help buffer.
        // 1-based line number: line 3 in the file → cursor at
        // line index 2 in the buffer.
        let link = crate::help::HelpLink {
            range: lattice_protocol::Range::new(
                lattice_protocol::Position::ZERO,
                lattice_protocol::Position::new(0, 1),
            ),
            target: crate::help::HelpLinkTarget::Source {
                path: path.clone(),
                line: 3,
            },
        };
        // M.3.2.c.5: production reads route through buffer_locals;
        // seed the synthetic link there directly. The locals key
        // is the popup buffer's construction id (centred-popup
        // resolution rule).
        let buf_id = a.editor.popup_buffer.unwrap();
        a.with_popup_help_mut(|h| {
            h.cursor = lattice_protocol::Position::ZERO;
        });
        let mut existing_links = a
            .editor.buffer_locals
            .get(&buf_id)
            .and_then(|l| l.get::<crate::modes::HelpLinks>())
            .map(|l| l.0.clone())
            .unwrap_or_default();
        existing_links.push(link);
        a.editor.buffer_locals
            .entry(buf_id)
            .or_default()
            .insert(crate::modes::HelpLinks(existing_links));
        a.editor.active_buffer = BufferKind::Help;
        a.apply(Action::FollowLink);
        // The file should now be the active document.
        assert_eq!(a.editor.active_buffer, BufferKind::Document);
        let opened = a.document.path().expect("active doc has a path");
        assert_eq!(opened, path);
        // Cursor at line index 2 (1-based 3 → 0-based 2).
        assert_eq!(a.editor.cursor.line, 2);
        // NOTE: a `PluginPush` history entry is pushed *before*
        // `do_edit` runs, but `do_edit`'s new-file branch clears
        // the position history (so a fresh buffer's `<C-o>` doesn't
        // walk into the previous buffer's positions). That means
        // cross-buffer jumps from FollowLink and from
        // `jump_to_lsp_location` currently lose their walk-back
        // entry. Per-buffer position history is queued as the
        // proper fix; for now this test asserts the open-and-jump
        // primary behaviour and lets the history side-effect
        // regress until that fix lands.
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn follow_link_source_clamps_line_past_eof() {
        let path =
            std::env::temp_dir().join(format!("lattice-srclink-clamp-{}.rs", std::process::id()));
        std::fs::write(&path, "only-line\n").unwrap();
        let mut a = app_with("xx", 10);
        a.editor.command_line = "help".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        let link = crate::help::HelpLink {
            range: lattice_protocol::Range::new(
                lattice_protocol::Position::ZERO,
                lattice_protocol::Position::new(0, 1),
            ),
            target: crate::help::HelpLinkTarget::Source {
                path: path.clone(),
                line: 999,
            },
        };
        // M.3.2.c.5: production reads route through buffer_locals;
        // seed the synthetic link there directly. The locals key
        // is the popup buffer's construction id (centred-popup
        // resolution rule).
        let buf_id = a.editor.popup_buffer.unwrap();
        a.with_popup_help_mut(|h| {
            h.cursor = lattice_protocol::Position::ZERO;
        });
        let mut existing_links = a
            .editor.buffer_locals
            .get(&buf_id)
            .and_then(|l| l.get::<crate::modes::HelpLinks>())
            .map(|l| l.0.clone())
            .unwrap_or_default();
        existing_links.push(link);
        a.editor.buffer_locals
            .entry(buf_id)
            .or_default()
            .insert(crate::modes::HelpLinks(existing_links));
        a.editor.active_buffer = BufferKind::Help;
        a.apply(Action::FollowLink);
        // Out-of-range line should clamp to the last valid line,
        // not panic and not echo a confusing error.
        let last_line = a.document.snapshot().buffer.line_count().saturating_sub(1);
        assert_eq!(a.editor.cursor.line, last_line);
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn yank_then_paste_round_trips_word() {
        let mut a = app_with("hello world", 10);
        let yank = CommandInvocation::of(a.editor.builtins.yank.0).with_target(
            lattice_grammar::Target::Motion(a.editor.builtins.word_forward, lattice_grammar::Args::None),
        );
        a.apply(Action::Invoke(yank));
        // Move cursor to end of buffer.
        a.editor.cursor = Position::new(0, 11);
        a.apply(Action::PasteAfter);
        assert_eq!(a.document.text(), "hello worldhello ");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn open_lsp_log_in_pane_renders_per_server_records() {
        // Slice B: per-server log lives as a Document in the
        // registry; records arrive through the event-bus drain
        // (`drain_lsp_log_events`) rather than a snapshot rebuild.
        // B'.4: use cwd-backed instance so it matches
        // `resolve_lsp_instance_for("rust")`'s no-actor fallback.
        let mut app = app_with("hi\n", 5);
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(
                std::env::current_dir()
                    .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                    .as_path(),
            ),
        );
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Warn,
            lattice_lsp::LogSource::Stderr,
            "compile error",
        );
        // `open_lsp_log_in_pane` drains queued events first so the
        // buffer reflects pre-existing records.
        app.open_lsp_log_in_pane("rust");
        // Wait for LspServerLogMode's tokio task to drain the
        // pre-open record.
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        let log_id = app
            .editor.buffers
            .by_name(&lattice_lsp::lsp_server_log_name(&instance))
            .expect("per-instance log buffer registered");
        assert_eq!(app.active_pane_buffer_id(), log_id);
        let body = app
            .editor.buffers
            .document_handle(log_id)
            .expect("log buffer is a Document")
            .text();
        assert!(body.contains("compile error"), "got `{body}`");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn open_lsp_log_in_pane_excludes_trace_records() {
        let mut app = app_with("hi\n", 5);
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(
                std::env::current_dir()
                    .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                    .as_path(),
            ),
        );
        app.editor.lsp_logger.enable_trace(instance.clone());
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Trace,
            lattice_lsp::LogSource::Trace,
            "→ Request id=1",
        );
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Info,
            lattice_lsp::LogSource::Client,
            "lifecycle",
        );
        app.open_lsp_log_in_pane("rust");
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        let log_id = app
            .editor.buffers
            .by_name(&lattice_lsp::lsp_server_log_name(&instance))
            .expect("per-instance log buffer registered");
        let body = app.editor.buffers.document_handle(log_id).unwrap().text();
        // Trace records route to the trace buffer; non-trace
        // records (including lifecycle) land here.
        assert!(!body.contains("→ Request"), "got `{body}`");
        assert!(body.contains("lifecycle"), "got `{body}`");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn open_lsp_trace_log_in_pane_shows_only_trace_records() {
        let mut app = app_with("hi\n", 5);
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(
                std::env::current_dir()
                    .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                    .as_path(),
            ),
        );
        app.editor.lsp_logger.enable_trace(instance.clone());
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Trace,
            lattice_lsp::LogSource::Trace,
            "→ Request id=1",
        );
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Info,
            lattice_lsp::LogSource::Client,
            "lifecycle",
        );
        app.open_lsp_trace_log_in_pane("rust");
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        let trace_id = app
            .editor.buffers
            .by_name(&lattice_lsp::lsp_server_trace_log_name(&instance))
            .expect("per-instance trace buffer registered");
        let body = app.editor.buffers.document_handle(trace_id).unwrap().text();
        // Trace records here; non-trace records routed to the
        // matching `*lsp:rust:<ws>*` buffer instead.
        assert!(body.contains("→ Request"), "got `{body}`");
        assert!(!body.contains("lifecycle"), "got `{body}`");
    }

    #[test]
    fn app_boot_registers_every_built_in_major_mode() {
        let a = app_with("hi", 5);
        // Foundation
        assert!(
            a.editor.mode_registry
                .is_registered(lattice_mode::TextMode::mode_id())
        );
        // Languages (lattice-syntax)
        assert!(
            a.editor.mode_registry
                .is_registered(lattice_syntax::RustMode::mode_id())
        );
        assert!(
            a.editor.mode_registry
                .is_registered(lattice_syntax::PythonMode::mode_id())
        );
        assert!(
            a.editor.mode_registry
                .is_registered(lattice_syntax::JavascriptMode::mode_id())
        );
        assert!(
            a.editor.mode_registry
                .is_registered(lattice_syntax::MarkdownMode::mode_id())
        );
        // Buffer-kind majors (lattice-ui-tui)
        assert!(
            a.editor.mode_registry
                .is_registered(crate::modes::HelpMode::mode_id())
        );
        assert!(
            a.editor.mode_registry
                .is_registered(crate::modes::FileTreeMode::mode_id())
        );
        assert!(
            a.editor.mode_registry
                .is_registered(crate::modes::OilMode::mode_id())
        );
        // LSP log majors (lattice-lsp)
        assert!(
            a.editor.mode_registry
                .is_registered(lattice_lsp::modes::LspLogMode::mode_id())
        );
        assert!(
            a.editor.mode_registry
                .is_registered(lattice_lsp::modes::LspTraceLogMode::mode_id())
        );
        assert!(
            a.editor.mode_registry
                .is_registered(lattice_lsp::modes::LspServerLogMode::mode_id())
        );
    }

    #[test]
    fn open_file_tree_seeds_file_tree_locals() {
        // Construct a temp dir + file, open as a tree, confirm
        // the locals are populated.
        let tmp = std::env::temp_dir().join(format!("lattice-m3-2-c-2-{}", std::process::id()));
        let _ = std::fs::create_dir_all(&tmp);
        let f = tmp.join("file.txt");
        let _ = std::fs::write(&f, "hi");

        let mut a = app_with("hi", 5);
        // Drive via the production path. `do_open_file_tree`
        // constructs the FileTreeBuffer, calls
        // `seed_file_tree_locals`, inserts into the registry,
        // and activates the pane on it.
        a.do_open_file_tree(Some(tmp.clone()));
        let tree_id = a.active_pane_buffer_id();

        let locals = a
            .editor.buffer_locals
            .get(&tree_id)
            .expect("file-tree locals seeded");
        let root = locals
            .get::<crate::modes::FileTreeRoot>()
            .expect("FileTreeRoot local present");
        assert_eq!(root.0, tmp);
        let entries = locals
            .get::<crate::modes::FileTreeEntries>()
            .expect("FileTreeEntries local present");
        // At minimum the root row + the file row.
        assert!(entries.0.len() >= 2);
        // FileTreeNerdFonts seeded; concrete value matches
        // App's `theme.nerd_fonts` (we don't assert a specific
        // boolean since the theme default may evolve -- the
        // important contract is "the local exists post-seed").
        assert!(locals.get::<crate::modes::FileTreeNerdFonts>().is_some());

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn set_ui_nerd_fonts_rerenders_open_file_tree() {
        // Regression for the bug where toggling `ui.nerd_fonts`
        // updated the theme but left existing file-tree ropes
        // rendering the old palette. The rope embeds the icon
        // glyphs, so a palette flip must re-render every open
        // tree -- otherwise the user keeps seeing `?` boxes (or
        // BMP fallbacks) until they reopen the tree.
        let tmp =
            std::env::temp_dir().join(format!("lattice-tree-nerd-rerender-{}", std::process::id()));
        let _ = std::fs::create_dir_all(&tmp);
        std::fs::write(tmp.join("main.rs"), "").unwrap();

        let mut a = app_with("hi", 5);
        a.do_open_file_tree(Some(tmp.clone()));
        let tree_id = a.active_pane_buffer_id();

        // Default is BMP fallback -- the rope should contain
        // the source-code middle-dot, not the nerd-font rust
        // glyph.
        let body = a
            .editor.buffers
            .with_file_tree(tree_id, |t| t.content.as_string())
            .unwrap();
        assert!(
            body.contains("· main.rs"),
            "expected BMP fallback in rope, got: {body}"
        );
        assert!(
            !body.contains("󱘗 "),
            "nerd-font glyph leaked into default rope: {body}"
        );

        // Flip the typed option via the same path `:set` takes.
        // The change handler must re-render every open tree
        // against the new palette.
        submit_ex(&mut a, "set ui.nerd_fonts=on");

        let body = a
            .editor.buffers
            .with_file_tree(tree_id, |t| t.content.as_string())
            .unwrap();
        assert!(
            body.contains("󱘗 main.rs"),
            "expected nerd-font glyph post-toggle, got: {body}"
        );

        let _ = std::fs::remove_dir_all(&tmp);
    }

    #[test]
    fn follow_link_reads_link_from_buffer_locals() {
        // M.3.2.c.1: prove `do_help_follow_link` reads the
        // link data from `buffer_locals` (canonical), not the
        // HelpBuffer's struct field. We open a help buffer,
        // overwrite its locals with a synthetic link pointing
        // somewhere different than the buffer's actual links,
        // and verify FollowLink dispatches based on the
        // locals-side link.
        let mut a = app_with("xx", 10);
        let help = crate::help::HelpContent::from_lines(
            "test-locals-link",
            vec!["plain text -- no markdown link".into()],
        );
        let help_id = a.open_help_in_pane(help);

        // Replace the locals-side links with a synthetic
        // Topic link that the production reader should pick
        // up -- the HelpBuffer's own `links` is empty (no
        // markdown link in the source), so without the
        // locals-first read, FollowLink would say "no link
        // under cursor".
        let synthetic = crate::modes::HelpLinks(vec![crate::help::HelpLink {
            range: lattice_protocol::Range::new(
                lattice_protocol::Position::ZERO,
                lattice_protocol::Position::new(0, 5),
            ),
            target: crate::help::HelpLinkTarget::Topic("synthetic-topic".into()),
        }]);
        a.editor.buffer_locals
            .get_mut(&help_id)
            .expect("locals seeded")
            .insert(synthetic);

        a.editor.cursor = lattice_protocol::Position::new(0, 0);
        // `open_help_in_pane` already activates the pane on
        // the registered help buffer; FollowLink reads
        // `pane_tree.active().buffer_id` to look up locals.
        a.apply(Action::FollowLink);

        // The link target was `help:synthetic-topic`; the
        // FollowLink path routes Topic targets to
        // `:help <topic>`. The topic doesn't exist so we
        // expect an info echo about the topic; the key
        // assertion is "the link was found and dispatched"
        // which we observe via the message kind. If the
        // production path had read from the (empty) struct
        // field, the message would have been "no link under
        // cursor".
        let msg = a.editor.last_message.as_ref().expect("echo set by FollowLink");
        assert!(
            !msg.text.contains("no link under cursor"),
            "production reader should have found the link via buffer_locals, \
             got message: {}",
            msg.text
        );
    }
}
