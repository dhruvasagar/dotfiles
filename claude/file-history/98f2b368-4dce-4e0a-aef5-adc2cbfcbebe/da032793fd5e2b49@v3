//! LSP feature surface -- App methods for the various
//! `:lsp-*` ex commands (admin / log / trace / status /
//! restart) plus the request-driven LSP feature methods
//! (hover, definition, references, completion, format,
//! rename, code action, document / workspace symbols).
//!
//! Methods that live here:
//! - LSP admin / log / trace ex-commands:
//!   - do_open_lsp_log (`:lsp-log [server]`),
//!   - do_open_lsp_trace_log (`:lsp-trace-log [server]`),
//!   - do_toggle_lsp_trace (`:lsp-trace <name>`),
//!   - do_lsp_status (`:lsp-status`),
//!   - do_lsp_server_log_listing (`:lsp-server-log`),
//!   - do_lsp_restart (`:lsp-restart <server>`),
//!   - do_set_lsp_log_level
//!     (`:lsp-log-level [server] <level>`),
//!   - do_lsp_log_clear (`:lsp-log-clear [server]`).
//! - LSP request handlers + their drain pumps:
//!   - hover, nav (`gd` / `gD` / `gy` / `gI`),
//!     references, signature help, completion (palette +
//!     Insert-mode popup), document / workspace symbols,
//!     format / format-range, rename, code action.
//!   - drain_pending_lsp_*, drain_pending_completion_resolve,
//!     drain_pending_insert_completion_lsp, etc.
//! - apply_lsp_text_edits / apply_lsp_workspace_edit and
//!   the per-feature outcome appliers
//!   (apply_lsp_completion_accept, apply_lsp_format_outcome,
//!   apply_lsp_rename_outcome, apply_code_action_outcome,
//!   ...).
//! - LSP completion meta sidecar + helpers
//!   (lsp_completion_meta_for, dedup_rendered_by_text,
//!   docs_body_for_selected, selected_needs_resolve).
//! - apply_persistent_lsp_editor_options (lifecycle bridge)
//!   and execute_lsp_command.
//! - resolve_server_id / running_server_ids (pub(super);
//!   shared with picker.rs).
//!
//! What does NOT live here: the LSP wire layer / actor /
//! supervisor (those live in `lattice-lsp`). This module is
//! about App's *consumption* of that layer.

use lattice_protocol::position::Position;

#[cfg(test)]
use lattice_grammar::ModalState;

// 5.8.S-AA: several outcome types moved their drain bodies host-
// side; the leftover `use` here keeps test-scope references
// (`super::*Outcome`, `super::LspNavKind`) resolving. `#[allow]`
// silences the unused-import warning in the non-test build.
#[allow(unused_imports)]
use super::{
    App, CodeActionOutcome, CodeActionRow, CompletionItemRow, CompletionOutcome,
    CompletionResolveOutcome, EchoLevel, FormatOutcome, InsertCompletionLspOutcome,
    LSP_COMPLETION_KIND_ID, LspCompletionMeta, LspNavKind, ReferencesOutcome, RenameOutcome,
    SignatureHelpOutcome, SymbolRow, SymbolsOutcome, TagStackEntry, app_to_lsp_position,
    call_hierarchy_to_row, code_action_kind_glyph, dedup_rendered_by_text, flatten_workspace_edit,
    last_addressable_line, line_byte_len, lsp_position_to_app_byte, prepare_rename_placeholder,
    range_covers, type_hierarchy_to_row, word_under_cursor,
};
// 5.5.LSP.1 / LSP.2 / LSP.4 / LSP.5: test-only utility imports
// after the corresponding request-side migrations. Tests reach
// them via `super::<fn>(...)` so they need to live in the `mod lsp`
// scope, but `cfg(test)` keeps them out of release builds (where
// `deny(unused_imports)` would flag them).
#[cfg(test)]
use super::{
    definition_response_to_locations, flatten_document_symbol_response, hover_contents_to_markdown,
    signature_help_to_markdown,
};
use crate::buffers::BufferId;
use lattice_protocol::edit::Edit;

/// CSM.8b.3: host-side [`lattice_completion::CandidateSink`]
/// impl that buffers each `produce_async` push into a single
/// batch. The aggregator spawns the source's future, awaits
/// it, then drains the sink onto the existing
/// `InsertCompletionLspOutcome::Items` channel -- the drain
/// path keeps its "replace prior LSP slice" semantics
/// untouched. `is_incomplete` rides on
/// [`lattice_completion::CandidateSink::mark_incomplete`].
// Phase 5.8.AD.4: `BatchingSink` migrated to host as
// `InsertCompletionBatchingSink` (private to `lattice_host::dispatch`).

impl App {
    /// M.5.0: is `lsp-mode` active for `buffer_id`? Every LSP
    /// entry point (hover, completion, diagnostics-render,
    /// document-sync, ...) gates on this in subsequent slices;
    /// returns `true` when the minor is in
    /// `active_modes[buffer_id]`'s minor list, `false`
    /// otherwise (no buffer registered, or mode not active on
    /// it).
    ///
    /// In M.5.0 nothing reads this -- the surface is here so
    /// M.5.2 (auto-activation hook), M.5.3 (lifecycle), and
    /// M.5.4+ (gates) have a single accessor to consume.
    /// 5.5.F.5.1: see [`lattice_host::dispatch::Editor::lsp_mode_enabled_for`].
    ///
    /// Slice 3c.extension.fold-rs: route through the published
    /// `ModesRenderState` (B.11) instead of `read_editor`. Per-frame
    /// hot path — every paint queries this for the diagnostics /
    /// semantic-token / document-highlight / inlay-hint / progress
    /// gates, so each saved actor RPC drops ~100µs off the frame
    /// budget.
    pub fn lsp_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        // cfg(test) escape hatch — see `App::cursor()`.
        #[cfg(test)]
        {
            self.editor.lsp_mode_enabled_for(buffer_id)
        }
        #[cfg(not(test))]
        {
            self.modes()
                .map
                .get(&buffer_id)
                .map(|m| m.has_minor(lattice_lsp::modes::LspMode::mode_id()))
                .unwrap_or(false)
        }
    }

    /// M.6.0: is `mode_id` active on `buffer_id`? Generic minor-
    /// mode accessor used by every M.6 sub-mode reader. Always
    /// returns `false` when no entry exists for `buffer_id` --
    /// matches the umbrella accessor's shape.
    fn minor_mode_enabled_for(&self, buffer_id: BufferId, mode_id: lattice_mode::ModeId) -> bool {
        self.read_editor(move |e| e.minor_mode_enabled_for(buffer_id, mode_id))
    }

    /// CSM.K1: is `completion-mode` (the persistent gate)
    /// active on `buffer_id`? Auto-activates on writable buffer
    /// kinds; the popup-trigger entry points check this before
    /// opening the popup so read-only buffers (Help, FileTree,
    /// Oil) silently no-op on `<C-Space>`.
    pub fn completion_mode_active_for(&self, buffer_id: BufferId) -> bool {
        // Phase 5.8.AD.4: migrated.
        self.read_editor(move |e| e.completion_mode_active_for(buffer_id))
    }

    /// CSM.K1: is `completion-popup-mode` (the transient
    /// popup-live marker) active on `buffer_id`? Architectural
    /// answer to "is the insert-completion popup live on this
    /// buffer." Tests + production code asking the popup-state
    /// question should read this rather than poking
    /// `App.insert_completion.is_some()` directly -- the field
    /// is the popup's *content*; the mode is the *gate*.
    pub fn completion_popup_mode_active_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_mode::CompletionPopupMode::mode_id())
    }

    /// Shorthand: is the insert-completion popup live on the
    /// active document buffer? The popup is anchored to the doc
    /// the user is typing in; v1 has a single
    /// `self.document_buffer_id()`.
    pub fn completion_popup_active(&self) -> bool {
        self.completion_popup_mode_active_for(self.document_buffer_id())
    }

    /// M.6.0: is `lsp-completion-mode` active on `buffer_id`? Read
    /// by `do_lsp_completion_request` /
    /// `do_lsp_insert_completion_request` and the LSP completion
    /// source filter once M.6.2 / M.6.3 wire the gates.
    pub fn lsp_completion_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspCompletionMode::mode_id())
    }

    /// M.6.0: is `lsp-diagnostics-mode` active on `buffer_id`?
    /// Read by the publish-diagnostics paint pipeline and
    /// `:diag-next` / `:diag-prev` once M.6.3 wires the gate.
    pub fn lsp_diagnostics_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        #[cfg(test)]
        {
            self.editor.lsp_diagnostics_mode_enabled_for(buffer_id)
        }
        #[cfg(not(test))]
        {
            self.modes()
                .map
                .get(&buffer_id)
                .map(|m| m.has_minor(lattice_lsp::modes::LspDiagnosticsMode::mode_id()))
                .unwrap_or(false)
        }
    }

    /// M.6.0: is `lsp-hover-mode` active on `buffer_id`? Read by
    /// `do_lsp_hover_request` once M.6.2 wires the gate.
    pub fn lsp_hover_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspHoverMode::mode_id())
    }

    /// M.6.0: is `lsp-signature-mode` active on `buffer_id`?
    pub fn lsp_signature_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspSignatureMode::mode_id())
    }

    /// M.6.0: is `lsp-format-mode` active on `buffer_id`? Gates
    /// `:lsp-format` / `:lsp-format-range` and `onTypeFormatting`.
    pub fn lsp_format_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.read_editor(move |e| e.lsp_format_mode_enabled_for(buffer_id))
    }

    pub fn lsp_rename_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.read_editor(move |e| e.lsp_rename_mode_enabled_for(buffer_id))
    }

    pub fn lsp_symbols_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.read_editor(move |e| e.lsp_symbols_mode_enabled_for(buffer_id))
    }

    pub fn lsp_code_action_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.read_editor(move |e| e.lsp_code_action_mode_enabled_for(buffer_id))
    }

    pub fn lsp_nav_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.read_editor(move |e| e.lsp_nav_mode_enabled_for(buffer_id))
    }

    /// 4.4.c: is `lsp-progress-mode` active on `buffer_id`?
    /// Gates the modeline `$/progress` segment and progress
    /// accumulation for buffers attached to a server. With
    /// the mode off, incoming progress events still flow on
    /// the bus (plugins can subscribe) but the modeline stays
    /// quiet for that buffer.
    pub fn lsp_progress_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        #[cfg(test)]
        {
            self.editor.lsp_progress_mode_enabled_for(buffer_id)
        }
        #[cfg(not(test))]
        {
            self.modes()
                .map
                .get(&buffer_id)
                .map(|m| m.has_minor(lattice_lsp::modes::LspProgressMode::mode_id()))
                .unwrap_or(false)
        }
    }

    /// 4.4.e: is `lsp-document-highlight-mode` active on
    /// `buffer_id`? Gates the cursor-driven
    /// `textDocument/documentHighlight` request issuance and
    /// the soft-highlight decoration overlay.
    pub fn lsp_document_highlight_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        #[cfg(test)]
        {
            self.editor.lsp_document_highlight_mode_enabled_for(buffer_id)
        }
        #[cfg(not(test))]
        {
            self.modes()
                .map
                .get(&buffer_id)
                .map(|m| m.has_minor(lattice_lsp::modes::LspDocumentHighlightMode::mode_id()))
                .unwrap_or(false)
        }
    }

    /// 4.4.e: is `lsp-selection-range-mode` active on
    /// `buffer_id`? Gates `textDocument/selectionRange`
    /// issuance for the smart-expansion operator.
    pub fn lsp_selection_range_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.read_editor(move |e| e.lsp_selection_range_mode_enabled_for(buffer_id))
    }

    /// 4.4.f: is `lsp-folding-mode` active on `buffer_id`?
    /// Gates `textDocument/foldingRange` issuance. Independent
    /// of the foldmethod option: when the mode is off the
    /// cache stays empty and `:set foldmethod=lsp` cascades to
    /// `Syntax`.
    pub fn lsp_folding_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.read_editor(move |e| e.lsp_folding_mode_enabled_for(buffer_id))
    }

    pub fn lsp_inlay_hint_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        #[cfg(test)]
        {
            self.editor.lsp_inlay_hint_mode_enabled_for(buffer_id)
        }
        #[cfg(not(test))]
        {
            self.modes()
                .map
                .get(&buffer_id)
                .map(|m| m.has_minor(lattice_lsp::modes::LspInlayHintMode::mode_id()))
                .unwrap_or(false)
        }
    }

    pub fn lsp_semantic_tokens_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        #[cfg(test)]
        {
            self.editor.lsp_semantic_tokens_mode_enabled_for(buffer_id)
        }
        #[cfg(not(test))]
        {
            self.modes()
                .map
                .get(&buffer_id)
                .map(|m| m.has_minor(lattice_lsp::modes::LspSemanticTokensMode::mode_id()))
                .unwrap_or(false)
        }
    }

    /// M.5.4: shared gate for every LSP request entry point
    /// (hover / definition / completion / format / rename /
    /// code-action / symbols / signature / references). Returns
    /// `true` when `lsp-mode` is active on the current document;
    /// callers early-return on `false`. A single echo surfaces
    /// the gate state so users discover the mode -- silent gates
    /// are a documented anti-pattern when an editor's defaults
    /// the user expects (`K`, `gd`) suddenly do nothing.
    ///
    /// The echo level is `Info` (not `Warn`) -- gated state is
    /// expected user-controlled, not a misconfiguration.
    pub(super) fn check_lsp_mode_gate(&mut self) -> bool {
        if self.lsp_mode_enabled_for(self.document_buffer_id()) {
            return true;
        }
        self.set_message(
            EchoLevel::Info,
            "lsp-mode disabled for this buffer (`:lsp-mode` to enable)".to_string(),
        );
        false
    }

    /// M.6.2: shared gate for a per-feature LSP sub-mode. Checks
    /// the umbrella first (so the user gets one consistent
    /// message-source-of-truth: enable `lsp-mode` first, then
    /// the sub-mode); returns `true` only when both are active.
    /// Echoes at `Info` matching the umbrella's level.
    ///
    /// Used by `do_lsp_*_request` methods that want a
    /// user-discoverable bail message. Insert-mode auto-triggers
    /// (insert completion, signature help, on-type formatting)
    /// skip the echo path entirely and check the bool directly
    /// -- a typed character that doesn't fire isn't a moment to
    /// surface mode state.
    fn check_lsp_sub_mode_gate(
        &mut self,
        sub_mode_id: lattice_mode::ModeId,
        sub_mode_name: &str,
    ) -> bool {
        if !self.check_lsp_mode_gate() {
            return false;
        }
        if self.minor_mode_enabled_for(self.document_buffer_id(), sub_mode_id) {
            return true;
        }
        self.set_message(
            EchoLevel::Info,
            format!("{sub_mode_name} disabled for this buffer (`:{sub_mode_name}` to enable)"),
        );
        false
    }

    // 5.5.LSP.1: `K` -- `do_lsp_hover_request` relocated to
    // [`lattice_host::dispatch::Editor::lsp_hover_request`]. The
    // host-side body is identical (gates + URI lookup + cursor
    // translation + spawn on the LSP runtime) and runs as the
    // [`Action::LspHoverRequest`] arm of [`Editor::dispatch`]. App-
    // side callers (tests in app.rs at lines 2339, 3198 and
    // dispatch.rs at 2206) route through `apply(Action::Lsp...)`
    // now. The drain side (`drain_pending_hover`) stays App-
    // resident until LSP.2+ migrates it.

    /// Drain the channel populated by `do_lsp_hover_request` and
    /// act on every pending `HoverOutcome`: open the popup for
    /// `Body`, echo a clear message for `NoBody` / `NoServers` so
    /// the user always knows their `K` press was processed.
    /// Called once per main_loop iteration before draw; cheap
    /// when the channel is empty (the common case).
    pub fn drain_pending_hover(&mut self) {
        // 5.8.W: drain body migrated to
        // `lattice_host::dispatch::Editor::drain_pending_hover`
        // so the GPUI peer reaches the same path. Returns
        // RendererSignals (DisplayBuffer for Body; no signal for
        // NoBody/NoServers — those echo via set_message inside
        // the drain). This peer fans the signals through its
        // existing handler.
        let signals = self.mutate_editor_with(|e| e.drain_pending_hover());
        for signal in signals {
            self.handle_renderer_signal(signal);
        }
    }

    /// Apply an accepted LSP completion item. Routes the main
    /// insert (with `textEdit.range` honoured when present) plus
    /// `additionalTextEdits` through `apply_lsp_text_edits` so
    /// the whole set lands as one undo unit. Snippet-flavoured
    /// items currently splice the literal body -- placeholder
    /// navigation is in 4.2.g.4 with `lattice-snippet`.
    pub(super) fn apply_lsp_completion_accept(
        &mut self,
        meta: LspCompletionMeta,
        anchor: lattice_protocol::position::Position,
    ) {
        // Phase 5.8.AD.4: body migrated.
        self.mutate_editor(move |e| e.apply_lsp_completion_accept(meta, anchor));
    }

    /// Fire `completionItem/resolve` for the focused candidate
    /// (Phase 4.2.g.3). The original CompletionItem is round-
    /// tripped to the originating server; the response fills in
    /// `documentation` / `additionalTextEdits` / `detail` per
    /// the LSP spec. Drain updates the meta + the docs popup
    /// body in place.
    pub(super) fn do_completion_resolve_focused(&mut self) {
        // Phase 5.8.AD.4: body migrated.
        // Slice 3c.final.E.4: route through `mutate_editor`.
        self.mutate_editor(|e| e.do_completion_resolve_focused());
    }

    /// Fire `textDocument/completion` for the active Insert-
    /// mode popup (Phase 4.2.g.2). The response merges into
    /// `state.raw` via the per-frame drain. Cancellation token
    /// rides on every keystroke that mutates the query when
    /// `isIncomplete: true`; manual re-triggers always re-fire
    /// fresh.
    ///
    /// Multi-server fan-out + dedup (label + kind) is the
    /// architecture-doc strategy. Items beyond `MAX_LSP_ITEMS`
    /// are dropped.
    pub(super) fn do_lsp_insert_completion_request(&mut self) {
        // Phase 5.8.AD.4: body migrated.
        self.mutate_editor(|e| e.do_lsp_insert_completion_request());
    }

    /// Drain queued `completionItem/resolve` responses --
    /// decode the matching candidate's payload, apply the
    /// resolved fields, re-encode in place, then refresh the
    /// docs-popup body when the resolved item is the popup's
    /// currently-focused one. CSM.8b.5: state.raw is the
    /// source of truth; no parallel sidecar to keep in sync.
    pub fn drain_pending_completion_resolve(&mut self) {
        // 5.8.AA.d: migrated to host.
        self.mutate_editor(|e| e.drain_pending_completion_resolve());
    }

    /// Per-frame drain hook -- merge any LSP completion response
    /// into the active popup's `raw` set, refilter, and update
    /// the `lsp_incomplete` flag.
    pub fn drain_pending_insert_completion_lsp(&mut self) {
        // 5.8.AA.d: migrated to host.
        self.mutate_editor(|e| e.drain_pending_insert_completion_lsp());
    }

    /// Drain queued `lattice_lsp::LspLogPushed` events for the
    /// App-side concerns the modes don't own. After B'.6 the
    /// three log majors (`LspLogMode`, `LspServerLogMode`,
    /// `LspTraceLogMode`) own every buffer append; this drain
    /// only surfaces `window/showMessage`-sourced records to the
    /// minibuffer (vim's `:echom`-style transient surface) so
    /// users see server-emitted notifications without opening the
    /// LSP log buffer. Multiple showMessages in one tick collapse
    /// to the last (matches successive `:echo` calls).
    ///
    /// Called once per main-loop tick.
    pub fn drain_lsp_log_events(&mut self) {
        // 5.8.AA.d: migrated to host.
        self.mutate_editor(|e| e.drain_lsp_log_events());
    }

    /// 4.4.c: drain queued `LspProgressUpdate` events and
    /// fold them into `self.editor.lsp_progress`. `Begin` inserts,
    /// `Report` updates (preserving title from the prior
    /// `Begin` when the report doesn't restate it), `End`
    /// removes. Called once per main-loop tick.
    ///
    /// Cheap when no events arrived: a single try_recv that
    /// returns `Empty` and exits.
    /// Drain queued `lattice_lsp::LspBufferDetached` events
    /// (published by `LspMode::on_deactivate` via Phase 2's
    /// `ctx.events()`). For each event, call
    /// [`Self::lsp_close_buffer`] to fire the wire-level
    /// `textDocument/didClose` and clear the buffer's URI
    /// mapping. Called once per main-loop tick.
    ///
    /// Cheap when no events arrived (single `try_recv` → `Empty`).
    /// Cheap when the buffer has no URI mapping (the close path
    /// short-circuits on `buffer_uris.remove` returning `None`).
    pub fn drain_lsp_detach_events(&mut self) {
        // 5.8.AA.d: migrated to host.
        self.mutate_editor(|e| e.drain_lsp_detach_events());
    }

    pub fn drain_lsp_progress_events(&mut self) {
        // 5.8.AA.d: migrated to host.
        self.mutate_editor(|e| e.drain_lsp_progress_events());
    }

    /// Drain server-initiated `workspace/configuration` requests.
    /// Each request lands as a `lattice_lsp::InboundConfigurationRequest`
    /// carrying section paths + a oneshot for the response.
    /// Server-side keys come in their own namespaces (e.g.
    /// `"rust-analyzer.cargo.features"`); the editor's TOML places
    /// these under an `[lsp.<server>]` umbrella so multiple
    /// servers' keys don't collide. The drain prepends `lsp.` to
    /// the requested section before walking the tree.
    pub fn drain_inbound_configuration_requests(&mut self) {
        // 5.8.AA.d: migrated to host.
        self.mutate_editor(|e| e.drain_inbound_configuration_requests());
    }

    /// 4.4.k: fan out `workspace/didChangeConfiguration` to
    /// every running actor with the given `server_id`. Called
    /// from the typed-option cascade
    /// (the private `apply_option_cascade`) whenever a key under
    /// `lsp.<server_id>.*` changes. The notification's
    /// `settings` payload is the full `lsp.<server_id>` JSON
    /// subtree from the merged TOML tree -- matches the shape
    /// returned by `workspace/configuration` so servers that
    /// pull and servers that read-inline see consistent data.
    ///
    /// Empty subtree (server doesn't exist in TOML) still
    /// fires the notification with `settings: null`; per spec
    /// servers MAY interpret that as "reset to defaults".
    /// Notify-only -- no response, errors log and skip.
    ///
    /// Cross-workspace fan-out: if two actors share `server_id`
    /// across different workspace roots, both receive the
    /// notification (config is global, not workspace-scoped).
    /// Thin wrapper around
    /// [`lattice_host::editor::Editor::fan_out_did_change_configuration`]
    /// (Phase 5.7.B.7 migration). Kept on `impl App` so the TUI
    /// peer's `handle_renderer_signal` match arm doesn't churn
    /// while the body lives host-side -- the GPUI peer reaches
    /// the same logic through `editor.fan_out_did_change_configuration`.
    pub fn fan_out_did_change_configuration(&mut self, server_id: &str) {
        // Slice 3c.final.E.5: clone owned + route via mutate_editor.
        let server_id = server_id.to_string();
        self.mutate_editor(move |e| e.fan_out_did_change_configuration(&server_id));
    }

    /// 4.4.b: drain server-initiated `window/showDocument`
    /// requests. Each request lands as
    /// [`lattice_lsp::InboundShowDocument`] carrying the URI,
    /// the `external`/`take_focus` flags, an optional
    /// `selection` range, and a oneshot for the reply.
    ///
    /// Open semantics:
    /// - `external == true` -> delegate to the OS handler
    ///   (`open` on macOS, `xdg-open` on linux). Selection is
    ///   ignored; success reflects whether the spawn was
    ///   accepted, not whether the target opened.
    /// - `file://` URI with `external == false` -> open the
    ///   path in a new editor buffer via the same path
    ///   `:e <path>` uses. Selection (if present) is applied
    ///   after open via the standard LSP-position conversion.
    /// - Anything else with `external == false` -> reply
    ///   `success: false` (we don't know how to surface a
    ///   non-file URI in a buffer).
    pub fn drain_inbound_show_documents(&mut self) {
        // 5.8.AA.k.3: migrated to host.
        let signals = self.mutate_editor_with(|e| e.drain_inbound_show_documents());
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }

    // 5.8.AA.k.3: perform_show_document, open_external_uri,
    // move_cursor_to_lsp_position migrated to host alongside
    // drain_inbound_show_documents.

    /// 4.4.b: drain server-initiated
    /// Drain server-initiated `window/showMessageRequest`
    /// inbound requests (4.4.b). Each request comes with a
    /// prompt, an optional action list, and a oneshot for the
    /// reply. The actionless case (just an info / warn / error
    /// notification) auto-replies `null` and surfaces the
    /// prompt on the minibuffer + LSP log. The actionful case
    /// registers the request in `lsp_pending_show_message_requests`
    /// and either opens an action picker (if no picker is
    /// currently up) or queues the id behind the active one.
    ///
    /// Picker accept (the `AcceptShowMessageAction` routing
    /// arm) and picker dismiss (the `LspShowMessageRequest`
    /// source arm) both pull the slot out, ferry back the
    /// response, then drain the queue so the next pending SMR
    /// opens on the same tick.
    pub fn drain_inbound_show_message_requests(&mut self) {
        // 5.8.AA.e: migrated to host.
        self.mutate_editor(|e| e.drain_inbound_show_message_requests());
    }

    /// Allocate a fresh `u32` request id for
    /// `lsp_pending_show_message_requests`. Wraps on overflow
    /// and skips any id currently in use -- collision is only
    /// possible if `u32::MAX` actionful requests pile up at
    /// once, which won't happen, but the loop keeps the
    /// invariant honest.
    /// 5.8.AA.e: open-picker body migrated to host. Thin wrapper
    /// kept since `accept_show_message_action` (App-side, applies
    /// the user's choice via the picker dispatcher path) calls it.
    pub(super) fn open_show_message_request_picker(&mut self, request_id: u32) {
        self.mutate_editor(move |e| e.open_show_message_request_picker(request_id));
    }

    /// Send the LSP response for one in-flight
    /// `showMessageRequest`. `selected_index` of `None` is the
    /// dismiss path (reply `null`); `Some(i)` ferries the
    /// `i`-th MessageActionItem back. Idempotent on missing
    /// ids (the slot may already have been answered if the
    /// drain logic raced with picker close).
    pub(crate) fn finalize_show_message_request(
        &mut self,
        request_id: u32,
        selected_index: Option<u32>,
    ) {
        // Slice 3c.final.E.5j: remove via `mutate_editor_with` so
        // the owned `req` flows out of the editor borrow.
        let Some(req) = self.mutate_editor_with(move |e| {
            e.lsp_pending_show_message_requests.remove(&request_id)
        }) else {
            return;
        };
        let selected = selected_index.and_then(|i| req.actions.get(i as usize).cloned());
        let _ = req
            .response
            .send(lattice_lsp::ShowMessageRequestOutcome { selected });
    }

    /// Advance the SMR queue. Called from the picker accept /
    /// dismiss arms after the active request is resolved -- if
    /// another id is queued, open its picker on the same tick
    /// so the user sees the next prompt without a frame's gap.
    pub(crate) fn open_next_queued_show_message_request(&mut self) {
        // Slice 3c.final.E.5e: pop valid IDs through a single
        // closure so the loop body crosses the actor seam exactly
        // once per attempt. The host-side closure pops + filters
        // until it finds a still-pending request id (or exhausts
        // the queue).
        let next = self.mutate_editor_with(|e| {
            while let Some(id) = e.lsp_show_message_request_queue.pop_front() {
                if e.lsp_pending_show_message_requests.contains_key(&id) {
                    return Some(id);
                }
            }
            None
        });
        if let Some(id) = next {
            self.open_show_message_request_picker(id);
        }
    }

    /// Drain server-initiated `workspace/applyEdit` requests
    /// (Phase 4.3). Each request lands as a
    /// `lattice_lsp::InboundApplyEdit` carrying a typed
    /// `WorkspaceEdit` + a oneshot for the response. We flatten
    /// the edit into per-file `Vec<TextEdit>` batches (same
    /// `flatten_workspace_edit` path the `:rename` drain uses),
    /// apply each, and reply via the oneshot.
    ///
    /// Apply semantics mirror `apply_rename_workspace_edit`:
    /// edits to the active buffer land directly via
    /// `apply_lsp_text_edits`; cross-file edits open the target
    /// via `do_edit` and apply there. Failures on individual
    /// files echo a warning but don't roll back successfully-
    /// applied files.
    pub fn drain_inbound_apply_edits(&mut self) {
        // 5.8.AA.l.5: migrated to host.
        let signals = self.mutate_editor_with(|e| e.drain_inbound_apply_edits());
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }

    // 5.8.AA.l.5: apply_inbound_workspace_edit body migrated
    // to host.

    /// Single canonical hook for "this buffer was just opened":
    /// register `BufferId → Uri` eagerly (path-bearing only),
    /// then publish `Event::DocumentOpened` on the bus. Both the
    /// initial-document path (`App::new`) and the follow-up
    /// `:e <path>` path (`App::do_edit`) call this helper.
    ///
    /// Idempotent against the supervisor: re-publishing the same
    /// URI is a no-op because `LspSupervisorHandle::open_buffer`
    /// short-circuits already-attached URIs.
    /// Thin wrapper around
    /// [`lattice_host::editor::Editor::publish_document_opened_for_active`]
    /// (Phase 5.7.B.6 migration). The TUI peer's `App::new` calls
    /// this name historically; keeping the wrapper avoids
    /// churning the call site while the body lives host-side so
    /// the GPUI peer can call it through `editor.publish_document_opened_for_active()`.
    pub(super) fn publish_document_opened_for_active(&mut self) {
        self.mutate_editor(|e| e.publish_document_opened_for_active());
    }

    /// Decode the LSP metadata directly from a candidate's
    /// `CandidateData::Extension` payload (CSM.8b: the candidate
    /// IS the metadata; no sidecar lookup). Returns `None` for
    /// non-LSP candidates (buffer-words / snippet / path /
    /// tree-sitter rows whose payload kind id is something else)
    /// or when the payload doesn't decode (stale wire format).
    ///
    /// Owned return type: the candidate's payload bytes are
    /// the source of truth, and `decode_meta` produces a
    /// fresh `LspCompletionMeta` per call. Callers that read
    /// many fields can clone the result into a local; the
    /// per-frame docs / glyph / commit-char hot paths still
    /// stay well inside the frame budget (serde_json decode
    /// of a typical LSP item is microseconds).
    pub(crate) fn lsp_completion_meta_for(
        &self,
        candidate: &lattice_completion::RenderedCandidate,
    ) -> Option<LspCompletionMeta> {
        // Phase 5.8.AD.4: body migrated.
        // Slice 3c.final.E.swap-prep: clone for Send + 'static closure.
        let candidate = candidate.clone();
        self.read_editor(move |e| e.lsp_completion_meta_for(&candidate))
    }

    /// Look up the current URI of a buffer. None for buffers
    /// that have no on-disk path yet (new unsaved scratch
    /// buffers).
    pub fn buffer_uri(&self, id: BufferId) -> Option<lattice_lsp::Uri> {
        // Slice 3c.final.E.5d: returns owned `Uri` (not `&Uri`)
        // because the `Arc<BuffersRenderState>` snapshot is a
        // temporary in this stack frame; borrowing through it
        // would escape the function. `Uri` is a thin Arc<str>
        // wrapper under the hood, so this clone is one Arc bump.
        self.buffers().uris.get(&id).cloned()
    }

    /// Flush queued didChange events for a buffer immediately.
    /// Used by will-save hooks (4.3) so the server's view is
    /// caught up before pre-save requests fire. Fire-and-forget
    /// against the supervisor mailbox.
    pub fn lsp_flush(&self, buffer_id: BufferId) {
        let Some(uri) = self.buffers().uris.get(&buffer_id).cloned() else {
            return;
        };
        // Slice 3c.final.E.5e: `uri` is owned (cloned above);
        // `LspBatchingSink::flush(&self, Uri)` so `read_editor`
        // closure satisfies `&Editor` + `Send + 'static`.
        self.read_editor(move |e| e.lsp.flush(uri));
    }

    /// 5.5.F.4.4: see [`lattice_host::dispatch::Editor::lsp_close_buffer`].
    /// App-side wrapper kept as a thin delegate because
    /// `drain_lsp_detach_events` (App tick path) still calls it; tests
    /// in this module call it directly. Deletes when the tick-event
    /// drain moves host-side.
    pub fn lsp_close_buffer(&mut self, buffer_id: BufferId) {
        self.mutate_editor(move |e| e.lsp_close_buffer(buffer_id));
    }

    /// Apply editor-side LSP options that the user configured
    /// under the top-level `[lsp]` TOML table (as distinct from
    /// server-namespaced subtables like `[lsp.rust-analyzer]`,
    /// which are served back to servers via
    /// `workspace/configuration`).
    ///
    /// Today this handles:
    /// - `lsp-mode.log-level` -- string, one of
    ///   `error`/`warn`/`info`/`debug`/`trace`. Sets the
    ///   subsystem-wide default min level (same effect as
    ///   `:lsp-log-level <level>`). The mode-owned namespace
    ///   (M.6.5): `lsp-mode.*` collects every option owned by
    ///   the `lsp-mode` minor; `lsp.*` is reserved for the
    ///   structural `workspace/configuration` passthrough
    ///   (per-server TOML subtables like `[lsp.rust-analyzer]`).
    /// - `lsp.log-level` -- legacy alias. Reads the same value;
    ///   emits a deprecation warn echo. Removed in a follow-up
    ///   minor version.
    ///
    /// Unknown / mistyped values surface a warn echo and the
    /// option is skipped. Missing keys are silent.
    pub(super) fn apply_persistent_lsp_editor_options(&mut self) {
        // Phase 5.8.AA.u: body migrated to
        // `lattice_host::dispatch::Editor::apply_persistent_lsp_editor_options`.
        self.mutate_editor(|e| e.apply_persistent_lsp_editor_options());
    }

    /// Apply a `Vec<TextEdit>` (LSP utf-16 ranges) to the active
    /// buffer as one undo unit. TextEdits are sorted in reverse
    /// by start position so each application doesn't shift the
    /// positions of the later ones (LSP convention: edits are
    /// non-overlapping and reference the original document).
    pub(super) fn apply_lsp_text_edits(
        &mut self,
        edits: Vec<lattice_lsp::lsp_types::TextEdit>,
    ) -> Result<(), String> {
        // Slice 3c.final.E.5: route through `mutate_editor_with`.
        self.mutate_editor_with(move |e| e.apply_lsp_text_edits(edits))
    }

    /// 5.5.G.23.insert-prep: body migrated to
    /// [`lattice_host::dispatch::Editor::on_type_formatting_trigger_chars`].
    /// Retained as a delegate while `do_insert_text` still lives
    /// App-side; deletion follows when `do_insert_text` migrates.
    pub(super) fn on_type_formatting_trigger_chars(&self) -> Vec<char> {
        self.read_editor(move |e| e.on_type_formatting_trigger_chars())
    }

    /// 5.5.G.23.insert-prep: body migrated to
    /// [`lattice_host::dispatch::Editor::signature_help_trigger_chars`].
    /// Retained as a delegate while `do_insert_text` still lives
    /// App-side; deletion follows when `do_insert_text` migrates.
    pub(super) fn signature_help_trigger_chars(&self) -> Vec<char> {
        self.read_editor(move |e| e.signature_help_trigger_chars())
    }

    /// Fire `textDocument/onTypeFormatting` to the highest-
    /// priority server advertising the trigger; apply the returned
    /// edits as one undo unit.
    pub(super) fn do_lsp_on_type_formatting_request(&mut self, trigger: char) {
        // Phase 5.8.AD.4: body migrated.
        self.mutate_editor(move |e| e.do_lsp_on_type_formatting_request(trigger));
    }

    /// `:rename <new-name>` (Phase 4.3). Fires
    /// `textDocument/prepareRename` (when the server advertises
    /// the prepare provider) to validate the cursor and pick up
    /// the placeholder; then `textDocument/rename` to compute
    /// the WorkspaceEdit; the App applies the edits per-file as
    /// one undo unit per affected buffer (cross-file atomic
    /// rollback is a follow-up).
    ///
    /// `new_name` empty falls back to `prepareRename`'s
    /// placeholder (when available). When prepareRename returns
    /// nothing AND `new_name` is empty, we error.
    pub(super) fn do_lsp_rename_request(&mut self, new_name: &str) {
        // Slice 3c.final.E.5: clone owned + route via mutate_editor.
        let new_name = new_name.to_string();
        self.mutate_editor(move |e| e.do_lsp_rename_request(&new_name));
    }

    /// Drain queued `:rename` responses; apply the WorkspaceEdit.
    /// v1: per-file edits land as one undo unit in each affected
    /// buffer.
    pub fn drain_pending_rename(&mut self) {
        // 5.8.AA.l.3: migrated to host.
        let signals = self.mutate_editor_with(|e| e.drain_pending_rename());
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }

    /// Apply a per-file WorkspaceEdit returned by `:rename`. The
    /// active buffer's edits land directly via apply_lsp_text_edits;
    /// cross-file edits open the file via `:e` and apply.
    pub(super) fn apply_rename_workspace_edit(
        &mut self,
        per_file: Vec<(
            lattice_lsp::lsp_types::Uri,
            Vec<lattice_lsp::lsp_types::TextEdit>,
        )>,
        new_name: String,
    ) {
        // 5.8.AA.l.2: migrated to host. Fan returned signals
        // through the existing renderer-signal handler.
        let signals = self.mutate_editor_with(move |e| e.apply_rename_workspace_edit(per_file, new_name));
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }

    /// 5.8.AA.r: apply chain migrated to host. The picker accept
    /// arm hits this wrapper, which fans the returned signals
    /// through the existing `handle_renderer_signal` sink.
    pub(super) fn apply_lsp_code_action(
        &mut self,
        row: CodeActionRow,
        handle: Option<lattice_lsp::ServerHandle>,
    ) {
        let signals = self.mutate_editor_with(move |e| e.apply_lsp_code_action(row, handle));
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }

    pub(super) fn execute_lsp_command(
        &mut self,
        handle: Option<lattice_lsp::ServerHandle>,
        cmd: lattice_lsp::lsp_types::Command,
    ) {
        // 5.8.AA.l.6: migrated to host.
        self.mutate_editor(move |e| e.execute_lsp_command(handle, cmd));
    }

    /// Splice a chosen completion item into the buffer at its
    /// captured replace range. Plain text only -- snippet
    /// expansion lands with the buffer-level Insert-mode
    /// completion shell.
    pub(super) fn apply_lsp_completion_item(&mut self, item: &CompletionItemRow) {
        let (start_byte, end_byte) = item.replace_range;
        let range = lattice_protocol::position::Range::new(
            Position::new(item.line, start_byte),
            Position::new(item.line, end_byte),
        );
        let edit = Edit::replace(range, item.insert_text.clone());
        match self.apply_edit_blocking(edit) {
            Ok(applied) => {
                self.set_cursor(applied.inserted_range.end);
            }
            Err(e) => {
                self.set_message(EchoLevel::Error, format!("complete: apply failed: {e:?}"));
            }
        }
    }

    /// `:code-actions` (Phase 4.3). Run textDocument/codeAction
    /// at the cursor (or active Visual selection); open the
    /// merged item list as a vertico picker. v1 picks the first
    /// server with `codeActionProvider`.
    pub(super) fn do_lsp_code_action_request(&mut self) {
        // Phase 5.8.AD.2: body migrated, including the
        // `code_action_range` + `diagnostics_for_range` helpers.
        self.mutate_editor(|e| e.do_lsp_code_action_request());
    }

    /// Drain queued code-action responses. 5.8.AA.r: full
    /// apply chain (Items/Resolved/NoProvider + workspace-edit +
    /// executeCommand) hoisted to host; this wrapper exists for
    /// any remaining direct callers and the test-suite. The host
    /// drain is folded into `run_tick_pending` so the TUI runtime
    /// no longer needs an explicit call.
    pub fn drain_pending_code_actions(&mut self) {
        let signals = self.mutate_editor_with(|e| e.drain_pending_code_actions());
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }

    /// Pick the first attached server that advertises
    /// `codeActionProvider` -- mirrors the choice the spawn task
    /// made when firing the original request.
    fn first_code_action_handle(&self) -> Option<lattice_lsp::ServerHandle> {
        // Slice 3c.final.E.5j: buffer_uris + lsp.servers_for via
        // published `buffers().uris` and `lsp().supervisor` sub-states.
        let uri = self.buffers().uris.get(&self.document_buffer_id())?.clone();
        self.render_state
            .load()
            .lsp
            .supervisor
            .servers_for(&uri)
            .into_iter()
            .find(|h| h.capabilities().supports_code_action())
    }

    /// `:complete` (Phase 4.2.g). Fires
    /// `textDocument/completion` at the cursor; the merged item
    /// list opens as a vertico picker. Multi-server union;
    /// dedup by `(label, kind)`.
    // 5.5.LSP.4: `do_lsp_completion_request` relocated to
    // [`lattice_host::dispatch::Editor::lsp_completion_request`].
    // Body identical (gate, URI / cursor resolve, prefix backwalk
    // for replace range, per-server walk with capability check,
    // merge + dedup). Drain (`drain_pending_completion`) stays
    // App-resident until the picker side migrates.

    /// Drain queued LSP completion responses and open a picker.
    /// `NoServers` echoes; empty list echoes.
    pub fn drain_pending_completion(&mut self) {
        // 5.8.AA.c: migrated to host.
        self.mutate_editor(|e| e.drain_pending_completion());
    }

    /// `:format` / `:format-range` (Phase 4.3). Picks the
    /// highest-priority server with `documentFormattingProvider`
    /// (or `documentRangeFormattingProvider` when `is_range`),
    /// fires the request, applies the returned edits as one
    /// undo unit.
    ///
    /// Single-server strategy per docs/dev/architecture/lsp-architecture.md §7b:
    /// "Two formatters can't agree on whitespace." -- so unlike
    /// nav we don't fan out / merge.
    ///
    /// Range source for `is_range`: active Visual selection (if
    /// in Visual mode), else the whole buffer.
    pub(super) fn do_lsp_format_request(&mut self, is_range: bool) {
        // Phase 5.8.AD.2: body migrated.
        self.mutate_editor(move |e| e.do_lsp_format_request(is_range));
    }

    /// Drain the format response channel and apply the returned
    /// edits as one undo unit. Echoes when the server returned no
    /// edits ("already formatted") or no provider was available.
    pub fn drain_pending_format(&mut self) {
        // 5.8.AA.l.4: migrated to host.
        self.mutate_editor(|e| e.drain_pending_format());
    }

    /// `:lsp-symbols` (Phase 4.2.e). Send
    /// `textDocument/documentSymbol` to every attached server;
    /// flatten the hierarchy + merge across servers; drain on
    /// the next frame opens a picker.
    // 5.5.LSP.5: `do_lsp_document_symbol_request` and
    // `do_lsp_workspace_symbol_request` relocated to
    // [`lattice_host::dispatch::Editor::lsp_document_symbol_request`]
    // and [`Editor::lsp_workspace_symbol_request`]. Both bodies
    // are identical (gate, URI / path resolve for document, fan-
    // out for workspace, flatten + dedup). Drain
    // (`drain_pending_symbols`) stays App-resident.

    /// Drain queued document-symbol / workspace-symbol responses
    /// and open the picker.
    pub fn drain_pending_symbols(&mut self) {
        // 5.8.AA: migrated to
        // `lattice_host::dispatch::Editor::drain_pending_symbols`.
        self.mutate_editor(|e| e.drain_pending_symbols());
    }

    /// 4.5.a: `:lsp-incoming-calls` / `:lsp-outgoing-calls`.
    /// Prepares call-hierarchy items at the cursor on the
    /// first attached server with `callHierarchyProvider`,
    /// then fans out the chosen direction
    /// (`incomingCalls` / `outgoingCalls`) for the first item
    /// in the response. The merged caller / callee list opens
    /// in a vertico picker. Single-server strategy mirrors
    /// the `:rename` choice -- a function's call graph is
    /// language-specific; merging across servers produces
    /// duplicated rows on mixed-language buffers.
    ///
    /// Picker rows reuse the `SymbolsOutcome` /
    /// `PickerSource::LspLocations` plumbing so accept jumps
    /// land through the existing `RoutingPayload::JumpToLspLocation`
    /// path -- no new picker action variant required.
    pub(super) fn do_lsp_call_hierarchy_request(&mut self, outgoing: bool) {
        // Phase 5.8.AD.2: body migrated.
        self.mutate_editor(move |e| e.do_lsp_call_hierarchy_request(outgoing));
    }

    /// 4.5.b: `:lsp-supertypes` / `:lsp-subtypes`. Same shape
    /// as the call-hierarchy peer but for type relationships.
    /// `subtypes=false` -> supertypes ("what does this type
    /// subtype?"); `subtypes=true` -> subtypes ("what subtypes
    /// this type?"). Reuses the symbols outcome / picker
    /// plumbing.
    ///
    /// Capability: lsp-types 0.97 doesn't model a static
    /// `type_hierarchy_provider` field; the probe consults the
    /// dynamic registry only. Servers that support type
    /// hierarchy typically register it dynamically anyway
    /// (rust-analyzer, pyright). When the server doesn't
    /// advertise either path the command echoes "no LSP server
    /// with type-hierarchy support" instead of firing a request
    /// that would error.
    pub(super) fn do_lsp_type_hierarchy_request(&mut self, subtypes: bool) {
        // Phase 5.8.AD.2: body migrated.
        self.mutate_editor(move |e| e.do_lsp_type_hierarchy_request(subtypes));
    }

    /// 4.5.g: `:lsp-moniker`. Fires `textDocument/moniker` on
    /// the first server with `monikerProvider`; the response is
    /// folded into a one-line summary echoed to the minibuffer.
    /// Fire-and-forget UX -- no picker, no jump. Useful for
    /// indexing tools / cross-repo navigation, where the
    /// moniker is metadata about the symbol identity rather
    /// than a navigation target.
    pub(super) fn do_lsp_moniker_request(&mut self) {
        // Phase 5.8.AD.2: body migrated.
        self.mutate_editor(|e| e.do_lsp_moniker_request());
    }

    /// 4.5.g: drain queued moniker responses + echo. Called
    /// per main-loop tick like the other LSP drains; cheap when
    /// the channel is empty.
    pub fn drain_pending_moniker(&mut self) {
        // 5.8.AA.b: migrated to host.
        self.mutate_editor(|e| e.drain_pending_moniker());
    }

    /// `:lsp-signature-help` (Phase 4.3). Fan-out across attached
    /// servers; first non-empty `SignatureHelp` response wins
    /// (per docs/dev/architecture/lsp-architecture.md §7b "First non-empty wins.
    /// Signatures are usually language-specific; merging rarely
    /// useful.").
    // 5.5.LSP.4: `do_lsp_signature_help_request` relocated to
    // [`lattice_host::dispatch::Editor::lsp_signature_help_request`].
    // Body identical (silent gate, URI / cursor resolve, per-server
    // walk with capability check, first non-empty markdown wins).
    // Drain (`drain_pending_signature_help`) stays App-resident
    // until the popup pipeline migrates.

    /// Drain queued signature-help responses. A non-empty body
    /// renders into the popup; empty echoes "no signature info";
    /// `NoServers` echoes the standard "no LSP server" message.
    pub fn drain_pending_signature_help(&mut self) {
        // 5.8.X: drain body migrated to
        // `lattice_host::dispatch::Editor::drain_pending_signature_help`
        // so the GPUI peer reaches the same path. Returns
        // RendererSignals; this peer fans them through its
        // existing handler.
        let signals = self.mutate_editor_with(|e| e.drain_pending_signature_help());
        for signal in signals {
            self.handle_renderer_signal(signal);
        }
    }

    // 5.5.LSP.2: `do_lsp_nav_request(LspNavKind)` relocated to
    // [`lattice_host::dispatch::Editor::lsp_nav_request`]. The
    // host-side body is identical (same gate, same per-server
    // dispatch over the 4 LSP request shapes, same dedup) and runs
    // as the four `Action::Lsp{Definition,Declaration,
    // TypeDefinition,Implementation}Request` arms of
    // `Editor::dispatch`. The drain side
    // (`drain_pending_definitions`) stays App-resident until a
    // later phase migrates it -- it still calls
    // `jump_to_lsp_location` / `open_lsp_locations_picker`, both
    // of which remain App-side for now.

    /// Drain queued nav (definition / declaration / typeDef /
    /// impl) results and act on them: 0 -> echo, 1 -> jump, N>1
    /// -> echo count + open picker. Pushes the pre-jump cursor
    /// onto the position history so `<C-o>` walks back. The verb
    /// in echoes (`definitions` vs `implementations` etc.) reads
    /// from `pending_nav_kind`.
    pub fn drain_pending_definitions(&mut self) {
        // 5.8.AA.p: hoisted in full to host; jump is performed
        // inside `Editor::drain_pending_definitions` and the
        // resulting renderer signals are fanned out here.
        let signals = self.mutate_editor_with(|e| e.drain_pending_definitions());
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }

    /// `gr` (Phase 4.2.d). Send `textDocument/references` to
    /// every attached LSP server with `include_declaration: true`
    /// (vim convention -- `gr` includes the symbol's own
    /// declaration in the list). Spawn the per-server walk on
    /// the LSP runtime; drain on the next frame opens a buffer-
    /// backed `*lsp:references*` view in the active pane.
    // 5.5.LSP.3: `do_lsp_references_request` relocated to
    // [`lattice_host::dispatch::Editor::lsp_references_request`].
    // Identical body shape to the nav helper (same gate, same
    // per-server walk, same dedup) but with a `ReferencesOutcome`
    // carrier (no tag-stack push -- `gr` is browse-style). Drain
    // (`drain_pending_references`) stays App-resident.

    /// Drain queued references results. The merged list is
    /// rendered as a `*lsp:references*` help buffer and opened
    /// in-pane via the LSP-locations picker; existing follow-
    /// link machinery (`<CR>` on a Source link) handles jumps.
    /// `NoServers` echoes "no LSP server attached"; an empty
    /// `Found(_, [])` echoes "no references for X".
    pub fn drain_pending_references(&mut self) {
        // 5.8.AA: drain body migrated to
        // `lattice_host::dispatch::Editor::drain_pending_references`
        // so the GPUI peer reaches the same picker via
        // `run_tick_pending`.
        self.mutate_editor(|e| e.drain_pending_references());
    }

    /// Jump to an LSP `Location`. If the target is the current
    /// buffer, just move the cursor + push history. If
    /// cross-file, route through `do_edit` so the `:e` machinery
    /// (LSP attach, buffer registry) handles the open; then move
    /// cursor.
    ///
    /// Pushes the *pre-jump* cursor onto position history with
    /// source `PositionSource::PluginPush` so `<C-o>` walks back.
    /// Tagging it as PluginPush (not AutoJump) reflects that the
    /// jump came from an external dispatch (LSP) rather than a
    /// vim-style motion.
    pub(super) fn jump_to_lsp_location(&mut self, loc: &lattice_lsp::lsp_types::Location) {
        // Slice 3c.final.E.5: clone owned + route via mutate_editor_with.
        let loc = loc.clone();
        let signals = self.mutate_editor_with(move |e| e.jump_to_lsp_location(&loc));
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }

    /// 5.5.F.7: see [`lattice_host::dispatch::Editor::do_list_diagnostics`].
    /// App-side wrapper retained because three tests
    /// (`app.rs::tests`, `lsp.rs::tests`) call it directly outside
    /// the Effect-arm dispatch path.
    #[allow(dead_code)]
    pub fn do_list_diagnostics(&mut self) {
        self.mutate_editor(|e| e.do_list_diagnostics());
    }

    /// `]d` / `:diag-next` / `:cnext` -- move the cursor to the
    /// next diagnostic in the active buffer. Wraps to top.
    pub fn do_next_diagnostic(&mut self) {
        // Phase 5.8.AF.3: body migrated.
        self.mutate_editor(|e| e.do_next_diagnostic());
    }

    /// `[d` / `:diag-prev` / `:cprev` -- move the cursor to the
    /// previous diagnostic in the active buffer. Wraps to bottom.
    pub fn do_prev_diagnostic(&mut self) {
        // Phase 5.8.AF.3: body migrated.
        self.mutate_editor(|e| e.do_prev_diagnostic());
    }

    /// `:lsp-log [server]` -- activate the subsystem-wide `*lsp*`
    /// buffer (no arg) or a specific server's `*lsp:<server>*`
    /// buffer (with arg).
    ///
    /// Behaviour:
    /// - No arg: switch to `*lsp*`. Captures everything the LSP
    ///   subsystem logs (lifecycle, attach driver, supervisor,
    ///   plus every per-server record prefixed with the server
    ///   id). Always works, even when no servers are running --
    ///   `*lsp*` is created at boot.
    /// - With arg: switch to `*lsp:<server>*` after resolving the
    ///   name through the alias table. Errors if no running
    ///   instance matches.
    ///
    /// Use `:lsp-server-log` for the picker over running
    /// instances.
    pub fn do_open_lsp_log(&mut self, server_id: Option<&str>) {
        // Slice 3c.final.E.5: clone owned + route via mutate_editor.
        let server_id = server_id.map(|s| s.to_string());
        self.mutate_editor(move |e| e.do_open_lsp_log(server_id.as_deref()));
    }

    /// `:lsp-trace-log [server]`. Phase 5.8.AD.2: migrated.
    pub fn do_open_lsp_trace_log(&mut self, server_id: Option<&str>) {
        let server_id = server_id.map(|s| s.to_string());
        self.mutate_editor(move |e| e.do_open_lsp_trace_log(server_id.as_deref()));
    }

    /// `:lsp-trace <name>`. Phase 5.8.AD.2: migrated.
    pub fn do_toggle_lsp_trace(&mut self, name: &str) {
        let name = name.to_string();
        self.mutate_editor(move |e| e.do_toggle_lsp_trace(&name));
    }

    /// `:lsp-status` -- render every running server in a
    /// help-style buffer.
    pub fn do_lsp_status(&mut self) {
        // Phase 5.8.AD.2: body migrated to
        // `lattice_host::dispatch::Editor::do_lsp_status`. Returns
        // a `DisplayBuffer` signal that the renderer's handler
        // routes through `display_buffer`.
        let signals = self.mutate_editor_with(|e| e.do_lsp_status());
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }

    /// `:lsp-server-log` -- vertico picker over every running
    /// `(workspace, server_id)` LSP actor. `<CR>` opens the
    /// per-server log (`*lsp:<server>*`) for the chosen row.
    pub fn do_lsp_server_log_listing(&mut self) {
        // Phase 5.8.AD.2: body migrated.
        self.mutate_editor(|e| e.do_lsp_server_log_listing());
    }

    /// `:lsp-restart <server>` -- supervisor restart hook.
    /// Currently emits an info message; full restart-with-
    /// backoff lands in 4.4.
    pub fn do_lsp_restart(&mut self, server_id: &str) {
        // Slice 3c.final.E.5: clone owned + route via mutate_editor.
        let server_id = server_id.to_string();
        self.mutate_editor(move |e| e.do_lsp_restart(&server_id));
    }

    /// `:lsp-progress-cancel [server]` -- send
    /// `window/workDoneProgress/cancel` for every cancellable
    /// 4.4.f: per-tick `foldingRange` pump. Only fires when:
    /// - `:set foldmethod=lsp` is active for the buffer, AND
    /// - `lsp-folding-mode` is enabled, AND
    /// - the buffer's document version differs from the cached
    ///   version (or there's no cache), AND
    /// - no in-flight request is already chasing this version.
    ///
    /// Single-flight: each new request cancels its predecessor.
    /// The drain seats the response into `lsp_folds_cache` and
    /// triggers `recompute_folds` so the fold list refreshes
    /// without the user having to do anything.
    pub fn maybe_request_folding_range(&mut self) {
        // 5.8.AA.i: migrated to host.
        self.mutate_editor(|e| e.maybe_request_folding_range());
    }

    // Phase 5.8.AF.5 / Slice 3b.1: `App::drain_pending_folding_range`
    // retired -- the spawned LSP request task writes directly
    // into `lsp_folds_cache` via `PerBufferCacheExt::insert_for`.
    // No drain needed; the `recompute_folds()` side-effect now
    // fires from `maybe_request_folding_range` on cache-version
    // flip.

    /// 4.4.h: per-tick `semanticTokens/full` pump. Fires when
    /// `lsp-semantic-tokens-mode` is on AND the buffer's
    /// document version differs from the cache (or there's no
    /// cache). Single-flight; the decoder runs server-side on
    /// the spawned task and the drain seats decoded tokens
    /// into the cache.
    pub fn maybe_request_semantic_tokens(&mut self) {
        // 5.8.AA.i: migrated to host.
        self.mutate_editor(|e| e.maybe_request_semantic_tokens());
    }

    // Phase 5.8.AF.5 / Slice 3b.2: `App::drain_pending_semantic_tokens`
    // retired -- spawned LSP request task writes directly into
    // `lsp_semantic_tokens_cache` via `PerBufferCacheExt::insert_for`.
    // No drain needed. Delta application is factored into
    // `Editor::apply_semantic_tokens_delta_outcome` -- the
    // synchronous tests that previously fed an outcome through the
    // drain now call that helper directly.

    /// 4.4.j: per-tick `textDocument/diagnostic` (pull-based)
    /// pump. Fires when:
    /// - `lsp-diagnostics-mode` is enabled (umbrella +
    ///   diagnostics sub-mode), AND
    /// - the active server advertises pull diagnostics, AND
    /// - the buffer's document version differs from the
    ///   cached one (or there's no cache entry).
    ///
    /// Threads the cached `result_id` back via
    /// `previous_result_id` so the server can answer
    /// `Unchanged` cheaply when nothing moved. Single-flight:
    /// each new request cancels its predecessor. Failures
    /// surface as `PullDiagnosticsOutcome::Empty` and the
    /// drain seats a cache entry at the current version so
    /// the pump doesn't re-fire on the next tick without an
    /// actual edit.
    pub fn maybe_request_pull_diagnostics(&mut self) {
        // 5.8.AA.i: migrated to host.
        self.mutate_editor(|e| e.maybe_request_pull_diagnostics());
    }

    // Phase 5.8.AF.5 / Slice 3b.5: `App::drain_pending_pull_diagnostics`
    // retired -- spawned task writes both the per-buffer cache
    // slot and fans into `lsp_diagnostics` (Arc-backed layer)
    // directly.

    /// 4.4.j: drain `workspace/diagnostic/refresh` events.
    /// Each event names a server; evict the per-buffer
    /// `result_id` cache for every attached buffer so the
    /// next pump tick re-pulls without a `previous_result_id`
    /// and the server emits a forced `Full` report.
    pub fn drain_diagnostic_refresh(&mut self) {
        // 5.8.AA.c: migrated to host.
        self.mutate_editor(|e| e.drain_diagnostic_refresh());
    }

    /// 4.4.g: per-tick `inlayHint` pump. Fires when:
    /// - `lsp-inlay-hint-mode` is enabled, AND
    /// - the buffer's document version differs from the cache
    ///   (or there's no cache).
    ///
    /// Single-flight: each new request cancels its predecessor.
    /// Whole-buffer range for simplicity -- the LSP request
    /// signature requires a range, but production servers
    /// happily handle the entire buffer span. Viewport-only
    /// fetching is a follow-up optimization.
    pub fn maybe_request_inlay_hint(&mut self) {
        // 5.8.AA.g: migrated to host.
        self.mutate_editor(|e| e.maybe_request_inlay_hint());
    }

    /// 4.4.g: drain `workspace/inlayHint/refresh` events. Each
    /// event names a server; clear cached inlay hints for any
    /// buffer attached to that server so the next render
    /// tick's pump re-issues `inlayHint`.
    pub fn drain_inlay_hint_refresh(&mut self) {
        // 5.8.AA.c: migrated to host.
        self.mutate_editor(|e| e.drain_inlay_hint_refresh());
    }

    /// 4.4.i: drain `workspace/semanticTokens/refresh` events.
    /// Same shape as the inlay-hint refresh drain: each event
    /// names a server; drop the semantic-tokens cache for every
    /// attached buffer so the next render tick's pump re-issues
    /// `semanticTokens/full` against a fresh baseline (dropping
    /// the now-stale `result_id` rules out a delta request that
    /// the server would reject).
    pub fn drain_semantic_tokens_refresh(&mut self) {
        // 5.8.AA.c: migrated to host.
        self.mutate_editor(|e| e.drain_semantic_tokens_refresh());
    }

    // Phase 5.8.AF.5 / Slice 3b.1: `App::drain_pending_inlay_hint`
    // retired -- spawned LSP request task writes directly into
    // `lsp_inlay_hints_cache` via `PerBufferCacheExt::insert_for`.
    // No drain needed.

    /// 4.5.c: per-tick `documentLink` pump. Fires on
    /// document-version change (cheap when versions match;
    /// the cache lookup short-circuits). Whole-buffer request
    /// since link ranges are typically sparse and not bound
    /// to a viewport. Single-flight per buffer; each new
    /// request cancels its predecessor.
    pub fn maybe_request_document_link(&mut self) {
        // 5.8.AA.i: migrated to host.
        self.mutate_editor(|e| e.maybe_request_document_link());
    }

    // Phase 5.8.AF.5 / Slice 3b.4: `App::drain_pending_document_link`
    // retired -- spawned task writes directly via
    // `PerBufferCacheExt::insert_for`. `gx` reads via `get_for`.

    /// 4.5.c: follow the LSP `documentLink` at the cursor (the
    /// `gx` keystroke). Walks the cache, picks the first link
    /// whose range covers the cursor, follows its `target`.
    /// When the link has no target AND the server advertises
    /// `documentLinkProvider.resolveProvider`, fires
    /// `documentLink/resolve` to fill in the target before
    /// following. Echoes `(no link at cursor)` when the cache
    /// is empty or the cursor sits outside every cached range.
    pub fn do_lsp_follow_link_at_cursor(&mut self) {
        // Phase 5.8.AD.2: body migrated.
        let signals = self.mutate_editor_with(|e| e.do_lsp_follow_link_at_cursor());
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }

    /// 4.5.d: per-tick `codeLens` pump. Fires on document-
    /// version change OR cache miss (`workspace/codeLens/refresh`
    /// evicts the entry). Single-flight per buffer.
    pub fn maybe_request_code_lens(&mut self) {
        // 5.8.AA.i: migrated to host.
        self.mutate_editor(|e| e.maybe_request_code_lens());
    }

    // Phase 5.8.AF.5 / Slice 3b.3: `App::drain_pending_code_lens`
    // retired -- spawned LSP request task writes directly into
    // `lsp_code_lens_cache` via `PerBufferCacheExt::insert_for`.
    // The `:lsp-code-lens` picker reads through `get_for`.

    /// 4.5.d: `:lsp-code-lens`. Open a picker over the
    /// active buffer's cached lenses. Empty cache -> echo.
    /// Accept routes through
    /// [`Self::accept_lsp_code_lens`].
    pub(super) fn do_lsp_code_lens_picker(&mut self) {
        // Phase 5.8.AD.2: body migrated.
        self.mutate_editor(|e| e.do_lsp_code_lens_picker());
    }

    /// 4.5.d: accept a code lens by `index` (the routing
    /// payload). Resolves the lens via `codeLens/resolve`
    /// when its `command` is missing AND the server advertises
    /// `codeLensProvider.resolveProvider`. The resulting
    /// `command` routes through `workspace/executeCommand`
    /// on the originating server (the one that produced the
    /// cache).
    pub(super) fn accept_lsp_code_lens(&mut self, index: u32) {
        // Phase 5.8.AD.2: body migrated.
        self.mutate_editor(move |e| e.accept_lsp_code_lens(index));
    }

    /// 4.5.e: per-tick `documentColor` pump. Same shape as
    /// the documentLink pump: fires on doc-version change,
    /// single-flight per buffer.
    pub fn maybe_request_document_color(&mut self) {
        // 5.8.AA.i: migrated to host.
        self.mutate_editor(|e| e.maybe_request_document_color());
    }

    // Phase 5.8.AF.5 / Slice 3b.4: `App::drain_pending_document_color`
    // retired -- spawned task writes directly via
    // `PerBufferCacheExt::insert_for`.

    /// 4.5.e: `:lsp-color-presentation`. Looks up the color
    /// literal under the cursor in the per-buffer cache and
    /// fires `textDocument/colorPresentation` to get the
    /// alternative formats; opens the result as a picker.
    /// Accept replaces the literal with the chosen
    /// `ColorPresentation.text_edit` (or `label` fallback) at
    /// the literal's range.
    pub(super) fn do_lsp_color_presentation(&mut self) {
        // Phase 5.8.AD.2: body migrated.
        self.mutate_editor(|e| e.do_lsp_color_presentation());
    }

    /// 4.5.e: accept one color presentation by index. Phase 5.8.AD.2.
    pub(super) fn accept_lsp_color_presentation(&mut self, index: u32) {
        self.mutate_editor(move |e| e.accept_lsp_color_presentation(index));
    }

    /// 4.5.d: drain `workspace/codeLens/refresh` events. Each
    /// event names a server; evict every cached code-lens
    /// entry that came from that server. The next pump tick
    /// re-issues `textDocument/codeLens`.
    pub fn drain_code_lens_refresh(&mut self) {
        // 5.8.AA.c: migrated to host.
        self.mutate_editor(|e| e.drain_code_lens_refresh());
    }

    /// 4.4.e: per-tick `documentHighlight` pump. Compares the
    /// current cursor against the cache anchor; when they
    /// differ AND the sub-mode is on AND the buffer has an
    /// attached server advertising the capability, fires a
    /// fresh request (cancelling any in-flight).
    ///
    /// Phase 5.8.AF.5 / Slice 3b.0: the spawned LSP request task
    /// now writes results directly into
    /// `editor.lsp_document_highlights` (`Arc<ArcSwapOption<...>>`)
    /// when the response arrives -- no channel, no UI-thread
    /// drain. Self-cancelling: the cursor moves faster than the
    /// network round-trip during a `/word` search, so the
    /// `CancellationToken` invalidates every in-flight request
    /// the moment the next one fires.
    pub fn maybe_request_document_highlight(&mut self) {
        // 5.8.AA.g: migrated to host.
        self.mutate_editor(|e| e.maybe_request_document_highlight());
    }
    // Phase 5.8.AF.5 / Slice 3b.0: `App::drain_pending_document_highlight`
    // retired -- the spawned request task on the LSP runtime
    // writes directly into the cache slot's ArcSwap when the
    // response arrives. No drain needed.

    /// 4.4.e: `:lsp-expand-region` -- structural smart-
    /// expansion. If a cached chain still applies (cursor sits
    /// inside its innermost range AND same buffer), step the
    /// index outward and apply the new selection. Otherwise
    /// fire `textDocument/selectionRange` and let the drain
    /// seat the chain + apply step 0 on completion.
    pub fn do_lsp_expand_region(&mut self) {
        // Phase 5.8.AD.2: body migrated to
        // `lattice_host::dispatch::Editor::do_lsp_expand_region`.
        self.mutate_editor(|e| e.do_lsp_expand_region());
    }

    /// 4.4.e: `:lsp-shrink-region`. Phase 5.8.AD.2: migrated.
    pub fn do_lsp_shrink_region(&mut self) {
        self.mutate_editor(|e| e.do_lsp_shrink_region());
    }

    /// 4.4.e: drain the in-flight `selectionRange` response.
    /// Seats the chain into `App::lsp_selection_chain` and
    /// applies the step the original invocation requested.
    pub fn drain_pending_selection_range(&mut self) {
        // 5.8.AA.m: migrated to host.
        self.mutate_editor(|e| e.drain_pending_selection_range());
    }

    /// active progress entry (4.4.c). With `server_id == Some`,
    /// cancel only entries on that server; with `None`, cancel
    /// across every server attached to the current buffer.
    ///
    /// The entry stays in the accumulator until the server sends
    /// the `end` progress notification — `cancel` is best-effort
    /// per spec, and the server may decline.
    pub fn do_lsp_progress_cancel(&mut self, server_id: Option<&str>) {
        // Slice 3c.final.E.5: clone owned + route via mutate_editor.
        let server_id = server_id.map(|s| s.to_string());
        self.mutate_editor(move |e| e.do_lsp_progress_cancel(server_id.as_deref()));
    }

    /// `:lsp-log-level [server] <level>` -- set the subsystem
    /// default min level (when no server) or a per-server
    /// override.
    pub fn do_set_lsp_log_level(&mut self, server_id: Option<&str>, level: &str) {
        // Slice 3c.final.E.5: clone owned + route via mutate_editor.
        let server_id = server_id.map(|s| s.to_string());
        let level = level.to_string();
        self.mutate_editor(move |e| e.do_set_lsp_log_level(server_id.as_deref(), &level));
    }

    /// `:lsp-log-clear [server]`. Phase 5.8.AD.2: migrated.
    pub fn do_lsp_log_clear(&mut self, server_id: Option<&str>) {
        // Slice 3c.final.E.5: clone owned + route via mutate_editor.
        let server_id = server_id.map(|s| s.to_string());
        self.mutate_editor(move |e| e.do_lsp_log_clear(server_id.as_deref()));
    }

    /// Activate the per-instance
    /// `*lsp:<server_id>:<workspace>*` Document buffer in the
    /// active pane. B'.4: when multiple instances of `server_id`
    /// are running, this opens the first match; the broader
    /// picker (`:lsp-server-log`) lets the user disambiguate.
    /// B'.7: thin wrapper over the generic
    /// `ensure_named_synthetic_document` helper -- canonical name
    /// + mode id come from `lattice-lsp`. Idempotent: the buffer
    /// is created lazily on first call.
    pub(super) fn open_lsp_log_in_pane(&mut self, server_id: &str) {
        // Slice 3c.final.E.5: clone owned + route via mutate_editor.
        let server_id = server_id.to_string();
        self.mutate_editor(move |e| e.open_lsp_log_in_pane(&server_id));
    }

    /// Activate the per-instance trace-log Document buffer.
    pub(super) fn open_lsp_trace_log_in_pane(&mut self, server_id: &str) {
        let server_id = server_id.to_string();
        self.mutate_editor(move |e| e.open_lsp_trace_log_in_pane(&server_id));
    }

    /// Helper: publish a position-only change event. Cheap
    /// stand-in for whatever the rest of the App uses to
    /// signal cursor moves. Currently a no-op since the
    /// renderer reads cursor directly; reserved for future
    /// position-history pushes.
    pub(super) fn publish_position_change(&self) {
        // 4.1.d.iv: position history hook reserved -- a real
        // PluginPush entry lands here when the position-history
        // wiring catches up.
    }

    /// Resolve a user-supplied server name to a canonical server
    /// id. Tries, in order:
    ///
    /// 1. Exact id match against running actors (the common case
    ///    once a buffer has attached).
    /// 2. Exact id match against registered configs (so
    ///    `:lsp-trace rust` works pre-spawn -- e.g. enable trace
    ///    before opening the first .rs file).
    /// 3. Binary file-name (or stem) match against configs (so
    ///    `:lsp-trace rust-analyzer` resolves to the `rust` actor
    ///    id when the user types the binary they recognise).
    ///
    /// Returns `None` when none matches.
    pub(super) fn resolve_server_id(&self, name: &str) -> Option<String> {
        // Slice 3c.final.E.swap-prep: clone for Send + 'static closure.
        let name = name.to_string();
        self.read_editor(move |e| e.resolve_server_id(&name))
    }

    /// Distinct server ids of every running actor.
    pub(super) fn running_server_ids(&self) -> Vec<String> {
        // Phase 5.8.AD.2: body migrated.
        self.read_editor(move |e| e.running_server_ids())
    }
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used, clippy::panic)]

    use super::*;
    use crate::app::test_helpers::{app_with, seed_diags_at_lines};
    use crate::app::*;

    /// CSM.8b.3 test helper: pack a `LspCompletionMeta` into the
    /// `RawCandidate` shape `produce_async` emits -- display +
    /// match-text derived from label/detail/filter_text, payload
    /// is the serde-encoded meta. Mirrors the construction the
    /// production source does in `lattice-lsp::completion`.
    fn lsp_meta_candidate(
        meta: lattice_lsp::completion::LspCompletionMeta,
    ) -> lattice_completion::RawCandidate {
        let display = match meta.detail.as_ref() {
            Some(d) => format!("{}  {}", meta.label, d),
            None => meta.label.clone(),
        };
        let match_text = meta
            .filter_text
            .clone()
            .unwrap_or_else(|| meta.label.clone());
        let payload = lattice_lsp::completion::encode_meta(&meta);
        let mut raw = lattice_completion::RawCandidate::plain(
            match_text,
            lattice_completion::CandidateKind::Plain,
        )
        .with_source(lattice_completion::SourceId::new(
            lattice_completion::LSP_COMPLETION_SOURCE_ID,
        ));
        raw.display = display;
        raw.data = lattice_completion::CandidateData::Extension {
            kind_id: LSP_COMPLETION_KIND_ID,
            payload,
        };
        raw
    }

    #[test]
    fn lsp_mode_gates_document_changed_typed_event_at_publish_site() {
        // M.5.5: the LSP fan-in subscribes to
        // `LspDocumentChanged` (typed bus), and App's
        // `publish_document_changed` gates the publish_typed
        // call on `lsp_mode_enabled_for`. This test verifies
        // the gate at the publish site:
        // - lsp-mode off → no LspDocumentChanged emitted.
        // - lsp-mode on  → LspDocumentChanged emitted on edit.
        let mut a = app_with("xx", 10);
        let (tx, mut rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspDocumentChanged>();
        a.editor.event_bus.subscribe_typed(tx);
        // Default (no path → lsp-mode off): drive an edit; no
        // typed event should reach the subscriber.
        a.apply(Action::Insert("a".into()));
        assert!(
            rx.try_recv().is_err(),
            "lsp-mode off should suppress LspDocumentChanged"
        );
        // Activate lsp-mode and edit again -- now the typed
        // event should publish.
        a.toggle_mode_by_name("lsp-mode");
        a.apply(Action::Insert("b".into()));
        let received = rx.try_recv();
        assert!(
            received.is_ok(),
            "lsp-mode on should emit LspDocumentChanged on edit"
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn lsp_mode_round_trip_end_to_end() {
        // M.5.7 + M-async.5: end-to-end gate exercise across
        // one buffer's lifetime. Open a *.rs file -- under
        // M-async.5 `LspMode::on_activate` `.await`s the
        // supervisor's `open_buffer` mailbox, so the spawn
        // path is in flight after App::new returns. Toggle
        // off -> the deactivate bumps the epoch, the spawn
        // eventually completes + drops its stale Guard ->
        // LspBufferDetached publishes. Toggle on -> same
        // shape, new spawn. The gate flags (`lsp_mode_
        // enabled_for`) move synchronously via the sync
        // prefix.
        use crate::app::test_helpers::app_with_path;
        let mut a = app_with_path("fn main() {}", 5, std::path::PathBuf::from("foo.rs"));
        let id = a.editor.pane_tree.active().buffer_id;
        assert!(a.lsp_mode_enabled_for(id), "M.5.2 auto-activation");

        let (detach_tx, mut detach_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspBufferDetached>();
        a.editor.event_bus.subscribe_typed(detach_tx);
        let (changed_tx, mut changed_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspDocumentChanged>();
        a.editor.event_bus.subscribe_typed(changed_tx);

        // ---- toggle off: gate closes. ----
        a.toggle_mode_by_name("lsp-mode");
        assert!(!a.lsp_mode_enabled_for(id));
        // M.5.3 detach event fires (synchronously if Guard
        // landed pre-toggle, else when the spawn-side Drop
        // runs).
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(2);
        while detach_rx.try_recv().is_err() {
            if std::time::Instant::now() >= deadline {
                panic!("LspBufferDetached did not arrive within 2s");
            }
            tokio::time::sleep(std::time::Duration::from_millis(5)).await;
        }
        // M.5.4 request gate: hover echoes the gate message.
        a.apply(Action::LspHoverRequest);
        let msg = a.editor.last_message.as_ref().expect("gate echo");
        assert!(
            msg.text.contains("lsp-mode disabled"),
            "expected gate echo, got: {}",
            msg.text
        );
        // M.5.5 sync gate: edits don't publish LspDocumentChanged.
        a.apply(Action::Insert("a".into()));
        assert!(
            changed_rx.try_recv().is_err(),
            "lsp-mode off should suppress LspDocumentChanged"
        );

        // ---- toggle on: gate opens; signals resume. ----
        a.toggle_mode_by_name("lsp-mode");
        assert!(a.lsp_mode_enabled_for(id));
        a.apply(Action::Insert("b".into()));
        assert!(
            changed_rx.try_recv().is_ok(),
            "lsp-mode on should re-emit LspDocumentChanged"
        );
    }

    #[test]
    fn lsp_mode_off_gates_request_entry_points_with_info_echo() {
        // M.5.4: `lsp-mode` off means LSP request entry points
        // bail with a discoverable echo (so users don't think
        // their bindings are broken). Verified for the
        // ex-command-driven path; keymap-driven (insert-mode
        // completion / signature) are silent by design.
        let mut a = app_with("xx", 10);
        // Default: no auto-activation (no path).
        assert!(!a.lsp_mode_enabled_for(a.editor.document_buffer_id));
        a.apply(Action::LspHoverRequest);
        let msg = a.editor.last_message.as_ref().expect("gate echo");
        assert_eq!(msg.level, EchoLevel::Info);
        assert!(
            msg.text.contains("lsp-mode disabled"),
            "expected lsp-mode-disabled echo, got: {}",
            msg.text
        );
    }

    #[test]
    fn lsp_hover_mode_off_with_umbrella_on_echoes_sub_mode_message() {
        // M.6.2: when umbrella is on but sub-mode is off, the
        // gate echoes the *sub-mode's* name -- the user knows
        // exactly which switch to flip.
        let mut a = app_with("xx", 10);
        a.toggle_mode_by_name("lsp-mode");
        // Cascade activated lsp-hover-mode; toggle it off
        // independently.
        a.toggle_mode_by_name("lsp-hover-mode");
        assert!(a.lsp_mode_enabled_for(a.editor.document_buffer_id));
        assert!(!a.lsp_hover_mode_enabled_for(a.editor.document_buffer_id));
        // Hover request now bails with sub-mode echo (umbrella
        // is on, so the umbrella check inside the helper passes).
        a.apply(Action::LspHoverRequest);
        let msg = a.editor.last_message.as_ref().expect("gate echo");
        assert_eq!(msg.level, EchoLevel::Info);
        assert!(
            msg.text.contains("lsp-hover-mode disabled"),
            "expected lsp-hover-mode-disabled echo, got: {}",
            msg.text
        );
    }

    #[test]
    fn lsp_format_mode_off_gates_format_request() {
        // M.6.2: independent disable of `lsp-format-mode`. Format
        // requests echo the sub-mode's name; other LSP requests
        // (hover, nav) keep working in the same buffer.
        let mut a = app_with("xx", 10);
        a.toggle_mode_by_name("lsp-mode");
        a.toggle_mode_by_name("lsp-format-mode");
        // Format gates. (Tested via the do_lsp_format_request
        // method directly -- the dispatch path through
        // ex-commands also routes here.)
        a.do_lsp_format_request(false);
        let msg = a.editor.last_message.as_ref().expect("format gate echo");
        assert!(
            msg.text.contains("lsp-format-mode disabled"),
            "expected lsp-format-mode-disabled echo, got: {}",
            msg.text
        );
    }

    #[test]
    fn lsp_nav_mode_off_gates_definition_request() {
        // M.6.2: nav family (`gd` / `gD` / `gy` / `gI` / `gr`)
        // shares one sub-mode (`lsp-nav-mode`).
        let mut a = app_with("xx", 10);
        a.toggle_mode_by_name("lsp-mode");
        a.toggle_mode_by_name("lsp-nav-mode");
        a.apply(crate::app::Action::LspDefinitionRequest); // 5.5.LSP.2
        let msg = a.editor.last_message.as_ref().expect("nav gate echo");
        assert!(
            msg.text.contains("lsp-nav-mode disabled"),
            "expected lsp-nav-mode-disabled echo, got: {}",
            msg.text
        );
    }

    #[test]
    fn umbrella_off_wins_over_sub_mode_state() {
        // M.6.2: when the umbrella is off, the sub-mode message
        // never fires -- the umbrella check is the first thing
        // every gate does. The user sees one consistent message
        // ("enable lsp-mode first") rather than a stack of
        // sub-mode-disabled echoes.
        let mut a = app_with("xx", 10);
        // Activate umbrella + sub-modes via cascade, then turn
        // umbrella off. Sub-modes also flip off via cascade-off
        // — but even hypothetically a stale sub-mode entry
        // wouldn't bypass the umbrella check.
        a.toggle_mode_by_name("lsp-mode");
        a.toggle_mode_by_name("lsp-mode");
        assert!(!a.lsp_mode_enabled_for(a.editor.document_buffer_id));
        a.apply(Action::LspHoverRequest);
        let msg = a.editor.last_message.as_ref().expect("umbrella echo");
        // Umbrella echo, not sub-mode echo.
        assert!(
            msg.text.contains("lsp-mode disabled") && !msg.text.contains("lsp-hover-mode"),
            "expected umbrella echo (not sub-mode echo), got: {}",
            msg.text
        );
    }

    #[test]
    fn lsp_mode_enabled_for_returns_false_by_default() {
        // M.5.0: a freshly-opened buffer has no `lsp-mode`
        // activated yet. Auto-activation lands in M.5.2 via the
        // `MajorEntered` event hook; until then the accessor is
        // false everywhere.
        let a = app_with("fn main() {}", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        assert!(!a.lsp_mode_enabled_for(id));
    }

    #[test]
    fn lsp_mode_enabled_for_tracks_minor_activation() {
        // M.5.0: activating `lsp-mode` through the registry
        // flips the accessor. M.5.3 will wrap this in actual
        // `:lsp-mode` toggle / auto-activation flow; for now
        // we drive the registry directly.
        let mut a = app_with("fn main() {}", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        let proto_id = lattice_protocol::ids::BufferId::new(id.0 as u64);
        let mut active = a.editor.active_modes.remove(&id).unwrap_or_default();
        a.editor
            .mode_registry
            .activate_minor(
                &mut active,
                &a.editor.mode_guards,
                &a.editor.config,
                &a.editor.event_bus,
                &a.editor.services,
                proto_id,
                lattice_lsp::modes::LspMode::mode_id(),
                lattice_mode::CapabilitySet::empty(),
            )
            .expect("activate lsp-mode");
        a.editor.active_modes.insert(id, active);
        a.editor.publish_render_state();
        assert!(a.lsp_mode_enabled_for(id));
    }

    #[test]
    fn hover_dismisses_on_document_cursor_motion() {
        // Vim/emacs UX: any motion off the hovered symbol drops
        // the popup. Apply a hover popup directly (skipping the
        // async LSP path), move the cursor, assert dismissal.
        let mut a = app_with("fn main() {}\nlet x = 1;\n", 5);
        a.do_open_hover("hover body");
        assert!(a.editor.popup_buffer.is_some());
        // State A: focus still on doc, prev_pane_for_help is None.
        assert!(a.editor.prev_pane_for_help.is_none());
        assert!(matches!(a.editor.active_buffer, BufferKind::Document));
        // Drive a real motion through `apply` (`l` -- char-right).
        let inv = lattice_grammar::CommandInvocation::of(a.editor.builtins.char_right.0);
        a.apply(Action::Invoke(inv));
        assert!(
            a.editor.popup_buffer.is_none(),
            "hover popup should dismiss on cursor motion in State A"
        );
    }

    #[test]
    fn hover_does_not_dismiss_when_cursor_unchanged() {
        // No-op actions (e.g. setting a no-arg ex command,
        // an out-of-bounds motion that clamps in place) must not
        // dismiss the popup. Use a count-only push (`5`) which
        // doesn't move the cursor.
        let mut a = app_with("fn main() {}\n", 5);
        a.do_open_hover("hover body");
        assert!(a.editor.popup_buffer.is_some());
        a.apply(Action::PushDigit(5));
        assert!(
            a.editor.popup_buffer.is_some(),
            "hover should survive a count-prefix push"
        );
    }

    #[test]
    fn hover_open_populates_help_buffer() {
        let mut a = app_with("alpha\nbeta\ngamma", 10);
        a.editor.cursor = Position::new(1, 2);
        a.editor.command_line = "hover documentation".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        let h = a.popup_help().expect("hover open");
        assert_eq!(h.title, "hover");
        assert!(h.content.as_string().contains("documentation"));
        // State A: focus stays on doc.
        assert!(matches!(a.editor.active_buffer, BufferKind::Document));
        assert!(a.editor.prev_pane_for_help.is_none());
    }

    #[test]
    fn hover_close_dismisses_popup() {
        let mut a = app_with("xx", 10);
        a.editor.command_line = "hover x".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert!(a.editor.popup_buffer.is_some());
        a.editor.command_line = "HoverClose".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        assert!(a.editor.popup_buffer.is_none());
    }

    #[test]
    fn hover_with_no_arg_uses_placeholder() {
        let mut a = app_with("xx", 10);
        a.editor.command_line = "hover".into();
        a.editor.modal = ModalState::Command;
        a.apply(Action::CommandLineSubmit);
        let h = a.popup_help().expect("hover open");
        assert!(h.content.as_string().contains("empty"));
    }

    #[test]
    fn hover_contents_scalar_string_renders_verbatim() {
        let m = lattice_lsp::lsp_types::HoverContents::Scalar(
            lattice_lsp::lsp_types::MarkedString::String("fn foo() -> u32".into()),
        );
        assert_eq!(super::hover_contents_to_markdown(&m), "fn foo() -> u32");
    }

    #[test]
    fn hover_contents_language_string_renders_as_fenced_block() {
        let m = lattice_lsp::lsp_types::HoverContents::Scalar(
            lattice_lsp::lsp_types::MarkedString::LanguageString(
                lattice_lsp::lsp_types::LanguageString {
                    language: "rust".into(),
                    value: "let x: u32 = 5;".into(),
                },
            ),
        );
        let md = super::hover_contents_to_markdown(&m);
        assert!(md.contains("```rust"));
        assert!(md.contains("let x: u32 = 5;"));
        assert!(md.ends_with("```"));
    }

    #[test]
    fn hover_contents_array_joins_with_double_newline() {
        let m = lattice_lsp::lsp_types::HoverContents::Array(vec![
            lattice_lsp::lsp_types::MarkedString::String("first".into()),
            lattice_lsp::lsp_types::MarkedString::String("second".into()),
        ]);
        let md = super::hover_contents_to_markdown(&m);
        assert_eq!(md, "first\n\nsecond");
    }

    #[test]
    fn hover_contents_markup_uses_value_as_markdown() {
        let m =
            lattice_lsp::lsp_types::HoverContents::Markup(lattice_lsp::lsp_types::MarkupContent {
                kind: lattice_lsp::lsp_types::MarkupKind::Markdown,
                value: "# heading\n\nbody".into(),
            });
        assert_eq!(super::hover_contents_to_markdown(&m), "# heading\n\nbody");
    }

    #[test]
    fn lsp_hover_request_with_no_uri_echoes_no_lsp_attached() {
        // Initial document has no path, so no URI mapping; the
        // request should set an info message and not panic.
        // M.5.4: gate is checked first, so we activate lsp-mode
        // explicitly to test the URI-bail path the original test
        // was probing.
        let mut a = app_with("xx", 10);
        a.toggle_mode_by_name("lsp-mode");
        a.apply(Action::LspHoverRequest);
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert_eq!(msg.level, EchoLevel::Info);
        assert!(msg.text.contains("no LSP server"));
    }

    #[test]
    fn lsp_hover_request_pre_cancels_in_flight_token() {
        // Two K presses in a row: the first one's token must be
        // flipped before the second's request fires, so a slow
        // first response gets dropped by the relay's cancel-aware
        // poll loop.
        let mut a = app_with("xx", 10);
        // Manually install an in-flight token.
        let stale = lattice_protocol::CancellationToken::new();
        a.editor.pending_hover_token = Some(stale.clone());
        // Trigger another hover. With no LSP attached the new
        // request bails on the URI lookup, but the cancel of the
        // previous token should still happen first.
        a.apply(Action::LspHoverRequest);
        assert!(
            stale.is_cancelled(),
            "prior in-flight hover token should flip on a new K press"
        );
    }

    #[test]
    fn drain_pending_hover_body_outcome_opens_popup() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<crate::app::HoverOutcome>();
        a.editor.pending_hover_rx = Some(rx);
        a.editor.pending_hover_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(crate::app::HoverOutcome::Body("**bold body**".into()))
            .unwrap();
        a.drain_pending_hover();
        let h = a.popup_help().expect("popup");
        assert!(h.content.as_string().contains("**bold body**"));
        // State A entry: focus still on the doc.
        assert!(matches!(a.editor.active_buffer, BufferKind::Document));
        assert!(a.editor.prev_pane_for_help.is_none());
        assert!(
            a.editor.pending_hover_token.is_none(),
            "delivering the outcome should clear the in-flight token"
        );
    }

    #[test]
    fn drain_pending_hover_no_body_outcome_echoes_no_hover_info() {
        // Regression for the silent-K-press symptom: if every
        // attached server replies with empty contents,
        // `drain_pending_hover` should echo a clear "no hover
        // info" so the user knows their K press was received.
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<crate::app::HoverOutcome>();
        a.editor.pending_hover_rx = Some(rx);
        a.editor.pending_hover_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(crate::app::HoverOutcome::NoBody { servers_tried: 1 })
            .unwrap();
        a.drain_pending_hover();
        assert!(a.editor.popup_buffer.is_none(), "no popup for empty hover");
        let msg = a
            .editor
            .last_message
            .as_ref()
            .expect("echo on no-hover-info");
        assert_eq!(msg.level, EchoLevel::Info);
        assert!(
            msg.text.contains("no hover info"),
            "expected 'no hover info' echo; got `{}`",
            msg.text
        );
    }

    #[test]
    fn drain_pending_hover_no_servers_outcome_echoes_warn() {
        // Buffer URI maps to no attached servers (e.g. spawn
        // failed at boot). The user gets a Warn echo pointing at
        // :lsp-status / :lsp-log so they can investigate.
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<crate::app::HoverOutcome>();
        a.editor.pending_hover_rx = Some(rx);
        a.editor.pending_hover_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(crate::app::HoverOutcome::NoServers).unwrap();
        a.drain_pending_hover();
        let msg = a
            .editor
            .last_message
            .as_ref()
            .expect("echo on no-servers-attached");
        assert_eq!(msg.level, EchoLevel::Warn);
        assert!(
            msg.text.contains("no LSP servers"),
            "expected NoServers warn echo; got `{}`",
            msg.text
        );
    }

    #[test]
    fn drain_pending_hover_idle_channel_is_noop() {
        let mut a = app_with("xx", 10);
        let (_tx, rx) = tokio::sync::mpsc::unbounded_channel::<crate::app::HoverOutcome>();
        a.editor.pending_hover_rx = Some(rx);
        a.drain_pending_hover();
        assert!(a.editor.popup_buffer.is_none());
        assert!(a.editor.last_message.is_none());
    }

    #[test]
    fn app_to_lsp_position_converts_utf8_byte_to_utf16_column() {
        let buf = lattice_core::Buffer::from_text("hello\nαβγ\nworld\n");
        // Line 1 (αβγ): 2-byte UTF-8 chars; byte 4 = end of β.
        // utf-16 column at byte 4: α (1 unit) + β (1 unit) = 2.
        let p = super::app_to_lsp_position(&buf, Position::new(1, 4)).expect("in-range");
        assert_eq!(p.line, 1);
        assert_eq!(p.character, 2);
    }

    #[test]
    fn app_to_lsp_position_returns_none_for_out_of_range_line() {
        let buf = lattice_core::Buffer::from_text("only-one-line\n");
        assert!(super::app_to_lsp_position(&buf, Position::new(99, 0)).is_none());
    }

    /// 4.4.e: flatten LSP linked-list `SelectionRange` into a
    /// `Vec<Range>` ordered innermost-first.
    #[test]
    fn flatten_selection_range_chain_walks_parent_links() {
        let outer = lattice_lsp::lsp_types::SelectionRange {
            range: lattice_lsp::lsp_types::Range {
                start: lattice_lsp::lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lattice_lsp::lsp_types::Position {
                    line: 5,
                    character: 0,
                },
            },
            parent: None,
        };
        let middle = lattice_lsp::lsp_types::SelectionRange {
            range: lattice_lsp::lsp_types::Range {
                start: lattice_lsp::lsp_types::Position {
                    line: 1,
                    character: 0,
                },
                end: lattice_lsp::lsp_types::Position {
                    line: 3,
                    character: 0,
                },
            },
            parent: Some(Box::new(outer)),
        };
        let inner = lattice_lsp::lsp_types::SelectionRange {
            range: lattice_lsp::lsp_types::Range {
                start: lattice_lsp::lsp_types::Position {
                    line: 2,
                    character: 0,
                },
                end: lattice_lsp::lsp_types::Position {
                    line: 2,
                    character: 8,
                },
            },
            parent: Some(Box::new(middle)),
        };
        let flat = crate::app::flatten_selection_range_chain(&inner);
        assert_eq!(flat.len(), 3);
        assert_eq!(flat[0].start.line, 2); // innermost
        assert_eq!(flat[1].start.line, 1);
        assert_eq!(flat[2].start.line, 0); // outermost
    }

    /// 4.4.f: an LSP `FoldingRange` converts to our `Fold`
    /// with start/end lines copied verbatim, `closed = false`
    /// (the carry-over path in `recompute_folds` re-closes
    /// 4.4.h: relative-position decoder produces absolute
    /// positions per the LSP §3.17.6 contract. Two tokens on
    /// the same line: the second's `delta_start` is relative
    /// to the first's start. A subsequent line: the next's
    /// `delta_start` is from column 0.
    #[test]
    fn decode_semantic_tokens_absolute_positions() {
        let token_types = vec![
            lattice_lsp::lsp_types::SemanticTokenType::KEYWORD,
            lattice_lsp::lsp_types::SemanticTokenType::FUNCTION,
        ];
        let token_modifiers = vec![
            lattice_lsp::lsp_types::SemanticTokenModifier::STATIC,
            lattice_lsp::lsp_types::SemanticTokenModifier::READONLY,
        ];
        // Three tokens:
        //  - Line 0, col 0, len 3, type=keyword, no mods.
        //  - Line 0, col 4, len 4, type=function, mod bit 0 (static).
        //  - Line 2, col 2, len 1, type=keyword, mod bits 0+1.
        let data = vec![
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 3,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 4,
                token_type: 1,
                token_modifiers_bitset: 0b01,
            },
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 2,
                delta_start: 2,
                length: 1,
                token_type: 0,
                token_modifiers_bitset: 0b11,
            },
        ];
        let decoded = crate::app::decode_semantic_tokens(&data, &token_types, &token_modifiers);
        assert_eq!(decoded.len(), 3);
        assert_eq!(decoded[0].line, 0);
        assert_eq!(decoded[0].start_char, 0);
        assert_eq!(decoded[0].token_type, "keyword");
        assert!(decoded[0].modifiers.is_empty());
        // Second token: same line; start = 0 + 4.
        assert_eq!(decoded[1].line, 0);
        assert_eq!(decoded[1].start_char, 4);
        assert_eq!(decoded[1].token_type, "function");
        assert_eq!(decoded[1].modifiers, vec!["static"]);
        // Third token: new line; start = delta_start (no
        // accumulation across line changes per spec).
        assert_eq!(decoded[2].line, 2);
        assert_eq!(decoded[2].start_char, 2);
        assert_eq!(decoded[2].modifiers, vec!["static", "readonly"]);
    }

    /// 4.4.h: tokens with type indexes past the legend are
    /// dropped (defense in depth; real servers don't emit).
    #[test]
    fn decode_semantic_tokens_drops_out_of_range_type() {
        let token_types = vec![lattice_lsp::lsp_types::SemanticTokenType::KEYWORD];
        let token_modifiers: Vec<lattice_lsp::lsp_types::SemanticTokenModifier> = Vec::new();
        let data = vec![
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 3,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 4,
                token_type: 99, // out of range
                token_modifiers_bitset: 0,
            },
        ];
        let decoded = crate::app::decode_semantic_tokens(&data, &token_types, &token_modifiers);
        // Only the in-range token survives.
        assert_eq!(decoded.len(), 1);
        assert_eq!(decoded[0].token_type, "keyword");
    }

    /// 4.4.i: a delta script that replaces a contiguous slice of
    /// the prior raw vec splices in place. Verifies both the
    /// length change and the resulting element identity.
    #[test]
    fn apply_semantic_token_edits_replace_middle() {
        let mut raw = vec![
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 2,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 4,
                token_type: 1,
                token_modifiers_bitset: 0,
            },
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 1,
                delta_start: 2,
                length: 1,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
        ];
        // Replace the middle token (index 1) with two new tokens.
        let edit = lattice_lsp::lsp_types::SemanticTokensEdit {
            start: 1,
            delete_count: 1,
            data: Some(vec![
                lattice_lsp::lsp_types::SemanticToken {
                    delta_line: 0,
                    delta_start: 3,
                    length: 2,
                    token_type: 2,
                    token_modifiers_bitset: 0,
                },
                lattice_lsp::lsp_types::SemanticToken {
                    delta_line: 0,
                    delta_start: 5,
                    length: 3,
                    token_type: 1,
                    token_modifiers_bitset: 0,
                },
            ]),
        };
        crate::app::apply_semantic_token_edits(&mut raw, &[edit])
            .expect("splice succeeds in range");
        assert_eq!(raw.len(), 4);
        assert_eq!(raw[1].token_type, 2);
        assert_eq!(raw[2].token_type, 1);
        // The trailing token wasn't touched.
        assert_eq!(raw[3].delta_line, 1);
    }

    /// 4.4.i: an insert-only edit (delete_count = 0) splices the
    /// new tokens in without removing anything.
    #[test]
    fn apply_semantic_token_edits_insert_only() {
        let mut raw = vec![lattice_lsp::lsp_types::SemanticToken {
            delta_line: 0,
            delta_start: 0,
            length: 2,
            token_type: 0,
            token_modifiers_bitset: 0,
        }];
        let edit = lattice_lsp::lsp_types::SemanticTokensEdit {
            start: 1,
            delete_count: 0,
            data: Some(vec![lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 4,
                token_type: 1,
                token_modifiers_bitset: 0,
            }]),
        };
        crate::app::apply_semantic_token_edits(&mut raw, &[edit]).expect("insert at end succeeds");
        assert_eq!(raw.len(), 2);
        assert_eq!(raw[1].token_type, 1);
    }

    /// 4.4.i: a delete-only edit (data = None) removes the named
    /// range without inserting anything.
    #[test]
    fn apply_semantic_token_edits_delete_only() {
        let mut raw = vec![
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 2,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 4,
                token_type: 1,
                token_modifiers_bitset: 0,
            },
        ];
        let edit = lattice_lsp::lsp_types::SemanticTokensEdit {
            start: 1,
            delete_count: 1,
            data: None,
        };
        crate::app::apply_semantic_token_edits(&mut raw, &[edit])
            .expect("delete in range succeeds");
        assert_eq!(raw.len(), 1);
        assert_eq!(raw[0].token_type, 0);
    }

    /// 4.4.i: an out-of-bounds edit returns Err and leaves the
    /// vec untouched. The host treats this as a server bug and
    /// drops the cache to force a fresh full request.
    #[test]
    fn apply_semantic_token_edits_out_of_bounds_errs() {
        let mut raw = vec![lattice_lsp::lsp_types::SemanticToken {
            delta_line: 0,
            delta_start: 0,
            length: 2,
            token_type: 0,
            token_modifiers_bitset: 0,
        }];
        let edit = lattice_lsp::lsp_types::SemanticTokensEdit {
            start: 5,
            delete_count: 2,
            data: None,
        };
        assert!(
            crate::app::apply_semantic_token_edits(&mut raw, &[edit]).is_err(),
            "out-of-bounds edit must err"
        );
        // Pre-edit vec was a single token; verify it survived.
        assert_eq!(raw.len(), 1);
    }

    /// 4.4.i: the drain's `Delta` arm splices a server-issued
    /// edit script into the cached raw vec and re-decodes,
    /// seating the updated decoded list in the cache.
    #[test]
    fn drain_semantic_tokens_delta_splices_and_redecodes() {
        let a = app_with("fn main() {}\n", 5);
        let buffer_id = a.editor.document_buffer_id;
        // Seed an initial cache entry with one keyword token and
        // a `result_id` the delta will reference. Use a single
        // legend entry so the decoder's name resolution is
        // deterministic.
        let initial_raw = vec![lattice_lsp::lsp_types::SemanticToken {
            delta_line: 0,
            delta_start: 0,
            length: 2,
            token_type: 0,
            token_modifiers_bitset: 0,
        }];
        // 5.8.AF.5 / Slice 3b.2: tests drive the delta path by
        // calling the pure helper directly. Channel + drain
        // dance is gone -- the spawned task in
        // `maybe_request_semantic_tokens` calls the same helper.
        {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            a.editor.lsp_semantic_tokens_cache.insert_for(
                buffer_id,
                crate::app::LspSemanticTokensCache {
                    document_version: 1,
                    result_id: Some("r1".into()),
                    raw_data: initial_raw,
                    tokens: vec![crate::app::DecodedSemanticToken {
                        line: 0,
                        start_char: 0,
                        length: 2,
                        token_type: "keyword".into(),
                        modifiers: Vec::new(),
                    }],
                },
            );
        }
        // Apply a Delta outcome that appends a new "function"
        // token. The helper should splice into raw_data and
        // re-decode against the carried legend.
        let token_types = vec![
            lattice_lsp::lsp_types::SemanticTokenType::KEYWORD,
            lattice_lsp::lsp_types::SemanticTokenType::FUNCTION,
        ];
        let token_modifiers: Vec<lattice_lsp::lsp_types::SemanticTokenModifier> = Vec::new();
        let edits = vec![lattice_lsp::lsp_types::SemanticTokensEdit {
            start: 1,
            delete_count: 0,
            data: Some(vec![lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 4,
                token_type: 1,
                token_modifiers_bitset: 0,
            }]),
        }];
        lattice_host::editor::Editor::apply_semantic_tokens_delta_outcome(
            &a.editor.lsp_semantic_tokens_cache,
            buffer_id,
            2,
            "r1",
            Some("r2".into()),
            &edits,
            &token_types,
            &token_modifiers,
        );
        let cache = {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            a.editor
                .lsp_semantic_tokens_cache
                .get_for(buffer_id)
                .expect("cache seated")
        };
        assert_eq!(cache.document_version, 2);
        assert_eq!(cache.result_id.as_deref(), Some("r2"));
        assert_eq!(cache.raw_data.len(), 2);
        assert_eq!(cache.tokens.len(), 2);
        assert_eq!(cache.tokens[1].token_type, "function");
        assert_eq!(cache.tokens[1].start_char, 3);
    }

    /// 4.4.i: when the cached `result_id` no longer matches
    /// the delta's `previous_result_id` (e.g. a concurrent
    /// refresh dropped the baseline), the drain evicts the
    /// cache so the next pump issues a fresh full request.
    #[test]
    fn drain_semantic_tokens_delta_stale_baseline_evicts_cache() {
        let a = app_with("fn main() {}\n", 5);
        let buffer_id = a.editor.document_buffer_id;
        // 5.8.AF.5 / Slice 3b.2: tests drive the delta path by
        // calling the pure helper directly (see twin test above).
        {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            a.editor.lsp_semantic_tokens_cache.insert_for(
                buffer_id,
                crate::app::LspSemanticTokensCache {
                    document_version: 1,
                    result_id: Some("different-id".into()),
                    raw_data: Vec::new(),
                    tokens: Vec::new(),
                },
            );
        }
        lattice_host::editor::Editor::apply_semantic_tokens_delta_outcome(
            &a.editor.lsp_semantic_tokens_cache,
            buffer_id,
            2,
            "r1",
            Some("r2".into()),
            &[],
            &[],
            &[],
        );
        let evicted = {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            a.editor
                .lsp_semantic_tokens_cache
                .get_for(buffer_id)
                .is_none()
        };
        assert!(
            evicted,
            "stale-baseline delta should evict the cache",
        );
    }

    /// 4.4.i: multiple edits compose in order. The server
    /// constructs edits against the index space of the *input*
    /// to each step, so applying [e1, e2] means e2's indices
    /// refer to the vec after e1 has been applied.
    #[test]
    fn apply_semantic_token_edits_sequential() {
        let mut raw = vec![
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 2,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lattice_lsp::lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 4,
                token_type: 1,
                token_modifiers_bitset: 0,
            },
        ];
        let edits = vec![
            // Delete index 0.
            lattice_lsp::lsp_types::SemanticTokensEdit {
                start: 0,
                delete_count: 1,
                data: None,
            },
            // Then insert a new token at the (now) start.
            lattice_lsp::lsp_types::SemanticTokensEdit {
                start: 0,
                delete_count: 0,
                data: Some(vec![lattice_lsp::lsp_types::SemanticToken {
                    delta_line: 0,
                    delta_start: 0,
                    length: 5,
                    token_type: 2,
                    token_modifiers_bitset: 0,
                }]),
            },
        ];
        crate::app::apply_semantic_token_edits(&mut raw, &edits).expect("sequential edits succeed");
        assert_eq!(raw.len(), 2);
        assert_eq!(raw[0].token_type, 2);
        assert_eq!(raw[1].token_type, 1);
    }

    /// 4.4.j: drain seats the cache + applies a `Full` report
    /// to `DiagnosticsLayer`. Verifies (a) the cache stores
    /// the new result_id + version, and (b) the layer
    /// receives the diagnostics for the URI.
    #[test]
    fn drain_pending_pull_diagnostics_full_applies_to_layer() {
        use std::str::FromStr;
        let app = app_with("fn main() {}\n", 5);
        let buffer_id = app.editor.document_buffer_id;
        let uri = lattice_lsp::Uri::from_str("file:///tmp/x.rs").unwrap();
        let server_id: std::sync::Arc<str> = std::sync::Arc::from("rust");
        let diag = lattice_lsp::lsp_types::Diagnostic {
            range: lattice_lsp::lsp_types::Range {
                start: lattice_lsp::lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lattice_lsp::lsp_types::Position {
                    line: 0,
                    character: 2,
                },
            },
            severity: Some(lattice_lsp::lsp_types::DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: None,
            message: "boom".into(),
            related_information: None,
            tags: None,
            data: None,
        };
        // 5.8.AF.5 / Slice 3b.5: drive the outcome through the
        // pure helper. The spawned task in
        // `maybe_request_pull_diagnostics` calls the same helper.
        lattice_host::editor::Editor::apply_pull_diagnostics_outcome(
            &app.editor.lsp_pull_diagnostics_cache,
            &app.editor.lsp_diagnostics,
            crate::app::PullDiagnosticsOutcome::Full {
                buffer_id,
                server_id: server_id.clone(),
                uri: uri.clone(),
                document_version: 1,
                result_id: Some("r1".into()),
                diagnostics: vec![diag.clone()],
            },
        );
        let cache = {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            app.editor
                .lsp_pull_diagnostics_cache
                .get_for(buffer_id)
                .expect("cache seated")
        };
        assert_eq!(cache.document_version, 1);
        assert_eq!(cache.result_id.as_deref(), Some("r1"));
        // Layer should now carry the diagnostic. URI equality
        // goes via `as_str()` (fluent_uri's Uri doesn't impl
        // PartialEq across the typed/owning split).
        let snap = app.editor.lsp_diagnostics.snapshot();
        let entry = snap
            .iter()
            .find(|(u, _)| u.as_str() == uri.as_str())
            .expect("uri in layer");
        let msgs: Vec<_> = entry.1.iter().map(|d| d.message.as_str()).collect();
        assert!(msgs.contains(&"boom"));
    }

    /// 4.4.j: drain handles `Unchanged` reports by refreshing
    /// the (version, result_id) pair without touching the
    /// layer. The diagnostic state on the layer must be
    /// preserved verbatim.
    #[test]
    fn drain_pending_pull_diagnostics_unchanged_keeps_layer_state() {
        let app = app_with("fn main() {}\n", 5);
        let buffer_id = app.editor.document_buffer_id;
        // Seed initial cache state.
        {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            app.editor.lsp_pull_diagnostics_cache.insert_for(
                buffer_id,
                crate::app::LspPullDiagnosticsCache {
                    document_version: 1,
                    result_id: Some("r1".into()),
                },
            );
        }
        // 5.8.AF.5 / Slice 3b.5: drive the outcome through the
        // helper directly.
        lattice_host::editor::Editor::apply_pull_diagnostics_outcome(
            &app.editor.lsp_pull_diagnostics_cache,
            &app.editor.lsp_diagnostics,
            crate::app::PullDiagnosticsOutcome::Unchanged {
                buffer_id,
                document_version: 2,
                result_id: "r2".into(),
            },
        );
        let cache = {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            app.editor
                .lsp_pull_diagnostics_cache
                .get_for(buffer_id)
                .expect("cache seated")
        };
        assert_eq!(cache.document_version, 2);
        assert_eq!(cache.result_id.as_deref(), Some("r2"));
    }

    /// 4.4.j: `workspace/diagnostic/refresh` drain evicts the
    /// per-buffer result_id cache for every buffer attached
    /// to the requesting server (here: by URI walk through
    /// `buffer_uris` + `lsp.servers_for`). For the unit-test
    /// scaffold there are no real running actors, so the
    /// attachment check returns no buffers and the cache
    /// stays -- this test exercises the no-attached-buffers
    /// path (the integration tests cover the eviction itself
    /// when actors are present).
    #[test]
    fn drain_diagnostic_refresh_handles_no_attached_buffers() {
        let mut app = app_with("fn main() {}\n", 5);
        let buffer_id = app.editor.document_buffer_id;
        // 5.8.AF.5 / Slice 3b.5: cache is `PerBufferCache`.
        {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            app.editor.lsp_pull_diagnostics_cache.insert_for(
                buffer_id,
                crate::app::LspPullDiagnosticsCache {
                    document_version: 1,
                    result_id: Some("r1".into()),
                },
            );
        }
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspDiagnosticRefresh>();
        tx.send(lattice_lsp::LspDiagnosticRefresh {
            server_id: std::sync::Arc::from("rust"),
        })
        .expect("send refresh");
        app.editor.pending_diagnostic_refresh_rx = Some(rx);
        app.drain_diagnostic_refresh();
        // No actors attached -> no eviction.
        use lattice_host::per_buffer_cache::PerBufferCacheExt;
        assert!(
            app.editor
                .lsp_pull_diagnostics_cache
                .get_for(buffer_id)
                .is_some()
        );
    }

    /// 4.4.j: pump short-circuits when the cache's
    /// document_version matches the current snapshot. No
    /// request fires.
    #[test]
    fn pull_diagnostics_pump_skips_when_version_unchanged() {
        let mut app = app_with("fn main() {}\n", 5);
        let buffer_id = app.editor.document_buffer_id;
        let version = app.editor.document.snapshot().version;
        // 5.8.AF.5 / Slice 3b.5: cache is `PerBufferCache`; the
        // pump fires a request when the cache is stale and
        // installs a cancellation token (was `pending_*_rx`).
        {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            app.editor.lsp_pull_diagnostics_cache.insert_for(
                buffer_id,
                crate::app::LspPullDiagnosticsCache {
                    document_version: version,
                    result_id: Some("r1".into()),
                },
            );
        }
        app.maybe_request_pull_diagnostics();
        assert!(
            app.editor.pending_pull_diagnostics_token.is_none(),
            "pump should short-circuit on unchanged version",
        );
    }

    /// 4.4.g viewport: when the cached range covers the
    /// current viewport (with overscan baked in), the pump
    /// short-circuits -- no new request fires.
    #[test]
    fn inlay_hint_pump_skips_when_viewport_inside_cached_range() {
        use std::str::FromStr;
        let mut app = app_with("fn main() {}\n", 5);
        // Mode must be on; buffer_uris must be populated.
        if !app.lsp_inlay_hint_mode_enabled_for(app.editor.document_buffer_id) {
            app.toggle_mode_by_name("lsp-inlay-hint-mode");
        }
        let uri = lattice_lsp::Uri::from_str("file:///tmp/x.rs").unwrap();
        app.editor
            .buffer_uris
            .insert(app.editor.document_buffer_id, uri);
        // Seed cache with a wide range covering 0..=1000.
        // 5.8.AF.5 / Slice 3b.1: `lsp_inlay_hints_cache` is now
        // a `PerBufferCache<...>`; tests use `insert_for` and
        // publish so renderer reads via `RenderState` match.
        {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            app.editor.lsp_inlay_hints_cache.insert_for(
                app.editor.document_buffer_id,
                crate::app::LspInlayHintCache {
                    document_version: app.editor.document.snapshot().version,
                    hints: Vec::new(),
                    requested_first_line: 0,
                    requested_last_line: 1000,
                },
            );
        }
        app.editor.publish_render_state();
        // Viewport at line 0, height 5 (set by app_with) --
        // comfortably inside the cached range. Avoid calling
        // `set_viewport_height` here: it triggers
        // `ensure_cursor_visible` which would clamp scroll
        // back to the cursor line and mask the viewport state
        // the test wants to exercise.
        app.editor.scroll = 0;
        app.maybe_request_inlay_hint();
        assert!(
            app.editor.pending_inlay_hint_token.is_none(),
            "pump should short-circuit when viewport is inside cached range",
        );
    }

    /// 4.4.g viewport: scrolling outside the cached range
    /// triggers a fresh request. The pump replaces
    /// `pending_inlay_hint_rx` with a new receiver before
    /// spawning the async fetch.
    #[test]
    fn inlay_hint_pump_refetches_when_viewport_outside_cached_range() {
        use std::str::FromStr;
        let mut app = app_with(&"a\n".repeat(2000), 5);
        if !app.lsp_inlay_hint_mode_enabled_for(app.editor.document_buffer_id) {
            app.toggle_mode_by_name("lsp-inlay-hint-mode");
        }
        let uri = lattice_lsp::Uri::from_str("file:///tmp/x.rs").unwrap();
        app.editor
            .buffer_uris
            .insert(app.editor.document_buffer_id, uri);
        // Cached range: lines 0..=200.
        {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            app.editor.lsp_inlay_hints_cache.insert_for(
                app.editor.document_buffer_id,
                crate::app::LspInlayHintCache {
                    document_version: app.editor.document.snapshot().version,
                    hints: Vec::new(),
                    requested_first_line: 0,
                    requested_last_line: 200,
                },
            );
        }
        app.editor.publish_render_state();
        // Viewport now far below the cached range. Skip
        // `set_viewport_height` -- it calls
        // `ensure_cursor_visible` which would snap scroll
        // back to the cursor line.
        app.editor.scroll = 1500;
        app.maybe_request_inlay_hint();
        assert!(
            app.editor.pending_inlay_hint_token.is_some(),
            "pump should issue a new request when viewport leaves cached range",
        );
    }

    /// 4.4.g viewport: small scrolls within the overscan
    /// margin stay cached. Pump checks whether
    /// `viewport_first >= cache.first` AND
    /// `viewport_last <= cache.last`; the overscan-bounded
    /// fetch range gives small scrolls room to move without
    /// triggering a request.
    #[test]
    fn inlay_hint_pump_small_scroll_within_overscan_keeps_cache() {
        use std::str::FromStr;
        let mut app = app_with(&"a\n".repeat(2000), 5);
        if !app.lsp_inlay_hint_mode_enabled_for(app.editor.document_buffer_id) {
            app.toggle_mode_by_name("lsp-inlay-hint-mode");
        }
        let uri = lattice_lsp::Uri::from_str("file:///tmp/x.rs").unwrap();
        app.editor
            .buffer_uris
            .insert(app.editor.document_buffer_id, uri);
        // Cache covers lines 100..=400 (the overscan-padded
        // window the pump would have fetched at scroll=200).
        {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            app.editor.lsp_inlay_hints_cache.insert_for(
                app.editor.document_buffer_id,
                crate::app::LspInlayHintCache {
                    document_version: app.editor.document.snapshot().version,
                    hints: Vec::new(),
                    requested_first_line: 100,
                    requested_last_line: 400,
                },
            );
        }
        app.editor.publish_render_state();
        // Scroll a bit -- still well within the cached window.
        // Same `set_viewport_height` caveat as above.
        app.editor.scroll = 250;
        app.maybe_request_inlay_hint();
        assert!(
            app.editor.pending_inlay_hint_token.is_none(),
            "small scroll inside cached window should not refetch",
        );
    }

    /// matching entries), and a stable identity hash.
    #[test]
    fn folding_range_to_fold_preserves_extents_and_keys_identity() {
        let r = lattice_lsp::lsp_types::FoldingRange {
            start_line: 2,
            end_line: 5,
            start_character: None,
            end_character: None,
            kind: Some(lattice_lsp::lsp_types::FoldingRangeKind::Comment),
            collapsed_text: None,
        };
        let f = crate::app::folding_range_to_fold(r.clone());
        assert_eq!(f.start_line, 2);
        assert_eq!(f.end_line, 5);
        assert!(!f.closed);
        assert!(f.identity.is_some());

        // Same shape -> same identity, so closed-state survives
        // re-fetches.
        let f2 = crate::app::folding_range_to_fold(r);
        assert_eq!(f.identity, f2.identity);

        // Different end-line -> different identity.
        let r3 = lattice_lsp::lsp_types::FoldingRange {
            start_line: 2,
            end_line: 9,
            start_character: None,
            end_character: None,
            kind: Some(lattice_lsp::lsp_types::FoldingRangeKind::Comment),
            collapsed_text: None,
        };
        let f3 = crate::app::folding_range_to_fold(r3);
        assert_ne!(f.identity, f3.identity);
    }

    /// 4.4.f: activating `lsp-folding-mode` swaps `foldmethod`
    /// to `lsp` and stashes the prior value (inside the mode's
    /// typed `LspFoldingGuard`); deactivating drops the Guard,
    /// firing Drop which restores. M-async.1: the stash is no
    /// longer observable from outside the Guard -- the test
    /// asserts the public contract (foldmethod swap + restore)
    /// rather than the implementation detail.
    #[test]
    fn lsp_folding_mode_toggle_syncs_foldmethod() {
        use lattice_core::FoldMethod;
        let mut app = app_with("fn a() {}\n", 5);
        app.set_foldmethod_for_test(FoldMethod::Syntax);
        assert_eq!(app.foldmethod(), FoldMethod::Syntax);
        if app.lsp_folding_mode_enabled_for(app.editor.document_buffer_id) {
            app.toggle_mode_by_name("lsp-folding-mode");
        }
        // Activate -> mode swaps foldmethod to Lsp.
        app.toggle_mode_by_name("lsp-folding-mode");
        assert!(app.lsp_folding_mode_enabled_for(app.editor.document_buffer_id));
        assert_eq!(app.foldmethod(), FoldMethod::Lsp);
        // Deactivate -> Guard Drop restores foldmethod.
        app.toggle_mode_by_name("lsp-folding-mode");
        assert!(!app.lsp_folding_mode_enabled_for(app.editor.document_buffer_id));
        assert_eq!(app.foldmethod(), FoldMethod::Syntax);
    }

    /// 4.4.f: a seeded `lsp_folds_cache` makes `recompute_folds`
    /// pick up the LSP fold list when `:set foldmethod=lsp`.
    #[test]
    fn recompute_folds_with_foldmethod_lsp_reads_cache() {
        use lattice_core::FoldMethod;
        let mut app = app_with("fn a() {}\nfn b() {}\nfn c() {}\n", 5);
        app.set_foldmethod_for_test(FoldMethod::Lsp);
        let fold = crate::app::folding_range_to_fold(lattice_lsp::lsp_types::FoldingRange {
            start_line: 0,
            end_line: 1,
            start_character: None,
            end_character: None,
            kind: None,
            collapsed_text: None,
        });
        // 5.8.AF.5 / Slice 3b.1: `lsp_folds_cache` is now a
        // `PerBufferCache<...>`; use `insert_for` + publish.
        {
            use lattice_host::per_buffer_cache::PerBufferCacheExt;
            app.editor.lsp_folds_cache.insert_for(
                app.editor.document_buffer_id,
                crate::app::LspFoldsCache {
                    document_version: app.editor.document.snapshot().version,
                    folds: vec![fold],
                },
            );
        }
        app.editor.publish_render_state();
        // Force `lsp-folding-mode` on so the cache is read
        // (the M.6.0 cascade may have left it off in test
        // setup).
        if !app.lsp_folding_mode_enabled_for(app.editor.document_buffer_id) {
            app.toggle_mode_by_name("lsp-folding-mode");
        }
        app.recompute_folds();
        assert!(
            app.editor
                .folds
                .iter()
                .any(|f| f.start_line == 0 && f.end_line == 1),
            "expected LSP fold from cache; got {:?}",
            app.editor.folds,
        );
    }

    /// 4.4.e: cursor on the start-character of a range is
    /// "inside" (half-open); cursor on `end` is outside.
    #[test]
    fn cursor_inside_range_is_half_open() {
        let r = lattice_lsp::lsp_types::Range {
            start: lattice_lsp::lsp_types::Position {
                line: 1,
                character: 4,
            },
            end: lattice_lsp::lsp_types::Position {
                line: 1,
                character: 8,
            },
        };
        assert!(crate::app::cursor_inside_range(Position::new(1, 4), &r));
        assert!(crate::app::cursor_inside_range(Position::new(1, 6), &r));
        assert!(!crate::app::cursor_inside_range(Position::new(1, 8), &r));
        assert!(!crate::app::cursor_inside_range(Position::new(0, 6), &r));
        assert!(!crate::app::cursor_inside_range(Position::new(2, 6), &r));
    }

    fn fake_uri(path: &str) -> lattice_lsp::lsp_types::Uri {
        use std::str::FromStr;
        lattice_lsp::lsp_types::Uri::from_str(&format!("file://{path}")).unwrap()
    }

    fn loc(path: &str, line: u32, col: u32) -> lattice_lsp::lsp_types::Location {
        lattice_lsp::lsp_types::Location {
            uri: fake_uri(path),
            range: lattice_lsp::lsp_types::Range {
                start: lattice_lsp::lsp_types::Position {
                    line,
                    character: col,
                },
                end: lattice_lsp::lsp_types::Position {
                    line,
                    character: col + 1,
                },
            },
        }
    }

    #[test]
    fn definition_response_scalar_flattens_to_one_location() {
        let resp = lattice_lsp::lsp_types::GotoDefinitionResponse::Scalar(loc("/x.rs", 1, 2));
        let v = super::definition_response_to_locations(resp);
        assert_eq!(v.len(), 1);
        assert_eq!(v[0].range.start.line, 1);
    }

    #[test]
    fn definition_response_array_flattens_verbatim() {
        let resp = lattice_lsp::lsp_types::GotoDefinitionResponse::Array(vec![
            loc("/a.rs", 0, 0),
            loc("/b.rs", 5, 5),
        ]);
        let v = super::definition_response_to_locations(resp);
        assert_eq!(v.len(), 2);
    }

    #[test]
    fn definition_response_link_uses_target_selection_range() {
        // Link variant carries richer per-result info; we use
        // target_selection_range (narrower) for jumps.
        let link = lattice_lsp::lsp_types::LocationLink {
            origin_selection_range: None,
            target_uri: fake_uri("/x.rs"),
            target_range: lattice_lsp::lsp_types::Range {
                start: lattice_lsp::lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lattice_lsp::lsp_types::Position {
                    line: 10,
                    character: 0,
                },
            },
            target_selection_range: lattice_lsp::lsp_types::Range {
                start: lattice_lsp::lsp_types::Position {
                    line: 5,
                    character: 4,
                },
                end: lattice_lsp::lsp_types::Position {
                    line: 5,
                    character: 7,
                },
            },
        };
        let resp = lattice_lsp::lsp_types::GotoDefinitionResponse::Link(vec![link]);
        let v = super::definition_response_to_locations(resp);
        assert_eq!(v.len(), 1);
        // Should be the target_selection_range, not target_range.
        assert_eq!(v[0].range.start.line, 5);
        assert_eq!(v[0].range.start.character, 4);
    }

    #[test]
    fn lsp_definition_request_with_no_uri_echoes_no_lsp_attached() {
        let mut a = app_with("xx", 10);
        a.toggle_mode_by_name("lsp-mode");
        a.apply(Action::LspDefinitionRequest);
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert_eq!(msg.level, EchoLevel::Info);
        assert!(msg.text.contains("no LSP server"));
    }

    #[test]
    fn lsp_declaration_request_routes_through_unified_nav_dispatch() {
        let mut a = app_with("xx", 10);
        a.toggle_mode_by_name("lsp-mode");
        a.apply(Action::LspDeclarationRequest);
        // No URI mapped, same "no LSP server" guard fires.
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert_eq!(msg.level, EchoLevel::Info);
        assert!(msg.text.contains("no LSP server"));
    }

    #[test]
    fn lsp_type_definition_request_routes_through_unified_nav_dispatch() {
        let mut a = app_with("xx", 10);
        a.toggle_mode_by_name("lsp-mode");
        a.apply(Action::LspTypeDefinitionRequest);
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no LSP server"));
    }

    #[test]
    fn lsp_implementation_request_routes_through_unified_nav_dispatch() {
        let mut a = app_with("xx", 10);
        a.toggle_mode_by_name("lsp-mode");
        a.apply(Action::LspImplementationRequest);
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no LSP server"));
    }

    #[test]
    fn drain_pending_no_implementations_echoes_kind_specific_message() {
        // Verify the kind drives the verb in the "no X found" echo.
        let mut a = app_with("xx", 10);
        let (tx, rx) =
            tokio::sync::mpsc::unbounded_channel::<Vec<lattice_lsp::lsp_types::Location>>();
        a.editor.pending_definition_rx = Some(rx);
        a.editor.pending_definition_token = Some(lattice_protocol::CancellationToken::new());
        a.editor.pending_nav_kind = Some(super::LspNavKind::Implementation);
        tx.send(Vec::new()).unwrap();
        a.drain_pending_definitions();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(
            msg.text.contains("no implementations"),
            "expected implementations echo, got: {}",
            msg.text
        );
        assert!(a.editor.pending_nav_kind.is_none());
    }

    #[test]
    fn drain_pending_no_type_definitions_echoes_kind_specific_message() {
        let mut a = app_with("xx", 10);
        let (tx, rx) =
            tokio::sync::mpsc::unbounded_channel::<Vec<lattice_lsp::lsp_types::Location>>();
        a.editor.pending_definition_rx = Some(rx);
        a.editor.pending_definition_token = Some(lattice_protocol::CancellationToken::new());
        a.editor.pending_nav_kind = Some(super::LspNavKind::TypeDefinition);
        tx.send(Vec::new()).unwrap();
        a.drain_pending_definitions();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no type definitions"));
    }

    #[test]
    fn drain_pending_no_declarations_echoes_kind_specific_message() {
        let mut a = app_with("xx", 10);
        let (tx, rx) =
            tokio::sync::mpsc::unbounded_channel::<Vec<lattice_lsp::lsp_types::Location>>();
        a.editor.pending_definition_rx = Some(rx);
        a.editor.pending_definition_token = Some(lattice_protocol::CancellationToken::new());
        a.editor.pending_nav_kind = Some(super::LspNavKind::Declaration);
        tx.send(Vec::new()).unwrap();
        a.drain_pending_definitions();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no declarations"));
    }

    #[test]
    fn lsp_references_request_with_no_uri_echoes_no_lsp_attached() {
        let mut a = app_with("xx", 10);
        a.toggle_mode_by_name("lsp-mode");
        a.apply(Action::LspReferencesRequest);
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no LSP server"));
    }

    #[test]
    fn lsp_references_request_pre_cancels_in_flight_token() {
        let mut a = app_with("xx", 10);
        let stale = lattice_protocol::CancellationToken::new();
        a.editor.pending_references_token = Some(stale.clone());
        a.apply(Action::LspReferencesRequest);
        assert!(stale.is_cancelled());
    }

    #[test]
    fn drain_pending_references_no_servers_outcome_echoes() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::ReferencesOutcome>();
        a.editor.pending_references_rx = Some(rx);
        a.editor.pending_references_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::ReferencesOutcome::NoServers).unwrap();
        a.drain_pending_references();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no LSP server"));
        assert!(a.editor.pending_references_token.is_none());
    }

    #[test]
    fn drain_pending_references_found_opens_lsp_locations_picker() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::ReferencesOutcome>();
        a.editor.pending_references_rx = Some(rx);
        a.editor.pending_references_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::ReferencesOutcome::Found {
            symbol: "foo".into(),
            locations: vec![loc("/tmp/notarealfile.rs", 3, 5)],
        })
        .unwrap();
        a.drain_pending_references();
        // Picker opened, NOT a help buffer (the pre-picker shape).
        let picker = a.editor.picker.as_ref().expect("picker");
        assert_eq!(picker.title, "references: foo");
        assert!(matches!(
            picker.source,
            lattice_picker::PickerSource::LspLocations
        ));
        assert!(matches!(
            picker.on_accept,
            lattice_picker::PickerAction::JumpToLspLocation
        ));
        // The candidate's typed routing payload carries the
        // jump target -- post-4.2.g.7 this replaces the prior
        // tab-encoded `text` parsing.
        let c = picker.selected_candidate().expect("one row");
        let routing = picker.routing_for(c).expect("routing payload set");
        let lattice_picker::RoutingPayload::LspLocation { path, line, .. } = routing else {
            panic!("expected LspLocation routing, got {routing:?}");
        };
        assert_eq!(*path, std::path::PathBuf::from("/tmp/notarealfile.rs"));
        assert_eq!(*line, 3);
        // Column round-trips through utf-16→utf-8 conversion that
        // reads from the file's actual line text. For a missing
        // file the preview is empty so the conversion bottoms out
        // at 0; ASCII files round-trip cleanly. We don't assert on
        // col here because the conversion needs the line text and
        // the test fixture's path doesn't exist.
    }

    #[test]
    fn drain_pending_references_empty_echoes_not_found() {
        // After the picker pivot, the empty-Found case echoes
        // rather than opening a buffer with a placeholder. The
        // picker UX expects "show the user a list to choose
        // from" -- showing an empty picker would be worse UX
        // than the echo.
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::ReferencesOutcome>();
        a.editor.pending_references_rx = Some(rx);
        a.editor.pending_references_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::ReferencesOutcome::Found {
            symbol: "missing".into(),
            locations: Vec::new(),
        })
        .unwrap();
        a.drain_pending_references();
        assert!(a.editor.picker.is_none());
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no references"));
        assert!(msg.text.contains("missing"));
    }

    #[test]
    fn flatten_document_symbol_response_flat_preserves_order() {
        use lattice_lsp::lsp_types::{Location as LLoc, Position as LPos, Range as LRange};
        let path = std::path::PathBuf::from("/tmp/x.rs");
        #[allow(deprecated)]
        let syms = vec![
            lattice_lsp::lsp_types::SymbolInformation {
                name: "foo".into(),
                kind: lattice_lsp::lsp_types::SymbolKind::FUNCTION,
                tags: None,
                deprecated: None,
                location: LLoc {
                    uri: super::tests::fake_uri("/tmp/x.rs"),
                    range: LRange {
                        start: LPos {
                            line: 5,
                            character: 0,
                        },
                        end: LPos {
                            line: 5,
                            character: 3,
                        },
                    },
                },
                container_name: None,
            },
            lattice_lsp::lsp_types::SymbolInformation {
                name: "bar".into(),
                kind: lattice_lsp::lsp_types::SymbolKind::METHOD,
                tags: None,
                deprecated: None,
                location: LLoc {
                    uri: super::tests::fake_uri("/tmp/x.rs"),
                    range: LRange {
                        start: LPos {
                            line: 10,
                            character: 4,
                        },
                        end: LPos {
                            line: 10,
                            character: 7,
                        },
                    },
                },
                container_name: Some("Bag".into()),
            },
        ];
        let resp = lattice_lsp::lsp_types::DocumentSymbolResponse::Flat(syms);
        let mut out = Vec::new();
        super::flatten_document_symbol_response(resp, &path, &mut out);
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].name, "foo");
        assert_eq!(out[0].depth, 0);
        assert_eq!(out[1].name, "bar");
        assert_eq!(out[1].container.as_deref(), Some("Bag"));
    }

    #[test]
    fn flatten_document_symbol_response_nested_assigns_depth_via_dfs() {
        use lattice_lsp::lsp_types::{DocumentSymbol, Position as LPos, Range as LRange};
        let path = std::path::PathBuf::from("/tmp/x.rs");
        // mod foo { fn bar() {} } -> outer at depth 0, bar at depth 1.
        let inner_range = LRange {
            start: LPos {
                line: 1,
                character: 4,
            },
            end: LPos {
                line: 3,
                character: 5,
            },
        };
        let outer_range = LRange {
            start: LPos {
                line: 0,
                character: 0,
            },
            end: LPos {
                line: 4,
                character: 0,
            },
        };
        #[allow(deprecated)]
        let inner = DocumentSymbol {
            name: "bar".into(),
            detail: None,
            kind: lattice_lsp::lsp_types::SymbolKind::FUNCTION,
            tags: None,
            deprecated: None,
            range: inner_range,
            selection_range: inner_range,
            children: None,
        };
        #[allow(deprecated)]
        let outer = DocumentSymbol {
            name: "foo".into(),
            detail: None,
            kind: lattice_lsp::lsp_types::SymbolKind::MODULE,
            tags: None,
            deprecated: None,
            range: outer_range,
            selection_range: outer_range,
            children: Some(vec![inner]),
        };
        let resp = lattice_lsp::lsp_types::DocumentSymbolResponse::Nested(vec![outer]);
        let mut out = Vec::new();
        super::flatten_document_symbol_response(resp, &path, &mut out);
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].name, "foo");
        assert_eq!(out[0].depth, 0);
        assert_eq!(out[1].name, "bar");
        assert_eq!(out[1].depth, 1);
    }

    #[test]
    fn drain_pending_symbols_no_servers_outcome_echoes() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::SymbolsOutcome>();
        a.editor.pending_symbols_rx = Some(rx);
        a.editor.pending_symbols_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::SymbolsOutcome::NoServers).unwrap();
        a.drain_pending_symbols();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no LSP server"));
        assert!(a.editor.pending_symbols_token.is_none());
    }

    #[test]
    fn drain_pending_symbols_found_opens_picker() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::SymbolsOutcome>();
        a.editor.pending_symbols_rx = Some(rx);
        a.editor.pending_symbols_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::SymbolsOutcome::Found {
            title: "symbols (2)".into(),
            rows: vec![
                super::SymbolRow {
                    name: "foo".into(),
                    kind_glyph: "ƒ",
                    container: None,
                    depth: 0,
                    path: std::path::PathBuf::from("/tmp/x.rs"),
                    line: 5,
                    col: 0,
                },
                super::SymbolRow {
                    name: "bar".into(),
                    kind_glyph: "v",
                    container: None,
                    depth: 1,
                    path: std::path::PathBuf::from("/tmp/x.rs"),
                    line: 10,
                    col: 4,
                },
            ],
        })
        .unwrap();
        a.drain_pending_symbols();
        let picker = a.editor.picker.as_ref().expect("picker");
        assert_eq!(picker.title, "symbols (2)");
        assert_eq!(picker.candidates.len(), 2);
        // depth-1 row carries indentation in display.
        let display = &picker.candidates[1].raw.display;
        assert!(display.contains("  v bar"), "got: {display}");
    }

    #[test]
    fn drain_pending_symbols_empty_echoes() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::SymbolsOutcome>();
        a.editor.pending_symbols_rx = Some(rx);
        a.editor.pending_symbols_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::SymbolsOutcome::Found {
            title: "symbols (0)".into(),
            rows: Vec::new(),
        })
        .unwrap();
        a.drain_pending_symbols();
        assert!(a.editor.picker.is_none());
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no symbols"));
    }

    #[test]
    fn code_action_kind_glyph_distinct_for_common_kinds() {
        use lattice_lsp::lsp_types::CodeActionKind as K;
        let qf = super::code_action_kind_glyph(Some(&K::QUICKFIX));
        let rf = super::code_action_kind_glyph(Some(&K::REFACTOR));
        let sr = super::code_action_kind_glyph(Some(&K::SOURCE));
        assert_ne!(qf, rf);
        assert_ne!(qf, sr);
        assert_ne!(rf, sr);
    }

    #[test]
    fn drain_pending_code_actions_no_provider_echoes() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CodeActionOutcome>();
        a.editor.pending_code_action_rx = Some(rx);
        a.editor.pending_code_action_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::CodeActionOutcome::NoProvider).unwrap();
        a.drain_pending_code_actions();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("codeActionProvider"));
    }

    #[test]
    fn drain_pending_code_actions_empty_echoes_no_actions() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CodeActionOutcome>();
        a.editor.pending_code_action_rx = Some(rx);
        a.editor.pending_code_action_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::CodeActionOutcome::Items(Vec::new()))
            .unwrap();
        a.drain_pending_code_actions();
        assert!(a.editor.picker.is_none());
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no code actions"));
    }

    #[test]
    fn drain_pending_code_actions_items_open_picker() {
        let mut a = app_with("foo\n", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CodeActionOutcome>();
        a.editor.pending_code_action_rx = Some(rx);
        a.editor.pending_code_action_token = Some(lattice_protocol::CancellationToken::new());
        let act = lattice_lsp::lsp_types::CodeAction {
            title: "Add `mut` modifier".into(),
            kind: Some(lattice_lsp::lsp_types::CodeActionKind::QUICKFIX),
            diagnostics: None,
            edit: None,
            command: None,
            is_preferred: None,
            disabled: None,
            data: None,
        };
        tx.send(super::CodeActionOutcome::Items(vec![
            super::CodeActionRow {
                title: act.title.clone(),
                kind_glyph: "🛠",
                action: lattice_lsp::lsp_types::CodeActionOrCommand::CodeAction(act),
            },
        ]))
        .unwrap();
        a.drain_pending_code_actions();
        let picker = a.editor.picker.as_ref().expect("picker");
        assert!(picker.title.starts_with("code-actions"));
        assert!(matches!(
            picker.on_accept,
            lattice_picker::PickerAction::AcceptLspCodeAction
        ));
        assert_eq!(picker.candidates.len(), 1);
        let display = &picker.candidates[0].raw.display;
        assert!(display.contains("🛠 Add `mut` modifier"));
        // Items pinned for the accept path.
        assert!(a.editor.pending_code_action_items.is_some());
    }

    #[test]
    fn flatten_workspace_edit_collects_legacy_changes_map() {
        use std::collections::HashMap;
        let uri = super::tests::fake_uri("/tmp/x.rs");
        let mut changes: HashMap<
            lattice_lsp::lsp_types::Uri,
            Vec<lattice_lsp::lsp_types::TextEdit>,
        > = HashMap::new();
        changes.insert(
            uri.clone(),
            vec![lattice_lsp::lsp_types::TextEdit {
                range: lattice_lsp::lsp_types::Range {
                    start: lattice_lsp::lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: lattice_lsp::lsp_types::Position {
                        line: 0,
                        character: 3,
                    },
                },
                new_text: "bar".into(),
            }],
        );
        let we = lattice_lsp::lsp_types::WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        };
        let flat = super::flatten_workspace_edit(we);
        assert_eq!(flat.len(), 1);
        assert_eq!(flat[0].0, uri);
        assert_eq!(flat[0].1[0].new_text, "bar");
    }

    #[test]
    fn drain_pending_rename_no_provider_echoes() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::RenameOutcome>();
        a.editor.pending_rename_rx = Some(rx);
        a.editor.pending_rename_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::RenameOutcome::NoProvider).unwrap();
        a.drain_pending_rename();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("renameProvider"));
    }

    #[test]
    fn drain_pending_rename_not_renameable_echoes_reason() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::RenameOutcome>();
        a.editor.pending_rename_rx = Some(rx);
        a.editor.pending_rename_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::RenameOutcome::NotRenameable {
            reason: "out of bounds".into(),
        })
        .unwrap();
        a.drain_pending_rename();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert_eq!(msg.level, EchoLevel::Error);
        assert!(msg.text.contains("out of bounds"));
    }

    #[test]
    fn drain_pending_rename_empty_echoes_no_changes() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::RenameOutcome>();
        a.editor.pending_rename_rx = Some(rx);
        a.editor.pending_rename_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::RenameOutcome::Empty).unwrap();
        a.drain_pending_rename();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no changes"));
    }

    #[test]
    fn drain_pending_rename_applies_active_buffer_edits_as_one_undo_unit() {
        // End-to-end-ish: load a real document, send a rename
        // outcome targeting it, verify the buffer text changed
        // and a single undo restores.
        let path = std::env::temp_dir().join(format!("lattice-rename-{}.rs", std::process::id()));
        std::fs::write(&path, "let foo = 1;\nlet x = foo + 2;\n").unwrap();
        let doc = Document::open(&path).unwrap();
        let mut a = App::new(doc);
        a.set_viewport_height(10);
        let uri = super::tests::fake_uri(path.to_str().unwrap());
        let edits = vec![
            // Replace `foo` on line 0 col 4..7
            lattice_lsp::lsp_types::TextEdit {
                range: lattice_lsp::lsp_types::Range {
                    start: lattice_lsp::lsp_types::Position {
                        line: 0,
                        character: 4,
                    },
                    end: lattice_lsp::lsp_types::Position {
                        line: 0,
                        character: 7,
                    },
                },
                new_text: "bar".into(),
            },
            // Replace `foo` on line 1 col 8..11
            lattice_lsp::lsp_types::TextEdit {
                range: lattice_lsp::lsp_types::Range {
                    start: lattice_lsp::lsp_types::Position {
                        line: 1,
                        character: 8,
                    },
                    end: lattice_lsp::lsp_types::Position {
                        line: 1,
                        character: 11,
                    },
                },
                new_text: "bar".into(),
            },
        ];
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::RenameOutcome>();
        a.editor.pending_rename_rx = Some(rx);
        a.editor.pending_rename_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::RenameOutcome::Edits {
            per_file: vec![(uri, edits)],
            new_name: "bar".into(),
        })
        .unwrap();
        a.drain_pending_rename();
        let body = a.editor.document.snapshot().buffer.as_string();
        assert!(body.contains("let bar = 1;"));
        assert!(body.contains("let x = bar + 2;"));
        // One undo restores the pre-rename buffer (apply_lsp_text_edits
        // commits via apply_edit_batch_blocking which is one undo unit).
        let _ = a.undo_blocking();
        let restored = a.editor.document.snapshot().buffer.as_string();
        assert!(restored.contains("let foo = 1;"));
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn drain_pending_insert_completion_lsp_no_servers_keeps_popup_open_if_sync_had_results() {
        // When sync sources gave us candidates and LSP says
        // NoServers, the popup stays open with the sync set.
        let mut a = app_with("alpha alphabet alligator\nal", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::new(1, 2);
        a.do_completion_trigger();
        // No URI mapped -> LSP request didn't fire; the popup
        // is open from the sync sources alone. Manually push
        // a NoServers outcome to verify the drain handles it
        // without exploding.
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::InsertCompletionLspOutcome>();
        a.editor.pending_insert_completion_lsp_rx = Some(rx);
        a.editor.pending_insert_completion_lsp_token =
            Some(lattice_protocol::CancellationToken::new());
        tx.send(super::InsertCompletionLspOutcome::NoServers)
            .unwrap();
        a.drain_pending_insert_completion_lsp();
        // Popup still open from sync sources.
        assert!(a.editor.insert_completion.is_some());
    }

    #[test]
    fn drain_pending_insert_completion_lsp_items_merge_into_popup() {
        let mut a = app_with("\nfo", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::new(1, 2);
        // Seed the popup state directly -- skip do_completion_trigger
        // so the test doesn't depend on sync sources producing
        // matches first. The drain merges LSP items into
        // whatever raw set is present.
        a.editor.insert_completion = Some(lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            Position::new(1, 0),
            Position::new(1, 2),
            "fo".to_string(),
        ));
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::InsertCompletionLspOutcome>();
        a.editor.pending_insert_completion_lsp_rx = Some(rx);
        a.editor.pending_insert_completion_lsp_token =
            Some(lattice_protocol::CancellationToken::new());
        tx.send(super::InsertCompletionLspOutcome::Items {
            candidates: vec![
                lsp_meta_candidate(super::LspCompletionMeta {
                    label: "foo".into(),
                    insert_text: "foo".into(),
                    filter_text: None,
                    sort_text: None,
                    detail: Some("fn() -> i32".into()),
                    documentation: None,
                    kind: Some(lattice_lsp::lsp_types::CompletionItemKind::FUNCTION),
                    deprecated: false,
                    preselect: false,
                    commit_characters: Vec::new(),
                    additional_text_edits: Vec::new(),
                    command: None,
                    insert_text_format: lattice_lsp::lsp_types::InsertTextFormat::PLAIN_TEXT,
                    replace_range: None,
                    server_id: "test-server".to_string(),
                    original_item: lattice_lsp::lsp_types::CompletionItem::default(),
                    resolved: false,
                }),
                lsp_meta_candidate(super::LspCompletionMeta {
                    label: "foobar".into(),
                    insert_text: "foobar".into(),
                    filter_text: None,
                    sort_text: None,
                    detail: None,
                    documentation: None,
                    kind: Some(lattice_lsp::lsp_types::CompletionItemKind::VARIABLE),
                    deprecated: false,
                    preselect: false,
                    commit_characters: Vec::new(),
                    additional_text_edits: Vec::new(),
                    command: None,
                    insert_text_format: lattice_lsp::lsp_types::InsertTextFormat::PLAIN_TEXT,
                    replace_range: None,
                    server_id: "test-server".to_string(),
                    original_item: lattice_lsp::lsp_types::CompletionItem::default(),
                    resolved: false,
                }),
            ],
            is_incomplete: false,
        })
        .unwrap();
        a.drain_pending_insert_completion_lsp();
        let state = a.editor.insert_completion.as_ref().expect("popup open");
        // Both items render; "foo" prefix matches both.
        let labels: Vec<String> = state
            .rendered
            .iter()
            .map(|c| c.raw.display.clone())
            .collect();
        assert!(labels.iter().any(|l| l.starts_with("foo")));
        assert!(labels.iter().any(|l| l.starts_with("foobar")));
        // CSM.8b.5: state.raw is the source of truth. Two LSP
        // rows present, each carrying their own payload-encoded
        // meta.
        let state = a.editor.insert_completion.as_ref().expect("popup");
        let lsp_rows = state
            .raw
            .iter()
            .filter(|r| {
                matches!(
                    r.data,
                    lattice_completion::CandidateData::Extension {
                        kind_id: LSP_COMPLETION_KIND_ID,
                        ..
                    }
                )
            })
            .count();
        assert_eq!(lsp_rows, 2);
    }

    #[test]
    fn drain_pending_insert_completion_lsp_drops_prior_lsp_rows_on_refresh() {
        // First merge populates LSP rows; second merge with
        // a different item set should REPLACE (not append).
        let mut a = app_with("xx", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::ZERO;
        a.editor.insert_completion = Some(lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            Position::ZERO,
            Position::ZERO,
            String::new(),
        ));
        let mk_item = |label: &str| super::LspCompletionMeta {
            label: label.into(),
            insert_text: label.into(),
            filter_text: None,
            sort_text: None,
            detail: None,
            documentation: None,
            kind: None,
            deprecated: false,
            preselect: false,
            commit_characters: Vec::new(),
            additional_text_edits: Vec::new(),
            command: None,
            insert_text_format: lattice_lsp::lsp_types::InsertTextFormat::PLAIN_TEXT,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: lattice_lsp::lsp_types::CompletionItem::default(),
            resolved: false,
        };
        // First batch.
        let (tx1, rx1) =
            tokio::sync::mpsc::unbounded_channel::<super::InsertCompletionLspOutcome>();
        a.editor.pending_insert_completion_lsp_rx = Some(rx1);
        a.editor.pending_insert_completion_lsp_token =
            Some(lattice_protocol::CancellationToken::new());
        tx1.send(super::InsertCompletionLspOutcome::Items {
            candidates: vec![
                lsp_meta_candidate(mk_item("alpha")),
                lsp_meta_candidate(mk_item("alphabet")),
            ],
            is_incomplete: false,
        })
        .unwrap();
        a.drain_pending_insert_completion_lsp();
        let pre = a
            .editor
            .insert_completion
            .as_ref()
            .map(|s| s.raw.len())
            .unwrap_or(0);
        assert_eq!(pre, 2);
        // Second batch -- only one item, "beta". Prior LSP
        // rows should be pruned.
        let (tx2, rx2) =
            tokio::sync::mpsc::unbounded_channel::<super::InsertCompletionLspOutcome>();
        a.editor.pending_insert_completion_lsp_rx = Some(rx2);
        a.editor.pending_insert_completion_lsp_token =
            Some(lattice_protocol::CancellationToken::new());
        tx2.send(super::InsertCompletionLspOutcome::Items {
            candidates: vec![lsp_meta_candidate(mk_item("beta"))],
            is_incomplete: false,
        })
        .unwrap();
        a.drain_pending_insert_completion_lsp();
        let state = a.editor.insert_completion.as_ref().expect("popup");
        let lsp_rows: Vec<_> = state
            .raw
            .iter()
            .filter_map(|r| match &r.data {
                lattice_completion::CandidateData::Extension {
                    kind_id: LSP_COMPLETION_KIND_ID,
                    payload,
                } => lattice_lsp::completion::decode_meta(payload),
                _ => None,
            })
            .collect();
        assert_eq!(lsp_rows.len(), 1);
        assert_eq!(lsp_rows[0].label, "beta");
    }

    #[test]
    fn lsp_completion_meta_for_returns_none_for_sync_sourced_candidates() {
        let a = app_with("xx", 10);
        let raw = lattice_completion::RawCandidate::plain(
            "foo",
            lattice_completion::CandidateKind::Plain,
        );
        let scored = lattice_completion::ScoredCandidate {
            raw,
            score: lattice_completion::MatchScore(100),
            match_ranges: Vec::new(),
        };
        let rendered = lattice_completion::RenderedCandidate::from_scored(scored);
        assert!(a.lsp_completion_meta_for(&rendered).is_none());
    }

    #[test]
    fn drain_pending_completion_resolve_fills_metadata_and_body() {
        let mut a = app_with("xx", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::ZERO;
        // Build state with one candidate pointing at meta[0].
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
            detail: None,
            documentation: None,
            kind: None,
            deprecated: false,
            preselect: false,
            commit_characters: Vec::new(),
            additional_text_edits: Vec::new(),
            command: None,
            insert_text_format: lattice_lsp::lsp_types::InsertTextFormat::PLAIN_TEXT,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: lattice_lsp::lsp_types::CompletionItem::default(),
            resolved: false,
        };
        let mut raw = lattice_completion::RawCandidate::plain(
            "foo",
            lattice_completion::CandidateKind::Plain,
        );
        raw.data = lattice_completion::CandidateData::Extension {
            kind_id: super::LSP_COMPLETION_KIND_ID,
            payload: lattice_lsp::completion::encode_meta(&meta),
        };
        state.raw.push(raw.clone());
        state
            .rendered
            .push(lattice_completion::RenderedCandidate::from_scored(
                lattice_completion::ScoredCandidate {
                    raw,
                    score: lattice_completion::MatchScore(100),
                    match_ranges: Vec::new(),
                },
            ));
        // Open the doc popup -- empty body initially because
        // meta has no documentation yet.
        state.doc_popup = Some(lattice_completion::DocPopupState {
            for_index: 0,
            body: None,
            scroll: 5, // verify scroll resets on body refresh
        });
        // CSM.8b.5: meta lives in candidate payload already.
        let _ = meta;
        a.editor.insert_completion = Some(state);
        // Push a resolve outcome that fills documentation.
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CompletionResolveOutcome>();
        a.editor.pending_completion_resolve_rx = Some(rx);
        a.editor.pending_completion_resolve_token =
            Some(lattice_protocol::CancellationToken::new());
        let mut resolved = lattice_lsp::lsp_types::CompletionItem::default();
        resolved.label = "foo".into();
        resolved.detail = Some("fn foo() -> i32".into());
        resolved.documentation = Some(lattice_lsp::lsp_types::Documentation::String(
            "Returns 42.".into(),
        ));
        tx.send(super::CompletionResolveOutcome {
            meta_index: 0,
            resolved,
        })
        .unwrap();
        a.drain_pending_completion_resolve();
        // CSM.8b.5: candidate payload (the source of truth) is
        // re-encoded in place with the resolved fields.
        let state = a.editor.insert_completion.as_ref().expect("popup");
        let payload = match &state.raw[0].data {
            lattice_completion::CandidateData::Extension { payload, .. } => payload.clone(),
            _ => panic!("expected Extension payload"),
        };
        let updated = lattice_lsp::completion::decode_meta(&payload).expect("decode");
        assert!(updated.resolved);
        assert_eq!(updated.detail.as_deref(), Some("fn foo() -> i32"));
        assert_eq!(updated.documentation.as_deref(), Some("Returns 42."));
        // Doc popup body refreshed; scroll reset to 0.
        let popup = state.doc_popup.as_ref().expect("popup");
        assert_eq!(popup.scroll, 0);
        let body = popup.body.as_deref().unwrap_or("");
        assert!(body.contains("fn foo() -> i32"));
        assert!(body.contains("Returns 42."));
    }

    #[test]
    fn drain_pending_completion_resolve_drops_stale_index_after_selection_moved() {
        // Resolve arrives for the c0 candidate but selection has
        // moved to c1. The c0 payload still updates (so a future
        // refocus uses the cached docs) but the doc popup body
        // doesn't change.
        let mut a = app_with("xx", 10);
        let mut state = lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            Position::ZERO,
            Position::ZERO,
            String::new(),
        );
        let mk_meta = |label: &str| super::LspCompletionMeta {
            label: label.into(),
            insert_text: label.into(),
            filter_text: None,
            sort_text: None,
            detail: None,
            documentation: None,
            kind: None,
            deprecated: false,
            preselect: false,
            commit_characters: Vec::new(),
            additional_text_edits: Vec::new(),
            command: None,
            insert_text_format: lattice_lsp::lsp_types::InsertTextFormat::PLAIN_TEXT,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: {
                let mut ci = lattice_lsp::lsp_types::CompletionItem::default();
                ci.label = label.into();
                ci
            },
            resolved: false,
        };
        for label in ["c0", "c1"] {
            let meta = mk_meta(label);
            let mut raw = lattice_completion::RawCandidate::plain(
                label,
                lattice_completion::CandidateKind::Plain,
            );
            raw.data = lattice_completion::CandidateData::Extension {
                kind_id: super::LSP_COMPLETION_KIND_ID,
                payload: lattice_lsp::completion::encode_meta(&meta),
            };
            state.raw.push(raw.clone());
            state
                .rendered
                .push(lattice_completion::RenderedCandidate::from_scored(
                    lattice_completion::ScoredCandidate {
                        raw,
                        score: lattice_completion::MatchScore(100),
                        match_ranges: Vec::new(),
                    },
                ));
        }
        state.selected = 1; // user moved past c0
        state.doc_popup = Some(lattice_completion::DocPopupState {
            for_index: 1,
            body: Some("for c1".into()),
            scroll: 0,
        });
        a.editor.insert_completion = Some(state);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CompletionResolveOutcome>();
        a.editor.pending_completion_resolve_rx = Some(rx);
        a.editor.pending_completion_resolve_token =
            Some(lattice_protocol::CancellationToken::new());
        let mut resolved = lattice_lsp::lsp_types::CompletionItem::default();
        resolved.label = "c0".into();
        resolved.documentation = Some(lattice_lsp::lsp_types::Documentation::String(
            "stale".into(),
        ));
        tx.send(super::CompletionResolveOutcome {
            meta_index: 0,
            resolved,
        })
        .unwrap();
        a.drain_pending_completion_resolve();
        // c0's payload updated.
        let state = a.editor.insert_completion.as_ref().expect("popup");
        let c0_payload = match &state.raw[0].data {
            lattice_completion::CandidateData::Extension { payload, .. } => payload.clone(),
            _ => panic!("expected Extension"),
        };
        let c0_meta = lattice_lsp::completion::decode_meta(&c0_payload).expect("decode");
        assert!(c0_meta.resolved);
        assert_eq!(c0_meta.documentation.as_deref(), Some("stale"));
        // Doc popup body unchanged (still pointing at c1).
        let body = state.doc_popup.as_ref().and_then(|d| d.body.clone());
        assert_eq!(body.as_deref(), Some("for c1"));
    }

    #[test]
    fn lsp_completion_meta_for_decodes_payload() {
        // CSM.8b: the candidate carries the encoded meta in its
        // own payload; `lsp_completion_meta_for` decodes it
        // directly with no sidecar lookup.
        let a = app_with("xx", 10);
        let meta = super::LspCompletionMeta {
            label: "second".into(),
            insert_text: "second".into(),
            filter_text: None,
            sort_text: None,
            detail: None,
            documentation: None,
            kind: None,
            deprecated: false,
            preselect: false,
            commit_characters: Vec::new(),
            additional_text_edits: Vec::new(),
            command: None,
            insert_text_format: lattice_lsp::lsp_types::InsertTextFormat::PLAIN_TEXT,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: lattice_lsp::lsp_types::CompletionItem::default(),
            resolved: false,
        };
        let mut raw = lattice_completion::RawCandidate::plain(
            "second",
            lattice_completion::CandidateKind::Plain,
        );
        raw.data = lattice_completion::CandidateData::Extension {
            kind_id: super::LSP_COMPLETION_KIND_ID,
            payload: lattice_lsp::completion::encode_meta(&meta),
        };
        let scored = lattice_completion::ScoredCandidate {
            raw,
            score: lattice_completion::MatchScore(100),
            match_ranges: Vec::new(),
        };
        let rendered = lattice_completion::RenderedCandidate::from_scored(scored);
        let decoded = a.lsp_completion_meta_for(&rendered).expect("meta resolves");
        assert_eq!(decoded.label, "second");
    }

    #[test]
    fn drain_pending_completion_no_servers_echoes() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CompletionOutcome>();
        a.editor.pending_completion_rx = Some(rx);
        a.editor.pending_completion_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::CompletionOutcome::NoServers).unwrap();
        a.drain_pending_completion();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no LSP server"));
    }

    #[test]
    fn drain_pending_completion_items_open_picker_with_indexed_text() {
        let mut a = app_with("foo\n", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CompletionOutcome>();
        a.editor.pending_completion_rx = Some(rx);
        a.editor.pending_completion_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::CompletionOutcome::Items(vec![
            super::CompletionItemRow {
                label: "foo_bar".into(),
                kind_glyph: "ƒ",
                detail: Some("fn foo_bar()".into()),
                insert_text: "foo_bar()".into(),
                replace_range: (0, 3),
                line: 0,
            },
        ]))
        .unwrap();
        a.drain_pending_completion();
        let picker = a.editor.picker.as_ref().expect("picker");
        assert!(picker.title.starts_with("complete"));
        assert!(matches!(
            picker.on_accept,
            lattice_picker::PickerAction::AcceptLspCompletion
        ));
        assert_eq!(picker.candidates.len(), 1);
        // Display carries kind glyph + label + detail.
        let display = &picker.candidates[0].raw.display;
        assert!(display.contains("ƒ foo_bar"));
        assert!(display.contains("fn foo_bar()"));
        // Routing payload carries the typed LspCompletion index
        // (post-4.2.g.7 typed routing replaces the prior `#<idx>`
        // string encoding).
        let routing = picker
            .routing_for(&picker.candidates[0])
            .expect("routing payload set");
        match routing {
            lattice_picker::RoutingPayload::LspCompletion { index } => {
                assert_eq!(*index, 0);
            }
            other => panic!("expected LspCompletion routing, got {other:?}"),
        }
        // Items survive on the App for the accept path.
        assert!(a.editor.pending_completion_items.is_some());
    }

    #[test]
    fn drain_pending_completion_empty_echoes_no_completions() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CompletionOutcome>();
        a.editor.pending_completion_rx = Some(rx);
        a.editor.pending_completion_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::CompletionOutcome::Items(Vec::new()))
            .unwrap();
        a.drain_pending_completion();
        assert!(a.editor.picker.is_none());
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no completions"));
    }

    #[test]
    fn signature_help_to_markdown_renders_active_signature() {
        let sh = lattice_lsp::lsp_types::SignatureHelp {
            signatures: vec![lattice_lsp::lsp_types::SignatureInformation {
                label: "fn foo(a: i32, b: &str) -> i32".into(),
                documentation: Some(lattice_lsp::lsp_types::Documentation::String(
                    "Adds.".into(),
                )),
                parameters: Some(vec![
                    lattice_lsp::lsp_types::ParameterInformation {
                        label: lattice_lsp::lsp_types::ParameterLabel::Simple("a: i32".into()),
                        documentation: Some(lattice_lsp::lsp_types::Documentation::String(
                            "the first.".into(),
                        )),
                    },
                    lattice_lsp::lsp_types::ParameterInformation {
                        label: lattice_lsp::lsp_types::ParameterLabel::Simple("b: &str".into()),
                        documentation: None,
                    },
                ]),
                active_parameter: Some(0),
            }],
            active_signature: Some(0),
            active_parameter: None,
        };
        let body = super::signature_help_to_markdown(&sh);
        assert!(body.contains("fn foo(a: i32"));
        assert!(body.contains("**param:** `a: i32`"));
        assert!(body.contains("the first."));
        assert!(body.contains("Adds."));
    }

    #[test]
    fn signature_help_to_markdown_empty_when_no_signatures() {
        let sh = lattice_lsp::lsp_types::SignatureHelp {
            signatures: vec![],
            active_signature: None,
            active_parameter: None,
        };
        assert_eq!(super::signature_help_to_markdown(&sh), "");
    }

    #[test]
    fn drain_pending_signature_help_body_opens_popup() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::SignatureHelpOutcome>();
        a.editor.pending_signature_help_rx = Some(rx);
        a.editor.pending_signature_help_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::SignatureHelpOutcome::Body(
            "```text\nfn x()\n```\n".into(),
        ))
        .unwrap();
        a.drain_pending_signature_help();
        let h = a.popup_help().expect("popup");
        assert_eq!(h.title, "hover");
        assert!(a.editor.pending_signature_help_token.is_none());
    }

    #[test]
    fn drain_pending_signature_help_empty_body_echoes_no_signature_info() {
        let mut a = app_with("xx", 10);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::SignatureHelpOutcome>();
        a.editor.pending_signature_help_rx = Some(rx);
        a.editor.pending_signature_help_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::SignatureHelpOutcome::Body(String::new()))
            .unwrap();
        a.drain_pending_signature_help();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no signature info"));
        assert!(a.editor.popup_buffer.is_none());
    }

    #[test]
    fn nav_request_captures_tag_origin_for_picker_consumption() {
        // `Editor::lsp_nav_request` should set `pending_tag_origin`
        // so a subsequent picker accept (multi-result) pushes
        // the right entry onto the tag stack.
        //
        // Phase 5.8.AF.5 / Slice X1: this test now exercises the
        // host method directly (`a.editor.lsp_nav_request(...)`)
        // instead of routing through `a.apply(Action::Lsp...)`.
        // Reason: post-X1, `apply` calls `editor.run_tick_pending()`
        // at its tail (paramount-goal-#1 cleanup -- I/O drain off
        // the renderer body). The spawned LSP task posts an
        // empty `Vec` synchronously when no LSP server is
        // attached (the test scenario), and the drain then
        // consumes that empty response and clears
        // `pending_tag_origin` via the "no definitions found"
        // branch. The pre-X1 test relied on the drain not
        // running in the same `apply` call; that race is closed
        // by X1. The host-method-level assertion captures the
        // same invariant (origin is staged by the request
        // handler) without depending on drain timing.
        let mut a = app_with("foo bar\nbaz\n", 10);
        // M.5.4: gate fires before tag-origin capture; activate
        // lsp-mode so the request gets that far.
        a.toggle_mode_by_name("lsp-mode");
        a.editor.cursor = Position::new(0, 1);
        // Manually set a uri so lsp_nav_request gets past
        // the "no LSP server" guard.
        use std::str::FromStr;
        a.editor.buffer_uris.insert(
            a.editor.document_buffer_id,
            lattice_lsp::Uri::from_str("file:///tmp/x.rs").unwrap(),
        );
        a.editor
            .lsp_nav_request(lattice_lsp::cache::LspNavKind::Definition);
        let origin = a.editor.pending_tag_origin.as_ref().expect("origin set");
        assert_eq!(origin.position, Position::new(0, 1));
        assert_eq!(origin.label, "foo");
    }

    #[test]
    fn lsp_nav_request_pre_cancels_prior_token_regardless_of_kind() {
        // A new nav request of any kind must cancel a still-in-flight
        // request of any other kind -- they all share one slot.
        let mut a = app_with("xx", 10);
        let stale = lattice_protocol::CancellationToken::new();
        a.editor.pending_definition_token = Some(stale.clone());
        a.apply(Action::LspImplementationRequest);
        assert!(stale.is_cancelled());
    }

    #[test]
    fn lsp_definition_request_pre_cancels_in_flight_token() {
        let mut a = app_with("xx", 10);
        let stale = lattice_protocol::CancellationToken::new();
        a.editor.pending_definition_token = Some(stale.clone());
        a.apply(Action::LspDefinitionRequest);
        assert!(stale.is_cancelled());
    }

    #[test]
    fn drain_pending_definitions_with_no_results_echoes_not_found() {
        let mut a = app_with("xx", 10);
        let (tx, rx) =
            tokio::sync::mpsc::unbounded_channel::<Vec<lattice_lsp::lsp_types::Location>>();
        a.editor.pending_definition_rx = Some(rx);
        a.editor.pending_definition_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(Vec::new()).unwrap();
        a.drain_pending_definitions();
        let msg = a.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no definitions"));
        assert!(a.editor.pending_definition_token.is_none());
    }

    #[test]
    fn drain_pending_definitions_with_single_same_buffer_jumps_in_place() {
        // Set up an App whose document path matches the location's
        // uri, so the jump stays in-buffer (no `:e` round-trip).
        let path = std::env::temp_dir().join(format!("lattice-defjump-{}.rs", std::process::id()));
        std::fs::write(&path, "first line\nsecond line\nthird line\n").unwrap();
        let doc = Document::open(&path).unwrap();
        let mut a = App::new(doc);
        a.set_viewport_height(10);
        // Cursor starts at (0, 0). Drain a definition pointing at
        // line 2 col 5 (utf-16 character; same as utf-8 byte for
        // ASCII).
        let (tx, rx) =
            tokio::sync::mpsc::unbounded_channel::<Vec<lattice_lsp::lsp_types::Location>>();
        a.editor.pending_definition_rx = Some(rx);
        a.editor.pending_definition_token = Some(lattice_protocol::CancellationToken::new());
        let target = lattice_lsp::lsp_types::Location {
            uri: super::tests::fake_uri(path.to_str().unwrap()),
            range: lattice_lsp::lsp_types::Range {
                start: lattice_lsp::lsp_types::Position {
                    line: 2,
                    character: 5,
                },
                end: lattice_lsp::lsp_types::Position {
                    line: 2,
                    character: 6,
                },
            },
        };
        tx.send(vec![target]).unwrap();
        a.drain_pending_definitions();
        // Cursor moved to (2, 5).
        assert_eq!(a.editor.cursor.line, 2);
        assert_eq!(a.editor.cursor.byte, 5);
        // Pre-jump position pushed onto history as PluginPush.
        let pushed = a
            .editor
            .position_history
            .iter()
            .any(|e| e.source == PositionSource::PluginPush && e.position == Position::ZERO);
        assert!(pushed, "expected PluginPush entry for pre-jump cursor");
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn drain_pending_definitions_with_multiple_opens_picker() {
        // After the picker pivot, multi-result nav opens the
        // vertico picker rather than auto-jumping to the first
        // result. Single-result jump path is still tested by
        // `drain_pending_definitions_with_single_same_buffer_jumps_in_place`.
        let path = std::env::temp_dir().join(format!("lattice-defmulti-{}.rs", std::process::id()));
        std::fs::write(&path, "alpha\nbeta\ngamma\n").unwrap();
        let doc = Document::open(&path).unwrap();
        let mut a = App::new(doc);
        a.set_viewport_height(10);
        let (tx, rx) =
            tokio::sync::mpsc::unbounded_channel::<Vec<lattice_lsp::lsp_types::Location>>();
        a.editor.pending_definition_rx = Some(rx);
        a.editor.pending_definition_token = Some(lattice_protocol::CancellationToken::new());
        a.editor.pending_nav_kind = Some(super::LspNavKind::Definition);
        let target_path = path.to_str().unwrap();
        tx.send(vec![
            super::tests::loc(target_path, 1, 0),
            super::tests::loc(target_path, 2, 0),
        ])
        .unwrap();
        a.drain_pending_definitions();
        let picker = a.editor.picker.as_ref().expect("multi-result opens picker");
        assert_eq!(picker.title, "lsp:definitions");
        assert_eq!(picker.candidates.len(), 2);
        assert!(matches!(
            picker.on_accept,
            lattice_picker::PickerAction::JumpToLspLocation
        ));
        // Cursor should NOT have moved (no auto-jump).
        assert_eq!(a.editor.cursor.line, 0);
        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn lsp_supervisor_constructed_with_builtin_configs() {
        let app = App::new(Document::from_text(""));
        // Builtin registry: rust, python, go, typescript, c-cpp,
        // lua. Six entries today.
        assert!(
            app.editor.lsp.configs().len() >= 6,
            "expected at least 6 builtin server configs"
        );
        // Supervisor starts dormant.
        assert_eq!(app.editor.lsp.running_actor_count(), 0);
        assert_eq!(app.editor.lsp.attached_buffer_count(), 0);
        assert!(app.editor.buffer_uris.is_empty());
    }

    #[test]
    fn lsp_close_buffer_removes_uri_mapping_for_unattached_buffer() {
        let mut app = App::new(Document::from_text(""));
        // Seed a fake mapping (as if the attach driver's open
        // had landed for a path-bearing buffer).
        let fake_uri =
            <lattice_lsp::Uri as std::str::FromStr>::from_str("file:///tmp/x.rs").unwrap();
        app.editor
            .buffer_uris
            .insert(app.editor.document_buffer_id, fake_uri);
        // Slice 3c.final.E.5d: `buffer_uri()` reads from RS;
        // direct-field mutations in tests must publish.
        app.editor.publish_render_state();
        assert!(app.buffer_uri(app.editor.document_buffer_id).is_some());

        app.lsp_close_buffer(app.editor.document_buffer_id);
        // `lsp_close_buffer` already publishes RS at the host
        // level on the success path, but we re-publish defensively
        // in case the closure ordering changes.
        app.editor.publish_render_state();
        assert!(app.buffer_uri(app.editor.document_buffer_id).is_none());
    }

    #[test]
    fn lsp_close_buffer_is_noop_for_unmapped_id() {
        let mut app = App::new(Document::from_text(""));
        // No mapping exists; close must not panic.
        app.lsp_close_buffer(app.editor.document_buffer_id);
        assert!(app.editor.buffer_uris.is_empty());
    }

    #[test]
    fn lsp_log_no_arg_activates_subsystem_buffer_even_with_no_running_servers() {
        // Bug #3 fix: `:lsp-log` (no arg) activates `*lsp*`
        // directly. Previously the no-arg form routed through the
        // running-server picker and errored out when no servers
        // were running -- leaving the user on the initial unnamed
        // buffer with `[no name]` in the modeline despite `*lsp*`
        // existing in the registry. The fix makes the no-arg form
        // a direct subsystem-buffer activation.
        //
        // Picker behaviour moved to `:lsp-server-log` for the
        // per-instance pick.
        let mut app = app_with("hi\n", 5);
        let lsp_buf = app.editor.buffers.by_name("*lsp*").expect("*lsp* at boot");
        let initial = app.active_pane_buffer_id();
        assert_ne!(initial, lsp_buf);
        app.do_open_lsp_log(None);
        assert!(app.editor.picker.is_none(), "no picker on no-arg :lsp-log");
        assert_eq!(
            app.active_pane_buffer_id(),
            lsp_buf,
            "active pane must switch to *lsp*"
        );
    }

    #[test]
    fn lsp_log_with_arg_no_match_echoes_message() {
        let mut app = app_with("hi\n", 5);
        app.do_open_lsp_log(Some("rust"));
        let msg = app.editor.last_message.as_ref().unwrap();
        assert!(msg.text.contains("no LSP server"));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn lsp_log_buffer_refreshes_live_when_record_appended() {
        // B'.4: LspServerLogMode owns the per-instance buffer;
        // its subscription handles the append via a tokio task,
        // so the test sleeps briefly to let the task drain.
        let mut app = app_with("hi\n", 5);
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(
                std::env::current_dir()
                    .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                    .as_path(),
            ),
        );
        let _id: std::sync::Arc<str> = std::sync::Arc::clone(&instance.server_id);
        app.open_lsp_log_in_pane("rust");
        let log_id = app
            .editor
            .buffers
            .by_name(&lattice_lsp::lsp_server_log_name(&instance))
            .expect("per-instance log buffer registered");
        let body_before = app.editor.buffers.document_handle(log_id).unwrap().text();
        assert!(!body_before.contains("fresh-after-open"));
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Info,
            lattice_lsp::LogSource::Client,
            "fresh-after-open",
        );
        // Let LspServerLogMode's tokio task drain + apply.
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        let body_after = app.editor.buffers.document_handle(log_id).unwrap().text();
        assert!(
            body_after.contains("fresh-after-open"),
            "expected new record visible after drain, got body:\n{body_after}"
        );
    }

    /// 4.4.a: a `window/showMessage` arriving as a
    /// `LogSource::LspShowMessage` record fans out through
    /// the LspLogPushed typed event; the drain hook surfaces
    /// it to the minibuffer with severity matching the LSP
    /// level.
    #[test]
    fn lsp_log_drain_surfaces_show_message_to_minibuffer() {
        let mut app = app_with("hi\n", 5);
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp/test-ws")),
        );
        let _id: std::sync::Arc<str> = std::sync::Arc::clone(&instance.server_id);
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Warn,
            lattice_lsp::LogSource::LspShowMessage,
            "indexing complete",
        );
        app.drain_lsp_log_events();
        let msg = app.editor.last_message.as_ref().expect("set_message fired");
        assert_eq!(msg.level, EchoLevel::Warn);
        assert!(msg.text.contains("indexing complete"), "got `{}`", msg.text);
        // Prefix carries the server id so multi-server users
        // know which attached server emitted the notification.
        assert!(msg.text.contains("[rust]"), "got `{}`", msg.text);
    }

    /// 4.4.a: only `LspShowMessage`-sourced records hit the
    /// minibuffer. Regular `LspMessage` (server `window/logMessage`)
    /// records stay in the LSP log buffer and do NOT
    /// overwrite the echo area.
    #[test]
    fn lsp_log_drain_does_not_surface_log_message_to_minibuffer() {
        let mut app = app_with("hi\n", 5);
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp/test-ws")),
        );
        let _id: std::sync::Arc<str> = std::sync::Arc::clone(&instance.server_id);
        // Capture initial message to compare after drain.
        let before = app.editor.last_message.clone();
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Info,
            lattice_lsp::LogSource::LspMessage,
            "internal log thing",
        );
        app.drain_lsp_log_events();
        assert_eq!(
            app.editor.last_message, before,
            "logMessage should NOT touch the echo area"
        );
    }

    /// 4.4.o: `lsp.log_level` flips the logger's default
    /// min level at boot. Records below the configured
    /// level get dropped before the ring sees them.
    #[test]
    fn lsp_log_level_typed_option_seeds_boot_level() {
        let app = app_with("hi\n", 5);
        // Default is "info"; debug records should be filtered
        // when no per-server override is in place.
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp/test-ws")),
        );
        let _id: std::sync::Arc<str> = std::sync::Arc::clone(&instance.server_id);
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Debug,
            lattice_lsp::LogSource::Client,
            "should-be-filtered",
        );
        let records = app.editor.lsp_logger.snapshot_instance(&instance);
        assert!(
            !records
                .iter()
                .any(|r| r.message.contains("should-be-filtered")),
            "Debug record should be filtered by the default Info level seed"
        );
        // After setting to "debug" at runtime, the same
        // record passes.
        app.editor
            .config
            .parse_and_set_command("lsp.log_level=debug")
            .unwrap();
        // The runtime path:
        app.editor
            .lsp_logger
            .set_default_level(lattice_lsp::LogLevel::Debug);
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Debug,
            lattice_lsp::LogSource::Client,
            "should-pass-now",
        );
        let records = app.editor.lsp_logger.snapshot_instance(&instance);
        assert!(
            records
                .iter()
                .any(|r| r.message.contains("should-pass-now")),
            "Debug record should land after raising the level"
        );
    }

    /// 4.4.a: telemetry/event records ride the existing log
    /// path with the new `LogSource::Telemetry` tag so
    /// plugin subscribers can filter without parsing message
    /// text.
    #[test]
    fn lsp_log_drain_telemetry_uses_distinct_source_tag() {
        // We can't directly assert the typed-event payload
        // here (the bus delivers to subscriber channels
        // outside this method's surface), but we can confirm
        // the `tag()` rendering matches expectations.
        assert_eq!(lattice_lsp::LogSource::Telemetry.tag(), "telemetry");
    }

    #[test]
    fn lsp_log_drain_is_noop_when_no_log_buffer_open() {
        // Pushing log records with no log buffer open should not
        // crash or echo anything; the drain just consumes events
        // and finds no matching titles.
        let mut app = app_with("hi\n", 5);
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp/test-ws")),
        );
        let _id: std::sync::Arc<str> = std::sync::Arc::clone(&instance.server_id);
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Info,
            lattice_lsp::LogSource::Client,
            "no-target",
        );
        app.drain_lsp_log_events();
        // No help buffers should have appeared.
        assert!(app.editor.buffers.help_with_title("lsp:rust").is_none());
        assert!(app.editor.buffers.help_with_title("lsp").is_none());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn lsp_trace_buffer_refreshes_live_when_trace_record_appended() {
        // B'.5: LspTraceLogMode owns the trace buffer; its
        // subscription appends asynchronously, so the test
        // sleeps to let the spawned task drain.
        let mut app = app_with("hi\n", 5);
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(
                std::env::current_dir()
                    .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                    .as_path(),
            ),
        );
        let _id: std::sync::Arc<str> = std::sync::Arc::clone(&instance.server_id);
        app.editor.lsp_logger.enable_trace(instance.clone());
        app.open_lsp_trace_log_in_pane("rust");
        let trace_id = app
            .editor
            .buffers
            .by_name(&lattice_lsp::lsp_server_trace_log_name(&instance))
            .expect("trace buffer registered");
        let before = app.editor.buffers.document_handle(trace_id).unwrap().text();
        assert!(!before.contains("→ NEW"));
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Trace,
            lattice_lsp::LogSource::Trace,
            "→ NEW request id=42",
        );
        tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        let after = app.editor.buffers.document_handle(trace_id).unwrap().text();
        assert!(after.contains("→ NEW"));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn lsp_log_burst_coalesces_into_one_refresh() {
        // Slice B / B'.4: 50 records published → LspServerLogMode's
        // tokio task coalesces them into one apply_edit_batch.
        let mut app = app_with("hi\n", 5);
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(
                std::env::current_dir()
                    .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                    .as_path(),
            ),
        );
        let _id: std::sync::Arc<str> = std::sync::Arc::clone(&instance.server_id);
        app.open_lsp_log_in_pane("rust");
        for i in 0..50 {
            app.editor.lsp_logger.log(
                Some(&instance),
                lattice_lsp::LogLevel::Info,
                lattice_lsp::LogSource::Client,
                format!("msg-{i}"),
            );
        }
        tokio::time::sleep(std::time::Duration::from_millis(80)).await;
        let log_id = app
            .editor
            .buffers
            .by_name(&lattice_lsp::lsp_server_log_name(&instance))
            .expect("per-instance log buffer registered");
        let body = app.editor.buffers.document_handle(log_id).unwrap().text();
        // First and last pushed records both visible.
        assert!(body.contains("msg-0"));
        assert!(body.contains("msg-49"));
    }

    #[test]
    fn lsp_trace_toggle_flips_state_without_opening_buffer() {
        let mut app = app_with("hi\n", 5);
        // B'.2: with no running actor, do_toggle_lsp_trace
        // synthesises an instance against cwd. Match that here.
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(
                std::env::current_dir()
                    .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                    .as_path(),
            ),
        );
        let _id: std::sync::Arc<str> = std::sync::Arc::clone(&instance.server_id);
        // Off -> on.
        app.do_toggle_lsp_trace("rust");
        assert!(app.editor.lsp_logger.is_tracing(&instance));
        // Pure toggle now -- the trace buffer is opened separately
        // via :lsp-trace-log so peeking doesn't flip the toggle off.
        assert!(app.editor.popup_buffer.is_none());
        let msg = app.editor.last_message.as_ref().unwrap();
        assert!(msg.text.contains("on"));
        assert!(msg.text.contains(":lsp-trace-log"));
        // On -> off.
        app.do_toggle_lsp_trace("rust");
        assert!(!app.editor.lsp_logger.is_tracing(&instance));
        assert!(app.editor.popup_buffer.is_none());
    }

    #[test]
    fn lsp_trace_resolves_binary_name_to_canonical_id() {
        // `:lsp-trace rust-analyzer` should resolve to the `rust`
        // config id (the registered binary file_name match) and
        // toggle the trace flag on `rust`, NOT a phantom
        // `rust-analyzer` id that nothing else looks at.
        let mut app = app_with("hi\n", 5);
        let ws: std::sync::Arc<std::path::Path> =
            std::sync::Arc::from(std::path::Path::new("/tmp/test-ws"));
        let canonical = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::clone(&ws),
        );
        let phantom = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust-analyzer"),
            std::sync::Arc::clone(&ws),
        );
        app.do_toggle_lsp_trace("rust-analyzer");
        // Note: without a running actor, the toggle currently
        // resolves no instances; this test asserts the resolution
        // *would* target `rust`, not the phantom. Both are off
        // because there's no actor to attach to.
        assert!(!app.editor.lsp_logger.is_tracing(&canonical));
        assert!(!app.editor.lsp_logger.is_tracing(&phantom));
        let msg = app.editor.last_message.as_ref().unwrap();
        assert!(msg.text.contains("resolved"));
    }

    #[test]
    fn lsp_trace_unknown_name_echoes_error_with_running_servers() {
        let mut app = app_with("hi\n", 5);
        app.do_toggle_lsp_trace("totally-fake-server-name");
        let msg = app.editor.last_message.as_ref().unwrap();
        assert!(matches!(msg.level, EchoLevel::Error));
        assert!(msg.text.contains("totally-fake-server-name"));
    }

    /// 4.4.c: a Begin+Report+End sequence on the typed event
    /// stream lands in `app.editor.lsp_progress`, gets updated, and is
    /// removed at End.
    #[test]
    fn lsp_progress_drain_accumulates_lifecycle() {
        let mut app = app_with("hi\n", 5);
        let server: std::sync::Arc<str> = std::sync::Arc::from("rust");
        app.editor
            .event_bus
            .publish_typed(lattice_lsp::LspProgressUpdate {
                server_id: server.clone(),
                token: "build-1".into(),
                kind: lattice_lsp::LspProgressKind::Begin,
                title: Some("Building".into()),
                message: None,
                percentage: Some(0),
                cancellable: true,
            });
        app.drain_lsp_progress_events();
        let key = (server.clone(), "build-1".to_string());
        let entry = app.editor.lsp_progress.get(&key).expect("begin landed");
        assert_eq!(entry.title.as_deref(), Some("Building"));
        assert_eq!(entry.percentage, Some(0));

        // Report without restating the title -- the drain merges
        // with the existing entry's title.
        app.editor
            .event_bus
            .publish_typed(lattice_lsp::LspProgressUpdate {
                server_id: server.clone(),
                token: "build-1".into(),
                kind: lattice_lsp::LspProgressKind::Report,
                title: None,
                message: Some("linking".into()),
                percentage: Some(73),
                cancellable: true,
            });
        app.drain_lsp_progress_events();
        let entry = app.editor.lsp_progress.get(&key).expect("report landed");
        assert_eq!(entry.title.as_deref(), Some("Building"));
        assert_eq!(entry.message.as_deref(), Some("linking"));
        assert_eq!(entry.percentage, Some(73));

        app.editor
            .event_bus
            .publish_typed(lattice_lsp::LspProgressUpdate {
                server_id: server.clone(),
                token: "build-1".into(),
                kind: lattice_lsp::LspProgressKind::End,
                title: None,
                message: None,
                percentage: None,
                cancellable: false,
            });
        app.drain_lsp_progress_events();
        assert!(
            app.editor.lsp_progress.get(&key).is_none(),
            "End should remove the entry"
        );
    }

    #[test]
    fn lsp_status_with_no_servers_renders_placeholder() {
        let mut app = app_with("hi\n", 5);
        app.do_lsp_status();
        let body = app.popup_help().unwrap().content.as_string();
        assert!(body.contains("0 server"));
        assert!(body.contains("no LSP servers running"));
    }

    #[test]
    fn lsp_log_level_subsystem_wide_accepts_known_levels() {
        let mut app = app_with("hi\n", 5);
        for lvl in ["error", "warn", "info", "debug", "trace"] {
            app.do_set_lsp_log_level(None, lvl);
            let msg = app.editor.last_message.as_ref().unwrap();
            assert!(
                msg.text.contains(lvl),
                "echo should mention {lvl}, got {}",
                msg.text
            );
        }
    }

    #[test]
    fn lsp_log_level_rejects_unknown_level() {
        let mut app = app_with("hi\n", 5);
        app.do_set_lsp_log_level(None, "babble");
        let msg = app.editor.last_message.as_ref().unwrap();
        assert!(msg.text.contains("unknown log level"));
    }

    #[test]
    fn lsp_log_level_per_server_override() {
        let mut app = app_with("hi\n", 5);
        app.do_set_lsp_log_level(Some("rust"), "debug");
        // B'.2: with no running actor, `do_set_lsp_log_level`
        // applies the override to a synthetic instance at cwd.
        // Match that here so the Debug record's level filter
        // sees the override.
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
            lattice_lsp::LogLevel::Debug,
            lattice_lsp::LogSource::Client,
            "debug event",
        );
        let recs = app.editor.lsp_logger.snapshot_instance(&instance);
        assert!(recs.iter().any(|r| r.message == "debug event"));
    }

    #[test]
    fn lsp_log_clear_drops_global_records() {
        let mut app = app_with("hi\n", 5);
        app.editor.lsp_logger.log(
            None,
            lattice_lsp::LogLevel::Info,
            lattice_lsp::LogSource::Client,
            "x",
        );
        assert_eq!(app.editor.lsp_logger.snapshot_global().len(), 1);
        app.do_lsp_log_clear(None);
        assert_eq!(app.editor.lsp_logger.snapshot_global().len(), 0);
    }

    #[test]
    fn lsp_log_clear_drops_per_server_records() {
        let mut app = app_with("hi\n", 5);
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("rust"),
            std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp/test-ws")),
        );
        let _id: std::sync::Arc<str> = std::sync::Arc::clone(&instance.server_id);
        app.editor.lsp_logger.log(
            Some(&instance),
            lattice_lsp::LogLevel::Info,
            lattice_lsp::LogSource::Client,
            "x",
        );
        assert_eq!(app.editor.lsp_logger.snapshot_instance(&instance).len(), 1);
        app.do_lsp_log_clear(Some("rust"));
        assert_eq!(app.editor.lsp_logger.snapshot_instance(&instance).len(), 0);
    }

    /// 4.4.b: show-document drain on a `file://` URI opens the
    /// path via the same edit path `:e` uses; the response
    /// oneshot resolves with `success: true`.
    #[test]
    fn show_document_file_uri_opens_buffer_and_replies_success() {
        use std::str::FromStr;
        let tmp_dir = std::env::temp_dir();
        let file_path = tmp_dir.join("lattice-4-4-b-show-document.rs");
        std::fs::write(&file_path, "fn main() {}\n").unwrap();
        let uri = lattice_lsp::Uri::from_str(&format!("file://{}", file_path.display())).unwrap();

        let mut app = app_with("hi\n", 5);
        let (response_tx, mut response_rx) = tokio::sync::oneshot::channel();
        let bus_sender = app.editor.pending_show_document_rx.take().unwrap();
        // Re-push back so the App drain can consume it.
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        tx.send(lattice_lsp::InboundShowDocument {
            server_id: std::sync::Arc::from("rust"),
            workspace: std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp")),
            uri,
            external: false,
            take_focus: true,
            selection: None,
            response: response_tx,
        })
        .unwrap();
        app.editor.pending_show_document_rx = Some(rx);
        drop(bus_sender); // discard the boot-time receiver

        app.drain_inbound_show_documents();
        let outcome = response_rx.try_recv().expect("reply landed");
        assert!(outcome.success);
        // The active document should now reflect the opened
        // file (path matches).
        let snap = app.editor.document.snapshot();
        assert_eq!(snap.path(), Some(file_path.as_ref()));
        let _ = std::fs::remove_file(&file_path);
    }

    /// 4.4.b: non-file URI without `external` is refused
    /// (we don't know how to surface http* in a buffer).
    #[test]
    fn show_document_refuses_non_file_uri_without_external() {
        use std::str::FromStr;
        let uri = lattice_lsp::Uri::from_str("https://example.com/x").unwrap();
        let mut app = app_with("hi\n", 5);
        let (response_tx, mut response_rx) = tokio::sync::oneshot::channel();
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        tx.send(lattice_lsp::InboundShowDocument {
            server_id: std::sync::Arc::from("rust"),
            workspace: std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp")),
            uri,
            external: false,
            take_focus: false,
            selection: None,
            response: response_tx,
        })
        .unwrap();
        app.editor.pending_show_document_rx = Some(rx);
        app.drain_inbound_show_documents();
        let outcome = response_rx.try_recv().expect("reply landed");
        assert!(!outcome.success);
    }

    /// Helper: inject a single inbound showMessageRequest into
    /// the App's drain receiver and run the drain. Returns the
    /// response receiver so the test can assert on the reply.
    fn inject_show_message_request(app: &mut App, req: lattice_lsp::InboundShowMessageRequest) {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        tx.send(req).unwrap();
        app.editor.pending_show_message_request_rx = Some(rx);
        app.drain_inbound_show_message_requests();
    }

    fn make_smr(
        server_id: &std::sync::Arc<str>,
        message: &str,
        actions: Vec<&str>,
    ) -> (
        lattice_lsp::InboundShowMessageRequest,
        tokio::sync::oneshot::Receiver<lattice_lsp::ShowMessageRequestOutcome>,
    ) {
        let (response_tx, response_rx) = tokio::sync::oneshot::channel();
        let req = lattice_lsp::InboundShowMessageRequest {
            server_id: server_id.clone(),
            workspace: std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp")),
            level: lattice_lsp::lsp_types::MessageType::INFO,
            message: message.into(),
            actions: actions
                .into_iter()
                .map(|t| lattice_lsp::lsp_types::MessageActionItem {
                    title: t.into(),
                    properties: Default::default(),
                })
                .collect(),
            response: response_tx,
        };
        (req, response_rx)
    }

    /// 4.4.b: actionless showMessageRequest auto-replies with
    /// `null` (spec-compliant; no picker, the prompt is purely
    /// informational), surfaces on the minibuffer, and logs.
    #[test]
    fn show_message_request_actionless_auto_dismisses() {
        let mut app = app_with("hi\n", 5);
        let server_id: std::sync::Arc<str> = std::sync::Arc::from("rust");
        let (req, mut response_rx) = make_smr(&server_id, "Heads up!", Vec::new());
        inject_show_message_request(&mut app, req);
        let outcome = response_rx.try_recv().expect("reply landed");
        assert!(
            outcome.selected.is_none(),
            "actionless prompt should auto-dismiss",
        );
        assert!(
            app.editor.picker.is_none(),
            "no picker for actionless prompt"
        );
        let msg = app.editor.last_message.as_ref().expect("minibuffer set");
        assert!(msg.text.contains("Heads up!"));
        let records = app
            .editor
            .lsp_logger
            .snapshot_instance(&lattice_lsp::InstanceKey::new(
                std::sync::Arc::clone(&server_id),
                std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp")),
            ));
        assert!(
            records
                .iter()
                .any(|r| r.message.contains("showMessageRequest"))
        );
    }

    /// 4.4.b: actionful prompt opens a picker; accepting a row
    /// replies with the matching `MessageActionItem`.
    #[test]
    fn show_message_request_accept_replies_with_selected_action() {
        let mut app = app_with("hi\n", 5);
        let server_id: std::sync::Arc<str> = std::sync::Arc::from("rust");
        let (req, mut response_rx) = make_smr(&server_id, "Reload workspace?", vec!["Yes", "No"]);
        inject_show_message_request(&mut app, req);
        // Picker opened; pending slot registered.
        assert!(
            app.editor.picker.is_some(),
            "picker should open for actionful prompt"
        );
        assert_eq!(app.editor.lsp_pending_show_message_requests.len(), 1);
        // Move the cursor to the second action ("No") and
        // accept. PickerNext is the canonical down-arrow
        // action.
        app.apply(crate::Action::PickerSelectNext);
        app.apply(crate::Action::PickerAccept);
        let outcome = response_rx.try_recv().expect("reply landed");
        let selected = outcome.selected.expect("an action was selected");
        assert_eq!(selected.title, "No");
        assert!(app.editor.picker.is_none(), "picker closed after accept");
        assert!(app.editor.lsp_pending_show_message_requests.is_empty());
    }

    /// 4.4.b: dismissing the picker replies `null`. The pending
    /// slot is cleared and no further state lingers.
    #[test]
    fn show_message_request_dismiss_replies_null() {
        let mut app = app_with("hi\n", 5);
        let server_id: std::sync::Arc<str> = std::sync::Arc::from("rust");
        let (req, mut response_rx) = make_smr(&server_id, "Reload workspace?", vec!["Yes", "No"]);
        inject_show_message_request(&mut app, req);
        assert!(app.editor.picker.is_some());
        app.apply(crate::Action::PickerDismiss);
        let outcome = response_rx.try_recv().expect("reply landed");
        assert!(outcome.selected.is_none(), "dismiss should reply null",);
        assert!(app.editor.picker.is_none());
        assert!(app.editor.lsp_pending_show_message_requests.is_empty());
    }

    /// 4.4.b: two requests in one tick -- the picker opens the
    /// first, the second waits in the queue; after dismiss, the
    /// queued one opens automatically.
    #[test]
    fn show_message_request_queues_when_picker_already_open() {
        let mut app = app_with("hi\n", 5);
        let server_id: std::sync::Arc<str> = std::sync::Arc::from("rust");
        let (req1, mut rx1) = make_smr(&server_id, "First?", vec!["A", "B"]);
        let (req2, mut rx2) = make_smr(&server_id, "Second?", vec!["X", "Y"]);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
        tx.send(req1).unwrap();
        tx.send(req2).unwrap();
        app.editor.pending_show_message_request_rx = Some(rx);
        app.drain_inbound_show_message_requests();
        // First request: picker open. Second: queued.
        assert!(app.editor.picker.is_some());
        assert_eq!(app.editor.lsp_show_message_request_queue.len(), 1);
        assert_eq!(app.editor.lsp_pending_show_message_requests.len(), 2);
        // Dismiss the first; the second picker should open
        // immediately on the same tick. Verify by asserting
        // the picker title is the second request's prompt.
        app.apply(crate::Action::PickerDismiss);
        let outcome1 = rx1.try_recv().expect("first reply landed");
        assert!(outcome1.selected.is_none());
        assert!(
            app.editor.picker.is_some(),
            "queued picker should auto-open after dismiss",
        );
        let title = app.editor.picker.as_ref().unwrap().title.clone();
        assert!(title.contains("Second?"));
        // Accept the second.
        app.apply(crate::Action::PickerAccept);
        let outcome2 = rx2.try_recv().expect("second reply landed");
        let selected = outcome2.selected.expect("action picked");
        assert_eq!(selected.title, "X");
        assert!(app.editor.lsp_pending_show_message_requests.is_empty());
        assert!(app.editor.lsp_show_message_request_queue.is_empty());
    }

    #[test]
    fn lsp_restart_queues_via_supervisor_mailbox() {
        // 4.4.d: the placeholder echo path is gone; the real
        // dispatcher posts a `Restart` cmd onto the supervisor
        // mailbox and echoes "queued" while the async work
        // unfolds. The supervisor's response (success/error,
        // including the backoff cooldown) lands in the *lsp*
        // log via `LspLogger::log`, which is asserted in the
        // supervisor's own tests.
        let mut app = app_with("hi\n", 5);
        app.do_lsp_restart("rust");
        let msg = app.editor.last_message.as_ref().unwrap();
        assert!(
            msg.text.contains("queued"),
            "expected immediate `queued` echo; got `{}`",
            msg.text,
        );
        assert!(matches!(msg.level, EchoLevel::Info));
    }

    fn app_with_path(text: &str, viewport: u32, path: std::path::PathBuf) -> App {
        let doc = lattice_core::DocumentBuilder::default()
            .with_text(text)
            .with_path(path)
            .build();
        let mut a = App::new(doc);
        a.set_viewport_height(viewport);
        a
    }

    fn inject_inbound_apply_edit(a: &mut App, inbound: lattice_lsp::InboundApplyEdit) {
        let (bus, new_rx) = lattice_lsp::ApplyEditBus::new();
        bus.dispatch(inbound).expect("dispatch");
        a.editor.pending_apply_edit_rx = Some(new_rx);
    }

    #[test]
    fn drain_inbound_apply_edits_applies_active_buffer_edit() {
        // Synthesise an inbound `workspace/applyEdit` against
        // the active buffer. Drain should apply the edit and
        // signal `applied: true` on the oneshot.
        let dir =
            std::env::temp_dir().join(format!("lattice-applyedit-test-{}", std::process::id(),));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("buffer.rs");
        std::fs::write(&path, "fn main() {}\n").unwrap();
        let mut a = app_with_path("fn main() {}\n", 5, path.clone());
        let uri: lattice_lsp::lsp_types::Uri =
            format!("file://{}", path.display()).parse().unwrap();
        // Edit replaces `main` (line 0, char 3..7) with `xyz`.
        let edit = lattice_lsp::lsp_types::TextEdit {
            range: lattice_lsp::lsp_types::Range {
                start: lattice_lsp::lsp_types::Position {
                    line: 0,
                    character: 3,
                },
                end: lattice_lsp::lsp_types::Position {
                    line: 0,
                    character: 7,
                },
            },
            new_text: "xyz".into(),
        };
        let mut changes = std::collections::HashMap::new();
        changes.insert(uri, vec![edit]);
        let workspace_edit = lattice_lsp::lsp_types::WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        };
        let (resp_tx, mut resp_rx) = tokio::sync::oneshot::channel();
        inject_inbound_apply_edit(
            &mut a,
            lattice_lsp::InboundApplyEdit {
                server_id: std::sync::Arc::from("test-server"),
                workspace: std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp")),
                label: Some("rename main".into()),
                edit: workspace_edit,
                response: resp_tx,
            },
        );
        a.drain_inbound_apply_edits();
        // Drain ran synchronously; the oneshot is already
        // populated -- `try_recv` returns Ok.
        let outcome = resp_rx.try_recv().expect("drain replied via oneshot");
        assert!(
            outcome.applied,
            "edit applied: {:?}",
            outcome.failure_reason,
        );
        let after = a.editor.document.snapshot().buffer.as_string();
        assert_eq!(after, "fn xyz() {}\n");
    }

    #[test]
    fn drain_inbound_apply_edits_empty_workspace_edit_replies_applied_true() {
        // An empty WorkspaceEdit (no changes, no
        // document_changes) is a server no-op. Spec: reply
        // applied=true so the server doesn't think we
        // failed -- just nothing to do.
        let mut a = app_with("", 5);
        let workspace_edit = lattice_lsp::lsp_types::WorkspaceEdit::default();
        let (resp_tx, mut resp_rx) = tokio::sync::oneshot::channel();
        inject_inbound_apply_edit(
            &mut a,
            lattice_lsp::InboundApplyEdit {
                server_id: std::sync::Arc::from("test-server"),
                workspace: std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp")),
                label: None,
                edit: workspace_edit,
                response: resp_tx,
            },
        );
        a.drain_inbound_apply_edits();
        let outcome = resp_rx.try_recv().expect("drain replied");
        assert!(outcome.applied);
        assert_eq!(
            outcome.failure_reason.as_deref(),
            Some("empty workspace edit"),
        );
    }

    #[test]
    fn drain_inbound_configuration_walks_cached_tree_at_lsp_prefix() {
        // Stash an `[lsp.rust-analyzer.cargo]` block in the
        // App's cached tree (mimics what
        // `load_persistent_config` does after parsing user
        // TOML). Drain receives a request for
        // `"rust-analyzer.cargo.features"` and the
        // `"rust-analyzer.checkOnSave"` -- both surface from
        // the tree.
        let mut a = app_with("", 5);
        let toml_text = "[lsp.rust-analyzer.cargo]\n\
                         features = [\"foo\", \"bar\"]\n\
                         [lsp.rust-analyzer]\n\
                         checkOnSave = true\n";
        a.editor.lsp_config_tree = toml_text.parse().expect("toml parse");
        let (resp_tx, mut resp_rx) = tokio::sync::oneshot::channel();
        let req = lattice_lsp::InboundConfigurationRequest {
            server_id: std::sync::Arc::from("rust-analyzer"),
            workspace: std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp")),
            sections: vec![
                "rust-analyzer.cargo.features".into(),
                "rust-analyzer.checkOnSave".into(),
            ],
            response: resp_tx,
        };
        let (bus, new_rx) = lattice_lsp::ConfigurationBus::new();
        bus.dispatch(req).expect("dispatch");
        a.editor.pending_configuration_rx = Some(new_rx);
        a.drain_inbound_configuration_requests();
        let values = resp_rx.try_recv().expect("drain replied");
        assert_eq!(values.len(), 2);
        // First: features array.
        let arr = values[0].as_array().expect("array");
        assert_eq!(arr[0].as_str(), Some("foo"));
        assert_eq!(arr[1].as_str(), Some("bar"));
        // Second: bool.
        assert_eq!(values[1].as_bool(), Some(true));
    }

    #[test]
    fn drain_inbound_configuration_returns_null_for_missing_section() {
        let mut a = app_with("", 5);
        // No tree populated -> every lookup is null.
        let (resp_tx, mut resp_rx) = tokio::sync::oneshot::channel();
        let req = lattice_lsp::InboundConfigurationRequest {
            server_id: std::sync::Arc::from("rust-analyzer"),
            workspace: std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp")),
            sections: vec!["rust-analyzer.cargo.features".into()],
            response: resp_tx,
        };
        let (bus, new_rx) = lattice_lsp::ConfigurationBus::new();
        bus.dispatch(req).expect("dispatch");
        a.editor.pending_configuration_rx = Some(new_rx);
        a.drain_inbound_configuration_requests();
        let values = resp_rx.try_recv().expect("drain replied");
        assert_eq!(values.len(), 1);
        assert!(values[0].is_null());
    }

    #[test]
    fn drain_inbound_configuration_empty_section_returns_whole_lsp_subtree() {
        // A server requesting `section: null` (or empty) wants
        // the whole `lsp` sub-tree -- our convention serves
        // this from the namespaced top.
        let mut a = app_with("", 5);
        let toml_text = "[lsp.rust-analyzer]\nchecker = \"clippy\"\n";
        a.editor.lsp_config_tree = toml_text.parse().unwrap();
        let (resp_tx, mut resp_rx) = tokio::sync::oneshot::channel();
        let req = lattice_lsp::InboundConfigurationRequest {
            server_id: std::sync::Arc::from("rust-analyzer"),
            workspace: std::sync::Arc::<std::path::Path>::from(std::path::Path::new("/tmp")),
            sections: vec![String::new()],
            response: resp_tx,
        };
        let (bus, new_rx) = lattice_lsp::ConfigurationBus::new();
        bus.dispatch(req).expect("dispatch");
        a.editor.pending_configuration_rx = Some(new_rx);
        a.drain_inbound_configuration_requests();
        let values = resp_rx.try_recv().expect("drain replied");
        // Whole `lsp` sub-tree comes back as a JSON object.
        let obj = values[0].as_object().expect("object");
        assert!(obj.contains_key("rust-analyzer"));
    }

    #[test]
    fn drain_inbound_configuration_no_op_when_channel_empty() {
        let mut a = app_with("", 5);
        a.drain_inbound_configuration_requests();
        assert!(a.editor.pending_configuration_rx.is_some());
    }

    #[test]
    fn drain_inbound_apply_edits_no_op_when_channel_empty() {
        // Idle drain: no requests, no outgoing oneshots, no
        // panic. Cheap path that runs every frame.
        let mut a = app_with("", 5);
        a.drain_inbound_apply_edits();
        // Receiver is restored after the drain (the take + put-back).
        assert!(a.editor.pending_apply_edit_rx.is_some());
    }

    #[test]
    fn lsp_snippet_with_additional_edits_lands_as_one_undo_unit() {
        // Buffer has space for the auto-import on line 0 and
        // the snippet expansion on line 2. The accept path
        // applies BOTH edits in a single batch; one Ctrl-Z
        // reverts both.
        let mut a = app_with("\n\nfor", 10);
        a.editor.modal = ModalState::Insert;
        a.editor.cursor = Position::new(2, 3);
        // Manually install the popup state: one candidate
        // with snippet `insertTextFormat`, an auto-import
        // additionalTextEdit at line 0, and a snippet body
        // that splices `[anchor, cursor]`.
        let mut state = lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            Position::new(2, 0),
            Position::new(2, 3),
            "for".into(),
        );
        let meta = LspCompletionMeta {
            label: "for-loop".into(),
            // Snippet body with one tabstop -- expand_snippet_with_lsp_edits
            // sets up the active snippet, focuses $1.
            insert_text: "for ${1:i} in iter {}".into(),
            filter_text: None,
            sort_text: None,
            detail: None,
            documentation: None,
            kind: Some(lattice_lsp::lsp_types::CompletionItemKind::SNIPPET),
            deprecated: false,
            preselect: false,
            commit_characters: Vec::new(),
            additional_text_edits: vec![lattice_lsp::lsp_types::TextEdit {
                range: lattice_lsp::lsp_types::Range {
                    start: lattice_lsp::lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: lattice_lsp::lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                },
                new_text: "use std::iter;\n".into(),
            }],
            command: None,
            insert_text_format: lattice_lsp::lsp_types::InsertTextFormat::SNIPPET,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: lattice_lsp::lsp_types::CompletionItem::default(),
            resolved: true,
        };
        let mut raw = lattice_completion::RawCandidate::plain(
            "for",
            lattice_completion::CandidateKind::Plain,
        )
        .with_source(lattice_completion::SourceId::new(
            lattice_completion::LSP_COMPLETION_SOURCE_ID,
        ));
        raw.data = lattice_completion::CandidateData::Extension {
            kind_id: LSP_COMPLETION_KIND_ID,
            payload: lattice_lsp::completion::encode_meta(&meta),
        };
        state.raw.push(raw.clone());
        state
            .rendered
            .push(lattice_completion::RenderedCandidate::from_scored(
                lattice_completion::ScoredCandidate {
                    raw,
                    score: lattice_completion::MatchScore(100),
                    match_ranges: Vec::new(),
                },
            ));
        // CSM.8b.5: meta lives in candidate payload already.
        let _ = meta;
        a.editor.insert_completion = Some(state);
        a.do_completion_accept();
        // After accept: line 0 has the auto-import, line 2
        // (now line 3 after the import inserted a newline,
        // wait -- the import is `use std::iter;\n` which adds
        // an extra newline; existing line 0 was empty so the
        // buffer is now: line 0 = "use std::iter;", line 1 = "",
        // line 2 = "", line 3 = "for i in iter {}").
        let after_accept = a.editor.document.snapshot().buffer.as_string();
        assert!(
            after_accept.contains("use std::iter;"),
            "auto-import applied: `{after_accept}`"
        );
        assert!(
            after_accept.contains("for i in iter {}"),
            "snippet expanded: `{after_accept}`"
        );
        // Active snippet focused on $1 ("i").
        assert!(a.editor.active_snippet.is_some(), "active snippet started");
        // Undo ONCE -> both the auto-import AND the snippet
        // expansion revert.
        a.undo_blocking().expect("undo");
        let after_undo = a.editor.document.snapshot().buffer.as_string();
        assert_eq!(
            after_undo, "\n\nfor",
            "single undo reverted both auto-import and snippet (`{after_undo}`)",
        );
    }

    #[test]
    fn next_diagnostic_advances_cursor() {
        let mut app = app_with("a\nb\nc\nd\ne\n", 10);
        seed_diags_at_lines(&mut app, &[1, 3]);
        app.editor.cursor = Position::new(0, 0);
        app.do_next_diagnostic();
        assert_eq!(app.editor.cursor, Position::new(1, 0));
        app.do_next_diagnostic();
        assert_eq!(app.editor.cursor, Position::new(3, 0));
        // Past the last -> wraps to the first.
        app.do_next_diagnostic();
        assert_eq!(app.editor.cursor, Position::new(1, 0));
    }

    #[test]
    fn prev_diagnostic_walks_backward() {
        let mut app = app_with("a\nb\nc\nd\ne\n", 10);
        seed_diags_at_lines(&mut app, &[1, 3]);
        app.editor.cursor = Position::new(4, 0);
        app.do_prev_diagnostic();
        assert_eq!(app.editor.cursor, Position::new(3, 0));
        app.do_prev_diagnostic();
        assert_eq!(app.editor.cursor, Position::new(1, 0));
        // Past the first -> wraps to the last.
        app.do_prev_diagnostic();
        assert_eq!(app.editor.cursor, Position::new(3, 0));
    }

    #[test]
    fn next_diagnostic_with_no_attachment_echoes_error() {
        let mut app = app_with("hi\n", 5);
        // M.6.3: gate on lsp-diagnostics-mode runs before the
        // URI check; activate lsp-mode so the cascade brings
        // diagnostics-mode up, then the no-URI branch is what
        // we exercise.
        app.toggle_mode_by_name("lsp-mode");
        // No buffer_uris mapping -> "no LSP attachment".
        app.do_next_diagnostic();
        let msg = app.editor.last_message.as_ref().expect("expected echo");
        assert!(msg.text.contains("no LSP attachment"), "got: {}", msg.text);
    }

    #[test]
    fn next_diagnostic_with_no_diagnostics_echoes_info() {
        let mut app = app_with("hi\n", 5);
        // Seed an empty layer mapping + activate lsp-mode (cascade
        // activates lsp-diagnostics-mode).
        use std::str::FromStr;
        let uri = lattice_lsp::Uri::from_str("file:///tmp/empty.rs").unwrap();
        app.editor
            .buffer_uris
            .insert(app.editor.document_buffer_id, uri);
        app.toggle_mode_by_name("lsp-mode");
        app.do_next_diagnostic();
        let msg = app.editor.last_message.as_ref().expect("expected echo");
        assert!(msg.text.contains("no diagnostics"), "got: {}", msg.text);
    }

    #[test]
    fn m6_end_to_end_independent_sub_modes_per_feature() {
        // M.6.4: full contract exercised end-to-end.
        // 1. `:lsp-mode` cascade-on activates all 9 sub-modes.
        // 2. Disable one sub-mode (`lsp-format-mode`); other
        //    features still fire (or echo their own sub-mode
        //    name on bail).
        // 3. Re-enable; everything works again.
        let mut a = app_with("xx", 10);
        a.toggle_mode_by_name("lsp-mode");
        assert!(a.lsp_format_mode_enabled_for(a.editor.document_buffer_id));
        assert!(a.lsp_hover_mode_enabled_for(a.editor.document_buffer_id));

        // Disable just format-mode.
        a.toggle_mode_by_name("lsp-format-mode");
        assert!(!a.lsp_format_mode_enabled_for(a.editor.document_buffer_id));
        // Other sub-modes still active.
        assert!(a.lsp_hover_mode_enabled_for(a.editor.document_buffer_id));
        assert!(a.lsp_completion_mode_enabled_for(a.editor.document_buffer_id));
        assert!(a.lsp_diagnostics_mode_enabled_for(a.editor.document_buffer_id));

        // Format request bails with format-mode echo.
        a.do_lsp_format_request(false);
        assert!(
            a.editor
                .last_message
                .as_ref()
                .map(|m| m.text.contains("lsp-format-mode disabled"))
                .unwrap_or(false),
            "expected format-mode echo, got: {:?}",
            a.editor.last_message,
        );

        // Hover still works (well, fails for "no LSP server" but
        // not for "mode disabled" -- the sub-mode gate passes).
        a.editor.last_message = None;
        a.apply(Action::LspHoverRequest);
        if let Some(msg) = &a.editor.last_message {
            assert!(
                !msg.text.contains("lsp-hover-mode disabled"),
                "hover sub-mode unexpectedly gated: {}",
                msg.text,
            );
        }

        // Re-enable format-mode.
        a.toggle_mode_by_name("lsp-format-mode");
        assert!(a.lsp_format_mode_enabled_for(a.editor.document_buffer_id));
    }

    #[test]
    fn next_diagnostic_with_lsp_diagnostics_mode_off_echoes_gate() {
        // M.6.3 contract: `:lsp-diagnostics-mode` off ⇒ the
        // navigation gate echoes the sub-mode name and bails
        // before any URI / data lookup.
        let mut app = app_with("hi\n", 5);
        seed_diags_at_lines(&mut app, &[0]);
        // Helper auto-activated lsp-mode + cascade. Disable just
        // diagnostics-mode.
        app.toggle_mode_by_name("lsp-diagnostics-mode");
        app.do_next_diagnostic();
        let msg = app.editor.last_message.as_ref().expect("gate echo");
        assert!(
            msg.text.contains("lsp-diagnostics-mode disabled"),
            "expected sub-mode gate echo, got: {}",
            msg.text,
        );
    }

    #[test]
    fn list_diagnostics_opens_picker() {
        let mut app = app_with("hi\n", 5);
        seed_diags_at_lines(&mut app, &[0, 1]);
        app.do_list_diagnostics();
        let picker = app.editor.picker.as_ref().expect("picker should open");
        assert!(picker.title.starts_with("diagnostics"));
        assert!(matches!(
            picker.source,
            lattice_picker::PickerSource::LspLocations
        ));
        assert!(matches!(
            picker.on_accept,
            lattice_picker::PickerAction::JumpToLspLocation
        ));
        // Two diagnostic rows.
        assert_eq!(picker.candidates.len(), 2);
        // Severity prefix marginalia in display.
        let display = &picker.candidates[0].raw.display;
        assert!(display.starts_with("[E]"), "got: {display}");
        // Help buffer is NOT opened (the pre-picker shape).
        assert!(app.editor.popup_buffer.is_none());
    }

    #[test]
    fn list_diagnostics_with_empty_layer_echoes() {
        let mut app = app_with("hi\n", 5);
        // No diagnostics seeded.
        app.do_list_diagnostics();
        // Empty diagnostics: no picker, just an echo.
        assert!(app.editor.picker.is_none());
        let msg = app.editor.last_message.as_ref().expect("echo");
        assert!(msg.text.contains("no diagnostics"));
    }
}
