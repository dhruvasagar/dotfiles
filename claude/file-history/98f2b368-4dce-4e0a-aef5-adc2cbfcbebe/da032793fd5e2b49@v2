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

use lattice_grammar::ModalState;

use lattice_protocol::Event;

use super::{
    App, BufferKind, CodeActionOutcome, CodeActionRow, CompletionItemRow, CompletionOutcome,
    CompletionResolveOutcome, EchoLevel, FormatOutcome, HoverOutcome, InsertCompletionLspOutcome,
    LSP_COMPLETION_KIND_ID, LspCompletionMeta, LspNavKind, ReferencesOutcome, RenameOutcome,
    SignatureHelpOutcome, SymbolRow, SymbolsOutcome, TagStackEntry, app_to_lsp_position,
    call_hierarchy_to_row, code_action_kind_glyph, completion_kind_glyph, dedup_rendered_by_text,
    definition_response_to_locations, flatten_document_symbol_response, flatten_workspace_edit,
    hover_contents_to_markdown, is_word_char_byte, last_addressable_line, line_byte_len,
    lsp_position_to_app_byte, prepare_rename_placeholder, range_covers, signature_help_to_markdown,
    symbol_information_to_row, type_hierarchy_to_row, word_under_cursor, workspace_symbol_to_row,
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
struct BatchingSink {
    items: std::sync::Mutex<Vec<lattice_completion::RawCandidate>>,
    is_incomplete: std::sync::atomic::AtomicBool,
}

impl BatchingSink {
    fn new() -> Self {
        Self {
            items: std::sync::Mutex::new(Vec::new()),
            is_incomplete: std::sync::atomic::AtomicBool::new(false),
        }
    }

    /// Take the buffered candidate set and the incomplete
    /// flag. Called by the spawn wrapper after the future
    /// resolves.
    fn drain(&self) -> (Vec<lattice_completion::RawCandidate>, bool) {
        let items = std::mem::take(&mut *self.items.lock().expect("BatchingSink mutex"));
        let is_incomplete = self
            .is_incomplete
            .load(std::sync::atomic::Ordering::Relaxed);
        (items, is_incomplete)
    }
}

impl lattice_completion::CandidateSink for BatchingSink {
    fn push(&self, candidate: lattice_completion::RawCandidate) {
        self.items
            .lock()
            .expect("BatchingSink mutex")
            .push(candidate);
    }
    fn mark_incomplete(&self) {
        self.is_incomplete
            .store(true, std::sync::atomic::Ordering::Relaxed);
    }
}

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
    pub fn lsp_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.editor.active_modes
            .get(&buffer_id)
            .map(|modes| modes.has_minor(lattice_lsp::modes::LspMode::mode_id()))
            .unwrap_or(false)
    }

    /// M.6.0: is `mode_id` active on `buffer_id`? Generic minor-
    /// mode accessor used by every M.6 sub-mode reader. Always
    /// returns `false` when no entry exists for `buffer_id` --
    /// matches the umbrella accessor's shape.
    fn minor_mode_enabled_for(&self, buffer_id: BufferId, mode_id: lattice_mode::ModeId) -> bool {
        self.editor.active_modes
            .get(&buffer_id)
            .map(|modes| modes.has_minor(mode_id))
            .unwrap_or(false)
    }

    /// CSM.K1: is `completion-mode` (the persistent gate)
    /// active on `buffer_id`? Auto-activates on writable buffer
    /// kinds; the popup-trigger entry points check this before
    /// opening the popup so read-only buffers (Help, FileTree,
    /// Oil) silently no-op on `<C-Space>`.
    pub fn completion_mode_active_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_mode::CompletionMode::mode_id())
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
    /// `self.editor.document_buffer_id`.
    pub fn completion_popup_active(&self) -> bool {
        self.completion_popup_mode_active_for(self.editor.document_buffer_id)
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
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspDiagnosticsMode::mode_id())
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
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspFormatMode::mode_id())
    }

    /// M.6.0: is `lsp-rename-mode` active on `buffer_id`?
    pub fn lsp_rename_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspRenameMode::mode_id())
    }

    /// M.6.0: is `lsp-symbols-mode` active on `buffer_id`? Gates
    /// `:lsp-symbols` and `:lsp-workspace-symbol`.
    pub fn lsp_symbols_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspSymbolsMode::mode_id())
    }

    /// M.6.0: is `lsp-code-action-mode` active on `buffer_id`?
    pub fn lsp_code_action_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspCodeActionMode::mode_id())
    }

    /// M.6.0: is `lsp-nav-mode` active on `buffer_id`? Gates
    /// definition / declaration / type-def / impl + references.
    pub fn lsp_nav_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspNavMode::mode_id())
    }

    /// 4.4.c: is `lsp-progress-mode` active on `buffer_id`?
    /// Gates the modeline `$/progress` segment and progress
    /// accumulation for buffers attached to a server. With
    /// the mode off, incoming progress events still flow on
    /// the bus (plugins can subscribe) but the modeline stays
    /// quiet for that buffer.
    pub fn lsp_progress_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspProgressMode::mode_id())
    }

    /// 4.4.e: is `lsp-document-highlight-mode` active on
    /// `buffer_id`? Gates the cursor-driven
    /// `textDocument/documentHighlight` request issuance and
    /// the soft-highlight decoration overlay.
    pub fn lsp_document_highlight_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(
            buffer_id,
            lattice_lsp::modes::LspDocumentHighlightMode::mode_id(),
        )
    }

    /// 4.4.e: is `lsp-selection-range-mode` active on
    /// `buffer_id`? Gates `textDocument/selectionRange`
    /// issuance for the smart-expansion operator.
    pub fn lsp_selection_range_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(
            buffer_id,
            lattice_lsp::modes::LspSelectionRangeMode::mode_id(),
        )
    }

    /// 4.4.f: is `lsp-folding-mode` active on `buffer_id`?
    /// Gates `textDocument/foldingRange` issuance. Independent
    /// of the foldmethod option: when the mode is off the
    /// cache stays empty and `:set foldmethod=lsp` cascades to
    /// `Syntax`.
    pub fn lsp_folding_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspFoldingMode::mode_id())
    }

    /// 4.4.g: is `lsp-inlay-hint-mode` active on `buffer_id`?
    /// Gates `textDocument/inlayHint` issuance and the
    /// renderer overlay paint.
    pub fn lsp_inlay_hint_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(buffer_id, lattice_lsp::modes::LspInlayHintMode::mode_id())
    }

    /// 4.4.h: is `lsp-semantic-tokens-mode` active on
    /// `buffer_id`? Gates `textDocument/semanticTokens/full`
    /// issuance and the renderer's per-kind overlay.
    pub fn lsp_semantic_tokens_mode_enabled_for(&self, buffer_id: BufferId) -> bool {
        self.minor_mode_enabled_for(
            buffer_id,
            lattice_lsp::modes::LspSemanticTokensMode::mode_id(),
        )
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
        if self.lsp_mode_enabled_for(self.editor.document_buffer_id) {
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
        if self.minor_mode_enabled_for(self.editor.document_buffer_id, sub_mode_id) {
            return true;
        }
        self.set_message(
            EchoLevel::Info,
            format!("{sub_mode_name} disabled for this buffer (`:{sub_mode_name}` to enable)"),
        );
        false
    }

    /// `K` (Phase 4.2.b). Send `textDocument/hover` to every LSP
    /// server attached to the active document; the spawned task
    /// awaits the actor's response on the LSP runtime, so the
    /// keystroke handler returns instantly. The markdown body
    /// arrives back through `pending_hover_rx` and the next
    /// frame's `drain_pending_hover` feeds it into the popup.
    ///
    /// **Multi-server merge** is "first non-empty wins" for
    /// 4.2.b. **Cancellation**: any prior in-flight hover's
    /// token is flipped before the new request fires, so a slow
    /// server can't drop a stale popup over the new cursor
    /// position.
    pub(super) fn do_lsp_hover_request(&mut self) {
        // Already focused into the popup (State B) -- K is a
        // no-op. To get a fresh hover the user dismisses with
        // Esc / q, repositions in the doc, then presses K.
        if matches!(self.editor.active_buffer, BufferKind::Help) {
            return;
        }
        // Popup shown but focus still on main buffer (State A) --
        // second K transfers focus into the popup. No new LSP
        // request fires; we just promote.
        if self.editor.popup_buffer.is_some() {
            self.focus_help_popup();
            return;
        }
        // First K -- fire a fresh hover request. Cancel any
        // in-flight first. (Cancel-stale-work runs before the
        // M.5.4 gate so the prior request's relay loop sees the
        // flip even when the gate is now closed.)
        if let Some(token) = self.editor.pending_hover_token.take() {
            token.cancel();
        }
        // M.6.2: lsp-hover-mode gate (umbrella check inside).
        if !self.check_lsp_sub_mode_gate(
            lattice_lsp::modes::LspHoverMode::mode_id(),
            "lsp-hover-mode",
        ) {
            return;
        }

        // Resolve the active buffer's URI. No URI = no LSP for
        // this buffer (e.g. unsaved scratch); echo + bail.
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server attached to current buffer".to_string(),
            );
            return;
        };

        // Build the LSP-side cursor position. App's cursor is
        // (line, col_byte) in utf-8; LSP wants utf-16 columns.
        let snapshot = self.document.snapshot();
        let lsp_position = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => {
                self.set_message(EchoLevel::Error, "hover: cursor out of buffer".to_string());
                return;
            }
        };

        // Fresh channel + token for this request.
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<HoverOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_hover_rx = Some(rx);
        self.editor.pending_hover_token = Some(token.clone());

        let lsp = self.editor.lsp.clone();
        let logger = self.editor.lsp_logger.clone();
        let request_started = std::time::Instant::now();
        let request_uri = uri.as_str().to_string();
        crate::runtime::spawn_on_lsp_runtime(async move {
            // Snapshot the attached handles under the supervisor
            // lock, then drop it before awaiting any per-server
            // response.
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            if handles.is_empty() {
                let _ = tx.send(HoverOutcome::NoServers);
                return;
            }
            let mut tried = 0usize;
            for handle in handles {
                if token.is_cancelled() {
                    return;
                }
                tried += 1;
                let params = lsp_types::HoverParams {
                    text_document_position_params: lsp_types::TextDocumentPositionParams {
                        text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                        position: lsp_position,
                    },
                    work_done_progress_params: Default::default(),
                };
                let instance = handle.instance();
                logger.log(
                    Some(&instance),
                    lattice_lsp::LogLevel::Debug,
                    lattice_lsp::LogSource::Client,
                    format!(
                        "hover requested @ {request_uri} line {} character {}",
                        lsp_position.line, lsp_position.character
                    ),
                );
                match handle.hover(params, token.clone()).await {
                    Ok(Some(hover)) => {
                        let body = hover_contents_to_markdown(&hover.contents);
                        if !body.trim().is_empty() {
                            logger.log(
                                Some(&instance),
                                lattice_lsp::LogLevel::Debug,
                                lattice_lsp::LogSource::Client,
                                format!(
                                    "hover reply: {} bytes after {:?}",
                                    body.len(),
                                    request_started.elapsed()
                                ),
                            );
                            let _ = tx.send(HoverOutcome::Body(body));
                            return;
                        }
                        // Server replied but the body's empty.
                        logger.log(
                            Some(&instance),
                            lattice_lsp::LogLevel::Debug,
                            lattice_lsp::LogSource::Client,
                            "hover reply: empty body (server still indexing?)".to_string(),
                        );
                    }
                    Ok(None) => {
                        logger.log(
                            Some(&instance),
                            lattice_lsp::LogLevel::Debug,
                            lattice_lsp::LogSource::Client,
                            "hover reply: null (cursor not on a known symbol, or server still indexing)"
                                .to_string(),
                        );
                    }
                    Err(e) => {
                        logger.log(
                            Some(&instance),
                            lattice_lsp::LogLevel::Warn,
                            lattice_lsp::LogSource::Client,
                            format!("hover error: {e}"),
                        );
                    }
                }
            }
            // Walked every server, none had a non-empty body.
            let _ = tx.send(HoverOutcome::NoBody {
                servers_tried: tried,
            });
        });
    }

    /// Drain the channel populated by `do_lsp_hover_request` and
    /// act on every pending `HoverOutcome`: open the popup for
    /// `Body`, echo a clear message for `NoBody` / `NoServers` so
    /// the user always knows their `K` press was processed.
    /// Called once per main_loop iteration before draw; cheap
    /// when the channel is empty (the common case).
    pub fn drain_pending_hover(&mut self) {
        let Some(mut rx) = self.editor.pending_hover_rx.take() else {
            return;
        };
        // Last-writer-wins -- if a stale outcome and a fresh one
        // both queued, surface the latest.
        let mut latest: Option<HoverOutcome> = None;
        while let Ok(outcome) = rx.try_recv() {
            latest = Some(outcome);
        }
        if let Some(outcome) = latest {
            match outcome {
                HoverOutcome::Body(body) => {
                    self.do_open_hover(&body);
                }
                HoverOutcome::NoBody { servers_tried } => {
                    self.set_message(
                        EchoLevel::Info,
                        format!(
                            "no hover info at cursor ({servers_tried} server{} replied)",
                            if servers_tried == 1 { "" } else { "s" }
                        ),
                    );
                }
                HoverOutcome::NoServers => {
                    self.set_message(
                        EchoLevel::Warn,
                        "hover: no LSP servers attached for this buffer (\
                         check :lsp-status / :lsp-log)"
                            .to_string(),
                    );
                }
            }
            // Outcome delivered: clear the in-flight token so a
            // subsequent motion doesn't try to flip a stale token.
            self.editor.pending_hover_token = None;
        }
        self.editor.pending_hover_rx = Some(rx);
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
        // Main edit: prefer the server-supplied range when
        // present; else replace `[anchor, cursor]`.
        let main_range = match meta.replace_range {
            Some(r) => r,
            None => {
                let start = lsp_types::Position {
                    line: anchor.line,
                    character: lattice_lsp::position::utf8_byte_to_utf16_column(
                        &self
                            .document
                            .snapshot()
                            .buffer
                            .line(anchor.line)
                            .unwrap_or_default(),
                        anchor.byte,
                    ),
                };
                let end = lsp_types::Position {
                    line: self.editor.cursor.line,
                    character: lattice_lsp::position::utf8_byte_to_utf16_column(
                        &self
                            .document
                            .snapshot()
                            .buffer
                            .line(self.editor.cursor.line)
                            .unwrap_or_default(),
                        self.editor.cursor.byte,
                    ),
                };
                lsp_types::Range { start, end }
            }
        };
        // Apply additionalTextEdits + main as one batch via the
        // existing path. Sort + reverse-apply is handled there;
        // pass everything together so undo is atomic.
        let mut edits: Vec<lsp_types::TextEdit> = meta.additional_text_edits.clone();
        edits.push(lsp_types::TextEdit {
            range: main_range,
            new_text: meta.insert_text.clone(),
        });
        if let Err(e) = self.apply_lsp_text_edits(edits) {
            self.set_message(EchoLevel::Error, format!("completion: apply failed: {e}"));
            return;
        }
        // Position the cursor at the end of the just-inserted
        // text. Compute it from the inserted text length.
        let inserted_lines: Vec<&str> = meta.insert_text.split('\n').collect();
        if inserted_lines.len() == 1 {
            self.editor.cursor = lattice_protocol::position::Position::new(
                main_range.start.line,
                lattice_lsp::position::utf16_column_to_utf8_byte(
                    &self
                        .document
                        .snapshot()
                        .buffer
                        .line(main_range.start.line)
                        .unwrap_or_default(),
                    main_range.start.character + inserted_lines[0].len() as u32,
                ),
            );
        } else {
            // Multi-line insert (rare for plain completions;
            // common for snippets once 4.2.g.4 lands).
            let last_line_idx = main_range.start.line + (inserted_lines.len() as u32 - 1);
            let last_line_text = inserted_lines.last().unwrap_or(&"");
            self.editor.cursor = lattice_protocol::position::Position::new(
                last_line_idx,
                last_line_text.len() as u32,
            );
        }
        // Optional: fire the LSP `command` payload (e.g. server-
        // side post-accept hooks).
        if let Some(cmd) = meta.command.clone() {
            let uri = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned();
            if let Some(uri) = uri {
                let handle = self
                    .editor.lsp
                    .servers_for(&uri)
                    .into_iter()
                    .find(|h| h.capabilities().supports_execute_command());
                self.execute_lsp_command(handle, cmd);
            }
        }
    }

    /// Fire `completionItem/resolve` for the focused candidate
    /// (Phase 4.2.g.3). The original CompletionItem is round-
    /// tripped to the originating server; the response fills in
    /// `documentation` / `additionalTextEdits` / `detail` per
    /// the LSP spec. Drain updates the meta + the docs popup
    /// body in place.
    pub(super) fn do_completion_resolve_focused(&mut self) {
        // Cancel any prior in-flight resolve -- the focus moved
        // to a different candidate.
        if let Some(token) = self.editor.pending_completion_resolve_token.take() {
            token.cancel();
        }
        let Some(state) = self.insert_completion.as_ref() else {
            return;
        };
        let Some(cand) = state.rendered.get(state.selected) else {
            return;
        };
        let Some(meta) = self.lsp_completion_meta_for(cand) else {
            return;
        };
        if meta.resolved {
            return;
        }
        let original = meta.original_item.clone();
        let server_id = meta.server_id.clone();
        // CSM.8b: index of this meta entry within state.raw's
        // LSP-row sequence. The drain populates the parallel
        // sidecar in the same order, so this lookup hits the
        // matching meta. Match on full payload bytes since two
        // distinct items will always serialise differently (the
        // produce_async dedup happens up-stream).
        let candidate_payload = match &cand.raw.data {
            lattice_completion::CandidateData::Extension { payload, .. } => payload.clone(),
            _ => return,
        };
        let Some(state_ref) = self.insert_completion.as_ref() else {
            return;
        };
        let Some(meta_index) = state_ref
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
            .position(|r| match &r.data {
                lattice_completion::CandidateData::Extension { payload, .. } => {
                    payload == &candidate_payload
                }
                _ => false,
            })
        else {
            return;
        };
        // Resolve URI to find the originating server handle.
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<CompletionResolveOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_completion_resolve_rx = Some(rx);
        self.editor.pending_completion_resolve_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handle = lsp
                .servers_for(&uri)
                .into_iter()
                .find(|h| h.server_id() == &*server_id);
            let Some(handle) = handle else {
                return;
            };
            if !handle.capabilities().completion_resolve_provider() {
                return;
            }
            if token.is_cancelled() {
                return;
            }
            // `request_with_cancel` takes `&str` method name and
            // a serializable param; the resolved item comes back
            // as `CompletionItem`.
            let pending = handle
                .request_with_cancel::<lsp_types::CompletionItem, lsp_types::CompletionItem>(
                    "completionItem/resolve",
                    original,
                    token.clone(),
                );
            let Ok(resolved) = pending.await else {
                return;
            };
            let _ = tx.send(CompletionResolveOutcome {
                meta_index,
                resolved,
            });
        });
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
        // M.6.2: lsp-completion-mode gate (umbrella implied by
        // M.6.1 cascade: sub-mode can't be on without umbrella).
        // Insert-mode is high-frequency; silent on bail (the user
        // is typing, not invoking a discrete command).
        if !self.lsp_completion_mode_enabled_for(self.editor.document_buffer_id) {
            return;
        }
        if let Some(token) = self.editor.pending_insert_completion_lsp_token.take() {
            token.cancel();
        }
        // Path-completion mode (4.2.g.6 (2/2)) suppresses LSP
        // completion -- the popup is showing filesystem entries.
        if self.completion_in_path_context {
            return;
        }
        // Per-language sources filter (Phase 4.2.g.5 (3b/3)).
        let language = self.active_language_id();
        let effective = self.effective_completion_for(&language);
        let lsp_id =
            lattice_completion::SourceId::new(lattice_completion::LSP_COMPLETION_SOURCE_ID);
        if !effective.source_enabled(&lsp_id) {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            // No URI -- no LSP. Sync sources still populate the
            // popup; just skip the LSP request silently.
            return;
        };
        let snapshot = self.document.snapshot();
        let lsp_position = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => return,
        };
        // CSM.8b.3: pre-check "no servers attached" synchronously
        // so the existing popup-empty-close gate fires without
        // waiting on a no-op async round-trip.
        if self.editor.lsp.servers_for(&uri).is_empty() {
            let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<InsertCompletionLspOutcome>();
            self.editor.pending_insert_completion_lsp_rx = Some(rx);
            self.editor.pending_insert_completion_lsp_token = None;
            let _ = tx.send(InsertCompletionLspOutcome::NoServers);
            return;
        }
        // CSM.8b.3: dispatch through the cached `LspCompletionSource`
        // (contributed by `lsp-completion-mode`) instead of the
        // bespoke inline fan-out. The source's `produce_async`
        // owns the multi-server fan-out + dedup + per-item
        // payload encode; this method just gates, builds the
        // snapshot, and bridges the sink output onto the
        // existing channel.
        let Some(state) = self.insert_completion.as_ref() else {
            return;
        };
        let trigger = state.trigger.clone();
        let cursor = state.cursor;
        let anchor = state.anchor;
        let query = state.query.clone();
        let source = self
            .editor.buffer_locals
            .get(&self.editor.document_buffer_id)
            .and_then(|locals| locals.get::<lattice_mode::ActiveCompletionSources>())
            .and_then(|s| {
                s.0.iter().find_map(|c| match &c.kind {
                    lattice_completion::CompletionSourceKind::Async(src) if c.id == lsp_id => {
                        Some(src.clone())
                    }
                    _ => None,
                })
            });
        let Some(source) = source else {
            // Mode active but cache hasn't seeded the source yet
            // -- shouldn't happen in practice (M.6.1 cascade
            // recomputes). Silent bail.
            return;
        };
        let ctx_snapshot = lattice_completion::InsertContextSnapshot {
            cursor,
            anchor,
            query,
            trigger,
            case_sensitive: false,
            language: language.clone(),
            tree_sitter_symbols: Vec::new(),
            path_context: false,
            buffer_dir: None,
            uri: Some(uri.as_str().to_string()),
            lsp_position: Some((lsp_position.line, lsp_position.character)),
        };
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<InsertCompletionLspOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_insert_completion_lsp_rx = Some(rx);
        self.editor.pending_insert_completion_lsp_token = Some(token.clone());
        let sink = std::sync::Arc::new(BatchingSink::new());
        let sink_for_fut = sink.clone();
        let token_for_fut = token.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let fut = source.produce_async(ctx_snapshot, sink_for_fut, token_for_fut);
            fut.await;
            let (candidates, is_incomplete) = sink.drain();
            let _ = tx.send(InsertCompletionLspOutcome::Items {
                candidates,
                is_incomplete,
            });
        });
    }

    /// Drain queued `completionItem/resolve` responses --
    /// decode the matching candidate's payload, apply the
    /// resolved fields, re-encode in place, then refresh the
    /// docs-popup body when the resolved item is the popup's
    /// currently-focused one. CSM.8b.5: state.raw is the
    /// source of truth; no parallel sidecar to keep in sync.
    pub fn drain_pending_completion_resolve(&mut self) {
        let Some(mut rx) = self.editor.pending_completion_resolve_rx.take() else {
            return;
        };
        let mut latest: Option<CompletionResolveOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_completion_resolve_rx = Some(rx);
        let Some(outcome) = latest else {
            return;
        };
        self.editor.pending_completion_resolve_token = None;
        let Some(state) = self.insert_completion.as_mut() else {
            return;
        };
        // Find the n-th LSP row in state.raw (n = outcome.meta_index).
        let target_raw_idx = state
            .raw
            .iter()
            .enumerate()
            .filter(|(_, r)| {
                matches!(
                    r.data,
                    lattice_completion::CandidateData::Extension {
                        kind_id: LSP_COMPLETION_KIND_ID,
                        ..
                    }
                )
            })
            .nth(outcome.meta_index)
            .map(|(idx, _)| idx);
        let Some(target_raw_idx) = target_raw_idx else {
            return;
        };
        // Decode → apply → re-encode.
        let resolved = outcome.resolved;
        let mut meta = match &state.raw[target_raw_idx].data {
            lattice_completion::CandidateData::Extension { payload, .. } => {
                match lattice_lsp::completion::decode_meta(payload) {
                    Some(m) => m,
                    None => return,
                }
            }
            _ => return,
        };
        if let Some(d) = resolved.documentation.as_ref() {
            let body = match d {
                lsp_types::Documentation::String(s) => s.clone(),
                lsp_types::Documentation::MarkupContent(mc) => mc.value.clone(),
            };
            meta.documentation = Some(body);
        }
        if let Some(detail) = resolved.detail.clone() {
            meta.detail = Some(detail);
        }
        if let Some(adds) = resolved.additional_text_edits.clone() {
            meta.additional_text_edits = adds;
        }
        if let Some(cmd) = resolved.command.clone() {
            meta.command = Some(cmd);
        }
        meta.resolved = true;
        let new_payload = lattice_lsp::completion::encode_meta(&meta);
        state.raw[target_raw_idx].data = lattice_completion::CandidateData::Extension {
            kind_id: LSP_COMPLETION_KIND_ID,
            payload: new_payload.clone(),
        };
        // Refresh the docs popup body when this resolve was for
        // the currently-focused candidate. Match by payload bytes
        // since state.rendered's order differs from state.raw's
        // and we already have the fresh payload here.
        let Some(doc_popup) = state.doc_popup.as_mut() else {
            return;
        };
        let Some(cand) = state.rendered.get(state.selected) else {
            return;
        };
        // The selection's payload was the pre-resolve form; we
        // just rewrote state.raw[target_raw_idx], so the
        // selection still references the OLD payload via
        // state.rendered. Match by the pre-resolve payload of
        // state.raw[target_raw_idx] -- which equalled the
        // selection's payload before the rewrite. After the
        // rewrite we lost that reference, so we instead match
        // by the encoded original_item (which doesn't change
        // through resolve -- servers preserve it).
        let cand_payload = match &cand.raw.data {
            lattice_completion::CandidateData::Extension { payload, .. } => payload.clone(),
            _ => return,
        };
        let cand_original = lattice_lsp::completion::decode_meta(&cand_payload)
            .map(|m| m.original_item)
            .unwrap_or_default();
        // Compare original_items (LSP servers don't mutate them
        // across resolve).
        if cand_original != meta.original_item {
            return;
        }
        // Build the body from the freshly-resolved meta.
        let detail = meta
            .detail
            .clone()
            .filter(|s| !s.is_empty())
            .map(|s| format!("```\n{s}\n```"));
        let docs = meta.documentation.clone().filter(|s| !s.is_empty());
        doc_popup.body = match (detail, docs) {
            (Some(d), Some(b)) => Some(format!("{d}\n\n{b}")),
            (Some(d), None) => Some(d),
            (None, Some(b)) => Some(b),
            (None, None) => Some("(no documentation)".to_string()),
        };
        doc_popup.scroll = 0;
    }

    /// Per-frame drain hook -- merge any LSP completion response
    /// into the active popup's `raw` set, refilter, and update
    /// the `lsp_incomplete` flag.
    pub fn drain_pending_insert_completion_lsp(&mut self) {
        let Some(mut rx) = self.editor.pending_insert_completion_lsp_rx.take() else {
            return;
        };
        let mut latest: Option<InsertCompletionLspOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_insert_completion_lsp_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        self.editor.pending_insert_completion_lsp_token = None;
        let Some(state) = self.insert_completion.as_mut() else {
            // Popup closed before the response arrived; drop it.
            return;
        };
        match outcome {
            InsertCompletionLspOutcome::NoServers => {
                // Nothing to merge; sync sources stand alone.
            }
            InsertCompletionLspOutcome::Items {
                candidates,
                is_incomplete,
            } => {
                // Drop any prior LSP rows from raw.
                state.raw.retain(|c| {
                    !matches!(
                        c.data,
                        lattice_completion::CandidateData::Extension {
                            kind_id: LSP_COMPLETION_KIND_ID,
                            ..
                        }
                    )
                });
                // CSM.8b.5: candidates arrive pre-built by
                // `LspCompletionSource::produce_async` with the
                // full `LspCompletionMeta` serde-encoded into the
                // `Extension` payload. State.raw IS the source
                // of truth -- no parallel sidecar.
                for raw in candidates.into_iter() {
                    if matches!(
                        raw.data,
                        lattice_completion::CandidateData::Extension {
                            kind_id: LSP_COMPLETION_KIND_ID,
                            ..
                        }
                    ) {
                        state.raw.push(raw);
                    }
                }
                state.lsp_incomplete = is_incomplete;
            }
        }
        // Refilter against the (now-merged) raw set. Inline
        // mirror of refilter_insert_completion's body (we have
        // a mutable borrow on `state` here so calling the
        // helper would re-borrow).
        let matcher = lattice_completion::FuzzyInsertMatcher::new();
        let mut scored: Vec<lattice_completion::ScoredCandidate> = state
            .raw
            .iter()
            .filter_map(|raw| {
                lattice_completion::CandidateMatcher::matches(&matcher, &state.query, raw).map(
                    |(score, ranges)| lattice_completion::ScoredCandidate {
                        raw: raw.clone(),
                        score,
                        match_ranges: ranges,
                    },
                )
            })
            .collect();
        let ranker = lattice_completion::InsertRanker::new();
        let freq = &self.completion_accept_freq;
        let config = &self.editor.config;
        ranker.rank_with_bonus(&mut scored, |raw| {
            let priority = match raw.source.as_ref().map(|s| s.as_str()) {
                Some("gen:lsp-completion") => *config
                    .get_typed::<lattice_config::CompletionSourceLspPriority>()
                    .expect("CompletionSourceLspPriority"),
                Some("gen:snippet") => *config
                    .get_typed::<lattice_config::CompletionSourceSnippetPriority>()
                    .expect("CompletionSourceSnippetPriority"),
                Some("gen:buffer-words") => *config
                    .get_typed::<lattice_config::CompletionSourceBufferWordsPriority>()
                    .expect("CompletionSourceBufferWordsPriority"),
                _ => 0,
            }
            .clamp(0, u32::MAX as i64) as u32;
            let freq_bonus = freq
                .get(&(raw.text.clone(), raw.kind))
                .copied()
                .unwrap_or(0)
                .min(lattice_completion::InsertRanker::FREQUENCY_BONUS_CAP);
            priority.saturating_add(freq_bonus)
        });
        state.rendered = scored
            .into_iter()
            .map(lattice_completion::RenderedCandidate::from_scored)
            .collect();
        dedup_rendered_by_text(&mut state.rendered);
        if !state.rendered.is_empty() && state.selected >= state.rendered.len() {
            state.selected = state.rendered.len() - 1;
        }
        if state.rendered.is_empty() {
            // No matches after merge -- close the popup.
            self.insert_completion = None;
        }
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
        let Some(mut rx) = self.editor.lsp_log_event_rx.take() else {
            return;
        };
        let mut last_show: Option<(EchoLevel, String)> = None;
        while let Ok(event) = rx.try_recv() {
            let lattice_lsp::LspLogPushed {
                server_id,
                workspace: _,
                level,
                source,
                message,
            } = event;
            if source == "show" {
                let echo_level = match level.as_str() {
                    "error" => EchoLevel::Error,
                    "warn" => EchoLevel::Warn,
                    _ => EchoLevel::Info,
                };
                let prefix = server_id
                    .as_deref()
                    .map(|id| format!("[{id}] "))
                    .unwrap_or_default();
                last_show = Some((echo_level, format!("{prefix}{message}")));
            }
            // All other records -- subsystem, per-instance, trace --
            // are owned by the three log majors (B'.3 / B'.4 / B'.5).
        }
        if let Some((level, msg)) = last_show {
            self.set_message(level, msg);
        }
        self.editor.lsp_log_event_rx = Some(rx);
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
        let Some(mut rx) = self.editor.pending_lsp_detach_rx.take() else {
            return;
        };
        // Collect first so the subsequent `lsp_close_buffer`
        // calls (which take `&mut self`) don't conflict with
        // the receiver borrow.
        let mut buffer_ids: Vec<BufferId> = Vec::new();
        while let Ok(event) = rx.try_recv() {
            // `LspBufferDetached.id` is a `DocumentId`; the
            // mode published it via `DocumentId::new(buffer_id
            // .raw())`, so we reverse the conversion here.
            // `BufferId` is `pub u32` here; `DocumentId.raw()` is
            // `u64`. The mode published via `DocumentId::new(
            // ctx.buffer_id().0 as u64)` so the value fits.
            buffer_ids.push(BufferId(event.id.raw() as u32));
        }
        self.editor.pending_lsp_detach_rx = Some(rx);
        for buffer_id in buffer_ids {
            self.lsp_close_buffer(buffer_id);
        }
    }

    pub fn drain_lsp_progress_events(&mut self) {
        let Some(mut rx) = self.editor.lsp_progress_event_rx.take() else {
            return;
        };
        while let Ok(event) = rx.try_recv() {
            let key = (event.server_id.clone(), event.token.clone());
            match event.kind {
                lattice_lsp::LspProgressKind::Begin => {
                    self.editor.lsp_progress.insert(key, event);
                }
                lattice_lsp::LspProgressKind::Report => {
                    // LSP §3.16: report inherits title from
                    // the preceding begin. If the begin entry
                    // was already removed (shouldn't happen
                    // in practice), insert as-is.
                    if let Some(prev) = self.editor.lsp_progress.get(&key) {
                        let title = event.title.clone().or_else(|| prev.title.clone());
                        let merged = lattice_lsp::LspProgressUpdate {
                            server_id: event.server_id,
                            token: event.token,
                            kind: event.kind,
                            title,
                            message: event.message,
                            percentage: event.percentage.or(prev.percentage),
                            cancellable: event.cancellable,
                        };
                        self.editor.lsp_progress.insert(key, merged);
                    } else {
                        self.editor.lsp_progress.insert(key, event);
                    }
                }
                lattice_lsp::LspProgressKind::End => {
                    self.editor.lsp_progress.remove(&key);
                }
            }
        }
        self.editor.lsp_progress_event_rx = Some(rx);
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
        let Some(mut rx) = self.editor.pending_configuration_rx.take() else {
            return;
        };
        let mut requests: Vec<lattice_lsp::InboundConfigurationRequest> = Vec::new();
        while let Ok(req) = rx.try_recv() {
            requests.push(req);
        }
        self.editor.pending_configuration_rx = Some(rx);
        for req in requests {
            let values: Vec<serde_json::Value> = req
                .sections
                .iter()
                .map(|section| self.lookup_lsp_config_section(section))
                .collect();
            let _ = req.response.send(values);
        }
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
    pub fn fan_out_did_change_configuration(&mut self, server_id: &str) {
        let settings = self.lookup_lsp_config_section(server_id);
        let params = lsp_types::DidChangeConfigurationParams { settings };
        let supervisor = self.editor.lsp.clone();
        for (_key, handle) in supervisor.running_actors() {
            if handle.server_id() != server_id {
                continue;
            }
            if let Err(e) = handle.did_change_configuration(params.clone()) {
                let instance = handle.instance();
                self.editor.lsp_logger.log(
                    Some(&instance),
                    lattice_lsp::LogLevel::Warn,
                    lattice_lsp::LogSource::Client,
                    format!("workspace/didChangeConfiguration fan-out failed: {e}"),
                );
            }
        }
    }

    /// Look up a server-supplied `section` path in the cached
    /// TOML tree at `lsp.<section>`. Returns `Value::Null` when
    /// the path is missing or the TOML value can't be converted
    /// to JSON. Empty section ("all") returns the whole `lsp`
    /// sub-tree.
    fn lookup_lsp_config_section(&self, section: &str) -> serde_json::Value {
        let path = if section.is_empty() {
            "lsp".to_string()
        } else {
            format!("lsp.{section}")
        };
        let toml_value = match lattice_config::lookup_dotted_path(&self.editor.lsp_config_tree, &path) {
            Some(v) => v,
            None => return serde_json::Value::Null,
        };
        // toml::Value -> serde_json::Value via the round-trip
        // serialiser. Both crates speak serde, so this is the
        // direct path -- no manual variant matching.
        serde_json::to_value(toml_value).unwrap_or(serde_json::Value::Null)
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
        let Some(mut rx) = self.editor.pending_show_document_rx.take() else {
            return;
        };
        let mut requests: Vec<lattice_lsp::InboundShowDocument> = Vec::new();
        while let Ok(req) = rx.try_recv() {
            requests.push(req);
        }
        self.editor.pending_show_document_rx = Some(rx);
        for req in requests {
            let instance = lattice_lsp::InstanceKey::new(
                std::sync::Arc::clone(&req.server_id),
                std::sync::Arc::clone(&req.workspace),
            );
            let success = self.perform_show_document(
                &instance,
                &req.uri,
                req.external,
                req.take_focus,
                req.selection,
            );
            let _ = req
                .response
                .send(lattice_lsp::ShowDocumentOutcome { success });
        }
    }

    fn perform_show_document(
        &mut self,
        instance: &lattice_lsp::InstanceKey,
        uri: &lattice_lsp::Uri,
        external: bool,
        take_focus: bool,
        selection: Option<lsp_types::Range>,
    ) -> bool {
        let uri_str = uri.as_str().to_string();
        if external {
            return self.open_external_uri(instance, &uri_str);
        }
        // In-editor branch: only `file://` URIs reach here.
        if !uri_str.starts_with("file://") {
            self.editor.lsp_logger.log(
                Some(instance),
                lattice_lsp::LogLevel::Warn,
                lattice_lsp::LogSource::Client,
                format!("showDocument: refusing non-file URI {uri_str:?} without `external`"),
            );
            return false;
        }
        let Some(path) = lattice_lsp::actor::uri_to_path(uri) else {
            self.editor.lsp_logger.log(
                Some(instance),
                lattice_lsp::LogLevel::Warn,
                lattice_lsp::LogSource::Client,
                format!("showDocument: malformed file URI {uri_str:?}"),
            );
            return false;
        };
        // `take_focus` defaults to false in spec; we don't
        // implement preview-without-focus today (every `:e`
        // moves focus). The flag stays honoured by always
        // moving focus when true and accepting that the
        // false case still moves focus -- documented in
        // lsp-features.md.
        let _ = take_focus;
        self.do_edit(Some(path), false);
        if let Some(range) = selection {
            self.move_cursor_to_lsp_position(range.start);
        }
        true
    }

    fn open_external_uri(&mut self, instance: &lattice_lsp::InstanceKey, uri: &str) -> bool {
        // Pick the platform's open command. We don't take an
        // optional `App.external_open_command` config knob
        // yet -- the OS defaults cover the supported
        // platforms.
        #[cfg(target_os = "macos")]
        let cmd = "open";
        #[cfg(target_os = "windows")]
        let cmd = "explorer";
        #[cfg(all(unix, not(target_os = "macos")))]
        let cmd = "xdg-open";
        match std::process::Command::new(cmd).arg(uri).spawn() {
            Ok(_) => {
                self.editor.lsp_logger.log(
                    Some(instance),
                    lattice_lsp::LogLevel::Info,
                    lattice_lsp::LogSource::Client,
                    format!("showDocument(external): {uri}"),
                );
                true
            }
            Err(e) => {
                self.editor.lsp_logger.log(
                    Some(instance),
                    lattice_lsp::LogLevel::Warn,
                    lattice_lsp::LogSource::Client,
                    format!("showDocument(external) failed: {e}"),
                );
                false
            }
        }
    }

    fn move_cursor_to_lsp_position(&mut self, position: lsp_types::Position) {
        // Position arrives in LSP utf-16; convert via the
        // active document's encoding before moving. Best-
        // effort: if the line is out of buffer, leave the
        // cursor where `do_edit` put it.
        let snapshot = self.document.snapshot();
        let byte = crate::app::lsp_position_to_app_byte(
            &snapshot.buffer,
            position.line,
            position.character,
        );
        if snapshot.buffer.line(position.line).is_some() {
            self.editor.cursor = lattice_protocol::Position {
                line: position.line,
                byte,
            };
        }
    }

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
        let Some(mut rx) = self.editor.pending_show_message_request_rx.take() else {
            return;
        };
        let mut requests: Vec<lattice_lsp::InboundShowMessageRequest> = Vec::new();
        while let Ok(req) = rx.try_recv() {
            requests.push(req);
        }
        self.editor.pending_show_message_request_rx = Some(rx);
        let mut last_minibuffer: Option<(EchoLevel, String)> = None;
        for req in requests {
            let labels: Vec<&str> = req.actions.iter().map(|a| a.title.as_str()).collect();
            let labels_joined = labels.join(" / ");
            let echo_level = match req.level {
                lsp_types::MessageType::ERROR => EchoLevel::Error,
                lsp_types::MessageType::WARNING => EchoLevel::Warn,
                _ => EchoLevel::Info,
            };
            let log_level = match req.level {
                lsp_types::MessageType::ERROR => lattice_lsp::LogLevel::Error,
                lsp_types::MessageType::WARNING => lattice_lsp::LogLevel::Warn,
                _ => lattice_lsp::LogLevel::Info,
            };
            let instance = lattice_lsp::InstanceKey::new(
                std::sync::Arc::clone(&req.server_id),
                std::sync::Arc::clone(&req.workspace),
            );
            // Actionless requests: spec-compliant `null` reply
            // (the prompt is purely informational); surface the
            // prompt on the minibuffer + LSP log and move on.
            if req.actions.is_empty() {
                self.editor.lsp_logger.log(
                    Some(&instance),
                    log_level,
                    lattice_lsp::LogSource::LspShowMessage,
                    format!("showMessageRequest: {}", req.message),
                );
                last_minibuffer =
                    Some((echo_level, format!("[{}] {}", req.server_id, req.message)));
                let _ = req
                    .response
                    .send(lattice_lsp::ShowMessageRequestOutcome { selected: None });
                continue;
            }
            // Actionful: register and either open the picker
            // (if none is up) or queue. The log breadcrumb names
            // every request so nothing is lost when multiple
            // arrive together.
            let request_id = self.allocate_smr_request_id();
            self.editor.lsp_logger.log(
                Some(&instance),
                log_level,
                lattice_lsp::LogSource::LspShowMessage,
                format!(
                    "showMessageRequest #{request_id}: {} [actions: {labels_joined}]",
                    req.message
                ),
            );
            last_minibuffer = Some((
                echo_level,
                format!("[{}] {} [{labels_joined}]", req.server_id, req.message),
            ));
            self.editor.lsp_pending_show_message_requests
                .insert(request_id, req);
            if self.editor.picker.is_some() {
                self.editor.lsp_show_message_request_queue.push_back(request_id);
            } else {
                self.open_show_message_request_picker(request_id);
            }
        }
        if let Some((level, text)) = last_minibuffer {
            self.set_message(level, text);
        }
    }

    /// Allocate a fresh `u32` request id for
    /// `lsp_pending_show_message_requests`. Wraps on overflow
    /// and skips any id currently in use -- collision is only
    /// possible if `u32::MAX` actionful requests pile up at
    /// once, which won't happen, but the loop keeps the
    /// invariant honest.
    fn allocate_smr_request_id(&mut self) -> u32 {
        loop {
            let id = self.editor.lsp_next_show_message_request_id;
            self.editor.lsp_next_show_message_request_id =
                self.editor.lsp_next_show_message_request_id.wrapping_add(1);
            if !self.editor.lsp_pending_show_message_requests.contains_key(&id) {
                return id;
            }
        }
    }

    /// Open the `window/showMessageRequest` action picker for
    /// the given pending-slot id. One row per `MessageActionItem`
    /// title; payload is `AcceptShowMessageAction { request_id,
    /// action_index }` so the accept arm can locate both the
    /// inbound slot and the chosen action. The picker title is
    /// the server-prefixed prompt so the user always sees what
    /// they're answering.
    pub(super) fn open_show_message_request_picker(&mut self, request_id: u32) {
        let Some(req) = self.editor.lsp_pending_show_message_requests.get(&request_id) else {
            return;
        };
        let server_id = req.server_id.to_string();
        let title = format!("[{}] {}", server_id, req.message);
        let items: Vec<(
            lattice_completion::RawCandidate,
            lattice_picker::RoutingPayload,
        )> = req
            .actions
            .iter()
            .enumerate()
            .map(|(i, act)| {
                let mut raw = lattice_completion::RawCandidate::plain(
                    act.title.clone(),
                    lattice_completion::CandidateKind::Plain,
                );
                raw.display = format!("{}. {}", i + 1, act.title);
                (
                    raw,
                    lattice_picker::RoutingPayload::AcceptShowMessageAction {
                        request_id,
                        action_index: i as u32,
                    },
                )
            })
            .collect();
        let mut p = lattice_picker::Picker::new(
            &title,
            lattice_picker::PickerSource::LspShowMessageRequest {
                request_id,
                server_id,
            },
            lattice_picker::PickerAction::AcceptShowMessageAction,
        );
        p.set_raw_candidates_with_routing(items);
        self.editor.picker = Some(p);
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
        let Some(req) = self.editor.lsp_pending_show_message_requests.remove(&request_id) else {
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
        while let Some(next_id) = self.editor.lsp_show_message_request_queue.pop_front() {
            if self
                .editor.lsp_pending_show_message_requests
                .contains_key(&next_id)
            {
                self.open_show_message_request_picker(next_id);
                return;
            }
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
        let Some(mut rx) = self.editor.pending_apply_edit_rx.take() else {
            return;
        };
        let mut requests: Vec<lattice_lsp::InboundApplyEdit> = Vec::new();
        while let Ok(req) = rx.try_recv() {
            requests.push(req);
        }
        self.editor.pending_apply_edit_rx = Some(rx);
        for req in requests {
            let outcome =
                self.apply_inbound_workspace_edit(&req.server_id, req.label.as_deref(), req.edit);
            let _ = req.response.send(outcome);
        }
    }

    /// Apply one server-initiated WorkspaceEdit to the editor's
    /// buffers. Returns the `lattice_lsp::ApplyEditOutcome` the
    /// actor's response task ferries back to the server.
    fn apply_inbound_workspace_edit(
        &mut self,
        server_id: &std::sync::Arc<str>,
        label: Option<&str>,
        edit: lsp_types::WorkspaceEdit,
    ) -> lattice_lsp::ApplyEditOutcome {
        let per_file = flatten_workspace_edit(edit);
        if per_file.is_empty() {
            // Spec: when the edit is empty there's nothing to do;
            // reply applied=true with a clarifying note.
            return lattice_lsp::ApplyEditOutcome {
                applied: true,
                failure_reason: Some("empty workspace edit".into()),
            };
        }
        let mut applied_files = 0usize;
        let mut failed_files: Vec<String> = Vec::new();
        let mut total_edits = 0usize;
        for (uri, edits) in per_file {
            let target_path = match lattice_lsp::actor::uri_to_path(&uri) {
                Some(p) => p,
                None => {
                    failed_files.push(format!("{uri:?} (malformed URI)"));
                    continue;
                }
            };
            let edit_count = edits.len();
            if self
                .document
                .path()
                .map(|p| p == target_path)
                .unwrap_or(false)
            {
                if let Err(e) = self.apply_lsp_text_edits(edits) {
                    failed_files.push(format!("{}: {e}", target_path.display()));
                    continue;
                }
                applied_files += 1;
                total_edits += edit_count;
            } else {
                // Cross-file edits: open via `:e` then apply.
                self.do_edit(Some(target_path.clone()), false);
                if matches!(
                    self.editor.last_message.as_ref().map(|m| m.level),
                    Some(EchoLevel::Error)
                ) {
                    failed_files.push(format!("{}: open failed", target_path.display()));
                    continue;
                }
                if let Err(e) = self.apply_lsp_text_edits(edits) {
                    failed_files.push(format!("{}: {e}", target_path.display()));
                    continue;
                }
                applied_files += 1;
                total_edits += edit_count;
            }
        }
        // Echo a status line for the user.
        let label_text = label.map(|l| format!(" `{l}`")).unwrap_or_default();
        let summary = if failed_files.is_empty() {
            format!(
                "{server_id}: applyEdit{label_text} -> {total_edits} edit{} across {applied_files} file{}",
                if total_edits == 1 { "" } else { "s" },
                if applied_files == 1 { "" } else { "s" },
            )
        } else {
            format!(
                "{server_id}: applyEdit{label_text} partial -- {applied_files} ok, {} failed: {}",
                failed_files.len(),
                failed_files.join("; "),
            )
        };
        let echo_level = if failed_files.is_empty() {
            EchoLevel::Info
        } else {
            EchoLevel::Warn
        };
        self.set_message(echo_level, summary.clone());
        lattice_lsp::ApplyEditOutcome {
            applied: applied_files > 0,
            failure_reason: if failed_files.is_empty() {
                None
            } else {
                Some(format!(
                    "{} file{} failed: {}",
                    failed_files.len(),
                    if failed_files.len() == 1 { "" } else { "s" },
                    failed_files.join("; "),
                ))
            },
        }
    }

    /// Single canonical hook for "this buffer was just opened":
    /// register `BufferId → Uri` eagerly (path-bearing only),
    /// then publish `Event::DocumentOpened` on the bus. Both the
    /// initial-document path (`App::new`) and the follow-up
    /// `:e <path>` path (`App::do_edit`) call this helper.
    ///
    /// Idempotent against the supervisor: re-publishing the same
    /// URI is a no-op because `LspSupervisorHandle::open_buffer`
    /// short-circuits already-attached URIs.
    pub(super) fn publish_document_opened_for_active(&mut self) {
        let snap = self.document.snapshot();
        let path_opt = snap.path().map(std::path::Path::to_path_buf);
        let version = snap.text_version;
        let text = snap.buffer.as_string();
        let buffer_id = self.editor.document_buffer_id;
        drop(snap);

        if let Some(ref path) = path_opt {
            let uri = lattice_lsp::actor::uri_from_path(path);
            self.editor.buffer_uris.insert(buffer_id, uri);
        }

        self.editor.event_bus.publish(Event::DocumentOpened {
            id: lattice_protocol::ids::DocumentId::new(buffer_id.0 as u64),
            path: path_opt,
            version,
            text,
        });
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
        let lattice_completion::CandidateData::Extension { kind_id, payload } = &candidate.raw.data
        else {
            return None;
        };
        if *kind_id != LSP_COMPLETION_KIND_ID {
            return None;
        }
        lattice_lsp::completion::decode_meta(payload)
    }

    /// Look up the current URI of a buffer. None for buffers
    /// that have no on-disk path yet (new unsaved scratch
    /// buffers).
    pub fn buffer_uri(&self, id: BufferId) -> Option<&lattice_lsp::Uri> {
        self.editor.buffer_uris.get(&id)
    }

    /// Flush queued didChange events for a buffer immediately.
    /// Used by will-save hooks (4.3) so the server's view is
    /// caught up before pre-save requests fire. Fire-and-forget
    /// against the supervisor mailbox.
    pub fn lsp_flush(&self, buffer_id: BufferId) {
        let Some(uri) = self.editor.buffer_uris.get(&buffer_id).cloned() else {
            return;
        };
        self.editor.lsp.flush(uri);
    }

    /// Detach a buffer from every attached LSP server. Called
    /// from the bdelete path. Sends `didClose` per server +
    /// clears the URI's diagnostics. Fire-and-forget against
    /// the supervisor mailbox.
    pub fn lsp_close_buffer(&mut self, buffer_id: BufferId) {
        let Some(uri) = self.editor.buffer_uris.remove(&buffer_id) else {
            return;
        };
        self.editor.lsp.close_buffer(uri);
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
        // M.6.5: prefer the canonical `lsp-mode.log-level`. Fall
        // back to the legacy `lsp.log-level` with a deprecation
        // echo so existing TOMLs keep working for one minor
        // version.
        let canonical =
            lattice_config::lookup_dotted_path(&self.editor.lsp_config_tree, "lsp-mode.log-level")
                .and_then(|v| v.as_str())
                .map(String::from);
        let legacy = lattice_config::lookup_dotted_path(&self.editor.lsp_config_tree, "lsp.log-level")
            .and_then(|v| v.as_str())
            .map(String::from);
        let (key, level) = match (canonical, legacy) {
            (Some(v), _) => ("lsp-mode.log-level", v),
            (None, Some(v)) => {
                self.set_message(
                    EchoLevel::Warn,
                    "config: `lsp.log-level` is deprecated; rename to `lsp-mode.log-level` \
                     (the `lsp.*` namespace is reserved for `workspace/configuration` \
                     server subtables)"
                        .to_string(),
                );
                ("lsp.log-level", v)
            }
            (None, None) => return,
        };
        match lattice_lsp::LogLevel::parse(&level) {
            Some(parsed) => self.editor.lsp_logger.set_default_level(parsed),
            None => self.set_message(
                EchoLevel::Warn,
                format!(
                    "config: {key}: unknown level {level:?}; expected error/warn/info/debug/trace"
                ),
            ),
        }
    }

    /// Apply a `Vec<TextEdit>` (LSP utf-16 ranges) to the active
    /// buffer as one undo unit. TextEdits are sorted in reverse
    /// by start position so each application doesn't shift the
    /// positions of the later ones (LSP convention: edits are
    /// non-overlapping and reference the original document).
    pub(super) fn apply_lsp_text_edits(
        &mut self,
        mut edits: Vec<lsp_types::TextEdit>,
    ) -> Result<(), String> {
        edits.sort_by(|a, b| {
            b.range
                .start
                .line
                .cmp(&a.range.start.line)
                .then_with(|| b.range.start.character.cmp(&a.range.start.character))
        });
        let snap = self.document.snapshot();
        let mut lattice_edits: Vec<Edit> = Vec::with_capacity(edits.len());
        for te in edits {
            let start_line = te.range.start.line;
            let end_line = te.range.end.line;
            let start_byte =
                lsp_position_to_app_byte(&snap.buffer, start_line, te.range.start.character);
            let end_byte = lsp_position_to_app_byte(&snap.buffer, end_line, te.range.end.character);
            let range = lattice_protocol::position::Range::new(
                lattice_protocol::position::Position::new(start_line, start_byte),
                lattice_protocol::position::Position::new(end_line, end_byte),
            );
            lattice_edits.push(Edit::replace(range, te.new_text));
        }
        self.apply_edit_batch_blocking(lattice_edits)
            .map(|_| ())
            .map_err(|e| format!("{e:?}"))
    }

    /// Union of onTypeFormatting trigger characters across LSP
    /// servers attached to the active document.
    pub(super) fn on_type_formatting_trigger_chars(&self) -> Vec<char> {
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id) else {
            return Vec::new();
        };
        let handles = self.editor.lsp.servers_for(uri);
        let mut chars: Vec<char> = Vec::new();
        for h in handles {
            for c in h.capabilities().on_type_formatting_trigger_chars() {
                if !chars.contains(&c) {
                    chars.push(c);
                }
            }
        }
        chars
    }

    /// Union of signature-help trigger characters across every
    /// LSP server attached to the active document. Empty when
    /// no server advertises the provider.
    pub(super) fn signature_help_trigger_chars(&self) -> Vec<char> {
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id) else {
            return Vec::new();
        };
        let handles = self.editor.lsp.servers_for(uri);
        let mut chars: Vec<char> = Vec::new();
        for h in handles {
            for c in h.capabilities().signature_help_trigger_chars() {
                if !chars.contains(&c) {
                    chars.push(c);
                }
            }
        }
        chars
    }

    /// Fire `textDocument/onTypeFormatting` to the highest-
    /// priority server advertising the trigger; apply the returned
    /// edits as one undo unit.
    pub(super) fn do_lsp_on_type_formatting_request(&mut self, trigger: char) {
        // M.6.2: lsp-format-mode gate. Insert-mode trigger; silent
        // (same shape as completion -- typed character that
        // doesn't fire isn't a moment to surface mode state).
        if !self.lsp_format_mode_enabled_for(self.editor.document_buffer_id) {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        let snapshot = self.document.snapshot();
        let pos = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => return,
        };
        let lsp = self.editor.lsp.clone();
        let trigger_str = trigger.to_string();
        let options = lsp_types::FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            properties: Default::default(),
            trim_trailing_whitespace: Some(true),
            insert_final_newline: Some(true),
            trim_final_newlines: Some(true),
        };
        // OnTypeFormatting fires per-character; apply the result
        // via the same async drain path the format request uses.
        // Reuse `pending_format_*` since onType and `:format` are
        // mutually exclusive in time.
        if let Some(token) = self.editor.pending_format_token.take() {
            token.cancel();
        }
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<FormatOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_format_rx = Some(rx);
        self.editor.pending_format_token = Some(token.clone());
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            let chosen = handles
                .into_iter()
                .find(|h| h.capabilities().supports_on_type_formatting());
            let Some(handle) = chosen else {
                let _ = tx.send(FormatOutcome::NoProvider { is_range: false });
                return;
            };
            let params = lsp_types::DocumentOnTypeFormattingParams {
                text_document_position: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri },
                    position: pos,
                },
                ch: trigger_str,
                options,
            };
            let edits = handle
                .on_type_formatting(params, token)
                .await
                .ok()
                .flatten()
                .unwrap_or_default();
            let _ = tx.send(FormatOutcome::Edits(edits));
        });
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
        if let Some(token) = self.editor.pending_rename_token.take() {
            token.cancel();
        }
        // M.6.2: lsp-rename-mode gate (umbrella check inside).
        if !self.check_lsp_sub_mode_gate(
            lattice_lsp::modes::LspRenameMode::mode_id(),
            "lsp-rename-mode",
        ) {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server attached to current buffer".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let lsp_position = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => {
                self.set_message(EchoLevel::Error, "rename: cursor out of buffer");
                return;
            }
        };
        let new_name = new_name.to_string();
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<RenameOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_rename_rx = Some(rx);
        self.editor.pending_rename_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            let chosen = handles
                .into_iter()
                .find(|h| h.capabilities().supports_rename());
            let Some(handle) = chosen else {
                let _ = tx.send(RenameOutcome::NoProvider);
                return;
            };
            // Optional prepareRename. If the server advertises
            // prepare and refuses, surface the reason; if it
            // accepts, also use the placeholder when the user
            // didn't supply a name.
            let mut effective_name = new_name.clone();
            if handle.capabilities().supports_prepare_rename() {
                if token.is_cancelled() {
                    return;
                }
                let pos = lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    position: lsp_position,
                };
                match handle.prepare_rename(pos, token.clone()).await {
                    Ok(Some(prep)) => {
                        if effective_name.is_empty() {
                            effective_name = prepare_rename_placeholder(&prep).unwrap_or_default();
                        }
                    }
                    Ok(None) => {
                        let _ = tx.send(RenameOutcome::NotRenameable {
                            reason: "server refused rename at this position".into(),
                        });
                        return;
                    }
                    Err(_) => {
                        // Fall through to rename.
                    }
                }
            }
            if effective_name.is_empty() {
                let _ = tx.send(RenameOutcome::NotRenameable {
                    reason: "rename requires a new name".into(),
                });
                return;
            }
            let params = lsp_types::RenameParams {
                text_document_position: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    position: lsp_position,
                },
                new_name: effective_name.clone(),
                work_done_progress_params: Default::default(),
            };
            match handle.rename(params, token.clone()).await {
                Ok(Some(workspace_edit)) => {
                    let per_file = flatten_workspace_edit(workspace_edit);
                    if per_file.is_empty() {
                        let _ = tx.send(RenameOutcome::Empty);
                    } else {
                        let _ = tx.send(RenameOutcome::Edits {
                            per_file,
                            new_name: effective_name,
                        });
                    }
                }
                _ => {
                    let _ = tx.send(RenameOutcome::Empty);
                }
            }
        });
    }

    /// Drain queued `:rename` responses; apply the WorkspaceEdit.
    /// v1: per-file edits land as one undo unit in each affected
    /// buffer.
    pub fn drain_pending_rename(&mut self) {
        let Some(mut rx) = self.editor.pending_rename_rx.take() else {
            return;
        };
        let mut latest: Option<RenameOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_rename_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        self.editor.pending_rename_token = None;
        match outcome {
            RenameOutcome::NoProvider => {
                self.set_message(EchoLevel::Info, "no server with renameProvider")
            }
            RenameOutcome::NotRenameable { reason } => {
                self.set_message(EchoLevel::Error, format!("rename: {reason}"))
            }
            RenameOutcome::Empty => self.set_message(EchoLevel::Info, "rename: no changes"),
            RenameOutcome::Edits { per_file, new_name } => {
                self.apply_rename_workspace_edit(per_file, new_name);
            }
        }
    }

    /// Apply a per-file WorkspaceEdit returned by `:rename`. The
    /// active buffer's edits land directly via apply_lsp_text_edits;
    /// cross-file edits open the file via `:e` and apply.
    pub(super) fn apply_rename_workspace_edit(
        &mut self,
        per_file: Vec<(lsp_types::Uri, Vec<lsp_types::TextEdit>)>,
        new_name: String,
    ) {
        let mut applied_files = 0usize;
        let mut total_edits = 0usize;
        let mut deferred_files: Vec<String> = Vec::new();
        for (uri, edits) in per_file {
            let target_path = match lattice_lsp::actor::uri_to_path(&uri) {
                Some(p) => p,
                None => continue,
            };
            let edit_count = edits.len();
            if self
                .document
                .path()
                .map(|p| p == target_path)
                .unwrap_or(false)
            {
                if let Err(e) = self.apply_lsp_text_edits(edits) {
                    self.set_message(
                        EchoLevel::Error,
                        format!("rename: apply failed for active buffer: {e}"),
                    );
                    return;
                }
                applied_files += 1;
                total_edits += edit_count;
            } else {
                // Cross-file edits: open the file via :e and apply.
                self.do_edit(Some(target_path.clone()), false);
                if matches!(
                    self.editor.last_message.as_ref().map(|m| m.level),
                    Some(EchoLevel::Error)
                ) {
                    deferred_files.push(target_path.display().to_string());
                    continue;
                }
                if let Err(e) = self.apply_lsp_text_edits(edits) {
                    self.set_message(
                        EchoLevel::Error,
                        format!("rename: apply failed for {}: {e}", target_path.display()),
                    );
                    return;
                }
                applied_files += 1;
                total_edits += edit_count;
            }
        }
        let mut summary = format!(
            "rename -> {new_name}: {total_edits} edit{} across {applied_files} file{}",
            if total_edits == 1 { "" } else { "s" },
            if applied_files == 1 { "" } else { "s" },
        );
        if !deferred_files.is_empty() {
            summary.push_str(&format!(
                " (skipped {}: open the file then re-run)",
                deferred_files.join(", ")
            ));
        }
        self.set_message(EchoLevel::Info, summary);
    }

    /// Apply a chosen code-action. The action may carry an
    /// inline `WorkspaceEdit`, a `Command`, both, or neither
    /// (resolve required). The `handle` is the server that
    /// produced the action -- resolve / executeCommand routes
    /// back to it.
    pub(super) fn apply_lsp_code_action(
        &mut self,
        row: CodeActionRow,
        handle: Option<lattice_lsp::ServerHandle>,
    ) {
        let action = match row.action {
            // Bare command -- skip resolve, route through executeCommand.
            lsp_types::CodeActionOrCommand::Command(cmd) => {
                self.execute_lsp_command(handle, cmd);
                return;
            }
            lsp_types::CodeActionOrCommand::CodeAction(ca) => ca,
        };
        // Resolve when the action arrived without `edit` AND a
        // handle is available.
        let needs_resolve = action.edit.is_none() && action.command.is_none();
        if needs_resolve {
            let Some(handle) = handle else {
                self.set_message(
                    EchoLevel::Error,
                    "code-action: cannot resolve (no server handle)".to_string(),
                );
                return;
            };
            self.spawn_code_action_resolve_apply(handle, action);
            return;
        }
        self.apply_resolved_code_action(handle, action);
    }

    /// Async path for codeAction/resolve. Spawns a task that
    /// resolves the action then queues the resolved version
    /// back to the App for apply via the same channel the
    /// initial code-action request used.
    fn spawn_code_action_resolve_apply(
        &mut self,
        handle: lattice_lsp::ServerHandle,
        action: lsp_types::CodeAction,
    ) {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<CodeActionOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        // Stash the original handle so the post-resolve dispatch
        // can route back to the same server.
        self.editor.pending_code_action_rx = Some(rx);
        self.editor.pending_code_action_token = Some(token.clone());
        self.editor.pending_code_action_handle = Some(handle.clone());
        crate::runtime::spawn_on_lsp_runtime(async move {
            if token.is_cancelled() {
                return;
            }
            let resolved = match handle.code_action_resolve(action.clone(), token).await {
                Ok(r) => r,
                Err(_) => action,
            };
            let _ = tx.send(CodeActionOutcome::Resolved(resolved));
        });
    }

    /// Apply a fully-resolved code-action: WorkspaceEdit (when
    /// present) lands as one undo unit per affected buffer;
    /// `Command` (when present) routes through
    /// `workspace/executeCommand`. Both can fire for the same
    /// action -- LSP spec allows it.
    fn apply_resolved_code_action(
        &mut self,
        handle: Option<lattice_lsp::ServerHandle>,
        action: lsp_types::CodeAction,
    ) {
        if let Some(edit) = action.edit {
            let per_file = flatten_workspace_edit(edit);
            if !per_file.is_empty() {
                self.apply_rename_workspace_edit(per_file, action.title.clone());
            }
        }
        if let Some(cmd) = action.command {
            self.execute_lsp_command(handle, cmd);
        }
    }

    /// Fire `workspace/executeCommand` for a code-action's
    /// command payload. Server response is opaque.
    pub(super) fn execute_lsp_command(
        &mut self,
        handle: Option<lattice_lsp::ServerHandle>,
        cmd: lsp_types::Command,
    ) {
        let Some(handle) = handle else {
            self.set_message(
                EchoLevel::Error,
                format!("execute_command: no server handle for `{}`", cmd.command),
            );
            return;
        };
        if !handle.capabilities().supports_execute_command() {
            self.set_message(
                EchoLevel::Error,
                format!(
                    "execute_command: server doesn't advertise executeCommandProvider for `{}`",
                    cmd.command
                ),
            );
            return;
        }
        let params = lsp_types::ExecuteCommandParams {
            command: cmd.command.clone(),
            arguments: cmd.arguments.unwrap_or_default(),
            work_done_progress_params: Default::default(),
        };
        let title = cmd.title.clone();
        let token = lattice_protocol::CancellationToken::new();
        crate::runtime::spawn_on_lsp_runtime(async move {
            // Fire-and-forget; the response is rarely useful
            // beyond error logging.
            let _ = handle.execute_command(params, token).await;
        });
        self.set_message(EchoLevel::Info, format!("dispatched: {title}"));
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
                self.editor.cursor = applied.inserted_range.end;
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
        if let Some(token) = self.editor.pending_code_action_token.take() {
            token.cancel();
        }
        // Browse-style; not a tag-intent drill-down.
        self.editor.pending_tag_origin = None;
        // M.6.2: lsp-code-action-mode gate (after cancel-stale-work).
        if !self.check_lsp_sub_mode_gate(
            lattice_lsp::modes::LspCodeActionMode::mode_id(),
            "lsp-code-action-mode",
        ) {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server attached to current buffer".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let range = self.code_action_range(&snapshot.buffer);
        let context = lsp_types::CodeActionContext {
            diagnostics: self.diagnostics_for_range(&uri, &range),
            only: None,
            trigger_kind: Some(lsp_types::CodeActionTriggerKind::INVOKED),
        };
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<CodeActionOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_code_action_rx = Some(rx);
        self.editor.pending_code_action_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        let stash = std::sync::Arc::new(std::sync::Mutex::new(None::<lattice_lsp::ServerHandle>));
        let stash_for_task = stash.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            let chosen = handles
                .into_iter()
                .find(|h| h.capabilities().supports_code_action());
            let Some(handle) = chosen else {
                let _ = tx.send(CodeActionOutcome::NoProvider);
                return;
            };
            *stash_for_task.lock().unwrap() = Some(handle.clone());
            let params = lsp_types::CodeActionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                range,
                context,
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            };
            if let Ok(Some(resp)) = handle.code_action(params, token.clone()).await {
                let rows: Vec<CodeActionRow> = resp
                    .into_iter()
                    .map(|act| {
                        let (title, kind_glyph) = match &act {
                            lsp_types::CodeActionOrCommand::Command(c) => {
                                (c.title.clone(), code_action_kind_glyph(None))
                            }
                            lsp_types::CodeActionOrCommand::CodeAction(ca) => {
                                (ca.title.clone(), code_action_kind_glyph(ca.kind.as_ref()))
                            }
                        };
                        CodeActionRow {
                            title,
                            kind_glyph,
                            action: act,
                        }
                    })
                    .collect();
                let _ = tx.send(CodeActionOutcome::Items(rows));
            } else {
                let _ = tx.send(CodeActionOutcome::Items(Vec::new()));
            }
        });
        let _ = stash;
    }

    /// LSP-shape range for the current code-action request.
    /// Visual selection when active; point range at cursor otherwise.
    fn code_action_range(&self, buffer: &lattice_core::Buffer) -> lsp_types::Range {
        if let lattice_grammar::ModalState::Visual(_) = self.editor.modal {
            let anchor = self.editor.visual_anchor.unwrap_or(self.editor.cursor);
            let head = self.editor.cursor;
            let (start_pos, end_pos) = if (anchor.line, anchor.byte) <= (head.line, head.byte) {
                (anchor, head)
            } else {
                (head, anchor)
            };
            let start = app_to_lsp_position(buffer, start_pos).unwrap_or(lsp_types::Position {
                line: 0,
                character: 0,
            });
            let end = app_to_lsp_position(buffer, end_pos).unwrap_or(start);
            lsp_types::Range { start, end }
        } else {
            let p = app_to_lsp_position(buffer, self.editor.cursor).unwrap_or(lsp_types::Position {
                line: 0,
                character: 0,
            });
            lsp_types::Range { start: p, end: p }
        }
    }

    /// Diagnostics overlapping `range` in `uri`, converted to
    /// the LSP shape codeAction servers expect in
    /// `CodeActionContext`. Servers use these to emit quick-fix
    /// actions tied to specific diagnostics.
    fn diagnostics_for_range(
        &self,
        uri: &lattice_lsp::Uri,
        range: &lsp_types::Range,
    ) -> Vec<lattice_lsp::Diagnostic> {
        self.editor.lsp_diagnostics
            .diagnostics_for(uri)
            .into_iter()
            .filter(|d| {
                d.range.end.line > range.start.line
                    || (d.range.end.line == range.start.line
                        && d.range.end.character > range.start.character)
            })
            .filter(|d| {
                d.range.start.line < range.end.line
                    || (d.range.start.line == range.end.line
                        && d.range.start.character <= range.end.character)
            })
            .collect()
    }

    /// Drain queued code-action responses. Items pin to App + open
    /// a picker. Resolve responses (single-row outcomes seeded with
    /// the resolved action) apply directly when the original handle
    /// is still pinned.
    pub fn drain_pending_code_actions(&mut self) {
        let Some(mut rx) = self.editor.pending_code_action_rx.take() else {
            return;
        };
        let mut latest: Option<CodeActionOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_code_action_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        self.editor.pending_code_action_token = None;
        match outcome {
            CodeActionOutcome::NoProvider => {
                self.set_message(
                    EchoLevel::Info,
                    "no server with codeActionProvider".to_string(),
                );
            }
            CodeActionOutcome::Resolved(action) => {
                let handle = self.editor.pending_code_action_handle.take();
                self.apply_resolved_code_action(handle, action);
            }
            CodeActionOutcome::Items(items) => {
                if items.is_empty() {
                    self.set_message(EchoLevel::Info, "no code actions".to_string());
                    return;
                }
                let total = items.len();
                let pairs: Vec<(
                    lattice_completion::RawCandidate,
                    lattice_picker::RoutingPayload,
                )> = items
                    .iter()
                    .enumerate()
                    .map(|(i, item)| {
                        let mut c = lattice_completion::RawCandidate::plain(
                            item.title.clone(),
                            lattice_completion::CandidateKind::Plain,
                        );
                        c.display = format!("{} {}", item.kind_glyph, item.title);
                        (
                            c,
                            lattice_picker::RoutingPayload::LspCodeAction { index: i as u32 },
                        )
                    })
                    .collect();
                let handle = self.first_code_action_handle();
                self.editor.pending_code_action_items = Some(items);
                self.editor.pending_code_action_handle = handle;
                let mut p = lattice_picker::Picker::new(
                    format!("code-actions ({total})"),
                    lattice_picker::PickerSource::LspLocations,
                    lattice_picker::PickerAction::AcceptLspCodeAction,
                );
                p.set_raw_candidates_with_routing(pairs);
                self.editor.picker = Some(p);
            }
        }
    }

    /// Pick the first attached server that advertises
    /// `codeActionProvider` -- mirrors the choice the spawn task
    /// made when firing the original request.
    fn first_code_action_handle(&self) -> Option<lattice_lsp::ServerHandle> {
        let uri = self.editor.buffer_uris.get(&self.editor.document_buffer_id)?;
        self.editor.lsp
            .servers_for(uri)
            .into_iter()
            .find(|h| h.capabilities().supports_code_action())
    }

    /// `:complete` (Phase 4.2.g). Fires
    /// `textDocument/completion` at the cursor; the merged item
    /// list opens as a vertico picker. Multi-server union;
    /// dedup by `(label, kind)`.
    pub(super) fn do_lsp_completion_request(&mut self) {
        if let Some(token) = self.editor.pending_completion_token.take() {
            token.cancel();
        }
        // Browse-style; not a tag-intent drill-down.
        self.editor.pending_tag_origin = None;
        // M.6.2: lsp-completion-mode gate (after cancel-stale-work).
        if !self.check_lsp_sub_mode_gate(
            lattice_lsp::modes::LspCompletionMode::mode_id(),
            "lsp-completion-mode",
        ) {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server attached to current buffer".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let lsp_position = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => return,
        };
        // Compute the prefix replace range: walk back from the
        // cursor over word characters. The server may override
        // via `text_edit` per-item; this is the fallback.
        let line_text = snapshot.buffer.line(self.editor.cursor.line).unwrap_or_default();
        let cursor_byte = self.editor.cursor.byte as usize;
        let mut start = cursor_byte;
        let bytes = line_text.as_bytes();
        while start > 0 && start <= bytes.len() && is_word_char_byte(bytes[start - 1]) {
            start -= 1;
        }
        let prefix_start = start as u32;
        let cursor_line = self.editor.cursor.line;
        let cursor_col = self.editor.cursor.byte;
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<CompletionOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_completion_rx = Some(rx);
        self.editor.pending_completion_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            if handles.is_empty() {
                let _ = tx.send(CompletionOutcome::NoServers);
                return;
            }
            let mut all: Vec<CompletionItemRow> = Vec::new();
            for handle in handles {
                if token.is_cancelled() {
                    return;
                }
                if !handle.capabilities().supports_completion() {
                    continue;
                }
                let params = lsp_types::CompletionParams {
                    text_document_position: lsp_types::TextDocumentPositionParams {
                        text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                        position: lsp_position,
                    },
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                    context: None,
                };
                if let Ok(Some(resp)) = handle.completion(params, token.clone()).await {
                    let items = match resp {
                        lsp_types::CompletionResponse::Array(items) => items,
                        lsp_types::CompletionResponse::List(list) => list.items,
                    };
                    for ci in items {
                        let label = ci.label;
                        let kind_glyph = completion_kind_glyph(ci.kind);
                        let detail = ci.detail.clone();
                        let insert_text = ci.insert_text.clone().unwrap_or_else(|| label.clone());
                        all.push(CompletionItemRow {
                            label,
                            kind_glyph,
                            detail,
                            insert_text,
                            replace_range: (prefix_start, cursor_col),
                            line: cursor_line,
                        });
                    }
                }
            }
            // Dedup by (label, kind glyph) -- avoid two servers
            // emitting the same name twice.
            all.sort_by(|a, b| {
                a.label
                    .cmp(&b.label)
                    .then_with(|| a.kind_glyph.cmp(b.kind_glyph))
            });
            all.dedup_by(|a, b| a.label == b.label && a.kind_glyph == b.kind_glyph);
            let _ = tx.send(CompletionOutcome::Items(all));
        });
    }

    /// Drain queued LSP completion responses and open a picker.
    /// `NoServers` echoes; empty list echoes.
    pub fn drain_pending_completion(&mut self) {
        let Some(mut rx) = self.editor.pending_completion_rx.take() else {
            return;
        };
        let mut latest: Option<CompletionOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_completion_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        self.editor.pending_completion_token = None;
        match outcome {
            CompletionOutcome::NoServers => {
                self.set_message(EchoLevel::Info, "no LSP server attached".to_string());
            }
            CompletionOutcome::Items(items) => {
                if items.is_empty() {
                    self.set_message(EchoLevel::Info, "no completions".to_string());
                    return;
                }
                let total = items.len();
                let pairs: Vec<(
                    lattice_completion::RawCandidate,
                    lattice_picker::RoutingPayload,
                )> = items
                    .iter()
                    .enumerate()
                    .map(|(i, item)| {
                        let mut c = lattice_completion::RawCandidate::plain(
                            item.label.clone(),
                            lattice_completion::CandidateKind::Plain,
                        );
                        c.display = match &item.detail {
                            Some(d) => format!("{} {}  {d}", item.kind_glyph, item.label),
                            None => format!("{} {}", item.kind_glyph, item.label),
                        };
                        (
                            c,
                            lattice_picker::RoutingPayload::LspCompletion { index: i as u32 },
                        )
                    })
                    .collect();
                self.editor.pending_completion_items = Some(items);
                let mut p = lattice_picker::Picker::new(
                    format!("complete ({total})"),
                    lattice_picker::PickerSource::LspLocations,
                    lattice_picker::PickerAction::AcceptLspCompletion,
                );
                p.set_raw_candidates_with_routing(pairs);
                self.editor.picker = Some(p);
            }
        }
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
        if let Some(token) = self.editor.pending_format_token.take() {
            token.cancel();
        }
        // M.6.2: lsp-format-mode gate (after cancel-stale-work).
        if !self.check_lsp_sub_mode_gate(
            lattice_lsp::modes::LspFormatMode::mode_id(),
            "lsp-format-mode",
        ) {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server attached to current buffer".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let last_line = last_addressable_line(&snapshot.buffer);
        // Range resolution.
        let range_lines: Option<(u32, u32)> = if is_range {
            // Use the active Visual selection if any, else the whole buffer.
            if let ModalState::Visual(_) = self.editor.modal {
                let anchor = self.editor.visual_anchor.unwrap_or(self.editor.cursor);
                let head = self.editor.cursor;
                let (s, e): (u32, u32) = if anchor.line <= head.line {
                    (anchor.line, head.line)
                } else {
                    (head.line, anchor.line)
                };
                Some((s, e.min(last_line)))
            } else {
                Some((0u32, last_line))
            }
        } else {
            None
        };
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<FormatOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_format_rx = Some(rx);
        self.editor.pending_format_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        // Compute the LSP range parameters when needed.
        let lsp_range = range_lines.map(|(s, e)| {
            let end_line_text_len = line_byte_len(&snapshot.buffer, e);
            let line_text = snapshot.buffer.line(e).unwrap_or_default();
            let end_char =
                lattice_lsp::position::utf8_byte_to_utf16_column(&line_text, end_line_text_len);
            lsp_types::Range {
                start: lsp_types::Position {
                    line: s,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: e,
                    character: end_char,
                },
            }
        });
        // Conservative formatting options.
        let options = lsp_types::FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            properties: Default::default(),
            trim_trailing_whitespace: Some(true),
            insert_final_newline: Some(true),
            trim_final_newlines: Some(true),
        };
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            // Pick the first server advertising the right provider.
            let chosen: Option<lattice_lsp::ServerHandle> = handles.into_iter().find(|h| {
                let caps = h.capabilities();
                if lsp_range.is_some() {
                    caps.supports_range_formatting()
                } else {
                    caps.supports_formatting()
                }
            });
            let Some(handle) = chosen else {
                let _ = tx.send(FormatOutcome::NoProvider {
                    is_range: lsp_range.is_some(),
                });
                return;
            };
            let edits: Option<Vec<lsp_types::TextEdit>> = if let Some(range) = lsp_range {
                let params = lsp_types::DocumentRangeFormattingParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    range,
                    options: options.clone(),
                    work_done_progress_params: Default::default(),
                };
                handle
                    .range_formatting(params, token.clone())
                    .await
                    .ok()
                    .flatten()
            } else {
                let params = lsp_types::DocumentFormattingParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    options,
                    work_done_progress_params: Default::default(),
                };
                handle
                    .formatting(params, token.clone())
                    .await
                    .ok()
                    .flatten()
            };
            let edits = edits.unwrap_or_default();
            let _ = tx.send(FormatOutcome::Edits(edits));
        });
    }

    /// Drain the format response channel and apply the returned
    /// edits as one undo unit. Echoes when the server returned no
    /// edits ("already formatted") or no provider was available.
    pub fn drain_pending_format(&mut self) {
        let Some(mut rx) = self.editor.pending_format_rx.take() else {
            return;
        };
        let mut latest: Option<FormatOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_format_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        self.editor.pending_format_token = None;
        match outcome {
            FormatOutcome::NoProvider { is_range } => {
                let kind = if is_range { "range " } else { "" };
                self.set_message(
                    EchoLevel::Info,
                    format!("no server with {kind}formatting provider"),
                );
            }
            FormatOutcome::Edits(edits) => {
                if edits.is_empty() {
                    self.set_message(
                        EchoLevel::Info,
                        "format: no changes (already formatted)".to_string(),
                    );
                    return;
                }
                let n = edits.len();
                match self.apply_lsp_text_edits(edits) {
                    Ok(()) => self.set_message(
                        EchoLevel::Info,
                        format!("format: applied {n} edit{}", if n == 1 { "" } else { "s" }),
                    ),
                    Err(e) => {
                        self.set_message(EchoLevel::Error, format!("format: apply failed: {e}"))
                    }
                }
            }
        }
    }

    /// `:lsp-symbols` (Phase 4.2.e). Send
    /// `textDocument/documentSymbol` to every attached server;
    /// flatten the hierarchy + merge across servers; drain on
    /// the next frame opens a picker.
    pub(super) fn do_lsp_document_symbol_request(&mut self) {
        if let Some(token) = self.editor.pending_symbols_token.take() {
            token.cancel();
        }
        // Outline browse; not a tag-intent drill-down.
        self.editor.pending_tag_origin = None;
        // M.6.2: lsp-symbols-mode gate (after cancel-stale-work).
        if !self.check_lsp_sub_mode_gate(
            lattice_lsp::modes::LspSymbolsMode::mode_id(),
            "lsp-symbols-mode",
        ) {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server attached to current buffer".to_string(),
            );
            return;
        };
        let path = match lattice_lsp::actor::uri_to_path(&uri) {
            Some(p) => p,
            None => {
                self.set_message(
                    EchoLevel::Error,
                    "documentSymbol: buffer URI is not a file".to_string(),
                );
                return;
            }
        };
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<SymbolsOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_symbols_rx = Some(rx);
        self.editor.pending_symbols_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            if handles.is_empty() {
                let _ = tx.send(SymbolsOutcome::NoServers);
                return;
            }
            let mut all: Vec<SymbolRow> = Vec::new();
            for handle in handles {
                if token.is_cancelled() {
                    return;
                }
                let params = lsp_types::DocumentSymbolParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                if let Ok(Some(resp)) = handle.document_symbol(params, token.clone()).await {
                    flatten_document_symbol_response(resp, &path, &mut all);
                }
            }
            // Dedup by (path, line, col, name).
            all.sort_by(|a, b| {
                a.path
                    .cmp(&b.path)
                    .then_with(|| a.line.cmp(&b.line))
                    .then_with(|| a.col.cmp(&b.col))
                    .then_with(|| a.name.cmp(&b.name))
            });
            all.dedup_by(|a, b| {
                a.path == b.path && a.line == b.line && a.col == b.col && a.name == b.name
            });
            let title = format!("symbols ({})", all.len());
            let _ = tx.send(SymbolsOutcome::Found { title, rows: all });
        });
    }

    /// `:lsp-workspace-symbol [query]` (Phase 4.2.f).
    pub(super) fn do_lsp_workspace_symbol_request(&mut self, query: &str) {
        if let Some(token) = self.editor.pending_symbols_token.take() {
            token.cancel();
        }
        // Workspace search browse; not a tag-intent drill-down.
        self.editor.pending_tag_origin = None;
        // M.6.2: lsp-symbols-mode gate (after cancel-stale-work).
        if !self.check_lsp_sub_mode_gate(
            lattice_lsp::modes::LspSymbolsMode::mode_id(),
            "lsp-symbols-mode",
        ) {
            return;
        }
        // Workspace symbol is workspace-scoped, so we fan out
        // over EVERY server the supervisor has running -- not
        // just servers attached to the current buffer.
        let lsp = self.editor.lsp.clone();
        let query = query.to_string();
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<SymbolsOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_symbols_rx = Some(rx);
        self.editor.pending_symbols_token = Some(token.clone());
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = lsp.all_running_handles();
            if handles.is_empty() {
                let _ = tx.send(SymbolsOutcome::NoServers);
                return;
            }
            let mut all: Vec<SymbolRow> = Vec::new();
            for handle in handles {
                if token.is_cancelled() {
                    return;
                }
                let params = lsp_types::WorkspaceSymbolParams {
                    query: query.clone(),
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                let Ok(Some(resp)) = handle.workspace_symbol(params, token.clone()).await else {
                    continue;
                };
                match resp {
                    // Legacy `Vec<SymbolInformation>` shape.
                    lsp_types::WorkspaceSymbolResponse::Flat(syms) => {
                        for sym in syms {
                            if let Some(row) = symbol_information_to_row(&sym) {
                                all.push(row);
                            }
                        }
                    }
                    // Modern `Vec<WorkspaceSymbol>` shape (LSP 3.17+).
                    lsp_types::WorkspaceSymbolResponse::Nested(syms) => {
                        for sym in syms {
                            if let Some(row) = workspace_symbol_to_row(&handle, sym, &token).await {
                                all.push(row);
                            }
                        }
                    }
                }
            }
            all.sort_by(|a, b| {
                a.path
                    .cmp(&b.path)
                    .then_with(|| a.line.cmp(&b.line))
                    .then_with(|| a.col.cmp(&b.col))
                    .then_with(|| a.name.cmp(&b.name))
            });
            all.dedup_by(|a, b| {
                a.path == b.path && a.line == b.line && a.col == b.col && a.name == b.name
            });
            let title = if query.is_empty() {
                format!("workspace-symbols ({})", all.len())
            } else {
                format!("workspace-symbols {query:?} ({})", all.len())
            };
            let _ = tx.send(SymbolsOutcome::Found { title, rows: all });
        });
    }

    /// Drain queued document-symbol / workspace-symbol responses
    /// and open the picker.
    pub fn drain_pending_symbols(&mut self) {
        let Some(mut rx) = self.editor.pending_symbols_rx.take() else {
            return;
        };
        let mut latest: Option<SymbolsOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_symbols_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        self.editor.pending_symbols_token = None;
        match outcome {
            SymbolsOutcome::NoServers => {
                self.set_message(EchoLevel::Info, "no LSP server attached".to_string());
            }
            SymbolsOutcome::Found { title, rows } => {
                if rows.is_empty() {
                    self.set_message(EchoLevel::Info, "no symbols".to_string());
                    return;
                }
                let picker_rows: Vec<lattice_picker::LspLocationRow> = rows
                    .into_iter()
                    .map(|r| {
                        let indent = "  ".repeat(r.depth as usize);
                        let preview = if let Some(c) = r.container {
                            format!("{indent}{} {}  ({c})", r.kind_glyph, r.name)
                        } else {
                            format!("{indent}{} {}", r.kind_glyph, r.name)
                        };
                        lattice_picker::LspLocationRow {
                            path: r.path,
                            line: r.line,
                            col: r.col,
                            preview,
                            marginalia: String::new(),
                        }
                    })
                    .collect();
                let mut p = lattice_picker::Picker::new(
                    title,
                    lattice_picker::PickerSource::LspLocations,
                    lattice_picker::PickerAction::JumpToLspLocation,
                );
                p.set_lsp_locations(picker_rows);
                self.editor.picker = Some(p);
            }
        }
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
        if let Some(token) = self.editor.pending_symbols_token.take() {
            token.cancel();
        }
        // Tag-stack: a call-hierarchy drill-down is a
        // navigation intent; push the pre-jump cursor so
        // `<C-t>` pops it cleanly.
        let snapshot_for_label = self.document.snapshot();
        let label = word_under_cursor(&snapshot_for_label.buffer, self.editor.cursor).unwrap_or_default();
        self.editor.pending_tag_origin = Some(TagStackEntry {
            buffer: self.editor.active_buffer,
            buffer_id: self.active_pane_buffer_id(),
            position: self.editor.cursor,
            label,
        });
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no buffer URI -- save the file first".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let lsp_position = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => {
                self.set_message(EchoLevel::Warn, "cursor outside the document".to_string());
                return;
            }
        };
        let handles = self.editor.lsp.servers_for(&uri);
        let handle = handles
            .into_iter()
            .find(|h| h.capabilities().supports_call_hierarchy());
        let Some(handle) = handle else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server with call-hierarchy support".to_string(),
            );
            return;
        };
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<SymbolsOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_symbols_rx = Some(rx);
        self.editor.pending_symbols_token = Some(token.clone());
        let direction_label = if outgoing { "outgoing" } else { "incoming" };
        crate::runtime::spawn_on_lsp_runtime(async move {
            // 1. Prepare at cursor.
            let prepare_params = lsp_types::CallHierarchyPrepareParams {
                text_document_position_params: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri },
                    position: lsp_position,
                },
                work_done_progress_params: Default::default(),
            };
            let items = match handle
                .prepare_call_hierarchy(prepare_params, token.clone())
                .await
            {
                Ok(Some(items)) if !items.is_empty() => items,
                _ => {
                    let _ = tx.send(SymbolsOutcome::Found {
                        title: format!("{direction_label}-calls (no item at cursor)"),
                        rows: Vec::new(),
                    });
                    return;
                }
            };
            // First-item strategy: rare for a position to map
            // to >1 callable; when it does, the picker on the
            // subsequent step lets the user pick. v2 polish
            // can expose the prepare list as a pre-step
            // picker.
            let item = items.into_iter().next().expect("non-empty per match");
            let item_name = item.name.clone();
            // 2. Call the chosen direction.
            let mut rows: Vec<SymbolRow> = Vec::new();
            if outgoing {
                let p = lsp_types::CallHierarchyOutgoingCallsParams {
                    item: item.clone(),
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                if let Ok(Some(calls)) =
                    handle.call_hierarchy_outgoing_calls(p, token.clone()).await
                {
                    for call in calls {
                        if token.is_cancelled() {
                            return;
                        }
                        rows.push(call_hierarchy_to_row(&call.to, &item_name));
                    }
                }
            } else {
                let p = lsp_types::CallHierarchyIncomingCallsParams {
                    item: item.clone(),
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                if let Ok(Some(calls)) =
                    handle.call_hierarchy_incoming_calls(p, token.clone()).await
                {
                    for call in calls {
                        if token.is_cancelled() {
                            return;
                        }
                        rows.push(call_hierarchy_to_row(&call.from, &item_name));
                    }
                }
            }
            rows.sort_by(|a, b| {
                a.path
                    .cmp(&b.path)
                    .then_with(|| a.line.cmp(&b.line))
                    .then_with(|| a.col.cmp(&b.col))
                    .then_with(|| a.name.cmp(&b.name))
            });
            let title = format!("{direction_label}-calls of {item_name} ({})", rows.len());
            let _ = tx.send(SymbolsOutcome::Found { title, rows });
        });
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
        if let Some(token) = self.editor.pending_symbols_token.take() {
            token.cancel();
        }
        let snapshot_for_label = self.document.snapshot();
        let label = word_under_cursor(&snapshot_for_label.buffer, self.editor.cursor).unwrap_or_default();
        self.editor.pending_tag_origin = Some(TagStackEntry {
            buffer: self.editor.active_buffer,
            buffer_id: self.active_pane_buffer_id(),
            position: self.editor.cursor,
            label,
        });
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no buffer URI -- save the file first".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let lsp_position = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => {
                self.set_message(EchoLevel::Warn, "cursor outside the document".to_string());
                return;
            }
        };
        let handles = self.editor.lsp.servers_for(&uri);
        let handle = handles
            .into_iter()
            .find(|h| h.capabilities().supports_type_hierarchy());
        let Some(handle) = handle else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server with type-hierarchy support".to_string(),
            );
            return;
        };
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<SymbolsOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_symbols_rx = Some(rx);
        self.editor.pending_symbols_token = Some(token.clone());
        let direction_label = if subtypes { "subtypes" } else { "supertypes" };
        crate::runtime::spawn_on_lsp_runtime(async move {
            let prepare_params = lsp_types::TypeHierarchyPrepareParams {
                text_document_position_params: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri },
                    position: lsp_position,
                },
                work_done_progress_params: Default::default(),
            };
            let items = match handle
                .prepare_type_hierarchy(prepare_params, token.clone())
                .await
            {
                Ok(Some(items)) if !items.is_empty() => items,
                _ => {
                    let _ = tx.send(SymbolsOutcome::Found {
                        title: format!("{direction_label} (no type at cursor)"),
                        rows: Vec::new(),
                    });
                    return;
                }
            };
            let item = items.into_iter().next().expect("non-empty per match");
            let item_name = item.name.clone();
            let mut rows: Vec<SymbolRow> = Vec::new();
            if subtypes {
                let p = lsp_types::TypeHierarchySubtypesParams {
                    item: item.clone(),
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                if let Ok(Some(types)) = handle.type_hierarchy_subtypes(p, token.clone()).await {
                    for t in types {
                        if token.is_cancelled() {
                            return;
                        }
                        rows.push(type_hierarchy_to_row(&t, &item_name));
                    }
                }
            } else {
                let p = lsp_types::TypeHierarchySupertypesParams {
                    item: item.clone(),
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                if let Ok(Some(types)) = handle.type_hierarchy_supertypes(p, token.clone()).await {
                    for t in types {
                        if token.is_cancelled() {
                            return;
                        }
                        rows.push(type_hierarchy_to_row(&t, &item_name));
                    }
                }
            }
            rows.sort_by(|a, b| {
                a.path
                    .cmp(&b.path)
                    .then_with(|| a.line.cmp(&b.line))
                    .then_with(|| a.col.cmp(&b.col))
                    .then_with(|| a.name.cmp(&b.name))
            });
            let title = format!("{direction_label} of {item_name} ({})", rows.len());
            let _ = tx.send(SymbolsOutcome::Found { title, rows });
        });
    }

    /// 4.5.g: `:lsp-moniker`. Fires `textDocument/moniker` on
    /// the first server with `monikerProvider`; the response is
    /// folded into a one-line summary echoed to the minibuffer.
    /// Fire-and-forget UX -- no picker, no jump. Useful for
    /// indexing tools / cross-repo navigation, where the
    /// moniker is metadata about the symbol identity rather
    /// than a navigation target.
    pub(super) fn do_lsp_moniker_request(&mut self) {
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no buffer URI -- save the file first".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let lsp_position = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => {
                self.set_message(EchoLevel::Warn, "cursor outside the document".to_string());
                return;
            }
        };
        let handles = self.editor.lsp.servers_for(&uri);
        let handle = handles
            .into_iter()
            .find(|h| h.capabilities().supports_moniker());
        let Some(handle) = handle else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server with moniker support".to_string(),
            );
            return;
        };
        // Echo "querying..." synchronously; the response landing
        // overwrites the minibuffer when it arrives.
        self.set_message(EchoLevel::Info, "moniker: …".to_string());
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<String>();
        self.editor.pending_moniker_rx = Some(rx);
        let token = lattice_protocol::CancellationToken::new();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let params = lsp_types::MonikerParams {
                text_document_position_params: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri },
                    position: lsp_position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            };
            let outcome = match handle.moniker(params, token).await {
                Ok(Some(monikers)) if !monikers.is_empty() => monikers
                    .iter()
                    .map(|m| {
                        format!(
                            "{}:{}{}",
                            m.scheme,
                            m.identifier,
                            m.kind
                                .as_ref()
                                .map(|k| format!(" ({k:?})"))
                                .unwrap_or_default(),
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
                _ => "(none)".to_string(),
            };
            let _ = tx.send(outcome);
        });
    }

    /// 4.5.g: drain queued moniker responses + echo. Called
    /// per main-loop tick like the other LSP drains; cheap when
    /// the channel is empty.
    pub fn drain_pending_moniker(&mut self) {
        let Some(mut rx) = self.editor.pending_moniker_rx.take() else {
            return;
        };
        let mut latest: Option<String> = None;
        while let Ok(s) = rx.try_recv() {
            latest = Some(s);
        }
        self.editor.pending_moniker_rx = Some(rx);
        if let Some(msg) = latest {
            self.set_message(EchoLevel::Info, format!("moniker: {msg}"));
        }
    }

    /// `:lsp-signature-help` (Phase 4.3). Fan-out across attached
    /// servers; first non-empty `SignatureHelp` response wins
    /// (per docs/dev/architecture/lsp-architecture.md §7b "First non-empty wins.
    /// Signatures are usually language-specific; merging rarely
    /// useful.").
    pub(super) fn do_lsp_signature_help_request(&mut self) {
        if let Some(token) = self.editor.pending_signature_help_token.take() {
            token.cancel();
        }
        // M.6.2: lsp-signature-mode gate (after cancel-stale-work).
        // Insert-mode auto-trigger; silent (matches completion /
        // on-type-format -- typed character that doesn't fire
        // isn't a moment to surface mode state).
        if !self.lsp_signature_mode_enabled_for(self.editor.document_buffer_id) {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server attached to current buffer".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let lsp_position = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => return,
        };
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<SignatureHelpOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_signature_help_rx = Some(rx);
        self.editor.pending_signature_help_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            if handles.is_empty() {
                let _ = tx.send(SignatureHelpOutcome::NoServers);
                return;
            }
            for handle in handles {
                if token.is_cancelled() {
                    return;
                }
                if !handle.capabilities().supports_signature_help() {
                    continue;
                }
                let params = lsp_types::SignatureHelpParams {
                    text_document_position_params: lsp_types::TextDocumentPositionParams {
                        text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                        position: lsp_position,
                    },
                    work_done_progress_params: Default::default(),
                    context: None,
                };
                if let Ok(Some(sh)) = handle.signature_help(params, token.clone()).await {
                    let body = signature_help_to_markdown(&sh);
                    if !body.is_empty() {
                        let _ = tx.send(SignatureHelpOutcome::Body(body));
                        return;
                    }
                }
            }
            let _ = tx.send(SignatureHelpOutcome::Body(String::new()));
        });
    }

    /// Drain queued signature-help responses. A non-empty body
    /// renders into the popup; empty echoes "no signature info";
    /// `NoServers` echoes the standard "no LSP server" message.
    pub fn drain_pending_signature_help(&mut self) {
        let Some(mut rx) = self.editor.pending_signature_help_rx.take() else {
            return;
        };
        let mut latest: Option<SignatureHelpOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_signature_help_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        self.editor.pending_signature_help_token = None;
        match outcome {
            SignatureHelpOutcome::NoServers => {
                self.set_message(
                    EchoLevel::Info,
                    "no LSP server attached to current buffer".to_string(),
                );
            }
            SignatureHelpOutcome::Body(body) if body.is_empty() => {
                self.set_message(EchoLevel::Info, "no signature info".to_string());
            }
            SignatureHelpOutcome::Body(body) => {
                self.do_open_hover(&body);
            }
        }
    }

    /// Generic dispatch for the four navigation flavours
    /// (definition / declaration / typeDefinition / implementation
    /// -- DESIGN.md §5.4 / docs/dev/notes/lsp-features.md). All share the
    /// same `Vec<Location>` shape, so dispatch is parameterised
    /// by `LspNavKind`; the kind selects the LSP method and
    /// drives the user-facing echo from `drain_pending_definitions`.
    ///
    /// Multi-server merge: every server's response is flattened
    /// to `Vec<Location>`; the union is deduplicated by
    /// `(uri, range.start)`. A single result jumps; multiple
    /// results echo a count and open the LSP-locations picker.
    pub(super) fn do_lsp_nav_request(&mut self, kind: LspNavKind) {
        if let Some(token) = self.editor.pending_definition_token.take() {
            token.cancel();
        }
        // M.6.2: lsp-nav-mode gate (after cancel-stale-work).
        // Keymap-driven (`gd` / `gD` / `gy` / `gI`); echo so
        // users find out why nothing happened when nav is gated.
        if !self.check_lsp_sub_mode_gate(lattice_lsp::modes::LspNavMode::mode_id(), "lsp-nav-mode")
        {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server attached to current buffer".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let lsp_position = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => {
                self.set_message(
                    EchoLevel::Error,
                    format!("{}: cursor out of buffer", kind.noun_singular()),
                );
                return;
            }
        };
        // Capture the pre-jump origin for the tag stack -- the
        // gd family is "drill down" navigation, so users expect
        // <C-t> to walk back even after navigating through a
        // chain of definitions.
        let label = word_under_cursor(&snapshot.buffer, self.editor.cursor).unwrap_or_default();
        self.editor.pending_tag_origin = Some(TagStackEntry {
            buffer: self.editor.active_buffer,
            buffer_id: self.active_pane_buffer_id(),
            position: self.editor.cursor,
            label,
        });
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<Vec<lsp_types::Location>>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_definition_rx = Some(rx);
        self.editor.pending_definition_token = Some(token.clone());
        self.editor.pending_nav_kind = Some(kind);
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            let mut all: Vec<lsp_types::Location> = Vec::new();
            for handle in handles {
                if token.is_cancelled() {
                    return;
                }
                let pos_params = lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    position: lsp_position,
                };
                let resp_locs = match kind {
                    LspNavKind::Definition => {
                        let params = lsp_types::GotoDefinitionParams {
                            text_document_position_params: pos_params,
                            work_done_progress_params: Default::default(),
                            partial_result_params: Default::default(),
                        };
                        handle
                            .goto_definition(params, token.clone())
                            .await
                            .ok()
                            .flatten()
                            .map(definition_response_to_locations)
                            .unwrap_or_default()
                    }
                    LspNavKind::Declaration => {
                        let params = lsp_types::request::GotoDeclarationParams {
                            text_document_position_params: pos_params,
                            work_done_progress_params: Default::default(),
                            partial_result_params: Default::default(),
                        };
                        handle
                            .goto_declaration(params, token.clone())
                            .await
                            .ok()
                            .flatten()
                            .map(definition_response_to_locations)
                            .unwrap_or_default()
                    }
                    LspNavKind::TypeDefinition => {
                        let params = lsp_types::request::GotoTypeDefinitionParams {
                            text_document_position_params: pos_params,
                            work_done_progress_params: Default::default(),
                            partial_result_params: Default::default(),
                        };
                        handle
                            .goto_type_definition(params, token.clone())
                            .await
                            .ok()
                            .flatten()
                            .map(definition_response_to_locations)
                            .unwrap_or_default()
                    }
                    LspNavKind::Implementation => {
                        let params = lsp_types::request::GotoImplementationParams {
                            text_document_position_params: pos_params,
                            work_done_progress_params: Default::default(),
                            partial_result_params: Default::default(),
                        };
                        handle
                            .goto_implementation(params, token.clone())
                            .await
                            .ok()
                            .flatten()
                            .map(definition_response_to_locations)
                            .unwrap_or_default()
                    }
                };
                all.extend(resp_locs);
            }
            // Dedup by (uri, range.start).
            all.sort_by(|a, b| {
                let au = a.uri.as_str();
                let bu = b.uri.as_str();
                au.cmp(bu)
                    .then_with(|| a.range.start.line.cmp(&b.range.start.line))
                    .then_with(|| a.range.start.character.cmp(&b.range.start.character))
            });
            all.dedup_by(|a, b| a.uri.as_str() == b.uri.as_str() && a.range.start == b.range.start);
            let _ = tx.send(all);
        });
    }

    /// Backwards-compat wrapper. Tests + plugin contributions may
    /// reach for the named `do_lsp_definition_request`; this keeps
    /// the public surface intact while the unified `do_lsp_nav_request`
    /// handles the actual work.
    pub fn do_lsp_definition_request(&mut self) {
        self.do_lsp_nav_request(LspNavKind::Definition)
    }

    /// Drain queued nav (definition / declaration / typeDef /
    /// impl) results and act on them: 0 -> echo, 1 -> jump, N>1
    /// -> echo count + open picker. Pushes the pre-jump cursor
    /// onto the position history so `<C-o>` walks back. The verb
    /// in echoes (`definitions` vs `implementations` etc.) reads
    /// from `pending_nav_kind`.
    pub fn drain_pending_definitions(&mut self) {
        let Some(mut rx) = self.editor.pending_definition_rx.take() else {
            return;
        };
        let mut latest: Option<Vec<lsp_types::Location>> = None;
        while let Ok(locs) = rx.try_recv() {
            latest = Some(locs);
        }
        self.editor.pending_definition_rx = Some(rx);
        let locs = match latest {
            Some(l) => l,
            None => return,
        };
        // Result delivered; clear the in-flight token.
        self.editor.pending_definition_token = None;
        let kind = self
            .editor.pending_nav_kind
            .take()
            .unwrap_or(LspNavKind::Definition);
        let noun = kind.noun_plural();

        match locs.len() {
            0 => {
                // No drill-down happened; drop the captured tag
                // origin so a follow-up nav doesn't see a stale
                // value.
                self.editor.pending_tag_origin = None;
                self.set_message(EchoLevel::Info, format!("no {noun} found"));
            }
            1 => {
                // Vim-style "do what I mean" -- a single-result
                // nav request still jumps directly. Single-result
                // jump pushes the tag stack now.
                if let Some(origin) = self.editor.pending_tag_origin.take() {
                    self.editor.tag_stack.push(origin);
                }
                self.jump_to_lsp_location(&locs[0]);
            }
            _ => {
                // Multi-result -- the picker will consume the
                // pending tag origin on accept.
                self.open_lsp_locations_picker(format!("lsp:{noun}"), &locs);
            }
        }
    }

    /// `gr` (Phase 4.2.d). Send `textDocument/references` to
    /// every attached LSP server with `include_declaration: true`
    /// (vim convention -- `gr` includes the symbol's own
    /// declaration in the list). Spawn the per-server walk on
    /// the LSP runtime; drain on the next frame opens a buffer-
    /// backed `*lsp:references*` view in the active pane.
    pub(super) fn do_lsp_references_request(&mut self) {
        if let Some(token) = self.editor.pending_references_token.take() {
            token.cancel();
        }
        // Browse-style; not a tag-intent drill-down.
        self.editor.pending_tag_origin = None;
        // M.6.2: lsp-nav-mode gate (after cancel-stale-work).
        // `gr` is part of the nav family.
        if !self.check_lsp_sub_mode_gate(lattice_lsp::modes::LspNavMode::mode_id(), "lsp-nav-mode")
        {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server attached to current buffer".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let lsp_position = match app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => {
                self.set_message(
                    EchoLevel::Error,
                    "references: cursor out of buffer".to_string(),
                );
                return;
            }
        };
        let symbol = word_under_cursor(&snapshot.buffer, self.editor.cursor).unwrap_or_default();
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<ReferencesOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_references_rx = Some(rx);
        self.editor.pending_references_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            if handles.is_empty() {
                let _ = tx.send(ReferencesOutcome::NoServers);
                return;
            }
            let mut all: Vec<lsp_types::Location> = Vec::new();
            for handle in handles {
                if token.is_cancelled() {
                    return;
                }
                let params = lsp_types::ReferenceParams {
                    text_document_position: lsp_types::TextDocumentPositionParams {
                        text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                        position: lsp_position,
                    },
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                    context: lsp_types::ReferenceContext {
                        include_declaration: true,
                    },
                };
                if let Ok(Some(locs)) = handle.references(params, token.clone()).await {
                    all.extend(locs);
                }
            }
            // Sort + dedup by (uri, range.start).
            all.sort_by(|a, b| {
                let au = a.uri.as_str();
                let bu = b.uri.as_str();
                au.cmp(bu)
                    .then_with(|| a.range.start.line.cmp(&b.range.start.line))
                    .then_with(|| a.range.start.character.cmp(&b.range.start.character))
            });
            all.dedup_by(|a, b| a.uri.as_str() == b.uri.as_str() && a.range.start == b.range.start);
            let _ = tx.send(ReferencesOutcome::Found {
                symbol,
                locations: all,
            });
        });
    }

    /// Drain queued references results. The merged list is
    /// rendered as a `*lsp:references*` help buffer and opened
    /// in-pane via the LSP-locations picker; existing follow-
    /// link machinery (`<CR>` on a Source link) handles jumps.
    /// `NoServers` echoes "no LSP server attached"; an empty
    /// `Found(_, [])` echoes "no references for X".
    pub fn drain_pending_references(&mut self) {
        let Some(mut rx) = self.editor.pending_references_rx.take() else {
            return;
        };
        let mut latest: Option<ReferencesOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_references_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        // Delivered; clear the in-flight token regardless of
        // shape so a follow-up gr fires fresh.
        self.editor.pending_references_token = None;
        match outcome {
            ReferencesOutcome::NoServers => {
                self.set_message(
                    EchoLevel::Info,
                    "no LSP server attached to current buffer".to_string(),
                );
            }
            ReferencesOutcome::Found { symbol, locations } => {
                if locations.is_empty() {
                    let label = if symbol.is_empty() {
                        "(symbol)".to_string()
                    } else {
                        format!("\"{symbol}\"")
                    };
                    self.set_message(EchoLevel::Info, format!("no references for {label}"));
                    return;
                }
                let title = if symbol.is_empty() {
                    "lsp:references".to_string()
                } else {
                    format!("references: {symbol}")
                };
                self.open_lsp_locations_picker(title, &locations);
            }
        }
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
    pub(super) fn jump_to_lsp_location(&mut self, loc: &lsp_types::Location) {
        let target_path = match lattice_lsp::actor::uri_to_path(&loc.uri) {
            Some(p) => p,
            None => {
                self.set_message(
                    EchoLevel::Error,
                    format!("definition target uri is not a file: {}", loc.uri.as_str()),
                );
                return;
            }
        };
        // Push pre-jump cursor before doing anything else so a
        // subsequent <C-o> walks back to where we started, not
        // to the target.
        self.push_position_history(self.editor.cursor, super::PositionSource::PluginPush);

        // Same buffer? Just update the cursor.
        let same_buffer = self
            .document
            .path()
            .map(|p| p == target_path)
            .unwrap_or(false);
        if !same_buffer {
            self.do_edit(Some(target_path), false);
        }
        // Convert LSP target position back to App (line, byte).
        let snap = self.document.snapshot();
        let line_text = snap.buffer.line(loc.range.start.line).unwrap_or_default();
        // utf-16 -> utf-8 byte.
        let byte =
            lattice_lsp::position::utf16_column_to_utf8_byte(&line_text, loc.range.start.character);
        self.editor.cursor = lattice_protocol::position::Position::new(loc.range.start.line, byte);
    }

    /// `:diagnostics` -- open every published diagnostic across
    /// every attached server in a vertico-style picker. Severity
    /// glyph in the marginalia (`[E]` / `[W]` / `[I]` / `[H]`)
    /// and the diagnostic message as the preview text.
    pub fn do_list_diagnostics(&mut self) {
        // `:diagnostics` is a browse-style picker, not a tag-
        // intent drill-down -- clear any stale nav origin so a
        // later JumpToLspLocation accept doesn't push a phantom
        // tag stack entry.
        self.editor.pending_tag_origin = None;
        let snapshot = self.editor.lsp_diagnostics.snapshot();
        if snapshot.is_empty() {
            self.set_message(EchoLevel::Info, "no diagnostics".to_string());
            return;
        }
        let mut rows: Vec<lattice_picker::LspLocationRow> = Vec::new();
        for (uri, diags) in snapshot {
            let path = match lattice_lsp::actor::uri_to_path(&uri) {
                Some(p) => p,
                None => continue,
            };
            for d in diags {
                let sev = match d.severity {
                    Some(lattice_lsp::DiagnosticSeverity::ERROR) => "[E]",
                    Some(lattice_lsp::DiagnosticSeverity::WARNING) => "[W]",
                    Some(lattice_lsp::DiagnosticSeverity::INFORMATION) => "[I]",
                    Some(lattice_lsp::DiagnosticSeverity::HINT) => "[H]",
                    _ => "[?]",
                };
                rows.push(lattice_picker::LspLocationRow {
                    path: path.clone(),
                    line: d.range.start.line,
                    col: d.range.start.character,
                    preview: crate::help::one_line(&d.message),
                    marginalia: sev.to_string(),
                });
            }
        }
        if rows.is_empty() {
            self.set_message(EchoLevel::Info, "no diagnostics".to_string());
            return;
        }
        let total = rows.len();
        let mut p = lattice_picker::Picker::new(
            format!("diagnostics ({total})"),
            lattice_picker::PickerSource::LspLocations,
            lattice_picker::PickerAction::JumpToLspLocation,
        );
        p.set_lsp_locations(rows);
        self.editor.picker = Some(p);
    }

    /// `]d` / `:diag-next` / `:cnext` -- move the cursor to the
    /// next diagnostic in the active buffer. Wraps to top.
    pub fn do_next_diagnostic(&mut self) {
        // M.6.3: lsp-diagnostics-mode gate. The diagnostic store
        // may still hold data (the wire layer keeps writing)
        // but navigation respects the user-side disable.
        if !self.check_lsp_sub_mode_gate(
            lattice_lsp::modes::LspDiagnosticsMode::mode_id(),
            "lsp-diagnostics-mode",
        ) {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id) else {
            self.set_message(EchoLevel::Error, "no LSP attachment".to_string());
            return;
        };
        let mut diags = self.editor.lsp_diagnostics.diagnostics_for(uri);
        if diags.is_empty() {
            self.set_message(EchoLevel::Info, "no diagnostics in buffer".to_string());
            return;
        }
        diags.sort_by_key(|d| (d.range.start.line, d.range.start.character));
        let cursor = self.editor.cursor;
        let Some(next) = diags
            .iter()
            .find(|d| {
                d.range.start.line > cursor.line
                    || (d.range.start.line == cursor.line && d.range.start.character > cursor.byte)
            })
            .or_else(|| diags.first())
            .map(|d| d.range.start)
        else {
            return;
        };
        self.editor.cursor = Position::new(next.line, next.character);
        self.publish_position_change();
    }

    /// `[d` / `:diag-prev` / `:cprev` -- move the cursor to the
    /// previous diagnostic in the active buffer. Wraps to bottom.
    pub fn do_prev_diagnostic(&mut self) {
        // M.6.3: lsp-diagnostics-mode gate (symmetric to next).
        if !self.check_lsp_sub_mode_gate(
            lattice_lsp::modes::LspDiagnosticsMode::mode_id(),
            "lsp-diagnostics-mode",
        ) {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id) else {
            self.set_message(EchoLevel::Error, "no LSP attachment".to_string());
            return;
        };
        let mut diags = self.editor.lsp_diagnostics.diagnostics_for(uri);
        if diags.is_empty() {
            self.set_message(EchoLevel::Info, "no diagnostics in buffer".to_string());
            return;
        }
        diags.sort_by_key(|d| (d.range.start.line, d.range.start.character));
        let cursor = self.editor.cursor;
        let Some(prev) = diags
            .iter()
            .rev()
            .find(|d| {
                d.range.start.line < cursor.line
                    || (d.range.start.line == cursor.line && d.range.start.character < cursor.byte)
            })
            .or_else(|| diags.last())
            .map(|d| d.range.start)
        else {
            return;
        };
        self.editor.cursor = Position::new(prev.line, prev.character);
        self.publish_position_change();
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
        match server_id {
            None => {
                // B'.7: thin wrapper -- name + mode id come from
                // `lattice-lsp`, the generic helper does the
                // find-or-create + activate. The mode owns its
                // subscription and ring seed, so we don't need
                // to drain queued events here.
                let id = self.ensure_named_synthetic_document(
                    lattice_lsp::LSP_SUBSYSTEM_LOG_NAME,
                    lattice_lsp::modes::LspLogMode::mode_id(),
                    Self::SYNTHETIC_BUFFER_FLAGS,
                );
                self.activate_buffer(id);
            }
            Some(name) => {
                self.open_lsp_picker(
                    "lsp-log",
                    Some(name.to_string()),
                    lattice_picker::PickerAction::OpenLspLog,
                );
            }
        }
    }

    /// `:lsp-trace-log [server]` -- open the JSON-RPC trace ring
    /// in the active pane. Same dispatch shape as `:lsp-log`:
    /// picker on no-arg or multi-match, direct open on single
    /// match. **Does not toggle tracing** -- pair with
    /// `:lsp-trace <server>` to start / stop the wire trace; this
    /// command only views the records.
    pub fn do_open_lsp_trace_log(&mut self, server_id: Option<&str>) {
        self.open_lsp_picker(
            "lsp-trace-log",
            server_id.map(|s| s.to_string()),
            lattice_picker::PickerAction::OpenLspTraceLog,
        );
    }

    /// `:lsp-trace <name>` -- toggle JSON-RPC trace for the
    /// server. Pure toggle: the trace buffer is opened by the
    /// separate `:lsp-trace-log [server]` command so peeking
    /// mid-stream doesn't flip the toggle off.
    pub fn do_toggle_lsp_trace(&mut self, name: &str) {
        let resolved = self.resolve_server_id(name);
        let Some(server_id) = resolved else {
            let running = self.running_server_ids();
            let listing = if running.is_empty() {
                "no LSP servers running".to_string()
            } else {
                format!("running: {}", running.join(", "))
            };
            self.set_message(
                EchoLevel::Error,
                format!("lsp-trace: no server matches {name:?} ({listing})"),
            );
            return;
        };
        // B'.2: trace is per-instance. Toggle every running actor
        // with this server_id; if no actors match, toggle a
        // synthetic instance against cwd so pre-spawn toggling
        // still works (the running actor inherits the flag when
        // the supervisor builds its `InstanceKey`).
        let mut now_on_any = false;
        let mut toggled_instances: Vec<lattice_lsp::InstanceKey> = Vec::new();
        for (_key, handle) in self.editor.lsp.running_actors() {
            if handle.server_id() != server_id {
                continue;
            }
            let instance = handle.instance();
            let on = self.editor.lsp_logger.toggle_trace(instance.clone());
            if on {
                now_on_any = true;
            }
            toggled_instances.push(instance);
        }
        if toggled_instances.is_empty() {
            let synth = lattice_lsp::InstanceKey::new(
                std::sync::Arc::<str>::from(server_id.as_str()),
                std::sync::Arc::<std::path::Path>::from(
                    std::env::current_dir()
                        .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                        .as_path(),
                ),
            );
            let on = self.editor.lsp_logger.toggle_trace(synth.clone());
            if on {
                now_on_any = true;
            }
            toggled_instances.push(synth);
        }
        // Slice B: trace buffer lifecycle is bound to the toggle.
        // Creating it eagerly on toggle-on means `:ls` shows it
        // immediately and `:b *lsp:<id>:<ws>:trace*` works before
        // any record flows. The buffer survives toggle-off so
        // captured history stays browsable; the user can `:bd`
        // to discard. B'.4: one buffer per toggled instance.
        if now_on_any {
            for inst in &toggled_instances {
                let name = lattice_lsp::lsp_server_trace_log_name(inst);
                self.ensure_named_synthetic_document(
                    &name,
                    lattice_lsp::modes::LspTraceLogMode::mode_id(),
                    Self::SYNTHETIC_BUFFER_FLAGS,
                );
            }
        }
        let label = if now_on_any { "on" } else { "off" };
        let alias_note = if server_id != name {
            format!(" (resolved {name:?} -> {server_id:?})")
        } else {
            String::new()
        };
        // 4.4.b: drive the server's trace level over the wire
        // via `$/setTrace`. Matched against every running actor
        // with this id (one server may attach to multiple
        // workspaces). Failures log + skip -- the local flag
        // already flipped, and a server that ignores setTrace
        // still feeds the trace ring from wire-frame records.
        let trace_value = if now_on_any {
            lsp_types::TraceValue::Verbose
        } else {
            lsp_types::TraceValue::Off
        };
        for (_key, handle) in self.editor.lsp.running_actors() {
            if handle.server_id() != server_id {
                continue;
            }
            if let Err(e) = handle.set_trace(trace_value) {
                let instance = handle.instance();
                self.editor.lsp_logger.log(
                    Some(&instance),
                    lattice_lsp::LogLevel::Warn,
                    lattice_lsp::LogSource::Client,
                    format!("setTrace failed: {e}"),
                );
            }
        }
        self.set_message(
            EchoLevel::Info,
            format!(
                "lsp-trace {server_id}: {label}{alias_note} (use :lsp-trace-log {server_id} to view)"
            ),
        );
    }

    /// `:lsp-status` -- render every running server in a
    /// help-style buffer.
    pub fn do_lsp_status(&mut self) {
        let buffer = lattice_lsp::help_views::lsp_status_help(&self.editor.lsp)
            .with_markdown_syntax(self.editor.lang_registry.clone());
        self.display_buffer(
            buffer,
            lattice_core::ui::display::BufferDisplayCategory::LspStatus,
        );
    }

    /// `:lsp-server-log` -- vertico picker over every running
    /// `(workspace, server_id)` LSP actor. `<CR>` opens the
    /// per-server log (`*lsp:<server>*`) for the chosen row.
    pub fn do_lsp_server_log_listing(&mut self) {
        self.open_lsp_picker(
            "lsp-server-log",
            None,
            lattice_picker::PickerAction::OpenLspLog,
        );
    }

    /// `:lsp-restart <server>` -- supervisor restart hook.
    /// Currently emits an info message; full restart-with-
    /// backoff lands in 4.4.
    pub fn do_lsp_restart(&mut self, server_id: &str) {
        // 4.4.d: drive the supervisor mailbox. Backoff lives on
        // the supervisor side; the user-facing surface here is
        // just success / error / report. The call is async but
        // we don't have an executor on the UI thread, so we
        // spawn into the LSP runtime and let the result echo
        // via the bus-message path.
        let supervisor = self.editor.lsp.clone();
        let id_owned = server_id.to_string();
        let runtime = crate::runtime::lsp_runtime();
        let logger = self.editor.lsp_logger.clone();
        runtime.spawn(async move {
            match supervisor.restart_server(id_owned.clone()).await {
                Ok(report) => {
                    logger.log(
                        None,
                        lattice_lsp::LogLevel::Info,
                        lattice_lsp::LogSource::Client,
                        format!(
                            "lsp-restart {}: respawned {} actor(s); replayed didOpen on {} uri(s)",
                            report.server_id,
                            report.respawned.len(),
                            report.replayed_uris.len(),
                        ),
                    );
                }
                Err(e) => {
                    logger.log(
                        None,
                        lattice_lsp::LogLevel::Warn,
                        lattice_lsp::LogSource::Client,
                        format!("lsp-restart {}: {}", id_owned, e),
                    );
                }
            }
        });
        self.set_message(EchoLevel::Info, format!("lsp-restart {server_id}: queued"));
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
        use lattice_core::FoldMethod;
        if !matches!(self.foldmethod(), FoldMethod::Lsp) {
            return;
        }
        if !self.lsp_folding_mode_enabled_for(self.editor.document_buffer_id) {
            return;
        }
        let snapshot = self.document.snapshot();
        let version = snapshot.version;
        if let Some(cache) = self.editor.lsp_folds_cache.get(&self.editor.document_buffer_id)
            && cache.document_version == version
        {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        if let Some(token) = self.editor.pending_folding_range_token.take() {
            token.cancel();
        }
        let buffer_id = self.editor.document_buffer_id;
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::FoldingRangeOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_folding_range_rx = Some(rx);
        self.editor.pending_folding_range_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            let Some(handle) = handles
                .into_iter()
                .find(|h| h.capabilities().supports_folding_range())
            else {
                let _ = tx.send(super::FoldingRangeOutcome::Empty {
                    buffer_id,
                    document_version: version,
                });
                return;
            };
            let params = lsp_types::FoldingRangeParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            };
            match handle.folding_range(params, token.clone()).await {
                Ok(Some(ranges)) if !ranges.is_empty() => {
                    let folds = ranges
                        .into_iter()
                        .map(crate::app::folding_range_to_fold)
                        .collect();
                    let _ = tx.send(super::FoldingRangeOutcome::Items {
                        buffer_id,
                        document_version: version,
                        folds,
                    });
                }
                _ => {
                    let _ = tx.send(super::FoldingRangeOutcome::Empty {
                        buffer_id,
                        document_version: version,
                    });
                }
            }
        });
    }

    /// 4.4.f: drain the in-flight `foldingRange` response.
    /// Coalesces multiple queued outcomes to the latest. On a
    /// successful response, seats the cache + triggers a
    /// `recompute_folds` so the renderer picks up the new
    /// extents on the next frame.
    pub fn drain_pending_folding_range(&mut self) {
        let Some(mut rx) = self.editor.pending_folding_range_rx.take() else {
            return;
        };
        let mut latest: Option<super::FoldingRangeOutcome> = None;
        while let Ok(outcome) = rx.try_recv() {
            latest = Some(outcome);
        }
        self.editor.pending_folding_range_rx = Some(rx);
        let Some(outcome) = latest else {
            return;
        };
        match outcome {
            super::FoldingRangeOutcome::Items {
                buffer_id,
                document_version,
                folds,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                self.editor.lsp_folds_cache.insert(
                    buffer_id,
                    super::LspFoldsCache {
                        document_version,
                        folds,
                    },
                );
                self.recompute_folds();
            }
            super::FoldingRangeOutcome::Empty {
                buffer_id,
                document_version,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                // Remember the version so we don't re-issue
                // immediately; a future edit will bump the
                // version and re-trigger.
                self.editor.lsp_folds_cache.insert(
                    buffer_id,
                    super::LspFoldsCache {
                        document_version,
                        folds: Vec::new(),
                    },
                );
                self.recompute_folds();
            }
        }
    }

    /// 4.4.h: per-tick `semanticTokens/full` pump. Fires when
    /// `lsp-semantic-tokens-mode` is on AND the buffer's
    /// document version differs from the cache (or there's no
    /// cache). Single-flight; the decoder runs server-side on
    /// the spawned task and the drain seats decoded tokens
    /// into the cache.
    pub fn maybe_request_semantic_tokens(&mut self) {
        if !self.lsp_semantic_tokens_mode_enabled_for(self.editor.document_buffer_id) {
            return;
        }
        let snapshot = self.document.snapshot();
        let version = snapshot.version;
        // 4.4.i: pull the prior result_id (if any) before the
        // version-equal early-return. The delta path uses the
        // previous result_id; if we already cached this version
        // we have nothing to do.
        let prior = self.editor.lsp_semantic_tokens_cache.get(&self.editor.document_buffer_id);
        if let Some(cache) = prior
            && cache.document_version == version
        {
            return;
        }
        let prior_result_id = prior.and_then(|c| c.result_id.clone());
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        if let Some(token) = self.editor.pending_semantic_tokens_token.take() {
            token.cancel();
        }
        let buffer_id = self.editor.document_buffer_id;
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::SemanticTokensOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_semantic_tokens_rx = Some(rx);
        self.editor.pending_semantic_tokens_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            let Some(handle) = handles
                .into_iter()
                .find(|h| h.capabilities().supports_semantic_tokens())
            else {
                let _ = tx.send(super::SemanticTokensOutcome::Empty {
                    buffer_id,
                    document_version: version,
                });
                return;
            };
            // Capture the legend snapshot per-request: a server
            // can in principle re-register its legend via
            // dynamic registration, so reading at the moment
            // we decode keeps us aligned with what the server
            // wrote.
            let caps = handle.capabilities();
            let token_types = caps.semantic_token_types();
            let token_modifiers = caps.semantic_token_modifiers();
            // 4.4.i: prefer full/delta when a prior result_id
            // exists AND the server advertises delta support.
            // Falls back to a plain `full` request when either
            // side hasn't supplied the prerequisites.
            if let (Some(prev_id), true) = (prior_result_id, caps.supports_semantic_tokens_delta())
            {
                let params = lsp_types::SemanticTokensDeltaParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    previous_result_id: prev_id.clone(),
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                };
                match handle
                    .semantic_tokens_full_delta(params, token.clone())
                    .await
                {
                    Ok(Some(lsp_types::SemanticTokensFullDeltaResult::Tokens(t))) => {
                        let decoded = crate::app::decode_semantic_tokens(
                            &t.data,
                            &token_types,
                            &token_modifiers,
                        );
                        let _ = tx.send(super::SemanticTokensOutcome::Items {
                            buffer_id,
                            document_version: version,
                            result_id: t.result_id,
                            raw_data: t.data,
                            tokens: decoded,
                        });
                    }
                    Ok(Some(lsp_types::SemanticTokensFullDeltaResult::TokensDelta(d))) => {
                        let _ = tx.send(super::SemanticTokensOutcome::Delta {
                            buffer_id,
                            document_version: version,
                            previous_result_id: prev_id,
                            new_result_id: d.result_id,
                            edits: d.edits,
                            token_types: token_types.clone(),
                            token_modifiers: token_modifiers.clone(),
                        });
                    }
                    _ => {
                        let _ = tx.send(super::SemanticTokensOutcome::Empty {
                            buffer_id,
                            document_version: version,
                        });
                    }
                }
                return;
            }
            let params = lsp_types::SemanticTokensParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            };
            match handle.semantic_tokens_full(params, token.clone()).await {
                Ok(Some(lsp_types::SemanticTokensResult::Tokens(t))) => {
                    let decoded =
                        crate::app::decode_semantic_tokens(&t.data, &token_types, &token_modifiers);
                    let _ = tx.send(super::SemanticTokensOutcome::Items {
                        buffer_id,
                        document_version: version,
                        result_id: t.result_id,
                        raw_data: t.data,
                        tokens: decoded,
                    });
                }
                Ok(Some(lsp_types::SemanticTokensResult::Partial(_))) => {
                    // Partial-result streaming is a 4.4.i / future
                    // optimization; treat the partial response as
                    // "nothing for now" and wait for the next
                    // full request after the next edit.
                    let _ = tx.send(super::SemanticTokensOutcome::Empty {
                        buffer_id,
                        document_version: version,
                    });
                }
                _ => {
                    let _ = tx.send(super::SemanticTokensOutcome::Empty {
                        buffer_id,
                        document_version: version,
                    });
                }
            }
        });
    }

    /// 4.4.h: drain the in-flight `semanticTokens/full`
    /// response. Coalesces multiple queued outcomes to the
    /// latest. On success seats the cache; on Empty seats an
    /// empty list so we don't re-issue immediately.
    pub fn drain_pending_semantic_tokens(&mut self) {
        let Some(mut rx) = self.editor.pending_semantic_tokens_rx.take() else {
            return;
        };
        let mut latest: Option<super::SemanticTokensOutcome> = None;
        while let Ok(outcome) = rx.try_recv() {
            latest = Some(outcome);
        }
        self.editor.pending_semantic_tokens_rx = Some(rx);
        let Some(outcome) = latest else {
            return;
        };
        match outcome {
            super::SemanticTokensOutcome::Items {
                buffer_id,
                document_version,
                result_id,
                raw_data,
                tokens,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                self.editor.lsp_semantic_tokens_cache.insert(
                    buffer_id,
                    super::LspSemanticTokensCache {
                        document_version,
                        result_id,
                        raw_data,
                        tokens,
                    },
                );
            }
            // 4.4.i: splice the server-issued edit script into
            // the cached raw token vec, re-decode for the
            // renderer, and seat the updated cache entry.
            // Stale-baseline check: if our current cache's
            // result_id no longer matches `previous_result_id`
            // we drop the cache and let the next pump issue a
            // fresh `full` request. Same fallback on splice
            // failure (out-of-bounds edit indices).
            super::SemanticTokensOutcome::Delta {
                buffer_id,
                document_version,
                previous_result_id,
                new_result_id,
                edits,
                token_types,
                token_modifiers,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                let Some(cache) = self.editor.lsp_semantic_tokens_cache.get(&buffer_id) else {
                    return;
                };
                if cache.result_id.as_deref() != Some(previous_result_id.as_str()) {
                    self.editor.lsp_semantic_tokens_cache.remove(&buffer_id);
                    return;
                }
                let mut raw_data = cache.raw_data.clone();
                if crate::app::apply_semantic_token_edits(&mut raw_data, &edits).is_err() {
                    self.editor.lsp_semantic_tokens_cache.remove(&buffer_id);
                    return;
                }
                let decoded =
                    crate::app::decode_semantic_tokens(&raw_data, &token_types, &token_modifiers);
                self.editor.lsp_semantic_tokens_cache.insert(
                    buffer_id,
                    super::LspSemanticTokensCache {
                        document_version,
                        result_id: new_result_id,
                        raw_data,
                        tokens: decoded,
                    },
                );
            }
            super::SemanticTokensOutcome::Empty {
                buffer_id,
                document_version,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                self.editor.lsp_semantic_tokens_cache.insert(
                    buffer_id,
                    super::LspSemanticTokensCache {
                        document_version,
                        result_id: None,
                        raw_data: Vec::new(),
                        tokens: Vec::new(),
                    },
                );
            }
        }
    }

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
        if !self.lsp_diagnostics_mode_enabled_for(self.editor.document_buffer_id) {
            return;
        }
        let snapshot = self.document.snapshot();
        let version = snapshot.version;
        let prior = self
            .editor.lsp_pull_diagnostics_cache
            .get(&self.editor.document_buffer_id);
        if let Some(cache) = prior
            && cache.document_version == version
        {
            return;
        }
        let prior_result_id = prior.and_then(|c| c.result_id.clone());
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        if let Some(token) = self.editor.pending_pull_diagnostics_token.take() {
            token.cancel();
        }
        let buffer_id = self.editor.document_buffer_id;
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::PullDiagnosticsOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_pull_diagnostics_rx = Some(rx);
        self.editor.pending_pull_diagnostics_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = lsp.servers_for(&uri);
            let Some(handle) = handles
                .into_iter()
                .find(|h| h.capabilities().supports_pull_diagnostics())
            else {
                let _ = tx.send(super::PullDiagnosticsOutcome::Empty {
                    buffer_id,
                    document_version: version,
                });
                return;
            };
            let identifier = handle.capabilities().diagnostic_identifier();
            let server_id_arc: std::sync::Arc<str> = std::sync::Arc::from(handle.server_id());
            let params = lsp_types::DocumentDiagnosticParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                identifier,
                previous_result_id: prior_result_id,
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            };
            match handle.document_diagnostic(params, token.clone()).await {
                Ok(lsp_types::DocumentDiagnosticReportResult::Report(report)) => match report {
                    lsp_types::DocumentDiagnosticReport::Full(full) => {
                        let inner = full.full_document_diagnostic_report;
                        let _ = tx.send(super::PullDiagnosticsOutcome::Full {
                            buffer_id,
                            server_id: server_id_arc,
                            uri,
                            document_version: version,
                            result_id: inner.result_id,
                            diagnostics: inner.items,
                        });
                    }
                    lsp_types::DocumentDiagnosticReport::Unchanged(unchanged) => {
                        let _ = tx.send(super::PullDiagnosticsOutcome::Unchanged {
                            buffer_id,
                            document_version: version,
                            result_id: unchanged.unchanged_document_diagnostic_report.result_id,
                        });
                    }
                },
                // Partial streaming: treat as Empty for v1 (a
                // follow-up could splice partial-result chunks
                // the same way semantic-tokens partials would).
                _ => {
                    let _ = tx.send(super::PullDiagnosticsOutcome::Empty {
                        buffer_id,
                        document_version: version,
                    });
                }
            }
        });
    }

    /// 4.4.j: drain the in-flight `textDocument/diagnostic`
    /// response. Full reports flow into `DiagnosticsLayer` via
    /// the same `DiagnosticEvent` shape push diagnostics use;
    /// Unchanged reports refresh just the cache's
    /// (version, result_id) pair; Empty reports seat the
    /// version so the pump doesn't re-fire idly.
    pub fn drain_pending_pull_diagnostics(&mut self) {
        let Some(mut rx) = self.editor.pending_pull_diagnostics_rx.take() else {
            return;
        };
        let mut latest: Option<super::PullDiagnosticsOutcome> = None;
        while let Ok(outcome) = rx.try_recv() {
            latest = Some(outcome);
        }
        self.editor.pending_pull_diagnostics_rx = Some(rx);
        let Some(outcome) = latest else {
            return;
        };
        match outcome {
            super::PullDiagnosticsOutcome::Full {
                buffer_id,
                server_id,
                uri,
                document_version,
                result_id,
                diagnostics,
            } => {
                self.editor.lsp_pull_diagnostics_cache.insert(
                    buffer_id,
                    super::LspPullDiagnosticsCache {
                        document_version,
                        result_id,
                    },
                );
                // Cast the doc version into LSP's i32 (the
                // version that rides on `DiagnosticEvent` is
                // the *server's* notion of version, but our
                // pull response doesn't carry one; threading
                // our own version is the closest analogue and
                // lets the layer's stale-drop logic stay
                // honest).
                let version_i32 = i32::try_from(document_version).ok();
                self.editor.lsp_diagnostics.apply(lattice_lsp::DiagnosticEvent {
                    server_id,
                    uri,
                    version: version_i32,
                    diagnostics: std::sync::Arc::from(diagnostics.into_boxed_slice()),
                });
            }
            super::PullDiagnosticsOutcome::Unchanged {
                buffer_id,
                document_version,
                result_id,
            } => {
                self.editor.lsp_pull_diagnostics_cache.insert(
                    buffer_id,
                    super::LspPullDiagnosticsCache {
                        document_version,
                        result_id: Some(result_id),
                    },
                );
            }
            super::PullDiagnosticsOutcome::Empty {
                buffer_id,
                document_version,
            } => {
                self.editor.lsp_pull_diagnostics_cache.insert(
                    buffer_id,
                    super::LspPullDiagnosticsCache {
                        document_version,
                        result_id: None,
                    },
                );
            }
        }
    }

    /// 4.4.j: drain `workspace/diagnostic/refresh` events.
    /// Each event names a server; evict the per-buffer
    /// `result_id` cache for every attached buffer so the
    /// next pump tick re-pulls without a `previous_result_id`
    /// and the server emits a forced `Full` report.
    pub fn drain_diagnostic_refresh(&mut self) {
        let Some(mut rx) = self.editor.pending_diagnostic_refresh_rx.take() else {
            return;
        };
        let mut refreshes: Vec<lattice_lsp::LspDiagnosticRefresh> = Vec::new();
        while let Ok(event) = rx.try_recv() {
            refreshes.push(event);
        }
        self.editor.pending_diagnostic_refresh_rx = Some(rx);
        if refreshes.is_empty() {
            return;
        }
        let buffer_ids: Vec<BufferId> = self
            .editor.buffer_uris
            .iter()
            .filter_map(|(id, uri)| {
                let handles = self.editor.lsp.servers_for(uri);
                let attached = handles.iter().any(|h| {
                    refreshes
                        .iter()
                        .any(|r| r.server_id.as_ref() == h.server_id())
                });
                if attached { Some(*id) } else { None }
            })
            .collect();
        for buffer_id in buffer_ids {
            self.editor.lsp_pull_diagnostics_cache.remove(&buffer_id);
        }
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
        if !self.lsp_inlay_hint_mode_enabled_for(self.editor.document_buffer_id) {
            return;
        }
        let snapshot = self.document.snapshot();
        let version = snapshot.version;
        // 4.4.g viewport polish: pick the line range to fetch.
        // Visible viewport ± overscan margin, clamped to the
        // buffer's last addressable line. Refetch when:
        //   - document version changed, OR
        //   - viewport scrolled outside the cached requested
        //     range (with overscan giving small scrolls room
        //     before triggering a new request).
        // Small files where the overscanned range already
        // covers the whole buffer naturally see the same cache
        // hit semantics as the prior whole-buffer pump.
        const OVERSCAN_LINES: u32 = 100;
        let last_buffer_line = last_addressable_line(&snapshot.buffer);
        let viewport_first = self.editor.scroll;
        let viewport_last = self
            .editor.scroll
            .saturating_add(self.editor.viewport_height.saturating_sub(1));
        let requested_first = viewport_first.saturating_sub(OVERSCAN_LINES);
        let requested_last = viewport_last
            .saturating_add(OVERSCAN_LINES)
            .min(last_buffer_line);
        if let Some(cache) = self.editor.lsp_inlay_hints_cache.get(&self.editor.document_buffer_id)
            && cache.document_version == version
            && viewport_first >= cache.requested_first_line
            && viewport_last <= cache.requested_last_line
        {
            return;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        if let Some(token) = self.editor.pending_inlay_hint_token.take() {
            token.cancel();
        }
        let buffer_id = self.editor.document_buffer_id;
        // LSP positions are 0-based; end of range is exclusive
        // so we set end.line = requested_last + 1 with
        // character = 0 to cover the entire last line without
        // utf-16 column conversion.
        let range = lsp_types::Range {
            start: lsp_types::Position {
                line: requested_first,
                character: 0,
            },
            end: lsp_types::Position {
                line: requested_last.saturating_add(1),
                character: 0,
            },
        };
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::InlayHintOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_inlay_hint_rx = Some(rx);
        self.editor.pending_inlay_hint_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            let Some(handle) = handles
                .into_iter()
                .find(|h| h.capabilities().supports_inlay_hint())
            else {
                let _ = tx.send(super::InlayHintOutcome::Empty {
                    buffer_id,
                    document_version: version,
                    requested_first_line: requested_first,
                    requested_last_line: requested_last,
                });
                return;
            };
            let params = lsp_types::InlayHintParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                range,
                work_done_progress_params: Default::default(),
            };
            match handle.inlay_hint(params, token.clone()).await {
                Ok(Some(hints)) if !hints.is_empty() => {
                    let _ = tx.send(super::InlayHintOutcome::Items {
                        buffer_id,
                        document_version: version,
                        hints,
                        requested_first_line: requested_first,
                        requested_last_line: requested_last,
                    });
                }
                _ => {
                    let _ = tx.send(super::InlayHintOutcome::Empty {
                        buffer_id,
                        document_version: version,
                        requested_first_line: requested_first,
                        requested_last_line: requested_last,
                    });
                }
            }
        });
    }

    /// 4.4.g: drain `workspace/inlayHint/refresh` events. Each
    /// event names a server; clear cached inlay hints for any
    /// buffer attached to that server so the next render
    /// tick's pump re-issues `inlayHint`.
    pub fn drain_inlay_hint_refresh(&mut self) {
        let Some(mut rx) = self.editor.pending_inlay_hint_refresh_rx.take() else {
            return;
        };
        let mut refreshes: Vec<lattice_lsp::LspInlayHintRefresh> = Vec::new();
        while let Ok(event) = rx.try_recv() {
            refreshes.push(event);
        }
        self.editor.pending_inlay_hint_refresh_rx = Some(rx);
        if refreshes.is_empty() {
            return;
        }
        // Build a set of (BufferId) entries to invalidate. A
        // buffer is "attached" to a server when its URI shows
        // up in the supervisor's per-server attachment map.
        // Simple coarser-than-needed approach: any buffer
        // whose attached servers list contains the named id
        // gets its cache cleared.
        let buffer_ids: Vec<BufferId> = self
            .editor.buffer_uris
            .iter()
            .filter_map(|(id, uri)| {
                let handles = self.editor.lsp.servers_for(uri);
                let attached = handles.iter().any(|h| {
                    refreshes
                        .iter()
                        .any(|r| r.server_id.as_ref() == h.server_id())
                });
                if attached { Some(*id) } else { None }
            })
            .collect();
        for buffer_id in buffer_ids {
            self.editor.lsp_inlay_hints_cache.remove(&buffer_id);
        }
    }

    /// 4.4.i: drain `workspace/semanticTokens/refresh` events.
    /// Same shape as the inlay-hint refresh drain: each event
    /// names a server; drop the semantic-tokens cache for every
    /// attached buffer so the next render tick's pump re-issues
    /// `semanticTokens/full` against a fresh baseline (dropping
    /// the now-stale `result_id` rules out a delta request that
    /// the server would reject).
    pub fn drain_semantic_tokens_refresh(&mut self) {
        let Some(mut rx) = self.editor.pending_semantic_tokens_refresh_rx.take() else {
            return;
        };
        let mut refreshes: Vec<lattice_lsp::LspSemanticTokensRefresh> = Vec::new();
        while let Ok(event) = rx.try_recv() {
            refreshes.push(event);
        }
        self.editor.pending_semantic_tokens_refresh_rx = Some(rx);
        if refreshes.is_empty() {
            return;
        }
        let buffer_ids: Vec<BufferId> = self
            .editor.buffer_uris
            .iter()
            .filter_map(|(id, uri)| {
                let handles = self.editor.lsp.servers_for(uri);
                let attached = handles.iter().any(|h| {
                    refreshes
                        .iter()
                        .any(|r| r.server_id.as_ref() == h.server_id())
                });
                if attached { Some(*id) } else { None }
            })
            .collect();
        for buffer_id in buffer_ids {
            self.editor.lsp_semantic_tokens_cache.remove(&buffer_id);
        }
    }

    /// 4.4.g: drain the in-flight `inlayHint` response.
    /// Coalesces multiple queued outcomes to the latest.
    pub fn drain_pending_inlay_hint(&mut self) {
        let Some(mut rx) = self.editor.pending_inlay_hint_rx.take() else {
            return;
        };
        let mut latest: Option<super::InlayHintOutcome> = None;
        while let Ok(outcome) = rx.try_recv() {
            latest = Some(outcome);
        }
        self.editor.pending_inlay_hint_rx = Some(rx);
        let Some(outcome) = latest else {
            return;
        };
        match outcome {
            super::InlayHintOutcome::Items {
                buffer_id,
                document_version,
                hints,
                requested_first_line,
                requested_last_line,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                self.editor.lsp_inlay_hints_cache.insert(
                    buffer_id,
                    super::LspInlayHintCache {
                        document_version,
                        hints,
                        requested_first_line,
                        requested_last_line,
                    },
                );
            }
            super::InlayHintOutcome::Empty {
                buffer_id,
                document_version,
                requested_first_line,
                requested_last_line,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                // Cache empty list so we don't re-issue
                // immediately; bumped on next edit / when the
                // viewport scrolls outside the requested range.
                self.editor.lsp_inlay_hints_cache.insert(
                    buffer_id,
                    super::LspInlayHintCache {
                        document_version,
                        hints: Vec::new(),
                        requested_first_line,
                        requested_last_line,
                    },
                );
            }
        }
    }

    /// 4.5.c: per-tick `documentLink` pump. Fires on
    /// document-version change (cheap when versions match;
    /// the cache lookup short-circuits). Whole-buffer request
    /// since link ranges are typically sparse and not bound
    /// to a viewport. Single-flight per buffer; each new
    /// request cancels its predecessor.
    pub fn maybe_request_document_link(&mut self) {
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        let snapshot = self.document.snapshot();
        let version = snapshot.version;
        if let Some(cache) = self.editor.lsp_document_links_cache.get(&self.editor.document_buffer_id)
            && cache.document_version == version
        {
            return;
        }
        if let Some(token) = self.editor.pending_document_links_token.take() {
            token.cancel();
        }
        let buffer_id = self.editor.document_buffer_id;
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::DocumentLinksOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_document_links_rx = Some(rx);
        self.editor.pending_document_links_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = lsp.servers_for(&uri);
            let Some(handle) = handles
                .into_iter()
                .find(|h| h.capabilities().supports_document_link())
            else {
                let _ = tx.send(super::DocumentLinksOutcome::Empty {
                    buffer_id,
                    document_version: version,
                });
                return;
            };
            let params = lsp_types::DocumentLinkParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            };
            match handle.document_link(params, token.clone()).await {
                Ok(Some(links)) if !links.is_empty() => {
                    let _ = tx.send(super::DocumentLinksOutcome::Items {
                        buffer_id,
                        document_version: version,
                        links,
                    });
                }
                _ => {
                    let _ = tx.send(super::DocumentLinksOutcome::Empty {
                        buffer_id,
                        document_version: version,
                    });
                }
            }
        });
    }

    /// 4.5.c: drain queued `documentLink` responses + seat the
    /// cache. Cancels stale single-flight tokens; `gx`
    /// consults the cache when triggered.
    pub fn drain_pending_document_link(&mut self) {
        let Some(mut rx) = self.editor.pending_document_links_rx.take() else {
            return;
        };
        let mut latest: Option<super::DocumentLinksOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_document_links_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        self.editor.pending_document_links_token = None;
        match outcome {
            super::DocumentLinksOutcome::Items {
                buffer_id,
                document_version,
                links,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                self.editor.lsp_document_links_cache.insert(
                    buffer_id,
                    super::LspDocumentLinksCache {
                        document_version,
                        links,
                    },
                );
            }
            super::DocumentLinksOutcome::Empty {
                buffer_id,
                document_version,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                // Empty cache prevents re-issuing for the same
                // version; bumped on next edit.
                self.editor.lsp_document_links_cache.insert(
                    buffer_id,
                    super::LspDocumentLinksCache {
                        document_version,
                        links: Vec::new(),
                    },
                );
            }
        }
    }

    /// 4.5.c: follow the LSP `documentLink` at the cursor (the
    /// `gx` keystroke). Walks the cache, picks the first link
    /// whose range covers the cursor, follows its `target`.
    /// When the link has no target AND the server advertises
    /// `documentLinkProvider.resolveProvider`, fires
    /// `documentLink/resolve` to fill in the target before
    /// following. Echoes `(no link at cursor)` when the cache
    /// is empty or the cursor sits outside every cached range.
    pub fn do_lsp_follow_link_at_cursor(&mut self) {
        let snapshot = self.document.snapshot();
        let Some(pos) = app_to_lsp_position(&snapshot.buffer, self.editor.cursor) else {
            return;
        };
        let buffer_id = self.editor.document_buffer_id;
        let link = self
            .editor.lsp_document_links_cache
            .get(&buffer_id)
            .and_then(|c| c.links.iter().find(|l| range_covers(l.range, pos)).cloned());
        let Some(link) = link else {
            self.set_message(EchoLevel::Info, "no link at cursor".to_string());
            return;
        };
        // Direct target hit -> follow.
        if let Some(target) = link.target.clone() {
            self.follow_document_link_target(&target);
            return;
        }
        // No target -> need documentLink/resolve. Capability
        // gate: silently skip when the server doesn't advertise
        // resolveProvider (the link wouldn't get a target on a
        // round-trip anyway).
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        let handles = self.editor.lsp.servers_for(&uri);
        let Some(handle) = handles.into_iter().find(|h| {
            let c = h.capabilities();
            c.supports_document_link() && c.document_link_resolve_provider()
        }) else {
            self.set_message(
                EchoLevel::Info,
                "link has no target (server doesn't resolve)".to_string(),
            );
            return;
        };
        let token = lattice_protocol::CancellationToken::new();
        // `block_on` on the LSP runtime so we don't deadlock the
        // UI thread waiting on a network round-trip. The user
        // typed `gx`; a few-ms wait is acceptable; long resolves
        // are bounded by the protocol's cancellation token.
        let resolved =
            lattice_runtime::block_on(
                async move { handle.document_link_resolve(link, token).await },
            );
        match resolved {
            Ok(resolved) => {
                if let Some(target) = resolved.target {
                    self.follow_document_link_target(&target);
                } else {
                    self.set_message(
                        EchoLevel::Info,
                        "link has no target after resolve".to_string(),
                    );
                }
            }
            Err(_) => {
                self.set_message(EchoLevel::Warn, "documentLink/resolve failed".to_string());
            }
        }
    }

    /// Follow a `documentLink` target URI: `file://` URIs open
    /// the path in a new buffer (same path as `:e`); anything
    /// else (`http(s)://`, etc.) delegates to the OS handler.
    /// Non-`file://` URIs use the same dispatch as
    /// `window/showDocument`'s `external` path.
    fn follow_document_link_target(&mut self, target: &lsp_types::Uri) {
        let target_str = target.as_str();
        if target_str.starts_with("file://") {
            if let Some(path) = lattice_lsp::actor::uri_to_path(target) {
                self.do_edit(Some(path), false);
            } else {
                self.set_message(
                    EchoLevel::Warn,
                    format!("could not parse file URI: {target_str}"),
                );
            }
            return;
        }
        // External URI -> OS handler. Reuse the same dispatch
        // shape `window/showDocument` uses for `external = true`.
        // No per-instance tagging here -- the link came from the
        // active document's cache, not a server callback, so we
        // synthesise an `<editor>` instance whose workspace is
        // the current cwd (best-effort -- the log routes
        // somewhere reasonable).
        let instance = lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from("<editor>"),
            std::sync::Arc::<std::path::Path>::from(
                std::env::current_dir()
                    .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                    .as_path(),
            ),
        );
        let opened = self.open_external_uri(&instance, target_str);
        if !opened {
            self.set_message(EchoLevel::Warn, format!("could not open {target_str}"));
        }
    }

    /// 4.5.d: per-tick `codeLens` pump. Fires on document-
    /// version change OR cache miss (`workspace/codeLens/refresh`
    /// evicts the entry). Single-flight per buffer.
    pub fn maybe_request_code_lens(&mut self) {
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        let snapshot = self.document.snapshot();
        let version = snapshot.version;
        if let Some(cache) = self.editor.lsp_code_lens_cache.get(&self.editor.document_buffer_id)
            && cache.document_version == version
        {
            return;
        }
        if let Some(token) = self.editor.pending_code_lens_token.take() {
            token.cancel();
        }
        let buffer_id = self.editor.document_buffer_id;
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CodeLensOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_code_lens_rx = Some(rx);
        self.editor.pending_code_lens_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = lsp.servers_for(&uri);
            let Some(handle) = handles
                .into_iter()
                .find(|h| h.capabilities().supports_code_lens())
            else {
                let _ = tx.send(super::CodeLensOutcome::Empty {
                    buffer_id,
                    document_version: version,
                });
                return;
            };
            let server_id_arc: std::sync::Arc<str> = std::sync::Arc::from(handle.server_id());
            let params = lsp_types::CodeLensParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            };
            match handle.code_lens(params, token.clone()).await {
                Ok(Some(lenses)) if !lenses.is_empty() => {
                    let _ = tx.send(super::CodeLensOutcome::Items {
                        buffer_id,
                        document_version: version,
                        server_id: server_id_arc,
                        lenses,
                    });
                }
                _ => {
                    let _ = tx.send(super::CodeLensOutcome::Empty {
                        buffer_id,
                        document_version: version,
                    });
                }
            }
        });
    }

    /// 4.5.d: drain queued `codeLens` responses + seat the
    /// cache. The cache feeds `:lsp-code-lens`; the picker
    /// reads from it.
    pub fn drain_pending_code_lens(&mut self) {
        let Some(mut rx) = self.editor.pending_code_lens_rx.take() else {
            return;
        };
        let mut latest: Option<super::CodeLensOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_code_lens_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        self.editor.pending_code_lens_token = None;
        match outcome {
            super::CodeLensOutcome::Items {
                buffer_id,
                document_version,
                server_id,
                lenses,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                self.editor.lsp_code_lens_cache.insert(
                    buffer_id,
                    super::LspCodeLensCache {
                        document_version,
                        lenses,
                        server_id,
                    },
                );
            }
            super::CodeLensOutcome::Empty {
                buffer_id,
                document_version,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                // Empty cache prevents re-issuing for the same
                // version; bumped on next edit / refresh.
                self.editor.lsp_code_lens_cache.insert(
                    buffer_id,
                    super::LspCodeLensCache {
                        document_version,
                        lenses: Vec::new(),
                        server_id: std::sync::Arc::from("<none>"),
                    },
                );
            }
        }
    }

    /// 4.5.d: `:lsp-code-lens`. Open a picker over the
    /// active buffer's cached lenses. Empty cache -> echo.
    /// Accept routes through
    /// [`Self::accept_lsp_code_lens`].
    pub(super) fn do_lsp_code_lens_picker(&mut self) {
        let buffer_id = self.editor.document_buffer_id;
        let Some(cache) = self.editor.lsp_code_lens_cache.get(&buffer_id).cloned() else {
            self.set_message(EchoLevel::Info, "no code lenses (cache empty)".to_string());
            return;
        };
        if cache.lenses.is_empty() {
            self.set_message(EchoLevel::Info, "no code lenses".to_string());
            return;
        }
        let pairs: Vec<(
            lattice_completion::RawCandidate,
            lattice_picker::RoutingPayload,
        )> = cache
            .lenses
            .iter()
            .enumerate()
            .map(|(i, lens)| {
                let title = lens
                    .command
                    .as_ref()
                    .map(|c| c.title.clone())
                    .unwrap_or_else(|| {
                        format!("(unresolved lens at line {})", lens.range.start.line)
                    });
                let line = lens.range.start.line;
                let display = format!("{line:>4}  {title}");
                let mut c = lattice_completion::RawCandidate::plain(
                    title,
                    lattice_completion::CandidateKind::Plain,
                );
                c.display = display;
                (
                    c,
                    lattice_picker::RoutingPayload::LspCodeLens { index: i as u32 },
                )
            })
            .collect();
        let total = pairs.len();
        self.editor.pending_code_lens_items = Some(cache.lenses.clone());
        self.editor.pending_code_lens_server = Some(cache.server_id.clone());
        let mut p = lattice_picker::Picker::new(
            format!("code-lens ({total})"),
            lattice_picker::PickerSource::LspLocations,
            lattice_picker::PickerAction::AcceptLspCodeLens,
        );
        p.set_raw_candidates_with_routing(pairs);
        self.editor.picker = Some(p);
    }

    /// 4.5.d: accept a code lens by `index` (the routing
    /// payload). Resolves the lens via `codeLens/resolve`
    /// when its `command` is missing AND the server advertises
    /// `codeLensProvider.resolveProvider`. The resulting
    /// `command` routes through `workspace/executeCommand`
    /// on the originating server (the one that produced the
    /// cache).
    pub(super) fn accept_lsp_code_lens(&mut self, index: u32) {
        let Some(items) = self.editor.pending_code_lens_items.take() else {
            return;
        };
        let server_id = self.editor.pending_code_lens_server.take();
        let Some(lens) = items.get(index as usize).cloned() else {
            self.set_message(
                EchoLevel::Warn,
                format!("code-lens accept: index {index} out of bounds"),
            );
            return;
        };
        let Some(server_id) = server_id else {
            self.set_message(
                EchoLevel::Warn,
                "code-lens accept: no server id captured".to_string(),
            );
            return;
        };
        // Look up the originating server handle by id.
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        let handle = self
            .editor.lsp
            .servers_for(&uri)
            .into_iter()
            .find(|h| h.server_id() == server_id.as_ref());
        let Some(handle) = handle else {
            self.set_message(
                EchoLevel::Warn,
                format!("code-lens accept: server `{server_id}` no longer attached"),
            );
            return;
        };
        // Resolve lazily when needed.
        let command = if let Some(cmd) = lens.command.clone() {
            Some(cmd)
        } else if handle.capabilities().code_lens_resolve_provider() {
            let handle_for_resolve = handle.clone();
            let token = lattice_protocol::CancellationToken::new();
            let resolved = lattice_runtime::block_on(async move {
                handle_for_resolve.code_lens_resolve(lens, token).await
            });
            match resolved {
                Ok(r) => r.command,
                Err(_) => {
                    self.set_message(EchoLevel::Warn, "codeLens/resolve failed".to_string());
                    return;
                }
            }
        } else {
            self.set_message(
                EchoLevel::Info,
                "code lens has no command (server doesn't resolve)".to_string(),
            );
            return;
        };
        let Some(command) = command else {
            self.set_message(
                EchoLevel::Info,
                "code lens has no command after resolve".to_string(),
            );
            return;
        };
        self.execute_lsp_command(Some(handle), command);
    }

    /// 4.5.e: per-tick `documentColor` pump. Same shape as
    /// the documentLink pump: fires on doc-version change,
    /// single-flight per buffer.
    pub fn maybe_request_document_color(&mut self) {
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        let snapshot = self.document.snapshot();
        let version = snapshot.version;
        if let Some(cache) = self.editor.lsp_document_color_cache.get(&self.editor.document_buffer_id)
            && cache.document_version == version
        {
            return;
        }
        if let Some(token) = self.editor.pending_document_color_token.take() {
            token.cancel();
        }
        let buffer_id = self.editor.document_buffer_id;
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::DocumentColorOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_document_color_rx = Some(rx);
        self.editor.pending_document_color_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = lsp.servers_for(&uri);
            let Some(handle) = handles
                .into_iter()
                .find(|h| h.capabilities().supports_color())
            else {
                let _ = tx.send(super::DocumentColorOutcome::Empty {
                    buffer_id,
                    document_version: version,
                });
                return;
            };
            let server_id_arc: std::sync::Arc<str> = std::sync::Arc::from(handle.server_id());
            let params = lsp_types::DocumentColorParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            };
            match handle.document_color(params, token.clone()).await {
                Ok(colors) if !colors.is_empty() => {
                    let _ = tx.send(super::DocumentColorOutcome::Items {
                        buffer_id,
                        document_version: version,
                        server_id: server_id_arc,
                        colors,
                    });
                }
                _ => {
                    let _ = tx.send(super::DocumentColorOutcome::Empty {
                        buffer_id,
                        document_version: version,
                    });
                }
            }
        });
    }

    /// 4.5.e: drain queued `documentColor` responses + seat
    /// the cache.
    pub fn drain_pending_document_color(&mut self) {
        let Some(mut rx) = self.editor.pending_document_color_rx.take() else {
            return;
        };
        let mut latest: Option<super::DocumentColorOutcome> = None;
        while let Ok(o) = rx.try_recv() {
            latest = Some(o);
        }
        self.editor.pending_document_color_rx = Some(rx);
        let outcome = match latest {
            Some(o) => o,
            None => return,
        };
        self.editor.pending_document_color_token = None;
        match outcome {
            super::DocumentColorOutcome::Items {
                buffer_id,
                document_version,
                server_id,
                colors,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                self.editor.lsp_document_color_cache.insert(
                    buffer_id,
                    super::LspDocumentColorCache {
                        document_version,
                        colors,
                        server_id,
                    },
                );
            }
            super::DocumentColorOutcome::Empty {
                buffer_id,
                document_version,
            } => {
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                self.editor.lsp_document_color_cache.insert(
                    buffer_id,
                    super::LspDocumentColorCache {
                        document_version,
                        colors: Vec::new(),
                        server_id: std::sync::Arc::from("<none>"),
                    },
                );
            }
        }
    }

    /// 4.5.e: `:lsp-color-presentation`. Looks up the color
    /// literal under the cursor in the per-buffer cache and
    /// fires `textDocument/colorPresentation` to get the
    /// alternative formats; opens the result as a picker.
    /// Accept replaces the literal with the chosen
    /// `ColorPresentation.text_edit` (or `label` fallback) at
    /// the literal's range.
    pub(super) fn do_lsp_color_presentation(&mut self) {
        let snapshot = self.document.snapshot();
        let Some(pos) = app_to_lsp_position(&snapshot.buffer, self.editor.cursor) else {
            return;
        };
        let buffer_id = self.editor.document_buffer_id;
        let cache = self.editor.lsp_document_color_cache.get(&buffer_id).cloned();
        let Some(cache) = cache else {
            self.set_message(
                EchoLevel::Info,
                "no color literal at cursor (cache empty)".to_string(),
            );
            return;
        };
        let entry = cache
            .colors
            .iter()
            .find(|c| range_covers(c.range, pos))
            .cloned();
        let Some(entry) = entry else {
            self.set_message(EchoLevel::Info, "no color literal at cursor".to_string());
            return;
        };
        let Some(uri) = self.editor.buffer_uris.get(&buffer_id).cloned() else {
            return;
        };
        let handle = self
            .editor.lsp
            .servers_for(&uri)
            .into_iter()
            .find(|h| h.server_id() == cache.server_id.as_ref());
        let Some(handle) = handle else {
            self.set_message(
                EchoLevel::Warn,
                format!(
                    "color-presentation: server `{}` no longer attached",
                    cache.server_id
                ),
            );
            return;
        };
        let params = lsp_types::ColorPresentationParams {
            text_document: lsp_types::TextDocumentIdentifier { uri },
            color: entry.color,
            range: entry.range,
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let token = lattice_protocol::CancellationToken::new();
        let presentations =
            lattice_runtime::block_on(
                async move { handle.color_presentation(params, token).await },
            );
        let presentations = match presentations {
            Ok(p) => p,
            Err(_) => {
                self.set_message(EchoLevel::Warn, "colorPresentation failed".to_string());
                return;
            }
        };
        if presentations.is_empty() {
            self.set_message(EchoLevel::Info, "no color alternatives".to_string());
            return;
        }
        let pairs: Vec<(
            lattice_completion::RawCandidate,
            lattice_picker::RoutingPayload,
        )> = presentations
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let mut c = lattice_completion::RawCandidate::plain(
                    p.label.clone(),
                    lattice_completion::CandidateKind::Plain,
                );
                c.display = p.label.clone();
                (
                    c,
                    lattice_picker::RoutingPayload::ColorPresentation { index: i as u32 },
                )
            })
            .collect();
        let total = pairs.len();
        self.editor.pending_color_presentations = Some(presentations);
        self.editor.pending_color_range = Some(entry.range);
        let mut p = lattice_picker::Picker::new(
            format!("color alternatives ({total})"),
            lattice_picker::PickerSource::LspLocations,
            lattice_picker::PickerAction::AcceptColorPresentation,
        );
        p.set_raw_candidates_with_routing(pairs);
        self.editor.picker = Some(p);
    }

    /// 4.5.e: accept one color presentation by index. Splices
    /// the chosen alternative at the cached color range. Uses
    /// `text_edit` when present; falls back to a plain replace
    /// with `label` otherwise.
    pub(super) fn accept_lsp_color_presentation(&mut self, index: u32) {
        let Some(items) = self.editor.pending_color_presentations.take() else {
            return;
        };
        let range = self.editor.pending_color_range.take();
        let Some(item) = items.get(index as usize).cloned() else {
            return;
        };
        let Some(range) = range else {
            return;
        };
        // Prefer `text_edit` (server controls the substitution
        // shape including surrounding context); fall back to a
        // plain range-replace with `label`.
        if let Some(edit) = item.text_edit {
            let _ = self.apply_lsp_text_edits(vec![edit]);
        } else {
            let edit = lsp_types::TextEdit {
                range,
                new_text: item.label,
            };
            let _ = self.apply_lsp_text_edits(vec![edit]);
        }
    }

    /// 4.5.d: drain `workspace/codeLens/refresh` events. Each
    /// event names a server; evict every cached code-lens
    /// entry that came from that server. The next pump tick
    /// re-issues `textDocument/codeLens`.
    pub fn drain_code_lens_refresh(&mut self) {
        let Some(mut rx) = self.editor.pending_code_lens_refresh_rx.take() else {
            return;
        };
        let mut servers: Vec<std::sync::Arc<str>> = Vec::new();
        while let Ok(ev) = rx.try_recv() {
            servers.push(ev.server_id);
        }
        self.editor.pending_code_lens_refresh_rx = Some(rx);
        if servers.is_empty() {
            return;
        }
        self.editor.lsp_code_lens_cache.retain(|_buf, cache| {
            !servers
                .iter()
                .any(|s| s.as_ref() == cache.server_id.as_ref())
        });
    }

    /// 4.4.e: per-tick `documentHighlight` pump. Compares the
    /// current cursor against the cache anchor; when they
    /// differ AND the sub-mode is on AND the buffer has an
    /// attached server advertising the capability, fires a
    /// fresh request (cancelling any in-flight). The drain
    /// (`drain_pending_document_highlight`) seats the response
    /// into `lsp_document_highlights`.
    ///
    /// Self-cancelling: the cursor moves faster than the
    /// network round-trip during a `/word` search, so the
    /// `CancellationToken` invalidates every in-flight request
    /// the moment the next one fires. Only the latest response
    /// ever lands in the cache.
    pub fn maybe_request_document_highlight(&mut self) {
        if !self.lsp_document_highlight_mode_enabled_for(self.editor.document_buffer_id) {
            // Mode off: clear stale state so the overlay
            // disappears the moment the user disables the mode.
            self.editor.lsp_document_highlights = None;
            self.editor.last_document_highlight_issue_cursor = None;
            if let Some(token) = self.editor.pending_document_highlight_token.take() {
                token.cancel();
            }
            return;
        }
        if self.editor.last_document_highlight_issue_cursor == Some(self.editor.cursor) {
            return;
        }
        // Invalidate stale cache if it belonged to a different
        // buffer.
        if let Some(cache) = self.editor.lsp_document_highlights.as_ref()
            && cache.buffer_id != self.editor.document_buffer_id
        {
            self.editor.lsp_document_highlights = None;
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            return;
        };
        let snapshot = self.document.snapshot();
        let Some(position) = crate::app::app_to_lsp_position(&snapshot.buffer, self.editor.cursor) else {
            return;
        };
        // Cancel any in-flight request so its response is
        // dropped if it lands after this one is issued.
        if let Some(token) = self.editor.pending_document_highlight_token.take() {
            token.cancel();
        }
        let buffer_id = self.editor.document_buffer_id;
        let anchor_cursor = self.editor.cursor;
        self.editor.last_document_highlight_issue_cursor = Some(anchor_cursor);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::DocumentHighlightOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_document_highlight_rx = Some(rx);
        self.editor.pending_document_highlight_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            let Some(handle) = handles
                .into_iter()
                .find(|h| h.capabilities().supports_document_highlight())
            else {
                let _ = tx.send(super::DocumentHighlightOutcome::Empty { buffer_id });
                return;
            };
            let params = lsp_types::DocumentHighlightParams {
                text_document_position_params: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            };
            match handle.document_highlight(params, token.clone()).await {
                Ok(Some(highlights)) if !highlights.is_empty() => {
                    let _ = tx.send(super::DocumentHighlightOutcome::Items {
                        buffer_id,
                        cursor: anchor_cursor,
                        highlights,
                    });
                }
                _ => {
                    let _ = tx.send(super::DocumentHighlightOutcome::Empty { buffer_id });
                }
            }
        });
    }

    /// 4.4.e: drain the in-flight `documentHighlight` response
    /// (if any). Coalesces multiple queued outcomes to the
    /// latest one so a burst of cursor moves only commits the
    /// final response.
    pub fn drain_pending_document_highlight(&mut self) {
        let Some(mut rx) = self.editor.pending_document_highlight_rx.take() else {
            return;
        };
        let mut latest: Option<super::DocumentHighlightOutcome> = None;
        while let Ok(outcome) = rx.try_recv() {
            latest = Some(outcome);
        }
        self.editor.pending_document_highlight_rx = Some(rx);
        let Some(outcome) = latest else {
            return;
        };
        match outcome {
            super::DocumentHighlightOutcome::Items {
                buffer_id,
                cursor,
                highlights,
            } => {
                // Guard against stale responses that arrived
                // after the active buffer switched out from
                // under the request.
                if buffer_id != self.editor.document_buffer_id {
                    return;
                }
                self.editor.lsp_document_highlights = Some(super::DocumentHighlightCache {
                    buffer_id,
                    cursor,
                    highlights,
                });
            }
            super::DocumentHighlightOutcome::Empty { buffer_id } => {
                if buffer_id == self.editor.document_buffer_id {
                    self.editor.lsp_document_highlights = None;
                }
            }
        }
    }

    /// 4.4.e: `:lsp-expand-region` -- structural smart-
    /// expansion. If a cached chain still applies (cursor sits
    /// inside its innermost range AND same buffer), step the
    /// index outward and apply the new selection. Otherwise
    /// fire `textDocument/selectionRange` and let the drain
    /// seat the chain + apply step 0 on completion.
    pub fn do_lsp_expand_region(&mut self) {
        if self.try_step_cached_region(super::SelectionRangeStep::Expand) {
            return;
        }
        self.issue_selection_range_request(super::SelectionRangeStep::Expand);
    }

    /// 4.4.e: `:lsp-shrink-region` -- step inward inside the
    /// cached chain. With no cache (e.g. user invoked shrink
    /// first), echo + bail; with the cursor at index 0, exit
    /// Visual mode.
    pub fn do_lsp_shrink_region(&mut self) {
        if self.try_step_cached_region(super::SelectionRangeStep::Shrink) {
            return;
        }
        // No active chain to shrink -- user likely invoked
        // shrink before expand. Echo + bail rather than issuing
        // a fresh request (a shrink without prior expand has no
        // meaningful step to take).
        self.set_message(
            EchoLevel::Info,
            "lsp-shrink-region: no active expansion to shrink".to_string(),
        );
    }

    /// Returns `true` when the cached chain was usable for the
    /// requested step (and the selection has been applied).
    /// `false` means the caller must fall back to issuing a
    /// fresh request (expand) or surfacing an error (shrink).
    fn try_step_cached_region(&mut self, step: super::SelectionRangeStep) -> bool {
        let Some(chain) = self.editor.lsp_selection_chain.as_ref() else {
            return false;
        };
        if chain.buffer_id != self.editor.document_buffer_id {
            self.editor.lsp_selection_chain = None;
            self.editor.lsp_selection_chain_index = 0;
            return false;
        }
        // Cache is anchored at the original cursor. If the
        // cursor has wandered outside the innermost range we
        // invalidate -- the chain belonged to a different
        // symbol position.
        let inside = chain
            .ranges
            .first()
            .is_some_and(|r| crate::app::cursor_inside_range(self.editor.cursor, r));
        if !inside {
            self.editor.lsp_selection_chain = None;
            self.editor.lsp_selection_chain_index = 0;
            return false;
        }
        match step {
            super::SelectionRangeStep::Expand => {
                let next = self.editor.lsp_selection_chain_index + 1;
                if next >= chain.ranges.len() {
                    self.set_message(EchoLevel::Info, "lsp-expand-region: outermost".to_string());
                    return true;
                }
                self.editor.lsp_selection_chain_index = next;
            }
            super::SelectionRangeStep::Shrink => {
                if self.editor.lsp_selection_chain_index == 0 {
                    // Collapse to cursor; exit Visual.
                    self.do_exit_visual();
                    return true;
                }
                self.editor.lsp_selection_chain_index -= 1;
            }
        }
        self.apply_selection_chain_step();
        true
    }

    fn apply_selection_chain_step(&mut self) {
        let Some(chain) = self.editor.lsp_selection_chain.as_ref() else {
            return;
        };
        let Some(range) = chain.ranges.get(self.editor.lsp_selection_chain_index).cloned() else {
            return;
        };
        let snapshot = self.document.snapshot();
        let anchor = lattice_protocol::Position {
            line: range.start.line,
            byte: crate::app::lsp_position_to_app_byte(
                &snapshot.buffer,
                range.start.line,
                range.start.character,
            ),
        };
        let head = lattice_protocol::Position {
            line: range.end.line,
            byte: crate::app::lsp_position_to_app_byte(
                &snapshot.buffer,
                range.end.line,
                range.end.character,
            ),
        };
        // Apply via Visual-mode plumbing: enter Visual if not
        // already, then seat the selection with anchor/head.
        use lattice_grammar::{ModalState, VisualKind};
        use lattice_protocol::selection::{Selection, SelectionSet, VisualMode};
        self.editor.modal = ModalState::Visual(VisualKind::Charwise);
        self.editor.visual_anchor = Some(anchor);
        // The cursor lands on the head; the head sits *inside*
        // the range (LSP ranges are half-open).
        self.editor.cursor = head;
        let sel = Selection {
            anchor,
            head,
            visual: Some(VisualMode::Charwise),
        };
        self.set_selections_blocking(SelectionSet::single(sel));
    }

    fn issue_selection_range_request(&mut self, step: super::SelectionRangeStep) {
        // M.6.2: lsp-selection-range-mode gate.
        if !self.check_lsp_sub_mode_gate(
            lattice_lsp::modes::LspSelectionRangeMode::mode_id(),
            "lsp-selection-range-mode",
        ) {
            return;
        }
        // Cancel any in-flight request first so the prior
        // response can't seat a stale chain.
        if let Some(token) = self.editor.pending_selection_range_token.take() {
            token.cancel();
        }
        let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
            self.set_message(
                EchoLevel::Info,
                "no LSP server attached to current buffer".to_string(),
            );
            return;
        };
        let snapshot = self.document.snapshot();
        let position = match crate::app::app_to_lsp_position(&snapshot.buffer, self.editor.cursor) {
            Some(p) => p,
            None => {
                self.set_message(
                    EchoLevel::Error,
                    "lsp-expand-region: cursor out of buffer".to_string(),
                );
                return;
            }
        };
        let anchor_cursor = self.editor.cursor;
        let anchor_buffer = self.editor.document_buffer_id;
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::SelectionRangeOutcome>();
        let token = lattice_protocol::CancellationToken::new();
        self.editor.pending_selection_range_rx = Some(rx);
        self.editor.pending_selection_range_token = Some(token.clone());
        let lsp = self.editor.lsp.clone();
        crate::runtime::spawn_on_lsp_runtime(async move {
            let handles: Vec<lattice_lsp::ServerHandle> = { lsp.servers_for(&uri) };
            let Some(handle) = handles
                .into_iter()
                .find(|h| h.capabilities().supports_selection_range())
            else {
                let _ = tx.send(super::SelectionRangeOutcome::NoProvider);
                return;
            };
            let params = lsp_types::SelectionRangeParams {
                text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                positions: vec![position],
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            };
            match handle.selection_range(params, token.clone()).await {
                Ok(Some(ranges)) if !ranges.is_empty() => {
                    let flat = super::flatten_selection_range_chain(&ranges[0]);
                    if flat.is_empty() {
                        let _ = tx.send(super::SelectionRangeOutcome::Empty);
                        return;
                    }
                    let _ = tx.send(super::SelectionRangeOutcome::Items {
                        anchor_cursor,
                        anchor_buffer,
                        ranges: flat,
                        pending_step: step,
                    });
                }
                _ => {
                    let _ = tx.send(super::SelectionRangeOutcome::Empty);
                }
            }
        });
    }

    /// 4.4.e: drain the in-flight `selectionRange` response.
    /// Seats the chain into `App::lsp_selection_chain` and
    /// applies the step the original invocation requested.
    pub fn drain_pending_selection_range(&mut self) {
        let Some(mut rx) = self.editor.pending_selection_range_rx.take() else {
            return;
        };
        // Coalesce: only the most recent outcome wins. A user
        // that mashes `:lsp-expand-region` faster than the
        // server can respond would otherwise step through the
        // older chain before the newer one lands.
        let mut latest: Option<super::SelectionRangeOutcome> = None;
        while let Ok(outcome) = rx.try_recv() {
            latest = Some(outcome);
        }
        self.editor.pending_selection_range_rx = Some(rx);
        let Some(outcome) = latest else {
            return;
        };
        match outcome {
            super::SelectionRangeOutcome::Items {
                anchor_cursor,
                anchor_buffer,
                ranges,
                pending_step,
            } => {
                self.editor.lsp_selection_chain = Some(super::LspSelectionChain {
                    buffer_id: anchor_buffer,
                    anchor_cursor,
                    ranges,
                });
                // Always start at index 0 (innermost); for an
                // expand-triggered fetch we then bump to 1 so
                // the user sees the first widening; for a
                // shrink-triggered fetch we leave at 0 (the
                // shrink-without-cache case is the user's bail
                // path).
                self.editor.lsp_selection_chain_index = match pending_step {
                    super::SelectionRangeStep::Expand => {
                        let chain = self.editor.lsp_selection_chain.as_ref().unwrap();
                        if chain.ranges.len() > 1 { 1 } else { 0 }
                    }
                    super::SelectionRangeStep::Shrink => 0,
                };
                self.apply_selection_chain_step();
            }
            super::SelectionRangeOutcome::NoProvider => {
                self.set_message(
                    EchoLevel::Info,
                    "lsp-expand-region: no server advertises selectionRange".to_string(),
                );
            }
            super::SelectionRangeOutcome::Empty => {
                self.set_message(
                    EchoLevel::Info,
                    "lsp-expand-region: server returned no ranges".to_string(),
                );
            }
        }
    }

    /// active progress entry (4.4.c). With `server_id == Some`,
    /// cancel only entries on that server; with `None`, cancel
    /// across every server attached to the current buffer.
    ///
    /// The entry stays in the accumulator until the server sends
    /// the `end` progress notification — `cancel` is best-effort
    /// per spec, and the server may decline.
    pub fn do_lsp_progress_cancel(&mut self, server_id: Option<&str>) {
        // Filter to attached servers when no explicit server is
        // given: we don't want `:lsp-progress-cancel` from a
        // buffer with no LSP attachment to fire cancels for a
        // background indexer the user can't see.
        let allowed: std::collections::HashSet<String> = match server_id {
            Some(id) => std::iter::once(id.to_string()).collect(),
            None => {
                let Some(uri) = self.editor.buffer_uris.get(&self.editor.document_buffer_id).cloned() else {
                    self.set_message(
                        EchoLevel::Info,
                        "lsp-progress-cancel: buffer has no URI".to_string(),
                    );
                    return;
                };
                self.editor.lsp
                    .servers_for(&uri)
                    .into_iter()
                    .map(|h| h.server_id().to_string())
                    .collect()
            }
        };
        if allowed.is_empty() {
            self.set_message(
                EchoLevel::Info,
                "lsp-progress-cancel: no attached servers".to_string(),
            );
            return;
        }
        // Build a map from server_id -> handles once so we don't
        // walk `running_actors()` per token.
        let mut handles_by_id: std::collections::HashMap<String, lattice_lsp::ServerHandle> =
            std::collections::HashMap::new();
        for (_key, h) in self.editor.lsp.running_actors() {
            let id = h.server_id().to_string();
            if allowed.contains(&id) {
                handles_by_id.insert(id, h);
            }
        }
        let mut sent = 0usize;
        let mut skipped_non_cancellable = 0usize;
        for ((sid, token), update) in &self.editor.lsp_progress {
            if !allowed.contains(sid.as_ref()) {
                continue;
            }
            if !update.cancellable {
                skipped_non_cancellable += 1;
                continue;
            }
            if matches!(update.kind, lattice_lsp::LspProgressKind::End) {
                continue;
            }
            if let Some(handle) = handles_by_id.get(sid.as_ref()) {
                let _ = handle.cancel_progress(token);
                sent += 1;
            }
        }
        let scope = match server_id {
            Some(id) => format!(" on {id}"),
            None => String::new(),
        };
        if sent == 0 && skipped_non_cancellable == 0 {
            self.set_message(
                EchoLevel::Info,
                format!("lsp-progress-cancel: no active progress{scope}"),
            );
        } else {
            self.set_message(
                EchoLevel::Info,
                format!(
                    "lsp-progress-cancel{scope}: sent {sent}, skipped {skipped_non_cancellable} non-cancellable"
                ),
            );
        }
    }

    /// `:lsp-log-level [server] <level>` -- set the subsystem
    /// default min level (when no server) or a per-server
    /// override.
    pub fn do_set_lsp_log_level(&mut self, server_id: Option<&str>, level: &str) {
        let Some(parsed) = lattice_lsp::LogLevel::parse(level) else {
            self.set_message(
                EchoLevel::Error,
                format!("unknown log level {level:?}; expected error/warn/info/debug/trace"),
            );
            return;
        };
        match server_id {
            None => {
                self.editor.lsp_logger.set_default_level(parsed);
                self.set_message(EchoLevel::Info, format!("lsp default log level: {level}"));
            }
            Some(id) => {
                // B'.2: apply the per-instance override to every
                // known instance whose server_id matches. The
                // logger's `known_instances` covers both running
                // actors and any instance with a non-empty ring
                // (e.g. a server that exited but whose buffer
                // is still open). With a single matching instance
                // this matches the pre-B'.2 per-server behaviour.
                let mut applied = 0usize;
                let targets: Vec<lattice_lsp::InstanceKey> = self
                    .editor.lsp_logger
                    .known_instances()
                    .into_iter()
                    .filter(|k| k.server_id.as_ref() == id)
                    .collect();
                // Augment with running actors that don't yet have
                // an entry in the logger (no records emitted yet).
                let mut seen: std::collections::HashSet<lattice_lsp::InstanceKey> =
                    targets.iter().cloned().collect();
                for (_key, handle) in self.editor.lsp.running_actors() {
                    if handle.server_id() != id {
                        continue;
                    }
                    let inst = handle.instance();
                    if seen.insert(inst.clone()) {
                        // Newly-discovered; will be applied below.
                    }
                }
                for inst in seen {
                    self.editor.lsp_logger.set_instance_level(inst, Some(parsed));
                    applied += 1;
                }
                // If nothing matched, still record a synthetic
                // instance against cwd so the pre-spawn case
                // (set level before the actor starts) works.
                if applied == 0 {
                    let synth = lattice_lsp::InstanceKey::new(
                        std::sync::Arc::<str>::from(id),
                        std::sync::Arc::<std::path::Path>::from(
                            std::env::current_dir()
                                .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                                .as_path(),
                        ),
                    );
                    self.editor.lsp_logger.set_instance_level(synth, Some(parsed));
                    applied = 1;
                }
                self.set_message(
                    EchoLevel::Info,
                    format!("lsp log level for {id}: {level} ({applied} instance(s))"),
                );
            }
        }
    }

    /// `:lsp-log-clear [server]` -- drop ring contents.
    pub fn do_lsp_log_clear(&mut self, server_id: Option<&str>) {
        match server_id {
            None => {
                self.editor.lsp_logger.clear_global();
                self.set_message(EchoLevel::Info, "*lsp* cleared".to_string());
            }
            Some(id) => {
                // B'.2: clear every known instance whose
                // server_id matches. Covers running actors,
                // exited-but-buffer-still-open instances, and
                // synthetic test instances.
                let targets: Vec<lattice_lsp::InstanceKey> = self
                    .editor.lsp_logger
                    .known_instances()
                    .into_iter()
                    .filter(|k| k.server_id.as_ref() == id)
                    .collect();
                let cleared = targets.len();
                for inst in targets {
                    self.editor.lsp_logger.clear_instance(&inst);
                }
                self.set_message(
                    EchoLevel::Info,
                    format!("*lsp:{id}* cleared ({cleared} instance(s))"),
                );
            }
        }
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
        let instance = self.resolve_lsp_instance_for(server_id);
        let name = lattice_lsp::lsp_server_log_name(&instance);
        let id = self.ensure_named_synthetic_document(
            &name,
            lattice_lsp::modes::LspServerLogMode::mode_id(),
            Self::SYNTHETIC_BUFFER_FLAGS,
        );
        self.activate_buffer(id);
    }

    /// Activate the per-instance
    /// `*lsp:<server_id>:<workspace>:trace*` Document buffer in
    /// the active pane.
    pub(super) fn open_lsp_trace_log_in_pane(&mut self, server_id: &str) {
        let instance = self.resolve_lsp_instance_for(server_id);
        let name = lattice_lsp::lsp_server_trace_log_name(&instance);
        let id = self.ensure_named_synthetic_document(
            &name,
            lattice_lsp::modes::LspTraceLogMode::mode_id(),
            Self::SYNTHETIC_BUFFER_FLAGS,
        );
        self.activate_buffer(id);
    }

    /// Pick an `InstanceKey` for `server_id`: prefer a running
    /// actor's instance, then any known-by-logger instance,
    /// then synthesise a cwd-based instance as a last resort
    /// (matches the fallback used by `:lsp-trace` / `:lsp-log-level`).
    fn resolve_lsp_instance_for(&self, server_id: &str) -> lattice_lsp::InstanceKey {
        for (_key, handle) in self.editor.lsp.running_actors() {
            if handle.server_id() == server_id {
                return handle.instance();
            }
        }
        for inst in self.editor.lsp_logger.known_instances() {
            if inst.server_id.as_ref() == server_id {
                return inst;
            }
        }
        lattice_lsp::InstanceKey::new(
            std::sync::Arc::<str>::from(server_id),
            std::sync::Arc::<std::path::Path>::from(
                std::env::current_dir()
                    .unwrap_or_else(|_| std::path::PathBuf::from("/"))
                    .as_path(),
            ),
        )
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
        for ((_, sid), _) in self.editor.lsp.running_actors() {
            if sid == name {
                return Some(sid);
            }
        }
        for cfg in self.editor.lsp.configs() {
            if cfg.id == name {
                return Some(cfg.id.clone());
            }
            let file = cfg
                .binary
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("");
            let stem = file.trim_end_matches(".exe");
            if file == name || stem == name {
                return Some(cfg.id.clone());
            }
        }
        None
    }

    /// Distinct server ids of every running actor. Used in echo
    /// messages so the user sees what's available.
    pub(super) fn running_server_ids(&self) -> Vec<String> {
        let mut ids: Vec<String> = self
            .editor.lsp
            .running_actors()
            .into_iter()
            .map(|((_, sid), _)| sid)
            .collect();
        ids.sort();
        ids.dedup();
        ids
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
        let id = a.pane_tree.active().buffer_id;
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
        a.do_lsp_definition_request();
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
        let id = a.pane_tree.active().buffer_id;
        assert!(!a.lsp_mode_enabled_for(id));
    }

    #[test]
    fn lsp_mode_enabled_for_tracks_minor_activation() {
        // M.5.0: activating `lsp-mode` through the registry
        // flips the accessor. M.5.3 will wrap this in actual
        // `:lsp-mode` toggle / auto-activation flow; for now
        // we drive the registry directly.
        let mut a = app_with("fn main() {}", 5);
        let id = a.pane_tree.active().buffer_id;
        let proto_id = lattice_protocol::ids::BufferId::new(id.0 as u64);
        let mut active = a.editor.active_modes.remove(&id).unwrap_or_default();
        a.editor.mode_registry
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
        let m = lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(
            "fn foo() -> u32".into(),
        ));
        assert_eq!(super::hover_contents_to_markdown(&m), "fn foo() -> u32");
    }

    #[test]
    fn hover_contents_language_string_renders_as_fenced_block() {
        let m = lsp_types::HoverContents::Scalar(lsp_types::MarkedString::LanguageString(
            lsp_types::LanguageString {
                language: "rust".into(),
                value: "let x: u32 = 5;".into(),
            },
        ));
        let md = super::hover_contents_to_markdown(&m);
        assert!(md.contains("```rust"));
        assert!(md.contains("let x: u32 = 5;"));
        assert!(md.ends_with("```"));
    }

    #[test]
    fn hover_contents_array_joins_with_double_newline() {
        let m = lsp_types::HoverContents::Array(vec![
            lsp_types::MarkedString::String("first".into()),
            lsp_types::MarkedString::String("second".into()),
        ]);
        let md = super::hover_contents_to_markdown(&m);
        assert_eq!(md, "first\n\nsecond");
    }

    #[test]
    fn hover_contents_markup_uses_value_as_markdown() {
        let m = lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
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
        let msg = a.editor.last_message.as_ref().expect("echo on no-hover-info");
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
            .editor.last_message
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
        let outer = lsp_types::SelectionRange {
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 5,
                    character: 0,
                },
            },
            parent: None,
        };
        let middle = lsp_types::SelectionRange {
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 1,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 3,
                    character: 0,
                },
            },
            parent: Some(Box::new(outer)),
        };
        let inner = lsp_types::SelectionRange {
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 2,
                    character: 0,
                },
                end: lsp_types::Position {
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
            lsp_types::SemanticTokenType::KEYWORD,
            lsp_types::SemanticTokenType::FUNCTION,
        ];
        let token_modifiers = vec![
            lsp_types::SemanticTokenModifier::STATIC,
            lsp_types::SemanticTokenModifier::READONLY,
        ];
        // Three tokens:
        //  - Line 0, col 0, len 3, type=keyword, no mods.
        //  - Line 0, col 4, len 4, type=function, mod bit 0 (static).
        //  - Line 2, col 2, len 1, type=keyword, mod bits 0+1.
        let data = vec![
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 3,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 4,
                token_type: 1,
                token_modifiers_bitset: 0b01,
            },
            lsp_types::SemanticToken {
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
        let token_types = vec![lsp_types::SemanticTokenType::KEYWORD];
        let token_modifiers: Vec<lsp_types::SemanticTokenModifier> = Vec::new();
        let data = vec![
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 3,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lsp_types::SemanticToken {
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
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 2,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 4,
                token_type: 1,
                token_modifiers_bitset: 0,
            },
            lsp_types::SemanticToken {
                delta_line: 1,
                delta_start: 2,
                length: 1,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
        ];
        // Replace the middle token (index 1) with two new tokens.
        let edit = lsp_types::SemanticTokensEdit {
            start: 1,
            delete_count: 1,
            data: Some(vec![
                lsp_types::SemanticToken {
                    delta_line: 0,
                    delta_start: 3,
                    length: 2,
                    token_type: 2,
                    token_modifiers_bitset: 0,
                },
                lsp_types::SemanticToken {
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
        let mut raw = vec![lsp_types::SemanticToken {
            delta_line: 0,
            delta_start: 0,
            length: 2,
            token_type: 0,
            token_modifiers_bitset: 0,
        }];
        let edit = lsp_types::SemanticTokensEdit {
            start: 1,
            delete_count: 0,
            data: Some(vec![lsp_types::SemanticToken {
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
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 2,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 4,
                token_type: 1,
                token_modifiers_bitset: 0,
            },
        ];
        let edit = lsp_types::SemanticTokensEdit {
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
        let mut raw = vec![lsp_types::SemanticToken {
            delta_line: 0,
            delta_start: 0,
            length: 2,
            token_type: 0,
            token_modifiers_bitset: 0,
        }];
        let edit = lsp_types::SemanticTokensEdit {
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
        let mut a = app_with("fn main() {}\n", 5);
        let buffer_id = a.editor.document_buffer_id;
        // Seed an initial cache entry with one keyword token and
        // a `result_id` the delta will reference. Use a single
        // legend entry so the decoder's name resolution is
        // deterministic.
        let initial_raw = vec![lsp_types::SemanticToken {
            delta_line: 0,
            delta_start: 0,
            length: 2,
            token_type: 0,
            token_modifiers_bitset: 0,
        }];
        a.editor.lsp_semantic_tokens_cache.insert(
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
        // Send a Delta outcome that appends a new "function"
        // token. The drain should splice into raw_data and
        // re-decode against the carried legend.
        let token_types = vec![
            lsp_types::SemanticTokenType::KEYWORD,
            lsp_types::SemanticTokenType::FUNCTION,
        ];
        let token_modifiers: Vec<lsp_types::SemanticTokenModifier> = Vec::new();
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<crate::app::SemanticTokensOutcome>();
        tx.send(crate::app::SemanticTokensOutcome::Delta {
            buffer_id,
            document_version: 2,
            previous_result_id: "r1".into(),
            new_result_id: Some("r2".into()),
            edits: vec![lsp_types::SemanticTokensEdit {
                start: 1,
                delete_count: 0,
                data: Some(vec![lsp_types::SemanticToken {
                    delta_line: 0,
                    delta_start: 3,
                    length: 4,
                    token_type: 1,
                    token_modifiers_bitset: 0,
                }]),
            }],
            token_types,
            token_modifiers,
        })
        .expect("send delta");
        a.editor.pending_semantic_tokens_rx = Some(rx);
        a.drain_pending_semantic_tokens();
        let cache = a
            .editor.lsp_semantic_tokens_cache
            .get(&buffer_id)
            .expect("cache seated");
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
        let mut a = app_with("fn main() {}\n", 5);
        let buffer_id = a.editor.document_buffer_id;
        a.editor.lsp_semantic_tokens_cache.insert(
            buffer_id,
            crate::app::LspSemanticTokensCache {
                document_version: 1,
                result_id: Some("different-id".into()),
                raw_data: Vec::new(),
                tokens: Vec::new(),
            },
        );
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<crate::app::SemanticTokensOutcome>();
        tx.send(crate::app::SemanticTokensOutcome::Delta {
            buffer_id,
            document_version: 2,
            previous_result_id: "r1".into(),
            new_result_id: Some("r2".into()),
            edits: Vec::new(),
            token_types: Vec::new(),
            token_modifiers: Vec::new(),
        })
        .expect("send delta");
        a.editor.pending_semantic_tokens_rx = Some(rx);
        a.drain_pending_semantic_tokens();
        assert!(
            a.editor.lsp_semantic_tokens_cache.get(&buffer_id).is_none(),
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
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 2,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 4,
                token_type: 1,
                token_modifiers_bitset: 0,
            },
        ];
        let edits = vec![
            // Delete index 0.
            lsp_types::SemanticTokensEdit {
                start: 0,
                delete_count: 1,
                data: None,
            },
            // Then insert a new token at the (now) start.
            lsp_types::SemanticTokensEdit {
                start: 0,
                delete_count: 0,
                data: Some(vec![lsp_types::SemanticToken {
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
        let mut app = app_with("fn main() {}\n", 5);
        let buffer_id = app.editor.document_buffer_id;
        let uri = lattice_lsp::Uri::from_str("file:///tmp/x.rs").unwrap();
        app.editor.buffer_uris.insert(buffer_id, uri.clone());
        let server_id: std::sync::Arc<str> = std::sync::Arc::from("rust");
        let diag = lsp_types::Diagnostic {
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 0,
                    character: 2,
                },
            },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: None,
            message: "boom".into(),
            related_information: None,
            tags: None,
            data: None,
        };
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<crate::app::PullDiagnosticsOutcome>();
        tx.send(crate::app::PullDiagnosticsOutcome::Full {
            buffer_id,
            server_id: server_id.clone(),
            uri: uri.clone(),
            document_version: 1,
            result_id: Some("r1".into()),
            diagnostics: vec![diag.clone()],
        })
        .expect("send full");
        app.editor.pending_pull_diagnostics_rx = Some(rx);
        app.drain_pending_pull_diagnostics();
        let cache = app
            .editor.lsp_pull_diagnostics_cache
            .get(&buffer_id)
            .expect("cache seated");
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
        let mut app = app_with("fn main() {}\n", 5);
        let buffer_id = app.editor.document_buffer_id;
        // Seed initial cache state.
        app.editor.lsp_pull_diagnostics_cache.insert(
            buffer_id,
            crate::app::LspPullDiagnosticsCache {
                document_version: 1,
                result_id: Some("r1".into()),
            },
        );
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<crate::app::PullDiagnosticsOutcome>();
        tx.send(crate::app::PullDiagnosticsOutcome::Unchanged {
            buffer_id,
            document_version: 2,
            result_id: "r2".into(),
        })
        .expect("send unchanged");
        app.editor.pending_pull_diagnostics_rx = Some(rx);
        app.drain_pending_pull_diagnostics();
        let cache = app
            .editor.lsp_pull_diagnostics_cache
            .get(&buffer_id)
            .expect("cache seated");
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
        app.editor.lsp_pull_diagnostics_cache.insert(
            buffer_id,
            crate::app::LspPullDiagnosticsCache {
                document_version: 1,
                result_id: Some("r1".into()),
            },
        );
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspDiagnosticRefresh>();
        tx.send(lattice_lsp::LspDiagnosticRefresh {
            server_id: std::sync::Arc::from("rust"),
        })
        .expect("send refresh");
        app.editor.pending_diagnostic_refresh_rx = Some(rx);
        app.drain_diagnostic_refresh();
        // No actors attached -> no eviction.
        assert!(app.editor.lsp_pull_diagnostics_cache.contains_key(&buffer_id));
    }

    /// 4.4.j: pump short-circuits when the cache's
    /// document_version matches the current snapshot. No
    /// request fires.
    #[test]
    fn pull_diagnostics_pump_skips_when_version_unchanged() {
        let mut app = app_with("fn main() {}\n", 5);
        let buffer_id = app.editor.document_buffer_id;
        let version = app.document.snapshot().version;
        app.editor.lsp_pull_diagnostics_cache.insert(
            buffer_id,
            crate::app::LspPullDiagnosticsCache {
                document_version: version,
                result_id: Some("r1".into()),
            },
        );
        app.maybe_request_pull_diagnostics();
        assert!(
            app.editor.pending_pull_diagnostics_rx.is_none(),
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
        app.editor.buffer_uris.insert(app.editor.document_buffer_id, uri);
        // Seed cache with a wide range covering 0..=1000.
        app.editor.lsp_inlay_hints_cache.insert(
            app.editor.document_buffer_id,
            crate::app::LspInlayHintCache {
                document_version: app.document.snapshot().version,
                hints: Vec::new(),
                requested_first_line: 0,
                requested_last_line: 1000,
            },
        );
        // Viewport at line 0, height 5 (set by app_with) --
        // comfortably inside the cached range. Avoid calling
        // `set_viewport_height` here: it triggers
        // `ensure_cursor_visible` which would clamp scroll
        // back to the cursor line and mask the viewport state
        // the test wants to exercise.
        app.editor.scroll = 0;
        app.maybe_request_inlay_hint();
        assert!(
            app.editor.pending_inlay_hint_rx.is_none(),
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
        app.editor.buffer_uris.insert(app.editor.document_buffer_id, uri);
        // Cached range: lines 0..=200.
        app.editor.lsp_inlay_hints_cache.insert(
            app.editor.document_buffer_id,
            crate::app::LspInlayHintCache {
                document_version: app.document.snapshot().version,
                hints: Vec::new(),
                requested_first_line: 0,
                requested_last_line: 200,
            },
        );
        // Viewport now far below the cached range. Skip
        // `set_viewport_height` -- it calls
        // `ensure_cursor_visible` which would snap scroll
        // back to the cursor line.
        app.editor.scroll = 1500;
        app.maybe_request_inlay_hint();
        assert!(
            app.editor.pending_inlay_hint_rx.is_some(),
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
        app.editor.buffer_uris.insert(app.editor.document_buffer_id, uri);
        // Cache covers lines 100..=400 (the overscan-padded
        // window the pump would have fetched at scroll=200).
        app.editor.lsp_inlay_hints_cache.insert(
            app.editor.document_buffer_id,
            crate::app::LspInlayHintCache {
                document_version: app.document.snapshot().version,
                hints: Vec::new(),
                requested_first_line: 100,
                requested_last_line: 400,
            },
        );
        // Scroll a bit -- still well within the cached window.
        // Same `set_viewport_height` caveat as above.
        app.editor.scroll = 250;
        app.maybe_request_inlay_hint();
        assert!(
            app.editor.pending_inlay_hint_rx.is_none(),
            "small scroll inside cached window should not refetch",
        );
    }

    /// matching entries), and a stable identity hash.
    #[test]
    fn folding_range_to_fold_preserves_extents_and_keys_identity() {
        let r = lsp_types::FoldingRange {
            start_line: 2,
            end_line: 5,
            start_character: None,
            end_character: None,
            kind: Some(lsp_types::FoldingRangeKind::Comment),
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
        let r3 = lsp_types::FoldingRange {
            start_line: 2,
            end_line: 9,
            start_character: None,
            end_character: None,
            kind: Some(lsp_types::FoldingRangeKind::Comment),
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
        let fold = crate::app::folding_range_to_fold(lsp_types::FoldingRange {
            start_line: 0,
            end_line: 1,
            start_character: None,
            end_character: None,
            kind: None,
            collapsed_text: None,
        });
        app.editor.lsp_folds_cache.insert(
            app.editor.document_buffer_id,
            crate::app::LspFoldsCache {
                document_version: app.document.snapshot().version,
                folds: vec![fold],
            },
        );
        // Force `lsp-folding-mode` on so the cache is read
        // (the M.6.0 cascade may have left it off in test
        // setup).
        if !app.lsp_folding_mode_enabled_for(app.editor.document_buffer_id) {
            app.toggle_mode_by_name("lsp-folding-mode");
        }
        app.recompute_folds();
        assert!(
            app.folds
                .iter()
                .any(|f| f.start_line == 0 && f.end_line == 1),
            "expected LSP fold from cache; got {:?}",
            app.folds,
        );
    }

    /// 4.4.e: cursor on the start-character of a range is
    /// "inside" (half-open); cursor on `end` is outside.
    #[test]
    fn cursor_inside_range_is_half_open() {
        let r = lsp_types::Range {
            start: lsp_types::Position {
                line: 1,
                character: 4,
            },
            end: lsp_types::Position {
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

    fn fake_uri(path: &str) -> lsp_types::Uri {
        use std::str::FromStr;
        lsp_types::Uri::from_str(&format!("file://{path}")).unwrap()
    }

    fn loc(path: &str, line: u32, col: u32) -> lsp_types::Location {
        lsp_types::Location {
            uri: fake_uri(path),
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line,
                    character: col,
                },
                end: lsp_types::Position {
                    line,
                    character: col + 1,
                },
            },
        }
    }

    #[test]
    fn definition_response_scalar_flattens_to_one_location() {
        let resp = lsp_types::GotoDefinitionResponse::Scalar(loc("/x.rs", 1, 2));
        let v = super::definition_response_to_locations(resp);
        assert_eq!(v.len(), 1);
        assert_eq!(v[0].range.start.line, 1);
    }

    #[test]
    fn definition_response_array_flattens_verbatim() {
        let resp =
            lsp_types::GotoDefinitionResponse::Array(vec![loc("/a.rs", 0, 0), loc("/b.rs", 5, 5)]);
        let v = super::definition_response_to_locations(resp);
        assert_eq!(v.len(), 2);
    }

    #[test]
    fn definition_response_link_uses_target_selection_range() {
        // Link variant carries richer per-result info; we use
        // target_selection_range (narrower) for jumps.
        let link = lsp_types::LocationLink {
            origin_selection_range: None,
            target_uri: fake_uri("/x.rs"),
            target_range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 10,
                    character: 0,
                },
            },
            target_selection_range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 5,
                    character: 4,
                },
                end: lsp_types::Position {
                    line: 5,
                    character: 7,
                },
            },
        };
        let resp = lsp_types::GotoDefinitionResponse::Link(vec![link]);
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
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<Vec<lsp_types::Location>>();
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
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<Vec<lsp_types::Location>>();
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
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<Vec<lsp_types::Location>>();
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
        use lsp_types::{Location as LLoc, Position as LPos, Range as LRange};
        let path = std::path::PathBuf::from("/tmp/x.rs");
        #[allow(deprecated)]
        let syms = vec![
            lsp_types::SymbolInformation {
                name: "foo".into(),
                kind: lsp_types::SymbolKind::FUNCTION,
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
            lsp_types::SymbolInformation {
                name: "bar".into(),
                kind: lsp_types::SymbolKind::METHOD,
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
        let resp = lsp_types::DocumentSymbolResponse::Flat(syms);
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
        use lsp_types::{DocumentSymbol, Position as LPos, Range as LRange};
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
            kind: lsp_types::SymbolKind::FUNCTION,
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
            kind: lsp_types::SymbolKind::MODULE,
            tags: None,
            deprecated: None,
            range: outer_range,
            selection_range: outer_range,
            children: Some(vec![inner]),
        };
        let resp = lsp_types::DocumentSymbolResponse::Nested(vec![outer]);
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
        use lsp_types::CodeActionKind as K;
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
        let act = lsp_types::CodeAction {
            title: "Add `mut` modifier".into(),
            kind: Some(lsp_types::CodeActionKind::QUICKFIX),
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
                action: lsp_types::CodeActionOrCommand::CodeAction(act),
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
        let mut changes: HashMap<lsp_types::Uri, Vec<lsp_types::TextEdit>> = HashMap::new();
        changes.insert(
            uri.clone(),
            vec![lsp_types::TextEdit {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 3,
                    },
                },
                new_text: "bar".into(),
            }],
        );
        let we = lsp_types::WorkspaceEdit {
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
            lsp_types::TextEdit {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 4,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 7,
                    },
                },
                new_text: "bar".into(),
            },
            // Replace `foo` on line 1 col 8..11
            lsp_types::TextEdit {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 1,
                        character: 8,
                    },
                    end: lsp_types::Position {
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
        let body = a.document.snapshot().buffer.as_string();
        assert!(body.contains("let bar = 1;"));
        assert!(body.contains("let x = bar + 2;"));
        // One undo restores the pre-rename buffer (apply_lsp_text_edits
        // commits via apply_edit_batch_blocking which is one undo unit).
        let _ = a.undo_blocking();
        let restored = a.document.snapshot().buffer.as_string();
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
        a.editor.pending_insert_completion_lsp_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::InsertCompletionLspOutcome::NoServers)
            .unwrap();
        a.drain_pending_insert_completion_lsp();
        // Popup still open from sync sources.
        assert!(a.insert_completion.is_some());
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
        a.insert_completion = Some(lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            Position::new(1, 0),
            Position::new(1, 2),
            "fo".to_string(),
        ));
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::InsertCompletionLspOutcome>();
        a.editor.pending_insert_completion_lsp_rx = Some(rx);
        a.editor.pending_insert_completion_lsp_token = Some(lattice_protocol::CancellationToken::new());
        tx.send(super::InsertCompletionLspOutcome::Items {
            candidates: vec![
                lsp_meta_candidate(super::LspCompletionMeta {
                    label: "foo".into(),
                    insert_text: "foo".into(),
                    filter_text: None,
                    sort_text: None,
                    detail: Some("fn() -> i32".into()),
                    documentation: None,
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
                    resolved: false,
                }),
                lsp_meta_candidate(super::LspCompletionMeta {
                    label: "foobar".into(),
                    insert_text: "foobar".into(),
                    filter_text: None,
                    sort_text: None,
                    detail: None,
                    documentation: None,
                    kind: Some(lsp_types::CompletionItemKind::VARIABLE),
                    deprecated: false,
                    preselect: false,
                    commit_characters: Vec::new(),
                    additional_text_edits: Vec::new(),
                    command: None,
                    insert_text_format: lsp_types::InsertTextFormat::PLAIN_TEXT,
                    replace_range: None,
                    server_id: "test-server".to_string(),
                    original_item: lsp_types::CompletionItem::default(),
                    resolved: false,
                }),
            ],
            is_incomplete: false,
        })
        .unwrap();
        a.drain_pending_insert_completion_lsp();
        let state = a.insert_completion.as_ref().expect("popup open");
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
        let state = a.insert_completion.as_ref().expect("popup");
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
        a.insert_completion = Some(lattice_completion::InsertCompletionState::open(
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
            insert_text_format: lsp_types::InsertTextFormat::PLAIN_TEXT,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: lsp_types::CompletionItem::default(),
            resolved: false,
        };
        // First batch.
        let (tx1, rx1) =
            tokio::sync::mpsc::unbounded_channel::<super::InsertCompletionLspOutcome>();
        a.editor.pending_insert_completion_lsp_rx = Some(rx1);
        a.editor.pending_insert_completion_lsp_token = Some(lattice_protocol::CancellationToken::new());
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
        a.editor.pending_insert_completion_lsp_token = Some(lattice_protocol::CancellationToken::new());
        tx2.send(super::InsertCompletionLspOutcome::Items {
            candidates: vec![lsp_meta_candidate(mk_item("beta"))],
            is_incomplete: false,
        })
        .unwrap();
        a.drain_pending_insert_completion_lsp();
        let state = a.insert_completion.as_ref().expect("popup");
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
            insert_text_format: lsp_types::InsertTextFormat::PLAIN_TEXT,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: lsp_types::CompletionItem::default(),
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
        a.insert_completion = Some(state);
        // Push a resolve outcome that fills documentation.
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CompletionResolveOutcome>();
        a.editor.pending_completion_resolve_rx = Some(rx);
        a.editor.pending_completion_resolve_token = Some(lattice_protocol::CancellationToken::new());
        let mut resolved = lsp_types::CompletionItem::default();
        resolved.label = "foo".into();
        resolved.detail = Some("fn foo() -> i32".into());
        resolved.documentation = Some(lsp_types::Documentation::String("Returns 42.".into()));
        tx.send(super::CompletionResolveOutcome {
            meta_index: 0,
            resolved,
        })
        .unwrap();
        a.drain_pending_completion_resolve();
        // CSM.8b.5: candidate payload (the source of truth) is
        // re-encoded in place with the resolved fields.
        let state = a.insert_completion.as_ref().expect("popup");
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
            insert_text_format: lsp_types::InsertTextFormat::PLAIN_TEXT,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: {
                let mut ci = lsp_types::CompletionItem::default();
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
        a.insert_completion = Some(state);
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<super::CompletionResolveOutcome>();
        a.editor.pending_completion_resolve_rx = Some(rx);
        a.editor.pending_completion_resolve_token = Some(lattice_protocol::CancellationToken::new());
        let mut resolved = lsp_types::CompletionItem::default();
        resolved.label = "c0".into();
        resolved.documentation = Some(lsp_types::Documentation::String("stale".into()));
        tx.send(super::CompletionResolveOutcome {
            meta_index: 0,
            resolved,
        })
        .unwrap();
        a.drain_pending_completion_resolve();
        // c0's payload updated.
        let state = a.insert_completion.as_ref().expect("popup");
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
            insert_text_format: lsp_types::InsertTextFormat::PLAIN_TEXT,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: lsp_types::CompletionItem::default(),
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
        let sh = lsp_types::SignatureHelp {
            signatures: vec![lsp_types::SignatureInformation {
                label: "fn foo(a: i32, b: &str) -> i32".into(),
                documentation: Some(lsp_types::Documentation::String("Adds.".into())),
                parameters: Some(vec![
                    lsp_types::ParameterInformation {
                        label: lsp_types::ParameterLabel::Simple("a: i32".into()),
                        documentation: Some(lsp_types::Documentation::String("the first.".into())),
                    },
                    lsp_types::ParameterInformation {
                        label: lsp_types::ParameterLabel::Simple("b: &str".into()),
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
        let sh = lsp_types::SignatureHelp {
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
        // `do_lsp_nav_request` should set `pending_tag_origin`
        // so a subsequent picker accept (multi-result) pushes
        // the right entry onto the tag stack.
        let mut a = app_with("foo bar\nbaz\n", 10);
        // M.5.4: gate fires before tag-origin capture; activate
        // lsp-mode so the request gets that far.
        a.toggle_mode_by_name("lsp-mode");
        a.editor.cursor = Position::new(0, 1);
        // Manually set a uri so do_lsp_nav_request gets past
        // the "no LSP server" guard.
        use std::str::FromStr;
        a.editor.buffer_uris.insert(
            a.editor.document_buffer_id,
            lattice_lsp::Uri::from_str("file:///tmp/x.rs").unwrap(),
        );
        a.apply(Action::LspDefinitionRequest);
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
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<Vec<lsp_types::Location>>();
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
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<Vec<lsp_types::Location>>();
        a.editor.pending_definition_rx = Some(rx);
        a.editor.pending_definition_token = Some(lattice_protocol::CancellationToken::new());
        let target = lsp_types::Location {
            uri: super::tests::fake_uri(path.to_str().unwrap()),
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 2,
                    character: 5,
                },
                end: lsp_types::Position {
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
            .editor.position_history
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
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<Vec<lsp_types::Location>>();
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
        app.editor.buffer_uris.insert(app.editor.document_buffer_id, fake_uri);
        assert!(app.buffer_uri(app.editor.document_buffer_id).is_some());

        app.lsp_close_buffer(app.editor.document_buffer_id);
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
            .editor.buffers
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
        app.editor.config
            .parse_and_set_command("lsp.log_level=debug")
            .unwrap();
        // The runtime path:
        app.editor.lsp_logger
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
            .editor.buffers
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
            .editor.buffers
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
        app.editor.event_bus.publish_typed(lattice_lsp::LspProgressUpdate {
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
        app.editor.event_bus.publish_typed(lattice_lsp::LspProgressUpdate {
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

        app.editor.event_bus.publish_typed(lattice_lsp::LspProgressUpdate {
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
        let snap = app.document.snapshot();
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
            level: lsp_types::MessageType::INFO,
            message: message.into(),
            actions: actions
                .into_iter()
                .map(|t| lsp_types::MessageActionItem {
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
        assert!(app.editor.picker.is_none(), "no picker for actionless prompt");
        let msg = app.editor.last_message.as_ref().expect("minibuffer set");
        assert!(msg.text.contains("Heads up!"));
        let records = app
            .editor.lsp_logger
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
        let uri: lsp_types::Uri = format!("file://{}", path.display()).parse().unwrap();
        // Edit replaces `main` (line 0, char 3..7) with `xyz`.
        let edit = lsp_types::TextEdit {
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 3,
                },
                end: lsp_types::Position {
                    line: 0,
                    character: 7,
                },
            },
            new_text: "xyz".into(),
        };
        let mut changes = std::collections::HashMap::new();
        changes.insert(uri, vec![edit]);
        let workspace_edit = lsp_types::WorkspaceEdit {
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
        let after = a.document.snapshot().buffer.as_string();
        assert_eq!(after, "fn xyz() {}\n");
    }

    #[test]
    fn drain_inbound_apply_edits_empty_workspace_edit_replies_applied_true() {
        // An empty WorkspaceEdit (no changes, no
        // document_changes) is a server no-op. Spec: reply
        // applied=true so the server doesn't think we
        // failed -- just nothing to do.
        let mut a = app_with("", 5);
        let workspace_edit = lsp_types::WorkspaceEdit::default();
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
            kind: Some(lsp_types::CompletionItemKind::SNIPPET),
            deprecated: false,
            preselect: false,
            commit_characters: Vec::new(),
            additional_text_edits: vec![lsp_types::TextEdit {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                },
                new_text: "use std::iter;\n".into(),
            }],
            command: None,
            insert_text_format: lsp_types::InsertTextFormat::SNIPPET,
            replace_range: None,
            server_id: "test-server".to_string(),
            original_item: lsp_types::CompletionItem::default(),
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
        a.insert_completion = Some(state);
        a.do_completion_accept();
        // After accept: line 0 has the auto-import, line 2
        // (now line 3 after the import inserted a newline,
        // wait -- the import is `use std::iter;\n` which adds
        // an extra newline; existing line 0 was empty so the
        // buffer is now: line 0 = "use std::iter;", line 1 = "",
        // line 2 = "", line 3 = "for i in iter {}").
        let after_accept = a.document.snapshot().buffer.as_string();
        assert!(
            after_accept.contains("use std::iter;"),
            "auto-import applied: `{after_accept}`"
        );
        assert!(
            after_accept.contains("for i in iter {}"),
            "snippet expanded: `{after_accept}`"
        );
        // Active snippet focused on $1 ("i").
        assert!(a.active_snippet.is_some(), "active snippet started");
        // Undo ONCE -> both the auto-import AND the snippet
        // expansion revert.
        a.undo_blocking().expect("undo");
        let after_undo = a.document.snapshot().buffer.as_string();
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
        app.editor.buffer_uris.insert(app.editor.document_buffer_id, uri);
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
            a.editor.last_message
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
