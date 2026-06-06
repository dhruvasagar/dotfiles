//! Modal-state transitions -- the buffer-level state machine
//! (Normal / Insert / Visual / Op-pending / Command /
//! Search) and the major-mode activation hooks that fire
//! on transition.
//!
//! Methods that live here:
//! - `modal_label` (status-line label of the current
//!   modal-state).
//!
//! Methods that move here in R.1 (deferred):
//! - `enter_normal`, `enter_insert`, `enter_replace`,
//!   `enter_op_pending`, `enter_command`, `enter_search`.
//! - The `ModeContext` builder used by major modes during
//!   `on_activate` -- including the `setup_buffer_locals`
//!   path that seeds `BufferLocals` from struct fields.
//! - Cursor-shape recompute on mode transitions (the
//!   App-side hook; renderer reads the resulting state).
//! - `set_active_mode_for_buffer` and the `Mode` trait
//!   dispatch that calls `on_activate` / `on_deactivate`.
//!
//! Insert-mode entry methods (`enter_insert_after`,
//! `enter_insert_line_start`, `enter_insert_line_end`,
//! `enter_insert_open_below`, `enter_insert_open_above`)
//! landed in `app/edit.rs` via R.1.47 because they pair
//! with the Insert / Replace edit primitives that already
//! lived there; they read as edit-flow rather than mode-
//! flow.
//!
//! What does NOT live here: the `Mode` trait itself
//! (lives in `crate::modes`), the per-major-mode impls
//! (`TextMode`, `RustMode`, ...) -- those stay in
//! `crate::modes`.

use lattice_grammar::ModalState;
use lattice_mode::ModeId;

use super::{App, BufferId, BufferKind};

impl App {
    /// 5.5.F.5.3: see [`lattice_host::dispatch::Editor::activate_major_for_buffer_kind`].
    /// Wrapper fans host-returned `RendererSignal`s through
    /// [`Self::handle_renderer_signal`].
    pub fn activate_major_for_buffer_kind(&mut self, buffer_id: BufferId, kind: BufferKind) {
        // Slice 3c.final.E.2: route through `mutate_editor_with`.
        let signals =
            self.mutate_editor_with(move |e| e.activate_major_for_buffer_kind(buffer_id, kind));
        for sig in signals {
            self.handle_renderer_signal(sig);
        }
    }

    // 5.5.F.5.2/F.5.3: `maybe_auto_activate_lsp_mode` +
    // `path_for_buffer` relocated to
    // [`lattice_host::dispatch::Editor`]; the App-side wrappers
    // delete entirely because their callers (`activate_major_for_buffer_kind`
    // and the activate/deactivate trio) all migrated host-side.

    /// M.5.1: programmatic activation of `mode_id` on `buffer_id`.
    /// Used by hooks (auto-activation on `MajorEntered` etc.) and
    /// by the auto-generated `:<mode-name>` toggle command. The
    /// registry decides Major-vs-Minor and runs the appropriate
    /// activation; for majors the previous major is deactivated
    /// first.
    ///
    /// On failure, surfaces an `EchoLevel::Warn` and returns
    /// without mutating state. Callers that need to know the
    /// outcome can read `self.editor.active_modes[buffer_id]` after.
    /// 5.5.F.5.2: see [`lattice_host::dispatch::Editor::activate_mode_by_id`].
    /// Wrapper fans host-returned `RendererSignal`s through
    /// [`Self::handle_renderer_signal`].
    pub fn activate_mode_by_id(&mut self, buffer_id: BufferId, mode_id: ModeId) {
        // Slice 3c.final.E.2: route through `mutate_editor_with`.
        let signals = self.mutate_editor_with(move |e| e.activate_mode_by_id(buffer_id, mode_id));
        for sig in signals {
            self.handle_renderer_signal(sig);
        }
    }

    /// M.5.1: programmatic deactivation of `mode_id` on
    /// `buffer_id`. Symmetric to [`Self::activate_mode_by_id`].
    /// Major deactivation leaves the buffer with no active major
    /// until the next activation; user-facing flows usually flow
    /// through the toggle command which performs swap rather
    /// than a bare deactivate.
    /// 5.5.F.5.2: see [`lattice_host::dispatch::Editor::deactivate_mode_by_id`].
    /// Wrapper fans host-returned `RendererSignal`s through
    /// [`Self::handle_renderer_signal`].
    pub fn deactivate_mode_by_id(&mut self, buffer_id: BufferId, mode_id: ModeId) {
        // Slice 3c.final.E.2: route through `mutate_editor_with`.
        let signals =
            self.mutate_editor_with(move |e| e.deactivate_mode_by_id(buffer_id, mode_id));
        for sig in signals {
            self.handle_renderer_signal(sig);
        }
    }

    /// M.5.3: lsp-mode activated on `buffer_id`. Emits
    /// `LspBufferAttached` on the editor's event bus so
    /// subscribers see the gate flip. Wire-level `didOpen` is
    /// already driven by the `attach_driver` subscribing to
    /// `Event::DocumentOpened` from the file-open path.
    // Phase 3 removed `on_lsp_mode_activated` /
    // `_deactivated`. The work they did is owned by the mode
    // now:
    //
    // - Event publication (`LspBufferAttached` /
    //   `LspBufferDetached`) lives in `LspMode::on_activate` /
    //   `on_deactivate` via `ctx.events()` (Phase 2).
    // - Sub-mode cascade (activate + deactivate of the 13
    //   LSP sub-modes) lives in `Mode::implies()` --
    //   `LspMode::new` builds the list once; the registry's
    //   `activate_minor` / `deactivate_minor` walk the list
    //   (the latter symmetrically via the Phase 3 cascade
    //   extension).
    //
    // The wire-level `didClose` (`lsp_close_buffer`) is the
    // only remaining App-side action; it lives at the
    // `deactivate_mode_by_id` call site directly. Moving it
    // requires a `LspBufferDetached` subscriber in boot;
    // queued for a follow-up slice.

    // 4.4.f: `lsp-folding-mode` lifecycle moved into
    // `LspFoldingMode::on_activate` / `on_deactivate` in
    // `lattice-lsp`. The mode owns its work; the App is just
    // the orchestrator. `drain_option_changes()` in the
    // activate/deactivate call sites picks up the option
    // mutation the mode emits via `ctx.editor.config()`.

    // Phase 3 removed `activate_lsp_sub_modes_for` /
    // `deactivate_lsp_sub_modes_for`. The sub-mode cascade
    // lives in the registry now, driven by
    // `LspMode::implies()` (a Vec built once at
    // `LspMode::new()`). `ModeRegistry::activate_minor`
    // walks `implies()` on activation; the symmetric
    // `deactivate_minor` extension walks it on
    // deactivation. Adding a new LSP sub-mode is one
    // entry in `LspMode::new()`'s `vec![...]`; no App-side
    // edits.

    /// M.5.1: toggle a mode by name on the active pane's buffer.
    /// This is the apply-fn target for the auto-generated
    /// `:<mode-name>` ex-commands (mode-architecture §9.6.1).
    /// Toggle semantics:
    /// - **Minor**: deactivate if active; activate if inactive.
    /// - **Major**: activate if not currently the major; if it's
    ///   already the active major, the registry treats this as
    ///   a *reload* (deactivate then re-activate, per §9.6).
    ///
    /// Activating a major that differs from the current major
    /// performs a swap -- the registry deactivates the previous
    /// major before activating the new one. Active minors stay
    /// untouched across the swap (their state lives in
    /// type-keyed `BufferLocals` owned per-mode; no
    /// `kill-all-local-variables` semantics).
    /// 5.5.F.5.2: see [`lattice_host::dispatch::Editor::toggle_mode_by_name`].
    /// Wrapper fans host-returned `RendererSignal`s through
    /// [`Self::handle_renderer_signal`].
    pub fn toggle_mode_by_name(&mut self, name: &str) {
        // Slice 3c.final.E.2: route through `mutate_editor_with`.
        // Closure needs `Send + 'static`, so promote `&str` to `String`.
        let name_owned = name.to_string();
        let signals = self.mutate_editor_with(move |e| e.toggle_mode_by_name(&name_owned));
        for sig in signals {
            self.handle_renderer_signal(sig);
        }
    }

    /// 5.5.F.5.3: see [`lattice_host::dispatch::Editor::drain_mode_lifecycle_events`].
    /// Wrapper fans host-returned `RendererSignal`s through
    /// [`Self::handle_renderer_signal`].
    pub fn drain_mode_lifecycle_events(&mut self) {
        // Slice 3c.final.E.2: route through `mutate_editor_with`.
        let signals = self.mutate_editor_with(|e| e.drain_mode_lifecycle_events());
        for sig in signals {
            self.handle_renderer_signal(sig);
        }
    }

    pub fn modal_label(&self) -> &'static str {
        // 3c.atomic.E: renderer-side modal read through the
        // published `ActiveDocumentRenderState` cell. The modeline
        // and other status renderers call this once per frame.
        match self.ad().modal {
            ModalState::Normal => "NORMAL",
            ModalState::Insert => "INSERT",
            ModalState::Visual(_) => "VISUAL",
            ModalState::OperatorPending => "O-PEND",
            ModalState::Command => "CMD",
            ModalState::Search(_) => "SEARCH",
            ModalState::Replace => "REPLACE",
        }
    }

    /// 5.5.G.17: body migrated to
    /// [`lattice_host::dispatch::Editor::enter_mode`]. Kept as a
    /// delegate -- a handful of paths still drive modal state
    /// directly (the `RepeatLastChange` apply arm, the
    /// `Effect::EnterMode` apply_effect arm, and several
    /// `do_*` helpers in `edit` / `motions` / `lsp` modules).
    /// These retire as their callers migrate host-side.
    pub(super) fn enter_mode(&mut self, state: ModalState) {
        // Slice 3c.final.E.2: route through `mutate_editor`.
        self.mutate_editor(move |e| e.enter_mode(state));
    }
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)]

    use crate::app::test_helpers::app_with;

    #[test]
    fn line_numbers_mode_overrides_typed_option_layer() {
        // M.7.0: the mode-contribution layer wins against the
        // typed-option layer. `Number` defaults to `true`; the
        // user can `:set nonumber` to flip the typed-option
        // layer to `false`, then `:line-numbers-mode` to
        // re-enable just for this buffer via the mode layer.
        // The resolved value reflects the mode contribution.
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        // Flip the typed-option layer to false globally.
        a.do_set("nonumber");
        assert!(!a.show_line_numbers(), "set nonumber should flip cache");
        // Activate `:line-numbers-mode` for this buffer; the
        // mode-contribution layer overrides the typed-option
        // layer's false.
        a.toggle_mode_by_name("line-numbers-mode");
        assert!(
            *a.resolved_option::<lattice_config::Number>(id),
            "mode contribution should override typed-option false",
        );
        assert!(a.show_line_numbers(), "hot-path cache should track");
        // Deactivating the mode removes the contribution; the
        // typed-option layer (false) takes over again.
        a.toggle_mode_by_name("line-numbers-mode");
        assert!(!a.show_line_numbers());
    }

    #[test]
    fn relative_line_numbers_mode_implies_line_numbers() {
        // M.7.0: `:relative-line-numbers-mode` contributes both
        // RelativeNumber=true AND Number=true (vim's `:set rnu`
        // implies `:set nu` cascade, baked into the mode's
        // contribution so users who never touch
        // `:line-numbers-mode` still get a visible gutter).
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        a.toggle_mode_by_name("relative-line-numbers-mode");
        assert!(*a.resolved_option::<lattice_config::RelativeNumber>(id));
        assert!(*a.resolved_option::<lattice_config::Number>(id));
    }

    #[test]
    fn wrap_mode_toggle_flips_wrap_lines() {
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        assert!(!a.wrap_lines());
        a.toggle_mode_by_name("wrap-mode");
        assert!(*a.resolved_option::<lattice_config::Wrap>(id));
        a.toggle_mode_by_name("wrap-mode");
        assert!(!a.wrap_lines());
    }

    #[test]
    fn set_number_true_activates_line_numbers_mode_on_active_buffer() {
        // M.7.1 convergence: `:set number=true` activates
        // `line-numbers-mode` on the active buffer; `:set
        // nonumber` deactivates it. The two surfaces stay in
        // sync.
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        // Default Number=true; M.7.1 means the mode should
        // already be active on the active buffer (initial cascade
        // when typed-option is true at startup). But initial
        // boot doesn't fire OptionChanged, so the mode isn't
        // pre-activated. Verify by explicit set.
        a.do_set("nonumber");
        assert!(
            !a.editor
                .active_modes
                .get(&id)
                .unwrap()
                .has_minor(lattice_mode::modes::LineNumbersMode::mode_id())
        );
        a.do_set("number");
        assert!(
            a.editor
                .active_modes
                .get(&id)
                .unwrap()
                .has_minor(lattice_mode::modes::LineNumbersMode::mode_id())
        );
        a.do_set("nonumber");
        assert!(
            !a.editor
                .active_modes
                .get(&id)
                .unwrap()
                .has_minor(lattice_mode::modes::LineNumbersMode::mode_id())
        );
    }

    #[test]
    fn set_wrap_converges_with_wrap_mode() {
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        let wrap_id = lattice_mode::modes::WrapMode::mode_id();
        a.do_set("wrap");
        assert!(a.editor.active_modes.get(&id).unwrap().has_minor(wrap_id));
        a.do_set("nowrap");
        assert!(!a.editor.active_modes.get(&id).unwrap().has_minor(wrap_id));
    }

    #[test]
    fn set_list_converges_with_whitespace_show_mode() {
        // M.7.2: `:set list` (vim alias for whitespace-show)
        // activates `whitespace-show-mode`; `:set nolist`
        // deactivates.
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        let mode_id = lattice_mode::modes::WhitespaceShowMode::mode_id();
        a.do_set("list");
        assert!(a.editor.active_modes.get(&id).unwrap().has_minor(mode_id));
        a.do_set("nolist");
        assert!(!a.editor.active_modes.get(&id).unwrap().has_minor(mode_id));
    }

    #[test]
    fn set_cursorline_converges_with_current_line_highlight_mode() {
        // M.7.2: `:set cursorline` (vim alias) ↔
        // `:current-line-highlight-mode`.
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        let mode_id = lattice_mode::modes::CurrentLineHighlightMode::mode_id();
        a.do_set("cursorline");
        assert!(a.editor.active_modes.get(&id).unwrap().has_minor(mode_id));
        a.do_set("nocursorline");
        assert!(!a.editor.active_modes.get(&id).unwrap().has_minor(mode_id));
    }

    #[test]
    fn line_numbers_mode_activation_matches_set_number() {
        // M.7.1 the other direction: activating the mode AND
        // running `:set number` produce the same observable
        // state. (Mode activation doesn't reach back into the
        // typed-option layer in v1 -- that's a separate
        // refinement -- but the resolved-option view converges
        // because the mode contribution wins regardless.)
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        a.do_set("nonumber");
        assert!(!a.show_line_numbers());
        // Mode activation flips the mode-contribution layer.
        a.toggle_mode_by_name("line-numbers-mode");
        assert!(
            a.show_line_numbers(),
            "mode contribution overrides set false"
        );
        // `:set number` activates the mode (already active --
        // no-op) and flips the typed-option layer.
        a.do_set("number");
        assert!(a.show_line_numbers());
        assert!(
            a.editor
                .active_modes
                .get(&id)
                .unwrap()
                .has_minor(lattice_mode::modes::LineNumbersMode::mode_id())
        );
    }

    #[test]
    fn read_only_mode_toggle_flips_read_only_resolved_value() {
        // M.7.0: `:read-only-mode` lets users mark an arbitrary
        // buffer read-only. `ReadOnly` itself is
        // `customizable = false` (no `:set` surface); this is
        // the user-typed pathway.
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        assert!(!*a.resolved_option::<lattice_config::ReadOnly>(id));
        a.toggle_mode_by_name("read-only-mode");
        assert!(*a.resolved_option::<lattice_config::ReadOnly>(id));
        a.toggle_mode_by_name("read-only-mode");
        assert!(!*a.resolved_option::<lattice_config::ReadOnly>(id));
    }

    #[test]
    fn toggle_minor_mode_by_name_activates_then_deactivates() {
        // M.5.1: `:lsp-mode` (or any minor name) toggles. First
        // call activates, second deactivates. The mode is
        // registered at boot so name lookup succeeds.
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        let lsp_mode = lattice_lsp::modes::LspMode::mode_id();
        assert!(!a.lsp_mode_enabled_for(id));
        a.toggle_mode_by_name("lsp-mode");
        assert!(a.lsp_mode_enabled_for(id));
        assert!(a.editor.active_modes.get(&id).unwrap().has_minor(lsp_mode));
        a.toggle_mode_by_name("lsp-mode");
        assert!(!a.lsp_mode_enabled_for(id));
    }

    #[test]
    fn activating_lsp_mode_cascades_all_sub_modes_on() {
        // M.6.1 cascade-on: toggling `:lsp-mode` activates every
        // LSP sub-mode in one step. The umbrella is the gate;
        // the sub-modes are user-controllable disable switches
        // that default to "track the umbrella".
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        a.toggle_mode_by_name("lsp-mode");
        assert!(a.lsp_mode_enabled_for(id));
        // All nine sub-modes flipped on.
        assert!(a.lsp_completion_mode_enabled_for(id));
        assert!(a.lsp_diagnostics_mode_enabled_for(id));
        assert!(a.lsp_hover_mode_enabled_for(id));
        assert!(a.lsp_signature_mode_enabled_for(id));
        assert!(a.lsp_format_mode_enabled_for(id));
        assert!(a.lsp_rename_mode_enabled_for(id));
        assert!(a.lsp_symbols_mode_enabled_for(id));
        assert!(a.lsp_code_action_mode_enabled_for(id));
        assert!(a.lsp_nav_mode_enabled_for(id));
    }

    #[test]
    fn deactivating_lsp_mode_cascades_all_sub_modes_off() {
        // M.6.1 cascade-off: toggling `:lsp-mode` off deactivates
        // every sub-mode atomically.
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        a.toggle_mode_by_name("lsp-mode");
        assert!(a.lsp_hover_mode_enabled_for(id));
        a.toggle_mode_by_name("lsp-mode");
        assert!(!a.lsp_mode_enabled_for(id));
        // All nine sub-modes flipped off.
        assert!(!a.lsp_completion_mode_enabled_for(id));
        assert!(!a.lsp_diagnostics_mode_enabled_for(id));
        assert!(!a.lsp_hover_mode_enabled_for(id));
        assert!(!a.lsp_signature_mode_enabled_for(id));
        assert!(!a.lsp_format_mode_enabled_for(id));
        assert!(!a.lsp_rename_mode_enabled_for(id));
        assert!(!a.lsp_symbols_mode_enabled_for(id));
        assert!(!a.lsp_code_action_mode_enabled_for(id));
        assert!(!a.lsp_nav_mode_enabled_for(id));
    }

    #[test]
    fn user_disabling_a_sub_mode_after_cascade_keeps_others_active() {
        // M.6.1 contract: cascade-on activates everything; user
        // can then independently disable one sub-mode and the
        // others stay on. This is the "disable LSP completion
        // but keep diagnostics" use case from §4.2.1.
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        a.toggle_mode_by_name("lsp-mode");
        // Independently disable `lsp-completion-mode`.
        a.toggle_mode_by_name("lsp-completion-mode");
        assert!(!a.lsp_completion_mode_enabled_for(id));
        // Other sub-modes still active.
        assert!(a.lsp_diagnostics_mode_enabled_for(id));
        assert!(a.lsp_hover_mode_enabled_for(id));
        assert!(a.lsp_format_mode_enabled_for(id));
        // Umbrella still active.
        assert!(a.lsp_mode_enabled_for(id));
    }

    #[test]
    fn re_activating_lsp_mode_after_user_disable_re_cascades_sub_modes_on() {
        // Edge case: user toggles lsp-mode off, then on again.
        // The cascade-on should re-activate every sub-mode,
        // including any the user had previously disabled.
        // (Toggling the umbrella is the user's "reset to defaults"
        // gesture.)
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        a.toggle_mode_by_name("lsp-mode");
        a.toggle_mode_by_name("lsp-hover-mode");
        assert!(!a.lsp_hover_mode_enabled_for(id));
        // Cycle the umbrella: cascade-off then cascade-on.
        a.toggle_mode_by_name("lsp-mode");
        a.toggle_mode_by_name("lsp-mode");
        assert!(a.lsp_hover_mode_enabled_for(id));
    }

    #[test]
    fn lsp_sub_modes_default_off_and_independently_toggleable() {
        // M.6.0: each sub-mode accessor returns false on a fresh
        // buffer; toggling each by name flips only that sub-mode.
        // (M.6.1 will add capability-driven cascade from
        // `:lsp-mode` activation -- this test pins the manual-
        // toggle pathway, which is the v1 escape hatch when a
        // user wants a sub-mode active independently of the
        // umbrella's auto-activation logic.)
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        // All nine sub-modes start off.
        assert!(!a.lsp_completion_mode_enabled_for(id));
        assert!(!a.lsp_diagnostics_mode_enabled_for(id));
        assert!(!a.lsp_hover_mode_enabled_for(id));
        assert!(!a.lsp_signature_mode_enabled_for(id));
        assert!(!a.lsp_format_mode_enabled_for(id));
        assert!(!a.lsp_rename_mode_enabled_for(id));
        assert!(!a.lsp_symbols_mode_enabled_for(id));
        assert!(!a.lsp_code_action_mode_enabled_for(id));
        assert!(!a.lsp_nav_mode_enabled_for(id));
        // Toggling `lsp-hover-mode` flips its accessor but no
        // other sub-mode's accessor.
        a.toggle_mode_by_name("lsp-hover-mode");
        assert!(a.lsp_hover_mode_enabled_for(id));
        assert!(!a.lsp_completion_mode_enabled_for(id));
        assert!(!a.lsp_diagnostics_mode_enabled_for(id));
        assert!(!a.lsp_format_mode_enabled_for(id));
        // Round-trip back off.
        a.toggle_mode_by_name("lsp-hover-mode");
        assert!(!a.lsp_hover_mode_enabled_for(id));
    }

    #[test]
    fn toggle_unknown_mode_name_emits_error_echo() {
        // Unknown name → error message; no state change.
        let mut a = app_with("hi", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        let before_minors_len = a
            .editor
            .active_modes
            .get(&id)
            .map(|m| m.minors().len())
            .unwrap_or(0);
        a.toggle_mode_by_name("definitely-not-a-mode");
        let msg = a.editor.last_message.as_ref().expect("error echo");
        assert!(msg.text.contains("not a registered mode"));
        let after_minors_len = a
            .editor
            .active_modes
            .get(&id)
            .map(|m| m.minors().len())
            .unwrap_or(0);
        assert_eq!(before_minors_len, after_minors_len);
    }

    #[test]
    fn toggle_major_mode_by_name_swaps_active_major() {
        // Major-mode toggle = swap. The buffer starts on the
        // resolver's pick (text-mode for plain content); flipping
        // to `markdown-mode` deactivates text-mode and activates
        // markdown-mode. Active minors stay untouched.
        let mut a = app_with("# heading", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        // Activate a minor first so we can verify it survives.
        a.toggle_mode_by_name("lsp-mode");
        assert!(a.lsp_mode_enabled_for(id));
        // Swap major.
        a.toggle_mode_by_name("markdown-mode");
        let modes = a.editor.active_modes.get(&id).expect("modes for buffer");
        assert_eq!(modes.major(), Some(lattice_syntax::MarkdownMode::mode_id()));
        // Minor unaffected by major swap (M.5 design).
        assert!(modes.has_minor(lattice_lsp::modes::LspMode::mode_id()));
    }

    #[test]
    fn lsp_mode_auto_activates_on_language_with_configured_server() {
        // M.5.2: opening a buffer whose path matches a configured
        // server's `file_patterns` auto-activates `lsp-mode`. The
        // bundled rust-analyzer config matches `*.rs`.
        use crate::app::test_helpers::app_with_path;
        let a = app_with_path("fn main() {}", 5, std::path::PathBuf::from("foo.rs"));
        let id = a.editor.pane_tree.active().buffer_id;
        assert!(
            a.lsp_mode_enabled_for(id),
            "lsp-mode should auto-activate on a *.rs buffer with rust-analyzer configured"
        );
    }

    #[test]
    fn lsp_mode_does_not_auto_activate_for_unconfigured_extensions() {
        // No bundled server matches `*.unknown_ext`, so lsp-mode
        // should stay inactive even after the major activates.
        use crate::app::test_helpers::app_with_path;
        let a = app_with_path(
            "plain text",
            5,
            std::path::PathBuf::from("notes.unknown_ext"),
        );
        let id = a.editor.pane_tree.active().buffer_id;
        assert!(
            !a.lsp_mode_enabled_for(id),
            "lsp-mode shouldn't auto-activate when no server config matches"
        );
    }

    #[test]
    fn lsp_mode_does_not_auto_activate_for_pathless_scratch_buffers() {
        // Scratch buffers without a path don't get auto-activation
        // (path-driven check). Standalone-server use cases require
        // explicit `:lsp-mode`.
        let a = app_with("fn main() {}", 5);
        let id = a.editor.pane_tree.active().buffer_id;
        assert!(!a.lsp_mode_enabled_for(id));
    }

    /// Wait up to `budget` for `predicate` to return true.
    /// Polls every 5ms; uses `tokio::time::sleep` so the
    /// current task yields to the runtime (letting the
    /// shared-runtime spawn from `LspMode::on_activate`
    /// complete). Returns whether the predicate held by the
    /// deadline.
    async fn wait_for(mut predicate: impl FnMut() -> bool, budget: std::time::Duration) -> bool {
        let deadline = std::time::Instant::now() + budget;
        while !predicate() {
            if std::time::Instant::now() >= deadline {
                return false;
            }
            tokio::time::sleep(std::time::Duration::from_millis(5)).await;
        }
        true
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn deactivating_lsp_mode_emits_lsp_buffer_detached_event() {
        // M.5.3 + M-async.5: deactivating `lsp-mode` publishes
        // `LspBufferDetached` on the editor bus. Under
        // M-async.5 `LspMode::on_activate` `.await`s the
        // supervisor's `open_buffer` mailbox, so the Guard
        // lands asynchronously after App::new returns. Wait
        // for the activation to settle before toggling off
        // (otherwise the deactivate hits an empty store and
        // no Drop fires synchronously).
        use crate::app::test_helpers::app_with_path;
        let mut a = app_with_path("fn main() {}", 5, std::path::PathBuf::from("foo.rs"));
        let id = a.editor.pane_tree.active().buffer_id;
        // Sync prefix mutated active_modes for lsp-mode; the
        // spawn task is still in flight (open_buffer.await
        // round-trips the supervisor mailbox).
        assert!(a.lsp_mode_enabled_for(id));
        // Subscribe BEFORE the activation settles so we don't
        // miss any events. The detach we care about fires
        // either synchronously (if Guard stashed by toggle-off
        // time) or on the spawn side (if activation's spawn
        // hasn't completed -- the stale Guard drops via
        // try_insert mismatch and still publishes
        // LspBufferDetached).
        let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspBufferDetached>();
        a.editor.event_bus.subscribe_typed(tx);
        a.toggle_mode_by_name("lsp-mode");
        assert!(!a.lsp_mode_enabled_for(id));
        let got_detach =
            wait_for(|| rx.try_recv().is_ok(), std::time::Duration::from_secs(2)).await;
        assert!(
            got_detach,
            "expected LspBufferDetached on bus after `:lsp-mode` toggle off",
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn deactivating_lsp_mode_clears_buffer_uri_mapping() {
        // M.5.3 + M-async.5: the wire-level `didClose` +
        // `buffer_uris` cleanup runs from the
        // `LspBufferDetached` drain, not from the mode-
        // activation path itself. Test mirrors the runtime
        // tick: toggle the mode, wait for the detach event to
        // hit the channel, then call the drain (consumes the
        // event + clears `buffer_uris`).
        use crate::app::test_helpers::app_with_path;
        let mut a = app_with_path("fn main() {}", 5, std::path::PathBuf::from("foo.rs"));
        let id = a.editor.pane_tree.active().buffer_id;
        assert!(a.buffer_uri(id).is_some());
        a.toggle_mode_by_name("lsp-mode");
        // Wait for the detach event to land on the bus.
        // `drain_lsp_detach_events` is non-blocking; without
        // the wait the spawn-side Drop hasn't fired yet.
        let ready = wait_for(
            || {
                a.drain_lsp_detach_events();
                a.buffer_uri(id).is_none()
            },
            std::time::Duration::from_secs(2),
        )
        .await;
        assert!(
            ready,
            "lsp-mode deactivate should clear buffer_uris[id] after detach drain"
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn lsp_mode_auto_activates_on_each_new_rust_buffer() {
        // Bug fix: pre-fix, only the boot buffer ran
        // `activate_major_for_buffer_kind`; `:e other.rs` skipped
        // it, so `lsp-mode` never woke up on subsequent buffers.
        // Post-fix, `activate_buffer_state` re-runs the major
        // activation (idempotent on re-entry), which triggers
        // the existing `maybe_auto_activate_lsp_mode` hook for
        // every newly-visited buffer whose path has a server
        // configured.
        use crate::app::test_helpers::{app_with_path, write_temp_file};
        let mut a = app_with_path("fn first() {}", 5, std::path::PathBuf::from("first.rs"));
        let first_id = a.editor.pane_tree.active().buffer_id;
        // Boot path's lsp-mode activation is async since
        // M-async.5 -- wait for it to settle.
        let first_attached = wait_for(
            || a.lsp_mode_enabled_for(first_id),
            std::time::Duration::from_secs(2),
        )
        .await;
        assert!(
            first_attached,
            "first rust buffer should auto-attach lsp-mode at boot"
        );
        // Open a second `.rs` file. Pre-fix this was the bug:
        // `do_edit` created the buffer and called
        // `activate_buffer_state` but skipped major activation,
        // so the auto-LSP hook never fired.
        let second_path = write_temp_file("auto-lsp-second.rs", "fn second() {}\n");
        a.do_edit(Some(second_path.clone()), false);
        let second_id = a.editor.document_buffer_id;
        assert_ne!(second_id, first_id, ":e should have opened a new buffer");
        let second_attached = wait_for(
            || a.lsp_mode_enabled_for(second_id),
            std::time::Duration::from_secs(2),
        )
        .await;
        assert!(
            second_attached,
            "lsp-mode should auto-activate on the second rust buffer after :e (buffer-switch fix)"
        );
        // First buffer's lsp-mode untouched -- per-buffer state.
        assert!(
            a.lsp_mode_enabled_for(first_id),
            "first buffer's lsp-mode should persist after switch"
        );
        // Switching back via the activate-document path: the
        // idempotency guard inside `activate_major_for_buffer_kind`
        // skips the registry reload, the auto-LSP hook short-
        // circuits (already-active minor). lsp-mode stays on.
        a.activate_document(first_id);
        assert!(
            a.lsp_mode_enabled_for(first_id),
            "first buffer's lsp-mode should still be active after switch-back via :b"
        );
        let _ = std::fs::remove_file(second_path);
    }

    #[test]
    fn lsp_mode_survives_major_swap() {
        // M.5 design: major swaps don't touch active minors. Open
        // a rust file (auto-activates lsp-mode), swap to text-mode,
        // lsp-mode stays active. User runs `:lsp-mode` to flip off.
        use crate::app::test_helpers::app_with_path;
        let mut a = app_with_path("fn main() {}", 5, std::path::PathBuf::from("foo.rs"));
        let id = a.editor.pane_tree.active().buffer_id;
        assert!(a.lsp_mode_enabled_for(id));
        a.toggle_mode_by_name("text-mode");
        assert!(
            a.lsp_mode_enabled_for(id),
            "lsp-mode should survive major-mode swap (M.5 design)"
        );
    }

    #[test]
    fn describe_events_renders_catalogue_grouped_by_source_crate() {
        // M.5.3.c: `:describe-events` walks `EVENT_DESCRIPTORS`
        // (the linkme distributed slice every `register_event!`
        // pushes into) and renders a help buffer. Every event
        // in the registry should appear in the rendered body.
        let mut a = app_with("hi", 5);
        a.do_describe_events();
        let h = a
            .popup_help()
            .expect(":describe-events should open a help popup");
        let body = h.content.as_string();
        // The three M.5.3.b LSP events should be listed.
        assert!(
            body.contains("lsp.buffer-attached"),
            "describe-events body missing `lsp.buffer-attached`; got:\n{body}"
        );
        assert!(
            body.contains("lsp.buffer-detached"),
            "describe-events body missing `lsp.buffer-detached`; got:\n{body}"
        );
        assert!(
            body.contains("lsp.log-pushed"),
            "describe-events body missing `lsp.log-pushed`; got:\n{body}"
        );
        // Source crate header for grouped section.
        assert!(
            body.contains("lattice-lsp"),
            "describe-events body should group by source crate; got:\n{body}"
        );
    }

    #[test]
    fn describe_event_renders_single_descriptor() {
        let mut a = app_with("hi", 5);
        a.do_describe_event("lsp.buffer-attached");
        let h = a
            .popup_help()
            .expect(":describe-event should open a help popup");
        let body = h.content.as_string();
        assert!(body.contains("lsp.buffer-attached"));
        assert!(body.contains("lattice-lsp"));
    }

    #[test]
    fn describe_event_unknown_name_emits_error_echo() {
        let mut a = app_with("hi", 5);
        a.do_describe_event("definitely-not-an-event");
        let msg = a.editor.last_message.as_ref().expect("error echo");
        assert!(msg.text.contains("no event named"));
        assert!(a.editor.popup_buffer.is_none());
    }

    #[test]
    fn toggle_command_resolves_through_ex_command_registry() {
        // M.5.1: the `:<mode-name>` ex-command auto-registered at
        // boot drives the same toggle. End-to-end through
        // `apply(Action::ExecuteEx(...))` would be the full flow;
        // here we verify the registry entry exists with the
        // expected name (the apply fn is exercised by the tests
        // above via direct `toggle_mode_by_name`).
        let a = app_with("hi", 5);
        // Every registered mode should have a corresponding
        // ex-command keyword in the registry.
        for (mode_id, _kind) in a.editor.mode_registry.iter_meta() {
            let name = mode_id.to_string();
            assert!(
                a.editor.registry.id_by_name(&name).is_some(),
                "no ex-command registered for mode `{name}`"
            );
        }
    }
}
