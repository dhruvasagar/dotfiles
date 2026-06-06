//! Boot / config-load / sync paths the runtime calls before the
//! main loop starts -- the App's once-per-launch infrastructure.
//!
//! Methods that live here:
//! - `App::new` (the once-per-launch constructor). Phase 5.7.B.1
//!   delegates the renderer-neutral boot to
//!   [`lattice_host::editor::Editor::boot`]; this method only
//!   builds the renderer wrapper and runs renderer-side
//!   post-boot wiring.
//! - `sync_keymap_overlays` (re-stack the popup / snippet
//!   minor-mode keymap layers in lockstep with overlay state).
//! - `sync_theme_from_config` (re-derive `App.theme`'s renderer-
//!   specific `Style` values from `ui.*` typed options).
//! - `load_persistent_config` (read user + project TOML and
//!   apply scalar overrides + bucket structural sub-tables).
//!
//! What does NOT live here: the option resolver itself
//! (`lattice-config`), the keymap registry
//! (`crate::keymap_registry`), the theme parser
//! (`crate::theme`). This module is the App's *boot wiring*
//! over those.

use lattice_core::Document;

use super::{App, BufferKind};

impl App {
    pub fn new(document: Document) -> Self {
        // Phase 5.7.B.1: the renderer-neutral construction body
        // moved to `lattice_host::editor::Editor::boot`.
        let mut editor = lattice_host::editor::Editor::boot(document);
        // Slice 3c.atomic.A: renderer-side clone of the editor's
        // RenderState cell, captured before Editor moves to the
        // actor thread.
        let render_state = editor.render_state.clone();
        // Slice 3c.final.E.1: clone the worker's output cell.
        let syntax_visible_spans_cell = editor.syntax_visible_spans_cell.clone();
        // Slice 3c.final.E.swap: run boot-time setup directly on
        // the owned Editor BEFORE handing it to the actor. Every
        // call below resolves to a host-side method; the App-side
        // wrappers that previously routed through `mutate_editor`
        // would all funnel through the actor's mailbox, which
        // doesn't exist yet at this point in construction.
        editor.sync_host_theme_from_config();
        editor.rebuild_option_cache();
        let doc_buf = editor.document_buffer_id;
        let _ = editor.activate_major_for_buffer_kind(doc_buf, BufferKind::Document);
        editor.publish_document_opened_for_active();
        editor.ensure_named_synthetic_document(
            lattice_lsp::LSP_SUBSYSTEM_LOG_NAME,
            lattice_lsp::modes::LspLogMode::mode_id(),
            crate::app::App::SYNTHETIC_BUFFER_FLAGS,
        );
        editor.ensure_messages_buffer();
        // Slice 3c.atomic.B: initial RS publish so `app.ad()`
        // returns boot-time editor state, not the Default.
        editor.publish_render_state();

        // Slice 3c.final.E.swap: hand Editor to the actor thread
        // (prod) or keep it inline (test). The cfg-gate is the
        // architectural split — production code can only reach
        // Editor through the actor handle's blocking RPCs, while
        // test code retains direct field access for fixtures that
        // mutate state without going through the dispatch path.
        #[cfg(not(test))]
        let editor_field = lattice_host::editor_actor::spawn_editor_actor(editor);
        #[cfg(test)]
        let editor_field = editor;

        let mut app = Self {
            #[cfg(not(test))]
            editor_actor: editor_field,
            #[cfg(test)]
            editor: editor_field,
            render_state,
            syntax_visible_spans_cell,
            pane_render_registry: crate::render::build_pane_render_registry(),
            theme: crate::theme::Theme::default(),
        };
        // App-side post-actor setup: rebuild the cached TUI theme
        // from the freshly-published `render_state.theme`. Reads
        // the published RS (already primed above), no editor borrow.
        app.rebuild_tui_theme();
        app
    }

    /// Re-stack the Insert-mode minor-mode overlays
    /// (completion popup + active snippet) so the layered
    /// keymap registry mirrors the App's overlay state. Called
    /// from the apply loop after every `Action`; cheap when
    /// nothing changed (single mutex acquisition + early
    /// return).
    ///
    /// Push order is enforced here so popup always sits at the
    /// top of the stack when both overlays are active: the
    /// method pops everything, then pushes snippet (if active),
    /// then popup (if active). Popup's `LayerId` is therefore
    /// always higher than snippet's, and popup wins on
    /// overlapping chords (preserving the legacy "popup
    /// precedes snippet" gating in `input::translate`).
    ///
    /// Slice 8.f.
    pub fn sync_keymap_overlays(&mut self) {
        // Slice 3c.final.E.5e: body hoisted to
        // [`lattice_host::dispatch::Editor::sync_keymap_overlays`].
        // Renderer delegates through `mutate_editor` so post-swap
        // the closure crosses the actor channel like every other
        // mutation.
        self.mutate_editor(|e| e.sync_keymap_overlays());
    }

    // Slice 3c.final.E.5e: `sync_completion_popup_mode_activation`
    // retired -- inlined into
    // [`lattice_host::dispatch::Editor::sync_keymap_overlays`]
    // alongside its sole caller. Original doc-comment preserved
    // for grep:
    //
    /// CSM.K1: bring `completion-popup-mode`'s activation state
    /// on the active document buffer in line with `want_popup`.
    /// Called from `sync_keymap_overlays` so the transient
    /// popup-mode tracks the popup open / close transitions
    /// without each `self.editor.insert_completion = ...` site having
    /// to know about it.
    ///
    /// Per-buffer scope: the popup belongs to the document the
    /// user is typing in. v1 has a single document buffer
    /// (`self.document_buffer_id()`); multi-document support
    /// activates this mode on whichever doc owns the popup at
    /// open time when that lands. Deactivation is symmetric.
    // (Body retired: inlined into the host-side method.)

    /// Re-derive `App.theme`'s renderer-specific `Style` values
    /// from the current `ui.*` option values in the config. Called
    /// at App-init time (after registration) and on every `:set
    /// ui.*` so the cached theme stays in lockstep with the
    /// canonical primitives in config.
    pub fn sync_theme_from_config(&mut self) {
        // Phase 5.5.E.6: the renderer-neutral half (read typed
        // options + write `editor.host_theme`) lives on the host
        // as `Editor::sync_host_theme_from_config`. Splitting the
        // function lets the option-cascade in `Editor::apply_option_cascade`
        // run the host half directly and emit `RendererSignal::ThemeChanged`;
        // the renderer (here) only owns the cached TUI-typed
        // mirror rebuild.
        // Slice 3c.final.E.3: route through `mutate_editor`.
        self.mutate_editor(|e| e.sync_host_theme_from_config());
        self.rebuild_tui_theme();
    }

    /// Rebuild the cached TUI-typed [`crate::theme::Theme`] from
    /// the renderer-neutral [`lattice_host::ui::theme::Theme`].
    /// Cheap (every field is `Copy`); the rebuild fires only on
    /// option cascade or on a host-emitted
    /// [`lattice_host::dispatch::RendererSignal::ThemeChanged`],
    /// never per frame. A future GPUI renderer implements an
    /// equivalent `rebuild_gpui_theme` on its own `App`.
    pub fn rebuild_tui_theme(&mut self) {
        self.theme = crate::theme::Theme::from(&self.render_state.load().theme);
    }

    /// Load `~/.editor.config/lattice/lattice.toml` (user) and
    /// `<workspace_root>/.lattice/config.toml` (project) in
    /// precedence order, applying scalar overrides to
    /// `self.editor.config` and bucketing structural sub-tables (per-
    /// language overrides, plugin sections) into
    /// `self.editor.pending_config_structural_sections` for their
    /// owners to drain.
    ///
    /// Called once by the runtime startup before the main loop
    /// (so the first frame already reflects user overrides).
    /// NOT called from `App::new` -- tests stay isolated from
    /// the user's real `~/.editor.config/lattice/`. Test fixtures that
    /// want to exercise the load path can call this directly
    /// with a synthesized workspace root.
    ///
    /// Loader diagnostics (parse errors, unknown keys,
    /// validation rejects) collapse into a single echo at the
    /// most-severe level: `Error` if any file failed to
    /// parse / read, `Warn` if any key was rejected, otherwise
    /// silent. Per-file `path:body` detail rides the message
    /// body so the user can see *which* file complained.
    pub fn load_persistent_config(&mut self, workspace_root: Option<&std::path::Path>) {
        // Slice 3c.final.E.3: clone path for the `Send + 'static`
        // closure, then route through `mutate_editor_with`.
        let workspace_root = workspace_root.map(|p| p.to_path_buf());
        let signals =
            self.mutate_editor_with(move |e| e.load_persistent_config(workspace_root.as_deref()));
        for s in signals {
            self.handle_renderer_signal(s);
        }
    }
}
