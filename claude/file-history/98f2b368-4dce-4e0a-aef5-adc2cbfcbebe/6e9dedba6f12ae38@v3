//! Phase 5.7: GPUI peer renderer scaffold for lattice.
//!
//! Design anchor: `docs/dev/architecture/design.md` §5.6 (rendering
//! layered architecture) + `docs/dev/architecture/phase-5-extraction.md`
//! slice 5.7 + post-Option-E pivot notes (lines 348–349). The
//! original §5.6.1 `Renderer` trait (with `Frame` / `InputEvent` /
//! `LayoutConstraints` / `paint`) was dropped: ratatui's pull-based
//! draw loop and GPUI's retained-mode element tree are structurally
//! different, and forcing a shared `Frame` type buys nothing. The
//! current renderer-trait shape (`lattice_host::Renderer`) is the
//! composition-typed associated-types version that came out of 5.B.
//!
//! Per the design (§5.6.1) the **GPU UI is the primary v1 surface**;
//! the TUI is a first-class peer for headless / SSH / low-bandwidth
//! use. This crate is the GPU peer; both renderers share the host
//! substrate (`lattice-host`) and never depend on each other.
//!
//! ## Slicing
//!
//! This file ships the renderer-neutral scaffold types — they build
//! on every host (including headless CI and WSL2 without display
//! libs) and exercise the host-substrate decoupling claim:
//!
//! - [`GpuiTheme`] — stub theme cache. Mirrors the role of
//!   `lattice-ui-tui::theme::Theme` (cached pre-computed render
//!   primitives so per-frame reads are direct) but will hold GPUI
//!   native style primitives (`Hsla`, `TextStyle`) in 5.7.B+. Empty
//!   today so the scaffold has no transitive gpui-link requirement.
//!
//! - [`GpuiPaneRenderRegistry`] — stub registry implementing
//!   [`lattice_host::pane_render::ProviderLookup`]. The host's
//!   mode-walk ([`lattice_host::pane_render::resolve_pane_render_mode`])
//!   already routes through it; storage will fill with the GPUI-typed
//!   render fn signature in 5.8+.
//!
//! - [`GpuiRenderer`] — `impl lattice_host::Renderer` zero-sized
//!   marker that surfaces the renderer-specific associated types
//!   to the host's renderer-trait machinery.
//!
//! - [`GpuiApp`] — the renderer-side composition root. Mirrors
//!   `lattice-ui-tui::app::App` in shape: `{editor, theme,
//!   pane_render_registry}` (an `lsp_file_watcher` field joins when
//!   the LSP runtime adapter for GPUI lands in 5.8+).
//!
//! The window-opening entry point lives in the `lattice-gpui`
//! binary (`src/bin/lattice_gpui.rs`) behind the `window` Cargo
//! feature. That binary pulls real gpui at link time; the lib above
//! does not. This split keeps the scaffold's test target buildable
//! everywhere while the binary is opt-in for environments with
//! display libs installed.
//!
//! Phase 5.7.B.1 landed [`lattice_host::editor::Editor::boot`]:
//! the renderer-neutral construction body (LSP subsystem,
//! command registry, mode registry, snippet registry, event
//! bus, completion / picker / config registries, syntax +
//! buffer registry seeding) lives on the host substrate. Phase
//! 5.7.B.2 (this slice) wires [`GpuiApp::new`] to call it:
//! `GpuiApp::new(document)` mirrors `lattice-ui-tui::App::new`'s
//! shape — the renderer-neutral half goes through `Editor::boot`,
//! the renderer-side wrapper holds the theme + pane-render
//! registry. Real dispatch + paint wiring (key events ->
//! `Action` -> `editor.dispatch`; paint reads `editor.document`
//! snapshot + cursor) follows in 5.7.B.3 / 5.7.B.4.

use lattice_core::{BufferKind, Document};
use lattice_host::Renderer;
use lattice_host::action::Action;
use lattice_host::chord::KeyChord;
use lattice_host::dispatch::{DispatchOutcome, RendererSignal};
use lattice_host::editor::Editor;
use lattice_host::input::TranslateContext;
use lattice_host::pane_render::ProviderLookup;
use lattice_host::render_state::{ActiveDocumentRenderState, RenderState};
use lattice_mode::ModeId;
use std::sync::Arc;

pub mod gpui_chord;

/// GPUI window-opening entry point ([`run`], [`document_from_path`],
/// [`document_from_first_arg`]). Behind the `window` Cargo feature
/// so the lib's headless build doesn't link gpui. Lifted from the
/// `lattice-gpui` binary in Phase 5.9 so `lattice-cli --gpu` can
/// reuse the same entry without duplicating the window setup.
#[cfg(feature = "window")]
pub mod window;

/// Phase 5.8.AF.5 / Slice X3.full.1: custom GPUI `Element` rendering
/// pane text via `ShapedLine::paint` -- replaces the per-char-Div
/// element tree that dominated `paint_us`.
#[cfg(feature = "window")]
// Phase 5.8.AF.6 / Slice X5: `editor_element` is normally
// crate-private (the only legitimate caller is `window::paint_pane`).
// With `bench-internals` it becomes `pub` so the frame-budget
// bench can call the pre-paint logic (`build_line_with_inlays`,
// `byte_to_combined_col`, ...) without spinning up a real GPUI
// window. The shaped-text + paint phases remain GPU-bound and
// are not reachable from the bench surface.
#[cfg(not(feature = "bench-internals"))]
pub(crate) mod editor_element;
#[cfg(feature = "bench-internals")]
pub mod editor_element;

#[cfg(feature = "window")]
pub use window::{document_from_path, run};

/// GPUI peer's typed theme cache.
///
/// Phase 5.7.B.12: real fields land for the surfaces the
/// binary currently paints (background, foreground, status
/// line, cursor inversion). Stored as `Rgba` so the render
/// hot path is a direct `.bg(self.theme.background)` etc. --
/// no per-frame conversion.
///
/// The defaults match the Catppuccin Mocha-ish palette the
/// binary's render used inline pre-5.7.B.12. Future host-side
/// slices will grow `host_theme` to carry window bg / fg /
/// status / cursor fields; once that lands,
/// [`GpuiApp::rebuild_gpui_theme`] reads from there and
/// `RendererSignal::ThemeChanged` will visibly recolor the
/// window on every `:set ui.*`. For now the rebuild is a
/// shape-only no-op -- the wiring is what this slice unblocks.
///
/// Why `Rgba` and not `Hsla`: gpui's `rgb(0x...) -> Rgba`
/// builder is the natural literal form; `.bg(Rgba)` /
/// `.text_color(Rgba)` accept it directly via `Into<Background>`
/// / `Into<Hsla>` blanket impls. Sticking with `Rgba` keeps the
/// theme literal-friendly + the binary's render free of
/// conversion boilerplate.
/// Color storage discipline: the lib stores `u32` packed
/// `0xRRGGBB` hex so it builds without the `window` feature (no
/// transitive `gpui` link). The binary converts to `gpui::Rgba`
/// at render time via `gpui::rgb(theme.background)`.
#[derive(Debug, Clone)]
pub struct GpuiTheme {
    /// Font family name for the editor window. Must be a monospace
    /// typeface. Configurable via `ui.font_family`; defaults to
    /// "Menlo" (built-in macOS monospace). Updated by
    /// `rebuild_gpui_theme` on every `RendererSignal::ThemeChanged`.
    pub font_family: String,
    /// Font size in points. Configurable via `ui.font_size`.
    pub font_size_pt: u32,
    /// Main document background.
    pub background: u32,
    /// Main document foreground (text + non-cursor chars).
    pub foreground: u32,
    /// Status-line background.
    pub status_background: u32,
    /// Status-line foreground.
    pub status_foreground: u32,
    /// Cursor block background (Block shape) + bar / underline
    /// border color (Bar / Underline shapes).
    pub cursor_background: u32,
    /// Cursor block foreground -- the character color when the
    /// block inverts (Normal/Visual mode cursor cell). Unused
    /// by Bar / Underline since the underlying char text stays
    /// the document foreground.
    pub cursor_foreground: u32,
    /// Popup-overlay background (DisplayBuffer help surface).
    /// Slightly darker than the main bg for visual separation.
    pub popup_background: u32,
    /// Popup-overlay border / accent color.
    pub popup_border: u32,
    /// Issue #35 (2026-05-22): picker match-range highlight
    /// color. Painted on the substring of each candidate that
    /// matched the query. Catppuccin Mocha peach by default —
    /// distinct from `foreground` so matches actually pop
    /// (previously used `cursor_background` which equals
    /// `foreground` in the default palette and made matches
    /// invisible).
    pub picker_match_highlight: u32,
    /// Picker marginalia (annotation / kind glyph) color.
    /// Mid-grey by default so it doesn't compete with the
    /// candidate text.
    pub picker_marginalia_fg: u32,
}

impl Default for GpuiTheme {
    fn default() -> Self {
        Self {
            font_family: String::from("Menlo"),
            font_size_pt: 14,
            // Catppuccin Mocha base.
            background: 0x1e1e2e,
            // Catppuccin Mocha text.
            foreground: 0xcdd6f4,
            // Catppuccin Mocha surface0.
            status_background: 0x313244,
            // Catppuccin Mocha green.
            status_foreground: 0xa6e3a1,
            // Catppuccin Mocha text (matches block-cursor "highlight").
            cursor_background: 0xcdd6f4,
            // Catppuccin Mocha base (inverted, for block-cursor char).
            cursor_foreground: 0x1e1e2e,
            // Catppuccin Mocha mantle (deeper than base).
            popup_background: 0x181825,
            // Catppuccin Mocha lavender (accent).
            popup_border: 0xb4befe,
            // Issue #35: Catppuccin Mocha peach — bright accent
            // distinct from `foreground` (text). Highly
            // visible against both light and dark backgrounds.
            picker_match_highlight: 0xfab387,
            // Catppuccin Mocha overlay1 — mid-grey for
            // marginalia / annotations. Softer than
            // `popup_border` (which doubles as the popup
            // accent) so kind glyphs don't dominate the row.
            picker_marginalia_fg: 0x7f849c,
        }
    }
}

/// Stub GPUI pane-render registry. Implements [`ProviderLookup`] so
/// the host's mode-walk already routes through it; storage fills with
/// the GPUI-typed render fn signature in 5.8+.
#[derive(Default)]
pub struct GpuiPaneRenderRegistry {
    /// Forward-compat placeholder. Replaced by
    /// `HashMap<ModeId, GpuiPaneRenderProvider>` once the GPUI
    /// render fn shape stabilises.
    _registered: std::collections::HashSet<ModeId>,
}

impl ProviderLookup for GpuiPaneRenderRegistry {
    fn has_provider(&self, mode: ModeId) -> bool {
        self._registered.contains(&mode)
    }
}

/// GPUI renderer marker. Holds no state — the renderer-trait
/// associated types point at the renderer-specific theme +
/// pane-render registry types this crate owns.
pub struct GpuiRenderer;

impl Renderer for GpuiRenderer {
    type Theme = GpuiTheme;
    type PaneRenderRegistry = GpuiPaneRenderRegistry;
}

/// The GPUI-side renderer composition root. Mirrors
/// `lattice-ui-tui::app::App` in shape: renderer-side caches
/// plus the renderer-neutral [`Editor`]. A future `lsp_file_watcher`
/// field joins when the LSP runtime adapter for GPUI lands.
pub struct GpuiApp {
    /// Slice 3c.final.E.swap: cfg-gated. Production builds hold an
    /// [`EditorActorHandle`]; test builds keep direct `Editor`
    /// ownership for fixtures that mutate state without going
    /// through the dispatch path. Same shape as TUI peer's `App`.
    #[cfg(not(test))]
    pub editor_actor: lattice_host::editor_actor::EditorActorHandle,
    #[cfg(test)]
    pub editor: Editor,
    /// Phase 5.8.AF.5 / Slice 3c.atomic.K: renderer-side clone of
    /// the editor's `RenderState` cell. Parallel of the TUI peer's
    /// `App.render_state` field (3c.atomic.A): isolates the
    /// renderer's read path from `self.editor` so the eventual
    /// `App.editor: Editor → handle` swap doesn't disturb call
    /// sites. Today both Arc handles point at the same underlying
    /// `ArcSwap<RenderState>` instance — readers observe identical
    /// values byte-for-byte regardless of which they go through.
    pub render_state: Arc<arc_swap::ArcSwap<RenderState>>,
    pub theme: GpuiTheme,
    pub pane_render_registry: GpuiPaneRenderRegistry,
    // Phase 5.8.AE: `popup_content` retired. Popup state is
    // unified in `editor.popup_buffer` (+ buffer-locals for
    // links/anchors/highlights). The binary's render reads
    // `editor.popup_help()` for the buffer and the host
    // accessors for metadata, so both renderer peers paint
    // popups from the same source-of-truth state.
}

impl GpuiApp {
    /// Build the GPUI peer's composition root from an initial
    /// [`Document`]. Mirrors `lattice-ui-tui::app::App::new`:
    /// delegates the renderer-neutral construction to
    /// [`Editor::boot`] (LSP subsystem, command / mode /
    /// completion / picker / config registries, snippet handle,
    /// event bus, syntax, buffer registry) and supplies the
    /// renderer-side caches alongside.
    ///
    /// Real dispatch (key events -> `Action` -> `editor.dispatch`)
    /// + paint (read `editor.document` snapshot + cursor) wire in
    /// 5.7.B.3 / 5.7.B.4. The renderer-side post-boot helpers
    /// the TUI peer runs after `Editor::boot`
    /// (`activate_major_for_buffer_kind`,
    /// `publish_document_opened_for_active`,
    /// `ensure_named_synthetic_document`,
    /// `ensure_messages_buffer`) plug in as the corresponding
    /// GPUI wiring lands -- their bodies are host-resident, so
    /// this peer will call the same methods via its own renderer-
    /// signal handler.
    pub fn new(document: Document) -> Self {
        let mut editor = Editor::boot(document);
        let render_state = editor.render_state.clone();
        // Slice 3c.final.E.swap: run boot-time setup directly on
        // the owned Editor BEFORE handing it to the actor. The
        // finalize_boot body's host-routed work runs here as
        // direct Editor calls. Renderer-side post-actor wiring
        // (theme rebuild) runs after the App is built.
        editor.rebuild_option_cache();
        if editor.viewport_height == 0 {
            editor.viewport_height = 30;
        }
        let doc_id = editor.document_buffer_id;
        let _ = editor.activate_major_for_buffer_kind(doc_id, BufferKind::Document);
        editor.publish_document_opened_for_active();
        editor.ensure_subsystem_buffers();
        let workspace_root = Editor::workspace_root_from_cwd();
        let _ = editor.load_persistent_config(workspace_root.as_deref());
        editor.apply_per_language_toml_overrides();
        // Initial RS publish so `app.ad()` returns boot state.
        editor.publish_render_state();

        // Slice 3c.final.E.swap: hand Editor to the actor (prod)
        // or keep inline (test).
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
            theme: GpuiTheme::default(),
            pane_render_registry: GpuiPaneRenderRegistry::default(),
        };
        // App-side post-actor: rebuild the cached GPUI theme from
        // the freshly-published `render_state.theme`.
        app.rebuild_gpui_theme();
        app
    }

    /// Phase 5.8.AF.5 / Slice 3c.atomic.K: wait-free snapshot of
    /// the active-buffer hot-path render state. Returns an `Arc`
    /// clone of `RenderState.active_document`. Parallel of the
    /// TUI peer's `App::ad()` -- the GPUI peer reads its
    /// renderer-side state through this method so the eventual
    /// `editor → handle` swap leaves call sites untouched.
    pub fn ad(&self) -> Arc<ActiveDocumentRenderState> {
        self.render_state.load().active_document.clone()
    }

    /// Slice 3c.final.B.7: published echo-area state — parallel
    /// of TUI peer's `App::messages()`.
    pub fn messages(&self) -> Arc<lattice_host::render_state::MessagesRenderState> {
        self.render_state.load().messages.clone()
    }

    /// Slice 3c.final.B.7: published modeline + cmdline + search
    /// state — parallel of TUI peer's `App::modeline()`.
    pub fn modeline(&self) -> Arc<lattice_host::render_state::ModelineRenderState> {
        self.render_state.load().modeline.clone()
    }

    /// Slice 3c.final.B.10: published typed-options registry —
    /// parallel of TUI peer's `App::options()`.
    pub fn options(&self) -> Arc<lattice_host::render_state::OptionsRenderState> {
        self.render_state.load().options.clone()
    }

    /// Slice 3c.final.B.11: published active-modes map —
    /// parallel of TUI peer's `App::modes()`.
    pub fn modes(&self) -> Arc<lattice_host::render_state::ModesRenderState> {
        self.render_state.load().modes.clone()
    }

    /// Slice 3c.final.B.9: published buffer-locals map —
    /// parallel of TUI peer's `App::buffer_locals()`.
    pub fn buffer_locals(
        &self,
    ) -> Arc<lattice_host::render_state::BufferLocalsRenderState> {
        self.render_state.load().buffer_locals.clone()
    }

    /// Phase 5.8.AF.5 / Slice 3c.final.E.4: routing helpers for
    /// editor mutations. Same shape as the TUI peer's
    /// `App::mutate_editor` / `App::mutate_editor_with`. Pre-swap
    /// runs the closure against `&mut self.editor` + publishes RS;
    /// post-swap delegates to the actor handle. Forward-compatible
    /// `Send + 'static` bounds.
    pub fn mutate_editor<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Editor) + Send + 'static,
    {
        #[cfg(not(test))]
        {
            self.editor_actor
                .mutate_blocking(Box::new(f))
                .expect("editor actor alive");
        }
        #[cfg(test)]
        {
            f(&mut self.editor);
            self.editor.publish_render_state();
        }
    }

    /// Variant for closures that return a value.
    pub fn mutate_editor_with<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Editor) -> R + Send + 'static,
        R: Send + 'static,
    {
        #[cfg(not(test))]
        {
            self.editor_actor.mutate_blocking_with(f)
        }
        #[cfg(test)]
        {
            let r = f(&mut self.editor);
            self.editor.publish_render_state();
            r
        }
    }

    /// Read-only helper -- parallels `App::read_editor` on the TUI
    /// peer. In production routes through the actor handle's
    /// `with_editor` blocking RPC; in tests calls directly.
    pub fn read_editor<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&Editor) -> R + Send + 'static,
        R: Send + 'static,
    {
        #[cfg(not(test))]
        {
            self.editor_actor.with_editor(f)
        }
        #[cfg(test)]
        {
            f(&self.editor)
        }
    }

    /// Phase 5.8.AF.5 / Slice 3c.atomic.K: publishing wrapper
    /// around `editor.viewport_height` writes. Parallel of the
    /// TUI peer's `App::set_viewport_height` (3c.atomic.D): the
    /// per-frame viewport recompute happens outside dispatch, so
    /// it must republish render-state for `ad().viewport_height`
    /// and `ad().scroll` to observe the new values. Without this
    /// wrapper, a window resize would leave the published
    /// viewport / scroll one frame behind every time.
    pub fn set_viewport_height(&mut self, height: u32) {
        // Slice 3c.final.C: route through dispatch so the
        // mutation goes via `Action::SetViewportHeight`. Dispatch
        // tail publishes RenderState; no manual publish needed.
        self.dispatch_action(lattice_host::action::Action::SetViewportHeight(height));
    }

    /// Issue #25 (2026-05-22): per-pane geometry hand-off. The
    /// renderer's per-frame layout pass fires one of these per
    /// leaf in the pane tree. Production routes through the
    /// editor actor's typed `SetPaneViewport` command;
    /// cfg(test) mutates the in-process editor directly
    /// (parallels the `mutate_editor_with` pattern). The
    /// host's handler writes onto `PaneState[idx]` and mirrors
    /// the active leaf's height into `Editor::viewport_height`
    /// for cursor-clamp + highlights worker.
    pub fn set_pane_viewport(&mut self, idx: usize, rows: u32, cols: u32) {
        #[cfg(not(test))]
        {
            let _ = self.editor_actor.set_pane_viewport(idx, rows, cols);
        }
        #[cfg(test)]
        {
            let active_idx = self.editor.pane_tree.active_index();
            let leaves = self.editor.pane_tree.leaves_mut();
            if idx < leaves.len() {
                leaves[idx].viewport_height = rows.max(1);
                leaves[idx].viewport_width = cols.max(1);
            }
            if idx == active_idx {
                self.editor.viewport_height = rows.max(1);
                self.editor.ensure_cursor_visible();
            }
            self.editor.publish_render_state();
        }
    }

    /// Dismiss any active help popup. Phase 5.8.AE: routes
    /// through the host's `dismiss_popup` so popup state lands
    /// uniformly across both peers. Slice 3c.final.C: now goes
    /// through `Action::DismissPopup` instead of a direct host
    /// call.
    pub fn dismiss_popup(&mut self) {
        self.dispatch_action(lattice_host::action::Action::DismissPopup);
    }

    /// X2.6 compatibility shim: the background `highlights_worker`
    /// now keeps the active-pane span cell up to date; the GPUI
    /// peer no longer needs to drive a synchronous recompute. Kept
    /// as a no-op so any external caller (smoke tests, demo code)
    /// continues to compile. Production paint reads through
    /// `render_state.syntax.visible_spans.load()`.
    pub fn refresh_highlights(&mut self) {
        // No-op; the worker subscribes to `Editor::highlight_wake`
        // and republishes on every `publish_render_state`.
    }

    /// Auto-scroll the active pane so the cursor stays in the
    /// visible viewport (`[scroll, scroll + viewport_height)`).
    /// Mirrors the TUI peer's per-tick scroll-clamp; both renderer
    /// peers funnel through the host's `editor.scroll` so any
    /// host-side cursor jump (a motion, a `:N`, a search bounce)
    /// is reflected here on the next paint.
    ///
    /// Phase 5.8.O: foundational for cursorline / visual selection
    /// (5.8.P, 5.8.Q) — those features paint *within* the viewport
    /// and would mis-render if the cursor sat outside it.
    pub fn ensure_cursor_in_viewport(&mut self) {
        // Slice 3c.final.C: route through `Action::EnsureCursorVisible`
        // so the per-frame mutation goes via dispatch. Tail
        // publishes RS — no manual publish needed.
        self.dispatch_action(lattice_host::action::Action::EnsureCursorVisible);
    }

    // Slice 3c.final.E.swap: `finalize_boot` retired. Its body
    // (rebuild_option_cache / activate_major_for_buffer_kind /
    // publish_document_opened_for_active / ensure_subsystem_buffers
    // / load_persistent_config / apply_per_language_toml_overrides)
    // was inlined into `GpuiApp::new`, running directly on the
    // owned Editor BEFORE the actor spawns — the App-side
    // wrappers would have funnelled through the actor mailbox
    // which doesn't exist yet at that point in construction.
    // `rebuild_gpui_theme` runs post-actor against the published
    // RS theme.

    /// Renderer-side handler for the [`RendererSignal`] stream
    /// the host emits from option cascades + mode lifecycle
    /// helpers. Phase 5.7.B.7 wiring -- mirrors the TUI peer's
    /// `App::handle_renderer_signal` in shape; each arm's body
    /// reflects the GPUI peer's current capability surface.
    ///
    /// As GPUI-side infrastructure grows (theme cache, popup +
    /// pane display, file-tree buffers, window-close bridge),
    /// the corresponding arms light up. The signal contract
    /// itself is stable -- the host doesn't know which renderer
    /// is on the other side; both peers see the same enum.
    pub fn handle_renderer_signal(&mut self, signal: RendererSignal) {
        match signal {
            // 5.7.B.12: the cache holds real `Rgba` fields now,
            // and the binary's render reads from `self.theme.*`
            // instead of inline rgb literals. Rebuilding from
            // `editor.host_theme` still needs window-bg / -fg /
            // -cursor mappings on the host side, so this arm
            // calls `rebuild_gpui_theme()` but the body is
            // currently a shape-only no-op. Once those host
            // mappings land, a `:set ui.bg=...` cascade will
            // visibly recolor the window on the next frame.
            RendererSignal::ThemeChanged => {
                self.rebuild_gpui_theme();
                tracing::debug!(
                    "ThemeChanged signal: rebuild_gpui_theme called (host_theme→GpuiTheme mapping is a stub for now)"
                );
            }
            // `editor.should_quit` was set alongside the signal
            // emission (see `Action::Quit` in `Editor::dispatch`).
            // The GPUI binary polls `should_quit` after each
            // `dispatch_keystroke` and calls `cx.quit()` from the
            // event handler -- the lib has no `gpui::App` context
            // to drive shutdown from here.
            RendererSignal::Quit => {
                tracing::debug!("Quit signal: editor.should_quit set; binary polls per-event");
            }
            // The TUI peer's file-tree buffers embed nerd-font
            // glyphs in their rendered rope, so a palette flip
            // needs a rope refresh. The GPUI peer doesn't have
            // file-tree buffers (or rope-side icon embedding)
            // yet; the toggle is read live from
            // `editor.host_theme.nerd_fonts` when those views
            // exist. No-op for now -- the host emits this
            // alongside `ThemeChanged` so renderers that don't
            // track file-tree state can drop it.
            RendererSignal::NerdFontsToggled => {
                tracing::debug!(
                    "NerdFontsToggled signal: no file-tree buffers in GPUI peer; no-op"
                );
            }
            // The fan-out body migrated to
            // `Editor::fan_out_did_change_configuration` in
            // Phase 5.7.B.7; both peers route through the same
            // LSP supervisor walk + per-actor `did_change_configuration`.
            RendererSignal::LspConfigChanged(server_id) => {
                self.mutate_editor(move |e| e.fan_out_did_change_configuration(&server_id));
            }
            // 5.7.B.10: surface the help content as a centered
            // popup overlay. Renderer-side state is just
            // `popup_content`; the binary's render reads the
            // field each frame and draws the overlay when
            // `Some`. The user dismisses with `Esc` (binary-
            // side pre-empt before chord dispatch). Future
            // refinement: resolve the request's `category` to a
            // typed `BufferDisplay` preference (popup vs
            // active-pane vs split) -- today's GPUI peer only
            // implements the popup surface, so every category
            // collapses to a popup overlay.
            RendererSignal::DisplayBuffer(req) => {
                // Phase 5.8.AE: route through host's display_buffer
                // so the popup state lands on `editor.popup_buffer`.
                // The binary's render reads `editor.popup_buffer` +
                // `editor.popup_help()` instead of the now-retired
                // `popup_content` field — keeping the popup state
                // unified across both peers.
                let lattice_host::dispatch::DisplayBufferRequest { content, category } = *req;
                tracing::debug!(
                    category = ?category,
                    title = %content.buffer.title,
                    "DisplayBuffer signal: routing through editor.display_buffer"
                );
                let (_id, mode_signals) = self.mutate_editor_with(move |e| e.display_buffer(content, category));
                // Mode-activate signals don't recurse meaningfully
                // for the popup paths (no further DisplayBuffer);
                // drain through the handler so any ThemeChanged
                // etc. still propagates.
                for s in mode_signals {
                    tracing::debug!(signal = ?s, "popup mode-activate cascade signal");
                }
            }
        }
    }

    /// Convenience entry point for GPUI's key-down event handler:
    /// `dispatch_keystroke(&ev.keystroke.key, mods.control, mods.alt,
    /// mods.shift, mods.platform)`. Walks the same pipeline as the
    /// TUI peer (chord normalisation → renderer-neutral `translate`
    /// → `editor.dispatch`); returns the [`DispatchOutcome`] for
    /// callers that want to inspect renderer signals (or
    /// [`None`] if the key string didn't map to a chord).
    ///
    /// Phase 5.7.B.3 scope: the outcome's renderer signals are
    /// returned but not yet fanned out (no GPUI-side equivalent of
    /// `App::handle_renderer_signal` yet). 5.7.B.4+ adds a signal
    /// handler so theme changes, option-cache rebuilds, and major-
    /// mode activations propagate to the renderer caches.
    pub fn dispatch_keystroke(
        &mut self,
        key: &str,
        control: bool,
        alt: bool,
        shift: bool,
        platform: bool,
    ) -> Option<DispatchOutcome> {
        let chord = gpui_chord::from_keystroke(key, control, alt, shift, platform)?;
        Some(self.dispatch_chord(chord))
    }

    /// Translate a canonical [`KeyChord`] into an [`Action`] (via
    /// the renderer-neutral host pipeline) and dispatch it. Used
    /// by [`Self::dispatch_keystroke`] after the GPUI-shaped event
    /// has been normalised, and directly by tests that drive the
    /// editor with synthetic chords.
    ///
    /// Builds [`TranslateContext`] from current `Editor` state.
    /// `chord_capture` is hard-wired to `false` for now: the
    /// `App`-side `chord_capture_active()` predicate in the TUI
    /// peer touches a cmdline arg-slot field that has no GPUI
    /// equivalent yet; when the GPUI peer grows a cmdline this
    /// reads from the same shared editor state.
    pub fn dispatch_chord(&mut self, chord: KeyChord) -> DispatchOutcome {
        // Investigation 2026-05-22: trace partial_chord at
        // dispatch_chord entry so we can see if it survives
        // publishes between keystrokes. info! so it lands in
        // *messages* without needing RUST_LOG.
        {
            let pre_partial = self.render_state.load().translator.partial_chord.clone();
            tracing::info!(
                "[chord-trace] CHORD {:?} partial_chord_from_rs={:?}",
                chord,
                pre_partial,
            );
        }
        let action = {
            // Slice 3c.atomic.K: read value-typed TranslateContext
            // inputs through the published render-state snapshot
            // (`ad()`). Parallel of the TUI peer's runtime.rs
            // migration (3c.atomic.J). The borrowed-reference
            // inputs (`builtins`, `keymap`, `partial_chord`) still
            // come off `self.editor` -- they need `&` lifetimes
            // the published state doesn't carry; they migrate
            // when the actor swap (3c.final) replaces `editor`
            // with a handle.
            // Slice 3c.final.E.5: translator inputs via published
            // substate (same pattern as TUI runtime.rs).
            let ad = self.ad();
            let translator = self.render_state.load().translator.clone();
            let insert_completion_open = self.render_state.load().completion.insert.is_some();
            let ctx = TranslateContext {
                modal: ad.modal,
                builtins: &translator.builtins,
                pending_count: ad.pending_count,
                op_count: ad.op_count,
                recording_macro: ad.macro_recording,
                active_buffer: ad.buffer_kind,
                completion_open: ad.completion_open,
                chord_capture: false,
                picker_open: ad.picker_open,
                insert_completion_open,
                snippet_active: ad.snippet_active,
                keymap: &translator.keymap,
                partial_chord: &translator.partial_chord,
            };
            lattice_host::input::translate(ctx, chord)
        };
        self.dispatch_action(action)
    }

    /// Rebuild the cached [`GpuiTheme`] from
    /// [`editor.host_theme`](lattice_host::editor::Editor).
    /// Mirrors the TUI peer's `App::rebuild_tui_theme`. Called
    /// at boot (inside `finalize_boot`) and on every
    /// [`RendererSignal::ThemeChanged`] so theme cascades
    /// propagate to the GPUI cache without the binary having
    /// to re-read `host_theme` per frame.
    ///
    /// Phase 5.7.B.12: the body is currently a shape-only
    /// no-op -- `host_theme` doesn't yet carry direct window-bg
    /// / fg / status / cursor fields the GPUI peer can map. The
    /// next iteration grows `host_theme` (or adds a GPUI-typed
    /// theme overlay in `lattice-host::ui::theme`) and this
    /// method translates each into `Rgba`. Until then the cache
    /// keeps its `Default` palette; the wiring is what this
    /// slice unblocks.
    pub fn rebuild_gpui_theme(&mut self) {
        // Phase 5.8.K: live-cascade host_theme → GpuiTheme for the
        // fields where the host's `Theme` carries a matching colour
        // today. Fields without a host source (the bulk of the
        // window-only palette: editor bg/fg, cursor inversion,
        // popup chrome) keep their Catppuccin defaults until the
        // host's `Theme` grows window-color fields. The wiring is
        // already in place — `RendererSignal::ThemeChanged` fires
        // this method — so adding host fields later flows through
        // automatically.
        // Slice 3c.final.E.5: theme via published top-level field.
        let host = self.render_state.load().theme;
        let host = &host;
        let defaults = GpuiTheme::default();

        // Status line ↔ host's `pane_status_active` (mirrors the
        // TUI's active-pane status row). Inactive pane status maps
        // to GpuiTheme's `popup_border` accent for now (visually
        // distinct from the active row).
        if let Some(bg) = host.pane_status_active.bg {
            self.theme.status_background = bg.to_rgb_u32(defaults.status_background);
        }
        if let Some(fg) = host.pane_status_active.fg {
            self.theme.status_foreground = fg.to_rgb_u32(defaults.status_foreground);
        }
        // Popup border ↔ host's pane_separator (same conceptual
        // role: thin accent line between visual regions).
        if let Some(fg) = host.pane_separator.fg {
            self.theme.popup_border = fg.to_rgb_u32(defaults.popup_border);
        }
        // Font family + size: read live from the published options
        // config so `:set ui.font_family=...` takes effect on the
        // next frame without restarting.
        let config = &self.render_state.load().options.config;
        if let Some(family) = config
            .get_typed::<lattice_host::ui::theme_options::UiFontFamily>()
        {
            self.theme.font_family = (**family).to_owned();
        }
        if let Some(size) = config
            .get_typed::<lattice_host::ui::theme_options::UiFontSize>()
        {
            self.theme.font_size_pt = (*size).max(4).min(96) as u32;
        }
    }

    /// Dispatch a single [`Action`] through `editor.dispatch` and
    /// drain the deferred-action queue iteratively.
    ///
    /// `editor.dispatch` runs ONE action and returns the outcome;
    /// for many actions (`Action::Invoke` resolving to an
    /// `AppEffect::EnterMode` / `EnterVisual` / etc.) the host
    /// emits a follow-up [`Action`] in
    /// `outcome.next_actions` rather than mutating directly --
    /// see `Editor::apply_app_effect` in `lattice_host::dispatch`.
    /// The TUI peer's `App::apply` drains the queue with a
    /// recursive call; this method does the same shape with an
    /// explicit FIFO loop so basic state transitions land without
    /// needing the renderer's full effect / signal fan-out yet
    /// (5.7.B.4+ work).
    ///
    /// `outcome.effects` are aggregated across the chain but NOT
    /// re-applied -- the host has already called
    /// `editor.handle_effect(effect.clone())` on each one during
    /// the inner dispatch (the renderer-coupled match arms remain
    /// a 5.7.B.4+ follow-up). `renderer_signals` accumulate the
    /// same way; callers can inspect them once the GPUI peer
    /// gains a signal handler.
    pub fn dispatch_action(&mut self, action: Action) -> DispatchOutcome {
        // Phase 5.8.AC.1: GPUI-side intercepts for App-only Action
        // arms. The host's `handle_action` doesn't dispatch these
        // (the TUI App's `apply` catches them with renderer-coupled
        // bodies). Until the full set migrates, the GPUI peer
        // intercepts the most-common ones here. The host's matching
        // `do_*` helpers cover the simple Buffer / Path routings;
        // exotic routings (mark / register / command / snippet /
        // LSP code-action / completion) still warn until they
        // migrate.
        match &action {
            Action::PickerAccept => {
                let signals = self.mutate_editor_with(|e| e.do_picker_accept());
                let mut outcome = DispatchOutcome::default();
                outcome.consumed = true;
                outcome.renderer_signals = signals.clone();
                for s in signals {
                    self.handle_renderer_signal(s);
                }
                return outcome;
            }
            Action::PickerDismiss => {
                let signals = self.mutate_editor_with(|e| e.do_picker_dismiss());
                let mut outcome = DispatchOutcome::default();
                outcome.consumed = true;
                outcome.renderer_signals = signals.clone();
                for s in signals {
                    self.handle_renderer_signal(s);
                }
                return outcome;
            }
            _ => {}
        }
        let mut outcome = self.mutate_editor_with(move |e| e.dispatch(action));
        let mut pending: std::collections::VecDeque<Action> =
            outcome.next_actions.drain(..).collect();
        while let Some(follow_up) = pending.pop_front() {
            let mut next_out = self.mutate_editor_with(move |e| e.dispatch(follow_up));
            pending.extend(next_out.next_actions.drain(..));
            outcome.effects.append(&mut next_out.effects);
            outcome
                .renderer_signals
                .append(&mut next_out.renderer_signals);
            outcome.consumed |= next_out.consumed;
            if self.render_state.load().lifecycle.should_quit {
                // Mirrors `App::apply`'s mid-macro-quit semantic:
                // a recorded `:q` short-circuits the rest of the
                // chain so we don't keep firing actions against
                // an editor that's tearing down.
                break;
            }
        }
        // Phase 5.8.AC.1: drain renderer-coupled effects. The
        // host's `editor.dispatch` already called
        // `editor.handle_effect` for every Effect, but the
        // renderer-coupled tail (`OpenBuffer` → `do_edit`,
        // `OpenBufferPicker` → `do_open_buffer_picker`,
        // `QuitEditor` → `do_quit`, ...) doesn't run inside the
        // host's `handle_effect`. The TUI peer drains these in
        // `App::apply_effect_app_arms`; the GPUI peer mirrors
        // those arms here for the variants whose handlers are
        // host-resident today. Variants whose handlers are still
        // App-only (file-tree / oil / hover / LSP-request /
        // diagnostics navigation / picker accept) log a one-line
        // trace; they'll light up as the matching host methods
        // land in subsequent slices.
        for effect in outcome.effects.iter().cloned() {
            self.apply_effect_gpui(effect);
        }
        // Action arms App handles that the host's `handle_action`
        // doesn't yet route through: `PickerAccept` / `PickerDismiss`
        // — these need a renderer-coupled close path. Until the
        // accept body migrates host-side, GPUI catches the action
        // by inspecting `outcome.consumed` and falling back to a
        // minimal close + activate-restore for the dismiss arm.
        // 5.7.B.7: drain renderer signals through the GPUI
        // handler. Drained BEFORE returning so callers see a
        // post-signal-handling editor state; the returned
        // outcome still carries the (now-handled) signal list
        // for callers that want to inspect or re-route them.
        let signals = std::mem::take(&mut outcome.renderer_signals);
        for signal in signals.iter().cloned() {
            self.handle_renderer_signal(signal);
        }
        // Restore the list on the outcome so the returned value
        // matches the historical TUI shape (App::apply also
        // hands these to the caller after fan-out).
        outcome.renderer_signals = signals;
        // Phase 5.8.AF.5 / Slice X1: drain pending LSP / event /
        // mode-lifecycle results here at the keystroke-driven
        // dispatch tail rather than in the per-frame body
        // (`crates/lattice-ui-gpui/src/window.rs::Render::render`).
        // Paramount goal #1 forbids I/O / event drain on the UI
        // thread; the renderer body is the UI thread.
        // `run_tick_pending` is the host aggregator that polls
        // ~30 channels for async results -- on a busy frame
        // (file open) it can take 49ms, which is 6x over the
        // 8ms-at-120Hz keystroke-to-glyph budget. Running it
        // here makes that cost happen during the keystroke that
        // caused the work (the open) instead of on the next
        // paint after open. The post-X1 perf trace expects
        // `tick_us` in `lattice_gpui::perf` to drop to ~0 once
        // dispatch tails take over the drain.
        //
        // Idle LSP arrivals (response with no keystroke in
        // flight) are NOT drained until the next keystroke:
        // see slice X1b (`docs/dev/operations/render-thread-
        // discipline-remediation.md` §X1b) for the wake-bridge
        // that closes that gap.
        let tick_signals = self.mutate_editor_with(|e| e.run_tick_pending());
        for signal in tick_signals {
            self.handle_renderer_signal(signal);
        }
        outcome
    }

    /// Renderer-coupled effect handler for the GPUI peer.
    ///
    /// Phase 5.8.AC.1: mirrors the role of TUI's
    /// `App::apply_effect_app_arms`. For ex-effects whose
    /// renderer-coupled tail lives host-side today
    /// (`OpenBuffer`, `QuitEditor`, `OpenBufferPicker`), call
    /// the host method and fan signals through the existing
    /// `RendererSignal` handler. For the rest (file-tree, oil,
    /// hover, picker open, LSP requests, diagnostics navigation),
    /// log a one-line trace + continue; those light up as the
    /// matching host methods land in subsequent slices.
    fn apply_effect_gpui(&mut self, effect: lattice_grammar::Effect) {
        use lattice_grammar::Effect;
        match effect {
            // Document-only effects the host has already
            // applied via `editor.handle_effect`; nothing for the
            // renderer to do.
            Effect::None
            | Effect::Edits(_)
            | Effect::SelectionChange(_)
            | Effect::EnterMode(_)
            | Effect::Yank { .. }
            | Effect::SetOption { .. }
            | Effect::ClearSearchHighlight
            | Effect::Echo { .. }
            | Effect::EchoRegisters
            | Effect::EchoMarks
            | Effect::ListBuffers
            | Effect::DescribeBuffer
            | Effect::ListKeymap
            | Effect::DescribeOption { .. }
            | Effect::ListOptions
            | Effect::DescribeOptionResolution { .. }
            | Effect::DescribeEvents
            | Effect::DescribeEvent { .. }
            | Effect::BufferNext
            | Effect::BufferPrev
            | Effect::BufferDelete { .. }
            | Effect::ListModes
            | Effect::DescribeMode { .. }
            | Effect::Customize { .. }
            | Effect::ListDiagnostics
            | Effect::DeleteCurrentLine
            | Effect::Substitute { .. }
            | Effect::DescribeCommand { .. }
            | Effect::Apropos { .. }
            | Effect::DescribeKey { .. }
            | Effect::AppAction(_) => {}
            // Renderer-coupled effects whose body lives host-side.
            Effect::QuitEditor { force } => self.mutate_editor(move |e| e.do_quit(force)),
            Effect::OpenBuffer { path, force } => self.apply_open_buffer(path, force),
            // Phase 5.8.AD.3: `:w` save with full LSP fan-out
            // (BeforeSave / willSave / willSaveWaitUntil /
            // didSave / didCreateFiles) is now host-resident.
            Effect::SaveBuffer { path } => self.mutate_editor(move |e| e.do_write(path)),
            // Phase 5.8.AD.2: LSP commands whose bodies migrated.
            Effect::LspStatus => {
                let signals = self.mutate_editor_with(|e| e.do_lsp_status());
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            Effect::LspRestart { server_id } => {
                self.mutate_editor(move |e| e.do_lsp_restart(&server_id));
            }
            Effect::LspExpandRegion => self.mutate_editor(|e| e.do_lsp_expand_region()),
            Effect::LspShrinkRegion => self.mutate_editor(|e| e.do_lsp_shrink_region()),
            Effect::LspProgressCancel { server_id } => {
                self.mutate_editor(move |e| e.do_lsp_progress_cancel(server_id.as_deref()));
            }
            Effect::SetLspLogLevel { server_id, level } => {
                self.mutate_editor(move |e| e.do_set_lsp_log_level(server_id.as_deref(), &level));
            }
            Effect::LspLogClear { server_id } => {
                self.mutate_editor(move |e| e.do_lsp_log_clear(server_id.as_deref()));
            }
            Effect::LspCodeAction => self.mutate_editor(|e| e.do_lsp_code_action_request()),
            Effect::LspFormat => self.mutate_editor(|e| e.do_lsp_format_request(false)),
            Effect::LspFormatRange => self.mutate_editor(|e| e.do_lsp_format_request(true)),
            Effect::LspRename { new_name } => {
                self.mutate_editor(move |e| e.do_lsp_rename_request(&new_name))
            }
            Effect::LspIncomingCalls => {
                self.mutate_editor(|e| e.do_lsp_call_hierarchy_request(false))
            }
            Effect::LspOutgoingCalls => {
                self.mutate_editor(|e| e.do_lsp_call_hierarchy_request(true))
            }
            Effect::LspSupertypes => {
                self.mutate_editor(|e| e.do_lsp_type_hierarchy_request(false))
            }
            Effect::LspSubtypes => self.mutate_editor(|e| e.do_lsp_type_hierarchy_request(true)),
            Effect::LspMoniker => self.mutate_editor(|e| e.do_lsp_moniker_request()),
            Effect::OpenLspLog { server_id } => {
                self.mutate_editor(move |e| e.do_open_lsp_log(server_id.as_deref()));
            }
            Effect::OpenLspTraceLog { server_id } => {
                self.mutate_editor(move |e| e.do_open_lsp_trace_log(server_id.as_deref()));
            }
            Effect::ToggleLspTrace { server_id } => {
                self.mutate_editor(move |e| e.do_toggle_lsp_trace(&server_id));
            }
            Effect::LspServerLogListing => self.mutate_editor(|e| e.do_lsp_server_log_listing()),
            Effect::LspCodeLens => self.mutate_editor(|e| e.do_lsp_code_lens_picker()),
            Effect::LspColorPresentation => {
                self.mutate_editor(|e| e.do_lsp_color_presentation())
            }
            // Phase 5.8.AD.4: completion / signature / snippet
            // entry points are host-resident; both peers reach
            // them through the same dispatch.
            Effect::LspSignatureHelp => self.mutate_editor(|e| e.lsp_signature_help_request()),
            Effect::LspComplete => self.mutate_editor(|e| e.lsp_completion_request()),
            Effect::SnippetExpand => self.mutate_editor(|e| e.do_snippet_expand_at_cursor()),
            // 5.5.LSP.5: symbol helpers host-side; both peers reach
            // them through the same dispatch.
            Effect::LspDocumentSymbol => self.mutate_editor(|e| e.lsp_document_symbol_request()),
            Effect::LspWorkspaceSymbol { query } => {
                self.mutate_editor(move |e| e.lsp_workspace_symbol_request(&query));
            }
            // Phase 5.8.AD.5: describe / hover / tutor / customize
            // entries now host-resident.
            Effect::OpenHelpTopic { topic } => {
                let signals = self.mutate_editor_with(move |e| e.do_open_help_topic(topic.as_deref()));
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            Effect::OpenHover { markdown } => {
                let signals = self.mutate_editor_with(move |e| e.do_open_hover(&markdown));
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            Effect::CloseHover => {
                // CloseHover is App-side popup state today; GPUI's
                // popup_content covers it directly.
                self.dismiss_popup();
            }
            Effect::Tutor { lesson } => {
                let signals = self.mutate_editor_with(move |e| e.do_tutor(lesson));
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            Effect::OpenBufferPicker => {
                let signals = self.mutate_editor_with(|e| e.do_open_buffer_picker());
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            // Phase 5.8.AF.3: `:messages` activates the
            // `*messages*` Document buffer host-side.
            Effect::OpenMessages => {
                let signals = self.mutate_editor_with(|e| e.do_open_messages());
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            // Phase 5.8.AF.3: diagnostic navigation host-side.
            Effect::NextDiagnostic => self.mutate_editor(|e| e.do_next_diagnostic()),
            Effect::PrevDiagnostic => self.mutate_editor(|e| e.do_prev_diagnostic()),
            // Phase 5.8.AF.3: `:reload-snippets` host-side.
            Effect::ReloadSnippets => self.mutate_editor(|e| e.do_reload_snippets()),
            // Phase 5.8.AF.3: `:toggle-mode` host-side. Returned
            // bool flags an unknown-mode-name miss (the host has
            // already echoed); GPUI has no further fan-out.
            Effect::ToggleMode { mode_name } => {
                let _ = self.mutate_editor_with(move |e| e.toggle_mode_by_name(&mode_name));
            }
            // Phase 5.8.AF.3: `:g` / `:v` host-side. Body effects
            // are drained through `apply_effect_gpui` so any
            // renderer-coupled tail (popup / picker fan-out) still
            // flows through this peer.
            Effect::Global {
                pattern,
                inverted,
                body,
            } => {
                // Slice 3c.final.E.swap: build outcome inside the
                // closure, return owned `DispatchOutcome` from
                // `mutate_editor_with`. Same pattern as TUI edit.rs.
                let mut out = self.mutate_editor_with(move |e| {
                    let mut out = lattice_host::dispatch::DispatchOutcome::default();
                    e.do_global(&pattern, inverted, body.as_ref(), &mut out);
                    out
                });
                for eff in std::mem::take(&mut out.effects) {
                    self.apply_effect_gpui(eff);
                }
            }
            // Phase 5.8.AF.3: `:picker <source>` host-side.
            Effect::OpenPicker { source, args } => {
                let signals = self.mutate_editor_with(move |e| e.open_picker(source, args));
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            // Phase 5.8.AD.1: oil + file-tree migrated host-side
            // so `:e .` / `:Oil` / `:Tree` work in GPUI.
            Effect::OpenOil { dir } => {
                let signals = self.mutate_editor_with(move |e| e.do_open_oil(dir));
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            Effect::OpenFileTree { root } => {
                let signals = self.mutate_editor_with(move |e| e.do_open_file_tree(root));
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            Effect::CloseFileTree => {
                let signals = self.mutate_editor_with(|e| e.dismiss_file_tree());
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            // Many recurses through the same handler so inner
            // arms hit their renderer-coupled tail too.
            Effect::Many(parts) => {
                for p in parts {
                    self.apply_effect_gpui(p);
                }
            }
        }
        // 5.8.AF.3 closeout: every renderer-neutral `Effect` is now
        // explicitly handled. The match is exhaustive — a future
        // variant becomes a compile error rather than a silent
        // runtime warning, which is the louder signal.
    }

    /// Wrapper around `editor.do_edit(path, force)` that translates
    /// the host's [`DoEditOutcome`] for the GPUI peer. Mirrors
    /// TUI's `App::do_edit`: `Directory` routes to `do_open_oil`,
    /// `Opened`/`Activated`/`Reloaded` fan signals through the
    /// renderer handler, `NoFileName`/`Failed` are silent (the
    /// host already echoed).
    fn apply_open_buffer(&mut self, path: Option<std::path::PathBuf>, force: bool) {
        use lattice_host::dispatch::DoEditOutcome;
        let outcome = self.mutate_editor_with(move |e| e.do_edit(path, force));
        match outcome {
            DoEditOutcome::NoFileName | DoEditOutcome::Failed => {}
            DoEditOutcome::Directory(dir) => {
                // Phase 5.8.AD.1: oil is now host-side; route the
                // directory `:e` through the same path the TUI peer
                // uses.
                let signals = self.mutate_editor_with(move |e| e.do_open_oil(Some(dir)));
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
            DoEditOutcome::Reloaded(signals)
            | DoEditOutcome::Activated(signals)
            | DoEditOutcome::Opened(signals) => {
                for s in signals {
                    self.handle_renderer_signal(s);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use lattice_grammar::ModalState;

    /// The 5.7 scaffold's core claim: a GPUI-side composition root
    /// constructs from the host substrate alone. If this builds, the
    /// host crate has no transitive ratatui / crossterm dep — that's
    /// the entire point of the slice.
    ///
    /// 5.7.B.2: `GpuiApp::new(document)` goes through
    /// `Editor::boot`, so this test also exercises that the host's
    /// boot path is callable without `lattice-ui-tui` in the dep
    /// tree (no transitive ratatui / crossterm pull-in).
    #[test]
    fn scaffold_constructs_without_ui_tui() {
        let app = GpuiApp::new(Document::empty());
        // `Editor::boot` populates the mode registry; the entry
        // count is nonzero so the host's mode-registration path
        // ran.
        assert!(app.editor.mode_registry.iter_meta().next().is_some());
        // 5.7.B.6: `GpuiApp::new` now also runs `finalize_boot`
        // which calls `activate_major_for_buffer_kind` for the
        // initial document buffer. The active-modes map should
        // therefore contain an entry for `document_buffer_id`.
        assert!(
            app.editor
                .active_modes
                .contains_key(&app.editor.document_buffer_id),
            "finalize_boot should activate a major mode for the initial document buffer"
        );
        // ProviderLookup is implemented on the registry; the
        // empty stub answers `false` for every mode id.
        let probe: &dyn ProviderLookup = &app.pane_render_registry;
        let dummy = lattice_mode::ModeId::new("__scaffold_probe__");
        assert!(!probe.has_provider(dummy));
    }

    /// End-to-end pipeline assertion: a key string flows through
    /// the GPUI peer's adapter, the host's `translate` path, and
    /// `editor.dispatch`, and the editor's modal state actually
    /// changes. This is the smallest credible "input dispatch
    /// works" test — it doesn't need GPUI's native event types
    /// or a window.
    ///
    /// `"i"` in Normal mode is canonical vim "enter Insert"; if
    /// the pipeline is wired correctly the editor's `modal` ends
    /// up in [`ModalState::Insert`].
    #[test]
    fn dispatch_keystroke_routes_i_to_insert_mode() {
        let mut app = GpuiApp::new(Document::empty());
        assert_eq!(app.editor.modal, ModalState::Normal);
        let outcome = app.dispatch_keystroke("i", false, false, false, false);
        assert!(
            outcome.is_some(),
            "`i` should normalise to a chord and dispatch"
        );
        assert_eq!(app.editor.modal, ModalState::Insert);
    }

    /// `dispatch_chord` is the path tests + programmatic drivers
    /// take. Sanity-check that handing it a canonical chord
    /// reaches the same state change as the keystroke entry
    /// point.
    #[test]
    fn dispatch_chord_drives_normal_to_insert_transition() {
        use lattice_host::chord::{KeyChord, KeyKind, KeyMods};
        let mut app = GpuiApp::new(Document::empty());
        let chord = KeyChord::new(KeyKind::Char('i'), KeyMods::NONE);
        app.dispatch_chord(chord);
        assert_eq!(app.editor.modal, ModalState::Insert);
    }

    /// Test-coverage gap user flagged 2026-05-21: chord sequences
    /// like `gg` had no GPUI end-to-end test. The TUI peer covers
    /// the keymap-handler layer (key_harness_gg_jumps_to_first_line)
    /// but GPUI's specific dispatch_keystroke path — including
    /// partial_chord publish + reload between keys — wasn't
    /// exercised.
    ///
    /// This test drives `gg` through `dispatch_keystroke`, the
    /// SAME path GPUI's `on_key_down` uses. If `partial_chord`
    /// doesn't survive the publish-and-reload cycle between the
    /// two `g` presses, the second `g` won't see the first one
    /// as prefix and dispatch will fail.
    #[test]
    fn gpui_dispatch_keystroke_handles_gg_chord_sequence() {
        // Seed a multi-line document so `gg` has somewhere to
        // jump to.
        let doc = Document::from_text("alpha\nbeta\ngamma\ndelta\nepsilon\n");
        let mut app = GpuiApp::new(doc);
        // Move cursor down 2 lines so `gg` has work to do.
        app.dispatch_keystroke("j", false, false, false, false);
        app.dispatch_keystroke("j", false, false, false, false);
        assert_eq!(
            app.editor.cursor.line, 2,
            "cursor should be on line 2 before gg"
        );

        // First `g`: should absorb into partial_chord, NOT execute.
        let outcome1 = app.dispatch_keystroke("g", false, false, false, false);
        assert!(outcome1.is_some(), "first `g` should dispatch");
        assert_eq!(
            app.editor.partial_chord.len(),
            1,
            "first `g` should populate partial_chord"
        );
        assert_eq!(
            app.editor.cursor.line, 2,
            "first `g` alone should NOT move the cursor"
        );

        // Verify the published RS reflects the partial_chord update
        // — this is the key invariant. If RS still shows empty
        // partial_chord, the second `g` won't find the prefix and
        // dispatch silently fails.
        assert_eq!(
            app.render_state.load().translator.partial_chord.len(),
            1,
            "published RS must observe the partial_chord update before next keystroke"
        );

        // Second `g`: with [g] as prefix, trie resolves gg →
        // motion:goto-first-line.
        let outcome2 = app.dispatch_keystroke("g", false, false, false, false);
        assert!(outcome2.is_some(), "second `g` should dispatch");
        assert_eq!(
            app.editor.cursor.line, 0,
            "gg should jump to line 0; got line {}",
            app.editor.cursor.line
        );
        assert!(
            app.editor.partial_chord.is_empty(),
            "partial_chord should clear after gg resolves"
        );
    }

    /// Test-coverage gap (sibling to gg): `zz` / `zt` / `zb`.
    /// These also live behind a chord-prefix state machine; the
    /// `z` keystroke absorbs into partial_chord, then the second
    /// keystroke (`z`/`t`/`b`) selects the action.
    #[test]
    fn gpui_dispatch_keystroke_handles_zz_chord_sequence() {
        // Long enough document that zz centering would be
        // observable (we don't assert viewport position, just
        // that the action dispatched and partial_chord clears).
        let doc = Document::from_text(&"line\n".repeat(20));
        let mut app = GpuiApp::new(doc);

        let outcome1 = app.dispatch_keystroke("z", false, false, false, false);
        assert!(outcome1.is_some(), "first `z` should dispatch");
        assert_eq!(
            app.editor.partial_chord.len(),
            1,
            "first `z` should populate partial_chord"
        );
        assert_eq!(
            app.render_state.load().translator.partial_chord.len(),
            1,
            "published RS must observe the z partial_chord update"
        );

        let outcome2 = app.dispatch_keystroke("z", false, false, false, false);
        assert!(outcome2.is_some(), "second `z` should dispatch");
        assert!(
            app.editor.partial_chord.is_empty(),
            "partial_chord should clear after zz resolves"
        );
    }

    /// Slice 3c.atomic.K: `GpuiApp::ad()` reflects the modal
    /// transition driven by dispatch. Proves the renderer-side
    /// publish chain (`dispatch_chord` → `dispatch_action` →
    /// dispatch tail `publish_render_state()`) reaches the
    /// `GpuiApp.render_state` cell, so paint-time reads through
    /// `ad()` see the freshest editor state.
    #[test]
    fn gpui_app_ad_reflects_dispatched_state() {
        use lattice_host::chord::{KeyChord, KeyKind, KeyMods};
        let mut app = GpuiApp::new(Document::empty());
        // Boot publish hydrates ad() before any dispatch.
        assert_eq!(app.ad().modal, ModalState::Normal);
        app.dispatch_chord(KeyChord::new(KeyKind::Char('i'), KeyMods::NONE));
        assert_eq!(
            app.ad().modal,
            ModalState::Insert,
            "ad() must observe the post-dispatch modal state through render_state"
        );
    }

    /// Slice 3c.atomic.K: `set_viewport_height` clamps height
    /// to a minimum of 1 and republishes so `ad().viewport_height`
    /// observes the change. Matches the contract the TUI peer's
    /// `App::set_viewport_height` exposes (3c.atomic.D).
    #[test]
    fn gpui_app_set_viewport_height_publishes() {
        let mut app = GpuiApp::new(Document::empty());
        app.set_viewport_height(24);
        assert_eq!(app.ad().viewport_height, 24);
        // Clamp: 0 becomes 1 (renderer's per-frame layout never
        // gives the buffer a zero-height pane, but the wrapper's
        // contract guarantees a usable lower bound regardless).
        app.set_viewport_height(0);
        assert_eq!(app.ad().viewport_height, 1);
    }
}
