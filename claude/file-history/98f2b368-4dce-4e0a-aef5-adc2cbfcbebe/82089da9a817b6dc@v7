//! GPUI window-opening entry point — [`run`] opens a native
//! GPUI window, constructs an [`EditorView`], drives the event
//! loop, and tears down cleanly on `editor.should_quit`.
//!
//! Phase 5.9 migration: the body used to live in
//! `src/bin/lattice_gpui.rs`. Moving it into the lib (behind
//! the `window` Cargo feature) lets `lattice-cli` route through
//! a single `lattice --gpu` flag without duplicating the
//! window setup. The `lattice-gpui` binary becomes a thin
//! shim: tracing init + CLI arg parse + [`run`].
//!
//! The whole module is `#[cfg(feature = "window")]`-gated by
//! `lib.rs`, so `cargo test -p lattice-ui-gpui` on a headless
//! host (without display libs) still works — gpui only links
//! when the `window` feature is on.
//!
//! ## Pipeline
//!
//! 1. [`run`] takes a [`Document`] (path-bearing or empty),
//!    constructs `gpui::Application::new()`, and opens a 720×480
//!    centered window whose root element is an [`EditorView`].
//! 2. [`EditorView::new`] calls `GpuiApp::new(document)`, which
//!    delegates to [`lattice_host::editor::Editor::boot`] for the
//!    full renderer-neutral setup (LSP, command + mode + snippet
//!    registries, syntax handle, buffer registry, persistent
//!    config, picker registry, ...).
//! 3. After `cx.open_window` returns, we do a second
//!    `window.update(cx, ...)` to focus the editor view + call
//!    `cx.activate(true)`. The focus call HAS to happen after
//!    `open_window` -- calling it inside the builder closure
//!    runs before gpui's focus tree is fully initialised and is
//!    silently dropped. The pattern matches gpui's
//!    `examples/input.rs`.
//! 4. Keystrokes flow `on_key_down` → `dispatch_keystroke` →
//!    `editor.dispatch` → renderer signals + next-action chain;
//!    `cx.notify()` schedules a repaint; `cx.stop_propagation()`
//!    prevents the platform default action map from claiming the
//!    chord.
//! 5. `editor.should_quit` → `cx.quit()` tears the application
//!    down.
//!
//! ## What renders today
//!
//! - **Document area**: per-character cells laid out under
//!   `flex_row` lines + `flex_col` columns on a monospace font,
//!   with a left-side line-number gutter (5.8.D). Syntax
//!   highlights (5.8.A) color each span via the Catppuccin Mocha
//!   palette when an `editor.syntax` handle exists.
//! - **Vim-style cursor**: three shapes via [`CursorShape`]
//!   (block / bar / underline) based on `editor.modal` (5.7.B.11).
//! - **Status line / minibuffer**: shows
//!   `<MODAL>   <path>[+]   L:<n>  C:<n>` (5.8.B), or the
//!   in-progress `:command` / `/pattern` line when in Command /
//!   Search modes (5.8.C).
//! - **DisplayBuffer popup** (5.7.B.10): centered overlay for
//!   `:ls`, `:describe-buffer`, etc. Esc dismisses.
//! - **Picker overlay** (5.8.E): for `:picker files`,
//!   `:picker commands`.
//! - **Insert-mode completion popup** (5.8.F): top-right
//!   anchored panel.

use anyhow::{Context as _, Result};
use gpui::{
    App, AppContext, Application, Bounds, Context, FocusHandle, Focusable, InteractiveElement,
    IntoElement, KeyDownEvent, ParentElement, Render, SharedString, Styled, TextRun, Window,
    WindowBounds, WindowOptions, div, font, px, rgb, size,
};
use lattice_core::Document;
use lattice_core::ui::pane::{PaneNode, PaneState};
use lattice_grammar::ModalState;
use lattice_host::cursor_shape::CursorShape;
use lattice_host::per_buffer_cache::PerBufferCacheExt;
use lattice_syntax::Style as SyntaxStyle;

use crate::{GpuiApp, GpuiTheme};

// Phase 5.8.AF.5 / Slice X3.full.2: `CellStyle` + `run_to_cell`
// (per-cell styled-Div construction) deleted -- replaced by the
// shaping-layer logic in `crate::editor_element::build_text_runs`.
// `syntax_color` + `style_at` are kept because the popup-overlay
// renderer below still walks chars and emits one Div per cell.
// Slice X3.full.3 absorbs the popup into a shaped-line path; at
// that point these helpers move into `editor_element` too.

/// Adapter: host-canonical [`Theme::syntax_style`] -> packed 24-bit
/// `0xRRGGBB`. Phase 5.8.AF.6 / issue-2 hoist: prior to this the
/// GPUI peer carried its own Catppuccin Mocha hex table that
/// diverged from the TUI's named-ANSI table. Both peers now route
/// through `lattice_host::ui::theme::Theme::syntax_style`, so a
/// single edit reflects everywhere.
///
/// `Theme::default()` is used today because per-instance theme
/// customization for syntax styles isn't wired through the cmdline
/// yet. The fallback when `fg` is unset is the Catppuccin Text
/// (`0xcdd6f4`) — matches what `SyntaxStyle::Default` resolves to
/// and what `EditorElement` paints on un-spanned bytes.
fn syntax_color(style: SyntaxStyle) -> u32 {
    let host_default = lattice_host::ui::theme::Theme::default();
    let host_style = host_default.syntax_style(style);
    host_style
        .fg
        .map(|c| c.to_rgb_u32(0xcdd6f4))
        .unwrap_or(0xcdd6f4)
}

/// Walk `spans` (one entry per line) and find the `Style` that
/// covers `byte`.
fn style_at(spans: &[lattice_syntax::StyledSpan], byte: usize) -> SyntaxStyle {
    for span in spans {
        if byte >= span.start && byte < span.end {
            return span.style;
        }
    }
    SyntaxStyle::Default
}

/// Reads the typed `picker.display` option and returns `true` iff
/// the user wants the vertico-style minibuffer layout (default).
/// Unknown / missing values fall back to the design default rather
/// than panicking -- the validator already gates set-time. Matches
/// the TUI peer's `picker_display_is_minibuffer` so both renderers
/// agree on the same source of truth.
/// Issue #25 (2026-05-22): per-pane geometry collector. Walks
/// the pane tree distributing the container's pixel rectangle
/// to each leaf based on split orientation, then converts each
/// leaf's allocated pixels into `(rows, cols)` for the host's
/// `PaneState.viewport_height` / `viewport_width` fields.
///
/// - `HorizontalSplit { top, bottom }` divides HEIGHT between
///   children; both get the full width.
/// - `VerticalSplit { left, right }` divides WIDTH between
///   children; both get the full height.
/// - `Leaf` consumes per-leaf chrome (status row + py + p_3
///   padding vertically; p_3 padding horizontally), converts
///   to row/col count via `row_px` / `col_px`.
///
/// Each leaf's `(pane_idx, rows, cols)` is pushed into `out`.
/// The caller fires `editor_actor.set_pane_viewport(idx, rows,
/// cols)` once per entry — the host writes per-pane values
/// into PaneState and mirrors the active pane's height into
/// `Editor::viewport_height` for cursor-clamp / highlights
/// worker.
fn collect_pane_geometries(
    node: &lattice_core::ui::pane::PaneNode,
    available_w_px: f32,
    available_h_px: f32,
    per_leaf_v_chrome_px: f32,
    per_leaf_h_chrome_px: f32,
    row_px: f32,
    col_px: f32,
    out: &mut Vec<(usize, u32, u32)>,
) {
    use lattice_core::ui::pane::PaneNode;
    match node {
        PaneNode::Leaf(idx) => {
            let usable_h = (available_h_px - per_leaf_v_chrome_px).max(0.0);
            let usable_w = (available_w_px - per_leaf_h_chrome_px).max(0.0);
            let rows = (usable_h / row_px).floor().max(1.0) as u32;
            let cols = (usable_w / col_px).floor().max(1.0) as u32;
            out.push((*idx, rows, cols));
        }
        PaneNode::HorizontalSplit { top, bottom, ratio } => {
            // Issue #28: ratio-aware split (0.5 = even).
            let top_h = available_h_px * *ratio;
            let bot_h = available_h_px - top_h;
            collect_pane_geometries(
                top,
                available_w_px,
                top_h,
                per_leaf_v_chrome_px,
                per_leaf_h_chrome_px,
                row_px,
                col_px,
                out,
            );
            collect_pane_geometries(
                bottom,
                available_w_px,
                bot_h,
                per_leaf_v_chrome_px,
                per_leaf_h_chrome_px,
                row_px,
                col_px,
                out,
            );
        }
        PaneNode::VerticalSplit { left, right, ratio } => {
            let left_w = available_w_px * *ratio;
            let right_w = available_w_px - left_w;
            collect_pane_geometries(
                left,
                left_w,
                available_h_px,
                per_leaf_v_chrome_px,
                per_leaf_h_chrome_px,
                row_px,
                col_px,
                out,
            );
            collect_pane_geometries(
                right,
                right_w,
                available_h_px,
                per_leaf_v_chrome_px,
                per_leaf_h_chrome_px,
                row_px,
                col_px,
                out,
            );
        }
    }
}

fn picker_display_is_minibuffer(app: &GpuiApp) -> bool {
    // Slice 3c.final.B.10: typed-options registry via published
    // `options()` sub-state — wait-free Arc clone.
    app.options()
        .config
        .get_typed::<lattice_config::core_options::PickerDisplay>()
        .map(|s| s.as_str() != "popup")
        .unwrap_or(true)
}

/// Slice `3c.unify.gpui-annotation-render`: shared candidate-row
/// builder for the picker (minibuffer + overlay) and cmdline-
/// completion (minibuffer + overlay) surfaces. Replaces four
/// near-identical inline row builders.
///
/// Layout: `[display + match highlights]   [annotations]` —
/// candidate text on the left, annotations right-aligned in a
/// dimmer colour. Matches the TUI peer's `candidate_to_line`
/// shape. Empty `annotations` → no right column (no extra width
/// reserved, no two-space gap).
///
/// `padded`: when true, applies `px_2()` for the strip variants
/// that need horizontal padding. The overlay variants paint
/// inside a bordered container that already has its own padding.
fn paint_candidate_row(
    cand: &lattice_completion::RenderedCandidate,
    selected: bool,
    theme: &GpuiTheme,
    padded: bool,
) -> gpui::Div {
    // Issue #35 (2026-05-22): match highlight now uses
    // `picker_match_highlight` (Catppuccin peach by default,
    // distinct from `foreground`). Previously used
    // `cursor_background` which is identical to `foreground`
    // in the Catppuccin Mocha defaults — match highlights
    // were invisible.
    let match_hl_fg = rgb(theme.picker_match_highlight);
    let row_bg = if selected {
        Some(rgb(theme.status_background))
    } else {
        None
    };
    let row_fg = if selected {
        rgb(theme.status_foreground)
    } else {
        rgb(theme.foreground)
    };
    // Marginalia (kind glyph on the left + annotations on the
    // right) uses `picker_marginalia_fg`. The selected-row
    // case bumps to a slightly brighter shade so it stays
    // legible against the status background.
    let marginalia_fg = if selected {
        rgb(theme.foreground)
    } else {
        rgb(theme.picker_marginalia_fg)
    };
    let display = &cand.raw.display;

    // Left side: display text with optional per-char match
    // highlighting. Fast path: no match ranges → single child
    // (empty-query "show all" hits this every row).
    let display_div: gpui::Div = if cand.match_ranges.is_empty() {
        div().child(display.clone()).text_color(row_fg)
    } else {
        let in_match = |byte_idx: usize| -> bool {
            cand.match_ranges
                .iter()
                .any(|r| byte_idx >= r.start && byte_idx < r.end)
        };
        let cells: Vec<gpui::Div> = display
            .char_indices()
            .map(|(byte_idx, c)| {
                let cell = div().child(c.to_string());
                if in_match(byte_idx) {
                    cell.text_color(match_hl_fg)
                } else {
                    cell.text_color(row_fg)
                }
            })
            .collect();
        div().flex().flex_row().children(cells)
    };

    // Issue #35-followup (2026-05-22): user reported the
    // marginalia floated to the far-right edge in maximized
    // windows. Previously `justify_between().w_full()` pushed
    // annotations across the whole screen; now annotations
    // sit right after the display text with a small gap.
    // The row's `w_full` is kept ONLY so the selected-row bg
    // fills the strip; child layout is natural (no
    // justify_between).
    let annotation_text = cand.annotations.join("  ");
    let mut row = div().flex().flex_row().w_full();
    if padded {
        row = row.px_2();
    }
    // Issue #35 (2026-05-22): left-margin kind glyph (one
    // ASCII char) so the user can scan candidates by kind
    // (`f` = file, `b` = buffer, `:` = command, etc.). Sits
    // in `marginalia_fg` so it doesn't compete with the
    // display text.
    let kind_glyph = format!("{} ", cand.raw.kind.glyph());
    row = row
        .child(div().text_color(marginalia_fg).child(kind_glyph))
        .child(display_div);
    if !annotation_text.is_empty() {
        // 2-space gap after the display text, then the
        // annotation. Sits close to the candidate rather
        // than at the screen edge.
        row = row.child(
            div()
                .text_color(marginalia_fg)
                .ml_2()
                .child(annotation_text),
        );
    }
    if let Some(bg) = row_bg { row.bg(bg) } else { row }
}

/// Resolve the diagnostic gutter glyph + colour for a severity by
/// reading the host's `Theme` (the same source the TUI peer uses
/// via `theme::diagnostic_glyph_and_style`). Returns the glyph
/// character and a Catppuccin-compatible `0xRRGGBB` color the
/// renderer applies to that cell.
///
/// Phase 5.8.I introduced this as hardcoded matches; 5.8.N hoists
/// the source of truth to `host_theme` so `:set ui.diagnostics.*`
/// overrides flow to both renderer peers identically. Falls back
/// to overlay2 grey on Unknown severities (rare; future LSP versions
/// could add new variants).
fn diagnostic_glyph_and_color(
    host_theme: &lattice_host::ui::theme::Theme,
    severity: lattice_lsp::DiagnosticSeverity,
) -> (char, u32) {
    let (glyph, style) = match severity {
        lattice_lsp::DiagnosticSeverity::ERROR => (
            host_theme.diagnostic_error_glyph,
            host_theme.diagnostic_error_style,
        ),
        lattice_lsp::DiagnosticSeverity::WARNING => (
            host_theme.diagnostic_warning_glyph,
            host_theme.diagnostic_warning_style,
        ),
        lattice_lsp::DiagnosticSeverity::INFORMATION => (
            host_theme.diagnostic_info_glyph,
            host_theme.diagnostic_info_style,
        ),
        lattice_lsp::DiagnosticSeverity::HINT => (
            host_theme.diagnostic_hint_glyph,
            host_theme.diagnostic_hint_style,
        ),
        _ => (
            host_theme.diagnostic_info_glyph,
            host_theme.diagnostic_info_style,
        ),
    };
    // 0x9399b2 is Catppuccin overlay2 — the v1 muted fallback if
    // the theme uses `Color::Default` (no concrete RGB).
    let color = style.fg.map(|c| c.to_rgb_u32(0x9399b2)).unwrap_or(0x9399b2);
    (glyph, color)
}

// 5.8.N: `CursorShape` lives host-side
// (`lattice_host::cursor_shape::CursorShape`); both renderer
// peers map the host shape to their native cursor primitive.
// Re-imported via `use` at the top of the file.

/// The renderer-side composition root rendered as a GPUI
/// `Entity`. Holds the [`GpuiApp`] + a [`FocusHandle`] so the
/// window's key events actually route to our dispatcher.
struct EditorView {
    app: GpuiApp,
    focus_handle: FocusHandle,
}

impl EditorView {
    fn new(document: Document, cx: &mut Context<Self>) -> Self {
        let app = GpuiApp::new(document);
        // X1b: spawn the worker-paint-request bridge. The
        // highlights worker fires `editor.paint_request.notify_one()`
        // after every `WorkerDecision::Recomputed`; this future
        // awaits each wake and calls `cx.notify()` so GPUI
        // schedules a paint even when no user input is in flight.
        // Without this bridge, an async worker recompute that
        // finishes while the user is idle (e.g. final reparse
        // after a held-key burst settles) would publish fresh
        // spans into `syntax_visible_spans_cell` that nothing
        // reads until the next keystroke -- breaking goal-#4
        // asynchronicity (the renderer would effectively poll on
        // keystrokes for worker output).
        //
        // `cx.spawn` runs the future on GPUI's foreground
        // executor; the `AsyncWindowContext` argument lets us
        // upgrade the weak entity handle and call `cx.notify()`.
        // The future exits cleanly when the weak handle can't
        // upgrade (window closed).
        // Slice 3c.final.B-extension: paint_request cached on GpuiApp
        // at boot (no read_editor round-trip — wait-free Arc clone).
        let paint_request = app.paint_request.clone();
        cx.spawn(async move |this, cx| {
            loop {
                paint_request.notified().await;
                if this
                    .update(cx, |_view, cx| cx.notify())
                    .is_err()
                {
                    break;
                }
            }
        })
        .detach();
        Self {
            app,
            focus_handle: cx.focus_handle(),
        }
    }

    fn on_key_down(&mut self, event: &KeyDownEvent, _window: &mut Window, cx: &mut Context<Self>) {
        let ks = &event.keystroke;
        tracing::debug!(
            key = %ks.key,
            ctrl = ks.modifiers.control,
            alt = ks.modifiers.alt,
            shift = ks.modifiers.shift,
            platform = ks.modifiers.platform,
            "lattice-gpui: key down"
        );
        // Slice 3c.final.B (group 3): popup gate via published
        // substate (`render_state.popup.is_open()`).
        // Slice 3c.final.E.5j: read RS via `App::render_state` (App
        // owns the same Arc, cloned at construction time).
        if self.app.render_state.load().popup.is_open()
            && (ks.key.eq_ignore_ascii_case("escape") || ks.key.eq_ignore_ascii_case("esc"))
        {
            tracing::debug!("lattice-gpui: dismissing popup overlay (Esc)");
            self.app.dismiss_popup();
            cx.stop_propagation();
            cx.notify();
            return;
        }
        let outcome = self.app.dispatch_keystroke(
            &ks.key,
            ks.modifiers.control,
            ks.modifiers.alt,
            ks.modifiers.shift,
            ks.modifiers.platform,
        );
        // 3c.atomic.H: post-dispatch tracing reads through the
        // published render-state cell. `dispatch_keystroke`'s
        // chain ends with `publish_render_state()` so `ad()` is
        // current here.
        let ad = self.app.ad();
        tracing::debug!(
            modal = ?ad.modal,
            cursor_line = ad.cursor.line,
            cursor_byte = ad.cursor.byte,
            dispatched = outcome.is_some(),
            "lattice-gpui: post-dispatch state"
        );
        cx.stop_propagation();
        cx.notify();
        // Slice 3c.final.B (group 6): lifecycle read via published
        // substate.
        if self.app.render_state.load().lifecycle.should_quit {
            tracing::info!("lattice-gpui: editor.should_quit set; closing application");
            cx.quit();
        }
    }
}

impl Focusable for EditorView {
    fn focus_handle(&self, _: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl EditorView {
    /// Recursive walker over `editor.pane_tree.root()`. Each
    /// [`PaneNode::Leaf`] paints via [`Self::paint_pane`]; splits
    /// wrap children in `flex_col` (horizontal split: stacked
    /// vertically) or `flex_row` (vertical split: side by side)
    /// with a thin divider border between them. The active leaf
    /// gets the richer rendering (cursor + cached highlights);
    /// inactive leaves get a simpler text + gutter + per-pane
    /// status view.
    ///
    /// Phase 5.8.H: multi-pane visible in the GPUI peer. Single-
    /// pane case still works (the root is just `Leaf(0)`); v1
    /// inactive-pane rendering deliberately omits highlights +
    /// cursor markers — those need per-pane caches the host
    /// already populates for the TUI peer via
    /// `refresh_pane_highlights`, queued for a future slice.
    fn paint_pane_tree(&self, node: &PaneNode, theme: &GpuiTheme, active_idx: usize) -> gpui::Div {
        match node {
            PaneNode::Leaf(idx) => self.paint_pane(*idx, theme, *idx == active_idx),
            PaneNode::HorizontalSplit { top, bottom, .. } => div()
                .flex()
                .flex_col()
                .flex_grow()
                .size_full()
                .child(
                    self.paint_pane_tree(top, theme, active_idx)
                        .flex_grow()
                        .border_b_1()
                        .border_color(rgb(theme.popup_border)),
                )
                .child(self.paint_pane_tree(bottom, theme, active_idx).flex_grow()),
            PaneNode::VerticalSplit { left, right, .. } => div()
                .flex()
                .flex_row()
                .flex_grow()
                .size_full()
                .child(
                    self.paint_pane_tree(left, theme, active_idx)
                        .flex_grow()
                        .border_r_1()
                        .border_color(rgb(theme.popup_border)),
                )
                .child(self.paint_pane_tree(right, theme, active_idx).flex_grow()),
        }
    }

    /// Paint a single pane. Active pane uses `editor.cursor` +
    /// `editor.visible_highlights` (refreshed at the top of
    /// render); inactive panes use the stashed `PaneState::cursor`
    /// and no highlights. Each pane gets its own status line at
    /// its bottom (path + cursor coords), which keeps the visible
    /// boundary between panes legible without a hard chrome border.
    fn paint_pane(&self, pane_idx: usize, theme: &GpuiTheme, is_active: bool) -> gpui::Div {
        // Slice 3c.final.E.swap: paint reads route through the
        // App's own `render_state` Arc (cloned from
        // `editor.render_state` at construction). No `&Editor`
        // borrow held across the function body.
        let ad = self.app.ad();
        let rs_guard = self.app.render_state.load();
        let active_spans_guard = rs_guard.syntax.visible_spans.load();
        // Phase 5.8.AF.5 / Slice 3c.final.B (group 1): pane tree
        // + buffer registry read through `rs_guard.panes` /
        // `rs_guard.buffers` instead of `editor.X` directly.
        let leaves = rs_guard.panes.tree.leaves();
        if pane_idx >= leaves.len() {
            return div().child(format!("(stale pane index {pane_idx})"));
        }
        let pane: &PaneState = &leaves[pane_idx];
        // Resolve the buffer's document handle. Inactive panes may
        // reference buffers different from `editor.document`; the
        // registry clone on `rs_guard.buffers` shares the editor's
        // `Arc<Mutex<...>>` so the lookup sees the latest state.
        let snapshot_opt = rs_guard
            .buffers
            .registry
            .document_handle(pane.buffer_id)
            .map(|h| h.snapshot());
        let Some(snapshot) = snapshot_opt else {
            return div()
                .p_3()
                .child(format!("(buffer {:?} unavailable)", pane.buffer_id));
        };
        // Phase 5.8.AF.5 / Slice 3c.atomic.L: instrument the
        // suspected per-frame waste site -- `snapshot.text()` is
        // a full rope-to-String copy and `split('\n').collect()`
        // builds a `Vec<&str>` over it. For a 100KB file that's
        // hundreds of KB of allocation per pane per frame.
        // `lattice_runtime::DocumentSnapshot::text` docstring
        // explicitly warns against calling on the hot path.
        let text_t0 = std::time::Instant::now();
        let text = snapshot.text();
        let text_us = text_t0.elapsed().as_micros() as u64;
        // 2026-05-22 issue #24 (cursor companion of pane_scroll
        // below): when popup owns active_buffer, ad.cursor
        // describes the popup buffer's cursor — the document
        // pane's cursor must come from its stashed snapshot.
        let popup_owns_active = ad.buffer_kind == lattice_core::BufferKind::Help;
        // When the popup has focus the document pane should look
        // inactive — no cursorline, no selection, no active status
        // bar — the same appearance it has when a different pane has
        // focus.
        let render_active = is_active && !popup_owns_active;
        let cursor = if render_active {
            ad.cursor
        } else {
            pane.cursor
        };
        let split_t0 = std::time::Instant::now();
        let raw_lines: Vec<&str> = text.split('\n').collect();
        let split_us = split_t0.elapsed().as_micros() as u64;
        tracing::debug!(
            target: "lattice_gpui::perf",
            pane_idx,
            is_active,
            text_bytes = text.len() as u64,
            line_count = raw_lines.len() as u64,
            text_us,
            split_us,
            "paint_pane text materialisation"
        );
        // 5.8.O: clip the visible window to `[scroll, scroll +
        // viewport_height)` so large docs don't render every line
        // every frame. Active pane reads scroll from `editor.scroll`
        // (ensure_cursor_in_viewport keeps it sane); inactive
        // panes read their stashed `PaneState::scroll`. The
        // gutter, status, and cursor maths still work in terms of
        // absolute line indices — only the iter range tightens.
        // 2026-05-22 issue #24: when active_buffer is Help, the
        // popup has "stolen" ad.scroll / ad.cursor (they now
        // describe the popup buffer's state, not the document
        // pane's). The document pane is still painted underneath
        // — its scroll should come from the stashed `pane.scroll`
        // captured at popup-open via `snapshot_active_pane`,
        // NOT from ad.scroll. Without this guard, opening
        // `:describe-buffer` scrolled the background document to
        // line 0 (the help buffer's initial scroll).
        let pane_scroll = if render_active {
            ad.scroll
        } else {
            pane.scroll
        };
        let viewport_height = ad.viewport_height.max(1);
        let visible_start = (pane_scroll as usize).min(raw_lines.len());
        let visible_end = (pane_scroll as usize)
            .saturating_add(viewport_height as usize)
            .min(raw_lines.len());

        let cursor_shape = if render_active {
            Some(CursorShape::for_mode(ad.modal))
        } else {
            None
        };

        // Slice X3.full.2: the element always paints the active
        // pane's `visible_spans`. Inactive panes whose buffer
        // differs from the active doc currently paint with the
        // active spans (visually-stale highlights) -- the
        // pre-existing slice 1 limitation; the per-pane span
        // cache resync into the element is a follow-up slice.
        let total_lines = raw_lines.len().max(1);
        let gutter_width = total_lines.to_string().len();

        // 5.8.I: per-line severity lookup. URI for this pane's
        // buffer comes from `rs_guard.buffers.uris` (slice 3c.final.B
        // group 1: published HashMap clone of `editor.buffer_uris`,
        // populated when LSP attaches). `None` means: unsaved
        // scratch, no LSP attachment, or LSP-mode disabled for this
        // buffer. The gutter then renders a blank sign column (one
        // space) so the line-number alignment stays stable
        // regardless of whether diagnostics are present.
        let uri = rs_guard.buffers.uris.get(&pane.buffer_id);
        // Phase 5.8.AF.5 / Slice 3a: read through the renderer's
        // `RenderState` contract instead of `editor.lsp_diagnostics`
        // directly. Symmetric with the TUI peer's
        // `severity_for_line` migration. `load_full` is wait-free
        // (~2ns); the returned snapshot's diagnostics layer is
        // internally `Arc<ArcSwap<...>>`-backed so the inner
        // `line_severity` call stays wait-free too.
        // Slice 3c.final.E.swap: render_state via App's own Arc.
        let render_state = self.app.render_state.load_full();
        let line_severity = |line_idx: u32| -> Option<lattice_lsp::DiagnosticSeverity> {
            uri.and_then(|u| render_state.diagnostics.layer.line_severity(u, line_idx))
        };

        // 5.8.N: severity glyph + colour come from host_theme so
        // `:set ui.diagnostics.*` overrides flow through identically
        // for both renderer peers.
        // Slice 3c.final.B (group 6): host theme via published
        // top-level field. Theme is `Copy` so this is a plain
        // struct move.
        let host_theme = rs_guard.theme;

        // Phase 5.8.AF.5 / Slice X3.full.2 + 3c.final.B (group 2):
        // gather per-row gutter metadata for the visible window.
        // Caller does the LSP / fold lookups so the element holds
        // only owned values. Folds + foldenable now come from
        // `rs_guard.active_document` rather than `editor.X` — the
        // predicate is inlined here since the published `Arc<[Fold]>`
        // doesn't carry the helper methods. Behaviour matches
        // `Editor::line_inside_closed_fold` + `fold_start_at`
        // (both gate on `option_cache.foldenable`).
        let folds = &rs_guard.active_document.folds;
        let foldenable = rs_guard.active_document.option_cache.foldenable;
        let line_inside_closed_fold = |line: u32| -> bool {
            foldenable
                && folds
                    .iter()
                    .any(|f| f.closed && line > f.start_line && line <= f.end_line)
        };
        let fold_start_at = |line: u32| -> bool {
            foldenable && folds.iter().any(|f| f.closed && f.start_line == line)
        };
        let gutter_meta: Vec<crate::editor_element::GutterLineMeta> = (visible_start..visible_end)
            .filter(|line_idx| !line_inside_closed_fold(*line_idx as u32))
            .map(|line_idx| {
                let fold_start = fold_start_at(line_idx as u32);
                let severity =
                    line_severity(line_idx as u32).map(|s| diagnostic_glyph_and_color(&host_theme, s));
                crate::editor_element::GutterLineMeta {
                    line_idx: line_idx as u32,
                    fold_start,
                    severity,
                }
            })
            .collect();

        // Active-pane cursor state. `None` on inactive panes so the
        // element doesn't paint a cursor marker there.
        //
        // Issue #19/#21 (2026-05-22): also `None` when active_buffer
        // is Help (State B: popup focused). The document pane is
        // still painted, but the cursor is "inside" the popup
        // conceptually. Hiding the document cursor signals to the
        // user that focus has shifted, complementing the popup
        // border-color change above.
        let cursor_state = match (render_active, cursor_shape) {
            (true, Some(shape)) => Some(crate::editor_element::CursorState {
                line: cursor.line,
                byte: cursor.byte,
                shape,
            }),
            _ => None,
        };

        // Slice X3.full.3 decoration data. All `Range`s in host
        // state are utf-8 byte coordinates; the lone exception is
        // LSP document-highlights (utf-16 columns) -- we convert
        // them here against actual line text so the element sees
        // only utf-8.
        // Slice 3c.final.B (group 2): visual_range / current_match /
        // all_matches / substitute_preview read through the published
        // `rs_guard.active_document` snapshot instead of `editor.X`.
        let visual_range = if render_active {
            rs_guard.active_document.visual_range
        } else {
            None
        };
        let current_match = if render_active {
            rs_guard.active_document.current_match
        } else {
            None
        };
        let all_matches: Vec<lattice_core::protocol::position::Range> = if render_active {
            rs_guard.active_document.all_matches.to_vec()
        } else {
            Vec::new()
        };
        let substitute_matches: Vec<lattice_core::protocol::position::Range> = if render_active {
            rs_guard
                .active_document
                .substitute_preview
                .as_ref()
                .map(|p| p.matches.to_vec())
                .unwrap_or_default()
        } else {
            Vec::new()
        };
        // LSP document-highlights: utf-16 columns at the protocol
        // boundary; convert to utf-8 byte offsets against the
        // hit's host line text. Painted on any pane sharing the
        // highlighted buffer (the per-buffer pump model).
        let rs_for_dh = self.app.render_state.load_full();
        let dh_guard = rs_for_dh.lsp.document_highlights.load_full();
        let doc_highlights: Vec<lattice_core::protocol::position::Range> = dh_guard
            .as_deref()
            .filter(|cache| cache.buffer_id == pane.buffer_id)
            .map(|cache| {
                cache
                    .highlights
                    .iter()
                    .map(|h| {
                        let start_line = h.range.start.line;
                        let end_line = h.range.end.line;
                        let start_text = raw_lines.get(start_line as usize).copied().unwrap_or("");
                        let end_text = raw_lines.get(end_line as usize).copied().unwrap_or("");
                        let start_byte = lattice_lsp::position::utf16_column_to_utf8_byte(
                            start_text,
                            h.range.start.character,
                        );
                        let end_byte = lattice_lsp::position::utf16_column_to_utf8_byte(
                            end_text,
                            h.range.end.character,
                        );
                        lattice_core::protocol::position::Range {
                            start: lattice_core::protocol::position::Position {
                                line: start_line,
                                byte: start_byte,
                            },
                            end: lattice_core::protocol::position::Position {
                                line: end_line,
                                byte: end_byte,
                            },
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();
        // Cursorline bg: host_theme drives `:set ui.cursor-line-bg`
        // overrides. Fallback Catppuccin surface0.
        // Slice 3c.final.B (group 6): reuse `host_theme` bound
        // above from `rs_guard.theme`.
        let cursorline_bg = host_theme.cursor_line_bg.to_rgb_u32(0x313244);

        // Slice X3.full.4: gather LSP inlay hints + diagnostic
        // underline ranges for this pane's buffer. Both arrive
        // in LSP coordinates (utf-16 character columns) and are
        // converted to utf-8 bytes against the buffer's actual
        // line text here, at the boundary. The element only sees
        // utf-8.
        let inlay_hints: Vec<crate::editor_element::InlayHintRow> = render_state
            .lsp
            .inlay_hints
            .get_for(pane.buffer_id)
            .map(|cache| {
                cache
                    .hints
                    .iter()
                    .map(|h| {
                        let line_idx = h.position.line;
                        let line_text =
                            raw_lines.get(line_idx as usize).copied().unwrap_or("");
                        let byte = lattice_lsp::position::utf16_column_to_utf8_byte(
                            line_text,
                            h.position.character,
                        );
                        let mut text = lattice_lsp::inlay_hint_label_text(&h.label);
                        if h.padding_left.unwrap_or(false) {
                            text.insert(0, ' ');
                        }
                        if h.padding_right.unwrap_or(false) {
                            text.push(' ');
                        }
                        crate::editor_element::InlayHintRow {
                            line: line_idx,
                            byte,
                            text,
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();

        // Diagnostic underlines: pull `Arc<[Diagnostic]>` for the
        // pane's URI (`None` => unsaved scratch / no LSP /
        // disabled). For each diagnostic, convert utf-16 → utf-8
        // against the corresponding line, resolve severity →
        // color via `diagnostic_glyph_and_color`.
        let diagnostic_underlines: Vec<crate::editor_element::DiagnosticUnderline> = uri
            .and_then(|u| render_state.diagnostics.layer.diagnostics_arc(u))
            .map(|diags| {
                diags
                    .iter()
                    .map(|d| {
                        let start_line = d.range.start.line;
                        let end_line = d.range.end.line;
                        let start_text =
                            raw_lines.get(start_line as usize).copied().unwrap_or("");
                        let end_text =
                            raw_lines.get(end_line as usize).copied().unwrap_or("");
                        let start_byte = lattice_lsp::position::utf16_column_to_utf8_byte(
                            start_text,
                            d.range.start.character,
                        );
                        let end_byte = lattice_lsp::position::utf16_column_to_utf8_byte(
                            end_text,
                            d.range.end.character,
                        );
                        let color = d
                            .severity
                            .map(|s| diagnostic_glyph_and_color(&host_theme, s).1)
                            // Unknown severity: fall back to overlay2
                            // (matches `diagnostic_glyph_and_color`).
                            .unwrap_or(0x9399b2);
                        crate::editor_element::DiagnosticUnderline {
                            range: lattice_core::protocol::position::Range {
                                start: lattice_core::protocol::position::Position {
                                    line: start_line,
                                    byte: start_byte,
                                },
                                end: lattice_core::protocol::position::Position {
                                    line: end_line,
                                    byte: end_byte,
                                },
                            },
                            color,
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();

        // Inlay color: Catppuccin overlay1; host-theme override
        // hook lands when `host_theme.inlay_foreground` is added.
        let inlay_color: u32 = 0x7f849c;

        // Per-pane status line at the pane's bottom. Format
        // matches the TUI's per-pane status: path + cursor coords.
        // Active pane uses the cursor color for the bar so the
        // user can tell which pane has focus at a glance.
        let path_label = snapshot
            .path()
            .map(|p| {
                let display = match std::env::current_dir()
                    .ok()
                    .and_then(|cwd| p.strip_prefix(&cwd).ok().map(|s| s.to_path_buf()))
                {
                    Some(rel) => rel.display().to_string(),
                    None => p.display().to_string(),
                };
                if snapshot.dirty {
                    format!("{display} [+]")
                } else {
                    display
                }
            })
            .unwrap_or_else(|| {
                if snapshot.dirty {
                    "[scratch][+]".to_string()
                } else {
                    "[scratch]".to_string()
                }
            });
        let status_line = format!("  {path_label}   L:{}  C:{}", cursor.line + 1, cursor.byte);
        let (status_bg, status_fg) = if render_active {
            (rgb(theme.cursor_background), rgb(theme.cursor_foreground))
        } else {
            (rgb(theme.status_background), rgb(theme.status_foreground))
        };

        // Phase 5.8.AF.5 / Slice X3.full.2: pane body is one
        // `EditorElement` that shapes + paints visible lines
        // directly via `WindowTextSystem::shape_line` +
        // `ShapedLine::paint`. The legacy `rows` Vec construction
        // (one Div per cell, per line) was deleted in this slice;
        // gutter + cursor moved into the element. Decoration
        // backgrounds (selection / hlsearch / doc-highlight /
        // cursorline / substitute) restore in slice X3.full.3;
        // inlay-hint virtual text + per-cell diagnostic
        // underlines restore in slice X3.full.4.
        let editor_element = crate::editor_element::EditorElement {
            pane_idx,
            theme: theme.clone(),
            text: std::sync::Arc::new(text),
            // Issue #25 (2026-05-22): per-pane visible_spans for
            // multi-split support. Active pane reads the live
            // visible_spans cell (the highlights worker writes
            // there continuously). Inactive panes read from
            // `pane_highlights[pane_idx]` populated by the
            // RefreshPaneHighlights dispatch fired in the render
            // body. Empty when the cache hasn't refreshed yet
            // (first frame after split); the renderer paints
            // plain text in that case until the next frame.
            visible_spans: if render_active {
                (*active_spans_guard).clone()
            } else {
                let pane_spans = rs_guard
                    .syntax
                    .pane_highlights
                    .get(&pane_idx)
                    .cloned()
                    .unwrap_or_else(|| {
                        std::sync::Arc::new(Vec::<Vec<lattice_syntax::StyledSpan>>::new())
                    });
                lattice_host::render_state::VisibleSpans {
                    spans: (*pane_spans).clone(),
                    computed_for_key: lattice_host::render_state::VisibleHighlightsKey::default(),
                }
                .into()
            },
            scroll: pane_scroll,
            viewport_height,
            gutter: gutter_meta,
            gutter_width,
            cursor: cursor_state,
            is_active: render_active,
            visual_range,
            current_match,
            all_matches,
            substitute_matches,
            doc_highlights,
            cursorline_bg,
            inlay_hints,
            diagnostic_underlines,
            inlay_color,
        };

        div()
            .flex()
            .flex_col()
            .flex_grow()
            .overflow_hidden()
            .child(
                div()
                    .flex_grow()
                    .p_3()
                    .flex()
                    .flex_col()
                    .overflow_hidden()
                    .child(editor_element.into_any_element()),
            )
            .child(
                div()
                    .bg(status_bg)
                    .text_color(status_fg)
                    .px_2()
                    .py_1()
                    .flex()
                    .flex_row()
                    .child(div().child(status_line)),
            )
    }
}

impl Render for EditorView {
    fn render(&mut self, window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        // Phase 5.8.AF.5 / Slice 3c.atomic.L: per-frame budget
        // breakdown on the `lattice_gpui::perf` tracing target.
        // Enable with `RUST_LOG=lattice_gpui::perf=info`. Emits
        // one info-line per frame summarising the time spent in
        // each phase (viewport, ensure_cursor, highlights, tick,
        // paint). One per-pane debug-line covers the document
        // text materialisation cost (`snapshot.text()` +
        // line split) -- the prime suspect for per-frame waste.
        let frame_start = std::time::Instant::now();
        // Read `picker.display` early so the viewport-height
        // recompute below can subtract the picker strip's rows
        // from the buffer area when the picker is open in
        // minibuffer mode. Re-derived later in the same render
        // body for the overlay-vs-strip branch.
        let picker_use_minibuffer = picker_display_is_minibuffer(&self.app);
        // 5.8.T: per-frame viewport-height recompute from the
        // window's current pixel bounds. The row height MUST
        // match what `EditorElement` actually paints with
        // (`font_size * 1.3`) — using a smaller estimate
        // overcounts visible rows, making `viewport_height` too
        // large, which lets the cursor scroll past the visible
        // area before the host's clamp triggers (Phase 5.8.AF.6
        // QoL fix: cursor disappearing off the bottom edge).
        //
        // `text_sm` in GPUI is 0.875 rem and EditorElement uses
        // a 1.3 line-height multiplier; deriving from
        // `window.rem_size()` keeps the estimate in sync if the
        // user later changes the rem base.
        //
        // `chrome_rows` shrinks the buffer area by however many
        // non-buffer rows the column carries: the status row at
        // the bottom is always 1; the picker minibuffer strip
        // (prompt + candidate band) adds more rows when the
        // user has `picker.display = "minibuffer"` and the
        // picker is open.
        let viewport_px = window.viewport_size();
        let rem = f32::from(window.rem_size());
        let font_size_px = rem * 0.875; // text_sm()
        let estimated_row_px = font_size_px * 1.3; // matches EditorElement::line_height
        let total_rows = (f32::from(viewport_px.height) / estimated_row_px).floor() as i32;
        // Issue #17 (2026-05-22): the previous calc subtracted
        // exactly 1 row for the modeline/cmdline bottom strip and
        // ignored every other piece of non-buffer chrome — `.p_3()`
        // on the pane content (1.5rem top+bottom), `.py_1()` on
        // the per-pane status row (~0.5rem), the status text line
        // itself (~1 row), and `.py_1()` on the global bottom row
        // (~0.5rem). Net: ~4 rows of chrome were billed as ~1.
        // The buffer area over-claimed ~3 rows, so cursor jumps
        // past G/}/etc. parked the cursor on rows actually painted
        // under the modeline / pane status.
        //
        // Compute chrome in pixels (the honest unit) and convert
        // to a row-equivalent via ceil() so we round up to a
        // safe-but-snug viewport. Per-pane chrome scales with
        // pane count via `flex_grow()` — each split pane carries
        // its own .p_3 + status — but the global bottom row is
        // single. For multi-pane, each pane's own chrome shrinks
        // its rendered buffer area; this calculation reserves
        // chrome for the active pane's view height.
        let pane_padding_v_px = rem * 0.75 * 2.0; // .p_3() top + bottom = 1.5rem
        let pane_padding_h_px = rem * 0.75 * 2.0; // .p_3() left + right = 1.5rem
        let pane_status_padding_px = rem * 0.25 * 2.0; // .py_1() = 0.5rem
        let pane_status_row_px = estimated_row_px; // status text line
        let global_bottom_padding_px = rem * 0.25 * 2.0; // .py_1() = 0.5rem
        let global_bottom_row_px = estimated_row_px; // modeline / cmdline content
        let per_leaf_v_chrome_px =
            pane_padding_v_px + pane_status_padding_px + pane_status_row_px;
        let per_leaf_h_chrome_px = pane_padding_h_px;
        let global_chrome_v_px = global_bottom_padding_px + global_bottom_row_px;
        // Slice 3c.final.B (group 3): picker read via published
        // substate. Bind the Arc so the `as_deref()` borrow lives
        // for the closure.
        let picker_substate = self.app.render_state.load().picker.clone();
        // Slice 3c.gpui-cmdline-completion: cmdline-completion strip
        // shares the same screen area as the picker minibuffer and
        // honors the same `picker.display` setting. The two are
        // mutually exclusive (picker doesn't activate during `:`
        // typing). The strip has NO separate prompt row — the
        // cmdline itself (bottom row) is the prompt.
        let completion_substate = self.app.render_state.load().completion.clone();
        let picker_strip_rows: i32 = if picker_use_minibuffer {
            picker_substate
                .state
                .as_deref()
                .map(|p| 1 + p.candidates.len().min(10) as i32) // 1 prompt + up to 10 cands
                .unwrap_or(0)
        } else {
            0
        };
        let cmdline_completion_strip_rows: i32 = if picker_use_minibuffer {
            completion_substate
                .state
                .as_deref()
                .filter(|s| !s.candidates.is_empty())
                .map(|s| s.candidates.len().min(10) as i32) // candidates only; cmdline is the prompt
                .unwrap_or(0)
        } else {
            0
        };
        // Issue #25 (2026-05-22): per-pane viewport_height +
        // viewport_width via `collect_pane_geometries`. Replaces
        // the prior single-global `set_viewport_height` call.
        // Each leaf gets its own geometry; the host mirrors the
        // active leaf's height into `Editor::viewport_height`
        // for cursor-clamp + highlights worker.
        //
        // Available height for the pane tree subtracts BOTH the
        // global bottom chrome (modeline + py_1) AND the picker /
        // cmdline-completion strips (which sit above the
        // modeline when minibuffer-mode picker is open).
        // Measure the actual cell advance by shaping a reference
        // character; GPUI's LineLayoutCache makes it O(1) after the
        // first frame. Used both for pane column geometry and
        // cursor-anchored popup placement so the two are consistent.
        // Clone font family before the block so the immutable borrow of
        // self.app drops before the mutable borrows below.
        let font_family_for_advance = self.app.theme.font_family.clone();
        let glyph_advance_px = {
            let ref_font = font(font_family_for_advance);
            let ref_run = TextRun {
                len: 1,
                font: ref_font,
                color: gpui::Rgba::default().into(),
                background_color: None,
                underline: None,
                strikethrough: None,
            };
            f32::from(
                window
                    .text_system()
                    .shape_line(SharedString::from("M"), px(font_size_px), &[ref_run], None)
                    .width,
            )
        };
        let strip_rows_px = (picker_strip_rows + cmdline_completion_strip_rows) as f32
            * estimated_row_px;
        // Issue #29 (2026-05-22): tabline claims one row at the
        // top when visible — subtract from available so per-pane
        // geometries see the correct buffer height.
        let tabline_visible = self.app.render_state.load().tabs.visible;
        let tabline_h_px = if tabline_visible {
            estimated_row_px
        } else {
            0.0
        };
        let avail_h_px = (f32::from(viewport_px.height)
            - global_chrome_v_px
            - strip_rows_px
            - tabline_h_px)
            .max(0.0);
        let avail_w_px = f32::from(viewport_px.width);
        let pane_tree_root = self.app.render_state.load().panes.tree.root().clone();
        let mut pane_geometries: Vec<(usize, u32, u32)> = Vec::new();
        collect_pane_geometries(
            &pane_tree_root,
            avail_w_px,
            avail_h_px,
            per_leaf_v_chrome_px,
            per_leaf_h_chrome_px,
            estimated_row_px,
            glyph_advance_px,
            &mut pane_geometries,
        );
        // Issue #27 (2026-05-22): fire `set_pane_viewport` per
        // leaf ONLY when the geometry actually changes vs. what's
        // already published. The unconditional per-frame fire
        // (the prior shape) sent N actor RPCs per frame, each
        // calling `publish_render_state`. At 60fps with 1 pane
        // that's 60 publishes/sec for no reason; with 4 panes,
        // 240/sec. The flood blocked the GPUI main thread —
        // mouse click no longer focused the window, drag /
        // maximize stopped working, alt+tab was the only way
        // in. Diff-then-send fixes it: in steady state (no
        // resize) the loop fires 0 commands.
        let current_rs = self.app.render_state.load();
        let current_leaves = current_rs.panes.tree.leaves();
        for (idx, rows, cols) in &pane_geometries {
            let needs_update = current_leaves
                .get(*idx)
                .map(|l| l.viewport_height != *rows || l.viewport_width != *cols)
                .unwrap_or(true);
            if needs_update {
                self.app.set_pane_viewport(*idx, *rows, *cols);
            }
        }
        // total_rows + the old chrome_rows / new_viewport
        // arithmetic retires; `set_pane_viewport` carries the
        // per-pane truth.
        let _ = total_rows;
        let after_viewport = std::time::Instant::now();
        // 5.8.O: keep the cursor inside the viewport before any
        // paint reads `editor.scroll`. Auto-scrolls if the cursor
        // moved past the visible window since the last frame.
        // `set_viewport_height` above already ran one round of
        // `ensure_cursor_visible`, but this also covers the case
        // where the viewport size didn't change but the cursor
        // moved past the existing window.
        self.app.ensure_cursor_in_viewport();
        let after_ensure = std::time::Instant::now();
        // Phase 5.8.AF.5 / Slice X2.5: the per-frame
        // `self.app.refresh_highlights()` call has been removed.
        // Active-pane highlights are now produced by the
        // background highlights worker
        // (`lattice_host::highlights_worker`) which subscribes to
        // `Editor::highlight_wake` and publishes results into
        // `render_state.syntax.visible_spans`. `paint_pane` reads
        // those spans through `rs_guard.syntax.visible_spans.load()`.
        // Pre-X2 cost: ~178µs at 80 lines per frame; post-X2: zero
        // UI-thread parse cost. Goal #1 violation B1 closed for the
        // GPUI peer.
        // 5.8.R: rebuild the per-pane cache for inactive Document
        // panes whose buffer differs from the active pane's. The
        // host method handles the same-doc short-circuit + reparse
        // gating; this peer just makes the call so paint_pane can
        // read `editor.pane_highlights[idx]` for the inactive case.
        // Slice 3c.final.C: pane-highlight refresh via dispatch.
        self.app
            .dispatch_action(lattice_host::action::Action::RefreshPaneHighlights);
        let after_highlights = std::time::Instant::now();
        // Phase 5.8.AF.5 / Slice X1: `run_tick_pending` no longer
        // runs in the renderer body. The drain moved to
        // `GpuiApp::dispatch_action`'s tail so it fires on the
        // keystroke that caused the work, not in `Render::render`.
        // Paramount goal #1 forbids I/O / event drain on the UI
        // thread. With X1 landed, `tick_us` in `lattice_gpui::perf`
        // should be ~0; if it ever climbs back, something has
        // re-introduced the violation -- audit per
        // `docs/dev/operations/render-thread-discipline-remediation.md`
        // §7 before merging.
        let after_tick = std::time::Instant::now();
        // Phase 5.8.AA.p/r/t: every per-tick drain (hover,
        // definitions, code-actions, live-picker, ...) is now
        // folded into `run_tick_pending` above; no per-paint
        // catch-up calls remain.
        // 3c.atomic.H: modeline label read through the published
        // render-state. Paint-time read; the apply loop above
        // has already published any modal change.
        let modal = self.app.ad().modal;

        let modal_label = match modal {
            ModalState::Normal => "NORMAL",
            ModalState::Insert => "INSERT",
            ModalState::Visual(_) => "VISUAL",
            ModalState::OperatorPending => "PENDING",
            ModalState::Command => "COMMAND",
            ModalState::Search(_) => "SEARCH",
            ModalState::Replace => "REPLACE",
        };
        // 5.8.C / 5.8.H: bottom global row. In Command/Search
        // modes it shows the in-progress `:cmd` / `/pattern`
        // minibuffer; otherwise it shows the global modal label.
        // Per-pane path + cursor coords now live inside each
        // pane's own status line (built in `paint_pane`).
        // Slice 3c.final.B.7: cmdline + search-line via published
        // `modeline()` sub-state — wait-free Arc clones.
        let modeline = self.app.modeline();
        let bottom_row: String = match modal {
            ModalState::Command => format!(":{}", modeline.cmdline_text),
            ModalState::Search(dir) => {
                let prefix = match dir {
                    lattice_grammar::SearchDirection::Forward => '/',
                    lattice_grammar::SearchDirection::Backward => '?',
                };
                let pattern = modeline.search_pattern.as_deref().unwrap_or("");
                format!("{prefix}{pattern}")
            }
            _ => format!("  {modal_label}"),
        };
        let bottom_is_minibuffer = matches!(modal, ModalState::Command | ModalState::Search(_));

        let theme = self.app.theme.clone();
        // 5.8.H: render the pane tree. `paint_pane_tree` walks
        // `rs.panes.tree.root()` recursively; each leaf paints
        // via `paint_pane` with active/inactive style. The active
        // leaf gets the refreshed `visible_highlights` cache + a
        // visible cursor; inactive leaves show plain text + no
        // cursor (their own stashed `PaneState::cursor` is read
        // for the per-pane status coords but no visible marker is
        // painted).
        //
        // Slice 3c.final.B (group 1): pane tree read through
        // the published `RenderState` instead of `editor.pane_tree`
        // directly. `paint_pane` loads its own `rs_guard` for
        // per-pane access; this load drives the recursion entry
        // point and shares the same Arc across the render body.
        // Slice 3c.final.E.5j: render-state load via App's own Arc
        // (cloned from `editor.render_state` at construction time).
        let render_state = self.app.render_state.load_full();
        let active_idx = render_state.panes.tree.active_index();
        let document_area = self
            .paint_pane_tree(render_state.panes.tree.root(), &theme, active_idx)
            .flex_grow();
        let after_paint = std::time::Instant::now();

        // Slice 3c.final.E.5j: insert-completion via the published
        // `completion().insert` sub-state (Arc-bump clone).
        let insert_completion = self.app.render_state.load().completion.insert.clone();
        let completion_overlay: Option<gpui::Div> = insert_completion
            .as_deref()
            .filter(|ic| !ic.rendered.is_empty())
            .map(|ic| {
                let max_visible = 10usize;
                let total = ic.rendered.len();
                let window_start = ic
                    .selected
                    .saturating_sub(max_visible / 2)
                    .min(total.saturating_sub(max_visible.min(total)));
                let window_end = (window_start + max_visible).min(total);
                // 5.8.AB.1: match-range highlighting in the
                // insert-completion popup, same rules as the
                // picker overlay.
                let match_hl_fg = rgb(theme.cursor_background);
                let visible: Vec<gpui::Div> = ic.rendered[window_start..window_end]
                    .iter()
                    .enumerate()
                    .map(|(i, cand)| {
                        let abs_idx = window_start + i;
                        let selected = abs_idx == ic.selected;
                        let row_bg = if selected {
                            Some(rgb(theme.status_background))
                        } else {
                            None
                        };
                        let row_fg = if selected {
                            rgb(theme.status_foreground)
                        } else {
                            rgb(theme.foreground)
                        };
                        let display = &cand.raw.display;
                        if cand.match_ranges.is_empty() {
                            let row = div().child(display.clone()).text_color(row_fg);
                            return if let Some(bg) = row_bg { row.bg(bg) } else { row };
                        }
                        let in_match = |byte_idx: usize| -> bool {
                            cand.match_ranges
                                .iter()
                                .any(|r| byte_idx >= r.start && byte_idx < r.end)
                        };
                        let cells: Vec<gpui::Div> = display
                            .char_indices()
                            .map(|(byte_idx, c)| {
                                let cell = div().child(c.to_string());
                                if in_match(byte_idx) {
                                    cell.text_color(match_hl_fg)
                                } else {
                                    cell.text_color(row_fg)
                                }
                            })
                            .collect();
                        let row = div().flex().flex_row().children(cells);
                        if let Some(bg) = row_bg { row.bg(bg) } else { row }
                    })
                    .collect();
                div()
                    .flex()
                    .flex_col()
                    .max_w(px(360.0))
                    .p_2()
                    .bg(rgb(theme.popup_background))
                    .text_color(rgb(theme.foreground))
                    .border_2()
                    .border_color(rgb(theme.popup_border))
                    .children(visible)
                    .child(
                        div()
                            .pt_1()
                            .text_color(rgb(theme.popup_border))
                            .child(format!(
                                " {} of {}  ·  <C-n>/<C-p> · <Tab>/<CR> accept · <Esc> cancel ",
                                if total == 0 { 0 } else { ic.selected + 1 },
                                total,
                            )),
                    )
            });

        // `picker.display` selects the picker UI shape. `"popup"`
        // floats a centred overlay over the buffer area; the
        // default `"minibuffer"` mirrors the TUI vertico layout:
        // the candidate band sits at the very bottom with the
        // prompt row directly above it, both *below* the status
        // row (the GPUI equivalent of the TUI mode-line / cmdline
        // pair). The two modes are mutually exclusive -- only the
        // one matching the config gets built and inserted into
        // the root. `picker_use_minibuffer` was already computed
        // at the top of `render` so the viewport-height
        // recompute could subtract the strip's rows.
        // Slice 3c.final.B (group 3): picker via published
        // substate; reuse `picker_substate` bound at top of render.
        let picker_overlay: Option<gpui::Div> = (!picker_use_minibuffer)
            .then(|| picker_substate.state.as_deref())
            .flatten()
            .map(|picker| {
            let max_visible = 30usize;
            let total = picker.candidates.len();
            let window_start = picker
                .selected
                .saturating_sub(max_visible / 2)
                .min(total.saturating_sub(max_visible.min(total)));
            let window_end = (window_start + max_visible).min(total);
            // 5.8.AB.1: paint matched bytes in cursor_background
            // so the user can see *why* each row matched their
            // query. The TUI peer can't easily render styled
            // per-character spans mid-row; GPUI walks the byte
            // sequence and emits one cell-div per char with the
            // matched ones tinted. `match_ranges` is half-open
            // Slice 3c.unify.gpui-annotation-render: rows now flow
            // through the shared `paint_candidate_row` helper that
            // also paints the right-aligned annotations column.
            // `padded: false` — the overlay container below
            // applies its own `.p_2()`.
            let visible_candidates: Vec<gpui::Div> = picker.candidates[window_start..window_end]
                .iter()
                .enumerate()
                .map(|(i, cand)| {
                    let abs_idx = window_start + i;
                    paint_candidate_row(cand, abs_idx == picker.selected, &theme, false)
                })
                .collect();
            // Width sizing: file pickers carry long paths (often
            // 60+ chars) and `:picker grep` adds a path:line
            // prefix column. The previous 720px cap clipped both.
            // Targeting ~80% of typical window width with a
            // generous min_w keeps annotation columns (kind /
            // detail) visible without dominating the screen on
            // ultrawides.
            div()
                .flex()
                .flex_col()
                .min_w(px(720.0))
                .w(px(1200.0))
                .max_w(px(1400.0))
                .max_h(px(640.0))
                .p_4()
                .bg(rgb(theme.popup_background))
                .text_color(rgb(theme.foreground))
                .border_2()
                .border_color(rgb(theme.popup_border))
                .child(
                    div()
                        .text_color(rgb(theme.popup_border))
                        .pb_1()
                        .child(format!(
                            " {} ({} / {}) ",
                            picker.title,
                            if total == 0 { 0 } else { picker.selected + 1 },
                            total,
                        )),
                )
                .child(
                    div()
                        .flex()
                        .flex_row()
                        .pb_2()
                        .child(div().text_color(rgb(theme.cursor_background)).child("> "))
                        .child(div().child(picker.query.clone()))
                        .child(
                            div()
                                .border_l_2()
                                .border_color(rgb(theme.cursor_background))
                                .child(" "),
                        ),
                )
                .child(div().child("───────────────".to_string()))
                .child(
                    div()
                        .flex()
                        .flex_col()
                        .overflow_hidden()
                        .children(visible_candidates),
                )
                .child(
                    div()
                        .pt_2()
                        .text_color(rgb(theme.popup_border))
                        .child("[ <C-n>/<C-p> navigate · <CR> accept · <Esc> cancel ]".to_string()),
                )
        });

        // Vertico-style minibuffer strip (matches TUI when
        // `picker.display = "minibuffer"`). Two rows below the
        // status line: prompt on top, candidate band below.
        // Selected candidate sits at the top of the band so the
        // eye path matches the TUI's prompt → selection
        // adjacency. Built as a normal flex_col child (not an
        // absolute overlay) so it claims layout space rather
        // than floating over the buffer.
        // Slice 3c.final.B (group 3): picker via published substate.
        let picker_minibuffer: Option<gpui::Div> = picker_use_minibuffer
            .then(|| picker_substate.state.as_deref())
            .flatten()
            .map(|picker| {
                const MAX_VISIBLE: usize = 10;
                let total = picker.candidates.len();
                let visible_count = total.min(MAX_VISIBLE).max(1);
                let scroll = if picker.selected < visible_count {
                    0
                } else {
                    picker.selected + 1 - visible_count
                };
                let window_end = (scroll + visible_count).min(total);
                // Slice 3c.unify.gpui-annotation-render: minibuffer
                // strip rows go through the shared
                // `paint_candidate_row` helper. `padded: true` —
                // strip has no surrounding container with its own
                // horizontal padding.
                let cand_rows: Vec<gpui::Div> = if total == 0 {
                    vec![div()
                        .px_2()
                        .text_color(rgb(theme.popup_border))
                        .child("  (no matches)".to_string())]
                } else {
                    picker.candidates[scroll..window_end]
                        .iter()
                        .enumerate()
                        .map(|(i, cand)| {
                            let abs_idx = scroll + i;
                            paint_candidate_row(
                                cand,
                                abs_idx == picker.selected,
                                &theme,
                                true,
                            )
                        })
                        .collect()
                };

                let count = format!(
                    "  ({} / {})",
                    if total == 0 { 0 } else { picker.selected + 1 },
                    total,
                );
                let prompt_row = div()
                    .px_2()
                    .flex()
                    .flex_row()
                    .bg(rgb(theme.background))
                    .text_color(rgb(theme.foreground))
                    .child(
                        div()
                            .text_color(rgb(theme.cursor_background))
                            .child(format!("{}> ", picker.title)),
                    )
                    .child(div().child(picker.query.clone()))
                    .child(
                        div()
                            .border_l_2()
                            .border_color(rgb(theme.cursor_background))
                            .child(" "),
                    )
                    .child(
                        div()
                            .text_color(rgb(theme.popup_border))
                            .child(count),
                    );

                div()
                    .flex()
                    .flex_col()
                    .child(prompt_row)
                    .child(
                        div()
                            .flex()
                            .flex_col()
                            .bg(rgb(theme.background))
                            .children(cand_rows),
                    )
            });

        // Slice 3c.gpui-cmdline-completion: cmdline-completion
        // minibuffer strip. Mirrors the picker minibuffer's shape
        // (vertico-style candidate band) but omits the title /
        // prompt row — the cmdline (`:cmd`) is the prompt. Honors
        // the same `picker.display` setting so the two completion
        // surfaces feel like one UI family.
        let cmdline_completion_minibuffer: Option<gpui::Div> = picker_use_minibuffer
            .then(|| completion_substate.state.as_deref())
            .flatten()
            .filter(|s| !s.candidates.is_empty())
            .map(|state| {
                const MAX_VISIBLE: usize = 10;
                let total = state.candidates.len();
                let visible_count = total.min(MAX_VISIBLE).max(1);
                // Picker keeps the selected row at the top of the
                // band; cmdline completion uses the same convention
                // so the eye path is identical across the two
                // surfaces.
                let scroll = if state.selected < visible_count {
                    0
                } else {
                    state.selected + 1 - visible_count
                };
                let window_end = (scroll + visible_count).min(total);
                // Slice 3c.unify.gpui-annotation-render: shared row
                // builder. `padded: true` for the minibuffer
                // strip variant.
                let cand_rows: Vec<gpui::Div> = state.candidates[scroll..window_end]
                    .iter()
                    .enumerate()
                    .map(|(i, cand)| {
                        let abs_idx = scroll + i;
                        paint_candidate_row(cand, abs_idx == state.selected, &theme, true)
                    })
                    .collect();
                div()
                    .flex()
                    .flex_col()
                    .bg(rgb(theme.background))
                    .children(cand_rows)
            });

        // Slice 3c.gpui-cmdline-completion: cmdline-completion
        // popup overlay. Mirrors the picker overlay's centered
        // float; activates when `picker.display = "popup"`. The
        // band content is identical to the minibuffer variant.
        let cmdline_completion_overlay: Option<gpui::Div> = (!picker_use_minibuffer)
            .then(|| completion_substate.state.as_deref())
            .flatten()
            .filter(|s| !s.candidates.is_empty())
            .map(|state| {
                const MAX_VISIBLE: usize = 30;
                let total = state.candidates.len();
                let window_start = state
                    .selected
                    .saturating_sub(MAX_VISIBLE / 2)
                    .min(total.saturating_sub(MAX_VISIBLE.min(total)));
                let window_end = (window_start + MAX_VISIBLE).min(total);
                // Slice 3c.unify.gpui-annotation-render: shared row
                // builder. `padded: false` — overlay container
                // applies its own `.p_2()`.
                let visible_candidates: Vec<gpui::Div> = state.candidates[window_start..window_end]
                    .iter()
                    .enumerate()
                    .map(|(i, cand)| {
                        let abs_idx = window_start + i;
                        paint_candidate_row(cand, abs_idx == state.selected, &theme, false)
                    })
                    .collect();
                div()
                    .flex()
                    .flex_col()
                    .min_w(px(320.0))
                    .max_w(px(720.0))
                    .max_h(px(480.0))
                    .p_2()
                    .bg(rgb(theme.popup_background))
                    .border_1()
                    .border_color(rgb(theme.popup_border))
                    .children(visible_candidates)
            });

        // Phase 5.8.AE + Slice 3c.final.B (group 3): read popup
        // state via the published substate. The Arc-wrapped
        // `HelpBuffer` + `help_highlights` slice live on
        // `render_state.popup.X`; bind locally so the borrows live
        // for the closure.
        let popup_substate = self.app.render_state.load().popup.clone();
        let popup_overlay: Option<gpui::Div> = popup_substate.help.as_deref().map(|buf| {
            let title = buf.title.clone();
            let body_text = buf.content.as_string();
            // M.3.2.c.5: highlights live in buffer-locals keyed by the
            // popup buffer id; published as `popup.help_highlights`.
            let highlights_owned: Vec<Vec<lattice_syntax::StyledSpan>> =
                popup_substate.help_highlights.to_vec();
            let line_highlights: &[Vec<lattice_syntax::StyledSpan>] =
                highlights_owned.as_slice();
            let body_lines: Vec<&str> = body_text.split('\n').collect();

            // When the popup is focused (State B), ad().scroll and
            // ad().cursor describe the popup buffer's scroll/cursor so
            // we can show the right content window and a cursor indicator.
            let ad = self.app.ad();
            let popup_focused = ad.buffer_kind == lattice_core::BufferKind::Help;
            // Max visible lines sized for the expanded 600px body.
            const MAX_POPUP_LINES: usize = 30;
            let popup_scroll = if popup_focused { ad.scroll as usize } else { 0 };
            let cursor_doc_line = if popup_focused {
                Some(ad.cursor.line as usize)
            } else {
                None
            };
            // Byte offset within the cursor line (for a char-wide block cursor).
            let cursor_byte = if popup_focused {
                Some(ad.cursor.byte as usize)
            } else {
                None
            };

            let popup_lines: Vec<gpui::Div> = body_lines
                .iter()
                .enumerate()
                .skip(popup_scroll)
                .take(MAX_POPUP_LINES)
                .map(|(idx, line)| {
                    let is_cursor_line = cursor_doc_line == Some(idx);
                    let spans: &[lattice_syntax::StyledSpan] =
                        line_highlights.get(idx).map(Vec::as_slice).unwrap_or(&[]);

                    if is_cursor_line {
                        // Render a character-wide block cursor at cursor_byte.
                        let cb = cursor_byte.unwrap_or(0);
                        let mut cells: Vec<gpui::Div> = line
                            .char_indices()
                            .map(|(byte_idx, c)| {
                                let syntax_style = style_at(spans, byte_idx);
                                if byte_idx == cb {
                                    div()
                                        .bg(rgb(theme.cursor_background))
                                        .text_color(rgb(theme.cursor_foreground))
                                        .child(c.to_string())
                                } else {
                                    div()
                                        .text_color(rgb(syntax_color(syntax_style)))
                                        .child(c.to_string())
                                }
                            })
                            .collect();
                        // Cursor past end-of-line: append a space block.
                        if cb >= line.len() {
                            cells.push(
                                div()
                                    .bg(rgb(theme.cursor_background))
                                    .text_color(rgb(theme.cursor_foreground))
                                    .child(" ".to_string()),
                            );
                        }
                        div().flex().flex_row().children(cells)
                    } else if spans.is_empty() {
                        div().child(line.to_string())
                    } else {
                        let cells: Vec<gpui::Div> = line
                            .char_indices()
                            .map(|(byte_idx, c)| {
                                let style = style_at(spans, byte_idx);
                                div()
                                    .text_color(rgb(syntax_color(style)))
                                    .child(c.to_string())
                            })
                            .collect();
                        div().flex().flex_row().children(cells)
                    }
                })
                .collect();

            let border_color = if popup_focused {
                rgb(theme.cursor_background)
            } else {
                rgb(theme.popup_border)
            };
            let header_hint = if popup_focused {
                " (j/k scroll · q/Esc dismiss)"
            } else {
                " (K to focus · Esc dismiss)"
            };
            div()
                .flex()
                .flex_col()
                .max_w(px(900.0))
                .max_h(px(600.0))
                .p_4()
                .bg(rgb(theme.popup_background))
                .text_color(rgb(theme.foreground))
                .border_2()
                .border_color(border_color)
                .child(
                    div()
                        .text_color(rgb(theme.popup_border))
                        .pb_2()
                        .child(format!(" {title}{header_hint} "))
                        .child(div().child("───────────────".to_string())),
                )
                .child(
                    div()
                        .flex()
                        .flex_col()
                        .overflow_hidden()
                        .children(popup_lines),
                )
        });

        // Issue #29 (2026-05-22): tabline strip. Visibility is
        // resolved by the publisher's `build_tabs_render_state`
        // based on `tabline.show` × tabs.len(). When visible,
        // the strip sits ABOVE document_area; the row's vertical
        // cost is accounted for in `collect_pane_geometries`
        // via `tabline_h_px`.
        let tabs_rs = self.app.render_state.load().tabs.clone();
        let tabline_element = if tabs_rs.visible {
            let mut row = div()
                .flex()
                .flex_row()
                .bg(rgb(theme.status_background))
                .text_color(rgb(theme.status_foreground));
            for (idx, item) in tabs_rs.items.iter().enumerate() {
                let label = format!(" {} {} ", idx + 1, item.label);
                // Slice 3 (2026-05-22): mouse click → switch
                // tab. The click handler dispatches
                // `Action::GoToTab(idx + 1)` so the same
                // semantic path used by `{N}gt` flows through
                // the host's `do_goto_tab`. Per-cell click
                // listener — no global tabline event-routing
                // needed.
                let target_n = (idx + 1) as u32;
                let mut cell = if idx == tabs_rs.active {
                    div()
                        .bg(rgb(theme.cursor_background))
                        .text_color(rgb(theme.cursor_foreground))
                } else {
                    div()
                };
                cell = cell.child(label).on_mouse_down(
                    gpui::MouseButton::Left,
                    cx.listener(move |this, _event, _window, cx| {
                        this.app.dispatch_action(
                            lattice_host::action::Action::GoToTab(target_n),
                        );
                        cx.notify();
                    }),
                );
                row = row.child(cell);
            }
            Some(row)
        } else {
            None
        };

        let mut root = div()
            .track_focus(&self.focus_handle)
            .on_key_down(cx.listener(Self::on_key_down))
            .flex()
            .flex_col()
            .size_full()
            .bg(rgb(theme.background))
            .text_color(rgb(theme.foreground))
            .text_sm()
            .font_family(theme.font_family.clone());
        if let Some(tabline) = tabline_element {
            root = root.child(tabline);
        }
        root = root
            .child(document_area)
            .child({
                let row = div()
                    .bg(rgb(theme.status_background))
                    .text_color(rgb(theme.status_foreground))
                    .px_2()
                    .py_1()
                    .flex()
                    .flex_row()
                    .child(div().child(bottom_row));
                if bottom_is_minibuffer {
                    row.child(
                        div()
                            .border_l_2()
                            .border_color(rgb(theme.cursor_background))
                            .child(" "),
                    )
                } else {
                    row
                }
            });

        if let Some(strip) = picker_minibuffer {
            root = root.child(strip);
        }
        // Slice 3c.gpui-cmdline-completion: minibuffer strip + overlay.
        // Mutually exclusive with the picker (the picker doesn't
        // activate while typing in `:`).
        if let Some(strip) = cmdline_completion_minibuffer {
            root = root.child(strip);
        }

        if let Some(overlay) = picker_overlay {
            root = root.child(
                div()
                    .absolute()
                    .inset_0()
                    .flex()
                    .justify_center()
                    .items_center()
                    .child(overlay),
            );
        }
        if let Some(overlay) = cmdline_completion_overlay {
            root = root.child(
                div()
                    .absolute()
                    .inset_0()
                    .flex()
                    .justify_center()
                    .items_center()
                    .child(overlay),
            );
        }
        if let Some(overlay) = completion_overlay {
            root = root.child(div().absolute().top_8().right_4().child(overlay));
        }
        if let Some(overlay) = popup_overlay {
            // Issue #18 (2026-05-22): respect
            // `popup_substate.placement` — the host already publishes
            // Centered vs CursorAnchored per-popup (hover/sigHelp
            // use CursorAnchored; `:describe-*` / `:lsp-status`
            // etc. use Centered). The old code unconditionally
            // centered every popup, which broke hover (`K`) UX:
            // the docs appeared in the middle of the screen
            // instead of next to the symbol under the cursor.
            //
            // For CursorAnchored: compute pixel position from the
            // cursor's (line - scroll, byte) screen coordinates +
            // the pane's `.p_3()` padding. Uses the same
            // `glyph_advance_px` measured above so popup x aligns
            // with the character columns EditorElement paints.
            let placement = popup_substate.placement;
            use lattice_core::ui::popup::PopupPlacement;
            root = match placement {
                PopupPlacement::Centered => root.child(
                    div()
                        .absolute()
                        .inset_0()
                        .flex()
                        .justify_center()
                        .items_center()
                        .child(overlay),
                ),
                PopupPlacement::CursorAnchored => {
                    // 2026-05-22 popup-anchor: use the cursor
                    // SNAPSHOT captured at popup-open time
                    // (`popup_substate.anchor`) instead of the
                    // live `ad.cursor`. Otherwise the popup
                    // follows cursor motions (regression
                    // reported in third triage round). Fall
                    // back to `ad.cursor` defensively for the
                    // pre-anchor-field state (None when boot is
                    // still mid-publish).
                    let ad = self.app.ad();
                    let anchor = popup_substate.anchor.unwrap_or(ad.cursor);
                    // Use doc_scroll_at_anchor (fixed at popup-open time)
                    // rather than ad.scroll: in State B, ad.scroll is the
                    // POPUP's scroll, causing a frame-jump on second K.
                    let cursor_screen_row =
                        anchor.line.saturating_sub(popup_substate.doc_scroll_at_anchor) as f32;
                    let cursor_byte_col = anchor.byte as f32;
                    // Pane's `.p_3()` adds 0.75rem padding before
                    // the editor element starts; account for it
                    // so popup x/y align with painted glyphs.
                    let pane_pad_px = rem * 0.75;
                    // Prefer placing below the cursor row; flip above
                    // when the popup would extend beyond the viewport.
                    const POPUP_MAX_H: f32 = 600.0;
                    let below_top = pane_pad_px + (cursor_screen_row + 1.0) * estimated_row_px;
                    let viewport_h = f32::from(viewport_px.height);
                    let top_px = if below_top + POPUP_MAX_H > viewport_h {
                        // Not enough room below — anchor above the cursor row.
                        let above_top =
                            pane_pad_px + cursor_screen_row * estimated_row_px - POPUP_MAX_H;
                        above_top.max(0.0)
                    } else {
                        below_top
                    };
                    let left_px = pane_pad_px + cursor_byte_col * glyph_advance_px;
                    root.child(
                        div()
                            .absolute()
                            .top(px(top_px))
                            .left(px(left_px))
                            .child(overlay),
                    )
                }
            };
        }
        // Phase 5.8.AF.5 / Slice 3c.atomic.L: per-frame budget log.
        // `after_paint` was captured immediately after
        // `paint_pane_tree` returned; the remaining work (overlay
        // assembly + return) is folded into the `tail_us` bucket.
        // Enable with `RUST_LOG=lattice_gpui::perf=info`.
        let frame_us = frame_start.elapsed().as_micros() as u64;
        tracing::debug!(
            target: "lattice_gpui::perf",
            frame_us,
            viewport_us = (after_viewport - frame_start).as_micros() as u64,
            ensure_us = (after_ensure - after_viewport).as_micros() as u64,
            highlights_us = (after_highlights - after_ensure).as_micros() as u64,
            tick_us = (after_tick - after_highlights).as_micros() as u64,
            paint_us = (after_paint - after_tick).as_micros() as u64,
            tail_us = (frame_start.elapsed() - (after_paint - frame_start)).as_micros() as u64,
            "frame budget"
        );
        root
    }
}

/// Open a GPUI window backed by [`EditorView`] and drive the
/// event loop until the editor signals quit. Synchronous —
/// blocks the calling thread for the lifetime of the window
/// (same shape as [`lattice_ui_tui::run`] for symmetry).
///
/// Phase 5.9: the body of `fn main` from the old `lattice-gpui`
/// binary, lifted into the lib so `lattice-cli` can route
/// `lattice --gpu <file>` through this single entry. The
/// `lattice-gpui` binary keeps a thin shim that calls this.
pub fn run(document: Document) -> Result<()> {
    Application::new().run(move |cx| {
        let bounds = Bounds::centered(None, size(px(720.0), px(480.0)), cx);
        let window = cx.open_window(
            WindowOptions {
                window_bounds: Some(WindowBounds::Windowed(bounds)),
                ..Default::default()
            },
            move |_window, cx| cx.new(|cx| EditorView::new(document, cx)),
        );
        let window = match window {
            Ok(w) => w,
            Err(e) => {
                tracing::error!(error = ?e, "lattice-gpui: failed to open editor window");
                return;
            }
        };
        // Focus + activation has to run AFTER `open_window`
        // returns -- calling `window.focus(...)` from inside the
        // builder closure runs before gpui has fully initialised
        // the window's focus tree, and the focus call is silently
        // dropped. The `window.update(cx, ...)` re-entry hands
        // back a real window context where focus actually sticks
        // (this is the pattern gpui's own `examples/input.rs`
        // uses).
        let focus_result = window.update(cx, |view, window, cx| {
            window.focus(&view.focus_handle.clone());
            cx.activate(true);
        });
        if let Err(e) = focus_result {
            tracing::error!(error = ?e, "lattice-gpui: failed to focus editor window");
        }
    });
    Ok(())
}

/// Open a [`Document`] from `path`, with friendly error
/// context. Available for callers that want to pre-open a
/// document before handing it to [`run`].
pub fn document_from_path(path: &std::path::Path) -> Result<Document> {
    Document::open(path).with_context(|| format!("opening {}", path.display()))
}

// 5.8.N: cursor-shape tests live host-side in
// `lattice_host::cursor_shape::tests`; the GPUI peer's local
// duplicates were removed. Window-side tests (popup focus, dispatch
// integration) still live in `crate::tests` at the lib's root.
