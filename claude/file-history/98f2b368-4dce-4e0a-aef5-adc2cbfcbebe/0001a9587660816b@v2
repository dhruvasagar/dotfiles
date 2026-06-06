//! Custom GPUI `Element` rendering pane text via direct cosmic-text
//! shaping (`WindowTextSystem::shape_line` + `ShapedLine::paint`).
//!
//! Phase 5.8.AF.5 / Slice X3.full.1 -> .2.
//!
//! ## Why this exists
//!
//! Pre-X3.full, `window::paint_pane` built an element tree of one
//! `Div` per styled character run, per visible line. Even after X3's
//! run-collapsing (~8x reduction vs one-Div-per-char), ~100-200
//! Divs per pane per frame is the dominant cost: `paint_us` in the
//! 1300-3000µs band at 60Hz, 85-95% of frame time, downstream of
//! GPUI's flex layout + composition pass. Paramount goal #1
//! (sub-frame input latency: keystroke -> glyph <= 8ms at 120Hz)
//! cannot be met while the element tree has that fan-out.
//!
//! This element collapses the entire pane body into ONE element-tree
//! node. `prepaint` calls `shape_line` once per visible line (text)
//! and once per visible line (gutter); `paint` calls
//! `ShapedLine::paint` accordingly + `Window::paint_quad` for the
//! pane background and cursor. The shaping itself was already
//! happening inside the per-Div text -- the saving is the upstream
//! flex layout + element composition pass.
//!
//! ## Slice scope
//!
//! - **X3.full.1 ✅**: pane background quad, per-line shaped text,
//!   syntax colouring from worker-published `VisibleSpans`.
//! - **X3.full.2 (this slice)**: cursor (block / bar / underline
//!   shapes painted via `paint_quad`); gutter (fold marker + severity
//!   sign + line number, one ShapedLine per visible row); legacy
//!   per-cell row construction in `window::paint_pane` deleted.
//! - **X3.full.3 (next)**: decoration backgrounds — visual
//!   selection, hlsearch, current_match, doc_highlight, cursorline,
//!   substitute preview.
//! - **X3.full.4**: inlay-hint virtual text + per-cell diagnostic
//!   underline + GPUI frame-budget bench.
//!
//! ## Cursor positioning (monospace approximation)
//!
//! Slice X3.full.2 positions the cursor at
//! `gutter_width_px + char_col * glyph_advance`. `glyph_advance`
//! is a monospace approximation (`font_size * 0.6`); the editor's
//! default font is monospace so this is visually correct for code
//! text. Proportional-font support requires reading per-glyph X
//! advances via `layout_line` — deferred to a follow-on slice when
//! the use case appears.
//!
//! ## Indexing contract
//!
//! `VisibleSpans.spans[i]` covers absolute buffer line
//! `scroll_at_compute_time + i`, per
//! `lattice-syntax::Syntax::highlight_lines`. The element reads
//! with `spans.get(line_idx - scroll)`. If the worker hasn't
//! caught up to the current `scroll` (X1b idle-wake gap), `get`
//! returns `None` and the line paints in `SyntaxStyle::Default` —
//! better visual fail-mode than a panic.
//!
//! See `docs/dev/operations/render-thread-discipline-remediation.md`
//! §X3.full.

#![cfg(feature = "window")]

use std::sync::Arc;

use gpui::{
    App, Bounds, DefiniteLength, Element, ElementId, GlobalElementId, InspectorElementId,
    IntoElement, LayoutId, Length, Pixels, SharedString, ShapedLine, Style, TextRun, Window, fill,
    point, px, rgb, size,
};
use lattice_host::cursor_shape::CursorShape;
use lattice_host::render_state::VisibleSpans;
use lattice_syntax::{Style as SyntaxStyle, StyledSpan};

use crate::GpuiTheme;

/// Adapter: host-canonical [`Theme::syntax_style`] -> packed 24-bit
/// `0xRRGGBB`. Phase 5.8.AF.6 / issue-2 hoist: identical body to
/// `window::syntax_color` because both renderer paths must read the
/// same canonical mapping; once `EditorElement` absorbs the popup
/// overlay too, the helpers merge.
fn syntax_color(style: SyntaxStyle) -> u32 {
    let host_default = lattice_host::ui::theme::Theme::default();
    let host_style = host_default.syntax_style(style);
    host_style
        .fg
        .map(|c| c.to_rgb_u32(0xcdd6f4))
        .unwrap_or(0xcdd6f4)
}

fn style_at(spans: &[StyledSpan], byte: usize) -> SyntaxStyle {
    for span in spans {
        if byte >= span.start && byte < span.end {
            return span.style;
        }
    }
    SyntaxStyle::Default
}

/// Per-visible-line gutter metadata. Pre-resolved by the caller so
/// the element doesn't reach into LSP / fold caches at paint time.
pub(crate) struct GutterLineMeta {
    /// Absolute buffer-line index this gutter row decorates
    /// (`scroll..scroll+viewport_height`, skipping folded lines).
    pub(crate) line_idx: u32,
    /// `true` => render the fold-start marker (►) in column 0.
    pub(crate) fold_start: bool,
    /// Pre-resolved diagnostic severity (glyph, colour). `None`
    /// renders a blank space in the severity column so alignment
    /// stays stable.
    pub(crate) severity: Option<(char, u32)>,
}

/// Active-pane cursor state. `None` on inactive panes.
pub(crate) struct CursorState {
    /// 0-based buffer-line index.
    pub(crate) line: u32,
    /// 0-based utf-8 byte offset into that line's text.
    pub(crate) byte: u32,
    /// Modal-shape (Block / Bar / Underline). Resolved by the
    /// caller via `CursorShape::for_mode`.
    pub(crate) shape: CursorShape,
}

/// One inlay-hint row (slice X3.full.4). Caller flattens the LSP
/// `InlayHintLabel` to a plain string and pre-applies `padding_left`
/// / `padding_right` spacing; the element splices `text` into the
/// shaped line at `byte` (utf-8 byte offset into the original
/// line's text). Sorting by `(line, byte)` happens in the element
/// per visible row -- the input list ordering is irrelevant.
pub(crate) struct InlayHintRow {
    pub(crate) line: u32,
    pub(crate) byte: u32,
    pub(crate) text: String,
}

/// One diagnostic underline range (slice X3.full.4). Caller converts
/// the LSP utf-16 range to a utf-8-byte `Range` against the buffer's
/// line text. `color` is `0xRRGGBB` resolved via
/// `diagnostic_glyph_and_color`. Painted as a 2px `paint_quad` along
/// the bottom of the row(s) the range covers.
pub(crate) struct DiagnosticUnderline {
    pub(crate) range: lattice_core::protocol::position::Range,
    pub(crate) color: u32,
}

/// Pane element. One instance per pane per frame.
///
/// Caller (`window::paint_pane`) extracts all referenced values up
/// front so the element holds only owned data; no borrow against
/// `GpuiApp` survives across layout / prepaint / paint method
/// boundaries.
pub(crate) struct EditorElement {
    /// Pane index inside the active pane tree. Used for
    /// `ElementId` so GPUI tracks the same element across frames.
    pub(crate) pane_idx: usize,
    /// Cached theme colours (bg, fg, cursor_bg, cursor_fg, ...).
    pub(crate) theme: GpuiTheme,
    /// Full document text (pre-extracted via `snapshot.text()`).
    /// `shape_line` panics on embedded newlines so the element
    /// splits on `\n` inside `prepaint`.
    pub(crate) text: Arc<String>,
    /// Worker-published spans. `spans[i]` covers absolute buffer
    /// line `scroll + i`.
    pub(crate) visible_spans: Arc<VisibleSpans>,
    /// Pane scroll (top visible doc line index, 0-based).
    pub(crate) scroll: u32,
    /// Visible viewport height in lines.
    pub(crate) viewport_height: u32,
    /// Per-visible-row gutter metadata (after fold filtering).
    /// Empty when the caller elects to render text-only (e.g. a
    /// future inactive-pane mode); `prepaint` falls back to a
    /// gutter-less walk in that case.
    pub(crate) gutter: Vec<GutterLineMeta>,
    /// Line-number column width in chars (max digits in
    /// `total_lines`).
    pub(crate) gutter_width: usize,
    /// Active-pane cursor state. `None` => inactive pane (no
    /// cursor marker painted).
    pub(crate) cursor: Option<CursorState>,
    /// True when this is the active pane. Drives whether
    /// cursorline / visual / hlsearch / substitute / doc-highlight
    /// backgrounds paint (inactive panes paint only the bg + text +
    /// gutter -- they don't carry a visual selection or search
    /// state).
    pub(crate) is_active: bool,
    /// Visual-mode selection range in utf-8 byte coordinates.
    /// `None` outside Visual mode. Caller-resolved; element paints
    /// the layered background. (Slice X3.full.3.)
    pub(crate) visual_range: Option<lattice_core::protocol::position::Range>,
    /// `current_match`: the primary search hit the cursor sits on.
    /// Painted with the strongest match colour (yellow bg).
    pub(crate) current_match: Option<lattice_core::protocol::position::Range>,
    /// All search matches in the doc (`hlsearch`). Each painted
    /// with the softer match bg.
    pub(crate) all_matches: Vec<lattice_core::protocol::position::Range>,
    /// Substitute-preview ranges (`:s/pattern/replacement/`). Use
    /// the destructive-preview colour.
    pub(crate) substitute_matches: Vec<lattice_core::protocol::position::Range>,
    /// LSP document-highlight ranges, already converted to utf-8
    /// byte ranges by the caller (utf-16→utf-8 happens at the
    /// boundary).
    pub(crate) doc_highlights: Vec<lattice_core::protocol::position::Range>,
    /// Background colour for the cursor line
    /// (`host_theme.cursor_line_bg` resolved by the caller, fallback
    /// Catppuccin surface0).
    pub(crate) cursorline_bg: u32,
    /// LSP inlay hints for the buffer (slice X3.full.4). Caller
    /// flattens labels and pre-applies padding; the element
    /// splices `text` at `byte` into each affected visible row's
    /// shaped line. Empty when the buffer has no hints / no LSP
    /// attachment.
    pub(crate) inlay_hints: Vec<InlayHintRow>,
    /// Per-diagnostic underline overlays (slice X3.full.4). Caller
    /// resolves utf16→utf8 and severity→color at the boundary.
    /// Empty when no diagnostics exist for the buffer.
    pub(crate) diagnostic_underlines: Vec<DiagnosticUnderline>,
    /// `0xRRGGBB` for inlay virtual-text. `host_theme` resolved by
    /// the caller, Catppuccin overlay1 (0x7f849c) fallback.
    pub(crate) inlay_color: u32,
}

/// Per-frame layout state. Slice X3.full.2 holds nothing.
pub(crate) struct EditorElementLayoutState;

/// State produced in `prepaint`, consumed by `paint`.
pub(crate) struct EditorElementPrepaintState {
    /// One `ShapedLine` per visible doc row (top-of-viewport
    /// first).
    shaped_text: Vec<ShapedLine>,
    /// One `ShapedLine` per visible doc row for the gutter (fold
    /// marker + severity sign + line number). Length matches
    /// `shaped_text`. Empty when `self.gutter` was empty.
    shaped_gutter: Vec<ShapedLine>,
    /// Pre-shaped cursor char for block cursors (re-stamped on top
    /// of the cursor quad in `cursor_foreground`). `None` for
    /// bar/underline cursors or when cursor is off-screen.
    shaped_cursor_char: Option<ShapedLine>,
    /// Cursor (char_column, row_in_viewport). `None` when cursor
    /// is `None` or off-screen.
    cursor_layout: Option<(u32, u32)>,
    /// Pixel height per shaped line.
    line_height: Pixels,
    /// Pixel width per monospace glyph (`font_size * 0.6`).
    glyph_advance: Pixels,
    /// Pixel width of the gutter column. Text + cursor anchor at
    /// `bounds.origin.x + gutter_width_px`.
    gutter_width_px: Pixels,
    /// Per-visible-row (absolute_buffer_line_idx, line_text_clone).
    /// Slice X3.full.3 reads this in `paint` to convert decoration
    /// byte ranges → char columns for `paint_quad` placement,
    /// without re-splitting `self.text` on the hot path.
    /// One entry per row; length matches `shaped_text`.
    row_meta: Vec<(u32, String)>,
    /// Per-visible-row inlay-hint metadata (slice X3.full.4).
    /// Each row carries a sorted `Vec<(orig_byte, char_width)>`:
    /// orig_byte is the utf-8 byte offset INTO THE ORIGINAL LINE
    /// where the hint was inserted; char_width is the number of
    /// chars the inlay text occupies in the spliced combined line.
    /// Used by `byte_to_combined_col` to remap cursor + decoration
    /// byte coordinates onto the post-splice column space (so the
    /// cursor block stays under the right glyph, etc.). Empty
    /// vector when the row has no inlays. Length matches
    /// `shaped_text`.
    inlay_offsets_per_row: Vec<Vec<(u32, u32)>>,
    /// Per-visible-row diagnostic underline segments (slice
    /// X3.full.4). Each entry is `(col_start, col_end_exclusive,
    /// color)` in combined-column space (after inlay splicing).
    /// Pre-computed in `prepaint` so `paint` is a simple walk +
    /// `paint_quad`. Length matches `shaped_text`.
    diagnostic_segments_per_row: Vec<Vec<(u32, u32, u32)>>,
}

impl IntoElement for EditorElement {
    type Element = Self;
    fn into_element(self) -> Self::Element {
        self
    }
}

impl Element for EditorElement {
    type RequestLayoutState = EditorElementLayoutState;
    type PrepaintState = EditorElementPrepaintState;

    fn id(&self) -> Option<ElementId> {
        Some(ElementId::Name(SharedString::from(format!(
            "lattice-editor-pane-{}",
            self.pane_idx
        ))))
    }

    fn source_location(&self) -> Option<&'static std::panic::Location<'static>> {
        None
    }

    fn request_layout(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        window: &mut Window,
        cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState) {
        let mut style = Style::default();
        style.flex_grow = 1.0;
        style.size.width = Length::Definite(DefiniteLength::Fraction(1.0));
        style.size.height = Length::Definite(DefiniteLength::Fraction(1.0));
        let layout_id = window.request_layout(style, [], cx);
        (layout_id, EditorElementLayoutState)
    }

    fn prepaint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _layout: &mut Self::RequestLayoutState,
        window: &mut Window,
        _cx: &mut App,
    ) -> Self::PrepaintState {
        let raw_lines: Vec<&str> = self.text.split('\n').collect();

        let text_style = window.text_style();
        let font = text_style.font();
        let font_size = text_style.font_size.to_pixels(window.rem_size());
        let line_height: Pixels = font_size * 1.3;
        // Measure the actual advance width of one monospace cell by
        // shaping a reference character. GPUI's LineLayoutCache
        // memoises the result, so this shape call costs one hash
        // lookup after the first frame. Fallback to the 0.6
        // approximation if shaping fails (shouldn't happen for any
        // real font, but keeps the renderer non-panicking).
        let glyph_advance: Pixels = {
            let ref_run = TextRun {
                len: 1,
                font: font.clone(),
                color: gpui::Rgba::default().into(),
                background_color: None,
                underline: None,
                strikethrough: None,
            };
            window
                .text_system()
                .shape_line(SharedString::from("M"), font_size, &[ref_run], None)
                .width
        };
        let gutter_chars: usize = if self.gutter.is_empty() {
            0
        } else {
            2 + self.gutter_width + 1
        };
        let gutter_width_px: Pixels = glyph_advance * (gutter_chars as f32);

        let row_capacity = self.gutter.len().max(self.viewport_height as usize);
        let mut shaped_text = Vec::with_capacity(row_capacity);
        let mut shaped_gutter = Vec::with_capacity(self.gutter.len());
        let mut row_meta: Vec<(u32, String)> = Vec::with_capacity(row_capacity);
        let mut inlay_offsets_per_row: Vec<Vec<(u32, u32)>> =
            Vec::with_capacity(row_capacity);
        let mut diagnostic_segments_per_row: Vec<Vec<(u32, u32, u32)>> =
            Vec::with_capacity(row_capacity);

        // Slice X3.full.4: precompute per-line inlay-hint lists,
        // sorted by byte offset, so the per-row shaping loop just
        // filters by line. Typical doc has 0-3 inlays per line; the
        // sort is over the buffer's full hint set (also typically
        // small -- hundreds at worst).
        let mut sorted_inlays: Vec<&InlayHintRow> = self.inlay_hints.iter().collect();
        sorted_inlays.sort_by_key(|h| (h.line, h.byte));

        let shape_row =
            |line: &str,
             line_spans: &[StyledSpan],
             line_idx: u32,
             window: &mut Window|
             -> (ShapedLine, Vec<(u32, u32)>) {
                let inlays_on_line: Vec<(usize, &str)> = sorted_inlays
                    .iter()
                    .filter(|h| h.line == line_idx)
                    .map(|h| (h.byte as usize, h.text.as_str()))
                    .collect();
                let (combined, runs, inlay_offsets) = build_line_with_inlays(
                    line,
                    line_spans,
                    &inlays_on_line,
                    &font,
                    self.inlay_color,
                );
                let shaped = window.text_system().shape_line(
                    SharedString::from(combined),
                    font_size,
                    &runs,
                    None,
                );
                (shaped, inlay_offsets)
            };

        // Per-row diagnostic-segment computation. Walks
        // `self.diagnostic_underlines` against (line_idx, line_text,
        // inlay_offsets); returns (col_start, col_end_excl, color)
        // tuples in combined-column space.
        let diag_segments_for_row =
            |line_idx: u32, line_text: &str, inlay_offsets: &[(u32, u32)]| -> Vec<(u32, u32, u32)> {
                let mut segs = Vec::new();
                let line_len = line_text.len();
                for d in &self.diagnostic_underlines {
                    let r = &d.range;
                    if line_idx < r.start.line || line_idx > r.end.line {
                        continue;
                    }
                    let start_byte = if line_idx == r.start.line {
                        (r.start.byte as usize).min(line_len)
                    } else {
                        0
                    };
                    let end_byte = if line_idx == r.end.line {
                        (r.end.byte as usize).min(line_len)
                    } else {
                        line_len
                    };
                    if end_byte <= start_byte {
                        continue;
                    }
                    let col_start =
                        byte_to_combined_col(line_text, start_byte, inlay_offsets) as u32;
                    let col_end =
                        byte_to_combined_col(line_text, end_byte, inlay_offsets) as u32;
                    if col_end <= col_start {
                        continue;
                    }
                    segs.push((col_start, col_end, d.color));
                }
                segs
            };

        if self.gutter.is_empty() {
            // Slice 1 fallback (no gutter metadata supplied):
            // walk the visible window directly. Folds aren't
            // applied here because the caller would need to
            // pre-filter the rows for fold-skipping; without that,
            // the fallback paints every line in
            // `[scroll, scroll+viewport_height)`.
            let visible_start = (self.scroll as usize).min(raw_lines.len());
            let visible_end = (self.scroll as usize)
                .saturating_add(self.viewport_height.max(1) as usize)
                .min(raw_lines.len());
            for line_idx in visible_start..visible_end {
                let line = raw_lines[line_idx];
                let rel = line_idx.saturating_sub(self.scroll as usize);
                let line_spans: &[StyledSpan] = self
                    .visible_spans
                    .spans
                    .get(rel)
                    .map(Vec::as_slice)
                    .unwrap_or(&[]);
                let (shaped, inlay_offsets) =
                    shape_row(line, line_spans, line_idx as u32, window);
                let diag_segs = diag_segments_for_row(line_idx as u32, line, &inlay_offsets);
                shaped_text.push(shaped);
                row_meta.push((line_idx as u32, line.to_string()));
                inlay_offsets_per_row.push(inlay_offsets);
                diagnostic_segments_per_row.push(diag_segs);
            }
        } else {
            // Gutter-driven walk: caller already pre-filtered the
            // visible rows (skipping folded lines) and built the
            // gutter metadata; the text rows mirror that filter.
            for meta in &self.gutter {
                let line_idx = meta.line_idx as usize;
                let line = raw_lines.get(line_idx).copied().unwrap_or("");
                let rel = line_idx.saturating_sub(self.scroll as usize);
                let line_spans: &[StyledSpan] = self
                    .visible_spans
                    .spans
                    .get(rel)
                    .map(Vec::as_slice)
                    .unwrap_or(&[]);
                let (shaped, inlay_offsets) =
                    shape_row(line, line_spans, meta.line_idx, window);
                let diag_segs = diag_segments_for_row(meta.line_idx, line, &inlay_offsets);
                shaped_text.push(shaped);
                row_meta.push((meta.line_idx, line.to_string()));
                inlay_offsets_per_row.push(inlay_offsets);
                diagnostic_segments_per_row.push(diag_segs);

                let gutter_text = format_gutter_text(meta, self.gutter_width);
                let gutter_runs = build_gutter_runs(&gutter_text, meta, font.clone());
                let shaped_g = window.text_system().shape_line(
                    SharedString::from(gutter_text),
                    font_size,
                    &gutter_runs,
                    None,
                );
                shaped_gutter.push(shaped_g);
            }
        }

        // Cursor layout + char pre-shaping.
        let (cursor_layout, shaped_cursor_char) = match &self.cursor {
            None => (None, None),
            Some(c) => {
                // Cursor row inside the visible viewport: linear
                // search over `self.gutter` (the source of truth
                // for "which absolute line is at viewport row R"
                // once folds are involved). If gutter is empty
                // (fallback), compute directly from `scroll`.
                let row_in_viewport = if self.gutter.is_empty() {
                    let row = (c.line as i64) - (self.scroll as i64);
                    if (0..self.viewport_height as i64).contains(&row) {
                        Some(row as u32)
                    } else {
                        None
                    }
                } else {
                    self.gutter
                        .iter()
                        .position(|m| m.line_idx == c.line)
                        .map(|r| r as u32)
                };
                match row_in_viewport {
                    None => (None, None),
                    Some(row) => {
                        let line = raw_lines.get(c.line as usize).copied().unwrap_or("");
                        let byte = (c.byte as usize).min(line.len());
                        // Slice X3.full.4: remap byte → combined col
                        // via the cursor row's inlay offsets so the
                        // block cursor stays under the right glyph
                        // when inlays are present earlier on the line.
                        let cursor_row_inlays: &[(u32, u32)] = inlay_offsets_per_row
                            .get(row as usize)
                            .map(Vec::as_slice)
                            .unwrap_or(&[]);
                        let char_col =
                            byte_to_combined_col(line, byte, cursor_row_inlays) as u32;
                        let shaped = if matches!(c.shape, CursorShape::Block) && byte < line.len() {
                            let rest = &line[byte..];
                            let ch = rest.chars().next().unwrap_or(' ');
                            let runs = vec![TextRun {
                                len: ch.len_utf8(),
                                font: font.clone(),
                                color: rgb(self.theme.cursor_foreground).into(),
                                background_color: None,
                                underline: None,
                                strikethrough: None,
                            }];
                            Some(window.text_system().shape_line(
                                SharedString::from(ch.to_string()),
                                font_size,
                                &runs,
                                None,
                            ))
                        } else {
                            None
                        };
                        (Some((char_col, row)), shaped)
                    }
                }
            }
        };

        EditorElementPrepaintState {
            shaped_text,
            shaped_gutter,
            shaped_cursor_char,
            cursor_layout,
            line_height,
            glyph_advance,
            gutter_width_px,
            row_meta,
            inlay_offsets_per_row,
            diagnostic_segments_per_row,
        }
    }

    fn paint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        bounds: Bounds<Pixels>,
        _layout: &mut Self::RequestLayoutState,
        prepaint: &mut Self::PrepaintState,
        window: &mut Window,
        cx: &mut App,
    ) {
        // Pane background.
        window.paint_quad(fill(bounds, rgb(self.theme.background)));
        let line_height = prepaint.line_height;
        let advance = prepaint.glyph_advance;
        let text_origin_x = bounds.origin.x + prepaint.gutter_width_px;

        // Slice X3.full.3: per-row decoration backgrounds. Layered
        // bottom -> top so the strongest signal wins visually:
        //   cursorline (full row) -> doc_highlight -> hlsearch ->
        //   current_match -> visual -> substitute.
        // `paint_quad` overwrites (no blending in gpui 0.2.2), so
        // the last paint at a given (col, row) is what the user
        // sees. Inactive panes paint only the doc-highlight layer
        // (search / selection / substitute / cursorline are active-
        // pane state) so a tagged-but-inactive pane still hints at
        // symbol relations.
        if self.is_active && !prepaint.row_meta.is_empty() {
            // Cursorline: paint a full-row quad on whichever
            // visible row hosts the cursor.
            if let Some((_, cur_row)) = prepaint.cursor_layout {
                let row_y = bounds.origin.y + line_height * (cur_row as f32);
                let pane_width = bounds.size.width;
                let row_bounds = Bounds::new(
                    point(bounds.origin.x, row_y),
                    gpui::size(pane_width, line_height),
                );
                window.paint_quad(fill(row_bounds, rgb(self.cursorline_bg)));
            }
        }
        // Per-line range overlays (active pane only). Doc-highlight
        // paints on every pane that shares the active buffer; the
        // caller decides what to populate.
        for (row_idx, (line_idx, line_text)) in prepaint.row_meta.iter().enumerate() {
            let row_y = bounds.origin.y + line_height * (row_idx as f32);
            // Slice X3.full.4: each row's inlay offsets feed
            // overlay col-math so decoration backgrounds reflow
            // around inlay virtual text alongside the spliced
            // shaped line.
            let row_inlays: &[(u32, u32)] = prepaint
                .inlay_offsets_per_row
                .get(row_idx)
                .map(Vec::as_slice)
                .unwrap_or(&[]);
            // doc_highlight is the only overlay that paints on
            // inactive panes (its caller only populates the field
            // for the buffer that has highlights pumped).
            paint_range_overlay(
                window,
                &self.doc_highlights,
                *line_idx,
                line_text,
                row_inlays,
                text_origin_x,
                row_y,
                advance,
                line_height,
                0x585b70, // Catppuccin surface2 (doc-highlight bg).
            );
            if !self.is_active {
                continue;
            }
            // hlsearch (all_matches): softer overlay0 bg under all
            // search hits; current_match overrides on its range.
            paint_range_overlay(
                window,
                &self.all_matches,
                *line_idx,
                line_text,
                row_inlays,
                text_origin_x,
                row_y,
                advance,
                line_height,
                0x6c7086, // Catppuccin overlay0.
            );
            // current_match: stronger yellow bg.
            if let Some(r) = &self.current_match {
                paint_range_overlay(
                    window,
                    std::slice::from_ref(r),
                    *line_idx,
                    line_text,
                    row_inlays,
                    text_origin_x,
                    row_y,
                    advance,
                    line_height,
                    0xf9e2af, // Catppuccin yellow.
                );
            }
            // visual_range: surface1 bg under the selection.
            if let Some(r) = &self.visual_range {
                paint_range_overlay(
                    window,
                    std::slice::from_ref(r),
                    *line_idx,
                    line_text,
                    row_inlays,
                    text_origin_x,
                    row_y,
                    advance,
                    line_height,
                    0x45475a, // Catppuccin surface1.
                );
            }
            // substitute preview: top-layer red bg (destructive
            // preview always wins so the user sees what's about to
            // change).
            paint_range_overlay(
                window,
                &self.substitute_matches,
                *line_idx,
                line_text,
                row_inlays,
                text_origin_x,
                row_y,
                advance,
                line_height,
                0xf38ba8, // Catppuccin red.
            );
        }

        // Gutter.
        for (i, shaped_g) in prepaint.shaped_gutter.iter().enumerate() {
            let line_y = bounds.origin.y + line_height * (i as f32);
            let origin = point(bounds.origin.x, line_y);
            if let Err(err) = shaped_g.paint(origin, line_height, window, cx) {
                tracing::warn!(
                    target: "lattice_gpui::editor_element",
                    row = i,
                    pane = self.pane_idx,
                    error = ?err,
                    "gutter ShapedLine::paint failed"
                );
            }
        }

        // Text body.
        for (i, shaped_line) in prepaint.shaped_text.iter().enumerate() {
            let line_y = bounds.origin.y + line_height * (i as f32);
            let origin = point(text_origin_x, line_y);
            if let Err(err) = shaped_line.paint(origin, line_height, window, cx) {
                tracing::warn!(
                    target: "lattice_gpui::editor_element",
                    line_index = self.scroll as usize + i,
                    pane = self.pane_idx,
                    error = ?err,
                    "text ShapedLine::paint failed"
                );
            }
        }

        // Slice X3.full.4: per-cell diagnostic underlines. Painted
        // after the text body so the underline sits visually under
        // the glyphs (2px quad along the row bottom). Pre-computed
        // in `prepaint` so this loop is allocation-free; segments
        // are already in combined-column space.
        for (row_idx, segs) in prepaint.diagnostic_segments_per_row.iter().enumerate() {
            if segs.is_empty() {
                continue;
            }
            let row_y = bounds.origin.y + line_height * (row_idx as f32);
            let underline_y = row_y + line_height - px(2.0);
            for (col_start, col_end, color) in segs {
                if col_end <= col_start {
                    continue;
                }
                let quad_x = text_origin_x + advance * (*col_start as f32);
                let quad_w = advance * ((*col_end - *col_start) as f32);
                let quad_bounds =
                    Bounds::new(point(quad_x, underline_y), size(quad_w, px(2.0)));
                window.paint_quad(fill(quad_bounds, rgb(*color)));
            }
        }

        // Cursor (painted on top for bar/underline; block re-stamps
        // the covered char in cursor_foreground via shaped_cursor_char).
        if let (Some(cursor), Some((char_col, row))) = (&self.cursor, prepaint.cursor_layout) {
            let cursor_x = text_origin_x + prepaint.glyph_advance * (char_col as f32);
            let cursor_y = bounds.origin.y + line_height * (row as f32);
            let origin = point(cursor_x, cursor_y);
            let advance = prepaint.glyph_advance;
            match cursor.shape {
                CursorShape::Block => {
                    let cell = Bounds::new(origin, size(advance, line_height));
                    window.paint_quad(fill(cell, rgb(self.theme.cursor_background)));
                    if let Some(shaped) = &prepaint.shaped_cursor_char {
                        if let Err(err) = shaped.paint(origin, line_height, window, cx) {
                            tracing::warn!(
                                target: "lattice_gpui::editor_element",
                                pane = self.pane_idx,
                                error = ?err,
                                "cursor char ShapedLine::paint failed"
                            );
                        }
                    }
                }
                CursorShape::Bar => {
                    let bar = Bounds::new(origin, size(px(2.0), line_height));
                    window.paint_quad(fill(bar, rgb(self.theme.cursor_background)));
                }
                CursorShape::Underline => {
                    let underline_origin = point(origin.x, origin.y + line_height - px(2.0));
                    let underline = Bounds::new(underline_origin, size(advance, px(2.0)));
                    window.paint_quad(fill(underline, rgb(self.theme.cursor_background)));
                }
            }
        }
    }
}

/// Convert a utf-8 byte offset within `line` to a char column in
/// the inlay-spliced combined line. `inlay_offsets` is a sorted
/// slice of `(orig_byte, char_width)`: every inlay whose
/// `orig_byte <= byte` shifts the column by its `char_width` (the
/// element splices inlay text BEFORE the char at `orig_byte`).
///
/// Empty `inlay_offsets` reduces this to a plain
/// `line[..byte].chars().count()` -- the slice-X3.full.3 behaviour
/// before inlay support landed.
///
/// Saturating: `byte >= line.len()` returns the line's char count
/// plus the sum of every inlay's `char_width`.
pub fn byte_to_combined_col(line: &str, byte: usize, inlay_offsets: &[(u32, u32)]) -> usize {
    let base = if byte >= line.len() {
        line.chars().count()
    } else {
        line[..byte].chars().count()
    };
    let inlay_shift: usize = inlay_offsets
        .iter()
        .filter(|(orig, _)| (*orig as usize) <= byte)
        .map(|(_, w)| *w as usize)
        .sum();
    base + inlay_shift
}

/// Paint a coloured quad behind every `ranges[i]` that intersects
/// `line_idx`'s row. `line_text` + `inlay_offsets` drive the utf-8
/// byte → combined-char-column conversion (monospace advance
/// assumption matches the cursor and gutter maths).
///
/// Slice X3.full.3 paints BACKGROUNDS only -- the underlying syntax
/// colours of the text remain unchanged. Vim's classic
/// "current_match inverts fg" is deferred until a slice that
/// re-shapes the covered text with a different `TextRun`; the bg
/// alone is enough to make matches visible against the syntax
/// palette.
#[allow(clippy::too_many_arguments)]
fn paint_range_overlay(
    window: &mut gpui::Window,
    ranges: &[lattice_core::protocol::position::Range],
    line_idx: u32,
    line_text: &str,
    inlay_offsets: &[(u32, u32)],
    text_origin_x: gpui::Pixels,
    row_y: gpui::Pixels,
    advance: gpui::Pixels,
    line_height: gpui::Pixels,
    color: u32,
) {
    let line_len = line_text.len();
    for r in ranges {
        if line_idx < r.start.line || line_idx > r.end.line {
            continue;
        }
        let start_byte = if line_idx == r.start.line {
            (r.start.byte as usize).min(line_len)
        } else {
            0
        };
        let end_byte = if line_idx == r.end.line {
            (r.end.byte as usize).min(line_len)
        } else {
            line_len
        };
        if end_byte <= start_byte {
            continue;
        }
        let col_start = byte_to_combined_col(line_text, start_byte, inlay_offsets);
        let col_end = byte_to_combined_col(line_text, end_byte, inlay_offsets);
        if col_end <= col_start {
            continue;
        }
        let quad_x = text_origin_x + advance * (col_start as f32);
        let quad_w = advance * ((col_end - col_start) as f32);
        let quad_bounds = gpui::Bounds::new(
            gpui::point(quad_x, row_y),
            gpui::size(quad_w, line_height),
        );
        window.paint_quad(gpui::fill(quad_bounds, gpui::rgb(color)));
    }
}

/// Slice X3.full.4 helper: build a `(combined_text, runs,
/// inlay_offsets)` triple for one visible line, splicing inlay
/// virtual text into the line at each hint's byte offset.
///
/// - `combined_text` is the line text with inlay strings inserted
///   at each `(orig_byte, _)` position; passed to
///   `WindowTextSystem::shape_line` as-is.
/// - `runs` colours every byte of `combined_text`. Adjacent chars
///   with the same colour merge into one run (the analogue of
///   `build_text_runs`'s collapse); inlay chars always carry
///   `inlay_color` and so break the merge across their boundary.
/// - `inlay_offsets` is `[(orig_byte, char_width)]` sorted by
///   `orig_byte`. Each entry records that an inlay of `char_width`
///   chars was inserted BEFORE byte `orig_byte` of the original
///   line -- so cursor + decoration code can apply the shift via
///   `byte_to_combined_col`.
///
/// `inlays` MUST be sorted by `orig_byte` ascending. Trailing
/// hints whose `orig_byte >= line.len()` paint at end-of-line.
pub fn build_line_with_inlays(
    line: &str,
    spans: &[StyledSpan],
    inlays: &[(usize, &str)],
    font: &gpui::Font,
    inlay_color: u32,
) -> (String, Vec<TextRun>, Vec<(u32, u32)>) {
    let inlay_byte_total: usize = inlays.iter().map(|(_, t)| t.len()).sum();
    let mut b = LineRunBuilder::new(font, line.len() + inlay_byte_total);

    let mut inlay_idx = 0usize;
    for (orig_byte, ch) in line.char_indices() {
        while inlay_idx < inlays.len() && inlays[inlay_idx].0 <= orig_byte {
            let (off, text) = inlays[inlay_idx];
            let char_width = text.chars().count() as u32;
            b.inlay_offsets.push((off as u32, char_width));
            b.emit(text, inlay_color);
            inlay_idx += 1;
        }
        let color = syntax_color(style_at(spans, orig_byte));
        let mut buf = [0u8; 4];
        b.emit(ch.encode_utf8(&mut buf), color);
    }
    // Trailing inlays at or past EOL.
    while inlay_idx < inlays.len() {
        let (off, text) = inlays[inlay_idx];
        let char_width = text.chars().count() as u32;
        b.inlay_offsets.push((off as u32, char_width));
        b.emit(text, inlay_color);
        inlay_idx += 1;
    }
    b.finish()
}

/// Internal builder for `build_line_with_inlays`. Tracks the
/// in-progress text run so adjacent same-colour chars merge.
struct LineRunBuilder<'a> {
    combined: String,
    runs: Vec<TextRun>,
    inlay_offsets: Vec<(u32, u32)>,
    font: &'a gpui::Font,
    current_color: u32,
    current_len: usize,
    started: bool,
}

impl<'a> LineRunBuilder<'a> {
    fn new(font: &'a gpui::Font, capacity: usize) -> Self {
        Self {
            combined: String::with_capacity(capacity),
            runs: Vec::new(),
            inlay_offsets: Vec::new(),
            font,
            current_color: 0,
            current_len: 0,
            started: false,
        }
    }

    fn emit(&mut self, s: &str, color: u32) {
        if !self.started {
            self.current_color = color;
            self.current_len = s.len();
            self.started = true;
        } else if color == self.current_color {
            self.current_len += s.len();
        } else {
            self.runs
                .push(make_run_with_color(self.current_color, self.current_len, self.font));
            self.current_color = color;
            self.current_len = s.len();
        }
        self.combined.push_str(s);
    }

    fn finish(mut self) -> (String, Vec<TextRun>, Vec<(u32, u32)>) {
        if self.started && self.current_len > 0 {
            self.runs.push(make_run_with_color(
                self.current_color,
                self.current_len,
                self.font,
            ));
        }
        (self.combined, self.runs, self.inlay_offsets)
    }
}

fn make_run_with_color(color: u32, len: usize, font: &gpui::Font) -> TextRun {
    TextRun {
        len,
        font: font.clone(),
        color: rgb(color).into(),
        background_color: None,
        underline: None,
        strikethrough: None,
    }
}

/// Catppuccin Mocha overlay2 -- gutter line-number colour for
/// non-cursor lines.
const GUTTER_NORMAL_COLOR: u32 = 0x9399b2;
/// Catppuccin Mocha peach -- fold-start marker colour.
const FOLD_MARKER_COLOR: u32 = 0xfab387;
/// Fold-start glyph (right-pointing triangle).
const FOLD_MARKER_GLYPH: char = '►';

/// Format a gutter row's text content: 1 char fold marker + 1
/// char severity sign + N-char right-aligned line number + 1
/// space. Total width = `2 + gutter_width + 1`.
fn format_gutter_text(meta: &GutterLineMeta, gutter_width: usize) -> String {
    let fold = if meta.fold_start {
        FOLD_MARKER_GLYPH
    } else {
        ' '
    };
    let sev = meta.severity.map(|(g, _)| g).unwrap_or(' ');
    format!(
        "{fold}{sev}{num:>width$} ",
        fold = fold,
        sev = sev,
        num = meta.line_idx as usize + 1,
        width = gutter_width,
    )
}

/// Build the `TextRun`s for a gutter row. Three runs (fold, sev,
/// linenum+trailing-space) with their respective colours.
fn build_gutter_runs(text: &str, meta: &GutterLineMeta, font: gpui::Font) -> Vec<TextRun> {
    let mut runs = Vec::with_capacity(3);
    let mut bytes_consumed = 0usize;

    // Run 1: fold marker.
    let fold_color = if meta.fold_start {
        FOLD_MARKER_COLOR
    } else {
        GUTTER_NORMAL_COLOR
    };
    let fold_char = text.chars().next().unwrap_or(' ');
    let fold_len = fold_char.len_utf8();
    runs.push(TextRun {
        len: fold_len,
        font: font.clone(),
        color: rgb(fold_color).into(),
        background_color: None,
        underline: None,
        strikethrough: None,
    });
    bytes_consumed += fold_len;

    // Run 2: severity sign.
    let sev_color = meta.severity.map(|(_, c)| c).unwrap_or(GUTTER_NORMAL_COLOR);
    let sev_char = text[bytes_consumed..].chars().next().unwrap_or(' ');
    let sev_len = sev_char.len_utf8();
    runs.push(TextRun {
        len: sev_len,
        font: font.clone(),
        color: rgb(sev_color).into(),
        background_color: None,
        underline: None,
        strikethrough: None,
    });
    bytes_consumed += sev_len;

    // Run 3: line number + trailing space.
    let tail_len = text.len() - bytes_consumed;
    if tail_len > 0 {
        runs.push(TextRun {
            len: tail_len,
            font,
            color: rgb(GUTTER_NORMAL_COLOR).into(),
            background_color: None,
            underline: None,
            strikethrough: None,
        });
    }
    runs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_runs_no_spans_one_default_run() {
        let line = "let x = 1;";
        let (combined, runs, offsets) =
            build_line_with_inlays(line, &[], &[], &gpui::font("monospace"), 0x7f849c);
        assert!(offsets.is_empty());
        assert_eq!(combined, line);
        assert_eq!(runs.len(), 1, "no-span line collapses to a single run");
        assert_eq!(runs[0].len, line.len());
        let expected: gpui::Hsla = rgb(syntax_color(SyntaxStyle::Default)).into();
        assert_eq!(runs[0].color, expected);
    }

    #[test]
    fn text_runs_collapse_same_style_split_on_change() {
        let line = "abcde";
        let spans = vec![
            StyledSpan {
                start: 0,
                end: 2,
                style: SyntaxStyle::Keyword,
            },
            StyledSpan {
                start: 2,
                end: 3,
                style: SyntaxStyle::String,
            },
            StyledSpan {
                start: 3,
                end: 5,
                style: SyntaxStyle::Keyword,
            },
        ];
        let (_, runs, _) =
            build_line_with_inlays(line, &spans, &[], &gpui::font("monospace"), 0x7f849c);
        assert_eq!(runs.len(), 3);
        assert_eq!(runs[0].len, 2);
        assert_eq!(runs[1].len, 1);
        assert_eq!(runs[2].len, 2);
        let total: usize = runs.iter().map(|r| r.len).sum();
        assert_eq!(total, line.len());
    }

    #[test]
    fn text_runs_empty_line_no_runs() {
        let (combined, runs, offsets) =
            build_line_with_inlays("", &[], &[], &gpui::font("monospace"), 0x7f849c);
        assert!(combined.is_empty());
        assert!(runs.is_empty());
        assert!(offsets.is_empty());
    }

    #[test]
    fn gutter_text_format_renders_padding_and_default_chars() {
        let meta = GutterLineMeta {
            line_idx: 0,
            fold_start: false,
            severity: None,
        };
        // 1 (' ') + 1 (' ') + 3 ("  1") + 1 (' ') = 6 chars.
        assert_eq!(format_gutter_text(&meta, 3), "    1 ");
    }

    #[test]
    fn gutter_text_format_fold_marker() {
        let meta = GutterLineMeta {
            line_idx: 41,
            fold_start: true,
            severity: None,
        };
        // ► (fold) + ' ' (no severity) + "42 " (right-padded line #) + ' ' (trailing).
        assert_eq!(format_gutter_text(&meta, 3), "►  42 ");
    }

    #[test]
    fn gutter_text_format_severity_glyph() {
        let meta = GutterLineMeta {
            line_idx: 9,
            fold_start: false,
            severity: Some(('E', 0xff0000)),
        };
        assert_eq!(format_gutter_text(&meta, 2), " E10 ");
    }

    #[test]
    fn byte_to_combined_col_no_inlays_matches_char_count() {
        // 'café' is 5 bytes utf-8 (c=1, a=1, f=1, é=2). Char-col at
        // byte 3 (start of é) is 3; the EOL byte (5) is 4 chars.
        assert_eq!(byte_to_combined_col("café", 3, &[]), 3);
        assert_eq!(byte_to_combined_col("café", 5, &[]), 4); // EOL
        // ASCII fast path: char-col == byte for the whole prefix.
        assert_eq!(byte_to_combined_col("abc", 2, &[]), 2);
    }

    #[test]
    fn byte_to_combined_col_shifts_by_inlay_widths_before_or_at_byte() {
        // Line "let x = 1;", inlay ": i32" (5 chars) spliced at
        // byte 5 (just after "let x"). Cursor at byte 5 ("=") sits
        // AFTER the inlay -- the inlay's orig_byte (5) is <= cursor
        // byte (5), so col shifts by 5.
        let line = "let x = 1;";
        let inlays = vec![(5u32, 5u32)];
        assert_eq!(byte_to_combined_col(line, 5, &inlays), 5 + 5);
        // Cursor at byte 4 ("x") sits BEFORE the inlay -- no shift.
        assert_eq!(byte_to_combined_col(line, 4, &inlays), 4);
        // Cursor at EOL: shift still applies.
        assert_eq!(byte_to_combined_col(line, line.len(), &inlays), line.chars().count() + 5);
    }

    #[test]
    fn build_line_with_inlays_no_inlays_matches_build_text_runs() {
        let line = "let x = 1;";
        let spans = vec![StyledSpan {
            start: 0,
            end: 3,
            style: SyntaxStyle::Keyword,
        }];
        let font = gpui::font("monospace");
        let (combined, runs, offsets) =
            build_line_with_inlays(line, &spans, &[], &font, 0x7f849c);
        assert_eq!(combined, line, "no inlays => combined == line");
        assert!(offsets.is_empty(), "no inlays => no offsets");
        let total_len: usize = runs.iter().map(|r| r.len).sum();
        assert_eq!(total_len, line.len(), "runs must cover the line");
    }

    #[test]
    fn build_line_with_inlays_splices_text_and_records_offset() {
        let line = "let x = 1;";
        let spans: Vec<StyledSpan> = Vec::new();
        let inlays: Vec<(usize, &str)> = vec![(5, ": i32")];
        let font = gpui::font("monospace");
        let (combined, runs, offsets) =
            build_line_with_inlays(line, &spans, &inlays, &font, 0x7f849c);
        assert_eq!(
            combined, "let x: i32 = 1;",
            "inlay text spliced before byte 5 of orig line"
        );
        assert_eq!(offsets, vec![(5, 5)], "one inlay, 5 chars wide");
        let total_len: usize = runs.iter().map(|r| r.len).sum();
        assert_eq!(total_len, combined.len(), "runs cover combined text");
        // 3 runs: orig prefix (default color), inlay (overlay1),
        // orig suffix (default color again).
        assert_eq!(runs.len(), 3);
        let expected_inlay_color: gpui::Hsla = rgb(0x7f849c).into();
        assert_eq!(runs[1].color, expected_inlay_color);
    }

    #[test]
    fn build_line_with_inlays_trailing_inlay_appended_at_eol() {
        let line = "fn foo()";
        let inlays: Vec<(usize, &str)> = vec![(line.len(), " -> i32")];
        let (combined, _, offsets) =
            build_line_with_inlays(line, &[], &inlays, &gpui::font("monospace"), 0x7f849c);
        assert_eq!(combined, "fn foo() -> i32");
        assert_eq!(offsets, vec![(line.len() as u32, 7)]);
    }
}
