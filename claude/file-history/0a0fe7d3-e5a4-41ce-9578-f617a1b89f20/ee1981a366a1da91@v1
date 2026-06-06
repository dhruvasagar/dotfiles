//! Frame rendering. Pure where it can be (line composition is testable);
//! IO-bound where ratatui needs it (`draw_frame` accepting a `Frame`).
//!
//! Layout:
//!
//! +----------------------------------------------------------------+
//! | gutter | buffer text                                           |
//! | gutter | buffer text                                           |
//! | ...                                                            |
//! +----------------------------------------------------------------+
//! | mode line: \[NORMAL\]  path                line:col   lang     |
//! +----------------------------------------------------------------+

use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style as TuiStyle};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph, Wrap};

use lattice_grammar::{ModalState, SearchDirection};
use lattice_lsp::{Diagnostic as LspDiagnostic, DiagnosticSeverity};
use lattice_protocol::position::Range as ProtoRange;
use lattice_protocol::selection::VisualMode;
use lattice_runtime::DocumentSnapshot;
use lattice_syntax::{Lang, Style, StyledSpan};

use crate::app::{App, EchoLevel};

/// Render one terminal frame.
///
/// `snap` is the active document's snapshot, loaded once per frame
/// by the runtime via `app.snapshot_cache.load_arc()` (DESIGN.md
/// §5.6.8). All active-pane render paths read through this single
/// snapshot -- inactive panes (different documents) still go
/// through `entry.handle.snapshot()` since the cache is per-cell.
pub fn draw_frame(frame: &mut Frame, app: &App, snap: &DocumentSnapshot) {
    // Vertico-style layout (DESIGN.md §5.11.3, §5.9.7): when the
    // cmdline completion popup OR the picker is open, an extra row
    // band sits below the cmdline holding the candidate list. The
    // selected candidate sits visually adjacent to the prompt
    // (above for completion, below for picker), alternatives
    // fanning away. Without either open the layout is the standard
    // buffer / mode-line / cmdline three.
    //
    // Picker takes precedence over completion when both are open
    // (only one is reachable interactively at a time, but the
    // ordering matters for layout sizing).
    let picker_rows = app
        .picker
        .as_ref()
        .map(|p| popup_height(p.candidates.len().max(1)))
        .unwrap_or(0);
    let completion_rows = app
        .completion_state
        .as_ref()
        .map(|s| popup_height(s.candidates.len()))
        .unwrap_or(0);
    let extra_rows = picker_rows.max(completion_rows);

    let constraints: Vec<Constraint> = if extra_rows > 0 {
        vec![
            Constraint::Min(1),                    // buffer
            Constraint::Length(1),                 // mode line
            Constraint::Length(1),                 // cmdline / picker query
            Constraint::Length(extra_rows as u16), // candidate list (bottom)
        ]
    } else {
        vec![
            Constraint::Min(1),
            Constraint::Length(1),
            Constraint::Length(1),
        ]
    };
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints(constraints)
        .split(frame.area());

    draw_panes(frame, chunks[0], app, snap);
    draw_mode_line(frame, chunks[1], app, snap);
    // Picker query takes over the cmdline row when active; the
    // user types in the picker's own query buffer, not the `:`
    // line, so the cmdline / echo content is hidden until the
    // picker dismisses.
    if app.picker.is_some() {
        draw_picker_prompt(frame, chunks[2], app);
    } else {
        draw_command_or_echo(frame, chunks[2], app);
    }
    // Help popup overlay -- painted whenever a help_buffer is
    // set AND the active pane isn't already showing it as an
    // in-pane buffer (the in-pane case is handled by the Help
    // arm of `draw_panes`). Two scenarios trigger this:
    // - **State A** (active = Document, help_buffer = Some):
    //   first `K` shown the popup, focus is still on the doc;
    //   doc paints normally below, popup floats on top, no
    //   cursor inside the popup.
    // - **State B** (active = Help via popup mode, pane.buffer =
    //   Document): second `K` moved focus into the popup; popup
    //   paints with a visible cursor at `app.cursor`; doc paints
    //   as inactive (frozen at `pane.cursor`) below.
    let active_pane_kind = app.pane_tree.active().buffer;
    if app.help_buffer.is_some() && active_pane_kind != crate::buffers::BufferKind::Help {
        draw_help_overlay(frame, chunks[0], app, snap);
    }
    // Picker candidate list (precedence over completion popup --
    // only one is interactive at a time).
    if picker_rows > 0 {
        draw_picker_candidates(frame, chunks[3], app);
    } else if completion_rows > 0 {
        draw_completion_popup(frame, chunks[3], app);
    }
    // Insert-mode completion popup overlay (Phase 4.2.g.1).
    // Anchored at the cursor; floats over the buffer; doesn't
    // claim the cmdline row (so echoes can still appear).
    // Painted last so it sits on top of any pane-area widgets.
    if app.insert_completion.is_some() {
        draw_insert_completion_popup(frame, chunks[0], app, snap);
        // Side documentation popup (Phase 4.2.g.3) -- only
        // rendered when the user has flipped it on with
        // `<C-d>`. Anchored right of the candidate popup
        // when there's room; below otherwise.
        if let Some(state) = app.insert_completion.as_ref()
            && state.doc_popup.is_some()
        {
            draw_insert_completion_docs_popup(frame, chunks[0], app, snap);
        }
    }
}

/// Total rows the popup occupies (no borders -- vertico-style;
/// matches the picker's candidate-list shape) capped so it
/// never dominates the screen.
fn popup_height(candidate_count: usize) -> usize {
    const MAX_ROWS: usize = 10;
    candidate_count.min(MAX_ROWS).max(1)
}

/// Vertico-style cmdline completion popup (DESIGN.md §5.11.3,
/// **Insert-mode completion popup** (Phase 4.2.g.1, design
/// in `docs/insert-completion.md` §5). Multi-column layout:
/// `[kind glyph] [label]   [detail]   [src]`. Anchored below
/// the cursor at the popup's `anchor` position; falls back to
/// above when there's no room below. Selected row reverse-
/// videoed; matched byte ranges in the label are painted with
/// the match face.
///
/// Width capped at 60 cells; height capped at 12 rows. Doc-
/// popup side panel + width-aware column dropping land in
/// 4.2.g.3 / 4.2.g.5.
fn draw_insert_completion_popup(
    frame: &mut Frame,
    buffer_area: Rect,
    app: &App,
    snap: &DocumentSnapshot,
) {
    let Some(state) = app.insert_completion.as_ref() else {
        return;
    };
    if state.rendered.is_empty() {
        return;
    }
    // Width: cap at 60 cells, fits at least 30.
    let width: u16 = 60u16.min(buffer_area.width.saturating_sub(2)).max(30);
    // Height: cap at 12, but never more than the candidate
    // count + the selected row's surrounding band.
    let max_h: u16 = 12;
    let want_h = (state.rendered.len() as u16).min(max_h).max(1);
    // Anchor screen position: the cursor's screen position
    // is what we want, since the popup sits at the user's
    // typing point. Active pane content rect computed via
    // the helper from the hover popup path.
    let pane_rect = active_pane_content_rect(app, buffer_area).unwrap_or(buffer_area);
    let anchor_screen = cursor_screen_position_at(
        app,
        snap,
        pane_rect,
        app.cursor,
        app.scroll,
    );
    let (anchor_x, anchor_y) = anchor_screen.unwrap_or((buffer_area.x, buffer_area.y));
    // Below if there's room, else above.
    let area_bottom = buffer_area.y + buffer_area.height;
    let space_below = area_bottom.saturating_sub(anchor_y + 1);
    let space_above = anchor_y.saturating_sub(buffer_area.y);
    let height = want_h.min(space_below.max(space_above));
    if height == 0 {
        return;
    }
    let y = if space_below >= height {
        anchor_y + 1
    } else {
        anchor_y.saturating_sub(height)
    };
    let max_x = (buffer_area.x + buffer_area.width).saturating_sub(width);
    let x = anchor_x.min(max_x).max(buffer_area.x);
    let popup = Rect {
        x,
        y,
        width,
        height,
    };
    frame.render_widget(Clear, popup);
    // Window the visible slice so the selected row stays on
    // screen. Selected sticks at the top band when reachable;
    // scrolls down when the selection passes the visible-row
    // count.
    let visible_count = popup.height as usize;
    let scroll = if state.selected < visible_count {
        0
    } else {
        state.selected + 1 - visible_count
    };
    let lines: Vec<Line> = state
        .rendered
        .iter()
        .enumerate()
        .skip(scroll)
        .take(visible_count)
        .map(|(i, c)| insert_candidate_line(c, i == state.selected, popup.width))
        .collect();
    let para = Paragraph::new(lines);
    frame.render_widget(para, popup);
}

/// **Insert-mode completion docs side popup** (Phase
/// 4.2.g.3). Anchored right of the candidate popup when
/// there's room (typical wide terminals); falls back to
/// below the candidate popup when narrow. Renders the
/// focused item's `documentation` (lazy-resolved via
/// `completionItem/resolve`) wrapped to the popup width.
/// Title bar shows "docs" + the focused candidate's label.
///
/// `<C-f>` / `<C-b>` (inside the completion-popup minor
/// mode) page through the body via `state.doc_popup.scroll`.
fn draw_insert_completion_docs_popup(
    frame: &mut Frame,
    buffer_area: Rect,
    app: &App,
    snap: &DocumentSnapshot,
) {
    let Some(state) = app.insert_completion.as_ref() else {
        return;
    };
    let Some(doc_popup) = state.doc_popup.as_ref() else {
        return;
    };
    // Anchor: same anchor as the candidate popup. Pull the
    // active pane rect for placement math.
    let pane_rect = active_pane_content_rect(app, buffer_area).unwrap_or(buffer_area);
    let anchor_screen = cursor_screen_position_at(
        app,
        snap,
        pane_rect,
        app.cursor,
        app.scroll,
    );
    let (anchor_x, anchor_y) = anchor_screen.unwrap_or((buffer_area.x, buffer_area.y));
    // Candidate popup geometry (mirrors `draw_insert_completion_popup`).
    let cand_width: u16 = 60u16.min(buffer_area.width.saturating_sub(2)).max(30);
    let cand_height: u16 = 12u16.min(state.rendered.len() as u16).max(1);
    let area_bottom = buffer_area.y + buffer_area.height;
    let space_below = area_bottom.saturating_sub(anchor_y + 1);
    let cand_y = if space_below >= cand_height {
        anchor_y + 1
    } else {
        anchor_y.saturating_sub(cand_height)
    };
    let cand_max_x = (buffer_area.x + buffer_area.width).saturating_sub(cand_width);
    let cand_x = anchor_x.min(cand_max_x).max(buffer_area.x);
    // Docs popup: try to fit right of the candidate popup.
    // If there's not enough room, place below the candidate
    // popup instead.
    let cand_right = cand_x + cand_width;
    let space_right =
        (buffer_area.x + buffer_area.width).saturating_sub(cand_right + 1);
    let docs_width: u16 = 60u16.min(space_right).max(0);
    let (x, y, width, height) = if docs_width >= 30 {
        // Right side, same vertical extent as the candidate
        // popup.
        (
            cand_right + 1,
            cand_y,
            docs_width,
            cand_height,
        )
    } else {
        // Below the candidate popup, full popup width, capped
        // at remaining vertical space.
        let below_y = cand_y + cand_height;
        let below_h = area_bottom.saturating_sub(below_y).min(8);
        if below_h < 3 {
            return;
        }
        (cand_x, below_y, cand_width, below_h)
    };
    if width < 20 || height < 3 {
        return;
    }
    let popup = Rect {
        x,
        y,
        width,
        height,
    };
    frame.render_widget(Clear, popup);
    let block = Block::default()
        .borders(Borders::ALL)
        .title(" docs (<C-d>) ");
    let inner = block.inner(popup);
    frame.render_widget(block, popup);
    // Body. Plain-text rendering for v1; markdown highlight
    // can layer on once the help-popup pipeline gets reused
    // (4.2.g.5+). Wrap so long lines paginate naturally.
    let body_text: String = doc_popup
        .body
        .clone()
        .unwrap_or_else(|| "(loading…)".to_string());
    // Apply scroll: skip the first `scroll` lines.
    let visible_body: String = body_text
        .lines()
        .skip(doc_popup.scroll as usize)
        .collect::<Vec<_>>()
        .join("\n");
    let para = Paragraph::new(visible_body)
        .wrap(Wrap { trim: false })
        .style(TuiStyle::default().fg(Color::Gray));
    frame.render_widget(para, inner);
}

/// Render one Insert-mode-completion candidate row. Three
/// columns: kind glyph (3 cells) / label with match-face
/// highlighting (≤ 30 cells) / source tag right-aligned
/// (3-4 cells). Detail column lands in 4.2.g.3 once LSP
/// items carry signatures; for buffer-words there's no
/// detail to show.
fn insert_candidate_line<'a>(
    c: &'a lattice_completion::RenderedCandidate,
    selected: bool,
    width: u16,
) -> Line<'a> {
    let row_style = if selected {
        TuiStyle::default()
            .bg(Color::DarkGray)
            .add_modifier(Modifier::BOLD)
    } else {
        TuiStyle::default()
    };
    let match_style = TuiStyle::default()
        .fg(Color::Cyan)
        .add_modifier(Modifier::BOLD);
    let glyph = candidate_kind_glyph(c.raw.kind);
    // 3-cell kind column (selected/unselected) + leading space.
    let mut spans: Vec<Span<'a>> = vec![Span::styled(
        format!(" {glyph}  "),
        row_style,
    )];
    // Label with match-face spans on `c.match_ranges`.
    let label = &c.raw.display;
    let mut cursor = 0usize;
    let mut sorted: Vec<_> = c.match_ranges.clone();
    sorted.sort_by_key(|r| r.start);
    for range in sorted {
        if range.start >= label.len() || range.end > label.len() || range.start >= range.end {
            continue;
        }
        if range.start > cursor {
            spans.push(Span::styled(
                label[cursor..range.start].to_string(),
                row_style,
            ));
        }
        spans.push(Span::styled(
            label[range.start..range.end].to_string(),
            if selected {
                match_style
                    .bg(Color::DarkGray)
                    .add_modifier(Modifier::BOLD)
            } else {
                match_style
            },
        ));
        cursor = range.end;
    }
    if cursor < label.len() {
        spans.push(Span::styled(
            label[cursor..].to_string(),
            row_style,
        ));
    }
    // Source tag, right-aligned. Inferred from kind for v1 --
    // CandidateData::Plain doesn't carry a source id today.
    // 4.2.g.5 will plumb the SourceId into RenderedCandidate
    // (typed routing payload work) and this falls out.
    let source_tag = source_tag_for_kind(c.raw.kind);
    // Pad to push the tag right-aligned. Computing the
    // visible width of the spans we just emitted is cheap
    // for v1 (labels are short).
    let label_len: usize =
        spans.iter().map(|s| s.content.chars().count()).sum();
    let target_pad = (width as usize)
        .saturating_sub(label_len + source_tag.len() + 1);
    if target_pad > 0 {
        spans.push(Span::styled(" ".repeat(target_pad), row_style));
    }
    spans.push(Span::styled(
        format!(" {source_tag}"),
        if selected {
            row_style.fg(Color::DarkGray)
        } else {
            TuiStyle::default().fg(Color::DarkGray)
        },
    ));
    Line::from(spans)
}

/// Single-glyph icon for a candidate's `CandidateKind`.
/// Mirrors `symbol_kind_glyph` / `completion_kind_glyph` in
/// the LSP path -- once the LSP source plugs into the popup
/// (4.2.g.2) those map straight through.
fn candidate_kind_glyph(kind: lattice_completion::CandidateKind) -> &'static str {
    use lattice_completion::CandidateKind as K;
    match kind {
        K::Command => ":",
        K::Option => "⚙",
        K::File => "📄",
        K::Directory => "📁",
        K::Pattern => "/",
        K::Buffer => "▤",
        K::Register => "\"",
        K::Mark => "'",
        K::Chord => "⌘",
        K::Plain => "·",
        K::Extension(_) => "+",
    }
}

/// Source tag rendered right-aligned in the popup row. Today
/// inferred from kind; 4.2.g.5 plumbs the `SourceId` directly
/// onto the candidate so the tag matches the actual source.
fn source_tag_for_kind(kind: lattice_completion::CandidateKind) -> &'static str {
    use lattice_completion::CandidateKind as K;
    match kind {
        K::File | K::Directory => "path",
        K::Buffer => "buf",
        K::Plain => "buf",
        _ => "",
    }
}

/// §5.9.7). Sits BELOW the `:` prompt; the selected candidate is
/// the FIRST visible row (closest to the prompt above), alternatives
/// fan downward. Same visual shape as
/// [`draw_picker_candidates`] -- no border, no title bar, just the
/// candidate list. The candidate-count hint is appended to the
/// cmdline itself by [`draw_command_or_echo`] when completion is
/// open, matching the picker's prompt-inline `(n/m)` style.
fn draw_completion_popup(frame: &mut Frame, popup_area: Rect, app: &App) {
    let Some(state) = app.completion_state.as_ref() else {
        return;
    };
    if state.candidates.is_empty() {
        return;
    }

    frame.render_widget(Clear, popup_area);
    let inner = popup_area;

    // Visible window. Selected stays in view as the user advances
    // with Tab; once it would scroll off the bottom, we shift the
    // window so the selected sits at the bottom row.
    let visible_count = inner.height as usize;
    if visible_count == 0 {
        return;
    }
    let scroll = if state.selected < visible_count {
        0
    } else {
        state.selected + 1 - visible_count
    };
    let visible: Vec<Line> = state
        .candidates
        .iter()
        .enumerate()
        .skip(scroll)
        .take(visible_count)
        .map(|(i, c)| candidate_to_line(c, i == state.selected, inner.width))
        .collect();
    let para = Paragraph::new(visible);
    frame.render_widget(para, inner);
}

/// Render one candidate as a single styled line. Matched byte
/// ranges are painted with a distinct style; annotations
/// right-aligned in the row's remaining width.
fn candidate_to_line<'a>(
    c: &'a lattice_completion::RenderedCandidate,
    selected: bool,
    width: u16,
) -> Line<'a> {
    let prefix = if selected { "▶ " } else { "  " };
    let row_style = if selected {
        TuiStyle::default()
            .bg(Color::DarkGray)
            .add_modifier(Modifier::BOLD)
    } else {
        TuiStyle::default()
    };
    let match_style = TuiStyle::default()
        .fg(Color::Cyan)
        .add_modifier(Modifier::BOLD);

    // Build spans: text with match-range highlighting, then padding,
    // then annotations on the right.
    let text = &c.raw.display;
    let mut spans: Vec<Span<'a>> = Vec::new();
    spans.push(Span::styled(prefix, row_style));

    // Walk text + match_ranges to paint runs.
    let mut cursor = 0usize;
    let mut sorted_ranges: Vec<_> = c
        .match_ranges
        .iter()
        .filter(|r| r.start < r.end && r.end <= text.len())
        .cloned()
        .collect();
    sorted_ranges.sort_by_key(|r| r.start);
    for range in sorted_ranges {
        if range.start > cursor {
            spans.push(Span::styled(
                text[cursor..range.start].to_string(),
                row_style,
            ));
        }
        spans.push(Span::styled(
            text[range.start..range.end].to_string(),
            match_style,
        ));
        cursor = range.end;
    }
    if cursor < text.len() {
        spans.push(Span::styled(text[cursor..].to_string(), row_style));
    }

    // Annotations right-aligned. Use a foreground that contrasts
    // with the row background -- on a selected row, `DarkGray` fg
    // would vanish into the `DarkGray` bg, hiding the marginalia.
    let annotations = c.annotations.join("  ");
    if !annotations.is_empty() {
        let used: usize = prefix.len() + text.len();
        let want = annotations.len() + 2;
        let pad = (width as usize).saturating_sub(used + want);
        spans.push(Span::styled(" ".repeat(pad + 2), row_style));
        let annotation_fg = if selected {
            Color::Gray
        } else {
            Color::DarkGray
        };
        spans.push(Span::styled(annotations, row_style.fg(annotation_fg)));
    }
    Line::from(spans)
}

/// Draw the help buffer (DESIGN.md §5.11) as a centred popup. Popup
/// is the v1 display strategy; multi-buffer support brings split /
/// Vertico-style picker prompt (DESIGN.md §5.9.7) drawn in the
/// cmdline row when a [`crate::picker::Picker`] is open. Format:
/// `<title>> <query>` -- the title stands in for the `:` prompt
/// so the user knows what they're picking, and `query` is the
/// live filter they're typing. Sits at the screen bottom; the
/// candidate list is rendered below by
/// [`draw_picker_candidates`].
fn draw_picker_prompt(frame: &mut Frame, area: Rect, app: &App) {
    let Some(p) = app.picker.as_ref() else {
        return;
    };
    let count = if p.candidates.is_empty() {
        format!("(0/0) ")
    } else {
        format!("({}/{}) ", p.selected + 1, p.candidates.len())
    };
    let para = Paragraph::new(Line::from(vec![
        Span::styled(
            format!("{}> ", p.title),
            TuiStyle::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        ),
        Span::raw(p.query.clone()),
        Span::styled(
            format!("  {count}"),
            TuiStyle::default().fg(Color::DarkGray),
        ),
    ]));
    frame.render_widget(para, area);
}

/// Vertico-style candidate list (DESIGN.md §5.9.7) drawn in the
/// row band below the picker prompt. Selected row sits at the
/// TOP of the band (closest to the prompt below), alternatives
/// fan upward in match-rank order. Reuses [`candidate_to_line`]
/// for per-row rendering so match highlights + marginalia stay
/// consistent with the cmdline completion popup.
fn draw_picker_candidates(frame: &mut Frame, area: Rect, app: &App) {
    let Some(p) = app.picker.as_ref() else {
        return;
    };
    frame.render_widget(Clear, area);
    if p.candidates.is_empty() {
        let empty = Paragraph::new(Line::from(Span::styled(
            "  (no matches)",
            TuiStyle::default().fg(Color::DarkGray),
        )));
        frame.render_widget(empty, area);
        return;
    }
    // Window the visible slice around the selected candidate so
    // it's always on screen. With the prompt ABOVE the list
    // (vertico's flipped variant), the selected candidate sits at
    // the TOP of the band (closest to the prompt) so the eye
    // tracks naturally from query to selection.
    let visible_count = area.height as usize;
    if visible_count == 0 {
        return;
    }
    let scroll = if p.selected < visible_count {
        0
    } else {
        p.selected + 1 - visible_count
    };
    let visible: Vec<Line> = p
        .candidates
        .iter()
        .enumerate()
        .skip(scroll)
        .take(visible_count)
        .map(|(i, c)| candidate_to_line(c, i == p.selected, area.width))
        .collect();
    let para = Paragraph::new(visible);
    frame.render_widget(para, area);
}

/// tab / window targets per [`crate::help::HelpDisplayMode`]. Width is
/// `min(buffer_width - 4, 100)`, height is 70% of the buffer area.
/// Content is the [`crate::help::HelpBuffer`]'s rope text; we slice
/// the visible window from the rendered string. Link markup
/// (`[[…]]`) renders verbatim today; future passes paint the link
/// ranges with a distinct style and add a follow-link motion.
fn draw_help_overlay(
    frame: &mut Frame,
    buffer_area: Rect,
    app: &App,
    snap: &DocumentSnapshot,
) {
    let Some(help) = app.help_buffer.as_ref() else {
        return;
    };
    // Tooltip-style sizing: cap to a reasonable max so the popup
    // doesn't dominate the screen. Height auto-fits content (line
    // count + 2 for borders), capped at 20 rows or half the buffer
    // area, whichever is smaller. Width caps at 80 cells, with a
    // 30-cell minimum for usability.
    //
    // The inner height (height - 2 borders) is the popup's motion
    // viewport. App::help_popup_inner_height computes the same
    // value for `set_viewport_height`, so motion / scroll /
    // ensure_cursor_visible match the rows the renderer actually
    // paints; without that, `j` past the last visible row would
    // silently advance `cursor.line`.
    let line_count = help.line_count().max(1) as u16;
    let max_h = (buffer_area.height / 2).max(5).min(20);
    let height = (line_count.saturating_add(2)).min(max_h).max(5);
    let width = (buffer_area.width.saturating_sub(4)).clamp(30, 80);
    let popup = position_help_popup(app, snap, buffer_area, width, height);

    frame.render_widget(Clear, popup);

    let block = Block::default()
        .borders(Borders::ALL)
        .title(format!(" {} (q / Esc to dismiss) ", help.title));
    let inner = block.inner(popup);
    frame.render_widget(block, popup);

    // Pull the visible window out of the help buffer's rope text.
    // Allocates per frame, but only across the visible viewport
    // (~30 lines) -- well under any latency budget for a help
    // surface. Highlights were pre-computed at help-buffer build
    // time via the markdown grammar; we just look them up by line
    // and emit per-row styled spans.
    let viewport = inner.height as usize;
    // Active-buffer scroll lives on `app.scroll` after the
    // unification; help_buffer's own `scroll` field is archival
    // save-state synced at activation transitions.
    let scroll = if matches!(app.active_buffer, crate::buffers::BufferKind::Help) {
        app.scroll as usize
    } else {
        help.scroll
    };
    let lines = help.lines();
    let visible: Vec<Line> = lines
        .iter()
        .skip(scroll)
        .take(viewport)
        .enumerate()
        .map(|(i, l)| {
            let line_idx = scroll + i;
            let mut spans: Vec<lattice_syntax::StyledSpan> =
                help.highlights.get(line_idx).cloned().unwrap_or_default();
            // Layer Style::Link decoration on every link's label
            // range that touches this line. tree-sitter-md 0.3.x's
            // inline injection is unreliable so we paint link
            // styling from the parsed HelpLinks (same hlsearch-
            // style overlay model the buffer renderer uses).
            for link in help.links.iter() {
                if let Some((s, e)) = link_label_range_on_line(link, line_idx as u32) {
                    let line_len = l.len();
                    let s = s.min(line_len);
                    let e = e.min(line_len);
                    if s < e {
                        spans.push(lattice_syntax::StyledSpan {
                            start: s,
                            end: e,
                            style: lattice_syntax::Style::Link,
                        });
                    }
                }
            }
            let mut body = render_help_line(l, &spans);
            // Hlsearch / current_match overlays -- same painter
            // the document path and the in-pane help variant use,
            // so `/foo` in a focused popup shows highlights too.
            // Only paints when help is actually focused (search
            // state is active-buffer-relative).
            if matches!(app.active_buffer, crate::buffers::BufferKind::Help) {
                let line_len = l.len();
                for &range in app.all_matches.iter() {
                    if let Some((overlay_start, overlay_end)) =
                        match_overlay_range(range, line_idx as u32, line_len)
                    {
                        body = apply_match_overlay(
                            body,
                            overlay_start,
                            overlay_end,
                            hlsearch_style(),
                        );
                    }
                }
                if let Some(range) = app.current_match
                    && let Some((overlay_start, overlay_end)) =
                        match_overlay_range(range, line_idx as u32, line_len)
                {
                    body = apply_match_overlay(
                        body,
                        overlay_start,
                        overlay_end,
                        match_style(),
                    );
                }
            }
            Line::from(body)
        })
        .collect();
    // Always wrap inside help / log / `:lsp-trace-log` popups --
    // the content is prose / JSON-RPC payloads / log records, not
    // code, and the right-edge clip on long lines hides the data
    // the user opened the buffer to read. Decoupled from the
    // global `wrap` option (which targets code buffers); help
    // buffers wrap unconditionally. `trim: false` preserves
    // leading whitespace on continuation rows so markdown /
    // fenced-code indentation survives. Cursor positioning at the
    // end of this fn still uses the unwrapped line index -- it
    // lands on the wrap origin row, good-enough until the cursor
    // walker grows wrap-awareness.
    let para = Paragraph::new(visible).wrap(Wrap { trim: false });
    frame.render_widget(para, inner);

    // Place the terminal cursor INSIDE the popup only in State
    // B -- focus has moved into it (active_buffer == Help) and
    // vim grammar acts on the popup's content. In State A the
    // popup is shown but focus is still on the main buffer; the
    // cursor stays where the doc renderer placed it (on the
    // symbol the user K'd) so the user knows what the popup is
    // about. No cursor placement here in that case.
    if inner.height > 0
        && inner.width > 0
        && matches!(app.active_buffer, crate::buffers::BufferKind::Help)
    {
        let row_off = (app.cursor.line as usize).saturating_sub(scroll);
        let row_off = row_off.min(inner.height.saturating_sub(1) as usize);
        let col_off = (app.cursor.byte as usize).min(inner.width.saturating_sub(1) as usize);
        frame.set_cursor_position((inner.x + col_off as u16, inner.y + row_off as u16));
    }
}

/// Tooltip-style placement for the hover / help popup overlay.
///
/// Anchors the popup near the *document* cursor that triggered it,
/// rather than centering. Falls back to the centered position if
/// the cursor isn't visible (off-screen scroll, no active doc
/// pane, etc.).
///
/// Placement rules:
/// - Below cursor when there's room; otherwise above.
/// - Horizontally aligned to the cursor column, shifted left to
///   keep the popup inside `buffer_area`.
/// - In State A (active = Document) the doc cursor is `app.cursor`
///   / `app.scroll`; in State B (active = Help) it lives in the
///   active pane's stash.
fn position_help_popup(
    app: &App,
    snap: &DocumentSnapshot,
    buffer_area: Rect,
    width: u16,
    height: u16,
) -> Rect {
    let centered = || {
        let cx = buffer_area.x + buffer_area.width.saturating_sub(width) / 2;
        let cy = buffer_area.y + buffer_area.height.saturating_sub(height) / 2;
        Rect {
            x: cx,
            y: cy,
            width,
            height,
        }
    };
    let pane_area = match active_pane_content_rect(app, buffer_area) {
        Some(r) => r,
        None => return centered(),
    };
    // Active pane must be a Document for the anchor to make sense
    // (the popup is only painted when active_pane.buffer != Help,
    // so this is the State A / B case where the active pane shows
    // a doc).
    let (cursor, scroll) = match app.active_buffer {
        crate::buffers::BufferKind::Document => (app.cursor, app.scroll),
        _ => {
            let pane = app.pane_tree.active();
            (pane.cursor, pane.scroll)
        }
    };
    let Some((cx, cy)) = cursor_screen_position_at(app, snap, pane_area, cursor, scroll) else {
        return centered();
    };
    // Vertical: prefer below the cursor row; if the popup wouldn't
    // fit, place above. Pin to buffer_area bounds.
    let area_bottom = buffer_area.y + buffer_area.height;
    let space_below = area_bottom.saturating_sub(cy + 1);
    let space_above = cy.saturating_sub(buffer_area.y);
    let y = if space_below >= height {
        cy + 1
    } else if space_above >= height {
        cy.saturating_sub(height)
    } else if space_below >= space_above {
        // Not enough room either side -- pick the larger gap and
        // clamp the popup so it stays on-screen.
        area_bottom.saturating_sub(height).max(buffer_area.y)
    } else {
        buffer_area.y
    };
    // Horizontal: align to cursor column; shift left if it would
    // overflow the buffer area's right edge. Clamp to area.x.
    let max_x = (buffer_area.x + buffer_area.width).saturating_sub(width);
    let x = cx.min(max_x).max(buffer_area.x);
    Rect {
        x,
        y,
        width,
        height,
    }
}

/// Compute the *content* rect (status row excluded) of the active
/// pane within `buffer_area`. Replicates the layout
/// [`draw_panes`] computes per pane. Returns `None` if the pane
/// tree has no active leaf (shouldn't happen in practice).
fn active_pane_content_rect(app: &App, buffer_area: Rect) -> Option<Rect> {
    let pane_area = crate::pane::PaneRect {
        x: buffer_area.x,
        y: buffer_area.y,
        width: buffer_area.width,
        height: buffer_area.height,
    };
    let rects = app.pane_tree.compute_rects(pane_area);
    let active_idx = app.pane_tree.active_index();
    let multi = rects.len() > 1;
    let prect = rects
        .iter()
        .find(|(idx, _)| *idx == active_idx)
        .map(|(_, r)| *r)?;
    let rect = Rect {
        x: prect.x,
        y: prect.y,
        width: prect.width,
        height: prect.height,
    };
    if multi && rect.height >= 2 {
        Some(Rect {
            x: rect.x,
            y: rect.y,
            width: rect.width,
            height: rect.height - 1,
        })
    } else {
        Some(rect)
    }
}

/// True iff the active pane's buffer kind is the same kind as
/// `app.active_buffer`. When mismatched, the active pane is
/// painted as visually inactive (frozen at `pane.cursor`) -- the
/// scenario that matters is help-popup-overlay (State B) where
/// the active pane shows a Document but motions go to the help
/// popup's buffer.
fn pane_buffer_matches_active(app: &App, idx: usize) -> bool {
    app.pane_tree
        .leaves()
        .get(idx)
        .map(|p| p.buffer == app.active_buffer)
        .unwrap_or(false)
}

/// Paint the help buffer directly into a pane's content area
/// when help is the active in-pane buffer. Same per-line painter
/// the popup overlay uses, plus the document buffer's hlsearch /
/// current_match overlays so `/` `n` `N` look right.
///
/// No border, no title, no popup framing: the pane area IS the
/// help content. Per-pane status line (drawn separately by
/// `draw_pane_status_line`) shows the title.
fn draw_help_in_pane(frame: &mut Frame, area: Rect, app: &App) {
    let Some(help) = app.help_buffer.as_ref() else {
        return;
    };
    let viewport = area.height as usize;
    let scroll = app.scroll as usize;
    let lines = help.lines();
    let cursor_line = app.cursor.line as usize;
    let visible: Vec<Line> = lines
        .iter()
        .skip(scroll)
        .take(viewport)
        .enumerate()
        .map(|(i, l)| {
            let line_idx = scroll + i;
            let mut spans: Vec<lattice_syntax::StyledSpan> =
                help.highlights.get(line_idx).cloned().unwrap_or_default();
            for link in help.links.iter() {
                if let Some((s, e)) = link_label_range_on_line(link, line_idx as u32) {
                    let line_len = l.len();
                    let s = s.min(line_len);
                    let e = e.min(line_len);
                    if s < e {
                        spans.push(lattice_syntax::StyledSpan {
                            start: s,
                            end: e,
                            style: lattice_syntax::Style::Link,
                        });
                    }
                }
            }
            let mut body = render_help_line(l, &spans);
            let line_len = l.len();
            // Hlsearch overlay: every `app.all_matches` range that
            // touches this line. Same painter the document path
            // uses, so visual + match styles compose identically.
            for &range in app.all_matches.iter() {
                if let Some((overlay_start, overlay_end)) =
                    match_overlay_range(range, line_idx as u32, line_len)
                {
                    body = apply_match_overlay(body, overlay_start, overlay_end, hlsearch_style());
                }
            }
            // Current-match (the one the cursor is on after `/`
            // submit / `n` / `N`) gets the louder match style.
            if let Some(range) = app.current_match
                && let Some((overlay_start, overlay_end)) =
                    match_overlay_range(range, line_idx as u32, line_len)
            {
                body = apply_match_overlay(body, overlay_start, overlay_end, match_style());
            }
            Line::from(body)
        })
        .collect();
    let para = Paragraph::new(visible).wrap(Wrap { trim: false });
    frame.render_widget(para, area);
    // Cursor placement: same math as the popup path.
    if area.height > 0 && area.width > 0 {
        let row_off = cursor_line.saturating_sub(scroll);
        let row_off = row_off.min(area.height.saturating_sub(1) as usize);
        let col_off = (app.cursor.byte as usize).min(area.width.saturating_sub(1) as usize);
        frame.set_cursor_position((area.x + col_off as u16, area.y + row_off as u16));
    }
}

/// Inactive companion to `draw_help_in_pane`: paint a static help
/// view in a non-active pane (multi-pane sessions where one pane
/// holds a help buffer the user isn't currently looking at). No
/// cursor, dim styling.
fn draw_inactive_help(frame: &mut Frame, area: Rect, app: &App, pane: &crate::pane::PaneState) {
    // Inactive panes use the pane's stashed cursor / scroll
    // (active panes use `app.cursor` / `app.scroll`, but those
    // belong to the focused buffer which isn't this one).
    let scroll = pane.scroll as usize;
    let viewport = area.height as usize;
    // Look up the help content via the registry id this pane
    // tracks; fall back to the popup slot for the legacy path.
    let Some(help) = app
        .buffers
        .help(pane.buffer_id)
        .or(app.help_buffer.as_ref())
    else {
        return;
    };
    let lines = help.lines();
    let visible: Vec<Line> = lines
        .iter()
        .skip(scroll)
        .take(viewport)
        .enumerate()
        .map(|(i, l)| {
            let line_idx = scroll + i;
            let spans: Vec<lattice_syntax::StyledSpan> =
                help.highlights.get(line_idx).cloned().unwrap_or_default();
            Line::from(render_help_line(l, &spans))
        })
        .collect();
    frame.render_widget(Paragraph::new(visible).wrap(Wrap { trim: false }), area);
}

/// Lay the pane tree out across `area` and draw each pane
/// (DESIGN.md §5.9). Each pane renders its actual buffer content
/// (vim-style: no decorative borders) plus a one-row status line
/// at its bottom edge. The active pane's status line is reverse-
/// videoed so focus is unambiguous; inactive status lines are
/// dim. With a single pane we skip the status line so the buffer
/// area looks identical to the pre-split rendering.
fn draw_panes(frame: &mut Frame, area: Rect, app: &App, snap: &DocumentSnapshot) {
    let pane_area = crate::pane::PaneRect {
        x: area.x,
        y: area.y,
        width: area.width,
        height: area.height,
    };
    let rects = app.pane_tree.compute_rects(pane_area);
    let active = app.pane_tree.active_index();
    let multi = rects.len() > 1;
    for (idx, prect) in rects.iter().copied() {
        let rect = Rect {
            x: prect.x,
            y: prect.y,
            width: prect.width,
            height: prect.height,
        };
        // A pane is *active for input* iff it's the focused pane
        // AND the active buffer kind matches the pane's buffer.
        // The mismatch case is the help-popup-overlay scenario:
        // active pane shows a Document, but `app.active_buffer ==
        // Help` because the popup is focused (State B). The doc
        // must paint with its own (frozen) `pane.cursor`, not
        // `app.cursor` (which is help's). draw_inactive_document
        // already reads pane state, so we route there.
        let is_active = idx == active && pane_buffer_matches_active(app, idx);
        // Reserve the bottom row for the per-pane status line, but
        // only when there's more than one pane visible.
        let (content_rect, status_rect) = if multi && rect.height >= 2 {
            let content_h = rect.height - 1;
            (
                Rect {
                    x: rect.x,
                    y: rect.y,
                    width: rect.width,
                    height: content_h,
                },
                Some(Rect {
                    x: rect.x,
                    y: rect.y + content_h,
                    width: rect.width,
                    height: 1,
                }),
            )
        } else {
            (rect, None)
        };
        let panes = app.pane_tree.leaves();
        let Some(pane) = panes.get(idx) else {
            continue;
        };
        let pane = *pane;
        match pane.buffer {
            crate::buffers::BufferKind::Document => {
                if is_active {
                    draw_buffer(frame, content_rect, app, snap);
                } else {
                    draw_inactive_document(frame, content_rect, app, &pane, idx);
                }
            }
            crate::buffers::BufferKind::Help => {
                // Help-as-buffer (DESIGN.md §5.9): when help is the
                // active buffer it fills the pane area, just like
                // a document. The centred popup overlay is reserved
                // for the *transient* hover state where help_buffer
                // is set but active is another kind. Doing both
                // (popup + draw the doc behind it) would mean help
                // motions visibly scroll the doc backdrop, which
                // breaks the "help is just a buffer" model the
                // user expects.
                if is_active {
                    draw_help_in_pane(frame, content_rect, app);
                } else {
                    draw_inactive_help(frame, content_rect, app, &pane);
                }
            }
            crate::buffers::BufferKind::FileTree => {
                draw_file_tree_pane(frame, content_rect, app, &pane, is_active);
            }
        }
        if let Some(sr) = status_rect {
            draw_pane_status_line(frame, sr, app, &pane, is_active);
        }
    }
    // Draw vertical separators in the column gaps between
    // side-by-side panes. The separator overlays the boundary
    // column of the right-side pane; horizontal splits don't get
    // an explicit separator -- the per-pane status line at the
    // bottom of the upper pane already provides one.
    if multi {
        draw_pane_separators(frame, &rects, app);
    }
}

/// Walk the pane rects and draw a vertical separator wherever two
/// rects share a vertical seam (same y range, A's right edge ==
/// B's left edge). Uses [`Theme::pane_separator_vertical`] for the
/// glyph and [`Theme::pane_separator`] for the style.
fn draw_pane_separators(frame: &mut Frame, rects: &[(usize, crate::pane::PaneRect)], app: &App) {
    let glyph = app.theme.pane_separator_vertical;
    let style = app.theme.pane_separator;
    for (i, (_, a)) in rects.iter().enumerate() {
        for (_, b) in rects.iter().skip(i + 1) {
            let same_band = a.y == b.y && a.height == b.height;
            let adjacent = a.x + a.width == b.x;
            if same_band && adjacent {
                let col = a.x + a.width - 1;
                for row in a.y..a.y + a.height {
                    let r = Rect {
                        x: col,
                        y: row,
                        width: 1,
                        height: 1,
                    };
                    let para = Paragraph::new(Line::from(Span::styled(glyph.to_string(), style)));
                    frame.render_widget(para, r);
                }
            }
        }
    }
}

/// One-row status line at the bottom of a pane (vim's "statusline"
/// per-window). Active pane is reverse-videoed; inactive panes are
/// dim. Format: `path  line:col  [+]` (path, position, dirty
/// marker). Help and file-tree get their own labels.
fn draw_pane_status_line(
    frame: &mut Frame,
    area: Rect,
    app: &App,
    pane: &crate::pane::PaneState,
    is_active: bool,
) {
    let label = match pane.buffer {
        crate::buffers::BufferKind::Document => app
            .buffers
            .document(pane.buffer_id)
            .map(|e| {
                let path = e
                    .handle
                    .path()
                    .map(|p| p.display().to_string())
                    .unwrap_or_else(|| "[no name]".to_string());
                let dirty = if e.handle.dirty() { " [+]" } else { "" };
                format!("{path}{dirty}")
            })
            .unwrap_or_else(|| "[no buffer]".to_string()),
        crate::buffers::BufferKind::Help => app
            .help_buffer
            .as_ref()
            .map(|h| format!("[help] {}", h.title))
            .unwrap_or_else(|| "[help]".to_string()),
        crate::buffers::BufferKind::FileTree => app
            .buffers
            .file_tree(pane.buffer_id)
            .map(|t| format!("[tree] {}", t.root.display()))
            .unwrap_or_else(|| "[tree]".to_string()),
    };
    let pos = format!("{}:{}", pane.cursor.line + 1, pane.cursor.byte);
    let style = if is_active {
        app.theme.pane_status_active
    } else {
        app.theme.pane_status_inactive
    };
    // Compose: " label                pos "
    let width = area.width as usize;
    let total_text_len = label.chars().count() + pos.chars().count() + 3; // 1 lead + 2 sep
    let pad = if width > total_text_len {
        width - total_text_len
    } else {
        1
    };
    let line_text = format!(" {label}{}{pos} ", " ".repeat(pad));
    let truncated: String = line_text.chars().take(width).collect();
    let para = Paragraph::new(Line::from(Span::styled(truncated, style)));
    frame.render_widget(para, area);
}

/// Render a Document pane that isn't currently focused. Reads the
/// stashed cursor / scroll from `pane`, looks up the document by
/// `pane.buffer_id`, and renders gutter + visible lines with the
/// same syntax-highlight pipeline as the active pane. Inactive
/// highlights are sourced from [`App::pane_highlights`] (keyed by
/// pane index) when the doc differs from the active pane's, or
/// from [`App::visible_highlights`] when the docs match -- a
/// single parse covers both panes. The theme's
/// `inactive_pane_overlay` modifier (default: DIM) layers on top
/// of every span so focus stays unambiguous without losing color.
fn draw_inactive_document(
    frame: &mut Frame,
    area: Rect,
    app: &App,
    pane: &crate::pane::PaneState,
    pane_idx: usize,
) {
    let Some(entry) = app.buffers.document(pane.buffer_id) else {
        return;
    };
    let snap = entry.handle.snapshot();
    let total_lines = snap.buffer.line_count();
    let gutter_w = if app.show_line_numbers() {
        gutter_width(total_lines)
    } else {
        2
    };
    // Reserve the diagnostic-severity column on inactive panes
    // too so the gutter alignment matches the active pane when
    // they share a document.
    let buffer_w = (area.width as u32)
        .saturating_sub(gutter_w)
        .saturating_sub(DIAG_GUTTER_WIDTH);

    // Source for inactive-pane highlights:
    //  1. `pane_highlights[idx]` when the pane has a different
    //     document than the active pane (refreshed by
    //     `refresh_pane_highlights`).
    //  2. `visible_highlights` when the panes share a document
    //     AND the inactive pane's scroll matches the active's
    //     (avoids a redundant parse).
    //  3. Empty otherwise -- plain text, no syntax. Acceptable
    //     for the rare same-doc-different-scroll case.
    let active_doc_id = if matches!(app.active_buffer, crate::buffers::BufferKind::Document) {
        Some(app.document_buffer_id)
    } else {
        None
    };
    let highlights: Vec<Vec<lattice_syntax::StyledSpan>> =
        if let Some(spans) = app.pane_highlights.get(&pane_idx) {
            spans.clone()
        } else if active_doc_id == Some(pane.buffer_id) && pane.scroll == app.scroll {
            app.visible_highlights.clone()
        } else {
            Vec::new()
        };

    let dim_overlay = if app.theme.dim_inactive_panes {
        Some(app.theme.inactive_pane_overlay)
    } else {
        None
    };

    let mut lines: Vec<Line> = Vec::with_capacity(area.height as usize);
    for i in 0..area.height as u32 {
        let buf_line = pane.scroll + i;
        if buf_line >= total_lines {
            lines.push(empty_marker_line(gutter_w));
            continue;
        }
        let line_text = snap.buffer.line(buf_line).unwrap_or_default();
        let gutter = render_gutter_for_inactive(app, pane.cursor.line, buf_line, gutter_w);
        let spans = highlights.get(i as usize).map(Vec::as_slice).unwrap_or(&[]);
        let mut body = render_styled_line(&line_text, spans, buffer_w);
        if let Some(overlay) = dim_overlay {
            for span in body.iter_mut() {
                span.style = span.style.patch(overlay);
            }
        }
        // Inactive panes get a blank severity cell so the
        // alignment matches the active pane when they share a
        // document. Diagnostics on inactive panes are
        // intentionally minimal -- the active pane is the
        // canonical surface; inactive ones avoid visual noise.
        lines.push(combine_prefixed(
            vec![Span::styled(" ".to_string(), TuiStyle::default())],
            gutter,
            body,
        ));
    }
    frame.render_widget(Paragraph::new(lines), area);
}

/// Gutter render for an inactive pane. Uses the pane's stashed
/// cursor line for relative-numbering -- the active pane uses
/// `app.cursor.line` instead.
fn render_gutter_for_inactive(
    app: &App,
    cursor_line: u32,
    line_idx: u32,
    gutter_w: u32,
) -> Span<'static> {
    // Inactive panes don't carry their own fold state today (folds
    // live on the active App), so we format an empty glyph slot --
    // but use the same shared layout helper so column alignment
    // matches the active pane.
    if !app.show_line_numbers() {
        return Span::styled(
            format_gutter_cell("", gutter_w, None),
            TuiStyle::default().fg(Color::DarkGray),
        );
    }
    let n = if !app.relative_line_numbers() || line_idx == cursor_line {
        (line_idx + 1).to_string()
    } else {
        line_idx.abs_diff(cursor_line).to_string()
    };
    Span::styled(
        format_gutter_cell(&n, gutter_w, None),
        TuiStyle::default().fg(Color::DarkGray),
    )
}

/// Render a file-tree pane vim-style: no decorative border, just
/// the entries listed plain in the pane's content area with the
/// cursor row reverse-videoed when the pane is focused. Status
/// information (root path) lives in the per-pane status line, so
/// the content area is purely the tree text -- consistent with
/// how a Document pane looks.
fn draw_file_tree_pane(
    frame: &mut Frame,
    area: Rect,
    app: &App,
    pane: &crate::pane::PaneState,
    is_active: bool,
) {
    let Some(tree) = app.buffers.file_tree(pane.buffer_id) else {
        return;
    };
    // Active pane's live cursor / scroll live on `app.cursor` /
    // `app.scroll` (unified across buffer kinds). Inactive panes
    // use the pane's stashed cursor / scroll; the tree's own
    // `cursor` / `scroll` fields are archival save-state.
    let (cursor_line, scroll) = if is_active {
        (app.cursor.line as usize, app.scroll as usize)
    } else {
        (pane.cursor.line as usize, pane.scroll as usize)
    };
    let viewport = area.height as usize;
    let nerd_fonts = app.theme.nerd_fonts;
    let theme = &app.theme;
    let raw_text = tree.content.as_string();
    let lines: Vec<Line> = raw_text
        .split('\n')
        .enumerate()
        .zip(tree.entries.iter())
        .skip(scroll)
        .take(viewport)
        .map(|((i, raw_line), entry)| {
            let line_idx = scroll + i;
            let is_cursor = is_active && line_idx == cursor_line;
            let is_dir =
                matches!(entry.kind, crate::file_tree::FileTreeEntryKind::Directory { .. });
            let (_glyph, entry_style) =
                crate::icons::icon_for_entry(&entry.path, is_dir, nerd_fonts, theme);
            let cursor_mod = if is_cursor {
                Modifier::REVERSED
            } else {
                Modifier::empty()
            };
            let span_style = entry_style.add_modifier(cursor_mod);
            Line::from(Span::styled(raw_line.to_string(), span_style))
        })
        .collect();
    frame.render_widget(Paragraph::new(lines), area);
    if is_active && area.height > 0 && area.width > 0 {
        let row_off = (app.cursor.line as usize).saturating_sub(app.scroll as usize);
        let row_off = row_off.min(area.height.saturating_sub(1) as usize);
        let col_off = (app.cursor.byte as usize).min(area.width.saturating_sub(1) as usize);
        frame.set_cursor_position((area.x + col_off as u16, area.y + row_off as u16));
    }
}

fn draw_buffer(frame: &mut Frame, area: Rect, app: &App, snap: &DocumentSnapshot) {
    let lines = compose_visible_lines(app, snap, area.height as u32, area.width as u32);
    frame.render_widget(Paragraph::new(lines), area);

    // Place the buffer-area cursor only when the prompt isn't claiming it.
    // In Command (`:`) and Search (`/`, `?`) modal states the cursor lives
    // in the bottom prompt row -- handled by `draw_command_or_echo`.
    let prompt_owns_cursor = matches!(app.modal, ModalState::Command | ModalState::Search(_));
    if !prompt_owns_cursor
        && let Some((screen_x, screen_y)) = cursor_screen_position(app, snap, area)
    {
        frame.set_cursor_position((screen_x, screen_y));
    }
}

fn draw_command_or_echo(frame: &mut Frame, area: Rect, app: &App) {
    if matches!(app.modal, ModalState::Command) {
        // ":<typed>" with the cursor sitting at the end of the typed text.
        let prompt = format!(":{}", app.command_line);
        let cursor_col = area
            .x
            .saturating_add(prompt.len().min(area.width as usize) as u16);

        // Visual hints. Two non-mutually-exclusive layers show
        // after the cursor in a dim style:
        //   1. `auto_submit_after_chord` (missing-arg prompt
        //      armed by `:describe-key<CR>`): show a clear
        //      "press a chord" cue so the user knows the next
        //      keypress runs the lookup.
        //   2. Otherwise, if chord-capture is just active
        //      (cursor in a `Chord` arg slot), show a softer
        //      `(chord)` tag so the user knows the cmdline is
        //      consuming raw key events as chord tokens.
        let hint: Option<&'static str> = if app.auto_submit_after_chord {
            Some("press a chord")
        } else if app.chord_capture_active() {
            Some("(chord)")
        } else {
            None
        };

        let mut spans: Vec<Span<'_>> = vec![Span::raw(prompt)];
        if let Some(text) = hint {
            spans.push(Span::styled(
                text,
                TuiStyle::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            ));
        }
        // Vertico-style count hint when the completion popup is
        // open: `(selected/total)` faintly trailing the cmdline.
        // Mirrors the picker prompt's `(n/m)` so both surfaces
        // read the same.
        if let Some(state) = app.completion_state.as_ref()
            && !state.candidates.is_empty()
        {
            spans.push(Span::styled(
                format!("  ({}/{})", state.selected + 1, state.candidates.len()),
                TuiStyle::default().fg(Color::DarkGray),
            ));
        }
        let para = Paragraph::new(Line::from(spans));
        frame.render_widget(para, area);
        frame.set_cursor_position((cursor_col, area.y));
        return;
    }

    if let ModalState::Search(direction) = app.modal {
        let lead = match direction {
            SearchDirection::Forward => '/',
            SearchDirection::Backward => '?',
        };
        let pattern = app
            .search_line
            .as_ref()
            .map(|s| s.pattern.as_str())
            .unwrap_or("");
        let prompt = format!("{lead}{pattern}");
        let para = Paragraph::new(Line::from(prompt.clone()));
        frame.render_widget(para, area);
        let col = area
            .x
            .saturating_add(prompt.len().min(area.width as usize) as u16);
        frame.set_cursor_position((col, area.y));
        return;
    }

    let Some(msg) = &app.last_message else {
        // Nothing to show -- render nothing (the row stays blank).
        return;
    };
    let style = match msg.level {
        EchoLevel::Info => TuiStyle::default(),
        EchoLevel::Warn => TuiStyle::default().fg(Color::Yellow),
        EchoLevel::Error => TuiStyle::default()
            .fg(Color::Red)
            .add_modifier(Modifier::BOLD),
    };
    let para = Paragraph::new(Line::from(vec![Span::styled(msg.text.clone(), style)]));
    frame.render_widget(para, area);
}

/// Modeline segment listing the LSP servers attached to the active
/// document buffer. Empty string when no servers are attached, when
/// the active buffer has no URI yet, or when the supervisor mutex
/// is contended (the modeline never blocks the render thread -- the
/// next frame picks the indicator up). Multiple servers are joined
/// with `+` (`[lsp:rust+typos]`); the §5.4 multi-server merge model
/// means more than one is legitimate.
fn active_lsp_segment(app: &App) -> String {
    let Some(uri) = app.buffer_uris.get(&app.document_buffer_id) else {
        return String::new();
    };
    let Ok(sup) = app.lsp.try_lock() else {
        return String::new();
    };
    let handles = sup.servers_for(uri);
    if handles.is_empty() {
        return String::new();
    }
    let ids: Vec<&str> = handles.iter().map(|h| h.server_id()).collect();
    format!("[lsp:{}]", ids.join("+"))
}

fn draw_mode_line(frame: &mut Frame, area: Rect, app: &App, snap: &DocumentSnapshot) {
    // §5.6.8: the renderer reads through a single arc-swap
    // `Cache::load` per frame (loaded by the runtime) and reuses
    // that snapshot for the entire frame -- never round-trips the
    // actor.
    let path = snap
        .path()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "[no name]".to_string());
    let dirty = if snap.dirty { "[+]" } else { "   " };
    let pos = format!("{}:{}", app.cursor.line + 1, app.cursor.byte);
    let lang = Lang::detect_from_path(snap.path()).label();
    let mode_label = app.modal_label();
    let lsp_segment = active_lsp_segment(app);

    let left = format!("[{mode_label}] {dirty} {path}");
    let right = if lsp_segment.is_empty() {
        format!("{pos}  {lang}")
    } else {
        format!("{pos}  {lang}  {lsp_segment}")
    };

    let total = (area.width as usize).max(left.len() + right.len() + 1);
    let pad = total - left.len() - right.len();
    let line = format!("{left}{:pad$}{right}", "", pad = pad);

    let para = Paragraph::new(Line::from(vec![Span::styled(
        line,
        TuiStyle::default()
            .fg(Color::Black)
            .bg(Color::White)
            .add_modifier(Modifier::BOLD),
    )]));
    frame.render_widget(para, area);
}

/// Produce the visible buffer lines as `ratatui::text::Line`s, including
/// gutter (line numbers), tab expansion, and styled spans pulled from
/// `app.visible_highlights` (populated by the runtime via
/// `App::refresh_highlights`).
///
/// Spans are owned (`Cow::Owned`) so the returned `Line`s outlive the
/// document text we slice out of for this frame. One alloc per visible line
/// per frame -- negligible at terminal sizes (typically 50-100 lines).
pub fn compose_visible_lines(
    app: &App,
    snap: &DocumentSnapshot,
    height: u32,
    width: u32,
) -> Vec<Line<'static>> {
    // §5.6.8 contract: one snapshot per frame, used for everything.
    // The snapshot was loaded by the runtime via
    // `app.snapshot_cache.load_arc()` and threaded through.
    // §8.2 hot path: never materialise the whole buffer -- iterate
    // ropey's line API and pull only the visible window. A 100MB
    // log file should cost the same per-frame as a 100-line file.
    let total_lines = snap.buffer.line_count();
    let gutter_w = if app.show_line_numbers() {
        gutter_width(total_lines)
    } else {
        // Keep one cell of left padding for the empty-marker `~` line
        // and to mirror vim's `:set nonumber` (no gutter, but content
        // still has a one-cell margin from the edge).
        2
    };
    // Severity column is prepended (Phase 4.1.d.iii); reserve
    // one cell so buffer width stays correct.
    let buffer_w = width
        .saturating_sub(gutter_w)
        .saturating_sub(DIAG_GUTTER_WIDTH);

    // Compute visual selection range once (instead of per line).
    let visual_range = visual_selection_range(app);
    let block = visual_block_extents(app);

    // Build the visible-buffer-line ordering: starting from `scroll`,
    // skip lines inside closed folds, taking up to `height` entries.
    // Bound the walk by `total_lines` from ropey -- O(1).
    let mut visible: Vec<u32> = Vec::with_capacity(height as usize);
    let mut buf_line = app.scroll;
    while visible.len() < height as usize && buf_line < total_lines {
        if app.line_inside_closed_fold(buf_line) {
            buf_line += 1;
            continue;
        }
        visible.push(buf_line);
        // If this is a fold start, jump past the fold's interior in the
        // next iteration (the interior is hidden).
        if let Some(fold) = app.fold_start_at(buf_line) {
            buf_line = fold.end_line + 1;
        } else {
            buf_line += 1;
        }
    }

    let mut out = Vec::with_capacity(height as usize);
    for i in 0..height {
        let line_idx = match visible.get(i as usize) {
            Some(&l) => l,
            None => {
                out.push(empty_marker_line(gutter_w));
                continue;
            }
        };
        // Pull just this line's text (O(log n) lookup +
        // O(line_len) materialisation).
        let line_text = snap.buffer.line(line_idx).unwrap_or_default();
        let gutter = render_gutter_for(app, line_idx, gutter_w);
        // Highlight slot is keyed by buffer-line offset from
        // `scroll`, NOT by viewport row -- once closed folds skip
        // interior lines, viewport row `i` no longer corresponds
        // to buffer line `scroll + i`, and using the row index
        // would paint a post-fold line with stale spans for the
        // hidden interior.
        let spans = app.highlights_for_buffer_line(line_idx);
        let mut body = render_styled_line(&line_text, spans, buffer_w);
        let line_len = line_text.len();
        // Whether this line begins a closed fold. Used to append the
        // ` ┄ N lines folded` suffix AFTER overlay processing, so
        // visual selection / hlsearch / current_match still paint
        // the heading correctly.
        let closed_fold_at_start = app
            .fold_start_at(line_idx)
            .filter(|f| f.closed)
            .map(|f| {
                // The "N lines folded" suffix should reflect the
                // user's perception of how much content collapsed
                // onto this single visible row -- including any
                // sibling / nested closed folds whose headings are
                // themselves hidden by this fold and whose ranges
                // chain past `f.end_line`. Without this walk, two
                // touching folds (1..=3 then 3..=5, both closed)
                // visually hide 5 lines but report only the first
                // fold's own 3 lines, which doesn't match what the
                // user just collapsed.
                closed_fold_display_span(app, snap, f)
            });
        // Blockwise visual: per-line column band [min_col, max_col].
        // Charwise / Linewise visual go through `visual_range` instead.
        if let Some(b) = block
            && line_idx >= b.start_line
            && line_idx <= b.end_line
        {
            let start = (b.start_col as usize).min(line_len);
            let end = ((b.end_col as usize) + 1).min(line_len);
            if start < end {
                body = apply_match_overlay(body, start, end, visual_style());
            }
        } else if let Some(range) = visual_range
            && let Some((overlay_start, overlay_end)) =
                match_overlay_range(range, line_idx, line_len)
        {
            body = apply_match_overlay(body, overlay_start, overlay_end, visual_style());
        }
        // Hlsearch overlay: every other occurrence of the search pattern,
        // softer than the current_match style.
        for &range in app.all_matches.iter() {
            if let Some((overlay_start, overlay_end)) =
                match_overlay_range(range, line_idx, line_len)
            {
                body = apply_match_overlay(body, overlay_start, overlay_end, hlsearch_style());
            }
        }
        if let Some(range) = app.current_match
            && let Some((overlay_start, overlay_end)) =
                match_overlay_range(range, line_idx, line_len)
        {
            body = apply_match_overlay(body, overlay_start, overlay_end, match_style());
        }
        // LSP diagnostic underline overlay (Phase 4.1.d.iii):
        // for each diagnostic touching this line, underline the
        // affected range with the severity colour. Underline
        // modifier composes with any prior bg / fg overlays
        // (visual / hlsearch / current_match) -- all four can
        // co-exist on a single span without conflict.
        for d in diagnostics_on_line(app, snap, line_idx) {
            let start = if d.range.start.line == line_idx {
                (d.range.start.character as usize).min(line_len)
            } else {
                0
            };
            let end = if d.range.end.line == line_idx {
                (d.range.end.character as usize).min(line_len)
            } else {
                line_len
            };
            if start >= end {
                continue;
            }
            let color = match d.severity {
                Some(DiagnosticSeverity::ERROR) => Color::Red,
                Some(DiagnosticSeverity::WARNING) => Color::Yellow,
                Some(DiagnosticSeverity::INFORMATION) => Color::Blue,
                Some(DiagnosticSeverity::HINT) => Color::DarkGray,
                _ => Color::Blue,
            };
            body = apply_underline_overlay(body, start, end, color);
        }
        // Substitute live preview overlay (DESIGN.md §5.9.10): paint
        // the about-to-be-replaced ranges in a strike-through-ish
        // style so the user sees what will change before they hit
        // Enter. Distinct from hlsearch's plain match highlight.
        if let Some(preview) = app.substitute_preview.as_ref() {
            for &range in preview.matches.iter() {
                if let Some((overlay_start, overlay_end)) =
                    match_overlay_range(range, line_idx, line_len)
                {
                    body = apply_match_overlay(
                        body,
                        overlay_start,
                        overlay_end,
                        substitute_preview_style(),
                    );
                }
            }
        }
        // Heading-preserved fold render (`docs/help/folding.md`):
        // append the ` ┄ N lines folded` suffix AFTER all overlays
        // so the heading's syntax / visual / search styling is
        // preserved, with the dim summary trailing off the right.
        if let Some(n) = closed_fold_at_start {
            body.push(Span::styled(
                format!(" ┄ {n} lines folded"),
                TuiStyle::default().fg(Color::DarkGray),
            ));
        }
        // Ghost text (Phase 4.2.g.7 polish). When the cursor
        // sits at end-of-line on this row AND the popup's
        // top-ranked candidate has a suffix to preview, paint
        // it as a dimmed inline overlay so the user sees the
        // most-likely accept inline. Cursor block visually
        // overlays the first ghost char (the typed prefix
        // ends right before it).
        if line_idx == app.cursor.line
            && (app.cursor.byte as usize) == line_text.len()
            && let Some(suffix) = app.completion_ghost_text_suffix()
        {
            body.push(Span::styled(
                suffix,
                TuiStyle::default()
                    .fg(Color::DarkGray)
                    .add_modifier(Modifier::ITALIC),
            ));
        }
        // LSP severity cell (Phase 4.1.d.iii). One cell pre-
        // pended to the gutter; severity glyph + colour when a
        // diagnostic touches the line, blank otherwise. Costs
        // one cell of gutter width on every frame -- visible
        // even when no diagnostics exist so the layout doesn't
        // shift when one arrives.
        let severity_cell = render_diagnostic_severity_cell(app, snap, line_idx);
        out.push(combine_prefixed(vec![severity_cell], gutter, body));
    }
    out
}

fn hlsearch_style() -> TuiStyle {
    // Softer than the primary match (which is yellow-bg). Cyan-bg reads
    // as "another instance of what you're searching for" without
    // stealing attention from the cursor's match.
    TuiStyle::default().bg(Color::Cyan).fg(Color::Black)
}

/// Style for substitute live-preview matches. Magenta-bg with a
/// strike-through reads as "this is going to be replaced if you
/// hit Enter" -- distinct from hlsearch's "this is what your
/// search is finding" cyan, and distinct from the current-match
/// yellow.
fn substitute_preview_style() -> TuiStyle {
    TuiStyle::default()
        .bg(Color::Magenta)
        .fg(Color::Black)
        .add_modifier(Modifier::CROSSED_OUT)
}

/// For blockwise Visual: the rectangle defined by the selection's
/// `(anchor, head)` positions. Returns `None` if not in blockwise mode.
fn visual_block_extents(app: &App) -> Option<BlockExtents> {
    if !matches!(
        app.modal,
        ModalState::Visual(lattice_grammar::VisualKind::Blockwise)
    ) {
        return None;
    }
    let sels = app.document.selections();
    let sel = sels.primary();
    let start_line = sel.anchor.line.min(sel.head.line);
    let end_line = sel.anchor.line.max(sel.head.line);
    let start_col = sel.anchor.byte.min(sel.head.byte);
    let end_col = sel.anchor.byte.max(sel.head.byte);
    Some(BlockExtents {
        start_line,
        end_line,
        start_col,
        end_col,
    })
}

#[derive(Debug, Clone, Copy)]
struct BlockExtents {
    start_line: u32,
    end_line: u32,
    start_col: u32,
    end_col: u32,
}

/// Compute the rendered range of the visual selection. Returns `None` if
/// not in Visual mode. For Linewise visual the byte extents on the first
/// and last lines are normalized to cover the full lines (mirrored from
/// the dispatcher's `Range::Selection` resolution).
fn visual_selection_range(app: &App) -> Option<ProtoRange> {
    if !matches!(app.modal, ModalState::Visual(_)) {
        return None;
    }
    let sels = app.document.selections();
    let sel = sels.primary();
    let (a, b) = if sel.anchor <= sel.head {
        (sel.anchor, sel.head)
    } else {
        (sel.head, sel.anchor)
    };
    match sel.visual {
        Some(VisualMode::Linewise) => {
            // Cover full lines from a.line to b.line. Use a large byte
            // index for `end.byte`; match_overlay_range clamps to line_len.
            Some(ProtoRange::new(
                lattice_protocol::position::Position::new(a.line, 0),
                lattice_protocol::position::Position::new(b.line, u32::MAX),
            ))
        }
        Some(VisualMode::Charwise) | None => {
            // Charwise: include the head byte (vim semantics).
            Some(ProtoRange::new(
                a,
                lattice_protocol::position::Position::new(b.line, b.byte.saturating_add(1)),
            ))
        }
        Some(VisualMode::Blockwise) => {
            // Stub: render as charwise for v1.
            Some(ProtoRange::new(
                a,
                lattice_protocol::position::Position::new(b.line, b.byte.saturating_add(1)),
            ))
        }
    }
}

fn visual_style() -> TuiStyle {
    // Distinct from the search-match style. Reverse video reads as
    // "selected" in vim's terminal default.
    TuiStyle::default()
        .bg(Color::Blue)
        .fg(Color::White)
        .add_modifier(Modifier::BOLD)
}

/// If `range` covers any bytes on `line_idx`, return the within-line
/// half-open byte interval `[start, end)`. `line_len` is the line's
/// content length excluding the trailing newline.
fn match_overlay_range(
    range: ProtoRange,
    line_idx: u32,
    line_len: usize,
) -> Option<(usize, usize)> {
    if line_idx < range.start.line || line_idx > range.end.line {
        return None;
    }
    let start = if line_idx == range.start.line {
        range.start.byte as usize
    } else {
        0
    };
    let end = if line_idx == range.end.line {
        range.end.byte as usize
    } else {
        line_len
    };
    if start >= end || start >= line_len {
        return None;
    }
    Some((start, end.min(line_len)))
}

fn apply_match_overlay(
    spans: Vec<Span<'static>>,
    overlay_start: usize,
    overlay_end: usize,
    overlay_style: TuiStyle,
) -> Vec<Span<'static>> {
    let mut out: Vec<Span<'static>> = Vec::with_capacity(spans.len() + 2);
    let mut cursor = 0usize;
    for span in spans {
        let s = span.content.as_ref().to_string();
        let span_start = cursor;
        let span_end = cursor + s.len();
        let overlap_start = span_start.max(overlay_start);
        let overlap_end = span_end.min(overlay_end);
        if overlap_start >= overlap_end {
            out.push(Span::styled(s, span.style));
        } else {
            if overlap_start > span_start {
                let pre = s[..overlap_start - span_start].to_string();
                out.push(Span::styled(pre, span.style));
            }
            let mid = s[overlap_start - span_start..overlap_end - span_start].to_string();
            out.push(Span::styled(mid, overlay_style));
            if overlap_end < span_end {
                let post = s[overlap_end - span_start..].to_string();
                out.push(Span::styled(post, span.style));
            }
        }
        cursor = span_end;
    }
    out
}

fn match_style() -> TuiStyle {
    TuiStyle::default()
        .bg(Color::Yellow)
        .fg(Color::Black)
        .add_modifier(Modifier::BOLD)
}

/// Trailing-side padding cells between the gutter's content and the
/// buffer column. The fold glyph still occupies one of those cells
/// (right next to the line number); the remaining cells are plain
/// space so the digits don't run flush against code. Two cells
/// total reads as visible breathing room without stealing more
/// buffer width than necessary.
const GUTTER_TRAILING_PAD: u32 = 2;

fn gutter_width(line_count: u32) -> u32 {
    // Layout: 1 cell leading pad + N digits + GUTTER_TRAILING_PAD
    // (which includes the fold-glyph slot). For line_count = 99 and
    // pad = 2 that's "_99_ " => 5 cells.
    let digits = line_count.max(1).ilog10() + 1;
    digits + 1 + GUTTER_TRAILING_PAD
}

/// Pick the gutter fold glyph for a buffer line: ▸ when the line
/// begins a closed fold, ▾ when it begins an open fold, or `None`
/// when the line is unaffiliated with any fold start.
/// (`docs/help/folding.md`).
fn fold_glyph_for(app: &App, line_idx: u32) -> Option<char> {
    let f = app.fold_start_at_any(line_idx)?;
    Some(if f.closed { '▸' } else { '▾' })
}

/// Format the gutter cell text for a numbered line.
/// Layout: `[leading_pad][label][separator][glyph_or_space]`.
/// The separator is one plain space sitting between the line
/// number and the rightmost cell so digits don't run flush against
/// the fold glyph; the glyph (or a plain space when no fold starts
/// on this line) occupies the rightmost cell, immediately
/// adjacent to the buffer column. This mirrors vim's
/// `signcolumn`-on-the-right convention -- e.g. ` 99 ▸` for a
/// closed fold's heading.
fn format_gutter_cell(label: &str, width: u32, glyph: Option<char>) -> String {
    // Rightmost cell is the glyph; one separator space sits before
    // the label. Leading pad fills the rest.
    let leading = (width as usize).saturating_sub(label.len() + 2);
    let g = glyph.unwrap_or(' ');
    format!("{:lead$}{label} {g}", "", lead = leading)
}

fn render_gutter(line_idx: u32, width: u32, glyph: Option<char>) -> Span<'static> {
    let n = (line_idx + 1).to_string();
    Span::styled(
        format_gutter_cell(&n, width, glyph),
        TuiStyle::default().fg(Color::DarkGray),
    )
}

fn render_gutter_for(app: &App, line_idx: u32, width: u32) -> Span<'static> {
    let glyph = fold_glyph_for(app, line_idx);
    if !app.show_line_numbers() {
        // No-numbers gutter: glyph (or empty) at the inner edge,
        // GUTTER_TRAILING_PAD - 1 trailing spaces, the rest leading
        // padding. The layout still aligns with the numbered case
        // so toggling `:set number` doesn't shift content.
        let label = "";
        return Span::styled(
            format_gutter_cell(label, width, glyph),
            TuiStyle::default().fg(Color::DarkGray),
        );
    }
    if !app.relative_line_numbers() || line_idx == app.cursor.line {
        return render_gutter(line_idx, width, glyph);
    }
    let dist = line_idx.abs_diff(app.cursor.line);
    let n = dist.to_string();
    Span::styled(
        format_gutter_cell(&n, width, glyph),
        TuiStyle::default().fg(Color::DarkGray),
    )
}

/// Width of the diagnostic-severity column prepended to the
/// gutter (Phase 4.1.d.iii). Always 1 cell when LSP is in use --
/// matches vim's `signcolumn=yes`. Costs one cell of gutter
/// width but keeps the layout stable when diagnostics
/// arrive / clear.
const DIAG_GUTTER_WIDTH: u32 = 1;

/// Build the severity-column cell for `line_idx`. Returns one
/// `Span` -- the severity glyph + the per-severity style when a
/// diagnostic touches the line, or a single space styled
/// dim-darkgray when nothing's there.
fn render_diagnostic_severity_cell(
    app: &App,
    snap: &DocumentSnapshot,
    line_idx: u32,
) -> Span<'static> {
    let theme = &app.theme;
    let blank = Span::styled(" ".to_string(), TuiStyle::default());
    let Some(severity) = severity_for_line(app, snap, line_idx) else {
        return blank;
    };
    let (glyph, style) = crate::theme::diagnostic_glyph_and_style(theme, severity);
    // The theme stores ratatui-native Style values, so no
    // conversion is needed here -- they're already the right
    // shape for `Span::styled`.
    Span::styled(glyph.to_string(), style)
}

/// Resolve the most-severe diagnostic on `line_idx` of the
/// active buffer. Walks `app.lsp_diagnostics` keyed by the
/// active URI (looked up via `app.buffer_uri`). Returns `None`
/// when:
/// - the active buffer has no URI (unsaved scratch), or
/// - the buffer has no LSP attachment, or
/// - no diagnostic touches the line.
pub(crate) fn severity_for_line(
    app: &App,
    _snap: &DocumentSnapshot,
    line_idx: u32,
) -> Option<DiagnosticSeverity> {
    let uri = app.buffer_uri(app.document_buffer_id)?;
    app.lsp_diagnostics.line_severity(uri, line_idx)
}

/// Diagnostics that overlap `line_idx` of the active buffer.
/// Used by the inline-underline overlay.
pub(crate) fn diagnostics_on_line(
    app: &App,
    _snap: &DocumentSnapshot,
    line_idx: u32,
) -> Vec<LspDiagnostic> {
    let Some(uri) = app.buffer_uri(app.document_buffer_id) else {
        return Vec::new();
    };
    app.lsp_diagnostics.diagnostics_on_line(uri, line_idx)
}


fn render_styled_line(line: &str, spans: &[StyledSpan], max_width: u32) -> Vec<Span<'static>> {
    let mut out: Vec<Span<'static>> = Vec::new();
    let mut cursor = 0usize;
    let bytes = line.as_bytes();
    // Spans from tree-sitter come in event order; re-sort by start byte so
    // the renderer's "no overlap" assumption holds. Also drop spans that
    // overlap a previous one (the highlighter resolves overlaps already, but
    // belt-and-braces).
    let mut sorted: Vec<StyledSpan> = spans.to_vec();
    sorted.sort_by_key(|s| (s.start, s.end));
    for span in sorted.iter() {
        if span.start < cursor || span.start >= bytes.len() {
            continue;
        }
        if span.start > cursor {
            out.push(Span::raw(line[cursor..span.start].to_string()));
        }
        let end = span.end.min(bytes.len());
        if end <= span.start {
            continue;
        }
        out.push(Span::styled(
            line[span.start..end].to_string(),
            style_to_tui(span.style),
        ));
        cursor = end;
    }
    if cursor < bytes.len() {
        out.push(Span::raw(line[cursor..].to_string()));
    }
    truncate_spans_to_width(out, max_width)
}

fn truncate_spans_to_width(spans: Vec<Span<'static>>, max_width: u32) -> Vec<Span<'static>> {
    // Naive byte-based truncation. Adequate for ASCII; non-ASCII display
    // width is a real problem we punt on until we own a width-aware shaping
    // path (Phase 9 / rich-buffer).
    let mut out = Vec::with_capacity(spans.len());
    let mut budget = max_width as usize;
    for span in spans {
        if budget == 0 {
            break;
        }
        let s = span.content.as_ref().to_string();
        if s.len() <= budget {
            budget -= s.len();
            out.push(Span::styled(s, span.style));
        } else {
            let cut = s
                .char_indices()
                .map(|(i, _)| i)
                .take_while(|i| *i <= budget)
                .last()
                .unwrap_or(0);
            out.push(Span::styled(s[..cut].to_string(), span.style));
            break;
        }
    }
    out
}

fn empty_marker_line(gutter_w: u32) -> Line<'static> {
    // Treat the `~` like a pseudo line-number label so its column
    // alignment matches `render_gutter`'s numbered output: leading
    // pad + `~` + GUTTER_TRAILING_PAD.
    // Prepended one-cell severity column blank (Phase 4.1.d.iii)
    // so the `~` lines align with body lines below the document.
    let cell = format_gutter_cell("~", gutter_w, None);
    Line::from(vec![
        Span::styled(" ".to_string(), TuiStyle::default()),
        Span::styled(cell, TuiStyle::default().fg(Color::DarkGray)),
    ])
}

/// Like [`combine_prefixed`] but accepts a multi-span prefix -- used by
/// the LSP diagnostic gutter where the leading severity cell
/// has its own per-severity style and can't share a span with
/// the line-number gutter (which is always dim-darkgray).
fn combine_prefixed(
    prefix: Vec<Span<'static>>,
    gutter: Span<'static>,
    mut body: Vec<Span<'static>>,
) -> Line<'static> {
    let mut all = Vec::with_capacity(prefix.len() + 1 + body.len());
    all.extend(prefix);
    all.push(gutter);
    all.append(&mut body);
    Line::from(all)
}

/// Apply an underline overlay over a byte range of a line's
/// existing styled spans. Unlike [`apply_match_overlay`], this
/// PRESERVES the underlying span's foreground / background and
/// only ADDs the `UNDERLINED` modifier + sets the underline
/// color. Used for inline LSP diagnostic decoration.
fn apply_underline_overlay(
    spans: Vec<Span<'static>>,
    overlay_start: usize,
    overlay_end: usize,
    underline_color: Color,
) -> Vec<Span<'static>> {
    let mut out: Vec<Span<'static>> = Vec::with_capacity(spans.len() + 2);
    let mut cursor = 0usize;
    for span in spans {
        let s = span.content.as_ref().to_string();
        let span_start = cursor;
        let span_end = cursor + s.len();
        let overlap_start = span_start.max(overlay_start);
        let overlap_end = span_end.min(overlay_end);
        if overlap_start >= overlap_end {
            out.push(Span::styled(s, span.style));
        } else {
            if overlap_start > span_start {
                let pre = s[..overlap_start - span_start].to_string();
                out.push(Span::styled(pre, span.style));
            }
            let mid = s[overlap_start - span_start..overlap_end - span_start].to_string();
            let mid_style = span
                .style
                .add_modifier(Modifier::UNDERLINED)
                .underline_color(underline_color);
            out.push(Span::styled(mid, mid_style));
            if overlap_end < span_end {
                let post = s[overlap_end - span_start..].to_string();
                out.push(Span::styled(post, span.style));
            }
        }
        cursor = span_end;
    }
    out
}

/// Number of buffer lines actually collapsed onto the visible row
/// where `fold` is rendered. Walks forward from `fold.end_line + 1`
/// through any chained closed folds whose ranges abut or sit
/// inside the cumulative hidden region, so the "N lines folded"
/// summary matches what the user just collapsed even when several
/// sibling folds touch (e.g. `(1, 3)` + `(3, 5)` from
/// `foldmethod=indent` on a top-level if/else).
fn closed_fold_display_span(
    app: &App,
    snap: &DocumentSnapshot,
    fold: &crate::app::Fold,
) -> u32 {
    let total_lines = snap.buffer.line_count();
    let mut end = fold.end_line;
    let mut probe = end.saturating_add(1);
    while probe < total_lines {
        // Probe land inside another closed fold's hidden body?
        // (Includes the case where the next fold *starts* at the
        // probe -- start_line is its heading, which would be
        // hidden by *us* extending across it.)
        let next_closed = app.folds.iter().find(|f| {
            f.closed
                && (probe == f.start_line
                    || (probe > f.start_line && probe <= f.end_line))
        });
        match next_closed {
            Some(f) => {
                end = end.max(f.end_line);
                probe = end.saturating_add(1);
            }
            None => break,
        }
    }
    end.saturating_sub(fold.start_line).saturating_add(1)
}

/// Translate a buffer line into the corresponding visible row index
/// in the active pane, accounting for closed folds. Walks the same
/// "skip closed-fold interior" algorithm `compose_visible_lines`
/// uses to build the visible-line list.
///
/// If `target` is hidden by a closed fold, the result is the row
/// where that fold's heading renders -- so the cursor projection
/// always lands on a line the user can see.
///
/// Returns `None` when the resulting row is past `viewport_height`
/// (the cursor is below the visible window) or before scroll.
/// Map a buffer line to the visible row inside a pane viewport,
/// taking closed folds into account. `scroll` is the pane's
/// top-of-viewport buffer line -- usually `app.scroll`, but the
/// popup-anchor path passes the active pane's stashed doc scroll
/// (State B) where the doc isn't the active buffer.
fn buffer_line_to_visible_row_with(
    app: &App,
    snap: &DocumentSnapshot,
    target: u32,
    viewport_height: u32,
    scroll: u32,
) -> Option<u32> {
    if target < scroll {
        return None;
    }
    let total_lines = snap.buffer.line_count();
    let mut buf_line = scroll;
    let mut row: u32 = 0;
    while row < viewport_height && buf_line < total_lines {
        // If a closed fold starts at buf_line, the fold's whole
        // range collapses onto this single visible row. The cursor
        // resolves to this row whether it's at the fold heading or
        // anywhere in the hidden body.
        let fold_at = app.fold_start_at(buf_line);
        let next_buf_line = match fold_at {
            Some(fold) => fold.end_line + 1,
            None => buf_line + 1,
        };
        let covers_target = match fold_at {
            Some(fold) => target >= fold.start_line && target <= fold.end_line,
            None => target == buf_line,
        };
        if covers_target {
            return Some(row);
        }
        if buf_line == target {
            // Defensive: the line wasn't claimed above (no fold,
            // not equal); should be unreachable, but return the
            // current row rather than None so the cursor still
            // shows somewhere sensible.
            return Some(row);
        }
        if app.line_inside_closed_fold(buf_line) {
            // Hidden interior line -- not the start of any fold but
            // still part of one (the renderer skips it). Don't
            // increment row; just advance buf_line.
            buf_line += 1;
            continue;
        }
        buf_line = next_buf_line;
        row += 1;
    }
    None
}

fn cursor_screen_position(
    app: &App,
    snap: &DocumentSnapshot,
    area: Rect,
) -> Option<(u16, u16)> {
    cursor_screen_position_at(app, snap, area, app.cursor, app.scroll)
}

/// Same as [`cursor_screen_position`] but with explicit `cursor`
/// and `scroll`. Used by the help-popup tooltip-anchor path where
/// the document's cursor / scroll live in the active pane's stash
/// (State B), not on `app.cursor` / `app.scroll` (which hold the
/// help buffer's). Folds are document-state and read straight off
/// `app`, which is correct for both states.
fn cursor_screen_position_at(
    app: &App,
    snap: &DocumentSnapshot,
    area: Rect,
    cursor: lattice_protocol::Position,
    scroll: u32,
) -> Option<(u16, u16)> {
    if cursor.line < scroll {
        return None;
    }
    // Map the buffer cursor line to the visible row taking closed
    // folds into account. If the cursor sits inside a closed fold's
    // hidden body, project it onto the fold's heading row -- the
    // user always sees the cursor on a real visible line, never
    // adrift inside collapsed content. This is the safety net for
    // any code path that sets `app.cursor` without first running
    // `snap_cursor_past_closed_folds` (e.g. edits that shift line
    // numbers underneath an unchanged cursor).
    let total_lines = snap.buffer.line_count().max(1);
    let row_in_view =
        buffer_line_to_visible_row_with(app, snap, cursor.line, area.height as u32, scroll)?;
    let gutter_w = if app.show_line_numbers() {
        gutter_width(total_lines)
    } else {
        2
    };
    // `cursor.byte` is a UTF-8 byte offset into the line; the
    // terminal places glyphs by display width, not byte count. A
    // line containing `§` (2 bytes / 1 cell) or a CJK glyph (3
    // bytes / 2 cells) puts the cursor at the wrong column if we
    // use the byte offset directly. Compute the display width of
    // the prefix `line[..cursor.byte]` -- handles ASCII (1:1),
    // Latin-1 / Greek / Cyrillic (multi-byte but 1 cell), CJK and
    // emoji (1-4 bytes, 2 cells).
    // Cursor column = severity_cell + gutter + display column.
    let col = DIAG_GUTTER_WIDTH + gutter_w + display_col_for_byte(&snap.buffer, cursor);
    Some((
        area.x.saturating_add(col.try_into().unwrap_or(u16::MAX)),
        area.y
            .saturating_add(row_in_view.try_into().unwrap_or(u16::MAX)),
    ))
}

/// Display column (terminal cells) of `pos.byte` within
/// `pos.line`. Falls back to `pos.byte` when the line is missing
/// or the byte index lands past the line end (so the cursor still
/// renders at a sensible position rather than disappearing).
fn display_col_for_byte(buffer: &lattice_core::Buffer, pos: lattice_protocol::Position) -> u32 {
    use unicode_width::UnicodeWidthStr;

    let line = match buffer.line(pos.line) {
        Some(s) => s,
        None => return pos.byte,
    };
    let byte = (pos.byte as usize).min(line.len());
    // Truncate to the prefix at a UTF-8 boundary. `is_char_boundary`
    // is true at index 0 and at every codepoint start; if the
    // caller happened to point inside a multi-byte char (motions
    // shouldn't, but guard anyway), step back to the previous
    // boundary so `&line[..byte]` is a valid str slice.
    let mut byte = byte;
    while byte > 0 && !line.is_char_boundary(byte) {
        byte -= 1;
    }
    UnicodeWidthStr::width(&line[..byte]) as u32
}

/// Project a link's label range onto a single rendered line.
/// Returns `Some((start_byte, end_byte))` (line-relative) when the
/// link covers any portion of the given line, `None` otherwise.
/// Used by the help-overlay renderer to paint Style::Link on each
/// link's label region.
fn link_label_range_on_line(link: &crate::help::HelpLink, line_idx: u32) -> Option<(usize, usize)> {
    let r = &link.range;
    if line_idx < r.start.line || line_idx > r.end.line {
        return None;
    }
    let start = if line_idx == r.start.line {
        r.start.byte as usize
    } else {
        0
    };
    let end = if line_idx == r.end.line {
        r.end.byte as usize
    } else {
        usize::MAX
    };
    if end <= start {
        return None;
    }
    Some((start, end))
}

/// Compose one help-buffer row into ratatui spans by:
/// 1. Walking the markdown highlight `StyledSpan`s and emitting
///    styled segments where they land.
/// 2. Filling unstyled gaps with `TuiStyle::default()`.
///
/// Help-link `[label](scheme:value)` markup is highlighted by the
/// markdown grammar's inline parser via `text.reference` -> `Style::Link`
/// when the inline injection fires; the renderer doesn't need to do
/// anything extra. (When the inline injection is silent on a given
/// row the link still renders as plain text -- the underlying
/// `[label]` and `(url)` characters stay visible, the navigation
/// extracted by `parse_help_links` works regardless.)
fn render_help_line(line: &str, spans: &[lattice_syntax::StyledSpan]) -> Vec<Span<'static>> {
    if spans.is_empty() {
        return vec![Span::raw(line.to_string())];
    }
    let bytes = line.as_bytes();
    let mut out: Vec<Span<'static>> = Vec::with_capacity(spans.len() * 2 + 1);
    let mut cursor = 0usize;
    // Spans should arrive sorted by start; defensive sort + drop
    // overlapping in case the highlighter emits an unusual order.
    let mut sorted: Vec<lattice_syntax::StyledSpan> = spans.to_vec();
    sorted.sort_by_key(|sp| (sp.start, sp.end));
    for span in sorted {
        if span.start < cursor || span.start >= bytes.len() {
            continue;
        }
        if span.start > cursor {
            out.push(Span::raw(line[cursor..span.start].to_string()));
        }
        let end = span.end.min(bytes.len());
        if end <= span.start {
            continue;
        }
        out.push(Span::styled(
            line[span.start..end].to_string(),
            style_to_tui(span.style),
        ));
        cursor = end;
    }
    if cursor < bytes.len() {
        out.push(Span::raw(line[cursor..].to_string()));
    }
    out
}

fn style_to_tui(s: Style) -> TuiStyle {
    match s {
        Style::Default => TuiStyle::default(),
        Style::Keyword => TuiStyle::default()
            .fg(Color::Magenta)
            .add_modifier(Modifier::BOLD),
        Style::Type => TuiStyle::default().fg(Color::Cyan),
        Style::String => TuiStyle::default().fg(Color::Green),
        Style::Number => TuiStyle::default().fg(Color::Yellow),
        Style::Function => TuiStyle::default().fg(Color::Blue),
        Style::Constant => TuiStyle::default().fg(Color::LightYellow),
        Style::Variable => TuiStyle::default(),
        Style::Operator => TuiStyle::default().fg(Color::White),
        Style::Punctuation => TuiStyle::default().fg(Color::Gray),
        Style::Attribute => TuiStyle::default().fg(Color::LightMagenta),
        Style::Comment | Style::LineComment => TuiStyle::default()
            .fg(Color::DarkGray)
            .add_modifier(Modifier::ITALIC),
        // Markup styles (markdown / org / future rich-text modes).
        // Headings cascade lighter / less-bold as level increases so
        // a doc's structure is visually scannable. H1 is the heaviest;
        // H6 is just bold-cyan.
        Style::Heading1 => TuiStyle::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD | Modifier::UNDERLINED),
        Style::Heading2 => TuiStyle::default()
            .fg(Color::LightYellow)
            .add_modifier(Modifier::BOLD),
        Style::Heading3 => TuiStyle::default()
            .fg(Color::LightBlue)
            .add_modifier(Modifier::BOLD),
        Style::Heading4 => TuiStyle::default()
            .fg(Color::LightMagenta)
            .add_modifier(Modifier::BOLD),
        Style::Heading5 => TuiStyle::default()
            .fg(Color::LightGreen)
            .add_modifier(Modifier::BOLD),
        Style::Heading6 => TuiStyle::default()
            .fg(Color::Cyan)
            .add_modifier(Modifier::BOLD),
        Style::Bold => TuiStyle::default().add_modifier(Modifier::BOLD),
        Style::Italic => TuiStyle::default().add_modifier(Modifier::ITALIC),
        Style::Link => TuiStyle::default()
            .fg(Color::Cyan)
            .add_modifier(Modifier::UNDERLINED),
        Style::Url => TuiStyle::default()
            .fg(Color::Blue)
            .add_modifier(Modifier::UNDERLINED),
        Style::MarkupRaw => TuiStyle::default()
            .fg(Color::LightCyan)
            .add_modifier(Modifier::DIM),
        Style::Markup => TuiStyle::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD),
    }
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used, clippy::panic)]
    use super::*;
    use crate::app::App;
    use lattice_core::Document;

    fn app_with(text: &str, viewport: u32) -> App {
        let mut a = App::new(Document::from_text(text));
        a.set_viewport_height(viewport);
        a.refresh_highlights();
        a
    }

    fn line_text(line: &Line<'_>) -> String {
        line.spans.iter().map(|s| s.content.as_ref()).collect()
    }

    #[test]
    fn gutter_width_for_small_buffers() {
        // Layout: 1 leading pad + N digits + GUTTER_TRAILING_PAD (2)
        // = N + 3 cells. 1-digit numbers => 4 cells (" 1  "),
        // 2-digit => 5 (" 99  "), 3-digit => 6 ("100  ").
        assert_eq!(gutter_width(1), 4);
        assert_eq!(gutter_width(9), 4);
        assert_eq!(gutter_width(10), 5);
        assert_eq!(gutter_width(99), 5);
        assert_eq!(gutter_width(100), 6);
    }

    #[test]
    fn render_gutter_separates_number_from_buffer_with_two_cells() {
        // Layout: `[lead][digits][space][glyph_or_space]`. With no
        // fold the rightmost cell is a plain space, so output ends
        // in two spaces -- one separator between digits and glyph
        // slot, one empty glyph slot.
        let span = render_gutter(0, gutter_width(1), None);
        let s = span.content.as_ref();
        assert!(s.ends_with("  "), "expected two trailing spaces, got {s:?}");
        assert!(s.contains('1'), "line number missing: {s:?}");
    }

    #[test]
    fn render_gutter_places_glyph_at_rightmost_cell() {
        // Closed fold ▸ sits at the inner edge of the gutter (next
        // to the buffer column) with a separator space between the
        // line number and the glyph -- the `[ 1 ▸]` layout.
        let span = render_gutter(0, gutter_width(1), Some('▸'));
        let s = span.content.as_ref();
        assert!(s.contains(" 1 ▸"), "expected ' 1 ▸' shape, got {s:?}");
        // Glyph is the last grapheme.
        assert!(s.ends_with('▸'), "glyph must be the rightmost cell: {s:?}");
    }

    #[test]
    fn compose_visible_lines_returns_height_lines_padded_with_marker() {
        let app = app_with("a\nb", 5);
        let lines = compose_visible_lines(&app, &app.document.snapshot(),5, 80);
        assert_eq!(lines.len(), 5);
        // Past EOF lines start with the `~` marker.
        let past_eof = format!("{:?}", lines[3]);
        assert!(past_eof.contains('~'), "expected ~ marker, got {past_eof}");
    }

    #[test]
    fn compose_visible_lines_starts_at_scroll_offset() {
        let mut app = app_with("0\n1\n2\n3\n4", 2);
        app.scroll = 2;
        let lines = compose_visible_lines(&app, &app.document.snapshot(),2, 80);
        // Line index 2 is "2"; expect that text in the rendered first line.
        let l0 = format!("{:?}", lines[0]);
        assert!(
            l0.contains('2'),
            "first visible line should be '2', got {l0}"
        );
    }

    #[test]
    fn cursor_position_advances_for_byte_offset() {
        let mut app = app_with("hello", 5);
        app.cursor.byte = 3;
        let area = Rect::new(0, 0, 80, 5);
        let pos = cursor_screen_position(&app, &app.document.snapshot(),area).unwrap();
        // severity_cell (1) + gutter_width(1)=4 + 3 = 8.
        assert_eq!(pos.0, 8);
        assert_eq!(pos.1, 0);
    }

    #[test]
    fn cursor_position_uses_display_width_for_multibyte_chars() {
        // `§` is 2 bytes / 1 cell in a terminal. With cursor.byte = 6
        // (the `P` of "Performance" on the line below), the rendered
        // column must be 5 cells in (`-`, ` `, `§`, `8`, ` `, `P`),
        // not 6 -- which is what the byte offset would give us if
        // we used it as the column.
        let mut app = app_with("- §8 Performance commitments", 5);
        app.cursor.byte = 6;
        let area = Rect::new(0, 0, 80, 5);
        let pos = cursor_screen_position(&app, &app.document.snapshot(),area).unwrap();
        // severity_cell (1) + gutter_w (4) + 5 display cells = 10.
        assert_eq!(pos.0, 10);
    }

    #[test]
    fn cursor_position_handles_cjk_double_width() {
        // CJK chars are 3 bytes / 2 cells. After "abc中" the cursor
        // at byte 6 (the space after the CJK char) should land at
        // display col 5 (a, b, c, 中=2 cells = total 5 cells).
        let mut app = app_with("abc中 def", 5);
        app.cursor.byte = 6; // past the 3-byte CJK char
        let area = Rect::new(0, 0, 80, 5);
        let pos = cursor_screen_position(&app, &app.document.snapshot(),area).unwrap();
        // severity_cell (1) + gutter_w (4) + 5 display cells = 10.
        assert_eq!(pos.0, 10);
    }

    #[test]
    fn cursor_position_is_none_when_out_of_view() {
        let mut app = app_with("a\nb\nc\nd\ne", 2);
        app.scroll = 0;
        app.cursor.line = 4; // not in viewport [0,1]
        let area = Rect::new(0, 0, 80, 2);
        assert!(cursor_screen_position(&app, &app.document.snapshot(),area).is_none());
    }

    #[test]
    fn cursor_inside_closed_fold_renders_at_fold_heading_row() {
        // Buffer: lines 0..6. Closed fold spans lines 2..=4. The
        // cursor sitting on hidden line 3 must render at the
        // heading row (= row 2 in the visible-line list, since
        // scroll=0). Without the fold-aware projection, the
        // cursor would draw at row 3, which doesn't correspond to
        // any drawn buffer line.
        let mut app = app_with("a\nb\nh\nx\ny\nz\nq", 7);
        app.cursor.line = 3; // hidden by fold
        app.cursor.byte = 0;
        // Push a closed fold over lines 2..=4.
        app.folds.push(crate::app::Fold {
            start_line: 2,
            end_line: 4,
            closed: true,
            identity: None,
        });
        let area = Rect::new(0, 0, 80, 7);
        let pos = cursor_screen_position(&app, &app.document.snapshot(),area).expect("cursor visible");
        // Visible rows: 0=line0, 1=line1, 2=line2 (heading + summary),
        // 3=line5, 4=line6. Cursor at hidden line 3 → screen row 2
        // (area.y + 2 since area.y is 0).
        assert_eq!(
            pos.1,
            area.y + 2,
            "cursor must render on the fold heading row, got row {}",
            pos.1
        );
    }

    #[test]
    fn render_styled_line_with_no_spans_round_trips_text() {
        let spans = render_styled_line("plain text", &[], 80);
        let joined: String = spans.iter().map(|s| s.content.as_ref()).collect();
        assert_eq!(joined, "plain text");
    }

    #[test]
    fn render_styled_line_emits_styled_span_at_offsets() {
        let span = StyledSpan {
            start: 0,
            end: 2,
            style: Style::Keyword,
        };
        let spans = render_styled_line("fn main()", &[span], 80);
        let first = spans
            .iter()
            .find(|s| s.style != TuiStyle::default())
            .expect("at least one styled span");
        assert_eq!(first.content.as_ref(), "fn");
    }

    #[test]
    fn render_styled_line_drops_overlapping_secondary_spans() {
        // Two spans at the same position; the second is ignored to keep the
        // renderer's no-overlap invariant. (tree-sitter-highlight already
        // resolves overlaps; this is belt-and-braces.)
        let primary = StyledSpan {
            start: 0,
            end: 4,
            style: Style::Keyword,
        };
        let overlap = StyledSpan {
            start: 2,
            end: 4,
            style: Style::String,
        };
        let spans = render_styled_line("test rest", &[primary, overlap], 80);
        let total: usize = spans.iter().map(|s| s.content.len()).sum();
        assert_eq!(total, "test rest".len());
    }

    #[test]
    fn truncation_does_not_overrun_max_width() {
        let spans = render_styled_line("this is a long line of text", &[], 6);
        let total: usize = spans.iter().map(|s| s.content.len()).sum();
        assert!(total <= 6, "rendered length {total} exceeded max width 6");
    }

    // ---- Match overlay ----

    use lattice_protocol::position::{Position, Range as ProtoRange};

    fn pos(l: u32, b: u32) -> Position {
        Position::new(l, b)
    }

    #[test]
    fn match_overlay_range_returns_within_line_interval_when_match_is_local() {
        // Match: (0,4)-(0,7) on a 11-char line.
        let r = ProtoRange::new(pos(0, 4), pos(0, 7));
        assert_eq!(match_overlay_range(r, 0, 11), Some((4, 7)));
    }

    #[test]
    fn match_overlay_range_returns_none_when_line_outside_match_band() {
        let r = ProtoRange::new(pos(1, 0), pos(1, 3));
        assert_eq!(match_overlay_range(r, 0, 10), None);
        assert_eq!(match_overlay_range(r, 2, 10), None);
    }

    #[test]
    fn match_overlay_range_extends_to_eol_for_first_line_of_multiline_match() {
        // Match starts on line 0 byte 5 and ends on line 1 byte 2.
        let r = ProtoRange::new(pos(0, 5), pos(1, 2));
        assert_eq!(match_overlay_range(r, 0, 10), Some((5, 10)));
        assert_eq!(match_overlay_range(r, 1, 8), Some((0, 2)));
    }

    #[test]
    fn match_overlay_range_returns_none_when_match_starts_past_line_end() {
        let r = ProtoRange::new(pos(0, 12), pos(0, 15));
        // Line is shorter than the match's start byte -- nothing to overlay.
        assert_eq!(match_overlay_range(r, 0, 10), None);
    }

    #[test]
    fn apply_match_overlay_splits_a_single_span() {
        let spans = vec![Span::raw("hello world".to_string())];
        let style = TuiStyle::default().bg(Color::Yellow);
        let out = apply_match_overlay(spans, 6, 11, style);
        // Expect three spans: "hello ", "world", and (none after, since 11 == len).
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].content.as_ref(), "hello ");
        assert_eq!(out[1].content.as_ref(), "world");
        assert_eq!(out[1].style, style);
    }

    #[test]
    fn apply_match_overlay_clips_when_match_partially_overlaps_styled_span() {
        // "fn main" with "fn" already styled as keyword; overlay covers "n m".
        let spans = vec![
            Span::styled("fn".to_string(), TuiStyle::default().fg(Color::Magenta)),
            Span::raw(" main".to_string()),
        ];
        let style = TuiStyle::default().bg(Color::Yellow);
        let out = apply_match_overlay(spans, 1, 4, style);
        // Pieces: "f" (kw), "n" (overlay), " m" (overlay), "ain" (raw)
        let texts: Vec<&str> = out.iter().map(|s| s.content.as_ref()).collect();
        assert_eq!(texts, vec!["f", "n", " m", "ain"]);
    }

    #[test]
    fn apply_match_overlay_passes_through_when_no_overlap() {
        let spans = vec![Span::raw("untouched".to_string())];
        let style = TuiStyle::default().bg(Color::Yellow);
        let out = apply_match_overlay(spans, 100, 110, style);
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].content.as_ref(), "untouched");
    }

    #[test]
    fn compose_visible_lines_appends_ghost_text_at_eol_when_enabled() {
        // With completion.ghost_text on AND popup open with a
        // prefix-matching top candidate, the cursor's line ends
        // with a dimmed span carrying the suffix.
        let mut app = app_with("foo", 5);
        app.modal = lattice_grammar::ModalState::Insert;
        app.cursor = pos(0, 3);
        app.config
            .set(app.core_options.completion_ghost_text, true)
            .expect("set ghost_text");
        // Install a popup with `foobar` as the top candidate
        // and `foo` as the typed query.
        let mut state = lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            app.cursor,
            app.cursor,
            "foo".into(),
        );
        let raw = lattice_completion::RawCandidate::plain(
            "foobar",
            lattice_completion::CandidateKind::Plain,
        );
        state.raw.push(raw.clone());
        state
            .rendered
            .push(lattice_completion::RenderedCandidate::from_scored(
                lattice_completion::ScoredCandidate {
                    raw,
                    score: lattice_completion::MatchScore(800),
                    match_ranges: Vec::new(),
                },
            ));
        app.insert_completion = Some(state);

        let lines = compose_visible_lines(&app, &app.document.snapshot(), 1, 80);
        let composed = line_text(&lines[0]);
        // The line should contain BOTH the buffer text `foo`
        // AND the ghost suffix `bar`.
        assert!(
            composed.contains("foo") && composed.contains("bar"),
            "expected ghost suffix appended; got `{composed}`",
        );
        // The LAST span on the line is the ghost — confirm it's
        // dim-styled (DarkGray) so it renders subtler than the
        // buffer text.
        let last = lines[0]
            .spans
            .last()
            .expect("at least one span on the rendered line");
        assert_eq!(last.content.as_ref(), "bar");
        assert_eq!(last.style.fg, Some(Color::DarkGray));
    }

    #[test]
    fn compose_visible_lines_no_ghost_when_cursor_not_at_eol() {
        // Cursor mid-line -> ghost would visually clash with
        // existing buffer content; producer suppresses.
        let mut app = app_with("foobaz", 5);
        app.modal = lattice_grammar::ModalState::Insert;
        app.cursor = pos(0, 3); // between `foo` and `baz`
        app.config
            .set(app.core_options.completion_ghost_text, true)
            .expect("set ghost_text");
        let mut state = lattice_completion::InsertCompletionState::open(
            lattice_completion::CompletionTrigger::Manual,
            app.cursor,
            app.cursor,
            "foo".into(),
        );
        let raw = lattice_completion::RawCandidate::plain(
            "foobar",
            lattice_completion::CandidateKind::Plain,
        );
        state.raw.push(raw.clone());
        state
            .rendered
            .push(lattice_completion::RenderedCandidate::from_scored(
                lattice_completion::ScoredCandidate {
                    raw,
                    score: lattice_completion::MatchScore(800),
                    match_ranges: Vec::new(),
                },
            ));
        app.insert_completion = Some(state);
        let lines = compose_visible_lines(&app, &app.document.snapshot(), 1, 80);
        let composed = line_text(&lines[0]);
        // `foobaz` from the buffer is fine; `foobar` (ghost)
        // mustn't sneak in.
        assert!(composed.contains("foobaz"));
        assert!(
            !composed.contains("foobar"),
            "ghost suppressed mid-line; got `{composed}`",
        );
    }

    #[test]
    fn compose_visible_lines_applies_match_overlay() {
        let mut app = app_with("hello world", 1);
        app.current_match = Some(ProtoRange::new(pos(0, 6), pos(0, 11)));
        let lines = compose_visible_lines(&app, &app.document.snapshot(),1, 80);
        let dump = format!("{:?}", lines[0]);
        // Spans should be split so "world" is its own span; we look for the
        // match style's signature in the debug dump.
        assert!(dump.contains("world"), "rendered: {dump}");
    }

    // ---- Visual selection rendering ----

    use lattice_grammar::VisualKind;
    use lattice_protocol::selection::{Selection, SelectionSet, VisualMode};

    #[test]
    fn visual_selection_range_is_none_when_not_in_visual() {
        let app = app_with("hello", 5);
        assert!(visual_selection_range(&app).is_none());
    }

    #[test]
    fn visual_selection_range_charwise_includes_head_byte() {
        let mut app = app_with("hello", 5);
        app.apply(crate::app::Action::EnterVisual(VisualKind::Charwise));
        // Move cursor to byte 2 -- selection extends from 0 to 2 inclusive.
        let sel = Selection {
            anchor: pos(0, 0),
            head: pos(0, 2),
            visual: Some(VisualMode::Charwise),
        };
        app.set_selections_blocking(SelectionSet::single(sel));
        let r = visual_selection_range(&app).expect("range");
        assert_eq!(r.start, pos(0, 0));
        // Charwise includes head: end byte = head.byte + 1.
        assert_eq!(r.end, pos(0, 3));
    }

    #[test]
    fn visual_selection_range_linewise_covers_full_lines() {
        let mut app = app_with("aaa\nbbb\nccc", 5);
        app.apply(crate::app::Action::EnterVisual(VisualKind::Linewise));
        let sel = Selection {
            anchor: pos(0, 1),
            head: pos(2, 1),
            visual: Some(VisualMode::Linewise),
        };
        app.set_selections_blocking(SelectionSet::single(sel));
        let r = visual_selection_range(&app).expect("range");
        assert_eq!(r.start, pos(0, 0));
        // Linewise end byte is u32::MAX so per-line clamping picks line_len.
        assert_eq!(r.end.line, 2);
    }

    #[test]
    fn visual_selection_range_normalises_reversed_anchor_head() {
        let mut app = app_with("hello", 5);
        app.apply(crate::app::Action::EnterVisual(VisualKind::Charwise));
        // anchor > head (the user moved leftward in Visual).
        let sel = Selection {
            anchor: pos(0, 4),
            head: pos(0, 1),
            visual: Some(VisualMode::Charwise),
        };
        app.set_selections_blocking(SelectionSet::single(sel));
        let r = visual_selection_range(&app).expect("range");
        assert_eq!(r.start, pos(0, 1));
        assert_eq!(r.end, pos(0, 5));
    }

    #[test]
    fn visual_block_extents_returns_none_when_not_blockwise() {
        let app = app_with("hello", 5);
        assert!(visual_block_extents(&app).is_none());
    }

    #[test]
    fn visual_block_extents_normalises_anchor_and_head() {
        let mut app = app_with("aaa\nbbb\nccc", 10);
        app.apply(crate::app::Action::EnterVisual(VisualKind::Blockwise));
        let sel = Selection {
            anchor: pos(2, 1),
            head: pos(0, 2),
            visual: Some(VisualMode::Blockwise),
        };
        app.set_selections_blocking(SelectionSet::single(sel));
        let b = visual_block_extents(&app).unwrap();
        assert_eq!(b.start_line, 0);
        assert_eq!(b.end_line, 2);
        assert_eq!(b.start_col, 1);
        assert_eq!(b.end_col, 2);
    }

    #[test]
    fn compose_visible_lines_overlays_visual_selection() {
        let mut app = app_with("hello world", 1);
        app.apply(crate::app::Action::EnterVisual(VisualKind::Charwise));
        let sel = Selection {
            anchor: pos(0, 0),
            head: pos(0, 4),
            visual: Some(VisualMode::Charwise),
        };
        app.set_selections_blocking(SelectionSet::single(sel));
        let lines = compose_visible_lines(&app, &app.document.snapshot(),1, 80);
        let dump = format!("{:?}", lines[0]);
        // The selected "hello" should appear as its own span(s); we just
        // verify the line still contains the original text after overlay.
        assert!(dump.contains("hello"));
        assert!(dump.contains("world"));
    }

    // --- Heading-preserved fold render -------------------------

    #[test]
    fn closed_fold_preserves_heading_and_appends_summary() {
        let mut app = app_with("# Heading\nbody one\nbody two\nafter\n", 5);
        app.set_foldmethod_for_test(crate::app::FoldMethod::Markdown);
        app.recompute_folds();
        // Close the heading fold.
        let idx = app
            .folds
            .iter()
            .position(|f| f.start_line == 0)
            .expect("heading fold");
        app.folds[idx].closed = true;
        let lines = compose_visible_lines(&app, &app.document.snapshot(),5, 80);
        let row0 = line_text(&lines[0]);
        // Heading text is preserved.
        assert!(row0.contains("# Heading"), "row0 = {row0:?}");
        // Summary suffix appended.
        assert!(row0.contains("lines folded"), "row0 = {row0:?}");
    }

    #[test]
    fn closed_fold_hides_interior_lines() {
        let mut app = app_with("# H\nhidden1\nhidden2\nshown\n", 5);
        app.set_foldmethod_for_test(crate::app::FoldMethod::Markdown);
        app.recompute_folds();
        let idx = app
            .folds
            .iter()
            .position(|f| f.start_line == 0)
            .expect("heading fold");
        app.folds[idx].closed = true;
        let lines = compose_visible_lines(&app, &app.document.snapshot(),5, 80);
        let blob: String = lines.iter().map(line_text).collect::<Vec<_>>().join("\n");
        assert!(!blob.contains("hidden1"), "interior leaked: {blob}");
        assert!(!blob.contains("hidden2"), "interior leaked: {blob}");
    }

    #[test]
    fn closed_fold_summary_includes_chained_closed_folds() {
        // Reproduces the user's "fold both branches of an if/else
        // under foldmethod=indent" case: two closed folds touch at
        // line 3 -- the outer (1, 3) hides 2..=3, the sibling
        // (3, 5) hides 4..=5 (its heading at 3 is itself hidden by
        // the first fold). Visually the user collapses 5 buffer
        // lines onto one row; the summary should report 5, not 3.
        let mut app = app_with("a\nb\nc\nd\ne\nf\ng\n", 7);
        app.folds.push(crate::app::Fold {
            start_line: 1,
            end_line: 3,
            closed: true,
            identity: None,
        });
        app.folds.push(crate::app::Fold {
            start_line: 3,
            end_line: 5,
            closed: true,
            identity: None,
        });
        let lines = compose_visible_lines(&app, &app.document.snapshot(),7, 80);
        // Find the row that summarises the chained folds (line 1's
        // heading row).
        let row1_text = line_text(&lines[1]);
        assert!(
            row1_text.contains("5 lines folded"),
            "expected '5 lines folded' for chained folds, got: {row1_text:?}"
        );
    }

    #[test]
    fn open_fold_renders_lines_normally_without_summary() {
        let mut app = app_with("# H\nbody\n", 5);
        app.set_foldmethod_for_test(crate::app::FoldMethod::Markdown);
        app.recompute_folds();
        // Leave the fold open (default).
        let lines = compose_visible_lines(&app, &app.document.snapshot(),5, 80);
        let row0 = line_text(&lines[0]);
        assert!(row0.contains("# H"), "row0 = {row0:?}");
        assert!(
            !row0.contains("lines folded"),
            "summary should only appear on closed folds: {row0:?}"
        );
    }

    // --- Fold gutter glyphs ------------------------------------

    #[test]
    fn open_fold_gutter_shows_down_glyph() {
        let mut app = app_with("# H\nbody\n", 5);
        app.set_foldmethod_for_test(crate::app::FoldMethod::Markdown);
        app.recompute_folds();
        let lines = compose_visible_lines(&app, &app.document.snapshot(),5, 80);
        let row0 = line_text(&lines[0]);
        assert!(row0.contains('▾'), "expected ▾ glyph on open fold: {row0:?}");
        assert!(!row0.contains('▸'), "did not expect ▸ glyph: {row0:?}");
    }

    #[test]
    fn closed_fold_gutter_shows_right_glyph() {
        let mut app = app_with("# H\nbody\n", 5);
        app.set_foldmethod_for_test(crate::app::FoldMethod::Markdown);
        app.recompute_folds();
        let idx = app
            .folds
            .iter()
            .position(|f| f.start_line == 0)
            .expect("heading fold");
        app.folds[idx].closed = true;
        let lines = compose_visible_lines(&app, &app.document.snapshot(),5, 80);
        let row0 = line_text(&lines[0]);
        assert!(row0.contains('▸'), "expected ▸ glyph on closed fold: {row0:?}");
        assert!(!row0.contains('▾'), "did not expect ▾ glyph: {row0:?}");
    }

    #[test]
    fn line_after_closed_fold_keeps_correct_syntax_highlighting() {
        // Reproduces a user-reported regression: with a closed fold
        // hiding interior lines, the next visible line was being
        // styled with stale spans from `visible_highlights[viewport_row]`
        // because the row index assumed `visible[i] == scroll + i`.
        // The fix indexes into `visible_highlights` by buffer-line
        // delta instead of viewport row.
        //
        // The struct fold now also swallows the trailing `}` (closer
        // inclusion), so the "next visible line" is the trailing
        // statement, not the brace.
        let src = "pub struct Buffer {\n    rope: Rope,\n}\nlet trailing = 1;\n";
        let mut app = app_with(src, 10);
        app.set_foldmethod_for_test(crate::app::FoldMethod::Indent);
        app.recompute_folds();
        let idx = app
            .folds
            .iter()
            .position(|f| f.start_line == 0)
            .expect("struct fold");
        app.folds[idx].closed = true;
        let lines = compose_visible_lines(&app, &app.document.snapshot(),4, 80);
        // Row 0: heading + " ┄ N lines folded".
        // Row 1: the post-fold statement -- correct content, not
        //        leaking interior spans.
        let row1 = line_text(&lines[1]);
        assert!(
            row1.contains("let trailing"),
            "row1 should be the post-fold statement: {row1:?}"
        );
        assert!(!row1.contains("rope"), "interior leaked: {row1:?}");
        assert!(!row1.contains('}'), "closer should be inside the fold: {row1:?}");
    }

    #[test]
    fn closed_indent_fold_swallows_trailing_close_brace() {
        // Vim's `foldmethod=indent` strictly excludes lines whose
        // indent isn't > start. We extend that with closer-line
        // inclusion: a `}` / `]` / `)` line at the same indent as
        // the fold start gets pulled in, so the user doesn't see an
        // orphan brace below `... ┄ N lines folded`.
        let src = "pub struct Buffer {\n    rope: Rope,\n}\n";
        let mut app = app_with(src, 5);
        app.set_foldmethod_for_test(crate::app::FoldMethod::Indent);
        app.recompute_folds();
        let f = app.folds.iter().find(|f| f.start_line == 0).expect("fold");
        assert_eq!(f.end_line, 2, "expected `}}` swallowed: {f:?}");
    }

    #[test]
    fn linewise_visual_highlights_closed_fold_heading() {
        // Regression: previously the closed-fold heading branch in
        // compose_visible_lines emitted the summary suffix and
        // `continue`'d before the visual overlay ran -- so V on a
        // closed-fold heading appeared unhighlighted. The summary
        // suffix is now appended AFTER overlay processing.
        let src = "pub struct Buffer {\n    rope: Rope,\n}\n";
        let mut app = app_with(src, 5);
        app.set_foldmethod_for_test(crate::app::FoldMethod::Indent);
        app.recompute_folds();
        let idx = app
            .folds
            .iter()
            .position(|f| f.start_line == 0)
            .expect("fold");
        app.folds[idx].closed = true;
        app.cursor = lattice_protocol::position::Position::new(0, 0);
        app.apply(crate::app::Action::EnterVisual(VisualKind::Linewise));
        let sel = Selection {
            anchor: pos(0, 0),
            head: pos(0, 0),
            visual: Some(VisualMode::Linewise),
        };
        app.set_selections_blocking(SelectionSet::single(sel));
        let lines = compose_visible_lines(&app, &app.document.snapshot(),5, 80);
        let visual_bg = visual_style().bg;
        let row0 = &lines[0];
        let has_visual_span = row0.spans.iter().any(|s| s.style.bg == visual_bg);
        assert!(
            has_visual_span,
            "linewise visual on a closed-fold heading must still overlay: {row0:?}"
        );
        // Summary suffix is still present.
        let row0_text = line_text(row0);
        assert!(
            row0_text.contains("lines folded"),
            "summary suffix lost: {row0_text:?}"
        );
    }

    #[test]
    fn linewise_visual_overlays_full_line_after_fold_change() {
        // After the v-line key, a line outside any fold should still
        // overlay correctly. This is a guard against the fold work
        // accidentally breaking line-visual on plain documents.
        let mut app = app_with("alpha\nbeta\ngamma\n", 5);
        app.cursor = lattice_protocol::position::Position::new(1, 0);
        app.apply(crate::app::Action::EnterVisual(VisualKind::Linewise));
        let sel = Selection {
            anchor: pos(1, 0),
            head: pos(1, 0),
            visual: Some(VisualMode::Linewise),
        };
        app.set_selections_blocking(SelectionSet::single(sel));
        let lines = compose_visible_lines(&app, &app.document.snapshot(),5, 80);
        // Verify the second visible line ("beta") has at least one
        // span styled with the visual color.
        let visual_bg = visual_style().bg;
        let row1 = &lines[1];
        let has_visual_span = row1.spans.iter().any(|s| s.style.bg == visual_bg);
        assert!(
            has_visual_span,
            "linewise visual should overlay the selected line: {row1:?}"
        );
    }

    #[test]
    fn lines_without_fold_start_have_no_glyph() {
        let mut app = app_with("# H\nbody one\nbody two\nafter\n", 5);
        app.set_foldmethod_for_test(crate::app::FoldMethod::Markdown);
        app.recompute_folds();
        let lines = compose_visible_lines(&app, &app.document.snapshot(),5, 80);
        // Row 1 (body one) is inside the fold, not a fold start.
        let row1 = line_text(&lines[1]);
        assert!(!row1.contains('▸'), "row1: {row1:?}");
        assert!(!row1.contains('▾'), "row1: {row1:?}");
    }

    // ---- LSP diagnostic rendering tests (Phase 4.1.d.iii) ----

    /// Helper: seed a diagnostic into the App's LSP layer for
    /// the given line range + severity, mapping the App's
    /// active buffer to a fake URI.
    fn seed_diagnostic(
        app: &mut App,
        line: u32,
        start_col: u32,
        end_col: u32,
        severity: lattice_lsp::DiagnosticSeverity,
        message: &str,
    ) {
        use std::str::FromStr;
        let uri = lattice_lsp::Uri::from_str("file:///tmp/x.rs").unwrap();
        app.buffer_uris.insert(app.document_buffer_id, uri.clone());
        let diag = lattice_lsp::Diagnostic {
            range: lattice_lsp::LspRange {
                start: lattice_lsp::LspPosition {
                    line,
                    character: start_col,
                },
                end: lattice_lsp::LspPosition {
                    line,
                    character: end_col,
                },
            },
            severity: Some(severity),
            code: None,
            code_description: None,
            source: None,
            message: message.into(),
            related_information: None,
            tags: None,
            data: None,
        };
        app.lsp_diagnostics.apply(lattice_lsp::DiagnosticEvent {
            server_id: std::sync::Arc::from("rust"),
            uri,
            version: None,
            diagnostics: std::sync::Arc::from(vec![diag].into_boxed_slice()),
        });
    }

    #[test]
    fn diagnostic_severity_glyph_appears_in_gutter_for_error() {
        let mut app = app_with("fn main() {}\nlet x = 1;\n", 5);
        seed_diagnostic(
            &mut app,
            0,
            0,
            7,
            lattice_lsp::DiagnosticSeverity::ERROR,
            "boom",
        );
        let lines = compose_visible_lines(&app, &app.document.snapshot(), 5, 80);
        // Row 0 has the error; expect the ■ glyph somewhere in
        // the rendered first span (the severity cell).
        let row0 = line_text(&lines[0]);
        assert!(
            row0.contains('■'),
            "expected error glyph on diag line; got {row0:?}"
        );
        // Row 1 has no diagnostic; should NOT have any
        // severity glyph.
        let row1 = line_text(&lines[1]);
        assert!(!row1.contains('■'), "row 1 should be clean: {row1:?}");
        assert!(!row1.contains('▲'), "row 1 should be clean: {row1:?}");
    }

    #[test]
    fn diagnostic_warning_uses_triangle_glyph() {
        let mut app = app_with("hello\n", 3);
        seed_diagnostic(
            &mut app,
            0,
            0,
            5,
            lattice_lsp::DiagnosticSeverity::WARNING,
            "warn",
        );
        let lines = compose_visible_lines(&app, &app.document.snapshot(), 3, 80);
        let row0 = line_text(&lines[0]);
        assert!(row0.contains('▲'), "expected warning glyph; got {row0:?}");
    }

    #[test]
    fn diagnostic_hint_uses_dot_glyph() {
        let mut app = app_with("hello\n", 3);
        seed_diagnostic(
            &mut app,
            0,
            0,
            1,
            lattice_lsp::DiagnosticSeverity::HINT,
            "hint",
        );
        let lines = compose_visible_lines(&app, &app.document.snapshot(), 3, 80);
        let row0 = line_text(&lines[0]);
        assert!(row0.contains('·'), "expected hint glyph; got {row0:?}");
    }

    #[test]
    fn most_severe_wins_per_line() {
        let mut app = app_with("hello\n", 3);
        seed_diagnostic(
            &mut app,
            0,
            0,
            3,
            lattice_lsp::DiagnosticSeverity::WARNING,
            "warn",
        );
        seed_diagnostic(
            &mut app,
            0,
            2,
            5,
            lattice_lsp::DiagnosticSeverity::ERROR,
            "err",
        );
        let lines = compose_visible_lines(&app, &app.document.snapshot(), 3, 80);
        let row0 = line_text(&lines[0]);
        // Error wins over warning on the same line for the
        // gutter glyph (most-severe semantics).
        assert!(row0.contains('■'), "row0 expected ■: {row0:?}");
    }

    #[test]
    fn modeline_lsp_segment_empty_when_no_uri_mapping() {
        let app = App::new(Document::from_text(""));
        // No initialize_lsp -> buffer_uris empty -> indicator hidden.
        assert_eq!(active_lsp_segment(&app), "");
    }

    #[test]
    fn modeline_lsp_segment_empty_when_no_servers_attached() {
        let mut app = App::new(Document::from_text(""));
        // Seed a URI mapping but no actor/attachment -- supervisor
        // returns an empty handle list, so the indicator stays empty.
        let fake_uri =
            <lattice_lsp::Uri as std::str::FromStr>::from_str("file:///tmp/x.rs").unwrap();
        app.buffer_uris.insert(app.document_buffer_id, fake_uri);
        assert_eq!(active_lsp_segment(&app), "");
    }

    #[test]
    fn no_lsp_attachment_no_severity_glyph() {
        let app = app_with("hello\n", 3);
        // No buffer_uri mapping -> no diagnostics queryable.
        let lines = compose_visible_lines(&app, &app.document.snapshot(), 3, 80);
        let row0 = line_text(&lines[0]);
        assert!(!row0.contains('■'), "no LSP -> no error glyph: {row0:?}");
        assert!(!row0.contains('▲'), "no LSP -> no warn glyph: {row0:?}");
    }

    #[test]
    fn diagnostic_underline_modifier_applied_to_overlap_range() {
        let mut app = app_with("hello world\n", 3);
        // Underline cols 6..11 ("world") with an error.
        seed_diagnostic(
            &mut app,
            0,
            6,
            11,
            lattice_lsp::DiagnosticSeverity::ERROR,
            "err",
        );
        let lines = compose_visible_lines(&app, &app.document.snapshot(), 3, 80);
        // Walk every span on row 0; at least one span covering
        // bytes 6..11 must have UNDERLINED set.
        let mut found_underline = false;
        for span in &lines[0].spans {
            if span.style.add_modifier.contains(Modifier::UNDERLINED)
                || span.style.sub_modifier.is_empty()
                    && span
                        .style
                        .add_modifier
                        .contains(Modifier::UNDERLINED)
            {
                found_underline = true;
                break;
            }
        }
        assert!(
            found_underline,
            "expected an UNDERLINED modifier somewhere in the row's spans: {:?}",
            lines[0]
        );
    }
}
