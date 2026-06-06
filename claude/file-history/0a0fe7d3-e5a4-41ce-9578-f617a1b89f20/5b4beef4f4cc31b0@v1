//! Translate `crossterm` key events into `Action`s.
//!
//! This is a small, pure function that reads modal state, the pending-key
//! buffer, and the catalog of built-in command IDs to decide what each key
//! press means. It is the v1 stand-in for the layered keymap engine
//! described in DESIGN.md §5.2.3 -- the *shape* matches (chord -> typed
//! invocation) so swapping in a real keymap layer later is mechanical.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use lattice_grammar::ModalState;
use lattice_grammar::SearchDirection;
use lattice_grammar::Target;
use lattice_grammar::VisualKind;
use lattice_grammar::args::Args;
use lattice_grammar::builtins::Builtins;
use lattice_grammar::command::CommandInvocation;
use lattice_grammar::register::Register;
use lattice_grammar::registry::{MotionId, OperatorId};

use crate::app::{Action, FindKind, Pending, ScrollPos, ViewportPos};
use crate::buffers::BufferKind;
use crate::pane::PaneDirection;

pub struct TranslateContext<'a> {
    pub modal: ModalState,
    pub pending: Pending,
    pub builtins: &'a Builtins,
    /// In-progress count prefix; `0` means none. Translate uses this to
    /// disambiguate the `0` key (line_start when no count in progress;
    /// digit-zero appended to count otherwise).
    pub pending_count: u32,
    /// True when a macro is currently being recorded. Translate uses
    /// this so `q` while recording stops, while `q` otherwise starts a
    /// new recording.
    pub recording_macro: bool,
    /// Which buffer the App's input pipeline currently routes to.
    /// Driven by [`crate::app::App::active_buffer`]; defaults to
    /// [`BufferKind::Document`]. Help buffers route through the
    /// same Normal-mode chord grammar (motions, `<C-o>` / `<C-i>`,
    /// `gg` / `G`, etc.) -- only three buffer-local bindings
    /// differ: `Esc` / `q` dismiss the help overlay, and `<CR>`
    /// follows the link under the cursor.
    pub active_buffer: BufferKind,
    /// True when the command-line completion popup is open
    /// (DESIGN.md §5.11.3). Tab / S-Tab / Enter / Esc are claimed
    /// by the popup before falling through to Command mode.
    pub completion_open: bool,
    /// True when the cmdline cursor sits on an `ArgKind::Chord`
    /// arg slot. In this mode every key event renders to a chord
    /// token and gets appended; the only edits are `<BS>` (delete
    /// last chord token), `<CR>` (submit), `<Esc>` (cancel). Lookup
    /// of multi-stroke sequences (`gg`, `<C-w>j`) is supported by
    /// pressing each chord in turn.
    pub chord_capture: bool,
    /// True when a picker (`Picker` overlay) is open. Picker
    /// claims every key before the modal handlers see it: char
    /// keys append to the query, `<Up>` / `<C-p>` / `<Down>` /
    /// `<C-n>` move selection, `<CR>` accepts, `<Esc>` dismisses.
    pub picker_open: bool,
    /// True when the **Insert-mode completion popup** is open
    /// (Phase 4.2.g.1). Activates the completion-popup minor
    /// mode: `<C-n>` / `<C-p>` navigate, `<C-y>` / `<Tab>` /
    /// `<CR>` accept, `<C-e>` cancels, `<Esc>` cancels and
    /// exits Insert. Bindings inside this layer override the
    /// usual Insert-mode + Normal-mode meanings (notably
    /// `<C-d>` becomes "toggle docs popup" instead of
    /// "shift-left-indent" / "half-page-down"). Closing the
    /// popup deactivates the layer; original bindings restore.
    pub insert_completion_open: bool,
    /// True when an `ActiveSnippet` is in flight (Phase
    /// 4.2.g.4). Activates the active-snippet minor mode:
    /// `<Tab>` jumps to the next placeholder, `<S-Tab>` to
    /// the previous, `<Esc>` exits the snippet (and Insert).
    /// The layer claims `<Tab>` ahead of Insert-mode's
    /// "insert literal tab" so placeholder navigation is
    /// stable; closing the snippet deactivates the layer.
    pub snippet_active: bool,
}

pub fn translate(ctx: TranslateContext<'_>, event: KeyEvent) -> Action {
    // Picker overlay precedes everything (DESIGN.md §5.9.7): the
    // user is in a focused "type to filter, Enter to act" state;
    // modal handlers never see these keys until the picker is
    // dismissed. `<C-c>` still drops the picker rather than the
    // app so an open picker isn't a foot-gun.
    if ctx.picker_open {
        return translate_picker(event);
    }

    // Insert-mode completion popup minor mode (Phase 4.2.g.1).
    // Active only while `App.insert_completion.is_some()`. Owns
    // the keys it overrides for the popup's lifetime; closing
    // the popup deactivates the layer and Insert-mode + Normal-
    // mode defaults restore (see `docs/insert-completion.md`
    // §3.8).
    if ctx.insert_completion_open
        && let Some(action) = translate_insert_completion_popup(event)
    {
        return action;
    }

    // Active-snippet minor mode (Phase 4.2.g.4). Active only
    // while `App.active_snippet.is_some()` AND we're in Insert
    // mode AND the completion popup isn't open (popup wins for
    // `<Tab>` -> accept). Claims `<Tab>` / `<S-Tab>` for
    // placeholder navigation and `<Esc>` to exit the snippet
    // before Insert-mode handlers see those keys; everything
    // else flows through to the normal Insert dispatch so the
    // user can keep typing inside placeholders.
    if ctx.snippet_active
        && matches!(ctx.modal, ModalState::Insert)
        && !ctx.insert_completion_open
        && let Some(action) = translate_active_snippet(event)
    {
        return action;
    }

    // Chord-capture overlay precedes the universal `<C-c>` -> Quit
    // hatch, because looking up `<C-c>`'s binding via
    // `:describe-key <C-c>` is a legitimate user need. The overlay
    // reserves Esc as the abort path, so the user is never stuck.
    if matches!(ctx.modal, ModalState::Command) && ctx.chord_capture {
        return translate_command_chord_capture(event);
    }

    // Universal escape hatch.
    if event.modifiers.contains(KeyModifiers::CONTROL) && matches!(event.code, KeyCode::Char('c')) {
        return Action::Quit;
    }

    // Buffer-local bindings for read-only buffers (Help / FileTree;
    // DESIGN.md §5.9 buffer-local keymap layer): a small fixed set
    // of bindings unique to those kinds (dismiss + follow-link)
    // intercept first, then everything else flows through
    // `translate_normal` so the chord grammar (`gg`, `<C-d>`,
    // `<C-o>` / `<C-i>`, motions, viewport jumps) works identically
    // to the document path. The cursor that those motions move is
    // decided at apply time by `App::active_buffer`, not here.
    if matches!(ctx.active_buffer, BufferKind::Help | BufferKind::FileTree)
        && matches!(ctx.modal, ModalState::Normal)
        && matches!(ctx.pending, Pending::None)
    {
        match event.code {
            KeyCode::Esc => return Action::HelpDismiss,
            KeyCode::Char('q') if !ctx.recording_macro => return Action::HelpDismiss,
            KeyCode::Enter => return Action::FollowLink,
            _ => {}
        }
    }

    match ctx.modal {
        ModalState::Insert => translate_insert(event, ctx.pending),
        ModalState::Normal => translate_normal(
            event,
            ctx.pending,
            ctx.builtins,
            ctx.pending_count,
            ctx.recording_macro,
        ),
        ModalState::Command => translate_command(event, ctx.completion_open, ctx.chord_capture),
        ModalState::Search(_) => translate_search(event),
        ModalState::Visual(kind) => translate_visual(event, kind, ctx.builtins),
        ModalState::Replace => translate_replace(event),
        // OperatorPending routes to no-op (it's a transient resolution
        // state inside translate_normal, not a top-level reachable state).
        _ => Action::None,
    }
}

fn translate_replace(event: KeyEvent) -> Action {
    if event.modifiers.contains(KeyModifiers::CONTROL) {
        return Action::None;
    }
    match event.code {
        KeyCode::Esc => Action::EnterMode(ModalState::Normal),
        KeyCode::Backspace => Action::ReplaceUndoLast,
        KeyCode::Enter => Action::Insert("\n".into()),
        KeyCode::Char(c) => Action::OverwriteChar(c),
        _ => Action::None,
    }
}

fn translate_visual(event: KeyEvent, kind: VisualKind, builtins: &Builtins) -> Action {
    if event.modifiers.contains(KeyModifiers::CONTROL) {
        return Action::None;
    }
    // Block-visual-only: I / A enter Insert with multi-line replay
    // wired in App::do_enter_block_visual_insert. In charwise /
    // linewise we don't bind these (vim's behavior is also distinct
    // there -- linewise `I` is a separate v2 feature).
    if matches!(kind, VisualKind::Blockwise) {
        match event.code {
            KeyCode::Char('I') => return Action::EnterBlockVisualInsert,
            KeyCode::Char('A') => return Action::EnterBlockVisualAppend,
            _ => {}
        }
    }
    match event.code {
        KeyCode::Esc => Action::ExitVisual,
        // Toggle: pressing `v` while in Visual exits.
        KeyCode::Char('v') => Action::ExitVisual,
        KeyCode::Char('V') => Action::ExitVisual,

        // Motions extend the selection. Reuse the same builtins; the
        // App layer rewrites the resulting SelectionChange so the anchor
        // is preserved.
        KeyCode::Char('h') | KeyCode::Left => invoke(builtins.char_left),
        KeyCode::Char('j') | KeyCode::Down => invoke(builtins.line_down),
        KeyCode::Char('k') | KeyCode::Up => invoke(builtins.line_up),
        KeyCode::Char('l') | KeyCode::Right => invoke(builtins.char_right),
        KeyCode::Char('0') | KeyCode::Home => invoke(builtins.line_start),
        KeyCode::Char('$') | KeyCode::End => invoke(builtins.line_end),
        KeyCode::Char('^') => invoke(builtins.first_non_blank),
        KeyCode::Char('w') => invoke(builtins.word_forward),
        KeyCode::Char('b') => invoke(builtins.word_backward),
        KeyCode::Char('e') => invoke(builtins.word_end),
        KeyCode::Char('W') => invoke(builtins.big_word_forward),
        KeyCode::Char('B') => invoke(builtins.big_word_backward),
        KeyCode::Char('E') => invoke(builtins.big_word_end),
        KeyCode::Char('}') => invoke(builtins.paragraph_forward),
        KeyCode::Char('{') => invoke(builtins.paragraph_backward),
        KeyCode::Char(')') => invoke(builtins.sentence_forward),
        KeyCode::Char('(') => invoke(builtins.sentence_backward),
        KeyCode::Char('G') => invoke(builtins.goto_last_line),

        // Operators on the selection. `Range::Selection` resolves to the
        // current document.selections().primary() in the dispatcher.
        KeyCode::Char('d') | KeyCode::Char('x') => Action::Invoke(
            CommandInvocation::of(builtins.delete.0).with_range(lattice_grammar::Range::Selection),
        ),
        KeyCode::Char('c') | KeyCode::Char('s') => Action::Invoke(
            CommandInvocation::of(builtins.change.0).with_range(lattice_grammar::Range::Selection),
        ),
        KeyCode::Char('y') => Action::Invoke(
            CommandInvocation::of(builtins.yank.0).with_range(lattice_grammar::Range::Selection),
        ),
        // Indent / dedent the lines covered by the selection.
        // Operator's range-walker iterates lines top-down so the
        // selection kind (charwise / linewise / blockwise) doesn't
        // change the result -- only the line span matters.
        KeyCode::Char('>') => Action::Invoke(
            CommandInvocation::of(builtins.indent_right.0)
                .with_range(lattice_grammar::Range::Selection),
        ),
        KeyCode::Char('<') => Action::Invoke(
            CommandInvocation::of(builtins.indent_left.0)
                .with_range(lattice_grammar::Range::Selection),
        ),

        _ => Action::None,
    }
}

fn translate_search(event: KeyEvent) -> Action {
    match event.code {
        KeyCode::Esc => Action::SearchCancel,
        KeyCode::Enter => Action::SearchSubmit,
        KeyCode::Backspace => Action::SearchBackspace,
        KeyCode::Char(c) if !event.modifiers.contains(KeyModifiers::CONTROL) => {
            Action::SearchAppend(c)
        }
        _ => Action::None,
    }
}

fn translate_command(event: KeyEvent, completion_open: bool, _chord_capture: bool) -> Action {
    // Note: chord-capture is dispatched at the top-level
    // `translate()` (so it precedes the universal Ctrl-C quit).
    // This signature still takes the bit so call sites stay
    // explicit, but if we reach here the overlay is off.

    // The completion popup claims a small set of keys first
    // (Tab / S-Tab / Enter / Esc) -- two-stage Esc per DESIGN.md
    // §5.11.3 Q6: first Esc dismisses the popup, second cancels
    // the command line. Other keys fall through; appending text
    // implicitly dismisses the popup (the App handler clears
    // `completion_state` on every typed char).
    if completion_open {
        match event.code {
            KeyCode::Tab => return Action::CommandLineCompleteOrAdvance,
            KeyCode::BackTab => return Action::CommandLineCompletePrev,
            KeyCode::Enter => return Action::CommandLineAcceptCompletion,
            KeyCode::Esc => return Action::CommandLineDismissCompletion,
            _ => {}
        }
    }

    if event.modifiers.contains(KeyModifiers::CONTROL) {
        return match event.code {
            KeyCode::Char('h') => Action::CommandLineDescribeUnderCursor,
            KeyCode::Char('u') => Action::CommandLineClear,
            KeyCode::Char('w') => Action::CommandLineDeleteWordBackward,
            _ => Action::None,
        };
    }

    match event.code {
        KeyCode::Esc => Action::CommandLineCancel,
        KeyCode::Enter => Action::CommandLineSubmit,
        KeyCode::Backspace => Action::CommandLineBackspace,
        KeyCode::Tab => Action::CommandLineCompleteOrAdvance,
        KeyCode::BackTab => Action::CommandLineCompletePrev,
        KeyCode::Up => Action::CommandLineHistoryPrev,
        KeyCode::Down => Action::CommandLineHistoryNext,
        KeyCode::Char(c) if !event.modifiers.contains(KeyModifiers::CONTROL) => {
            Action::CommandLineAppend(c)
        }
        _ => Action::None,
    }
}

/// Cmdline chord-capture overlay. Reserves the three minimal
/// edits (Esc/CR/BS); everything else flows through
/// `format_chord` and becomes one chord token in the cmdline.
fn translate_command_chord_capture(event: KeyEvent) -> Action {
    // Reserved keys -- these never become chord tokens because
    // they're how the user finishes / aborts / corrects. To look
    // up `<Esc>` / `<CR>` themselves, use the missing-arg prompt
    // path (`:describe-key<CR>` with no arg) which captures the
    // very next event.
    match event.code {
        KeyCode::Esc => return Action::CommandLineCancel,
        KeyCode::Enter => return Action::CommandLineSubmit,
        KeyCode::Backspace => return Action::CommandLineDeleteChord,
        _ => {}
    }
    match crate::chord::format_chord(&event) {
        Some(token) => Action::CommandLineAppendChord(token),
        // Release events / modifier-only presses don't have a
        // chord representation -- swallow them silently.
        None => Action::None,
    }
}

/// Picker-overlay key router. See [`crate::picker::Picker`] for
/// the data shape. Reserved keys (Esc / CR / BS / arrows /
/// Ctrl-{n,p,c}) drive the picker's intrinsic actions; printable
/// chars append to the query; everything else is swallowed.
fn translate_picker(event: KeyEvent) -> Action {
    if event.modifiers.contains(KeyModifiers::CONTROL) {
        return match event.code {
            // C-c dismisses the picker (not the app) so the user
            // can always abort.
            KeyCode::Char('c') => Action::PickerDismiss,
            KeyCode::Char('n') => Action::PickerSelectNext,
            KeyCode::Char('p') => Action::PickerSelectPrev,
            // C-u clears the query in one stroke (vim's cmdline
            // shortcut, applied here for consistency).
            KeyCode::Char('u') => Action::PickerBackspace, // approximate; per-char today
            _ => Action::None,
        };
    }
    match event.code {
        KeyCode::Esc => Action::PickerDismiss,
        KeyCode::Enter => Action::PickerAccept,
        KeyCode::Backspace => Action::PickerBackspace,
        KeyCode::Up => Action::PickerSelectPrev,
        KeyCode::Down => Action::PickerSelectNext,
        KeyCode::Tab => Action::PickerSelectNext,
        KeyCode::BackTab => Action::PickerSelectPrev,
        KeyCode::Char(c) => Action::PickerAppend(c),
        _ => Action::None,
    }
}

fn translate_insert(event: KeyEvent, pending: Pending) -> Action {
    // Resolve `<C-x>...` chord first. The previous keystroke
    // armed `Pending::AfterCtrlX`; this one picks the family
    // entry (`<C-o>` -> completion, future: `<C-s>` snippet,
    // `<C-f>` filename, ...).
    if matches!(pending, Pending::AfterCtrlX) {
        if event.modifiers.contains(KeyModifiers::CONTROL) {
            return match event.code {
                KeyCode::Char('o') => Action::CompletionTrigger,
                // `<C-x><C-s>` -- direct snippet expansion
                // (Phase 4.2.g.4). Looks up the word at the
                // cursor in the snippet registry and expands
                // it without surfacing the popup.
                KeyCode::Char('s') => Action::SnippetExpand,
                // Unrecognised follow-up: drop the pending
                // state and let the user retry.
                _ => Action::SetPending(Pending::None),
            };
        }
        return Action::SetPending(Pending::None);
    }
    // `<C-Space>` -- modern-editor manual completion trigger.
    // Crossterm reports Space-with-Ctrl as `Char(' ')` with the
    // CONTROL modifier set.
    if event.modifiers.contains(KeyModifiers::CONTROL)
        && matches!(event.code, KeyCode::Char(' '))
    {
        return Action::CompletionTrigger;
    }
    // `<C-x>` -- vim's "expansion-prefix." Today only routes to
    // `<C-x><C-o>` (omni-completion); future phases add
    // `<C-x><C-s>` (snippet expand) etc. Stash a pending state
    // for the next key.
    if event.modifiers.contains(KeyModifiers::CONTROL)
        && matches!(event.code, KeyCode::Char('x'))
    {
        return Action::SetPending(Pending::AfterCtrlX);
    }
    match event.code {
        KeyCode::Esc => Action::EnterMode(ModalState::Normal),
        KeyCode::Backspace => Action::DeleteCharBackward,
        KeyCode::Enter => Action::Insert("\n".into()),
        KeyCode::Tab => Action::Insert("\t".into()),
        KeyCode::Char(c) if !event.modifiers.contains(KeyModifiers::CONTROL) => {
            Action::Insert(c.to_string())
        }
        _ => Action::None,
    }
}

/// Completion-popup minor-mode keymap (Phase 4.2.g.1). Returns
/// `Some(Action)` when a key is claimed by the layer; `None`
/// when the host should fall through to Insert-mode handling.
/// Overrides Insert-mode + Normal-mode defaults for the popup's
/// lifetime; closing the popup deactivates this layer entirely.
///
/// See `docs/insert-completion.md` §6.1 for the full default
/// keymap.
fn translate_insert_completion_popup(event: KeyEvent) -> Option<Action> {
    let ctrl = event.modifiers.contains(KeyModifiers::CONTROL);
    match (event.code, ctrl) {
        // Navigation -- `<C-n>` / `<C-p>` / `<Down>` / `<Up>`.
        (KeyCode::Char('n'), true) | (KeyCode::Down, _) => Some(Action::CompletionNext),
        (KeyCode::Char('p'), true) | (KeyCode::Up, _) => Some(Action::CompletionPrev),
        // Accept -- `<C-y>` / `<Tab>` / `<CR>`.
        (KeyCode::Char('y'), true) => Some(Action::CompletionAccept),
        (KeyCode::Tab, _) => Some(Action::CompletionAccept),
        (KeyCode::Enter, _) => Some(Action::CompletionAccept),
        // Cancel-but-stay-in-Insert -- `<C-e>` (vim convention).
        (KeyCode::Char('e'), true) => Some(Action::CompletionCancel),
        // Cancel + exit Insert -- `<Esc>`.
        (KeyCode::Esc, _) => Some(Action::CompletionCancelAndExitInsert),
        // Re-trigger (LSP `isIncomplete` refresh path) --
        // `<C-Space>`.
        (KeyCode::Char(' '), true) => Some(Action::CompletionTrigger),
        // Toggle docs side popup -- `<C-d>`.
        (KeyCode::Char('d'), true) => Some(Action::CompletionToggleDocs),
        // Page the docs popup -- `<C-f>` / `<C-b>`. The host
        // ignores these when the docs popup isn't open, so
        // they can claim the chord unconditionally inside the
        // minor mode without breaking other features.
        (KeyCode::Char('f'), true) => Some(Action::CompletionDocsScrollDown),
        (KeyCode::Char('b'), true) => Some(Action::CompletionDocsScrollUp),
        // Commit-char polish (Phase 4.2.g.7). Single
        // unmodified character keys route through
        // `CompletionAcceptThenInsert`; the App's handler
        // checks the focused candidate's effective commit-
        // char set and either accepts-then-inserts or just
        // inserts (popup refilters as if this layer had
        // returned `None`). Routing every char through one
        // action keeps the input layer ignorant of the
        // App-side commit-char state.
        (KeyCode::Char(c), false) if !c.is_control() => {
            Some(Action::CompletionAcceptThenInsert(c))
        }
        // `<C-x><C-o>` etc. fall through to translate_insert,
        // which sets the AfterCtrlX pending state and the next
        // key resolves in Insert mode -- inside the popup we
        // accept that key as a re-trigger via the `<C-Space>`
        // path.
        _ => None,
    }
}

/// Active-snippet minor-mode keymap (Phase 4.2.g.4). Returns
/// `Some(Action)` when the key is claimed by the layer; `None`
/// when the host should fall through to Insert-mode handling
/// so the user can keep typing inside the placeholder.
///
/// Claims:
/// - `<Tab>` -> jump to next placeholder
/// - `<S-Tab>` -> jump to previous placeholder
/// - `<Esc>` -> exit the snippet (returns to Normal mode like
///   plain Insert; placeholders become plain text)
fn translate_active_snippet(event: KeyEvent) -> Option<Action> {
    match event.code {
        KeyCode::Tab if !event.modifiers.contains(KeyModifiers::SHIFT) => {
            Some(Action::SnippetNextPlaceholder)
        }
        KeyCode::BackTab => Some(Action::SnippetPrevPlaceholder),
        KeyCode::Tab if event.modifiers.contains(KeyModifiers::SHIFT) => {
            Some(Action::SnippetPrevPlaceholder)
        }
        KeyCode::Esc => Some(Action::SnippetLeave),
        _ => None,
    }
}

fn translate_normal(
    event: KeyEvent,
    pending: Pending,
    builtins: &Builtins,
    pending_count: u32,
    recording_macro: bool,
) -> Action {
    // Resolve any pending state first.
    match pending {
        Pending::AfterCtrlW => return resolve_after_ctrl_w(event),
        Pending::AfterG => return resolve_after_g(event, builtins),
        Pending::AfterOperator(op) => return resolve_after_operator(event, builtins, op),
        Pending::AfterFindChar { kind, operator } => {
            return resolve_after_find_char(event, builtins, kind, operator);
        }
        Pending::AfterTextObject { operator, around } => {
            return resolve_after_text_object(event, builtins, operator, around);
        }
        Pending::AfterZ => return resolve_after_z(event),
        Pending::AfterSetMark => return resolve_after_set_mark(event),
        Pending::AfterJumpMarkLine => return resolve_after_jump_mark(event, false),
        Pending::AfterJumpMarkExact => return resolve_after_jump_mark(event, true),
        Pending::AfterRegister => return resolve_after_register(event),
        Pending::AfterMacroStart => return resolve_after_macro_start(event),
        Pending::AfterMacroPlay => return resolve_after_macro_play(event),
        // Insert-mode-only pending (Phase 4.2.g.1). If we
        // somehow end up in Normal mode with this pending
        // state, drop it -- the chord doesn't have a Normal-
        // mode meaning.
        Pending::AfterCtrlX => return Action::SetPending(Pending::None),
        Pending::None => {}
    }

    if event.modifiers.contains(KeyModifiers::CONTROL) {
        return match event.code {
            KeyCode::Char('d') => invoke_with_count(builtins.line_down, 10),
            KeyCode::Char('u') => invoke_with_count(builtins.line_up, 10),
            KeyCode::Char('f') => Action::PageDown,
            KeyCode::Char('b') => Action::PageUp,
            KeyCode::Char('e') => Action::ScrollLineDown,
            KeyCode::Char('y') => Action::ScrollLineUp,
            KeyCode::Char('r') => Action::Redo,
            KeyCode::Char('o') => Action::JumpHistoryBack,
            KeyCode::Char('i') => Action::JumpHistoryForward,
            // `<C-t>` -- pop the tag stack (vim's `:pop`). The
            // tag stack is the LIFO chain of `gd` / `gD` / `gy`
            // / `gI` drill-downs, distinct from `<C-o>`'s jump
            // list. Both walks coexist with different push
            // semantics.
            KeyCode::Char('t') => Action::TagStackPop,
            // `<C-l>` -- vim's "redraw screen" key. Reparses syntax
            // and tells the runtime to clear the terminal so any
            // visual glitch (stale highlight cache, leftover ANSI
            // from a crashed sub-process, terminal-resize race)
            // gets repainted from scratch.
            KeyCode::Char('l') => Action::RedrawScreen,
            // Ctrl+V and Ctrl+Q both enter blockwise Visual. Vim binds
            // both for the same reason: many terminals (Konsole, Windows
            // Terminal, tmux paste-key) hijack Ctrl+V for clipboard
            // paste before it reaches us. Ctrl+Q is the universal
            // fallback. We also enable bracketed paste in `runtime.rs`,
            // so a hijacked Ctrl+V arrives as `Event::Paste` -- the
            // user's paste still works either way.
            KeyCode::Char('v') | KeyCode::Char('q') => Action::EnterVisual(VisualKind::Blockwise),
            // `<C-w>` -- window-management chord prefix. The next
            // key resolves to split / close / navigate (see
            // `resolve_after_ctrl_w`).
            KeyCode::Char('w') => Action::SetPending(Pending::AfterCtrlW),
            _ => Action::None,
        };
    }
    // `Tab` (without Ctrl) is conventionally Ctrl-I in vim's jump-list,
    // since terminals encode Tab as Ctrl-I. Bind both.
    if matches!(event.code, KeyCode::Tab) && event.modifiers.is_empty() {
        return Action::JumpHistoryForward;
    }

    // Numeric prefix: `1`-`9` always start (or extend) a count; `0` extends
    // an in-progress count but otherwise is line_start. This is vim's
    // standard count parsing, exactly.
    if let KeyCode::Char(c) = event.code
        && let Some(digit) = c.to_digit(10)
        && (digit > 0 || pending_count > 0)
    {
        return Action::PushDigit(digit as u8);
    }

    match event.code {
        // Macro record control. `q` while recording stops; otherwise it
        // pends for a register-name char.
        KeyCode::Char('q') if recording_macro => Action::StopMacroRecord,
        KeyCode::Char('q') => Action::SetPending(Pending::AfterMacroStart),
        // Macro play. `@@` is "repeat last"; everything else needs a
        // register-name follow-up.
        KeyCode::Char('@') => Action::SetPending(Pending::AfterMacroPlay),

        // Motions
        KeyCode::Char('h') | KeyCode::Left => invoke(builtins.char_left),
        KeyCode::Char('j') | KeyCode::Down => invoke(builtins.line_down),
        KeyCode::Char('k') | KeyCode::Up => invoke(builtins.line_up),
        KeyCode::Char('l') | KeyCode::Right => invoke(builtins.char_right),
        KeyCode::Char('0') | KeyCode::Home => invoke(builtins.line_start),
        KeyCode::Char('$') | KeyCode::End => invoke(builtins.line_end),
        KeyCode::Char('^') => invoke(builtins.first_non_blank),
        KeyCode::Char('w') => invoke(builtins.word_forward),
        KeyCode::Char('b') => invoke(builtins.word_backward),
        KeyCode::Char('e') => invoke(builtins.word_end),
        KeyCode::Char('W') => invoke(builtins.big_word_forward),
        KeyCode::Char('B') => invoke(builtins.big_word_backward),
        KeyCode::Char('E') => invoke(builtins.big_word_end),
        KeyCode::Char('}') => invoke(builtins.paragraph_forward),
        KeyCode::Char('{') => invoke(builtins.paragraph_backward),
        KeyCode::Char(')') => invoke(builtins.sentence_forward),
        KeyCode::Char('(') => invoke(builtins.sentence_backward),
        KeyCode::Char('G') => invoke(builtins.goto_last_line),

        // Viewport jumps
        KeyCode::Char('H') => Action::JumpViewport(ViewportPos::Top),
        KeyCode::Char('M') => Action::JumpViewport(ViewportPos::Middle),
        KeyCode::Char('L') => Action::JumpViewport(ViewportPos::Bottom),

        // Pending key sequences
        KeyCode::Char('g') => Action::SetPending(Pending::AfterG),
        KeyCode::Char('z') => Action::SetPending(Pending::AfterZ),

        // Operator-leading keys
        KeyCode::Char('d') => Action::SetPending(Pending::AfterOperator(builtins.delete)),
        KeyCode::Char('c') => Action::SetPending(Pending::AfterOperator(builtins.change)),
        KeyCode::Char('y') => Action::SetPending(Pending::AfterOperator(builtins.yank)),
        KeyCode::Char('>') => Action::SetPending(Pending::AfterOperator(builtins.indent_right)),
        KeyCode::Char('<') => Action::SetPending(Pending::AfterOperator(builtins.indent_left)),

        // Paste
        KeyCode::Char('p') => Action::PasteAfter,
        KeyCode::Char('P') => Action::PasteBefore,

        // Linewise yank shortcut: `Y` is equivalent to `yy` in vim's defaults.
        KeyCode::Char('Y') => Action::Invoke(
            CommandInvocation::of(builtins.yank.0).with_range(lattice_grammar::Range::CurrentLine),
        ),

        // Vim's `x` -- delete one char to the right.
        KeyCode::Char('x') => Action::Invoke(
            CommandInvocation::of(builtins.delete.0)
                .with_target(Target::Motion(builtins.char_right, Args::None)),
        ),

        // `D` = `d$`, `C` = `c$`, `S` = `cc` (substitute line).
        KeyCode::Char('D') => Action::Invoke(
            CommandInvocation::of(builtins.delete.0)
                .with_target(Target::Motion(builtins.line_end, Args::None)),
        ),
        KeyCode::Char('C') => Action::Invoke(
            CommandInvocation::of(builtins.change.0)
                .with_target(Target::Motion(builtins.line_end, Args::None)),
        ),
        KeyCode::Char('S') => Action::Invoke(
            CommandInvocation::of(builtins.change.0)
                .with_range(lattice_grammar::Range::CurrentLine),
        ),

        // Line join.
        KeyCode::Char('J') => Action::JoinLines { with_space: true },

        // Find-repeat (`;` keeps direction; `,` reverses).
        KeyCode::Char(';') => Action::FindRepeat { reverse: false },
        KeyCode::Char(',') => Action::FindRepeat { reverse: true },

        // Mode entry
        KeyCode::Char('i') => Action::EnterMode(ModalState::Insert),
        KeyCode::Char('a') => Action::EnterAppend,
        KeyCode::Char('o') => Action::OpenLineBelow,
        KeyCode::Char('O') => Action::OpenLineAbove,
        KeyCode::Char(':') => Action::EnterCommandLine,
        KeyCode::Char('v') => Action::EnterVisual(VisualKind::Charwise),
        KeyCode::Char('V') => Action::EnterVisual(VisualKind::Linewise),
        KeyCode::Char('R') => Action::EnterMode(ModalState::Replace),

        // Toggle case at cursor
        KeyCode::Char('~') => Action::ToggleCaseAtCursor,

        // LSP hover at cursor (Phase 4.2.b). Vim's `K` traditionally
        // runs `keywordprg` (man-page lookup); we repurpose it for
        // textDocument/hover to surface the symbol's docs without
        // leaving the buffer. Cancellation rides on motion / mode
        // change so a slow server can't drop a stale popup over a
        // moved cursor.
        KeyCode::Char('K') => Action::LspHoverRequest,

        // Search
        KeyCode::Char('/') => Action::EnterSearch(SearchDirection::Forward),
        KeyCode::Char('?') => Action::EnterSearch(SearchDirection::Backward),
        KeyCode::Char('n') => Action::SearchNext,
        KeyCode::Char('N') => Action::SearchPrevious,
        KeyCode::Char('*') => Action::SearchWordUnderCursor(SearchDirection::Forward),
        KeyCode::Char('#') => Action::SearchWordUnderCursor(SearchDirection::Backward),
        KeyCode::Char('%') => Action::MatchBracket,

        // Find-char on the current line
        KeyCode::Char('f') => Action::SetPending(Pending::AfterFindChar {
            kind: FindKind::Forward,
            operator: None,
        }),
        KeyCode::Char('F') => Action::SetPending(Pending::AfterFindChar {
            kind: FindKind::Backward,
            operator: None,
        }),
        KeyCode::Char('t') => Action::SetPending(Pending::AfterFindChar {
            kind: FindKind::TillForward,
            operator: None,
        }),
        KeyCode::Char('T') => Action::SetPending(Pending::AfterFindChar {
            kind: FindKind::TillBackward,
            operator: None,
        }),

        // Undo
        KeyCode::Char('u') => Action::Undo,

        // Dot-repeat
        KeyCode::Char('.') => Action::RepeatLastChange,

        // Register prefix: `"<reg>` selects the register for the next
        // operator or paste.
        KeyCode::Char('"') => Action::SetPending(Pending::AfterRegister),

        // Marks
        KeyCode::Char('m') => Action::SetPending(Pending::AfterSetMark),
        KeyCode::Char('\'') => Action::SetPending(Pending::AfterJumpMarkLine),
        KeyCode::Char('`') => Action::SetPending(Pending::AfterJumpMarkExact),

        // Paging
        KeyCode::PageDown => invoke_with_count(builtins.line_down, 10),
        KeyCode::PageUp => invoke_with_count(builtins.line_up, 10),

        _ => Action::None,
    }
}

/// Resolve the second key of a `<C-w>...` window-management chord
/// (DESIGN.md §5.9). vim keymap:
/// - `<C-w>s` / `<C-w>S` -- horizontal split (new pane below).
/// - `<C-w>v` -- vertical split (new pane right).
/// - `<C-w>c` / `<C-w>q` -- close active pane.
/// - `<C-w>h/j/k/l` -- navigate to spatial neighbour.
/// - `<C-w>w` / `<C-w><C-w>` -- cycle to next pane.
/// - `<C-w>W` -- cycle to previous pane.
/// - Anything else: clear pending and no-op.
fn resolve_after_ctrl_w(event: KeyEvent) -> Action {
    if matches!(event.code, KeyCode::Esc) {
        return Action::SetPending(Pending::None);
    }
    // Within the AfterCtrlW chord we accept either Ctrl-modified or
    // bare keys -- vim is lenient because <C-w> is a sticky prefix
    // (typing <C-w><C-w> for "next pane" is muscle memory).
    if event.modifiers.contains(KeyModifiers::CONTROL) {
        // Vim accepts Ctrl-modified second keys after `<C-w>` so
        // the user can hold Ctrl through the whole chord
        // (`<C-w><C-l>` and `<C-w>l` both navigate right). Many
        // terminals collapse `<C-h>` to Backspace and `<C-i>` to
        // Tab; we honour those mappings via the bare-key paths
        // below.
        return match event.code {
            KeyCode::Char('w') => Action::NextPane,
            KeyCode::Char('h') => Action::NavigatePane(PaneDirection::Left),
            KeyCode::Char('j') => Action::NavigatePane(PaneDirection::Down),
            KeyCode::Char('k') => Action::NavigatePane(PaneDirection::Up),
            KeyCode::Char('l') => Action::NavigatePane(PaneDirection::Right),
            KeyCode::Char('s') => Action::SplitPaneHorizontal,
            KeyCode::Char('v') => Action::SplitPaneVertical,
            KeyCode::Char('c') | KeyCode::Char('q') => Action::ClosePane,
            _ => Action::SetPending(Pending::None),
        };
    }
    match event.code {
        KeyCode::Char('s') | KeyCode::Char('S') => Action::SplitPaneHorizontal,
        KeyCode::Char('v') => Action::SplitPaneVertical,
        KeyCode::Char('c') | KeyCode::Char('q') => Action::ClosePane,
        KeyCode::Char('h') | KeyCode::Left | KeyCode::Backspace => {
            Action::NavigatePane(PaneDirection::Left)
        }
        KeyCode::Char('j') | KeyCode::Down => Action::NavigatePane(PaneDirection::Down),
        KeyCode::Char('k') | KeyCode::Up => Action::NavigatePane(PaneDirection::Up),
        KeyCode::Char('l') | KeyCode::Right => Action::NavigatePane(PaneDirection::Right),
        KeyCode::Char('w') | KeyCode::Tab => Action::NextPane,
        KeyCode::Char('W') | KeyCode::BackTab => Action::PrevPane,
        _ => Action::SetPending(Pending::None),
    }
}

fn resolve_after_g(event: KeyEvent, builtins: &Builtins) -> Action {
    match event.code {
        // `gg`: jump to first line.
        KeyCode::Char('g') => invoke(builtins.goto_first_line),
        // Case operators: `gU`/`gu`/`g~` enter operator-pending state with
        // the corresponding operator latched; the next key supplies the
        // motion or text-object target. `gUU`/`guu`/`g~~` operate on the
        // current line.
        KeyCode::Char('U') => Action::SetPending(Pending::AfterOperator(builtins.upper)),
        KeyCode::Char('u') => Action::SetPending(Pending::AfterOperator(builtins.lower)),
        KeyCode::Char('~') => Action::SetPending(Pending::AfterOperator(builtins.toggle_case)),
        // `gv`: reselect the last Visual selection.
        KeyCode::Char('v') => Action::ReselectLastVisual,
        // `gJ`: join lines without inserting a space.
        KeyCode::Char('J') => Action::JoinLines { with_space: false },
        // `g;` / `g,`: walk named-mark history.
        KeyCode::Char(';') => Action::WalkMarkHistoryBack,
        KeyCode::Char(',') => Action::WalkMarkHistoryForward,
        // `gd` (Phase 4.2.c): textDocument/definition. Walks every
        // attached LSP server's response, concats + dedups by
        // (uri, range). Single result jumps in-place; multiple
        // results open a `*lsp:definitions*` picker.
        KeyCode::Char('d') => Action::LspDefinitionRequest,
        // `gD`: textDocument/declaration (forward declaration /
        // header pointer in C-family, `extern` in Rust).
        KeyCode::Char('D') => Action::LspDeclarationRequest,
        // `gy`: textDocument/typeDefinition (the type of the
        // expression under the cursor).
        KeyCode::Char('y') => Action::LspTypeDefinitionRequest,
        // `gI` (capital): textDocument/implementation. Lowercase
        // `gi` belongs to vim's "go to last insert position" --
        // not yet wired.
        KeyCode::Char('I') => Action::LspImplementationRequest,
        // `gr`: textDocument/references. Opens a buffer-backed
        // list view of every call site. Vim's default `gr`
        // (virtual replace one char) isn't currently bound, so
        // no conflict.
        KeyCode::Char('r') => Action::LspReferencesRequest,
        _ => Action::SetPending(Pending::None),
    }
}

fn resolve_after_operator(event: KeyEvent, builtins: &Builtins, op: OperatorId) -> Action {
    if matches!(event.code, KeyCode::Esc) {
        return Action::SetPending(Pending::None);
    }
    // For now, only recognize a small set of motions as targets. Doubled
    // operator (e.g., `dd`) is a special case that maps to `Range::CurrentLine`.
    let target = match event.code {
        KeyCode::Char('w') => Target::Motion(builtins.word_forward, Args::None),
        KeyCode::Char('b') => Target::Motion(builtins.word_backward, Args::None),
        KeyCode::Char('e') => Target::Motion(builtins.word_end, Args::None),
        KeyCode::Char('W') => Target::Motion(builtins.big_word_forward, Args::None),
        KeyCode::Char('B') => Target::Motion(builtins.big_word_backward, Args::None),
        KeyCode::Char('E') => Target::Motion(builtins.big_word_end, Args::None),
        KeyCode::Char('}') => Target::Motion(builtins.paragraph_forward, Args::None),
        KeyCode::Char('{') => Target::Motion(builtins.paragraph_backward, Args::None),
        KeyCode::Char(')') => Target::Motion(builtins.sentence_forward, Args::None),
        KeyCode::Char('(') => Target::Motion(builtins.sentence_backward, Args::None),
        KeyCode::Char('h') | KeyCode::Left => Target::Motion(builtins.char_left, Args::None),
        KeyCode::Char('l') | KeyCode::Right => Target::Motion(builtins.char_right, Args::None),
        KeyCode::Char('j') | KeyCode::Down => Target::Motion(builtins.line_down, Args::None),
        KeyCode::Char('k') | KeyCode::Up => Target::Motion(builtins.line_up, Args::None),
        KeyCode::Char('0') | KeyCode::Home => Target::Motion(builtins.line_start, Args::None),
        KeyCode::Char('$') | KeyCode::End => Target::Motion(builtins.line_end, Args::None),
        KeyCode::Char('^') => Target::Motion(builtins.first_non_blank, Args::None),
        KeyCode::Char('d') if op == builtins.delete => {
            // `dd` -- delete current line. The dispatcher's CurrentLine range
            // covers the line content; the trailing newline is a known
            // limitation tracked in DESIGN.md §14 for proper linewise vim
            // semantics.
            return Action::Invoke(
                CommandInvocation::of(op.0).with_range(lattice_grammar::Range::CurrentLine),
            );
        }
        KeyCode::Char('c') if op == builtins.change => {
            // `cc` -- change current line: clear the line content and enter
            // Insert (the `change` operator handles the mode transition).
            return Action::Invoke(
                CommandInvocation::of(op.0).with_range(lattice_grammar::Range::CurrentLine),
            );
        }
        KeyCode::Char('y') if op == builtins.yank => {
            // `yy` -- yank current line into the unnamed register (linewise).
            return Action::Invoke(
                CommandInvocation::of(op.0).with_range(lattice_grammar::Range::CurrentLine),
            );
        }
        KeyCode::Char('>') if op == builtins.indent_right => {
            return Action::Invoke(
                CommandInvocation::of(op.0).with_range(lattice_grammar::Range::CurrentLine),
            );
        }
        KeyCode::Char('<') if op == builtins.indent_left => {
            return Action::Invoke(
                CommandInvocation::of(op.0).with_range(lattice_grammar::Range::CurrentLine),
            );
        }
        KeyCode::Char('U') if op == builtins.upper => {
            return Action::Invoke(
                CommandInvocation::of(op.0).with_range(lattice_grammar::Range::CurrentLine),
            );
        }
        KeyCode::Char('u') if op == builtins.lower => {
            return Action::Invoke(
                CommandInvocation::of(op.0).with_range(lattice_grammar::Range::CurrentLine),
            );
        }
        KeyCode::Char('~') if op == builtins.toggle_case => {
            return Action::Invoke(
                CommandInvocation::of(op.0).with_range(lattice_grammar::Range::CurrentLine),
            );
        }
        KeyCode::Char('f') => {
            return Action::SetPending(Pending::AfterFindChar {
                kind: FindKind::Forward,
                operator: Some(op),
            });
        }
        KeyCode::Char('F') => {
            return Action::SetPending(Pending::AfterFindChar {
                kind: FindKind::Backward,
                operator: Some(op),
            });
        }
        KeyCode::Char('t') => {
            return Action::SetPending(Pending::AfterFindChar {
                kind: FindKind::TillForward,
                operator: Some(op),
            });
        }
        KeyCode::Char('T') => {
            return Action::SetPending(Pending::AfterFindChar {
                kind: FindKind::TillBackward,
                operator: Some(op),
            });
        }
        KeyCode::Char('i') => {
            return Action::SetPending(Pending::AfterTextObject {
                operator: op,
                around: false,
            });
        }
        KeyCode::Char('a') => {
            return Action::SetPending(Pending::AfterTextObject {
                operator: op,
                around: true,
            });
        }
        _ => return Action::SetPending(Pending::None),
    };
    Action::Invoke(CommandInvocation::of(op.0).with_target(target))
}

fn resolve_after_macro_start(event: KeyEvent) -> Action {
    if matches!(event.code, KeyCode::Esc) {
        return Action::SetPending(Pending::None);
    }
    match event.code {
        KeyCode::Char(c) if c.is_ascii_alphanumeric() => Action::StartMacroRecord(c),
        _ => Action::SetPending(Pending::None),
    }
}

fn resolve_after_macro_play(event: KeyEvent) -> Action {
    if matches!(event.code, KeyCode::Esc) {
        return Action::SetPending(Pending::None);
    }
    match event.code {
        KeyCode::Char('@') => Action::PlayLastMacro,
        KeyCode::Char(c) if c.is_ascii_alphanumeric() => Action::PlayMacro(c),
        _ => Action::SetPending(Pending::None),
    }
}

fn resolve_after_register(event: KeyEvent) -> Action {
    if matches!(event.code, KeyCode::Esc) {
        return Action::SetPending(Pending::None);
    }
    let c = match event.code {
        KeyCode::Char(c) => c,
        _ => return Action::SetPending(Pending::None),
    };
    let reg = match c {
        'a'..='z' | 'A'..='Z' => Register::Named(c),
        '0'..='9' => Register::Numbered((c as u8) - b'0'),
        '"' => Register::Unnamed,
        '_' => Register::BlackHole,
        '+' | '*' => Register::System,
        _ => return Action::SetPending(Pending::None),
    };
    Action::SelectRegister(reg)
}

fn resolve_after_z(event: KeyEvent) -> Action {
    if matches!(event.code, KeyCode::Esc) {
        return Action::SetPending(Pending::None);
    }
    match event.code {
        KeyCode::Char('z') | KeyCode::Char('.') => Action::ScrollCursorTo(ScrollPos::Center),
        KeyCode::Char('t') | KeyCode::Enter => Action::ScrollCursorTo(ScrollPos::Top),
        KeyCode::Char('b') | KeyCode::Char('-') => Action::ScrollCursorTo(ScrollPos::Bottom),
        // Folds.
        KeyCode::Char('f') => Action::CreateFoldFromVisual,
        KeyCode::Char('o') => Action::OpenFoldAtCursor,
        KeyCode::Char('c') => Action::CloseFoldAtCursor,
        KeyCode::Char('a') => Action::ToggleFoldAtCursor,
        KeyCode::Char('R') => Action::OpenAllFolds,
        KeyCode::Char('M') => Action::CloseAllFolds,
        KeyCode::Char('d') => Action::DeleteFoldAtCursor,
        KeyCode::Char('j') => Action::GotoNextFold,
        KeyCode::Char('k') => Action::GotoPrevFold,
        KeyCode::Char('i') => Action::ToggleFoldEnable,
        _ => Action::SetPending(Pending::None),
    }
}

fn resolve_after_set_mark(event: KeyEvent) -> Action {
    if matches!(event.code, KeyCode::Esc) {
        return Action::SetPending(Pending::None);
    }
    match event.code {
        KeyCode::Char(c) if c.is_ascii_alphanumeric() => Action::SetMark(c),
        _ => Action::SetPending(Pending::None),
    }
}

fn resolve_after_jump_mark(event: KeyEvent, exact: bool) -> Action {
    if matches!(event.code, KeyCode::Esc) {
        return Action::SetPending(Pending::None);
    }
    match event.code {
        KeyCode::Char(c) if c.is_ascii_alphanumeric() => {
            if exact {
                Action::JumpToMarkExact(c)
            } else {
                Action::JumpToMarkLine(c)
            }
        }
        _ => Action::SetPending(Pending::None),
    }
}

fn resolve_after_text_object(
    event: KeyEvent,
    builtins: &Builtins,
    operator: OperatorId,
    around: bool,
) -> Action {
    if matches!(event.code, KeyCode::Esc) {
        return Action::SetPending(Pending::None);
    }
    let tobj = match event.code {
        KeyCode::Char('w') => {
            if around {
                builtins.around_word
            } else {
                builtins.inner_word
            }
        }
        KeyCode::Char('W') => {
            if around {
                builtins.around_big_word
            } else {
                builtins.inner_big_word
            }
        }
        KeyCode::Char('p') => {
            if around {
                builtins.around_paragraph
            } else {
                builtins.inner_paragraph
            }
        }
        KeyCode::Char('s') => {
            if around {
                builtins.around_sentence
            } else {
                builtins.inner_sentence
            }
        }
        KeyCode::Char('t') => {
            if around {
                builtins.around_tag
            } else {
                builtins.inner_tag
            }
        }
        KeyCode::Char('"') => {
            if around {
                builtins.around_quote_double
            } else {
                builtins.inner_quote_double
            }
        }
        KeyCode::Char('\'') => {
            if around {
                builtins.around_quote_single
            } else {
                builtins.inner_quote_single
            }
        }
        KeyCode::Char('`') => {
            if around {
                builtins.around_quote_backtick
            } else {
                builtins.inner_quote_backtick
            }
        }
        KeyCode::Char('(') | KeyCode::Char(')') | KeyCode::Char('b') => {
            if around {
                builtins.around_paren
            } else {
                builtins.inner_paren
            }
        }
        KeyCode::Char('[') | KeyCode::Char(']') => {
            if around {
                builtins.around_bracket
            } else {
                builtins.inner_bracket
            }
        }
        KeyCode::Char('{') | KeyCode::Char('}') | KeyCode::Char('B') => {
            if around {
                builtins.around_brace
            } else {
                builtins.inner_brace
            }
        }
        KeyCode::Char('<') | KeyCode::Char('>') => {
            if around {
                builtins.around_angle
            } else {
                builtins.inner_angle
            }
        }
        _ => return Action::SetPending(Pending::None),
    };
    Action::Invoke(
        CommandInvocation::of(operator.0).with_target(Target::TextObject(tobj, Args::None)),
    )
}

fn resolve_after_find_char(
    event: KeyEvent,
    builtins: &Builtins,
    kind: FindKind,
    operator: Option<OperatorId>,
) -> Action {
    if matches!(event.code, KeyCode::Esc) {
        return Action::SetPending(Pending::None);
    }
    let needle = match event.code {
        KeyCode::Char(c) => c,
        _ => return Action::SetPending(Pending::None),
    };
    let motion_id = match kind {
        FindKind::Forward => builtins.find_char_forward,
        FindKind::Backward => builtins.find_char_backward,
        FindKind::TillForward => builtins.till_char_forward,
        FindKind::TillBackward => builtins.till_char_backward,
    };
    match operator {
        None => Action::Invoke(CommandInvocation::of(motion_id.0).with_args(Args::Char(needle))),
        Some(op) => Action::Invoke(
            CommandInvocation::of(op.0).with_target(Target::Motion(motion_id, Args::Char(needle))),
        ),
    }
}

fn invoke(motion: MotionId) -> Action {
    Action::Invoke(CommandInvocation::of(motion.0))
}

fn invoke_with_count(motion: MotionId, count: u32) -> Action {
    Action::Invoke(
        CommandInvocation::of(motion.0).with_count(lattice_grammar::command::Count(count)),
    )
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used, clippy::panic)]
    use super::*;
    use lattice_grammar::CommandRegistry;
    use lattice_grammar::builtins::populate;

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn ctrl(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::CONTROL)
    }

    fn fixture() -> (CommandRegistry, Builtins) {
        let mut r = CommandRegistry::new();
        let b = populate(&mut r);
        (r, b)
    }

    fn ctx<'a>(modal: ModalState, pending: Pending, b: &'a Builtins) -> TranslateContext<'a> {
        TranslateContext {
            modal,
            pending,
            builtins: b,
            pending_count: 0,
            recording_macro: false,
            active_buffer: BufferKind::Document,
            completion_open: false,
            chord_capture: false,
            picker_open: false,
            insert_completion_open: false,
            snippet_active: false,
        }
    }

    fn ctx_with_count<'a>(
        modal: ModalState,
        pending: Pending,
        b: &'a Builtins,
        pending_count: u32,
    ) -> TranslateContext<'a> {
        TranslateContext {
            modal,
            pending,
            builtins: b,
            pending_count,
            recording_macro: false,
            active_buffer: BufferKind::Document,
            completion_open: false,
            chord_capture: false,
            picker_open: false,
            insert_completion_open: false,
            snippet_active: false,
        }
    }

    fn ctx_recording<'a>(
        modal: ModalState,
        pending: Pending,
        b: &'a Builtins,
    ) -> TranslateContext<'a> {
        TranslateContext {
            modal,
            pending,
            builtins: b,
            pending_count: 0,
            recording_macro: true,
            active_buffer: BufferKind::Document,
            completion_open: false,
            chord_capture: false,
            picker_open: false,
            insert_completion_open: false,
            snippet_active: false,
        }
    }

    fn ctx_chord_capture<'a>(b: &'a Builtins) -> TranslateContext<'a> {
        TranslateContext {
            modal: ModalState::Command,
            pending: Pending::None,
            builtins: b,
            pending_count: 0,
            recording_macro: false,
            active_buffer: BufferKind::Document,
            completion_open: false,
            chord_capture: true,
            picker_open: false,
            insert_completion_open: false,
            snippet_active: false,
        }
    }

    fn invocation_command(action: &Action) -> Option<lattice_protocol::ids::CommandId> {
        if let Action::Invoke(inv) = action {
            Some(inv.command)
        } else {
            None
        }
    }

    // ---- Universal ----

    #[test]
    fn ctrl_c_quits_in_any_mode() {
        let (_, b) = fixture();
        for modal in [ModalState::Normal, ModalState::Insert] {
            assert!(matches!(
                translate(ctx(modal, Pending::None, &b), ctrl(KeyCode::Char('c'))),
                Action::Quit
            ));
        }
    }

    // ---- Normal mode motions ----

    #[test]
    fn hjkl_invoke_corresponding_motions() {
        let (_, b) = fixture();
        let cases = [
            (KeyCode::Char('h'), b.char_left.0),
            (KeyCode::Char('j'), b.line_down.0),
            (KeyCode::Char('k'), b.line_up.0),
            (KeyCode::Char('l'), b.char_right.0),
        ];
        for (code, expected) in cases {
            let action = translate(ctx(ModalState::Normal, Pending::None, &b), key(code));
            assert_eq!(invocation_command(&action), Some(expected));
        }
    }

    #[test]
    fn arrows_alias_hjkl() {
        let (_, b) = fixture();
        let cases = [
            (KeyCode::Left, b.char_left.0),
            (KeyCode::Down, b.line_down.0),
            (KeyCode::Up, b.line_up.0),
            (KeyCode::Right, b.char_right.0),
        ];
        for (code, expected) in cases {
            let action = translate(ctx(ModalState::Normal, Pending::None, &b), key(code));
            assert_eq!(invocation_command(&action), Some(expected));
        }
    }

    #[test]
    fn zero_and_dollar_invoke_line_start_and_end() {
        let (_, b) = fixture();
        assert_eq!(
            invocation_command(&translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('0'))
            )),
            Some(b.line_start.0)
        );
        assert_eq!(
            invocation_command(&translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('$'))
            )),
            Some(b.line_end.0)
        );
    }

    #[test]
    fn capital_g_invokes_goto_last_line() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('G')),
        );
        assert_eq!(invocation_command(&action), Some(b.goto_last_line.0));
    }

    #[test]
    fn first_g_sets_pending_state() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('g'))
            ),
            Action::SetPending(Pending::AfterG)
        ));
    }

    #[test]
    fn second_g_with_pending_resolves_to_goto_first_line() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterG, &b),
            key(KeyCode::Char('g')),
        );
        assert_eq!(invocation_command(&action), Some(b.goto_first_line.0));
    }

    #[test]
    fn unrelated_key_after_pending_g_clears_pending() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterG, &b),
                key(KeyCode::Char('z'))
            ),
            Action::SetPending(Pending::None)
        ));
    }

    // ---- Mode entry ----

    #[test]
    fn i_enters_insert_mode() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('i'))
            ),
            Action::EnterMode(ModalState::Insert)
        ));
    }

    #[test]
    fn a_enters_append_mode() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('a'))
            ),
            Action::EnterAppend
        ));
    }

    #[test]
    fn o_opens_line_below() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('o'))
            ),
            Action::OpenLineBelow
        ));
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('O'))
            ),
            Action::OpenLineAbove
        ));
    }

    // ---- Operator-pending state ----

    #[test]
    fn d_sets_pending_operator() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('d')),
        );
        match action {
            Action::SetPending(Pending::AfterOperator(op)) => assert_eq!(op, b.delete),
            _ => panic!("expected SetPending(AfterOperator(delete))"),
        }
    }

    #[test]
    fn dw_resolves_to_delete_with_word_forward_target() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.delete), &b),
            key(KeyCode::Char('w')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.delete.0);
                match inv.target {
                    Some(Target::Motion(id, _)) => assert_eq!(id, b.word_forward),
                    other => panic!("expected motion target, got {other:?}"),
                }
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn dd_resolves_to_delete_current_line() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.delete), &b),
            key(KeyCode::Char('d')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.delete.0);
                assert_eq!(inv.range, Some(lattice_grammar::Range::CurrentLine));
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn esc_after_operator_cancels_pending() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterOperator(b.delete), &b),
                key(KeyCode::Esc)
            ),
            Action::SetPending(Pending::None)
        ));
    }

    #[test]
    fn x_resolves_directly_to_delete_char_right() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('x')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.delete.0);
                match inv.target {
                    Some(Target::Motion(id, _)) => assert_eq!(id, b.char_right),
                    other => panic!("expected motion target, got {other:?}"),
                }
            }
            _ => panic!("expected Invoke"),
        }
    }

    // ---- Insert mode ----

    #[test]
    fn esc_in_insert_returns_to_normal() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Insert, Pending::None, &b),
                key(KeyCode::Esc)
            ),
            Action::EnterMode(ModalState::Normal)
        ));
    }

    #[test]
    fn printable_char_in_insert_inserts_text() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Insert, Pending::None, &b),
            key(KeyCode::Char('h')),
        ) {
            Action::Insert(s) => assert_eq!(s, "h"),
            _ => panic!("expected Insert"),
        }
    }

    #[test]
    fn enter_in_insert_inserts_newline() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Insert, Pending::None, &b),
            key(KeyCode::Enter),
        ) {
            Action::Insert(s) => assert_eq!(s, "\n"),
            _ => panic!("expected Insert(\"\\n\")"),
        }
    }

    #[test]
    fn backspace_in_insert_deletes_char_backward() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Insert, Pending::None, &b),
                key(KeyCode::Backspace)
            ),
            Action::DeleteCharBackward
        ));
    }

    // ---- Undo / Redo ----

    #[test]
    fn u_in_normal_undoes() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('u'))
            ),
            Action::Undo
        ));
    }

    #[test]
    fn ctrl_r_in_normal_redoes() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('r'))
            ),
            Action::Redo
        ));
    }

    // ---- Command modal ----

    #[test]
    fn colon_in_normal_enters_command_line() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char(':'))
            ),
            Action::EnterCommandLine
        ));
    }

    #[test]
    fn printable_char_in_command_appends() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Command, Pending::None, &b),
            key(KeyCode::Char('w')),
        ) {
            Action::CommandLineAppend(c) => assert_eq!(c, 'w'),
            other => panic!("expected CommandLineAppend, got {other:?}"),
        }
    }

    #[test]
    fn enter_in_command_submits() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Command, Pending::None, &b),
                key(KeyCode::Enter)
            ),
            Action::CommandLineSubmit
        ));
    }

    #[test]
    fn esc_in_command_cancels() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Command, Pending::None, &b),
                key(KeyCode::Esc)
            ),
            Action::CommandLineCancel
        ));
    }

    #[test]
    fn backspace_in_command_pops() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Command, Pending::None, &b),
                key(KeyCode::Backspace)
            ),
            Action::CommandLineBackspace
        ));
    }

    #[test]
    fn up_in_command_emits_history_prev() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Command, Pending::None, &b),
                key(KeyCode::Up)
            ),
            Action::CommandLineHistoryPrev
        ));
    }

    #[test]
    fn down_in_command_emits_history_next() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Command, Pending::None, &b),
                key(KeyCode::Down)
            ),
            Action::CommandLineHistoryNext
        ));
    }

    #[test]
    fn ctrl_c_in_command_quits_immediately() {
        // Universal ctrl+c quits regardless of mode -- the user shouldn't
        // need to cancel the command line first.
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Command, Pending::None, &b),
                ctrl(KeyCode::Char('c'))
            ),
            Action::Quit
        ));
    }

    // ---- Search modal ----

    #[test]
    fn slash_in_normal_enters_forward_search() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('/')),
        ) {
            Action::EnterSearch(SearchDirection::Forward) => {}
            other => panic!("expected EnterSearch(Forward), got {other:?}"),
        }
    }

    #[test]
    fn question_in_normal_enters_backward_search() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('?')),
        ) {
            Action::EnterSearch(SearchDirection::Backward) => {}
            other => panic!("expected EnterSearch(Backward), got {other:?}"),
        }
    }

    #[test]
    fn n_in_normal_repeats_search_forward() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('n'))
            ),
            Action::SearchNext
        ));
    }

    #[test]
    fn capital_n_in_normal_repeats_search_reverse() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('N'))
            ),
            Action::SearchPrevious
        ));
    }

    #[test]
    fn printable_char_in_search_appends_to_pattern() {
        let (_, b) = fixture();
        let modal = ModalState::Search(SearchDirection::Forward);
        match translate(ctx(modal, Pending::None, &b), key(KeyCode::Char('f'))) {
            Action::SearchAppend(c) => assert_eq!(c, 'f'),
            other => panic!("expected SearchAppend, got {other:?}"),
        }
    }

    #[test]
    fn enter_in_search_submits() {
        let (_, b) = fixture();
        let modal = ModalState::Search(SearchDirection::Forward);
        assert!(matches!(
            translate(ctx(modal, Pending::None, &b), key(KeyCode::Enter)),
            Action::SearchSubmit
        ));
    }

    #[test]
    fn esc_in_search_cancels() {
        let (_, b) = fixture();
        let modal = ModalState::Search(SearchDirection::Backward);
        assert!(matches!(
            translate(ctx(modal, Pending::None, &b), key(KeyCode::Esc)),
            Action::SearchCancel
        ));
    }

    #[test]
    fn backspace_in_search_pops_pattern() {
        let (_, b) = fixture();
        let modal = ModalState::Search(SearchDirection::Forward);
        assert!(matches!(
            translate(ctx(modal, Pending::None, &b), key(KeyCode::Backspace)),
            Action::SearchBackspace
        ));
    }

    #[test]
    fn ctrl_c_in_search_quits_immediately() {
        let (_, b) = fixture();
        let modal = ModalState::Search(SearchDirection::Forward);
        assert!(matches!(
            translate(ctx(modal, Pending::None, &b), ctrl(KeyCode::Char('c'))),
            Action::Quit
        ));
    }

    // ---- WORD motions / D/C/S / J / ;/, ----

    #[test]
    fn capital_w_invokes_big_word_forward() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('W')),
        );
        assert_eq!(invocation_command(&action), Some(b.big_word_forward.0));
    }

    #[test]
    fn capital_b_invokes_big_word_backward() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('B')),
        );
        assert_eq!(invocation_command(&action), Some(b.big_word_backward.0));
    }

    #[test]
    fn capital_e_invokes_big_word_end() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('E')),
        );
        assert_eq!(invocation_command(&action), Some(b.big_word_end.0));
    }

    #[test]
    fn capital_d_invokes_delete_to_line_end() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('D')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.delete.0);
                match inv.target {
                    Some(Target::Motion(id, _)) => assert_eq!(id, b.line_end),
                    other => panic!("expected line_end target, got {other:?}"),
                }
            }
            other => panic!("expected Invoke, got {other:?}"),
        }
    }

    #[test]
    fn capital_c_invokes_change_to_line_end() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('C')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.change.0);
                match inv.target {
                    Some(Target::Motion(id, _)) => assert_eq!(id, b.line_end),
                    other => panic!("expected line_end target, got {other:?}"),
                }
            }
            other => panic!("expected Invoke, got {other:?}"),
        }
    }

    #[test]
    fn capital_s_invokes_change_current_line() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('S')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.change.0);
                assert_eq!(inv.range, Some(lattice_grammar::Range::CurrentLine));
            }
            other => panic!("expected Invoke, got {other:?}"),
        }
    }

    #[test]
    fn capital_j_emits_join_with_space() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('J')),
        ) {
            Action::JoinLines { with_space } => assert!(with_space),
            other => panic!("expected JoinLines(with_space=true), got {other:?}"),
        }
    }

    #[test]
    fn gj_after_g_emits_join_without_space() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::AfterG, &b),
            key(KeyCode::Char('J')),
        ) {
            Action::JoinLines { with_space } => assert!(!with_space),
            other => panic!("expected JoinLines(with_space=false), got {other:?}"),
        }
    }

    #[test]
    fn semicolon_emits_find_repeat_no_reverse() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char(';')),
        ) {
            Action::FindRepeat { reverse } => assert!(!reverse),
            other => panic!("expected FindRepeat, got {other:?}"),
        }
    }

    #[test]
    fn comma_emits_find_repeat_reverse() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char(',')),
        ) {
            Action::FindRepeat { reverse } => assert!(reverse),
            other => panic!("expected FindRepeat, got {other:?}"),
        }
    }

    #[test]
    fn d_capital_w_resolves_to_delete_big_word_forward() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.delete), &b),
            key(KeyCode::Char('W')),
        );
        match action {
            Action::Invoke(inv) => match inv.target {
                Some(Target::Motion(id, _)) => assert_eq!(id, b.big_word_forward),
                other => panic!("expected motion target, got {other:?}"),
            },
            _ => panic!("expected Invoke"),
        }
    }

    // ---- Macros: q, @ ----

    #[test]
    fn q_in_normal_when_not_recording_pends_for_macro_register() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('q'))
            ),
            Action::SetPending(Pending::AfterMacroStart)
        ));
    }

    #[test]
    fn q_in_normal_while_recording_stops() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx_recording(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('q'))
            ),
            Action::StopMacroRecord
        ));
    }

    #[test]
    fn at_in_normal_pends_for_macro_play() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('@'))
            ),
            Action::SetPending(Pending::AfterMacroPlay)
        ));
    }

    #[test]
    fn letter_after_q_starts_recording() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::AfterMacroStart, &b),
            key(KeyCode::Char('a')),
        ) {
            Action::StartMacroRecord(c) => assert_eq!(c, 'a'),
            other => panic!("expected StartMacroRecord, got {other:?}"),
        }
    }

    #[test]
    fn letter_after_at_plays_macro() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::AfterMacroPlay, &b),
            key(KeyCode::Char('q')),
        ) {
            Action::PlayMacro(c) => assert_eq!(c, 'q'),
            other => panic!("expected PlayMacro, got {other:?}"),
        }
    }

    #[test]
    fn at_at_plays_last_macro() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterMacroPlay, &b),
                key(KeyCode::Char('@'))
            ),
            Action::PlayLastMacro
        ));
    }

    #[test]
    fn esc_after_macro_pending_clears() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterMacroStart, &b),
                key(KeyCode::Esc)
            ),
            Action::SetPending(Pending::None)
        ));
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterMacroPlay, &b),
                key(KeyCode::Esc)
            ),
            Action::SetPending(Pending::None)
        ));
    }

    // ---- Folds: zf zo zc za zR zM zd ----

    #[test]
    fn zf_after_z_emits_create_fold_from_visual() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterZ, &b),
                key(KeyCode::Char('f'))
            ),
            Action::CreateFoldFromVisual
        ));
    }

    #[test]
    fn zo_after_z_emits_open_fold() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterZ, &b),
                key(KeyCode::Char('o'))
            ),
            Action::OpenFoldAtCursor
        ));
    }

    #[test]
    fn zc_after_z_emits_close_fold() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterZ, &b),
                key(KeyCode::Char('c'))
            ),
            Action::CloseFoldAtCursor
        ));
    }

    #[test]
    fn za_after_z_emits_toggle_fold() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterZ, &b),
                key(KeyCode::Char('a'))
            ),
            Action::ToggleFoldAtCursor
        ));
    }

    #[test]
    fn capital_z_r_after_z_opens_all() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterZ, &b),
                key(KeyCode::Char('R'))
            ),
            Action::OpenAllFolds
        ));
    }

    #[test]
    fn capital_z_m_after_z_closes_all() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterZ, &b),
                key(KeyCode::Char('M'))
            ),
            Action::CloseAllFolds
        ));
    }

    #[test]
    fn zd_after_z_deletes_fold() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterZ, &b),
                key(KeyCode::Char('d'))
            ),
            Action::DeleteFoldAtCursor
        ));
    }

    // ---- Blockwise visual ----

    #[test]
    fn ctrl_v_enters_blockwise_visual() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::None, &b),
            ctrl(KeyCode::Char('v')),
        ) {
            Action::EnterVisual(VisualKind::Blockwise) => {}
            other => panic!("expected EnterVisual(Blockwise), got {other:?}"),
        }
    }

    #[test]
    fn ctrl_q_is_alternate_blockwise_visual() {
        // Many terminals (Konsole, Windows Terminal, tmux paste-key)
        // intercept Ctrl+V for clipboard paste before it reaches us.
        // Vim binds Ctrl+Q as the alternate enter-block-visual key for
        // exactly this reason.
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::None, &b),
            ctrl(KeyCode::Char('q')),
        ) {
            Action::EnterVisual(VisualKind::Blockwise) => {}
            other => panic!("expected EnterVisual(Blockwise), got {other:?}"),
        }
    }

    #[test]
    fn lowercase_q_without_ctrl_still_pends_macro_record() {
        // Guard against the Ctrl+Q binding accidentally swallowing the
        // bare `q` that starts macro recording.
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('q')),
        ) {
            Action::SetPending(Pending::AfterMacroStart) => {}
            other => panic!("expected SetPending(AfterMacroStart), got {other:?}"),
        }
    }

    // ---- Help buffer (DESIGN.md §5.11, §5.9) ----
    //
    // Help is a regular buffer routed through `translate_normal` via
    // `App::active_buffer`. Only three buffer-local bindings differ
    // from the document path: `Esc` / `q` dismiss, `<CR>` follows
    // the link under the cursor. Everything else (motions, page
    // motions, `<C-o>` / `<C-i>`, `gg` / `G`) flows through the same
    // chord grammar -- the apply layer decides which cursor moves.

    fn ctx_help_active<'a>(
        modal: ModalState,
        pending: Pending,
        b: &'a Builtins,
    ) -> TranslateContext<'a> {
        TranslateContext {
            modal,
            pending,
            builtins: b,
            pending_count: 0,
            recording_macro: false,
            active_buffer: BufferKind::Help,
            completion_open: false,
            chord_capture: false,
            picker_open: false,
            insert_completion_open: false,
            snippet_active: false,
        }
    }

    #[test]
    fn help_active_intercepts_q_to_dismiss() {
        let (_, b) = fixture();
        // While help is the active buffer, `q` dismisses (does NOT
        // start macro recording, the usual Normal-mode meaning).
        assert!(matches!(
            translate(
                ctx_help_active(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('q'))
            ),
            Action::HelpDismiss
        ));
    }

    #[test]
    fn help_active_intercepts_esc_to_dismiss() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx_help_active(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Esc)
            ),
            Action::HelpDismiss
        ));
    }

    #[test]
    fn help_active_routes_enter_to_follow_link() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx_help_active(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Enter)
            ),
            Action::FollowLink
        ));
    }

    #[test]
    fn help_active_routes_jk_through_normal_motions() {
        // `j` in help is the *same* line_down motion as in Normal --
        // active_buffer routing in the apply layer redirects which
        // cursor moves; the chord grammar is unchanged.
        let (_, b) = fixture();
        let action = translate(
            ctx_help_active(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('j')),
        );
        assert_eq!(invocation_command(&action), Some(b.line_down.0));
    }

    #[test]
    fn help_active_routes_gg_through_chord_grammar() {
        // First `g` arms AfterG (same as Normal); second resolves to
        // goto_first_line. The buffer-local handler must NOT collapse
        // a bare `g` into `gg` -- that was the bug fc872ec papered
        // over with a help-specific chord engine.
        let (_, b) = fixture();
        let first = translate(
            ctx_help_active(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('g')),
        );
        assert!(matches!(first, Action::SetPending(Pending::AfterG)));
        let second = translate(
            ctx_help_active(ModalState::Normal, Pending::AfterG, &b),
            key(KeyCode::Char('g')),
        );
        assert_eq!(invocation_command(&second), Some(b.goto_first_line.0));
    }

    #[test]
    fn help_active_routes_capital_g_to_goto_last_line() {
        let (_, b) = fixture();
        let action = translate(
            ctx_help_active(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('G')),
        );
        assert_eq!(invocation_command(&action), Some(b.goto_last_line.0));
    }

    #[test]
    fn help_active_routes_ctrl_o_to_jump_history_back() {
        // `<C-o>` and `<C-i>` walk the unified position history --
        // crossing the document <-> help boundary is what
        // active_buffer routing makes possible.
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx_help_active(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('o'))
            ),
            Action::JumpHistoryBack
        ));
    }

    // ---- Pane navigation (DESIGN.md §5.9, B.1.b) ----

    #[test]
    fn ctrl_w_arms_after_ctrl_w_pending() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('w'))
            ),
            Action::SetPending(Pending::AfterCtrlW)
        ));
    }

    #[test]
    fn ctrl_w_l_navigates_right() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterCtrlW, &b),
                key(KeyCode::Char('l'))
            ),
            Action::NavigatePane(PaneDirection::Right)
        ));
    }

    #[test]
    fn ctrl_w_ctrl_l_also_navigates_right() {
        // Vim accepts the "Ctrl held throughout" form (`<C-w><C-l>`)
        // as well as the "release then press" form (`<C-w>l`).
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterCtrlW, &b),
                ctrl(KeyCode::Char('l'))
            ),
            Action::NavigatePane(PaneDirection::Right)
        ));
    }

    #[test]
    fn ctrl_w_ctrl_j_navigates_down() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterCtrlW, &b),
                ctrl(KeyCode::Char('j'))
            ),
            Action::NavigatePane(PaneDirection::Down)
        ));
    }

    #[test]
    fn ctrl_w_w_cycles_to_next_pane() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterCtrlW, &b),
                key(KeyCode::Char('w'))
            ),
            Action::NextPane
        ));
    }

    #[test]
    fn ctrl_w_capital_w_cycles_to_prev_pane() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterCtrlW, &b),
                key(KeyCode::Char('W'))
            ),
            Action::PrevPane
        ));
    }

    #[test]
    fn ctrl_c_still_quits_when_help_is_active() {
        // The universal escape hatch sits above the help intercept.
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx_help_active(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('c'))
            ),
            Action::Quit
        ));
    }

    // ---- Keymap drift detection (DESIGN.md §5.2.3, §5.11) ----

    /// Parse a chord-notation string from `keymap::default_keymap()` into
    /// a sequence of `KeyEvent`s. Recognises:
    /// - bare chars: `j` / `dw` / `gg`
    /// - special keys: `<Esc>`, `<CR>`, `<Tab>`, `<BS>`,
    ///   `<Up>`/`<Down>`/`<Left>`/`<Right>`, `<Home>`/`<End>`,
    ///   `<PageUp>`/`<PageDown>`
    /// - control chords: `<C-d>`, `<C-v>`, `<C-r>`, ...
    fn parse_chord_for_test(chord: &str) -> Vec<KeyEvent> {
        // `<` and `>` are valid bare chords (indent-left / indent-right
        // operators). Treat a single-char chord as a literal character
        // so the escape parser doesn't try to interpret `<` as the
        // start of a `<Special>` token.
        if chord.chars().count() == 1 {
            let c = chord.chars().next().expect("len == 1");
            return vec![KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE)];
        }
        let mut out = Vec::new();
        let mut chars = chord.chars().peekable();
        while let Some(c) = chars.next() {
            if c != '<' {
                out.push(KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE));
                continue;
            }
            let mut body = String::new();
            for n in chars.by_ref() {
                if n == '>' {
                    break;
                }
                body.push(n);
            }
            let evt = match body.as_str() {
                "Esc" => KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE),
                "CR" | "Enter" => KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE),
                "Tab" => KeyEvent::new(KeyCode::Tab, KeyModifiers::NONE),
                "BS" => KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE),
                "Up" => KeyEvent::new(KeyCode::Up, KeyModifiers::NONE),
                "Down" => KeyEvent::new(KeyCode::Down, KeyModifiers::NONE),
                "Left" => KeyEvent::new(KeyCode::Left, KeyModifiers::NONE),
                "Right" => KeyEvent::new(KeyCode::Right, KeyModifiers::NONE),
                "Home" => KeyEvent::new(KeyCode::Home, KeyModifiers::NONE),
                "End" => KeyEvent::new(KeyCode::End, KeyModifiers::NONE),
                "PageUp" => KeyEvent::new(KeyCode::PageUp, KeyModifiers::NONE),
                "PageDown" => KeyEvent::new(KeyCode::PageDown, KeyModifiers::NONE),
                other => {
                    if let Some(rest) = other.strip_prefix("S-") {
                        // Shift-modified specials: `<S-Tab>` is the
                        // primary user; crossterm reports it as
                        // `BackTab` (no SHIFT modifier on the event).
                        let evt = match rest {
                            "Tab" => KeyEvent::new(KeyCode::BackTab, KeyModifiers::NONE),
                            _ => match rest.chars().next() {
                                Some(c) => KeyEvent::new(
                                    KeyCode::Char(c),
                                    KeyModifiers::SHIFT,
                                ),
                                None => continue,
                            },
                        };
                        out.push(evt);
                        continue;
                    }
                    if let Some(rest) = other.strip_prefix("C-") {
                        // Recognise `Space` as a token before falling
                        // back to the single-char path so `<C-Space>`
                        // parses to `Char(' ') + CONTROL`. Same shape
                        // crossterm reports.
                        let evt = match rest {
                            "Space" => KeyEvent::new(
                                KeyCode::Char(' '),
                                KeyModifiers::CONTROL,
                            ),
                            _ => match rest.chars().next() {
                                Some(c) => KeyEvent::new(
                                    KeyCode::Char(c),
                                    KeyModifiers::CONTROL,
                                ),
                                None => continue,
                            },
                        };
                        evt
                    } else {
                        // Unrecognised special-key notation -- skip
                        // rather than panic; the drift test will fail
                        // with a clearer message about the descriptor.
                        continue;
                    }
                }
            };
            out.push(evt);
        }
        out
    }

    /// Walk a chord through `translate()` from the descriptor's
    /// starting mode, updating pending state across multi-key
    /// sequences. Returns the final Action.
    fn simulate_chord(
        chord: &str,
        mode: crate::keymap::BindingMode,
        builtins: &Builtins,
    ) -> Action {
        use crate::keymap::BindingMode;
        let modal = match mode {
            BindingMode::Visual => ModalState::Visual(lattice_grammar::VisualKind::Charwise),
            // CompletionPopup minor mode rides on top of Insert.
            BindingMode::Insert | BindingMode::CompletionPopup | BindingMode::AfterCtrlX => {
                ModalState::Insert
            }
            BindingMode::Replace => ModalState::Replace,
            BindingMode::Command => ModalState::Command,
            BindingMode::Search => ModalState::Search(lattice_grammar::SearchDirection::Forward),
            // After-* modes are pending substates of Normal: their
            // chords include the prefix (`gg`, `gU`, `zz`, ...) so we
            // start the walk from Normal pending=None and let
            // translate() set the pending state mid-sequence.
            _ => ModalState::Normal,
        };
        let active_buffer = if matches!(mode, BindingMode::Help) {
            BufferKind::Help
        } else {
            BufferKind::Document
        };
        // After-* modes whose chord doesn't start with the prefix
        // (e.g. `AfterCtrlX` whose chord is `<C-x><C-o>` -- which
        // *does* start with `<C-x>`) need the prefix in the chord.
        // The completion-popup minor mode is signalled host-side
        // by `App.insert_completion.is_some()`; in this harness we
        // toggle the equivalent context flag.
        let insert_completion_open = matches!(mode, BindingMode::CompletionPopup);
        let snippet_active = matches!(mode, BindingMode::Snippet);
        // Snippet minor mode rides on Insert.
        let modal = if snippet_active {
            ModalState::Insert
        } else {
            modal
        };
        let mut pending = Pending::None;
        let mut last = Action::None;
        for event in parse_chord_for_test(chord) {
            let ctx = TranslateContext {
                modal,
                pending,
                builtins,
                pending_count: 0,
                recording_macro: false,
                active_buffer,
                completion_open: false,
                chord_capture: false,
                picker_open: false,
                insert_completion_open,
                snippet_active,
            };
            last = translate(ctx, event);
            if let Action::SetPending(p) = &last {
                pending = *p;
            }
        }
        last
    }

    #[test]
    fn keymap_descriptors_dont_drift_from_translate() {
        // Every descriptor in `keymap::default_keymap()` must produce a
        // non-`None` Action when its chord is simulated through
        // `translate()` in the matching mode. This catches:
        //   - removed bindings (descriptor still in table)
        //   - moved bindings (descriptor in wrong mode)
        //   - typo'd chord notation
        // Adding a binding to `input.rs` without updating
        // `default_keymap()` is *not* caught here -- the inverse drift
        // is fine for v1 (descriptors are a discoverability surface;
        // unmentioned bindings still work).
        let (_, b) = fixture();
        for entry in crate::keymap::default_keymap() {
            let action = simulate_chord(entry.chord, entry.mode, &b);
            assert!(
                !matches!(action, Action::None),
                "keymap descriptor `{}` ({}) doc=`{}` produced Action::None -- \
                 binding may have been removed or moved",
                entry.chord,
                entry.mode.label(),
                entry.doc,
            );
        }
    }

    // ---- Mark history (g; / g,) ----

    #[test]
    fn g_semicolon_after_g_walks_mark_history_back() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterG, &b),
                key(KeyCode::Char(';'))
            ),
            Action::WalkMarkHistoryBack
        ));
    }

    #[test]
    fn g_comma_after_g_walks_mark_history_forward() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterG, &b),
                key(KeyCode::Char(','))
            ),
            Action::WalkMarkHistoryForward
        ));
    }

    // ---- LSP navigation (gd / gD / gy / gI) ----

    #[test]
    fn gd_after_g_emits_lsp_definition_request() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterG, &b),
                key(KeyCode::Char('d'))
            ),
            Action::LspDefinitionRequest
        ));
    }

    #[test]
    fn capital_g_d_after_g_emits_lsp_declaration_request() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterG, &b),
                key(KeyCode::Char('D'))
            ),
            Action::LspDeclarationRequest
        ));
    }

    #[test]
    fn gy_after_g_emits_lsp_type_definition_request() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterG, &b),
                key(KeyCode::Char('y'))
            ),
            Action::LspTypeDefinitionRequest
        ));
    }

    #[test]
    fn capital_g_i_after_g_emits_lsp_implementation_request() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterG, &b),
                key(KeyCode::Char('I'))
            ),
            Action::LspImplementationRequest
        ));
    }

    #[test]
    fn gr_after_g_emits_lsp_references_request() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterG, &b),
                key(KeyCode::Char('r'))
            ),
            Action::LspReferencesRequest
        ));
    }

    // ---- Insert-mode completion (Phase 4.2.g.1) ----

    fn ctx_insert_completion<'a>(b: &'a Builtins) -> TranslateContext<'a> {
        TranslateContext {
            modal: ModalState::Insert,
            pending: Pending::None,
            builtins: b,
            pending_count: 0,
            recording_macro: false,
            active_buffer: BufferKind::Document,
            completion_open: false,
            chord_capture: false,
            picker_open: false,
            insert_completion_open: true,
            snippet_active: false,
        }
    }

    #[test]
    fn ctrl_space_in_insert_triggers_completion() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Insert, Pending::None, &b),
                ctrl(KeyCode::Char(' '))
            ),
            Action::CompletionTrigger
        ));
    }

    #[test]
    fn ctrl_x_in_insert_arms_after_ctrl_x_pending() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Insert, Pending::None, &b),
                ctrl(KeyCode::Char('x'))
            ),
            Action::SetPending(Pending::AfterCtrlX)
        ));
    }

    #[test]
    fn ctrl_x_ctrl_o_resolves_to_completion_trigger() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Insert, Pending::AfterCtrlX, &b),
                ctrl(KeyCode::Char('o'))
            ),
            Action::CompletionTrigger
        ));
    }

    #[test]
    fn ctrl_x_followed_by_unrecognised_clears_pending() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Insert, Pending::AfterCtrlX, &b),
                ctrl(KeyCode::Char('z'))
            ),
            Action::SetPending(Pending::None)
        ));
    }

    #[test]
    fn popup_open_ctrl_n_navigates_next() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_insert_completion(&b), ctrl(KeyCode::Char('n'))),
            Action::CompletionNext
        ));
    }

    #[test]
    fn popup_open_ctrl_p_navigates_prev() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_insert_completion(&b), ctrl(KeyCode::Char('p'))),
            Action::CompletionPrev
        ));
    }

    #[test]
    fn popup_open_tab_accepts() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_insert_completion(&b), key(KeyCode::Tab)),
            Action::CompletionAccept
        ));
    }

    #[test]
    fn popup_open_ctrl_y_accepts() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_insert_completion(&b), ctrl(KeyCode::Char('y'))),
            Action::CompletionAccept
        ));
    }

    #[test]
    fn popup_open_enter_accepts() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_insert_completion(&b), key(KeyCode::Enter)),
            Action::CompletionAccept
        ));
    }

    #[test]
    fn popup_open_ctrl_e_cancels_keeps_insert() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_insert_completion(&b), ctrl(KeyCode::Char('e'))),
            Action::CompletionCancel
        ));
    }

    #[test]
    fn popup_open_esc_cancels_and_exits_insert() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_insert_completion(&b), key(KeyCode::Esc)),
            Action::CompletionCancelAndExitInsert
        ));
    }

    #[test]
    fn popup_open_ctrl_d_toggles_docs_only_inside_minor_mode() {
        let (_, b) = fixture();
        // Inside the popup minor mode -- claim it.
        assert!(matches!(
            translate(ctx_insert_completion(&b), ctrl(KeyCode::Char('d'))),
            Action::CompletionToggleDocs
        ));
        // OUTSIDE the minor mode (Normal mode) -- the popup
        // layer doesn't fire; falls through to Normal-mode
        // half-page-down. This verifies the layer's
        // confinement.
        let half_down = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            ctrl(KeyCode::Char('d')),
        );
        assert!(!matches!(half_down, Action::CompletionToggleDocs));
    }

    #[test]
    fn popup_open_ctrl_f_pages_docs_down() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_insert_completion(&b), ctrl(KeyCode::Char('f'))),
            Action::CompletionDocsScrollDown
        ));
    }

    #[test]
    fn popup_open_ctrl_b_pages_docs_up() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_insert_completion(&b), ctrl(KeyCode::Char('b'))),
            Action::CompletionDocsScrollUp
        ));
    }

    #[test]
    fn popup_open_ctrl_space_re_triggers() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_insert_completion(&b), ctrl(KeyCode::Char(' '))),
            Action::CompletionTrigger
        ));
    }

    // ---- Active-snippet minor mode (Phase 4.2.g.4) ----

    fn ctx_snippet_active<'a>(b: &'a Builtins) -> TranslateContext<'a> {
        TranslateContext {
            modal: ModalState::Insert,
            pending: Pending::None,
            builtins: b,
            pending_count: 0,
            recording_macro: false,
            active_buffer: BufferKind::Document,
            completion_open: false,
            chord_capture: false,
            picker_open: false,
            insert_completion_open: false,
            snippet_active: true,
        }
    }

    #[test]
    fn snippet_active_tab_jumps_to_next_placeholder() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_snippet_active(&b), key(KeyCode::Tab)),
            Action::SnippetNextPlaceholder
        ));
    }

    #[test]
    fn snippet_active_back_tab_jumps_to_prev_placeholder() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_snippet_active(&b), key(KeyCode::BackTab)),
            Action::SnippetPrevPlaceholder
        ));
    }

    #[test]
    fn snippet_active_esc_leaves_snippet() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_snippet_active(&b), key(KeyCode::Esc)),
            Action::SnippetLeave
        ));
    }

    #[test]
    fn snippet_active_other_keys_fall_through_to_insert() {
        let (_, b) = fixture();
        // A regular printable char inside a placeholder should
        // still hit the Insert-mode handler so the user can
        // overtype the default.
        let action = translate(ctx_snippet_active(&b), key(KeyCode::Char('x')));
        assert!(matches!(action, Action::Insert(s) if s == "x"));
    }

    #[test]
    fn snippet_active_yields_to_completion_popup() {
        // When both layers are active the popup wins for
        // `<Tab>` (popup uses Tab to accept, snippet uses Tab
        // to step). Otherwise navigating snippet placeholders
        // through the popup would be impossible.
        let (_, b) = fixture();
        let mut ctx = ctx_snippet_active(&b);
        ctx.insert_completion_open = true;
        let action = translate(ctx, key(KeyCode::Tab));
        assert!(matches!(action, Action::CompletionAccept));
    }

    #[test]
    fn snippet_active_only_in_insert_mode() {
        // Active-snippet layer must never claim keys outside
        // Insert mode; otherwise a stuck snippet could swallow
        // Normal-mode `<Tab>` (which is `<C-i>` -- jump-list
        // forward).
        let (_, b) = fixture();
        let mut ctx = ctx_snippet_active(&b);
        ctx.modal = ModalState::Normal;
        let action = translate(ctx, key(KeyCode::Tab));
        // Normal-mode `<Tab>` is `<C-i>` -- jump history forward.
        assert!(!matches!(action, Action::SnippetNextPlaceholder));
    }

    #[test]
    fn ctrl_x_ctrl_s_resolves_to_snippet_expand() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Insert, Pending::AfterCtrlX, &b),
                ctrl(KeyCode::Char('s'))
            ),
            Action::SnippetExpand
        ));
    }

    // ---- Position history ----

    #[test]
    fn ctrl_o_emits_jump_history_back() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('o'))
            ),
            Action::JumpHistoryBack
        ));
    }

    #[test]
    fn ctrl_i_emits_jump_history_forward() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('i'))
            ),
            Action::JumpHistoryForward
        ));
    }

    #[test]
    fn ctrl_l_emits_redraw_screen() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('l'))
            ),
            Action::RedrawScreen
        ));
    }

    #[test]
    fn tab_in_normal_emits_jump_history_forward() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Tab)
            ),
            Action::JumpHistoryForward
        ));
    }

    // ---- Register prefix ----

    #[test]
    fn quote_in_normal_sets_pending_after_register() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('"'))
            ),
            Action::SetPending(Pending::AfterRegister)
        ));
    }

    #[test]
    fn lowercase_letter_after_quote_selects_named_register() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterRegister, &b),
            key(KeyCode::Char('a')),
        );
        match action {
            Action::SelectRegister(Register::Named(c)) => assert_eq!(c, 'a'),
            other => panic!("expected SelectRegister(Named('a')), got {other:?}"),
        }
    }

    #[test]
    fn digit_after_quote_selects_numbered_register() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterRegister, &b),
            key(KeyCode::Char('0')),
        );
        match action {
            Action::SelectRegister(Register::Numbered(n)) => assert_eq!(n, 0),
            other => panic!("expected SelectRegister(Numbered(0)), got {other:?}"),
        }
    }

    #[test]
    fn underscore_after_quote_selects_black_hole() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterRegister, &b),
            key(KeyCode::Char('_')),
        );
        assert!(matches!(
            action,
            Action::SelectRegister(Register::BlackHole)
        ));
    }

    #[test]
    fn plus_after_quote_selects_system() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterRegister, &b),
            key(KeyCode::Char('+')),
        );
        assert!(matches!(action, Action::SelectRegister(Register::System)));
    }

    #[test]
    fn invalid_char_after_quote_clears_pending() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterRegister, &b),
                key(KeyCode::Char('@'))
            ),
            Action::SetPending(Pending::None)
        ));
    }

    // ---- ~ toggle case at cursor ----

    #[test]
    fn tilde_emits_toggle_case_at_cursor() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('~'))
            ),
            Action::ToggleCaseAtCursor
        ));
    }

    // ---- Word-search and matching-bracket ----

    #[test]
    fn star_emits_search_word_forward() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('*'))
            ),
            Action::SearchWordUnderCursor(SearchDirection::Forward)
        ));
    }

    #[test]
    fn hash_emits_search_word_backward() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('#'))
            ),
            Action::SearchWordUnderCursor(SearchDirection::Backward)
        ));
    }

    #[test]
    fn percent_emits_match_bracket() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('%'))
            ),
            Action::MatchBracket
        ));
    }

    // ---- Viewport motions: H, M, L, z*, Ctrl-F/B/Y/E ----

    #[test]
    fn capital_h_emits_jump_viewport_top() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('H')),
        );
        assert!(matches!(action, Action::JumpViewport(ViewportPos::Top)));
    }

    #[test]
    fn capital_m_emits_jump_viewport_middle() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('M')),
        );
        assert!(matches!(action, Action::JumpViewport(ViewportPos::Middle)));
    }

    #[test]
    fn capital_l_emits_jump_viewport_bottom() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('L')),
        );
        assert!(matches!(action, Action::JumpViewport(ViewportPos::Bottom)));
    }

    #[test]
    fn z_sets_pending_after_z() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('z')),
        );
        assert!(matches!(action, Action::SetPending(Pending::AfterZ)));
    }

    #[test]
    fn zz_emits_scroll_cursor_center() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterZ, &b),
            key(KeyCode::Char('z')),
        );
        assert!(matches!(action, Action::ScrollCursorTo(ScrollPos::Center)));
    }

    #[test]
    fn zt_emits_scroll_cursor_top() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterZ, &b),
            key(KeyCode::Char('t')),
        );
        assert!(matches!(action, Action::ScrollCursorTo(ScrollPos::Top)));
    }

    #[test]
    fn zb_emits_scroll_cursor_bottom() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterZ, &b),
            key(KeyCode::Char('b')),
        );
        assert!(matches!(action, Action::ScrollCursorTo(ScrollPos::Bottom)));
    }

    #[test]
    fn ctrl_f_emits_page_down() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('f'))
            ),
            Action::PageDown
        ));
    }

    #[test]
    fn ctrl_b_emits_page_up() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('b'))
            ),
            Action::PageUp
        ));
    }

    #[test]
    fn ctrl_e_emits_scroll_line_down() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('e'))
            ),
            Action::ScrollLineDown
        ));
    }

    #[test]
    fn ctrl_y_emits_scroll_line_up() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                ctrl(KeyCode::Char('y'))
            ),
            Action::ScrollLineUp
        ));
    }

    #[test]
    fn esc_after_z_pending_clears() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterZ, &b),
                key(KeyCode::Esc)
            ),
            Action::SetPending(Pending::None)
        ));
    }

    // ---- Replace mode ----

    #[test]
    fn capital_r_enters_replace_mode() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('R'))
            ),
            Action::EnterMode(ModalState::Replace)
        ));
    }

    #[test]
    fn char_in_replace_emits_overwrite() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Replace, Pending::None, &b),
            key(KeyCode::Char('z')),
        ) {
            Action::OverwriteChar(c) => assert_eq!(c, 'z'),
            other => panic!("expected OverwriteChar, got {other:?}"),
        }
    }

    #[test]
    fn esc_in_replace_returns_to_normal() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Replace, Pending::None, &b),
                key(KeyCode::Esc)
            ),
            Action::EnterMode(ModalState::Normal)
        ));
    }

    #[test]
    fn backspace_in_replace_emits_replace_undo_last() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Replace, Pending::None, &b),
                key(KeyCode::Backspace)
            ),
            Action::ReplaceUndoLast
        ));
    }

    #[test]
    fn enter_in_replace_inserts_newline() {
        let (_, b) = fixture();
        match translate(
            ctx(ModalState::Replace, Pending::None, &b),
            key(KeyCode::Enter),
        ) {
            Action::Insert(s) => assert_eq!(s, "\n"),
            other => panic!("expected Insert, got {other:?}"),
        }
    }

    // ---- Marks ----

    #[test]
    fn m_in_normal_sets_pending_after_set_mark() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('m')),
        );
        assert!(matches!(action, Action::SetPending(Pending::AfterSetMark)));
    }

    #[test]
    fn apostrophe_in_normal_sets_pending_after_jump_mark_line() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('\'')),
        );
        assert!(matches!(
            action,
            Action::SetPending(Pending::AfterJumpMarkLine)
        ));
    }

    #[test]
    fn backtick_in_normal_sets_pending_after_jump_mark_exact() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('`')),
        );
        assert!(matches!(
            action,
            Action::SetPending(Pending::AfterJumpMarkExact)
        ));
    }

    #[test]
    fn ma_after_m_emits_set_mark() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterSetMark, &b),
            key(KeyCode::Char('a')),
        );
        match action {
            Action::SetMark(c) => assert_eq!(c, 'a'),
            other => panic!("expected SetMark('a'), got {other:?}"),
        }
    }

    #[test]
    fn jump_mark_line_routes_correctly() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterJumpMarkLine, &b),
            key(KeyCode::Char('z')),
        );
        match action {
            Action::JumpToMarkLine(c) => assert_eq!(c, 'z'),
            other => panic!("expected JumpToMarkLine('z'), got {other:?}"),
        }
    }

    #[test]
    fn jump_mark_exact_routes_correctly() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterJumpMarkExact, &b),
            key(KeyCode::Char('A')),
        );
        match action {
            Action::JumpToMarkExact(c) => assert_eq!(c, 'A'),
            other => panic!("expected JumpToMarkExact('A'), got {other:?}"),
        }
    }

    #[test]
    fn esc_cancels_set_mark_pending() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterSetMark, &b),
                key(KeyCode::Esc)
            ),
            Action::SetPending(Pending::None)
        ));
    }

    #[test]
    fn non_alpha_after_set_mark_clears_pending() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::AfterSetMark, &b),
                key(KeyCode::Char(' '))
            ),
            Action::SetPending(Pending::None)
        ));
    }

    // ---- gv reselect ----

    #[test]
    fn gv_after_g_emits_reselect_visual() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterG, &b),
            key(KeyCode::Char('v')),
        );
        assert!(matches!(action, Action::ReselectLastVisual));
    }

    // ---- Indent and case operators ----

    #[test]
    fn gt_sets_pending_indent_right() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('>')),
        );
        match action {
            Action::SetPending(Pending::AfterOperator(op)) => {
                assert_eq!(op, b.indent_right);
            }
            other => panic!("expected SetPending(AfterOperator(indent_right)), got {other:?}"),
        }
    }

    #[test]
    fn lt_sets_pending_indent_left() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('<')),
        );
        match action {
            Action::SetPending(Pending::AfterOperator(op)) => {
                assert_eq!(op, b.indent_left);
            }
            other => panic!("expected SetPending(AfterOperator(indent_left)), got {other:?}"),
        }
    }

    #[test]
    fn double_gt_resolves_to_indent_right_current_line() {
        let (_, b) = fixture();
        let action = translate(
            ctx(
                ModalState::Normal,
                Pending::AfterOperator(b.indent_right),
                &b,
            ),
            key(KeyCode::Char('>')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.indent_right.0);
                assert_eq!(inv.range, Some(lattice_grammar::Range::CurrentLine));
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn gu_after_g_sets_pending_lower() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterG, &b),
            key(KeyCode::Char('u')),
        );
        match action {
            Action::SetPending(Pending::AfterOperator(op)) => assert_eq!(op, b.lower),
            other => panic!("expected SetPending(AfterOperator(lower)), got {other:?}"),
        }
    }

    #[test]
    fn capital_g_then_capital_u_sets_pending_upper() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterG, &b),
            key(KeyCode::Char('U')),
        );
        match action {
            Action::SetPending(Pending::AfterOperator(op)) => assert_eq!(op, b.upper),
            other => panic!("expected SetPending(AfterOperator(upper)), got {other:?}"),
        }
    }

    #[test]
    fn g_tilde_after_g_sets_pending_toggle_case() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterG, &b),
            key(KeyCode::Char('~')),
        );
        match action {
            Action::SetPending(Pending::AfterOperator(op)) => assert_eq!(op, b.toggle_case),
            other => panic!("expected SetPending(AfterOperator(toggle_case)), got {other:?}"),
        }
    }

    #[test]
    fn guu_resolves_to_lower_current_line() {
        let (_, b) = fixture();
        // After `gu`, pending = AfterOperator(lower). Pressing `u` doubles.
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.lower), &b),
            key(KeyCode::Char('u')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.lower.0);
                assert_eq!(inv.range, Some(lattice_grammar::Range::CurrentLine));
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn g_capital_u_w_resolves_to_upper_word_forward() {
        let (_, b) = fixture();
        // After `gU`, pending = AfterOperator(upper). Pressing `w` is the motion.
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.upper), &b),
            key(KeyCode::Char('w')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.upper.0);
                match inv.target {
                    Some(Target::Motion(id, _)) => assert_eq!(id, b.word_forward),
                    other => panic!("expected motion target, got {other:?}"),
                }
            }
            _ => panic!("expected Invoke"),
        }
    }

    // ---- Text object chord routing ----

    #[test]
    fn i_in_operator_pending_sets_text_object_pending_inner() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.delete), &b),
            key(KeyCode::Char('i')),
        );
        match action {
            Action::SetPending(Pending::AfterTextObject { operator, around }) => {
                assert_eq!(operator, b.delete);
                assert!(!around);
            }
            other => panic!("expected SetPending(AfterTextObject inner), got {other:?}"),
        }
    }

    #[test]
    fn a_in_operator_pending_sets_text_object_pending_around() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.delete), &b),
            key(KeyCode::Char('a')),
        );
        match action {
            Action::SetPending(Pending::AfterTextObject { operator, around }) => {
                assert_eq!(operator, b.delete);
                assert!(around);
            }
            other => panic!("expected SetPending(AfterTextObject around), got {other:?}"),
        }
    }

    #[test]
    fn diw_resolves_to_delete_inner_word() {
        let (_, b) = fixture();
        let pending = Pending::AfterTextObject {
            operator: b.delete,
            around: false,
        };
        let action = translate(
            ctx(ModalState::Normal, pending, &b),
            key(KeyCode::Char('w')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.delete.0);
                match inv.target {
                    Some(Target::TextObject(id, _)) => assert_eq!(id, b.inner_word),
                    other => panic!("expected text-object target, got {other:?}"),
                }
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn da_quote_resolves_to_delete_around_double_quote() {
        let (_, b) = fixture();
        let pending = Pending::AfterTextObject {
            operator: b.delete,
            around: true,
        };
        let action = translate(
            ctx(ModalState::Normal, pending, &b),
            key(KeyCode::Char('"')),
        );
        match action {
            Action::Invoke(inv) => match inv.target {
                Some(Target::TextObject(id, _)) => assert_eq!(id, b.around_quote_double),
                other => panic!("expected text-object target, got {other:?}"),
            },
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn ci_paren_resolves_to_change_inner_paren() {
        let (_, b) = fixture();
        let pending = Pending::AfterTextObject {
            operator: b.change,
            around: false,
        };
        let action = translate(
            ctx(ModalState::Normal, pending, &b),
            key(KeyCode::Char('(')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.change.0);
                match inv.target {
                    Some(Target::TextObject(id, _)) => assert_eq!(id, b.inner_paren),
                    other => panic!("expected text-object target, got {other:?}"),
                }
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    #[allow(non_snake_case)]
    fn diW_resolves_to_delete_inner_big_word() {
        let (_, b) = fixture();
        let pending = Pending::AfterTextObject {
            operator: b.delete,
            around: false,
        };
        let action = translate(
            ctx(ModalState::Normal, pending, &b),
            key(KeyCode::Char('W')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.delete.0);
                match inv.target {
                    Some(Target::TextObject(id, _)) => assert_eq!(id, b.inner_big_word),
                    other => panic!("expected text-object target, got {other:?}"),
                }
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    #[allow(non_snake_case)]
    fn daW_resolves_to_delete_around_big_word() {
        let (_, b) = fixture();
        let pending = Pending::AfterTextObject {
            operator: b.delete,
            around: true,
        };
        let action = translate(
            ctx(ModalState::Normal, pending, &b),
            key(KeyCode::Char('W')),
        );
        match action {
            Action::Invoke(inv) => match inv.target {
                Some(Target::TextObject(id, _)) => assert_eq!(id, b.around_big_word),
                other => panic!("expected text-object target, got {other:?}"),
            },
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn ci_angle_resolves_to_change_inner_angle() {
        let (_, b) = fixture();
        let pending = Pending::AfterTextObject {
            operator: b.change,
            around: false,
        };
        let action = translate(
            ctx(ModalState::Normal, pending, &b),
            key(KeyCode::Char('<')),
        );
        match action {
            Action::Invoke(inv) => match inv.target {
                Some(Target::TextObject(id, _)) => assert_eq!(id, b.inner_angle),
                other => panic!("expected text-object target, got {other:?}"),
            },
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn da_angle_via_closer_resolves_to_delete_around_angle() {
        // Both `<` and `>` should resolve to the angle text object.
        let (_, b) = fixture();
        let pending = Pending::AfterTextObject {
            operator: b.delete,
            around: true,
        };
        let action = translate(
            ctx(ModalState::Normal, pending, &b),
            key(KeyCode::Char('>')),
        );
        match action {
            Action::Invoke(inv) => match inv.target {
                Some(Target::TextObject(id, _)) => assert_eq!(id, b.around_angle),
                other => panic!("expected text-object target, got {other:?}"),
            },
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn esc_after_text_object_pending_clears() {
        let (_, b) = fixture();
        let pending = Pending::AfterTextObject {
            operator: b.delete,
            around: false,
        };
        assert!(matches!(
            translate(ctx(ModalState::Normal, pending, &b), key(KeyCode::Esc)),
            Action::SetPending(Pending::None)
        ));
    }

    // ---- Dot-repeat ----

    #[test]
    fn dot_in_normal_emits_repeat_last_change() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char('.'))
            ),
            Action::RepeatLastChange
        ));
    }

    // ---- Visual mode entry / exit ----

    #[test]
    fn v_in_normal_enters_charwise_visual() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('v')),
        );
        assert!(matches!(action, Action::EnterVisual(VisualKind::Charwise)));
    }

    #[test]
    fn capital_v_in_normal_enters_linewise_visual() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('V')),
        );
        assert!(matches!(action, Action::EnterVisual(VisualKind::Linewise)));
    }

    #[test]
    fn esc_in_visual_exits_to_normal() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Visual(VisualKind::Charwise), Pending::None, &b),
            key(KeyCode::Esc),
        );
        assert!(matches!(action, Action::ExitVisual));
    }

    #[test]
    fn v_in_visual_toggles_off() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Visual(VisualKind::Charwise), Pending::None, &b),
            key(KeyCode::Char('v')),
        );
        assert!(matches!(action, Action::ExitVisual));
    }

    #[test]
    fn motion_in_visual_returns_invocation() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Visual(VisualKind::Charwise), Pending::None, &b),
            key(KeyCode::Char('w')),
        );
        assert_eq!(invocation_command(&action), Some(b.word_forward.0));
    }

    #[test]
    fn d_in_visual_invokes_delete_with_selection_range() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Visual(VisualKind::Charwise), Pending::None, &b),
            key(KeyCode::Char('d')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.delete.0);
                assert_eq!(inv.range, Some(lattice_grammar::Range::Selection));
            }
            other => panic!("expected Invoke(delete, Selection), got {other:?}"),
        }
    }

    #[test]
    fn y_in_visual_invokes_yank_with_selection_range() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Visual(VisualKind::Charwise), Pending::None, &b),
            key(KeyCode::Char('y')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.yank.0);
                assert_eq!(inv.range, Some(lattice_grammar::Range::Selection));
            }
            other => panic!("expected Invoke(yank, Selection), got {other:?}"),
        }
    }

    #[test]
    fn c_in_visual_invokes_change_with_selection_range() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Visual(VisualKind::Charwise), Pending::None, &b),
            key(KeyCode::Char('c')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.change.0);
                assert_eq!(inv.range, Some(lattice_grammar::Range::Selection));
            }
            other => panic!("expected Invoke(change, Selection), got {other:?}"),
        }
    }

    #[test]
    fn gt_in_visual_invokes_indent_right_with_selection_range() {
        let (_, b) = fixture();
        for kind in [
            VisualKind::Charwise,
            VisualKind::Linewise,
            VisualKind::Blockwise,
        ] {
            let action = translate(
                ctx(ModalState::Visual(kind), Pending::None, &b),
                key(KeyCode::Char('>')),
            );
            match action {
                Action::Invoke(inv) => {
                    assert_eq!(inv.command, b.indent_right.0, "kind = {kind:?}");
                    assert_eq!(inv.range, Some(lattice_grammar::Range::Selection));
                }
                other => panic!(
                    "kind = {kind:?}, expected Invoke(indent_right, Selection), got {other:?}"
                ),
            }
        }
    }

    #[test]
    fn lt_in_visual_invokes_indent_left_with_selection_range() {
        let (_, b) = fixture();
        for kind in [
            VisualKind::Charwise,
            VisualKind::Linewise,
            VisualKind::Blockwise,
        ] {
            let action = translate(
                ctx(ModalState::Visual(kind), Pending::None, &b),
                key(KeyCode::Char('<')),
            );
            match action {
                Action::Invoke(inv) => {
                    assert_eq!(inv.command, b.indent_left.0, "kind = {kind:?}");
                    assert_eq!(inv.range, Some(lattice_grammar::Range::Selection));
                }
                other => panic!(
                    "kind = {kind:?}, expected Invoke(indent_left, Selection), got {other:?}"
                ),
            }
        }
    }

    // ---- Count prefix (1-9, 0 with count in progress) ----

    #[test]
    fn digit_1_to_9_emits_push_digit_in_normal_mode() {
        let (_, b) = fixture();
        for digit in 1u8..=9 {
            let c = char::from_digit(digit as u32, 10).unwrap();
            let action = translate(
                ctx(ModalState::Normal, Pending::None, &b),
                key(KeyCode::Char(c)),
            );
            assert!(matches!(action, Action::PushDigit(d) if d == digit));
        }
    }

    #[test]
    fn zero_with_no_count_invokes_line_start() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('0')),
        );
        assert_eq!(invocation_command(&action), Some(b.line_start.0));
    }

    #[test]
    fn zero_with_count_in_progress_extends_count() {
        let (_, b) = fixture();
        // pending_count == 1 -> '0' becomes a digit, not line_start.
        let action = translate(
            ctx_with_count(ModalState::Normal, Pending::None, &b, 1),
            key(KeyCode::Char('0')),
        );
        assert!(matches!(action, Action::PushDigit(0)));
    }

    #[test]
    fn digit_after_count_extends_count() {
        let (_, b) = fixture();
        let action = translate(
            ctx_with_count(ModalState::Normal, Pending::None, &b, 12),
            key(KeyCode::Char('3')),
        );
        // Translate just emits the digit; App accumulates 12 -> 123.
        assert!(matches!(action, Action::PushDigit(3)));
    }

    #[test]
    fn motion_after_count_dispatches_motion() {
        let (_, b) = fixture();
        let action = translate(
            ctx_with_count(ModalState::Normal, Pending::None, &b, 3),
            key(KeyCode::Char('w')),
        );
        // Translate doesn't attach the count -- App applies it on Invoke.
        assert_eq!(invocation_command(&action), Some(b.word_forward.0));
    }

    // ---- Find-char / till-char (f, F, t, T) ----

    #[test]
    fn f_sets_pending_find_forward() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('f')),
        );
        match action {
            Action::SetPending(Pending::AfterFindChar { kind, operator }) => {
                assert_eq!(kind, FindKind::Forward);
                assert!(operator.is_none());
            }
            other => panic!("expected SetPending(AfterFindChar Forward), got {other:?}"),
        }
    }

    #[test]
    fn capital_f_sets_pending_find_backward() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('F')),
        );
        match action {
            Action::SetPending(Pending::AfterFindChar { kind, .. }) => {
                assert_eq!(kind, FindKind::Backward);
            }
            other => panic!("expected SetPending(AfterFindChar Backward), got {other:?}"),
        }
    }

    #[test]
    fn t_sets_pending_till_forward() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('t')),
        );
        match action {
            Action::SetPending(Pending::AfterFindChar { kind, .. }) => {
                assert_eq!(kind, FindKind::TillForward);
            }
            other => panic!("expected SetPending(AfterFindChar TillForward), got {other:?}"),
        }
    }

    #[test]
    fn capital_t_sets_pending_till_backward() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('T')),
        );
        match action {
            Action::SetPending(Pending::AfterFindChar { kind, .. }) => {
                assert_eq!(kind, FindKind::TillBackward);
            }
            other => panic!("expected SetPending(AfterFindChar TillBackward), got {other:?}"),
        }
    }

    #[test]
    fn f_then_char_resolves_to_motion_with_args_char() {
        let (_, b) = fixture();
        let pending = Pending::AfterFindChar {
            kind: FindKind::Forward,
            operator: None,
        };
        let action = translate(
            ctx(ModalState::Normal, pending, &b),
            key(KeyCode::Char('z')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.find_char_forward.0);
                assert_eq!(inv.args, lattice_grammar::Args::Char('z'));
            }
            other => panic!("expected Invoke(find_char_forward), got {other:?}"),
        }
    }

    #[test]
    fn df_then_char_composes_delete_with_find_target() {
        let (_, b) = fixture();
        // First press: `d` in Normal -> AfterOperator(delete).
        let after_d = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('d')),
        );
        let op = match after_d {
            Action::SetPending(Pending::AfterOperator(op)) => op,
            other => panic!("expected SetPending(AfterOperator), got {other:?}"),
        };
        // Second press: `f` in operator-pending -> AfterFindChar with stashed op.
        let after_df = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(op), &b),
            key(KeyCode::Char('f')),
        );
        let pending = match after_df {
            Action::SetPending(p) => p,
            other => panic!("expected SetPending, got {other:?}"),
        };
        // Third press: `x` -> Invoke delete with find_char_forward target.
        let after_dfx = translate(
            ctx(ModalState::Normal, pending, &b),
            key(KeyCode::Char('x')),
        );
        match after_dfx {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.delete.0);
                match inv.target {
                    Some(Target::Motion(id, args)) => {
                        assert_eq!(id, b.find_char_forward);
                        assert_eq!(args, lattice_grammar::Args::Char('x'));
                    }
                    other => panic!("expected motion target, got {other:?}"),
                }
            }
            other => panic!("expected Invoke(delete, find_target), got {other:?}"),
        }
    }

    #[test]
    fn esc_after_find_pending_clears_pending() {
        let (_, b) = fixture();
        let pending = Pending::AfterFindChar {
            kind: FindKind::Forward,
            operator: None,
        };
        let action = translate(ctx(ModalState::Normal, pending, &b), key(KeyCode::Esc));
        assert!(matches!(action, Action::SetPending(Pending::None)));
    }

    // ---- New motions: b, e, ^ ----

    #[test]
    fn b_invokes_word_backward() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('b')),
        );
        assert_eq!(invocation_command(&action), Some(b.word_backward.0));
    }

    #[test]
    fn e_invokes_word_end() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('e')),
        );
        assert_eq!(invocation_command(&action), Some(b.word_end.0));
    }

    #[test]
    fn caret_invokes_first_non_blank() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('^')),
        );
        assert_eq!(invocation_command(&action), Some(b.first_non_blank.0));
    }

    #[test]
    fn db_resolves_to_delete_word_backward() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.delete), &b),
            key(KeyCode::Char('b')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.delete.0);
                match inv.target {
                    Some(Target::Motion(id, _)) => assert_eq!(id, b.word_backward),
                    other => panic!("expected motion target, got {other:?}"),
                }
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn de_resolves_to_delete_word_end() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.delete), &b),
            key(KeyCode::Char('e')),
        );
        match action {
            Action::Invoke(inv) => match inv.target {
                Some(Target::Motion(id, _)) => assert_eq!(id, b.word_end),
                other => panic!("expected motion target, got {other:?}"),
            },
            _ => panic!("expected Invoke"),
        }
    }

    // ---- change operator: c, cw, cc ----

    #[test]
    fn c_sets_pending_operator_change() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('c')),
        );
        match action {
            Action::SetPending(Pending::AfterOperator(op)) => assert_eq!(op, b.change),
            other => panic!("expected SetPending(AfterOperator(change)), got {other:?}"),
        }
    }

    #[test]
    fn cw_resolves_to_change_with_word_forward_target() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.change), &b),
            key(KeyCode::Char('w')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.change.0);
                match inv.target {
                    Some(Target::Motion(id, _)) => assert_eq!(id, b.word_forward),
                    other => panic!("expected motion target, got {other:?}"),
                }
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn cc_resolves_to_change_with_current_line_range() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.change), &b),
            key(KeyCode::Char('c')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.change.0);
                assert_eq!(inv.range, Some(lattice_grammar::Range::CurrentLine));
            }
            _ => panic!("expected Invoke"),
        }
    }

    // ---- yank operator + paste ----

    #[test]
    fn y_sets_pending_operator_yank() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('y')),
        );
        match action {
            Action::SetPending(Pending::AfterOperator(op)) => assert_eq!(op, b.yank),
            other => panic!("expected SetPending(AfterOperator(yank)), got {other:?}"),
        }
    }

    #[test]
    fn yw_resolves_to_yank_word_forward() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.yank), &b),
            key(KeyCode::Char('w')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.yank.0);
                match inv.target {
                    Some(Target::Motion(id, _)) => assert_eq!(id, b.word_forward),
                    other => panic!("expected motion target, got {other:?}"),
                }
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn yy_resolves_to_yank_current_line() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.yank), &b),
            key(KeyCode::Char('y')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.yank.0);
                assert_eq!(inv.range, Some(lattice_grammar::Range::CurrentLine));
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn capital_y_aliases_to_yank_current_line() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('Y')),
        );
        match action {
            Action::Invoke(inv) => {
                assert_eq!(inv.command, b.yank.0);
                assert_eq!(inv.range, Some(lattice_grammar::Range::CurrentLine));
            }
            _ => panic!("expected Invoke"),
        }
    }

    #[test]
    fn p_lowercase_is_paste_after() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('p')),
        );
        assert!(matches!(action, Action::PasteAfter));
    }

    #[test]
    fn p_uppercase_is_paste_before() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::None, &b),
            key(KeyCode::Char('P')),
        );
        assert!(matches!(action, Action::PasteBefore));
    }

    #[test]
    fn dd_is_not_treated_as_change_current_line() {
        // Regression check: the `cc` arm should only fire for op == change.
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.delete), &b),
            key(KeyCode::Char('c')),
        );
        // Delete operator + 'c' key: no specific motion, fallback clears pending.
        assert!(matches!(action, Action::SetPending(Pending::None)));
    }

    #[test]
    fn d_caret_resolves_to_delete_first_non_blank() {
        let (_, b) = fixture();
        let action = translate(
            ctx(ModalState::Normal, Pending::AfterOperator(b.delete), &b),
            key(KeyCode::Char('^')),
        );
        match action {
            Action::Invoke(inv) => match inv.target {
                Some(Target::Motion(id, _)) => assert_eq!(id, b.first_non_blank),
                other => panic!("expected motion target, got {other:?}"),
            },
            _ => panic!("expected Invoke"),
        }
    }

    // ---- Chord-capture (DESIGN.md §B.1, ArgKind::Chord) ----

    #[test]
    fn chord_capture_translates_ctrl_letter_to_chord_token() {
        let (_, b) = fixture();
        let action = translate(ctx_chord_capture(&b), ctrl(KeyCode::Char('c')));
        match action {
            Action::CommandLineAppendChord(s) => assert_eq!(s, "<C-c>"),
            other => panic!("expected CommandLineAppendChord, got {other:?}"),
        }
    }

    #[test]
    fn chord_capture_translates_plain_letter_unwrapped() {
        let (_, b) = fixture();
        let action = translate(ctx_chord_capture(&b), key(KeyCode::Char('g')));
        match action {
            Action::CommandLineAppendChord(s) => assert_eq!(s, "g"),
            other => panic!("expected CommandLineAppendChord, got {other:?}"),
        }
    }

    #[test]
    fn chord_capture_reserves_esc_for_cancel() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_chord_capture(&b), key(KeyCode::Esc)),
            Action::CommandLineCancel
        ));
    }

    #[test]
    fn chord_capture_reserves_enter_for_submit() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_chord_capture(&b), key(KeyCode::Enter)),
            Action::CommandLineSubmit
        ));
    }

    #[test]
    fn chord_capture_reserves_backspace_for_delete_chord() {
        let (_, b) = fixture();
        assert!(matches!(
            translate(ctx_chord_capture(&b), key(KeyCode::Backspace)),
            Action::CommandLineDeleteChord
        ));
    }

    #[test]
    fn chord_capture_translates_special_keys_with_angles() {
        let (_, b) = fixture();
        // Up arrow -- the canonical chord is `<Up>`, not Esc.
        let action = translate(ctx_chord_capture(&b), key(KeyCode::Up));
        match action {
            Action::CommandLineAppendChord(s) => assert_eq!(s, "<Up>"),
            other => panic!("expected CommandLineAppendChord, got {other:?}"),
        }
    }
}
