# Lattice — Implementation Tracker

This doc is the **current-state ledger** for the v1.0 build. It maps every
feature back to its anchor in ../architecture/design.md / CLAUDE.md and shows what's done,
what's in flight, and what's still pending.

Commit history is the authoritative log of *what changed when*. This file is
the authoritative answer to *where are we against the spec*.

When closing a session, update the status of every row touched. When opening
a session, read the **In-progress** and **Up next** sections — those are the
session pickup points.

---

## v1.0 scope at a glance

The four paramount goals from CLAUDE.md (in priority order when they
conflict):

1. **Performance.** Sub-frame keystroke-to-glyph (§8.2 commitments).
2. **Extensibility.** WebAssembly Component Model plugin host (§5.5, §9).
3. **Strict vim modal editing** with one deviation: unified command/grammar
   dispatch (§5.2.1).
4. **Asynchronous, multi-threaded core** that never blocks the UI (§3, §5.7).

The roadmap (§13) is divided into 11 phases (Phase 0 through Phase 10).
Phases 0–3 land the foundation, modal engine, terminal UI, and tree-sitter.
Phases 4–10 add LSP, GPU rendering, plugin host, modes, rich buffer
rendering, and v1.0 polish.

### Build order: core first, plugins later

../architecture/design.md §3.1 codifies the fast-path-vs-orchestration split:
features that fire per-keystroke (LSP wire + dispatch, snippet
engine, picker UI, completion engine, modal grammar) live in
core; opinionated tools built *on top of* the editor (magit
clone, fuzzy file finder, git inline overlays, markdown
preview, test runners) ship as plugins.

Concretely: Phases 4–6 grow the core surface. Phase 7 (plugin
host) lands after, designing WIT against an exercised set of
trait seams rather than speculative ones. Phase 8 / 8b
build the bundled-plugin catalog on top of a stable host.

Reasons this ordering wins:

- **Performance guard.** WASM-call overhead (typed call p99
  < 500ns; round-trip < 5μs) is real on the keystroke
  hot-path. Per-keystroke subsystems amortize that cost
  poorly even with batching.
- **Trait surface quality.** Each phase-4 feature
  (LspSupervisor, Picker, completion AsyncCandidateGenerator,
  active-snippet engine) is itself the trait surface plugins
  will eventually implement against. Designing WIT after we
  see five concrete patterns is far better than after two.
- **No sunk cost.** The crates we've built so far
  (`lattice-lsp`, `lattice-completion`, the picker module)
  are correctly placed -- no migration debt is accumulating.

---

## Phase status

| Phase | Title                                 | Status                   | Notes                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
|-------|---------------------------------------|--------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 0     | Foundation                            | ✅ done                  | Workspace, lattice-core, document/buffer/undo, file I/O, protocol enums                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| 1     | Modal Editing                         | ✅ done                  | Modal engine, full chord routing, motions / operators / text objects / counts / registers / marks / macros / dot-repeat (incl. insert-replay) / search (incl. hlsearch + substitute live preview) / folds / ex-commands (every command -- including `:s` / `:g` / `:v` via `Args::List` -- registered as `ExCommandSpec` peers, dispatched through unified `grammar::execute()` per §5.2.1, §B.2). Blockwise visual: per-row dispatch for `d` / `y` / `c` plus blockwise paste; `>` / `<` indent each line in the block; `I` / `A` enter Insert at the block's left/right column with the typed prefix replicated to every row on Esc. Every operator lands as a single undo unit -- counts on linewise ops (`2dd`, `2>>`), block-visual rectangle ops, and I/A replications all collapse to one `u`. |
| 2     | Terminal UI Bootstrap                 | ✅ done                  | crossterm + ratatui; modal cursor; mode line; gutter                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| 3     | Tree-Sitter                           | ✅ done (Rust/Python/JS/Markdown) + Option B incremental reparse | Highlights wired through a shared `LangRegistry` (process-wide `Arc`); injection callback resolves fenced ` ```rust ``` ` blocks in markdown to the rust config (and any registered language to its config) without per-document copies. Markdown is the dual-grammar split (block + inline). Grammar extension API used by builtins, not yet by plugins. New `Style` variants (`Heading1..6`, `Bold`, `Italic`, `Link`, `Url`, `MarkupRaw`, `Markup`) for precise theme targeting. **Option B (B.1–B.5) lit up incremental reparse + frame-level span cache** — see "Option B: incremental reparse + span cache" below. |
| 4     | LSP                                   | 🚧 in progress (4.5 — wire complete; 3 trigger-UX rows deferred) | `lattice-lsp` crate plus full App-side wiring across five phases. **4.1** foundation: wire + actor + handshake + utf-8/16/32 doc sync + diagnostics broadcast + supervisor + edit-dispatch + open-on-`:e`. **4.2** navigation: hover (`K`), definition family (`gd`/`gD`/`gy`/`gI`), references (`gr`), symbols (`:lsp-symbols` / `:lsp-workspace-symbol` w/ resolve), completion (Insert-mode shell + LSP source + docs popup + `completionItem/resolve` + snippets + ranking + per-language overrides + tree-sitter symbols + path source + commit chars + ghost text + cross-source dedup). **4.3** edits: formatting + rangeFormatting + onTypeFormatting; signatureHelp + Insert autopilot; rename + prepareRename; codeAction + resolve + executeCommand; willSave + willSaveWaitUntil (format-on-save) + didSave; `workspace/applyEdit` inbound channel. **4.4** polish: `window/showMessage` + `showMessageRequest` + `showDocument`, `$/progress` + modeline + cancel, `:lsp-restart`, `documentHighlight` overlay, `selectionRange` + `:lsp-expand/shrink-region`, `foldingRange` + `FoldMethod::Lsp` + `lsp-folding-mode`, `inlayHint` virtual-text overlay, `semanticTokens/full` + `/full/delta` overlay, `textDocument/diagnostic` pull, `workspace/didChangeConfiguration` fan-out from typed-option cascade, `workspace/didChangeWatchedFiles` (notify watcher + globset matcher), `workspace/didCreateFiles`, dynamic `registerCapability` / `unregisterCapability` two-way index, `lsp.log_level` / `lsp.log_capacity` TOML. **4.5** expansion (wire + host complete): call hierarchy (`:lsp-incoming-calls` / `:lsp-outgoing-calls`), type hierarchy (`:lsp-supertypes` / `:lsp-subtypes`), `documentLink` + resolve (`gx`), `codeLens` + resolve (`:lsp-code-lens`), `documentColor` + `colorPresentation` (`:lsp-color-presentation`), `moniker` (`:lsp-moniker`). **Strong-reason deferred at the trigger UX layer**: `linkedEditingRange` (needs shadow-edit machinery), `inlineValue` (needs DAP), `inlineCompletion` (lsp-types `proposed` flag); `willRenameFiles` / `willDeleteFiles` / `inlayHint/resolve` (need editor-driven rename/delete + inlay-interaction UX). All multi-result LSP lookups + `:diagnostics` route through one vertico picker. Cancellation tokens plumbed through every wrapper. M.6 sub-mode cascade: 15 sub-modes (completion, diagnostics, hover, signature, format, rename, symbols, code-action, nav, progress, document-highlight, selection-range, folding, inlay-hint, semantic-tokens) toggle individually or via the `lsp-mode` umbrella. Per-feature matrix in [`../notes/lsp-features.md`](../notes/lsp-features.md). |
| 5     | GPU Rendering Foundation              | 🟡 in progress (5.0–5.4 ✅; 5.B ✅; 5.5.A–G.final ✅; 5.5.H first sweep ✅ (12 dead App-side delegates retired + 2 marked allow(dead_code) for test-only callers); 5.5.LSP.1–5 ✅ (10 LSP request arms migrated host-side: hover / definition / declaration / typeDef / impl / references / signature-help / completion / documentSymbol / workspaceSymbol; drains + picker + jump machinery stay App-side; FollowLink deferred — cross-cuts `do_edit` / `open_external_uri`); 5.5.SNIPPET.1 ✅ (`<C-x><C-s>` snippet expand migrated host-side: `do_snippet_expand_at_cursor` + `expand_snippet` + `snippet_variable_context` + `active_language_id` → `Editor`; App-side keeps 1-line delegates for `expand_snippet_with_lsp_edits` / picker accept / completion accept callers; `Effect::SnippetExpand` routes through `editor.do_snippet_expand_at_cursor()`); 5.5.G.23 ✅ keystone — every operator / motion / text-object / ex-command keystroke now resolves host-side: 4 commits (foundation: `DispatchOutcome.effects` + `foldenable` / `fold_start_at[_any]` / `snap_cursor_past_closed_folds` / `dispatch_blocking` / `effect_mutates[_or_yanks]` / `is_blank_line` → `Editor`; split: `App::apply_effect` factored into wrapper + `apply_effect_app_arms` so the dispatch loop can drain host-emitted effects without double-running `handle_effect`; runners: `run_oil_invocation` / `run_help_invocation` / `run_file_tree_invocation` / `run_read_only_motion` → `Editor`; invoke: `run_invocation` + `run_document_invocation` + `apply_effect_host` recursive `Effect::Many` flatten → `Editor`, `Action::Invoke(inv)` wired to `editor.run_invocation`); `pre_active` / `pre_cursor` snapshot moved before `editor.dispatch` so the State-A hover-popup auto-dismiss hook still sees pre-motion cursor; macros / RepeatLastChange / FindRepeat / Completion family / CommandLine cluster / Picker accept-dismiss / FollowLink / OilNavigateUp / Insert remain App-side pending their own helper chains; 5.6+ open); 5.5.G.23.insert-prep ✅ (`signature_help_trigger_chars` + `on_type_formatting_trigger_chars` + `dedup_rendered_by_text` → `Editor`); 5.5.G.23.next-actions ✅ (`DispatchOutcome.next_actions: Vec<Action>` channel + new `Action::LspOnTypeFormattingRequest(char)` / `Action::LspInsertCompletionRequest` variants for host→App LSP autopilot follow-ups; renderer drains via `self.apply(action)` with `should_quit` short-circuit); 5.5.G.23.insert ✅ (`Action::Insert(s)` host-handled via `editor.do_insert_text` — full Insert-mode keystroke path: buffer edit + dot-repeat recording + block-visual edit accounting + insert-completion live-refresh + SignatureHelp / OnTypeFormatting autopilots; LSP follow-ups defer through `out.next_actions`; `maybe_refresh_insert_completion_after_edit` migrated host-side, emits `LspInsertCompletionRequest` on `isIncomplete`); 5.5.G.23.macros ✅ (`Action::PlayMacro` / `PlayLastMacro` / `RepeatLastChange` / `FindRepeat` host-handled — `do_play_macro` pushes recorded actions through `next_actions` with `should_quit` short-circuit; dot-repeat re-dispatches through host's `run_invocation` + replays the captured Insert tail through host's `do_insert_text`; find-repeat synthesises a `CommandInvocation`; bug-fix: `Effect::EnterMode(mode)` added to host's `handle_effect` so post-`run_invocation` modal checks see the flipped state synchronously); 5.5.G.23.cmdline-prep ✅ (cmdline completion helpers: `compute_completion_state` + `refresh_completion_popup` + `open_completion_popup` + `completion_auto_insert_single` + `CompletionComputeError` + `prefer_aliases_for_command_candidates` + `subsequence_match_ranges` → `Editor` / host fns); 5.5.G.23.cmdline ✅ (every `:`-line keystroke now resolves host-side: 8-arm cluster — Append / Backspace / Submit / Clear / DeleteWordBackward / DescribeUnderCursor / AppendChord / CompleteOrAdvance — host-handled via `editor.do_command_line_*` methods; `execute_ex_line` + `try_resolve_missing_arg_prompt` + `chord_capture_active` + `refresh_substitute_preview` + `collect_substitute_matches_for_line` + `delete_trailing_word` + `COMMAND_HISTORY_CAP` + `MissingArgPrompt` all migrated; Submit dispatches through host's `execute_ex_line` which routes ex-command effects via `apply_effect_host`). **App::apply remaining (13 explicit arms, all renderer-coupled)**: 2 host-emit landing zones (`LspOnTypeFormattingRequest` + `LspInsertCompletionRequest` — intentionally App-side as the host→renderer LSP autopilot channel); 8 Completion arms (`CompletionTrigger` / `Next` / `Prev` / `Accept` / `ToggleDocs` / `FilterToSource` / `FilterClear` / `AcceptThenInsert` — deep deps on `populate_insert_completion_sync` / `lsp_completion_meta_for` / `refilter_insert_completion` / `effective_completion_for` / `effective_commit_chars_for` / `do_completion_resolve_focused` + LSP async machinery); 2 Picker arms (`PickerAccept` / `PickerDismiss` — file-open + SMR queue + `activate_buffer` chain); 1 `LspFollowLinkAtCursor` (cross-cuts `do_edit` + `open_external_uri`); 1 `FollowLink` (BufferKind-dispatched into 4 kind-specific helpers); 1 `OilNavigateUp` (`do_open_oil` chain). Each pulls in 2–5 App-side helpers with their own LSP-async / file-open / popup chains; estimate 5–10 more focused commit chains to genuinely empty App::apply. 5.5.G.24 ✅ AppEffect router collapse: `Editor::apply_app_effect(app, out)` in `lattice-host::dispatch` mutates editor fields directly for `AbsorbOperatorPrefix` (only structural arm — `pending_count` → `op_count`, `partial_chord.extend(prefix)` via host-side `keymap_normal::operator_prefix`) and pushes a follow-up `Action` into `out.next_actions` for every other variant; `Effect::AppAction(app)` wired into host's `handle_effect`; App-side `apply_app_effect` retired entirely; `Effect::AppAction(_)` in `apply_effect_app_arms` collapses to grouped no-op. The unified deferred-Action channel (`next_actions`) now handles macro replay + dot-repeat + AppEffect-derived follow-ups under a single drain in the renderer's `apply` wrapper. 5.5.H sweep ✅ (reframed): the 13 remaining App::apply arms are documented in `App::apply` as the **intentional architectural seam**, not migration debt — every one is a renderer-coupled landing zone (LSP autopilot, completion popup + LSP async resolve, picker SMR + file-open, follow-link / external-uri / BufferKind-dispatch). The `lattice-ui-gpui` peer renderer will implement its own equivalents on its own App-equivalent against its own LSP runtime / window-open / picker UI. Dispatch surface is gpui-ready. **5.6 ✅** pane-provider mode-walk lookup → host: new `lattice_host::pane_render` module with `trait ProviderLookup { fn has_provider(&self, mode: ModeId) -> bool; }` + `pub fn resolve_pane_render_mode<L: ProviderLookup>(editor, buffer_id, lookup) -> Option<ModeId>` — host owns the algorithm (walk active minors most-recently-activated first, then major); `lattice-ui-tui::pane_render::PaneRenderRegistry` implements `ProviderLookup` (single `contains_key` probe, no v-table on the paint hot path); `App::pane_render_provider` collapses to two lines (resolve mode host-side, fetch TUI-typed provider from registry). A future `lattice-ui-gpui` ships its own typed registry + `ProviderLookup` impl and reuses the host walk verbatim. **5.7.A ✅** GPUI peer-renderer scaffold: new `lattice-ui-gpui` crate with `GpuiTheme` (stub theme cache), `GpuiPaneRenderRegistry` (impl `lattice_host::pane_render::ProviderLookup` via HashSet probe), `GpuiRenderer` (impl `lattice_host::Renderer` — surfaces renderer-specific assoc types `Theme` / `PaneRenderRegistry` to the host's renderer-trait machinery), `GpuiApp { editor, theme, pane_render_registry }` (peer composition root in the same shape as `lattice-ui-tui::app::App`). `gpui = "0.2.2"` dep gated behind a `window` Cargo feature; scaffold lib + tests build everywhere (incl. headless CI / WSL2 without display libs). Placeholder window binary (`src/bin/lattice_gpui.rs`, `required-features = ["window"]`) opens a 720x480 native window with "Lattice (GPUI) — 5.7 scaffold" centred text — opt-in for hosts with `libxcb1-dev` / `libxkbcommon-dev` / `libxkbcommon-x11-dev` installed; verified to compile under `--features window`. The host-substrate decoupling claim is now provable: `lattice-ui-gpui` depends only on `lattice-host` + `lattice-core` + `lattice-mode`, never on `lattice-ui-tui`. Boot logic (LSP subsystem, command/mode/snippet registries, event bus) is renderer-neutral and slated to migrate to `Editor::boot` in a future slice; until then `GpuiApp::new` uses `Editor::default`. 5.7.B (real input dispatch + paint wiring) + 5.8+ feature parity + 5.9 CLI renderer-select flags follow. Tests: 185 host + 1419 ui-tui + 1 ui-gpui = 1605 green | Phase 5 splits the host out of `lattice-ui-tui` so GPUI can land as a peer renderer. Plan: [`../archive/phase-5-extraction.md`](../archive/phase-5-extraction.md). **Status:** ~24k LoC migrated from `lattice-ui-tui` to `lattice-host`/`lattice-core`. 5.0 audit ✅, 5.1 empty crate ✅, 5.2 HOST modules ✅ (keymap catalogs closed alongside 5.4), 5.3 renderer-neutral theme ✅, 5.4 chord + input + keymap-family split ✅ (5 commits — leaf translators, normal-mode dispatch, keymap_normal, keymap_{insert,visual,replace}, input.rs entry point). **5.B App→Editor composition** ([`../archive/phase-5b-app-design.md`](../archive/phase-5b-app-design.md)) ✅: pivoted from Option D (generics) to Option E (composition) in 5.B.3; clusters 1–12 from the plan shipped across 5.B.4 → 5.B.19 plus a C1–C5 wave (cmdline completion, visible-highlights, popup snapshot, pane_tree, document/snapshot_cache, folds, LSP file watcher); final tail (completion cluster #8 remainder + popup back-stack + `pending_config_structural_sections`) closed in 5.B.20. `App` shrank from ~6k → 4,040 LoC and now holds just four fields (`editor`, `pane_render_registry`, `theme`, `lsp_file_watcher`); `Editor` is 822 LoC. Tests: 1424 ui-tui + 177 host + 180 lsp = 1781. **Remaining (post-5.4 plan revision)**: 5.5 dispatch extraction (`App::apply` → `Editor::dispatch`, the last big architectural unlock for parallel GPUI work — focused design doc [`../archive/phase-5-dispatch-extraction.md`](../archive/phase-5-dispatch-extraction.md)), 5.6 pane-provider lookup → host (small mechanical move), 5.7 `lattice-ui-gpui` scaffold, 5.8+ GPUI feature parity, 5.9 CLI renderer-select flags. The old 5.5 (trait-objected PaneRenderer) and 5.6 (`lattice-render` crate) are dropped — both were artefacts of the pre-Option-E `App<R>` plan that the 5.B composition pivot retired. |
| 6     | Document Renderer + UI Components     | ⛔ not started           | Popups, pickers, panels-as-buffers all live in §5.9                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| 7     | Plugin Host                           | ⛔ not started           | wasmtime + Component Model + WIT scaffolding                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| 8     | Major/Minor Modes + Reference Plugins | ⛔ not started           | Major / minor modes are themselves plugins (§5.8.3)                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| 8b    | Bundled plugins                       | ⛔ not started           | Curated set of first-party WASM Component plugins shipping with the editor binary -- LSP server manager (lighthouse), plugin manager, fuzzy-finder, project grep, git client, snippet engine, editing helpers, diff viewer, outline sidebar, format-on-save, test runner, markdown preview. Each crate lives at `crates/lattice-plugin-<name>/`. See ../architecture/design.md §5.5.6 for the strategy + the seven WIT prerequisites Phase 7 must expose. Depends on Phase 7. |
| 9     | Rich Buffer Rendering                 | ⛔ not started           | Per-line shaped path, Fenwick height index                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| 10    | Polish + v1.0                         | ⛔ not started           | `*scratch:rust*` live-eval workflow (§10), accessibility, packaging, themes                                                                                                                                                                                                                                                                                                                                                                                                                                          |

Active focus: **Phase 5.8 GPUI polish + feature parity** (most
recent block of work, 2026-05-22 — see "Recent Phase 5.8 polish
slices" below). **Phase 4 (LSP) wind-down complete** at the
wire+host layer (4.5 ✅; 5 trigger-UX rows strong-reason
deferred). **Phase 7 (Plugin Host) on deck** as the next
architectural step.

Recent Phase 5.8 polish slices (2026-05-22)
-------------------------------------------

A two-day burst of user-driven bug fixes + small features
brought the GPUI peer to functional parity with TUI for the
window-management surface. Each slice is a single commit:

- **#16** (`cb63f11`) — vim chord clear-guard fixed: `gg`,
  `zz`, `zt`, `zb`, `dw` etc. broke when `EnsureCursorVisible`
  cleared `partial_chord` between keystrokes. Renderer-housekeeping
  actions now exempt from the clear-guard. Regression tests
  landed.
- **#17** (`835a6d5`) — GPUI viewport chrome math: pixel-
  accurate calc of pane padding + status row + global bottom
  padding so cursor doesn't park under modeline.
- **#18** (`525f2f3`) — LSP popup placement: GPUI honors
  `PopupPlacement::CursorAnchored` instead of always centring.
- **#19 / #21** (`4d29ace`) — State-B popup focus visual
  indicator (brighter border + hidden document cursor +
  header hint).
- **#20** (`039881c`) — hover popup auto-dismiss on cursor
  motion hoisted into `Editor::dispatch` (works in both peers).
- **#22** (`ade38c6`) — cmdline-completion `<C-n>` / `<C-p>`
  symmetric with picker.
- **#23 / messages overhaul** (`e5cf4d2`) — tracing routed
  to BOTH stderr AND `*messages*` buffer via composed fmt +
  MessagesLayer; CLI `-v` / `-q` / `--log-level` flags via
  `set_boot_log_level`.
- **#24** (`60c619e`) — `:describe-buffer` no longer scrolls
  background document: GPUI uses `pane.scroll` / `pane.cursor`
  when popup owns active_buffer.
- **#25** (`100c33f`) — multi-pane viewport math + per-pane
  `visible_spans` so highlights work after splits.
- **#26 / popup-anchor** (`60c619e`) — popup pinned to the
  symbol it was invoked from (CursorAnchored reads
  `popup_substate.anchor`, not live `ad.cursor`).
- **per-pane viewport** (`a446382`) — `PaneState.viewport_height`
  + `PaneState.viewport_width`, set per-leaf by the
  renderer's per-frame layout pass; replaces the single
  global `Editor::viewport_height`. Active leaf's height is
  auto-mirrored back into `Editor::viewport_height` for the
  cursor-clamp / highlights-worker code paths.
- **#27** (`e91298a`, `cb0111d`) — critical mouse-focus
  regression: `set_pane_viewport` was firing N actor RPCs
  per frame, flooding the actor and blocking the GPUI main
  thread. Diff-then-send so steady state fires 0 commands.
- **#28** (`3204029`) — split ratios (`<C-w>=`, `<C-w>+`,
  `<C-w>-`, `<C-w>>`, `<C-w><`); `PaneNode::*Split` gains a
  `ratio: f32` field; ratio-aware `compute_rects_recursive`
  + GPUI `collect_pane_geometries`.
- **#29 tabs slice 1** (`57a3978`) — `TabId` / `TabSlot` /
  `Editor.tabs` + Action variants (NextTab / PrevTab /
  GoToTab / NewTab / CloseTab) + chord bindings (`gt`, `gT`,
  `{N}gt`) + ex commands (`:tabnew`, `:tabnext`, `:tabprev`,
  `:tabclose`). Count plumbing via `run_invocation` intercept.
- **#29 tabs slice 2** (`b19d228`) — tabline rendering in
  both peers; new `Tabline` option group + `tabline.show`
  typed-enum (`never` / `auto` / `always`).
- **#29 tabs slice 3** (`f138661`) — `:tabonly`, `:tabmove`,
  `:tabnew <path>`, GPUI mouse-click on tab, `<C-PageDown>` /
  `<C-PageUp>` aliases.
- **#32 picker open-target** (`ea7891e`) — picker `<C-s>` /
  `<C-v>` / `<C-t>` open file candidate in split / vsplit /
  tab; one-shot override via `picker_open_target` field with
  `mem::take` at `do_picker_accept` entry + restore-before-call
  pattern for all 7 inner `apply_picker_outcome` sites.
  Regression test: `picker_open_target_resets_after_no_op_accept`.
- **#33 picker legacy-path + GPUI symbolic-key adapter**
  (`ea1e546`) — GPUI's `keystroke.key` uses names like
  `"equal"` / `"plus"` / `"greater"`; added symbol-name map
  so `<C-w>=` etc. resolve. Picker's legacy imperative arms
  (`:b` buffer picker uses these) now also honor the
  open-target override.
- **Font config** (`a224e52`) — `ui.font_family` /
  `ui.font_size` options; GPUI cursor alignment fix.

Plugin-facing API surface preserved at four layers across all
slices: Editor methods (`do_*_tab`, `do_split_pane`, etc.);
typed `Action` variants; `AppEffect` variants; named
`CommandId`s (`action:next-tab`, `action:picker-accept-in-split`,
etc.). Plugin authors compose by any of the four.

The natural next architectural step is **Phase 7 (Plugin
Host)**: paramount goal #2 (extensibility) is the most-deferred
goal today, and Phase 8 (modes-as-plugins), Phase 8b (bundled
plugins), and the user `init.rs` → WASM config path all gate on
it. The rustfmt + rustdoc cleanup just landed gives the WIT
surface a clean baseline to grow against.

LSP docs are comprehensive across audiences: design-doc readers
(`../architecture/design.md` §5.4), implementers
([`../architecture/lsp-architecture.md`](../architecture/lsp-architecture.md)),
users ([`../../user/lsp.md`](../../user/lsp.md) +
[`../../user/lsp-mode.md`](../../user/lsp-mode.md)), per-feature
trackers ([`../notes/lsp-features.md`](../notes/lsp-features.md) --
every LSP 3.17 capability with status), and a manual verification
checklist ([`verify.md`](verify.md) §17–18 covering all 15
sub-modes + 4.4/4.5 features).

Phase 4 roadmap (history): 4.1 wire + actor + sync + diagnostics →
4.2 navigation + completion → 4.3 edits (rename / format / code
action / will-save) → 4.4 polish (semantic tokens delta, inlay
hints, folding, document highlight, selection range, pull
diagnostics, dynamic registration, file watcher, configuration
fan-out, progress, server-initiated UX) → 4.5 expansion (call /
type hierarchy, code lens, document link, document color,
moniker).

---

## Option B: incremental reparse + span cache

**The keystroke-to-fresh-tree architecture** specified in
../architecture/design.md §5.3 / §8.2: every edit produces an `EditDelta`,
threaded to the syntax worker which applies `tree.edit()` and
runs `Parser::parse(.., Some(&old_tree))` for incremental
reparse. The frame-level span cache turns steady-state
re-highlighting into a no-op.

Originally diagnosed against a user-visible bug ("after `>>`
the highlighting breaks instantly; characters bleed into stale
colours"). Root cause: spans computed against an old syntax
snapshot were being painted onto current document bytes; the
spans never caught up because (a) full reparse was the only
path, and (b) `document.text()` allocated the full buffer on
the input thread per keystroke. Both fixed.

| Slice | Status | What landed                                                                                                                                                                                                                                                                                                                                                       |
|-------|--------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| B.1   | ✅     | `Buffer::apply_edit` returns an `AppliedEdit { ..., delta: EditDelta }`. New `EditDelta` type in `lattice-protocol::edit` carrying tree-sitter-shaped byte/Position deltas. Six u32 writes + three Position copies at the tail of `apply_edit` -- bench `input_edit_construction` measures **1.87ns**, at §8.2's ~2ns floor. Pure-data slice.                                                                          |
| B.2/1 | ✅     | `Syntax::parse_at_with_edits(source, text_version, from_version, edits)` applies each delta via `tree.edit()` then `Parser::parse(_, Some(&old_tree))`. Layered guards fall back to full reparse on any inconsistency (no cached tree, from_version mismatch, byte-length mismatch). `SyntaxHandle` worker coalesces queued requests by accumulating edits in arrival order, capped at 256 per burst. |
| B.2/2 | ✅     | App accumulates `EditDelta`s on `pending_syntax_edits` via the four edit chokepoints (`apply_edit_blocking`, `apply_edit_batch_blocking`, `undo_blocking`, `redo_blocking`). `maybe_reparse_syntax` drains and ships them to the worker. Per-document `last_synced_syntax_version` on App + `DocumentEntry`. Includes the `apply_edit_*` family's `&self → &mut self` refactor (no interior mutability introduced). |
| B.3   | ✅     | Frame-level highlight span cache on App, keyed `(snapshot_ptr, text_version, scroll, viewport_height, fold_hash)`. Cache hit at **20ns flat** across all corpus sizes -- **8900× speedup** vs the pre-B.3 ~178µs full QueryCursor walk every frame. Steady-state norm (cursor blinking, no edit) → ~100% hit rate. Load-bearing for paramount goal #1's strict reading on the steady-state floor. |
| B.4   | ✅     | Parametrized parity matrix in `lattice-syntax::syntax::tests`: 27 tests pinning `incremental tree.to_sexp() == full-reparse tree.to_sexp()` across edge positions, multi-line shape changes, whitespace-only edits, sequential batches, per-language coverage (Rust / Python / JavaScript / Markdown), pathological / minimal-buffer cases. Hardens the silent-wrong-tree surface. |
| B.5   | ✅     | `ReparseRequest` carries `Buffer` (O(1) Arc-bump clone via ropey's internal sharing) instead of pre-materialized `String`. Worker calls `buffer.as_string()` on `spawn_blocking` thread. Bench `clone_vs_text`: **7.7ns** flat across sizes vs. pre-B.5 path's 79ns / 990ns / 189µs (10 / 1k / 100k lines). **24,500× faster at 100k lines** -- closes the last input-thread allocation in the syntax-reparse hot path per goal #1. |

**Combined post-Option-B per-keystroke cost on the input
thread**: ~100ns (Buffer::clone + Vec::take + mpsc send +
counter bump). The actual reparse (tree.edit + Parser::parse)
runs on the worker's `spawn_blocking` thread, ~50µs at 1600
lines / ~100µs at 16k lines.

**Combined steady-state per-frame cost on the input thread**:
~33µs (compose 13µs + cache check 20ns), down from ~192µs
pre-Option-B (the ~178µs `highlight_lines` walk every frame
was wasted work whenever nothing changed).

Bench rows live in `benchmarks.md` under "Native highlight"
(B.2/B.4 incremental rows), "Frame render" (B.3 cache rows),
and "Buffer ops" (B.1/B.5 rows). §8.2 floor-rows updated with
measured numbers.

Two follow-ups deliberately deferred:

- **Per-pattern QueryCursor caching** (benchmarks.md "Improvement
  target" on `highlight::rust/200`): drops the cache-miss highlight
  cost when an edit invalidates the span cache. Lower priority --
  cache miss only fires on actual changes.
- **Per-pane edit accumulation** for inactive panes (currently
  falls back to full reparse on the inactive-pane refresh path).
  Rare in practice; bounded.

---

## C-series: runtime fixes + flicker elimination

A follow-up to Option B that turned the algorithmically-correct
incremental-reparse pipeline into a user-visibly-flicker-free
syntax-highlighting experience. Originally diagnosed against a
specific user complaint: "even after Option B landed, syntax
highlighting still goes through a brief 'gone, then back' phase
on every edit -- especially noticeable on `>>` and `dd`. Vim has
zero visual indication of any update. We should match that bar."

The story across five slices:

| Slice | Status | What landed |
|-------|--------|-------------|
| C.1   | ✅     | `#[tokio::main]` on `lattice-cli`. Pre-C.1, `main` was synchronous and `tokio::runtime::Handle::try_current()` failed silently inside `App::new`, causing `SyntaxHandle::seeded`'s worker to never spawn. Option B's entire incremental-reparse pipeline was routing through a non-existent worker -- spans stayed at the initial-seeded snapshot forever. Also adds nested-runtime-safe `block_on` (via `block_in_place`) so `apply_edit_blocking` etc. still work from inside the runtime. |
| C.2   | ✅     | Worker publishes an intermediate snapshot AFTER `tree.edit()` shifts byte ranges, BEFORE running `Parser::parse`. The intermediate has the new source + tree-edited tree (byte ranges aligned with new content; tree shape pre-parse for the changed regions). Renderers see byte-aligned spans during the entire parse window -- lines below a delete or after a multi-byte insert paint at correct positions immediately. Splits `parse_at_with_edits` into `try_apply_intermediate` + `reparse_with_cached_tree`. |
| C.3   | ✅     | Two complementary fixes: `shift_highlights_for_edit` runs synchronously in `publish_document_changed` to keep `App.visible_highlights` line-aligned across line-deletes / line-inserts (drains entries on delete; inserts empty placeholders on insert). `refresh_highlights` HOLDs the existing spans when `snap.text_version() < document.text_version()` instead of recomputing against stale data -- the cache only updates from one CORRECT set to another, never through an empty intermediate. |
| C.4   | ✅     | Byte-shift spans within the affected line for in-line edits. `>>` (insert "    " at line start) shifts every span on the line right by 4 bytes immediately. Crossing spans get their end extended/contracted; spans collapsed to empty get dropped. Held spans on frame N+1 are byte-aligned with new content, identical to what the worker's recompute will produce on frame N+2 -- zero visual transition. |
| C.5   | ✅     | **The missing link.** Grammar-driven edits (`>>`, `dd`, `cc`, `c`, `y`, `x`, `D`, `C`, `Y`, every operator) flow through the dispatcher → actor → `Effect::Edits(applied)` → `App::handle_edits`, which previously only updated the cursor. They bypassed `publish_document_changed` entirely, so all the C.1–C.4 machinery had no effect on the most common edit shape (operators). Routing them through the chokepoint fixed three things at once: spans byte-shift on input thread, `pending_syntax_edits` accumulates so the worker does incremental reparse instead of falling back to full, AND LSP `didChange` fires for operator edits (server-side document drift on `>>` / `dd` is gone). |

**User-visible result.** The complete chain (B + C) gives:

- **No flicker on `>>`** — the indented line's spans byte-shift
  immediately on the input thread; the worker's recompute lands
  with identical spans; the renderer never paints a transition.
- **No flicker on `dd`** — the deleted line's entry drains;
  lines below inherit their (still-correct) spans at their new
  indices; the worker's recompute confirms.
- **No flicker on typing** — single-char insert/delete byte-shifts
  spans on the affected line; the user sees colors track the
  cursor's edit point continuously.
- **Steady-state highlight cost stays at ~20ns/frame** (the B.3
  cache hit). The C-series is correctness-not-perf for the hot
  path; it adds a sub-µs `shift_highlights_for_edit` call per
  edit but no per-frame overhead.

**Implementation surface.**

- `lattice-cli/src/main.rs`: `#[tokio::main(flavor = "multi_thread")]`.
- `lattice-runtime/src/runtime.rs::block_on`: `block_in_place`
  wrap when nested in a runtime context.
- `lattice-syntax/src/handle.rs::worker_main`: two-stage parse
  with intermediate `ArcSwap::store` between `try_apply_intermediate`
  and `reparse_with_cached_tree`. New `seeded_with_runtime`
  constructor (kept alongside `seeded` for tests in tokio
  context).
- `lattice-syntax/src/syntax.rs`: split `parse_at_with_edits`
  into `try_apply_intermediate` (fast, no parse) +
  `reparse_with_cached_tree` (slow, parse). The convenience
  method runs both back-to-back.
- `lattice-ui-tui/src/app/highlights.rs`:
  - `App.visible_highlights_key`: cache key holds `syntax_text_version`
    only (not `document_text_version`) so edits don't trigger
    recomputation against stale snapshots.
  - `refresh_highlights` stale-snapshot HOLD path.
  - `shift_highlights_for_edit` — line-shift on edit.
  - `shift_spans_within_line` — byte-shift on in-line edit.
- `lattice-ui-tui/src/app/dispatch.rs`:
  - `handle_edits` — routes grammar-driven edits through
    `publish_document_changed` (the latter lives in
    `app/lifecycle.rs`).

Three follow-ups deliberately deferred (open):

- **LSP-side staleness symptoms**: diagnostics retention (`:diagnostics`
  shows errors that no longer exist), hover-position offset on
  rapid edits. C.5's `didChange`-now-fires-for-operators may have
  partially fixed hover staleness as a side effect; diagnostics
  retention is a separate decoration-pipeline bug.
- **Per-pattern QueryCursor caching**: the benchmarks.md improvement
  target on `highlight::rust/200` (3.2ms → ~1ms achievable). Cache
  miss only fires on actual changes; lower priority.
- **Per-DocumentEntry edit accumulation** for inactive panes:
  currently they fall back to full reparse on the rare cross-doc
  refresh path. Bounded scope.

---

## B': mode-owned synthetic buffers

**The "modes own their buffers" contract** specified in
../architecture/design.md §5.10.5: every synthetic buffer
(`*lsp*`, per-instance `*lsp:<server>:<workspace>*`, per-instance
`*...:trace*`, `*messages*`, future `*scratch*` / `*compilation*`
/ REPL / plugin-emitted streams) is owned by a dedicated mode
end-to-end. The App holds no subsystem-specific buffer-handling
code; two host primitives -- `BufferStore` + `ServiceRegistry` --
carry the contract. Anchors: design.md §5.10.5 + §5.4.7 +
mode-architecture.md §7.

Originally motivated by user-visible bugs (modeline `[no name]`
on `:lsp-log`, `<C-o>` "no jumps" from synthetic buffers, missing
per-instance separation for multiple `rust-analyzer` processes
against different workspaces) plus the architectural observation
that `App::ensure_lsp_subsystem_buffer` / `App::drain_lsp_log_events`
/ `App::append_to_owned_buffer` were anti-extensible: every new
subsystem would copy-paste into the App. The recipe needs to be
mode-shaped instead.

| Slice | Status | What landed / what lands                                                                                                                                                                                                                                                                                                                                                       |
|-------|--------|-------------|
| B'.1a | ✅     | `BufferStore` trait + `BufferStoreHandle` defined in `lattice-mode`. `Send + Sync` so a mode's tokio task can call from any thread. Three methods: `find_by_name`, `ensure_named_document(name, major, flags)`, `handle_for(id)`. No implementation yet -- the trait is the contract surface for B'.3 onward.                                                                  |
| B'.1b | ✅     | `BufferRegistry` switched to interior mutability (`Arc<Mutex<BufferRegistryInner>>`). Methods take `&self` and lock internally; the type is `Clone` (Arc bump) so it threads naturally into `BufferStoreHandle`. Callback-based read/write API (`with_entry`, `with_help`, `with_oil`, `with_file_tree`, `for_each`) replaces reference returns -- guards never escape the lock. 30+ App-side call sites migrated; 1574 lattice-ui-tui tests stay green. Sharp edges to honour: no lock-across-`.await`, no re-entrant `with_X(\|_\| with_Y(...))` (`std::sync::Mutex` is not reentrant). |
| B'.2  | ✅     | Per-instance LSP logger keying. `InstanceKey { server_id: Arc<str>, workspace: Arc<Path> }` is the new canonical key; `LspLogger`'s per-server map becomes per-instance. `LogRecord` and `LspLogPushed` carry workspace alongside server_id. All four `Inbound*` bus payloads (applyEdit / configuration / showDocument / showMessageRequest) carry workspace so the App-side drain can route logs and side-effects to the correct instance. `ServerHandle::instance()` / `::workspace_root()` expose the canonical pair. App ex-commands (`:lsp-log-level` / `:lsp-log-clear` / `:lsp-trace`) walk running actors + the logger's `known_instances`, with a cwd-synth fallback so pre-spawn toggling still works. 1574 lattice-ui-tui tests pass. |
| B'.3  | ✅     | `LspLogMode` owns `*lsp*`. Hand-written `Mode` impl: `on_activate` pulls the buffer's `DocumentHandle` via the `BufferStoreHandle` service, subscribes to `LspLogPushed` events, spawns a tokio task that drains the subscription, coalesces, and applies one batched edit per drain cycle. `on_deactivate` removes the buffer-local subscription stash + unsubscribes. The App's `drain_lsp_log_events` skips the subsystem write when `LspLogMode` is the active major on `*lsp*`. `BufferStore` impl for `BufferRegistry`; `BufferStoreHandle` registered in `ServiceRegistry` at boot. `format_log_event_line` hoisted to `lattice-lsp::logging` so the mode can reuse it. Mode degrades gracefully when no service or runtime is wired (test paths). |
| B'.4  | ✅     | `LspServerLogMode` owns per-instance `*lsp:<server>:<workspace>*`. Hand-written `Mode` impl reads its `InstanceKey` from the `LspServerLogInstance` buffer-local (seeded by App before activation), subscribes to `LspLogPushed`, filters by `(server_id, workspace)`, skips trace records, batches appends. Buffer name + ensure helpers take `&InstanceKey`; App drain accumulates by `(server_id, workspace)` and skips per-server write when the mode is the active major. Both B'.3 and B'.4 modes also seed their buffer from the in-memory ring on activate so pre-existing records show up immediately; `LspLogger` registered as a service for the seed path. `open_lsp_log_in_pane` / `open_lsp_trace_log_in_pane` use a `resolve_lsp_instance_for` helper (running actor → known-by-logger → cwd-synth fallback). |
| B'.5  | ✅     | `LspTraceLogMode` owns per-instance `*lsp:<server>:<workspace>:trace*`. Hand-written mirror of B'.4 with inverted filter (`level == "trace" \|\| source == "trace"`). Reuses the `LspServerLogInstance` buffer-local for instance identity; tracks its own subscription via `LspTraceLogSubscription` so a single instance can have both buffers open simultaneously. Seeds from the in-memory ring on activate. App drain skips per-trace write when the mode is the active major. Retired the `lsp_log_mode!` macro since all three log majors are now hand-written. |
| B'.6  | ✅     | App LSP cleanup. `App::drain_lsp_log_events` slimmed to only fan `window/showMessage`-sourced records to the minibuffer -- every buffer append is mode-owned now (B'.3 / B'.4 / B'.5). `format_log_event_line` alias retired from `lattice-ui-tui` (canonical is in `lattice-lsp`). `App::ensure_lsp_*` buffer creators and `append_to_owned_buffer` stay -- still used at boot (subsystem buffer) and by ex-commands (per-instance lazy create); the modes activate on top. Added `log_append_pipeline_100_records` Criterion bench in `lattice-lsp/benches/lsp.rs`: publish 100 records through `LspLogger → EventBus → mpsc → coalescing drain → format`, measure wall time. Baseline ~87µs / 100 records (≈870ns / record) on dev hardware. Excludes the `apply_edit_batch` tail (depends on `lattice-grammar`, out of scope for this crate's deps); the document-actor benches already measure that side. |
| B'.7  | ✅     | `:lsp-log` / `:lsp-server-log` / `:lsp-trace-log` reshaped as thin wrappers. Canonical name builders + inverse parsers moved into `lattice-lsp::buffer_names` (`LSP_SUBSYSTEM_LOG_NAME`, `lsp_server_log_name(&InstanceKey)`, `lsp_server_trace_log_name(&InstanceKey)`, `parse_lsp_server_log_name(&str) -> Option<InstanceKey>`, `parse_lsp_trace_log_name`). `BufferStore::name_for(id) -> Option<String>` added so modes can read their buffer's synthetic name. `LspServerLogMode::on_activate` / `LspTraceLogMode::on_activate` now derive their `InstanceKey` straight from the name — the `LspServerLogInstance` buffer-local + the App-side seeding before activation are gone. App side collapsed `ensure_lsp_subsystem_log_buffer` / `ensure_lsp_server_log_buffer` / `ensure_lsp_server_trace_buffer` / `ensure_lsp_log_buffer_with_instance` / `ensure_lsp_log_owned_buffer` into a single generic `ensure_named_synthetic_document(name, mode_id, flags) -> BufferId` host helper. Ex-command handlers (`do_open_lsp_log`, `open_lsp_log_in_pane`, `open_lsp_trace_log_in_pane`) + the `:lsp-trace` toggle path + the boot path + the `*messages*` creator all route through that one helper; the handler computes `name` via `lattice-lsp` and picks the major-mode id, and that's all the subsystem-shaped knowledge it has. Drains of `drain_lsp_log_events` removed from the open paths (modes drive buffer content from the event bus, independent of that minibuffer-only drain). 1574 lattice-ui-tui tests + 183 lattice-lsp tests stay green. |

**Design-call answers baked in (per architect's confirmations):**

- *Buffer name canonicalization* — full workspace path as the
  canonical registry key; display label may shorten with `~/`.
- *Server-detach lifecycle* — the mode's `on_deactivate` runs
  when the supervisor signals server-exit; the subscription
  drops; the buffer survives (frozen — no further appends).
  `:bd` is the explicit removal path.
- *Memory cap* — unbounded transcript by default; `:lsp-log clear`
  drops the rope. Matches user mental model ("the buffer is the
  full log").
- *Subscription strategy* — one publisher (`LspLogPushed`), three
  filtering subscribers (one per mode). Simpler than three
  sub-publishers; tokio `broadcast` is the fan-out channel.
- *Bench coverage* — single cross-task append-throughput bench in
  B'.6 alongside the App-side delete, so regressions show up in
  the same slice that removes the old fallback path.

---

## M-async — async mode lifecycle (v1 commitment)

**Async mode lifecycle** specified in mode-architecture.md §7.1.
`Mode::on_activate` returns a `LifecycleFuture<'_, Self::Guard>`
that the dispatcher schedules on the runtime; deactivation drops
the typed Guard, firing its `Drop` impl for synchronous cleanup
(async cleanup uses `spawn_task` fire-and-forget — Zed's pattern).
The UI thread never blocks on mode activation. Hooks that need to
spawn a supervisor, await a handshake, drain a watcher, or close
a server connection do so naturally with `.await`.

**Why v1, not post-v1.** Paramount goal #1 (sub-frame input
latency) and paramount goal #4 (asynchronicity) both demand
that no synchronous path on the App thread does I/O. Today's
`LspMode::on_activate` is the empirical counter-example: first
`cargo run` blocks measurably while the rust-analyzer supervisor
spawns + initialises. Subsequent runs don't block because the
work is cached. The architectural fix is async lifecycle with
typed Guards, not caching.

**Sequence:** M-async lands *after* B' (committed at `7aca41d`)
and *before* messages-mode v1. B' establishes buffer-ownership
with sync activation (all activation sites are ex-command
handlers — sync is correct for those). M-async generalises the
lifecycle so messages-mode v1 gets async + Drop-based cleanup
for free.

**Design ref:** `docs/dev/architecture/mode-architecture.md` §7.1
(Drop-based Guard rationale, ctx shape, activation/deactivation
flow, re-entrancy invariants). Settled 2026-05-14; do not
re-litigate.

| Slice    | Status | What lands                                                                                                                                                                                                                                                                                                            |
|----------|--------|-------------|
| M-async.1 | ✅ | **Drop-based Mode trait redesign (sync drive).** Workspace-green. `Mode` trait gained `type Guard: Send + 'static`; `on_activate` returns `LifecycleFuture<'_, Self::Guard>`; `on_deactivate` removed (Drop = cleanup contract). `DynMode` (pub adapter trait) + blanket `impl<M: Mode> DynMode for M` does `Box<dyn Any + Send>` erasure. `ModeContext` redesigned: `Send + 'static`, no `BufferLocals` field, owned Arc handles (`config`, `events`, `services`). `GuardStore` (`HashMap<(BufferId, ModeId), Box<dyn Any + Send>>`) passed `&mut` to registry methods. Registry drives lifecycle futures synchronously via `poll_now`; deactivation drops the Guard. All Mode impls migrated atomically — marker modes (lattice-mode / -syntax / -snippet / -file-tree / -oil) get `type Guard = ();`; the five hand-written LSP modes (LspServerLogMode, LspLogMode, LspTraceLogMode, LspMode, LspFoldingMode) get typed Drop-based Guards; `lsp_sub_mode!` macro emits markers. App: `services: ServiceRegistry → Arc<ServiceRegistry>`, new `mode_guards: GuardStore` field, dispatcher caller sites updated, registry tests rewritten with `MockMode`/`MockGuard` drop-counter, `TestLocalsMode` Guard test in dispatch.rs. Workspace green: 1573 lattice-ui-tui + 191 + 17 + 9 + 12 + 4 lattice-lsp + 113 lattice-mode + remainder. End state: same user-visible behavior as before (App still blocks during lifecycle); new API shape in place for M-async.2 and M-async.3. |
| M-async.2 | ✅ | **Spawn-based lifecycle dispatch.** Workspace-green. Registry's lifecycle drive swapped: `poll_now(...)` → `lattice_runtime::spawn_task(async move { ... })`. New `spawn_task` helper added to `lattice-runtime`. Sync prefix synchronously mutates `active_modes`; spawned task awaits `on_activate_dyn(ctx)`, stashes the Guard, publishes `MajorEntered` / `MinorActivated` on success or `ModeActivationFailed` on `Err` (variant added; carries stringified reason). `ModeEvent` registered as `TypedEvent` (`mode.lifecycle`) via `register_event!` so the bus carries it. Registry method signatures: `Result<Vec<ModeEvent>, _>` → `Result<(), _>` (validation errors only). `GuardStore` wrapped in `Arc<Mutex<...>>` newtype `GuardStoreHandle` so the spawned task (tokio worker) and the App thread can both lock briefly without `&mut` lifetime gymnastics. `linkme` added as direct dep on `lattice-mode` (proc-macro path requirement). App caller sites updated: `services` already `Arc`-wrapped from M-async.1, `mode_guards: GuardStore → GuardStoreHandle`, deactivate methods now take `&Arc<EventBus>` so they can publish the deactivation event. Registry tests are `#[tokio::test]`, subscribe to `ModeEvent` on the bus, `.await` the channel for spawned-task completion. Workspace green: 113 lattice-mode + 191 lattice-lsp + 1573 lattice-ui-tui + remainder pass. **User-visible win:** the App thread no longer blocks on `on_activate.await`. Today's modes happen to be immediately-ready (they `tokio::spawn` work and return), so the win is theoretical for now -- it lands fully when `LspMode::on_activate` is rewritten to `.await` the LSP initialize handshake (a separate refinement). |
| M-async.3 | ✅ | **Cascade ordering + rollback.** Workspace-green. Cascade refactor: sync prefix walks the requested mode + its `implies()` tree, validates each, mutates `active_modes` for the whole tree, builds an ordered `Vec<CascadeStep>`. One driver walks the plan DFS, awaiting each step's `on_activate.await` before the next — eliminates races where a sub-mode reads parent's not-yet-written state. The driver is **try-sync-then-spawn**: polled once with a no-op waker; if the cascade is immediately-ready (today's marker modes + sync LSP modes), it completes on the App thread with no spawn boundary. The first `Pending` yield trips the spawn path; the remainder runs on the runtime. On lifecycle error the driver publishes `ModeActivationFailed` for the failing step plus a synthetic `ModeActivationFailed { reason: "cascade aborted by X" }` for every remaining unrun step. App-side `drain_mode_lifecycle_events` subscriber drains `ModeEvent` from the bus per tick; for each `ModeActivationFailed` it calls `deactivate_mode_by_id`. New test `cascade_abort_publishes_synthetic_failures_for_unrun_steps` documents the abort path; `implies_auto_activates_dependency` updated to assert sequential DFS order (parent first). |
| M-async.4 | ✅ | **Per-pair epoch counter for activate / deactivate serialization.** Workspace-green. Eliminates the latent race that would surface when a mode's `on_activate` future genuinely `.await`s (yields `Pending` on first poll): a synchronous deactivate arriving while the spawn is in flight can no longer leave a leaked Guard in a logically-inactive store slot. `GuardStore` grew `epochs: HashMap<(BufferId, ModeId), u64>`. New API: `bump_epoch(buf, mode) -> u64`, `current_epoch(buf, mode)`, `try_insert(buf, mode, my_epoch, guard) -> Result<(), Box<dyn Any + Send>>`. `remove(buf, mode)` bumps the epoch internally before removing, so any in-flight spawn's later `try_insert` fails the match. `purge_buffer(buf)` also bumps all `(buf, *)` epochs. `CascadeStep` carries the `epoch: u64` captured when the sync prefix queued it; the spawn driver calls `try_insert` with that epoch on `on_activate.await` success. On stale (`Err(stale_guard)`), the Box is dropped on the spawn side — the original Guard's `Drop` fires for out-of-band cleanup (publishes `LspBufferDetached`, restores `foldmethod`, etc.). No `MajorEntered` / `MinorActivated` published for the stale activation. New test `rapid_deactivate_during_pending_activate_drops_guard_on_spawn_side` exercises the race with a synthetic `.await`ing mock mode gated on a `tokio::sync::oneshot`. Doc update in mode-architecture.md §7.3.1. **Mechanism: lock-free epoch counter, not per-pair Mutex.** Considered + rejected: `tokio::sync::Mutex` per pair (deactivate would block / spawn extra task); epoch counter is minimal, sync-deactivate-friendly, and matches the Drop-based cleanup contract. |
| M-async.5 | ✅ | **`LspMode::on_activate` awaits initialize handshake.** Workspace-green. Rewrote `LspMode::on_activate` to drive the supervisor's `open_buffer(path, text).await` directly. The mode's "active" state genuinely tracks LSP readiness; the previous split (LspMode publishes intent → `attach_driver` does the work async) is gone. Modes own their work; the App is no longer a coordinator between `Event::DocumentOpened` and the LSP attach path. Flow: `LspMode::on_activate` resolves path + text via `ctx.service::<BufferStoreHandle>()`, calls `supervisor.open_buffer(path, text).await`, then publishes `LspBufferAttached` *after* the await completes (subscribers now see it as "operational"). On error → `Err(LifecycleFailed)` → `ModeActivationFailed` → App rollback. Path-less (scratch) buffers and missing-supervisor (test) paths short-circuit gracefully (no `.await`, no spawn). Removed: `lattice-lsp/src/attach_driver.rs` (52 LOC) + its boot-time `attach_driver::spawn` call. M-async.4 epoch counter is the load-bearing primitive: rapid `:lsp-mode` toggle while initialize is in-flight is reconciled lock-free (try_insert mismatch → Guard drops on spawn side → `LspBufferDetached` publishes). Bench `crates/lattice-lsp/benches/mode_activate.rs`: dispatch latency for 16-step lsp-mode cascade (with sub-modes) under two cases — no-server-config (open_buffer round-trips supervisor mailbox, returns Ok(empty)) and unregistered-supervisor (mode short-circuits). Measured 6.2 µs per activate/deactivate cycle in optimized builds (both cases); pinned in CI to catch regressions. Three App-level tests updated to `#[tokio::test(flavor = "multi_thread")]` with poll-and-sleep waits (the activate is now genuinely async so deactivate races the spawn — wait pattern matches the M-async.4 race test). Workspace green: 1573 lattice-ui-tui + 180 lattice-lsp + 76 lattice-mode + remainder pass. |

---

## messages-mode v1 (tracing-bridged echo log)

**The `*messages*` buffer** specified in
../architecture/design.md §5.10.6: every record `App::set_message`
produces, plus every `tracing::*` event from the editor + plugins,
flows through a single subscriber into one mode-owned buffer.
Anchor: design.md §5.10.6 + §5.10.5 (the B' contract it builds on).

May land in parallel with B' (no dependency); the async pieces
(tracing subscriber's append task) benefit from M-async-activate
once that lands but can ship with sync activation if scheduling
dictates.

| Slice        | Status | What lands                                                                                                                                                                                                                                                                                                                                |
|--------------|--------|-------------|
| msg-mode.1   | ✅     | **Tracing bridge + MessagesMode (combined .1 + .2 of original spec).** Workspace-green. `lattice_grammar::EchoLevel` gains `Trace` + `Debug` variants + `From<EchoLevel> for tracing::Level` + inverse impls. New `MessagesLayer` (`tracing_subscriber::Layer`) in `lattice-runtime` captures every event into a shared `Arc<Mutex<MessagesRing>>` + publishes `MessagePushed` on the editor event bus. New `install_messages_subscriber(ring, bus)` (idempotent via OnceLock) called once at App boot. New `MessagesMode` (Major, `ReadOnly = true`) in `lattice-mode/src/modes/messages.rs` replaces the `text-mode + read-only-mode` combo on `*messages*` — symmetric with `lsp-log-mode` for `*lsp*`. `App.messages` now `Arc<Mutex<MessagesRing>>` so the boot-installed layer (running on whatever thread emitted the event) can push into the same ring the App reads on the main thread for backlog seeding. **Two paths, one destination:** `App::set_message` keeps its direct ring + bus push (preserves per-App isolation for tests); the `MessagesLayer` captures all *other* `tracing::*` events in the editor (LSP layer logs, plugin host once 1.0 lands) into the same ring + bus. Both produce identical `MessageRecord` shapes. **Deferred from spec:** mode-driven subscriber lifecycle (Drop the subscriber when `:messages-mode` toggles off) — `set_global_default` is process-wide; making it mode-owned needs reload-handle plumbing (v1.1). |
| msg-mode.2   | ✅     | **`messages.filter` typed option.** Workspace-green. New `Messages` option group + `MessagesFilter: String` (default `"info"`). `lattice_runtime::install_messages_subscriber` now takes an initial filter spec + wraps the layer in `tracing_subscriber::reload::Layer<EnvFilter>`; the reload handle is stashed in a process-global `OnceLock`. `:set messages.filter=editor=info,lsp=debug,grammar=trace` triggers the option-change cascade which calls `reload_messages_filter(spec)` to swap the directive live without restarting the editor. Validator (`validate_messages_filter`) calls `EnvFilter::try_new` at `:set` time so unparseable directives are rejected before they reach the runtime. `tracing-subscriber` workspace dep gained `env-filter` feature. `MessagesFilterReloadError` enum carries the parse / no-subscriber / reload-failed cases for diagnostics (App surfaces as a warn echo if the reload fails on a test path). |
| msg-mode.3   | ✅     | **Syntax highlighting for `*messages*`.** Workspace-green. Theme gains `messages_timestamp_style` (dim), `messages_trace_style` (dim), `_debug_style` (cyan), `_info_style` (default), `_warn_style` (yellow + bold), `_error_style` (red + bold). New `messages_line_spans(line, theme, max_width)` in `render.rs` scans the fixed `HH:MM:SS.mmm LEVEL text` format produced by `format_message_record` (byte offsets 0..12 timestamp, 13..18 level token, 19.. body) and emits a ratatui `Vec<Span<'static>>` with per-token styles. In `compose_visible_lines_inner`, when the active pane's major mode is `messages-mode` (checked via `app.active_modes.get(&id).and_then(|m| m.major())`), the messages-line builder replaces the syntax-spans pipeline for every visible line. Malformed lines (empty rope tail, future formatter changes) fall through to plain rendering — no panic, no wrong color. Four new tests pin: each level token gets the matching theme style; timestamp dims; malformed line falls back; unknown level token falls back. |
| msg-mode.5   | ⛔     | Plugin host bridge (post-1.0 WIT): `host.log(level, target, msg)` flows through the same subscriber. Plugin telemetry shows up in `*messages*` with no plumbing on the plugin side. Gated by Phase 7.                                                                                                                                       |

---

## Phase 5 — GPU rendering foundation (host extraction → GPUI peer)

Anchor: [`../archive/phase-5-extraction.md`](../archive/phase-5-extraction.md) (the audit + slice plan).
Anchor: ../architecture/design.md §5.6 (renderer trait + layered architecture), §11 (project layout), §13 (Phase 5 / Phase 6 roadmap).

`lattice-ui-tui` is currently the host -- it owns `App`, dispatch, every picker source generator, LSP coordination, mode lifecycle, options cascade, AND ratatui paint. Phase 5 extracts the renderer-agnostic substrate into a new `lattice-host` crate so GPUI can be added as a peer (under `lattice-ui-gpui`), with `lattice-ui-tui` shrinking to a ratatui adapter behind the `lattice-render::Renderer` trait. The TUI moves behind `lattice --tui`; GPUI becomes the default only after parity.

| Slice          | Status | What lands |
|----------------|--------|-----------|
| 5.0            | ✅     | **Extraction audit doc** (`docs/dev/archive/phase-5-extraction.md`). Every module under `crates/lattice-ui-tui/src/` classified (HOST / TUI_RENDER / TUI_INPUT / THEME_TUI / BOOT / MIXED). Hard cases enumerated: theme is ratatui-typed throughout and leaks into `App.theme`; `chord.rs` + `input.rs` mix the renderer-neutral chord representation with crossterm adapters; `pane_render.rs` typedef hardcodes `&mut ratatui::Frame, Rect`. Target crate layout named. Slice ordering 5.1 → 5.last fixed. |
| 5.1            | ✅     | **`lattice-host` shell created.** New `crates/lattice-host` with empty `lib.rs` (docs only, no exports yet). Added to workspace members. `lattice-ui-tui` declares it as a dep (unused so far -- the seam is in place; 5.2 starts using it). Builds + 1592 ui-tui tests green; 0 lattice-host tests (intentional, the crate is intentionally empty). |
| 5.2            | ✅ done | **Move pure HOST modules.** Everything classified HOST in the audit that doesn't transitively touch `theme.rs` or `chord.rs`. lattice-ui-tui re-exports the moved types via `pub use lattice_host::*;` so downstream consumers don't break. **Waves shipped:** trivial shims; actions + excommand; host_generators; keymap leaves (keymap.rs, keymap_trie.rs, keymap_registry.rs). **Action extracted** (post-keymap-leaves): the `Action` enum + `FindKind` + `EchoMessage` + `EchoLevel` move to `lattice_host::action`. `lattice-ui-tui::app` re-exports them. **Keymap catalogs closed (5.4 unblock)**: the four `keymap_normal/insert/visual/replace.rs` files (~4,600 LoC of production code) and `input.rs` all move to lattice-host after the dispatch refactor lands in slice 5.4 — `dispatch_*` and `translate*` take `KeyChord` directly, the crossterm-coupled `KeyEvent → KeyChord` conversion lives only at the renderer boundary. See row 5.4 for details. |
| 5.3            | ✅     | **Renderer-neutral theme.** `lattice_host::ui::theme::{Theme, Style, Color, NamedColor, Modifiers}` defines the canonical neutral types; `Color` carries `Default`/`Named`/`Indexed`/`Rgb` variants. `lattice_ui_tui::theme::Theme` now grows a `From<&host::Theme>` impl + `host_style_to_ratatui` + `host_color_to_ratatui` helper conversions; `parse_color` delegates to the host parser. **App gains `host_theme: lattice_host::ui::theme::Theme`** alongside the existing TUI cache `theme: TuiTheme`; `sync_theme_from_config` writes the host theme first, then rebuilds the cached TUI adapter via `Theme::from(&self.host_theme)`. Render call sites unchanged (still read `app.theme.foo_style`). **Transitional duplication** -- both fields live on App; when GPUI lands and the TUI cache moves off App, the cached `theme` field collapses out and `host_theme` becomes the single canonical source. Tests: `host_theme_default_adapts_to_tui_theme_default` (round-trip default match), `parse_color_routes_through_host`, `ui_set_cascade_keeps_host_theme_and_tui_theme_in_sync` (cascade contract). 1535 ui-tui + 64 lattice-host = 1599 total. |
| 5.4            | ✅ done | **Split `chord.rs` + `input.rs` + keymap family.** Renderer-neutral `KeyChord` + the unified `translate(ctx, chord) -> Action` dispatcher + every per-mode catalog now live in `lattice-host`. The crossterm `KeyEvent → KeyChord` adapter stays in `lattice-ui-tui::chord` as free functions (orphan rules); `lattice-ui-tui::input` is a ~30-line crossterm shim that converts the event and forwards to `lattice_host::input::translate`. **Shipped in five slices (commits `3087e5d`, `08b45a3`, `b2e555a`, `b70baa0`, `fcb5512`):** (1) leaf translators `translate_search` / `translate_command` / `translate_command_chord_capture` / `translate_picker` take `KeyChord`; (2) `translate_normal` + `compute_normal_action` + `lookup_normal*` take `KeyChord`; the chord conversion hoists to a single point at the top of `translate()`; (3) `keymap_normal.rs` (2,771 → ~1,565 LoC of production code) moves to host; (4) `keymap_insert` / `keymap_visual` / `keymap_replace` move to host with their `dispatch_*` taking `&KeyChord`; (5) `input.rs` itself moves to host and `lattice-ui-tui::input` shrinks to the crossterm shim. Test modules stay in `lattice-ui-tui` next to the renderer-coupled `ev()` helpers that build chords via `chord::from_event(&KeyEvent { ... })`. 1424 ui-tui + 177 host + 180 lsp green. |
| 5.B            | ✅     | **App → Editor composition migration.** See [`../archive/phase-5b-app-design.md`](../archive/phase-5b-app-design.md). 5.B.0 audit (`f7416a1`), 5.B.1 host `Renderer` trait + `MinimalRenderer` (`6f651a4`), 5.B.2 `App<R>` generics (`3649c18`, reverted), 5.B.3 Option-E pivot (`3e0db86`) — `Editor` defined, `App.editor` field added. Clusters 1–12 from the plan landed across 5.B.4–5.B.19: macros (`8fc1d5b`), marks+registers (`58fb496`), position history + tag stack (`cedbaca`), search (`7a9fb4d`), vim repeat + visual (`1944035`), replace+insert (`0879473`), popup 3/4 (`4c7bd1e`), cmdline+echo (`49bbe15`), syntax (`e9203e1`), picker (`f3d0652`), config+modes (`2eb61ad`), modal+dispatch (`37b4dbd`), active-pane subset (`94dcebb`), LSP per-buffer caches (`acf5b2a`), LSP scaffolding (`d00b4d9`), LSP subsystem + server channels (`f1ba0f9`), LSP request channels (`d5b5b82`). C-wave finished the keystone fields: C1 cmdline `CompletionState` + `completion_registry` (`0d84c6f`), C2/C3 `VisibleHighlightsKey` + `PopupSnapshot` to host (`bb4911e` + `e51db26`), C4a `pane_tree` (`ed8c4ec`), C4b/c `document` + `snapshot_cache` (`db31aeb`), C4 final `folds` (`c57346c`), C5 `LspFileWatcher` (`e51b4ed` + `2e3af79` + `be5c1d0` + `3465f6f` + `9401d4e`). 5.B.20 (`f99af23`) closed out the completion-cluster tail: `insert_completion`, `snippet_registry`, `insert_completion_snippet_meta`, `completion_accept_freq`, `per_language_completion`, `completion_in_path_context`, `active_snippet`, `snippet_dirs`, `popup_back_stack`, `pending_config_structural_sections`; `SnippetCandidateMeta` moved to `lattice-host::state`. **Endpoint:** `App` holds `{editor, pane_render_registry, theme, lsp_file_watcher}` only — 4,040 LoC; `Editor` is 822 LoC. Tests: 1424 ui-tui + 177 host + 180 lsp = 1781, identical to pre-5.B baseline. |
| 5.5            | 🟡 in progress | **Dispatch extraction (`App::apply` → `Editor::dispatch`).** Largest remaining architectural unlock. Move the ~2.6k-LoC `apply` body + `apply_effect` table + `do_*` helpers from `lattice-ui-tui::app::dispatch` to `lattice_host::editor::Editor::dispatch(action) -> DispatchOutcome`. Adds a small `RendererSignal` enum (likely just `ThemeChanged` + `Quit` at v1) for the host-to-renderer side-effects today's TUI handles implicitly via per-tick repaint. Sliced 5.5.A–H. After this, `lattice-ui-gpui` can call `editor.dispatch(action)` with zero `lattice-ui-tui` dependency. Focused design doc: [`../archive/phase-5-dispatch-extraction.md`](../archive/phase-5-dispatch-extraction.md). ~1-2 weeks. **Sub-slice log:** 5.5.A scaffold `Editor::dispatch` + `DispatchOutcome` + `RendererSignal` (`ff6add5`) ✅; 5.5.B preamble — macro-recording capture + partial-chord clear → host (`fdbbf54`) ✅; 5.5.C move 10 helper-free `Action` arms into `Editor::dispatch` (`6c7ee59`) ✅; 5.5.D pure-editor helpers + read-only-help guard → host (`a743178`) ✅; 5.5.E.1 scaffold `Editor::handle_effect` + migrate the three helper-free `Effect` arms (`Effect::None`, `Effect::ClearSearchHighlight`, `Effect::Echo`) + relocate `echo_level_from_grammar` (`b319e29`) ✅; **5.5.E.2 echo-only ex-command listers (`Effect::EchoMarks` → `Editor::do_list_marks`, `Effect::EchoRegisters` → `Editor::do_list_registers`) + co-move `preview_register` (now `pub` in `lattice_host::dispatch`; ui-tui picker call sites updated) (`0198827`) ✅; 5.5.E.3 `store_yank` → `Editor::store_yank` (`Effect::Yank` migrated; oil-buffer narrow `apply_effect` re-implementation reroutes through `self.editor.store_yank`) (`8de114b`) ✅; 5.5.E.4 `set_selections_blocking` + `publish_selections_changed` → `Editor`; `visual_kind_to_mode` → host-side `pub fn` (`Effect::SelectionChange` migrated; all 13 ui-tui call sites — production + tests across `app/visual`, `app/lsp`, `app/edit`, `app/dispatch`, `render` — reroute through `editor.set_selections_blocking` / `lattice_host::dispatch::visual_kind_to_mode`) (`2e33e46`) ✅; **5.5.E.5** architecture-first: thread `DispatchOutcome` through `App::apply_effect` (rename `_outcome` → `outcome`, add `handle_renderer_signal` drain loop matching `ThemeChanged` → `rebuild_tui_theme` + `Quit` no-op) + split `sync_theme_from_config` into the existing host-theme writer + a new `App::rebuild_tui_theme` wrapper for the renderer-specific `Theme::from` tail. Signal pipe wired end-to-end; the first real emission lands in E.6 when `do_set` migrates host-side and `Effect::SetOption` pushes `ThemeChanged` on ui.* cascade (`42c4848`) ✅. **5.5.E.6** option-cascade infrastructure → host: `tui_options.rs` (typed-option declarations: `ui.dim_inactive` / `ui.separator{,_color}` / `ui.statusline_{active,inactive}_fg` / `ui.nerd_fonts`) relocated to `lattice_host::ui::theme_options` (renderer-neutral; `linkme` self-registers regardless of source crate, boot `init_from_linkme()` picks them up unchanged); new `Editor::sync_host_theme_from_config` reads the typed options + writes `editor.host_theme`; App's `sync_theme_from_config` collapses to `editor.sync_host_theme_from_config(); self.rebuild_tui_theme()`. Pure-editor helpers (`rebuild_option_cache`, `recompute_options_for_buffer`, `resolved_option<D>`) moved to `Editor`; App-side methods become 1-line delegates so the ~37 hot-path call sites for `app.resolved_option::<X>(buf)` etc. compile unchanged. Cascade engine (`do_set`, `drain_option_changes`, `apply_option_cascade`) migrated to `Editor`; cascade emits new `RendererSignal` variants (`NerdFontsToggled`, `MirrorOptionToModes(String)`, `LspConfigChanged(String)`) for renderer-coupled tails (`set_file_tree_nerd_fonts` rope refresh, mode-mirror activate/deactivate, LSP `didChangeConfiguration` fan-out) — `RendererSignal` dropped `Copy` to admit owned-String payloads; `messages.filter` reload + `relativenumber`-implies-`number` cascade + `foldmethod` recompute run host-side directly. `Effect::SetOption` arm migrated to `Editor::handle_effect` — App's `apply_effect` collapses it into the grouped no-op. `lsp_server_scope` helper + its unit test relocated alongside. Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **5.5.F.1** display-buffer signal pipe: introduces `RendererSignal::DisplayBuffer(Box<DisplayBufferRequest>)` carrying `{ content: lattice_help::HelpContent, category: BufferDisplayCategory }`; drops `PartialEq` / `Eq` derives on `RendererSignal` (HelpContent's syntax-highlight cache isn't value-equatable). Migrates `Effect::ListBuffers` and `Effect::DescribeBuffer`: host-side content builders `Editor::build_list_buffers_content` and `Editor::build_describe_buffer_content` build the `HelpContent` from `editor.*` reads (buffer-registry walk + per-kind formatting using `lattice_file_tree::modes::FileTreeRoot` / `lattice_oil::modes::OilDir` from `buffer_locals`; modes / cursor / dirty / mark / fold counts + `lattice_help::mode_link` link rendering). The Effect arms in `Editor::handle_effect` push `RendererSignal::DisplayBuffer(...)`; App's `apply_effect` collapses both into the grouped no-op; App's `handle_renderer_signal::DisplayBuffer(req)` routes through the existing `App::display_buffer` (which still owns popup / pane / split surface dispatch). The App-side `do_list_buffers` (~110 LoC) and `do_describe_buffer` (~62 LoC) bodies delete entirely (no remaining callers); the stale `mode_link` import in `app::help` drops. The App's `do_set` wrapper picks up `#[allow(dead_code)]` since prod no longer routes through it (E.6 already moved the cmdline path; the wrapper is retained for App-level cascade integration tests in `mode.rs` / `completion.rs` / `options.rs`). Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **5.5.F.2** describe-/list- batch through the F.1 DisplayBuffer pipe: migrates `Effect::DescribeCommand` / `Effect::Apropos` / `Effect::DescribeKey` / `Effect::ListKeymap` to `Editor::handle_effect`. Adds four host-side content builders (`Editor::build_describe_command_content` — fallible: pushes echo on unknown name; `build_apropos_content` — fallible: pushes echo on empty pattern; `build_describe_key_content` — infallible; `build_list_keymap_content`). Relocates `resolve_command_name_or_alias` from `lattice-ui-tui::app` to `lattice_host::excommand` alongside `aliases()` (in-module test updated to the fully-qualified path). App-side `do_describe_command` and `do_describe_key` retained as thin renderer-side wrappers (3-line each: call host builder, route through `display_buffer`) because in-App callers exist outside the Effect dispatcher (`HelpLinkTarget::Command` / `HelpLinkTarget::Chord` follow handlers + cmdline `<C-h>`); `do_apropos` and `do_list_keymap` delete entirely (Effect-only path). Stale imports cleaned (`crate::help::{command_link, key_link}` + `resolve_command_name_or_alias` from `super`). Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **5.5.F.3** option/event introspection batch through the F.1 DisplayBuffer pipe: migrates `Effect::DescribeOption` / `Effect::ListOptions` / `Effect::DescribeOptionResolution` / `Effect::DescribeEvents` / `Effect::DescribeEvent` to `Editor::handle_effect`. Adds five host-side content builders (`Editor::build_describe_option_content` — fallible: pushes E518 on unknown; `build_list_options_content` — infallible: walks `OPTION_DECLS` / `GROUP_DECLS` linkme slices; `build_describe_option_resolution_content` — fallible: walks the §6.1 layer model marking ⭐ on contributing layers via `editor.mode_registry` + `editor.active_modes` + `editor.buffer_local_overrides`; `build_describe_events_content` — infallible: walks `EVENT_DESCRIPTORS` linkme slice grouped by source crate; `build_describe_event_content` — fallible: descriptor lookup). App-side `do_describe_option` / `do_list_options` / `do_describe_option_resolution` delete entirely (Effect-only). `do_describe_events` / `do_describe_event` retained as thin 3-line `#[allow(dead_code)]` wrappers — prod path is Effect-driven; tests in `app/mode.rs` invoke them directly to assert renderer-side display routing. Stale imports cleaned (`crate::help::HelpContent` from `app/options.rs`). Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **Post-F.3 scope review.** Audited the remaining deferred items against the GPUI-conflict lens; design doc revised with three buckets — (A) correctly deferred (visible_highlights cache + runtime loop + Renderer trait — TUI-specific by design), (B) misclassified as deferred → pulled back in (apply_edit_blocking + edit-cluster Effects, activate_buffer + buffer-nav Effects, mode lifecycle, remaining describe-* family), (C) thin wrappers that are correctly App-local (cmdline `<C-h>` + help-link follow handlers). The F.1-F.3 `DisplayBuffer` signal-pipe pattern makes the bucket-B items tractable as `RendererSignal::*` emissions in ways they weren't when the original scope was written. Revised slice plan: F.4 (activate_buffer chain), F.5 (mode lifecycle), F.6 (remaining describe-*: describe-mode/list-modes/customize), F.7 (list-diagnostics), E.7 resurrected (apply_edit_blocking + edit cluster via `RendererSignal::EditsApplied(Vec<EditDelta>)`), G (collapse), H (cleanup). **5.5.F.4.1** snapshot/load helper migration (pure-editor plumbing): relocates `snapshot_active_pane` (~37 LoC), `snapshot_active_document` (~25 LoC), and `load_active_pane` (~15 LoC) from `lattice-ui-tui::app::lifecycle` to `lattice_host::dispatch::Editor`. All three bodies were 100% `editor.*` reads + writes (no helper calls, no render coupling); migration is mechanical substitution `self.editor.X` → `self.X` inside the new `impl Editor` block. App-side methods become 1-line delegates so the 23 ui-tui call sites across 8 files compile unchanged. Unblocks F.4.2 (`activate_buffer_state` host-side — depends on the snapshot helpers being reachable from inside `Editor` methods). Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **5.5.F.4.2** activate_* dispatch family migration: relocates `activate_buffer` (~25 LoC dispatch entry), `activate_document` (~120 LoC), `activate_file_tree` (~25 LoC), `activate_oil` (~15 LoC), `activate_help_in_pane` (~60 LoC) from `lattice-ui-tui::app::lifecycle` to `lattice_host::dispatch::Editor`. Also co-relocates `push_position_history` from `app::motions` (~28 LoC, pure-editor; called by `activate_buffer` + `activate_help_in_pane`) and the `POSITION_HISTORY_CAP` constant. Total ~273 LoC moved. **`activate_buffer` and `activate_document` return `bool`** — `true` signals the caller to run `activate_buffer_state()` next (the App-side tail that still calls `activate_major_for_buffer_kind` + `maybe_reparse_syntax`; both migrate when F.5 lands mode lifecycle). The bool is explicit cross-migration-window coordination, not a permanent signal-variant — it deletes when F.5+ brings the tail host-side. App's `activate_buffer` and `activate_document` become 2-line delegates (`if self.editor.activate_X(id) { self.activate_buffer_state(); }`); `activate_file_tree` / `activate_oil` / `activate_help_in_pane` collapse to 1-line delegates. The `PrevPaneState` import in `app::lifecycle` (only used by the migrated `activate_help_in_pane` body) drops. The 23 + 30 = ~53 ui-tui call sites across 8 + N files compile unchanged through the delegates. Unblocks F.4.3 (the Effect arms `BufferNext` / `BufferPrev` / `BufferDelete`). Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **5.5.F.4.3** buffer-nav Effect migration (partial — defers `BufferDelete` to F.4.4 due to `lsp_close_buffer` coupling): relocates `next_listed_buffer_id`, `prev_listed_buffer_id`, `listed_buffer_ids_sorted` (all pure-editor — only call `editor.buffers.listed_ids_sorted` + `editor.active_pane_buffer_id`) plus `do_buffer_next` / `do_buffer_prev` to `lattice_host::dispatch::Editor`. Both `do_*` methods return `bool` mirroring `activate_buffer`'s shape (true when the activation went through the full document-activation path, i.e. `activate_buffer_state` tail still needs to run). `Effect::BufferNext` / `Effect::BufferPrev` arms migrate to `Editor::handle_effect`; on `true` return, the arm pushes the new `RendererSignal::BufferActivated` variant. App's `handle_renderer_signal::BufferActivated` arm runs `self.activate_buffer_state()` — exactly the same App-side tail F.4.2's bool-return wrappers run inline. **`RendererSignal::BufferActivated` is transient.** It deletes (or transforms) in F.5 once mode lifecycle + the syntax cluster move host-side, leaving only the renderer-coupled `visible_highlights.clear()` / `pane_highlights.clear()` tail (genuinely TUI-specific, Bucket A in the post-F.3 review). App-side `do_buffer_next` / `do_buffer_prev` / `next_listed_buffer_id` / `prev_listed_buffer_id` / `listed_buffer_ids_sorted` delete entirely (Effect-only paths; no direct callers). Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **5.5.F.4.4** `do_buffer_delete` + `Effect::BufferDelete` migration: relocates `do_buffer_delete` (~60 LoC) and `lsp_close_buffer` (~6 LoC) bodies from `lattice-ui-tui::app::lifecycle` / `app::lsp` to `lattice_host::dispatch::Editor`. The `lsp_close_buffer` audit landed clean — both `editor.buffer_uris` (HashMap) and `editor.lsp` (LspSupervisorHandle fire-and-forget cmd channel) are already host-side fields, so the close path is a pure mechanical port with no signal/coordination machinery needed. `Editor::do_buffer_delete` returns `bool` matching `do_buffer_next` / `do_buffer_prev`'s shape — `true` when the successor activation went through the full document-activation path (caller runs `activate_buffer_state` tail). `Effect::BufferDelete { force }` arm migrates to `Editor::handle_effect`, reusing the existing `RendererSignal::BufferActivated` (added in F.4.3); App's `apply_effect` folds it into the grouped no-op. App-side `do_buffer_delete` wrapper deletes entirely (zero non-Effect callers); `lsp_close_buffer` wrapper kept as a thin delegate because the App-tick `drain_lsp_detach_events` path still calls it (deletes when tick-event drain moves host-side). Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **5.5.F.5** mode-lifecycle migration: 5 sub-slices retire `RendererSignal::MirrorOptionToModes` AND `RendererSignal::BufferActivated`, ending the renderer-side residue of mode dispatch. **F.5.1** pure-editor dep prep — `lsp_mode_enabled_for`, `recompute_active_completion_sources_for`, `path_for_buffer` → `Editor`; App-side becomes 1-line delegates so the existing call sites compile unchanged. **F.5.2** mode-lifecycle core — `activate_mode_by_id`, `deactivate_mode_by_id`, `maybe_auto_activate_lsp_mode`, `toggle_mode_by_name` → `Editor::dispatch`. Each returns `Vec<RendererSignal>` because the inner `drain_option_changes` can fan typed-option writes from a mode's `on_activate`/`on_deactivate` (e.g. `lsp-folding-mode` swapping `foldmethod=lsp`); App-side wrappers fan the returned signals through `handle_renderer_signal`. **F.5.3** kind/lang activation + lifecycle drain — `activate_major_for_buffer_kind` and `drain_mode_lifecycle_events` (M-async.3) → `Editor`; both return signals, both App-side wrappers fan. **F.5.4** option-cascade closure — `mirror_option_to_modes` → `Editor`; `apply_option_cascade` calls it synchronously and `signals.extend(...)`s the result instead of emitting `RendererSignal::MirrorOptionToModes`. The variant retires from the enum; the App-side `handle_renderer_signal` arm and `App::mirror_option_to_modes` wrapper delete entirely. **F.5.5** post-activation tail — `activate_buffer_state` → `Editor` (returns signals). `Effect::BufferNext` / `BufferPrev` / `BufferDelete` arms in `Editor::handle_effect` now run the full tail inline (`out.renderer_signals.extend(editor.activate_buffer_state())`); the transient `RendererSignal::BufferActivated` retires from the enum and the App-side `handle_renderer_signal` arm deletes. The renderer-coupled `visible_highlights` / `pane_highlights` cache clear is plain Editor-field writes (Bucket A — fields live host-side), so no replacement signal is needed. App-side `activate_buffer_state` wrapper collapses to a 4-line delegate that fans the returned signals — its three direct callers (`activate_document`, `activate_buffer`, `:e`-fresh-file path) compile unchanged. Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅ at every sub-slice boundary. **5.5.F.6** describe-mode / list-modes / customize migration through the F.1 `DisplayBuffer` pipe: relocates `do_list_modes` (~57 LoC), `do_describe_mode` (~70 LoC), `do_customize_picker` (~70 LoC), `do_customize_group` (~45 LoC), `do_customize_mode` (~75 LoC), and the shared `append_customize_row` (~45 LoC) from `lattice-ui-tui::app::help` to `lattice_host::dispatch::Editor` as five content builders (`build_list_modes_content` — infallible; `build_describe_mode_content` — fallible, pushes echo + returns None on unknown name; `build_customize_picker_content` — infallible; `build_customize_group_content` — fallible; `build_customize_mode_content` — fallible) + the host-side `append_customize_row` (private helper reused by group + mode views). `Effect::ListModes` / `Effect::DescribeMode { name }` / `Effect::Customize { name }` arms migrate to `Editor::handle_effect`; each emits `RendererSignal::DisplayBuffer` with the appropriate `BufferDisplayCategory` (`HelpList` for list-modes / customize-picker / customize-group / customize-mode; `HelpDescribe` for describe-mode). App's `apply_effect` folds the three Effect variants into the grouped no-op. **App-side wrappers retained**: `do_describe_mode` and `do_customize` stay as thin 3-line `#[allow(dead_code)]` wrappers because the `HelpLinkTarget::Mode` / `HelpLinkTarget::Customize` follow-handlers call them directly outside the Effect-arm dispatch path (analogous to F.2's `do_describe_command` / `do_describe_key` wrappers). `do_list_modes`, `do_customize_picker`, `do_customize_group`, `do_customize_mode`, and `append_customize_row` delete entirely (Effect-only or sub-routine, no direct App callers). `do_customize_edit` (cmdline-prefill, M.9.2) stays App-side — it mutates `command_line` + `modal` which is renderer-coupled cmdline state. Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **5.5.F.7** `:list-diagnostics` migration: relocates `do_list_diagnostics` (~46 LoC) from `lattice-ui-tui::app::lsp` to `lattice_host::dispatch::Editor`. The body was already 100% `editor.*` reads/writes (`editor.pending_tag_origin`, `editor.lsp_diagnostics.snapshot()`, `editor.picker = Some(_)`) plus three external calls (`lattice_lsp::actor::uri_to_path` — pure; `lattice_help::one_line` — pure; `lattice_picker::Picker` constructor — host-accessible). Migration is mechanical substitution. **Different signal shape from F.1–F.6**: this Effect produces a picker, not a help buffer; the picker is a renderer-neutral `Editor` field that the renderer reads each frame, so no `RendererSignal` is required — the host just writes `self.picker = Some(_)`. `Effect::ListDiagnostics` arm migrates to `Editor::handle_effect` as a single `editor.do_list_diagnostics()` call; App's `apply_effect` folds the variant into the grouped no-op. App-side wrapper retained as `#[allow(dead_code)]` 1-line delegate because three test callers (`app.rs:2640`, `lsp.rs:10457`, `lsp.rs:10481`) invoke it directly outside the Effect dispatcher. Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **5.5.E.7** *(resurrected — see post-F.3 scope review)* `apply_edit_blocking` / `handle_edits` cluster fully migrated host-side across 7 sub-slices. **E.7.1** `shift_highlights_for_edit` (Bucket A audit revealed the body is pure-`Editor` — `editor.scroll` + `editor.visible_highlights` reads/writes; no `RendererSignal::EditsApplied` needed) + private `shift_spans_within_line` → `Editor`; App-side becomes 1-line delegate then deletes in E.7.2 once `publish_document_changed` migrates. **E.7.2** `publish_document_changed` → `Editor`; App-side wrapper retained for the 5 prod call sites until E.7.3+E.7.7 close them; `App::shift_highlights_for_edit` wrapper deletes (zero callers). **E.7.3** `apply_edit_blocking`, `apply_edit_batch_blocking`, `undo_blocking`, `redo_blocking`, private `apply_edit_to_oil` → `Editor`; App-side wrappers collapse to delegates; `App::apply_edit_to_oil` deletes (`cac91be`). **E.7.4** `do_delete_line` (vim's `:d` / `:g/.../d` — full-line delete including trailing newline) → `Editor`; `Effect::DeleteCurrentLine` arm migrates to `Editor::handle_effect`; App's `apply_effect` folds into grouped no-op. **E.7.5** `do_substitute` (vim's `:s/pat/repl/[g]`) → `Editor` — pulled `fancy-regex` as a direct `lattice-host` dep (matches `lattice-core::search` usage); `Effect::Substitute` arm migrates; App-side `do_substitute` collapses to `#[allow(dead_code)]` delegate (no remaining callers post-arm-collapse); stale `Edit` import drops from `app/search.rs`. **E.7.6** planner-only migration for `do_global`: extracts `build_global_targets` (pattern validation + addressable-line scan + zero-match echo) → `Editor`. **The body-replay loop stays App-side** until the `Effect` router fully migrates host-side — a not-yet-migrated body effect would be a silent no-op via `Editor::handle_effect` alone, since `App::apply_effect` still owns the full router; once G.x retires `App::apply_effect`, the loop joins the planner host-side. **E.7.7** `handle_edits` chokepoint → `Editor`; cursor settles to `first.original_range.start` then routes through `Editor::publish_document_changed`. `Effect::Edits(edits)` arm migrates to `Editor::handle_effect`; App's `apply_effect` folds into grouped no-op. **App-side `handle_edits` + `publish_document_changed` wrappers delete entirely** (zero callers). Stale `AppliedEdit` imports drop from `app/dispatch.rs` + `app/lifecycle.rs`. Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green ✅. **5.5.G** *(in progress — sub-sliced because `App::apply` body is still ~420 LoC after F.7; the original "trivial collapse" framing assumed E covered the LSP / completion / motion clusters, but Bucket-A audits in F kept most of those App-side)*. **G.1** ✅ pure-editor fold / macro / snippet arms: eight fold arms (`OpenFoldAtCursor`, `CloseFoldAtCursor`, `ToggleFoldAtCursor`, `OpenAllFolds`, `CloseAllFolds`, `DeleteFoldAtCursor`, `GotoNextFold`, `GotoPrevFold`), two macro arms (`StartMacroRecord`, `StopMacroRecord`), and `SnippetLeave` migrate to `Editor::dispatch`. Helpers `do_set_fold_state_at_cursor`, `do_set_all_folds`, `do_goto_fold`, `do_delete_fold_at_cursor`, `do_start_macro_record`, `do_stop_macro_record` migrate to `Editor`; private selection-rule fns `innermost_fold_idx`, `fold_to_close_at`, `outermost_fold_idx` co-move. App-side helpers delete entirely (zero callers — tests exercise the arms via `app.apply(Action::*)`, not via direct method invocation). **G.2** ✅ visual + mark (4 arms: `EnterVisual`/`ExitVisual`/`ReselectLastVisual`/`SetMark`; `do_enter_visual`/`do_reselect_visual` to Editor, `do_exit_visual` kept as delegate for 4 App callers). **G.3** ✅ edit-cluster (10 arms: `Undo`/`Redo`/`JoinLines`/`ToggleCaseAtCursor`/`EnterAppend`/`OpenLineBelow`/`OpenLineAbove`/`OverwriteChar`/`ReplaceUndoLast`/`DeleteCharBackward`; 8 helpers + `previous_position` migrate; App's `redo_blocking` wrapper picks up `#[allow(dead_code)]`). **G.4** ✅ scroll / viewport / page / bracket / redraw (8 arms; 6 helpers + bracket-scan fns + `auto_open_folds_at_cursor` migrate; the latter kept App-delegate for 7 callers). **G.5** ✅ pane-navigation (6 arms; 5 helpers migrate; `do_close_pane`+`activate_pane` retained as App delegates because `do_quit` calls them. Note: G.5 originally targeted completion but reverted — `do_completion_next`/`prev` call App-side `refresh_docs_popup_for_selection` → LSP-coupled `do_completion_resolve_focused`). **G.6** ✅ mark-history walk (2 arms; `do_mark_history` + `do_walk_history` migrate; App-side `do_walk_history` retained as delegate because `do_jump_history` still calls it — jump-history blocked on `pop_popup_back` migrating). **G.7** ✅ tag-stack + mark-jump + popup-back-stack + jump-history (5 arms: `TagStackPop`/`JumpToMarkLine`/`JumpToMarkExact`/`JumpHistoryBack`/`JumpHistoryForward`; `seed_help_metadata_locals` + `pop_popup_back` migrate which unblocks the full jump-history walk + popup-back-stack pop; App-side wrappers retained as 1-line delegates for multi-call-site survivors). **G.8** ✅ snippet placeholder nav (2 arms: `SnippetNextPlaceholder`/`SnippetPrevPlaceholder`; expand stays App-side pending `active_language_id` + `is_word_char_byte` migration). **G.9** ✅ paste cluster (3 arms: `PasteAfter`/`PasteBefore`/`PasteText`; `do_paste*` + `read_register` migrate; `do_paste`+`do_paste_text` retained as delegates for `app/picker.rs` paste-picker accept + bracketed-paste runtime hook). **G.10** ✅ search-state cluster (7 arms: `SearchAppend`/`SearchBackspace`/`SearchSubmit`/`SearchCancel`/`SearchNext`/`SearchPrevious`/`SearchWordUnderCursor`; `preview_search`/`submit_search`/`cancel_search`/`repeat_search`/`do_search_word_under_cursor` migrate; free fns `compile_search_pattern`/`step_byte`/`is_word_char_byte` duplicated host-side — App copies stay for the substitute-preview path; `Action::FindRepeat` defers — calls `run_invocation`). **G.11** ✅ picker append/select + close-hover (5 arms: `PickerAppend`/`PickerBackspace`/`PickerSelectNext`/`PickerSelectPrev`/`CloseHover`; `bump_live_picker_debounce` + `preview_picker_selection` migrate; latter returns Vec<RendererSignal> for activate_buffer_state fan-out). **G.12** ✅ help-dismiss (1 arm: `HelpDismiss`; `dismiss_file_tree` migrates returning Vec<RendererSignal>; App-side delegate retained for `Effect::CloseFileTree` apply_effect arm). **Cumulative G.1–G.12: 64 arms migrated; App::apply body ~420 LoC → ~205 LoC. Tests: 1419 ui-tui + 185 host + 180 lsp = 1784 green at every boundary**. **G.7+ deferred clusters** (each blocked on App-side helpers migrating, each comparable in size to E.7 / F.5): Insert+EnterMode (LSP-trigger autopilot + `replicate_block_insert`), Completion family (`refresh_docs_popup_for_selection` + `refilter_insert_completion` + LSP resolve), Picker accept/dismiss (file open + SMR queue + preview restore), Help/Oil/FileTree follow+dismiss, Search family (`preview_search`/`submit_search`/etc.), CommandLine family (entangled with completion popup + substitute preview), LSP request arms (~12 — each routes through a `do_lsp_*` helper), Snippet expand, TagStackPop, PlayMacro+PlayLastMacro, JumpHistoryBack/Forward, JumpToMark*, Paste family (`read_register` App-side), RepeatLastChange, EnterBlockVisual*, Invoke. **G.final (true `App::apply` collapse) becomes mechanical once App-side helpers all delegate** — every remaining arm is renderer-neutral in principle but reaches into App-side machinery whose migration is the rest of the G.x series. 5.5.H (vestigial cleanup — remove the dead-code App delegates that have no remaining callers) requires G.final and stays pending. |
| 5.6            | ⛔ (revised) | **Pane-provider lookup → host.** Replaces the original 5.5 trait-object plan with the small mechanical move that fits Option-E composition. Mode-walking resolution (walk active minors then major) moves to `lattice_host::pane_render::lookup(&impl ProviderLookup, &Editor, BufferId) -> Option<&Provider>`. Each renderer keeps its concrete typed registry (no trait objects on the paint hot path). ~1-2 hours. |
| 5.7            | ⛔     | **`lattice-ui-gpui` scaffold.** New crate. Window opens. Compose `GpuiApp { editor: Editor, gpui_theme, gpui_pane_render_registry, ... }`. Render a placeholder. Validates end-to-end that the host substrate is reusable without ui-tui in the dep tree. ~1-2 weeks. |
| 5.8+           | 🟡 in progress | **GPUI feature parity + architectural async enforcement.** Sub-slices 5.8.AA through 5.8.AF have shipped iteratively: host migration of pickers, LSP request fan-out, popup unification, picker accept/dismiss closeout, every renderer-neutral `Effect` arm. **Phase 5.8.AF.5** (the active sub-phase) takes the next architectural step -- relocating every UI-thread drain off the renderer onto background tokio tasks via `RenderState` + `PerBufferCache` (paramount goal #4 enforcement). See [Phase 5.8.AF.5 — UI-thread relocation](#phase-58af5--ui-thread-relocation-paramount-goal-4) below for the slice ledger. |
| 5.9            | ✅     | **`lattice-cli --gui` / `--tui` flags + macOS app bundle + desktop identity.** `--gui` and `--tui` flags plumbed in `lattice-cli` via `clap` (mutually exclusive via `conflicts_with`); `use_gui = cli.gui \|\| (running_in_bundle && !cli.tui)` resolves final routing. **Window title** set via `TitlebarOptions { title: Some(SharedString::from("Lattice")), .. }` in `lattice-ui-gpui::window::run()`. **Linux desktop identity**: `app_id = Some("com.lattice-editor.lattice")` in `WindowOptions` for XDG taskbar / WM grouping. **macOS `.app` bundle**: `[package.metadata.bundle]` in `lattice-cli/Cargo.toml`; `assets/lattice.icns` generated from `assets/lattice.iconset/` (10 sizes: 16–512 + @2x variants) via `iconutil`; build with `cd crates/lattice-cli && cargo bundle --features gui`. **Bundle-context auto-detection** (`#[cfg(all(target_os = "macos", feature = "gui"))]`): when binary runs inside `…/Contents/MacOS/`, `std::env::current_exe()` inspects path components for `"Contents"` and auto-routes to GPUI without `--gui`; `--tui` overrides. `running_in_bundle = false` fallback on non-macOS / non-gui builds — no dead-code impact. **Linux XDG**: `assets/linux/com.lattice-editor.lattice.desktop` for system-wide `cp` install. Commit: `b8eb60d`. Default stays TUI for direct CLI invocations; `open Lattice.app` auto-selects GUI via bundle-context guard. |
| ~~old 5.5~~ (trait-objected `PaneRenderer` in host) | dropped | Superseded by the Option-E pivot. The registry is correctly renderer-specific; only the lookup is host. The revised 5.6 captures the actual remaining work. |
| ~~old 5.6~~ (`lattice-render` crate) | dropped | ratatui's pull-based draw and GPUI's retained-mode element tree are structurally different; a common `Frame` / `InputEvent` / `LayoutConstraints` surface buys nothing and constrains both renderers. Revisit only if a real shared primitive emerges (likely §5.6.7's `DocumentRenderer` — would live in `lattice-host::ui`, not a separate crate). |
| 5.9+           | ⛔     | **Real GPUI**: text rendering, atlas, panes, input, popups. Multiple sub-slices. |
| 5.last         | ⛔     | **Flip default to GPUI.** Separately scheduled, after parity + a release cycle as opt-in. |

**Decision-making heuristics applied (../architecture/CLAUDE.md):** Best long-term fit beats easy implementation -- extracting the host is months of work; the alternative (drop GPUI on top of the current entangled lattice-ui-tui) would compound the coupling that already needs unwinding. Confirm the plan before non-trivial work -- the 5.0 audit doc IS that confirmation step.

---

## Phase 5.8.AF.5 — UI-thread relocation (paramount goal #4)

Anchor: [../architecture/design.md §5.7](../architecture/design.md) (the async runtime + threading section, expanded with subsections §5.7.1 through §5.7.6 describing the architectural shape).

After 5.8.AF batch-3 closed the App-only `Effect` handler chapter, an audit of `Editor::run_tick_pending` revealed 47 separate per-tick operations running on the renderer thread -- LSP cache drains, request-firing pumps, file-watcher install + event fan-out. Most are non-blocking by virtue of `try_recv`, but two of them (`refresh_lsp_file_watcher` + `drain_lsp_fs_events`) blocked the UI for minutes on `:e <rust-file>` due to `notify::Watcher::watch(workspace_root, Recursive)` walking populated `target/` directories synchronously.

This phase relocates every UI-thread drain onto background tokio tasks via two primitives: `RenderState` (the renderer's wait-free read contract) and `PerBufferCache<T>` (copy-on-write shared HashMap). The migration shape is mechanical -- each drain follows the same template -- so once the substrate lands, the remaining sub-slices land per-feature without re-litigation.

| Slice | Status | Commit | What landed |
|-------|--------|--------|-------------|
| 1 | ✅ | `8d8c5b2` | **LSP file watcher off the UI thread.** `LspFileWatcherHandle { cmd_tx }` on `Editor`; a tokio task on the LSP runtime owns `notify::RecommendedWatcher`, the event rx, and the per-server subscription map. `Editor::refresh_lsp_file_watcher` becomes a fingerprint-gated non-blocking cmd-send; `drain_lsp_fs_events` deletes. `notify` API calls wrapped in `tokio::task::block_in_place` so a giant `target/` walk doesn't stall sibling LSP tasks. |
| 2 | ✅ | `a32b4eb` | **`.gitignore` filtering at the notify callback.** `WatcherIgnoreMatcher` combines per-root `Gitignore` + baked default globs (`**/target/**`, `**/.git/**`, `**/node_modules/**`, …). Shared via `Arc<ArcSwap<...>>` between the notify callback and the watcher task; events whose every path is ignored never enter the channel. 4 new tests on the matcher. Same `ignore` crate ripgrep/helix/zed use. |
| 3a | ✅ | `b6cd547` + `e3d4497` + `0ffd767` + `f9310e2` | **`RenderState` read contract.** New `lattice-host::render_state` module with `RenderState` + 11 sub-states (`active_document`, `buffers`, `panes`, `lsp`, `syntax`, `picker`, `completion`, `popup`, `messages`, `modeline`, `diagnostics`). `Editor::render_state: Arc<ArcSwap<RenderState>>` published at every dispatch + handle_effect tail via `build_render_state` (struct-update form `RenderState { override, ..Default::default() }`). Proof-of-life migration: TUI `severity_for_line` and GPUI `line_severity` switch from `editor.lsp_diagnostics` direct reads to `rs.diagnostics.layer.line_severity(...)`. 4 new tests. |
| 3b.0 | ✅ | `7f51492` | **`document_highlight` drain off the UI thread** (template for single-slot per-subsystem migration). `Editor.lsp_document_highlights` flips from `Option<DocumentHighlightCache>` to `Arc<ArcSwapOption<DocumentHighlightCache>>`. The spawned LSP request task clones the Arc and writes directly via `.store()` when the response arrives; no channel, no drain. `pending_document_highlight_rx` field + `drain_pending_document_highlight` method delete entirely. `LspRenderState.document_highlights` carries a clone of the cache slot Arc — renderer reads through `rs.lsp.document_highlights.load_full()` see the task's writes without re-publication. 1 new test locking the cross-clone invariant. |
| 3b.1 | ✅ | `aaf5980` | **`inlay_hint` + `folding_range` off the UI thread** (template for per-buffer `HashMap<BufferId, T>` migration). Introduces the **`PerBufferCache<T>`** primitive in `lattice_host::per_buffer_cache` with the `PerBufferCacheExt` trait (`get_for` / `insert_for` / `remove_for`). Both cache fields flip from raw `HashMap` to `PerBufferCache<T>`. Drains + `pending_*_rx` fields delete; the folding-range drain's `recompute_folds()` side-effect now fires from `maybe_request_folding_range` on cache-version flip (tracked via new `last_recomputed_lsp_fold_version` field). `LspRenderState` gains `inlay_hints` + `folds`. 4 new tests on `PerBufferCache`. |
| 3b.2 | ✅ | `de49271` | **`semantic_tokens` off the UI thread.** Same 3b.1 template: `PerBufferCache<SemanticTokensCache>`, spawned task writes via `insert_for`, drain deletes, renderer reads through `rs.lsp.semantic_tokens.get_for(...)`. Pure-helper extraction: `Editor::apply_semantic_tokens_delta_outcome` shared between the spawned task body and the unit tests so the cross-clone invariant has one source of truth. |
| 3b.3 | ✅ | `f574ede` | **`code_lens` off the UI thread + `PerBufferCacheExt::retain`.** Code-lens cache migrates by the same template. The migration surfaced a missing primitive: `retain` (drop entries matching a predicate) needed by the cache eviction path. Added with a two-pass implementation that skips the Arc realloc when nothing matches — cheap on the hot path. |
| 3b.4 | ✅ | `0762596` | **`document_link` + `document_color` off the UI thread.** Two more caches by the template. `LspRenderState` gains both. |
| 3b.5 | ✅ | `f25f9a2` | **`pull_diagnostics` off the UI thread.** Last of the per-buffer LSP caches. Shared `Editor::apply_pull_diagnostics_outcome` pure helper bridges the spawned task and tests. With 3b.5 done, every LSP feature cache routes through `PerBufferCache<T>` or `Arc<ArcSwapOption<T>>`; the renderer's hot-loop drain phase carries no LSP work. |
| 3b.signals | ⛔ planned | — | **Signal-emitting drains** (`hover`, `signature_help`, `definitions`, `references`-as-picker, etc.). Same template plus the `RendererSignal::DisplayBuffer` pipe stays in place; the *channel rx + drain* moves off-thread, the *signal emission* stays. |
| 3b.interactive | ⛔ planned | — | **Picker + completion + messages drains.** User-interactive state machines; pattern same as 3b.* but the state types are richer. |
| 3c.0 | ✅ | `f896ee6` | **Editor-actor scaffolding (dormant).** New `lattice-host::editor_actor` module defining `EditorCommand` (`Apply` / `HandleEffect` / `DispatchBlocking` / `Tick` / `Ping` / `Shutdown`) and `EditorActorHandle`. `spawn_editor_actor` dedicates a thread named `lattice-editor` running a `current_thread` tokio runtime that owns the `Editor` and processes commands serially. `signal_tx` fan-outs `RendererSignal`s emitted by `DispatchOutcome` cascades. 5 substrate tests (ping/pong, apply→publish, drop-joins-thread, explicit shutdown idempotent, tick-with-no-pending-work). |
| 3c.1 | ✅ | `ec48195` | **`ActiveDocumentRenderState` populated.** The struct gains 9 fields (`buffer_kind`, `document_buffer_id`, `active_pane_buffer_id`, `cursor`, `scroll`, `viewport_height`, `modal`, `visual_anchor`, `snapshot`). `Editor::build_render_state` populates them from the editor's current values; every dispatch / handle_effect tail publishes. This is the read-side contract for the 3c.atomic.* sub-series. |
| 3c.2 prep | ✅ | `750e51f` | **`App::ad()` accessor.** Returns `Arc<ActiveDocumentRenderState>` via `self.editor.render_state.load().active_document.clone()`. Renderer code migrates to `app.ad().X` instead of `app.editor.X` for active-document state. |
| 3c.atomic.A | ✅ | `578d557` | **`App.render_state` seam.** `App` gains its own `render_state: Arc<ArcSwap<RenderState>>` field, cloned from the editor at `App::new` time. `App::ad()` reads through `self.render_state` instead of `self.editor.render_state`. Today the two Arcs are the same cell, so observed values match byte-for-byte; the slice is purely the structural seam that lets later slices flip the writer to an `EditorActorHandle` without disturbing reader call sites. |
| 3c.atomic.B | ✅ | `3e3347f` | **render.rs production reads → `app.ad()`.** 58 reads of `app.editor.cursor` / `.scroll` / `.modal` / `.document_buffer_id` in `render.rs` (lines 1–3928, before the test module) migrate to read through the renderer-owned cell. Adds three publish points: `App::new` tail (primes render-state at boot), `App::activate_buffer` (host activation mutates active state outside dispatch), and 8 test fixtures that mutate `app.editor.*` directly. |
| 3c.atomic.B.1 | ✅ | `91eefb4` | **runtime.rs modal reads → `app.ad()`.** Extends 3c.atomic.B to the TUI main loop's four reads of `app.editor.modal` (three for per-frame cursor-style escape sequences, one for `TranslateContext`). Other fields the input loop reads (`builtins`, `pending_count`, `picker.is_some()`, etc.) stay on the editor field; not yet on `ActiveDocumentRenderState`. |
| 3c.atomic.C | ✅ | `d51ac6a` | **Typed write-side setters on `Editor`.** Adds `set_cursor` / `set_cursor_line` / `set_cursor_byte` / `set_scroll` / `set_modal` — each wraps a field write + `publish_render_state()`. Tests replace `editor.cursor = p; editor.publish_render_state();` with `editor.set_cursor(p)`; the seam artifact collapses. The setters define the OUT-OF-DISPATCH write surface; in-dispatch writes stay as raw field writes (the tail already publishes). |
| 3c.atomic.D | ✅ | `933837e` | **highlights.rs renderer-adjacent reads + `set_viewport_height` publish.** `refresh_highlights` (`&mut self`, post-dispatch) and `highlights_for_buffer_line` (pure renderer-side) read `document_buffer_id` / `scroll` / `viewport_height` through `self.ad()`. The migration surfaced a real publish gap: `App::set_viewport_height` mutated editor state outside dispatch without publishing — invisible until 2 readers migrated, fatal once they did. Fixed at source. |
| 3c.atomic.E | ✅ | `9fe7337` | **Combined doc-id + buffer-kind reads → `app.ad()`.** Three `app/lifecycle.rs` helpers (`document_folds_for`, `document_last_parsed_text_version_for`, `document_last_synced_syntax_version_for`) — each guards on `id == doc_id && active_buffer == Document`. `ad().buffer_kind` is the published mirror of `editor.active_buffer`, so the pair reads atomically via a single `self.ad()` call. Also migrates `motions.rs::help_popup_inner_height` and `mode.rs::modal_label`. Surfaced two more publish gaps: `App::open_popup` / `open_floating_popup` wrappers + `test_helpers::install_help`; all fixed at source. |
| 3c.atomic.F | ✅ | `51c173f` | **`EditorActorHandle` typed setter commands.** `EditorCommand` enum gains 6 variants (`SetCursor` / `SetCursorLine` / `SetCursorByte` / `SetScroll` / `SetModal` / `SetViewportHeight`). `EditorActorHandle` gains matching convenience methods (`handle.set_cursor(p)` etc.). The actor's `run_actor` body dispatches each to the in-process setter; the renderer thread observes the new state via its shared `Arc<ArcSwap<RenderState>>` clone. 5 new actor tests. The actor remains dormant — `App` still owns an `Editor` directly — but the API gap between the in-process and actor-mediated write surfaces is now closed. |
| 3c.atomic.* | ⏳ in-flight | — | **Remaining App-side reads.** `app/lsp.rs` (~10 production reads inside spawned tasks; need task-boundary analysis), inside-dispatch reads (correct as-is, kept on `editor.X`), and runtime-loop reads of fields not yet on `ActiveDocumentRenderState` (`builtins`, `pending_count`, `op_count`, `macro_recording`, `completion_state`, `picker`). Either widen the render-state surface or migrate through synchronous `Get<T>` actor commands; each cluster decides on its own. |
| 3c.final | 🔄 in progress | — | **Editor on its own dedicated thread.** Plan revised in `docs/dev/archive/3c-final-editor-thread.md` (2026-05-20). The earlier `EditorActorHandle` mechanical-swap shape is superseded by a dedicated-thread design: `std::thread::spawn` consumes `Action` values from an `mpsc::UnboundedSender`; the renderer holds the channel + `Arc<ArcSwap<RenderState>>` and never gets `&mut Editor`. Compile-time enforcement of paramount goal #4. Sliced A–G; the X-series predecessor work (worker, idle bridge, EditorElement) already decoupled the read path. |
| 3c.final.A | ✅ | _audit only_ | **Renderer ↔ Editor read/write audit.** Walked every renderer-thread `editor.*` access in `crates/lattice-ui-gpui` + `crates/lattice-ui-tui` (500 raw grep hits; ~120 distinct production sites after filtering test fixtures). Classified each as A (already-on-RenderState) / B (move-able) / C (mutation→Action) / D (sync-answer holdout) / N (App-side helper, moves with slice F). Enumerated the slice-B field set (10 sub-states, ~30 new fields) and the slice-C action additions (6 scalar variants). All sync-return holdouts resolved without reply-channels — every D-case lifts cleanly to RenderState. Output: `docs/dev/archive/3c-final-audit.md`. |
| 3c.final.B.1 | ✅ | — | **Panes + buffers RenderState foundation.** `PanesRenderState` gains `tree: Arc<PaneTree>`; `BuffersRenderState` gains `registry: BufferRegistry` (Arc-bump clone) + `uris: Arc<HashMap<BufferId, Uri>>`. `Editor::build_render_state` populates both. Three new render-state tests assert publication round-trips (pane-tree active index / leaves, registry clone observes post-publish inserts, URI map round-trip). Three new App accessors (`App::panes()` + `App::buffers()` in TUI, mirroring the existing `app.ad()` shape) plus a parallel pattern via `rs_guard` in the GPUI peer. All production reads of `app.editor.pane_tree.*` / `app.editor.buffers.*` / `app.editor.buffer_uris.*` in `render.rs` + `window.rs` migrated atomically. Workspace check + lattice-host (247) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green; both benches (`highlights_worker`, `editor_element_frame`) compile clean. |
| 3c.final.B.2 | ✅ | — | **Active-document decoration fields onto `ActiveDocumentRenderState`.** Seven new fields land: `folds: Arc<[Fold]>`, `all_matches: Arc<[Range]>`, `current_match: Option<Range>`, `visual_range: Option<Range>`, `substitute_preview: Option<Arc<SubstitutePreview>>`, `selections: Arc<SelectionSet>`, `option_cache: OptionCache`. Build path populates them; two new tests (`active_document_folds_reflects_editor_state`, `active_document_search_and_options_reflect_editor_state`) lock the round-trip contract. Renderer migration: GPUI `paint_pane` reads folds/visual/match/substitute from `rs_guard.active_document` and inlines the `line_inside_closed_fold` + `fold_start_at` predicates (the host helpers still exist for dispatch-side use). TUI `FrameView::from_app` + `for_buffer` swap the prior `Arc::from(Vec::clone)` to a one-Arc-clone off `rs.active_document.folds`; every `app.editor.{all_matches,current_match,option_cache,substitute_preview,visual_selection_range(),document.selections()}` read in production code now goes through `app.ad().X`. Two host-side mutators (`set_selections_blocking`, `toggle_mode_by_name`) gain tail `publish_render_state()` calls — they were called from out-of-dispatch sites (Visual extension, gv-reselect, mode cascades, tests) without republishing. Four direct-mutation tests gained manual publishes after `editor.folds.push` / `editor.folds[i].closed = true` / `editor.recompute_folds()`. Workspace check + lattice-host (249) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |
| 3c.final.B.3 | ✅ | — | **Picker + completion + popup substates populated.** `PickerRenderState` gains `state: Option<Arc<Picker>>`; `CompletionRenderState` gains `insert: Option<Arc<InsertCompletionState>>` + `state: Option<Arc<CompletionState>>`; `PopupRenderState` gains `buffer_id`, `help: Option<Arc<HelpBuffer>>`, `help_highlights: Arc<[Vec<StyledSpan>]>`, `placement: PopupPlacement`, plus an `is_open()` convenience. `Editor::build_render_state` populates each via `Option::map(Arc::new)` (zero-cost when closed). Two new tests (`popup_substate_defaults_closed`, `picker_and_completion_substates_default_closed`). Three new App accessors `App::picker_state()`, `App::completion()`, `App::popup()` mirror the existing `ad()` / `panes()` / `buffers()` shape. Migrated TUI production reads of `app.editor.{picker,completion_state,popup_buffer,insert_completion,popup_placement}` to the published substates — bound each substate Arc to a local so `as_deref()` borrows survive the function body (six sites). GPUI peer migrates `paint_pane`'s popup gate, the picker_strip_rows computation, both picker overlays, and the popup overlay to read via `render_state.{picker,popup}` substates directly. **Scope cut for follow-up:** the TUI's `App::popup_help()` wrapper still delegates to `self.editor.popup_help()` rather than reading `rs.popup.help`; migrating it requires adding `publish_render_state()` to every popup-mutation path (open / scroll / dismiss / follow-link / customize-pickers / …) which would balloon the slice. The published substate IS consumed directly by the GPUI peer; the TUI wrapper migration is queued as `3c.final.B.3b`. Workspace check + lattice-host (251) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |
| 3c.final.B.4 | ✅ | — | **LSP progress + supervisor handle into `LspRenderState`.** Two new fields: `progress: Arc<HashMap<(Arc<str>, String), LspProgressUpdate>>` (small per-publish HashMap clone, ~10 concurrent items typical) and `supervisor: LspSupervisorHandle` (internally `Arc<ArcSwap<SupervisorSnapshot>>`-backed so `Clone` is one Arc bump). `Editor::build_render_state` populates both. New test `lsp_progress_reflects_published_map` locks the round-trip. Migrated the two TUI production reads in `render.rs`'s LSP modeline indicator: `app.editor.lsp.servers_for(uri)` → `rs.lsp.supervisor.servers_for(uri)` and `&app.editor.lsp_progress` iter → `rs.lsp.progress.iter()`. Workspace check + lattice-host (252) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |
| 3c.final.B.5 | ✅ | — | **`TranslatorRenderState` — closes the audit's slice-D `TranslateContext` holdout.** New top-level sub-state carrying `builtins: lattice_grammar::builtins::Builtins` (`Copy`), `keymap: KeymapHandle` (Arc-backed; `Clone` is one Arc bump), `partial_chord: Arc<[KeyChord]>` (small slice, 0–2 entries typical). `RenderState` gains a `translator: Arc<TranslatorRenderState>` field. `Editor::build_render_state` populates it via the cheap-clone pattern. New test `translator_substate_reflects_editor_inputs` exercises a non-empty `partial_chord` round-trip. `runtime.rs`'s `TranslateContext` construction now binds `let translator = app.editor.render_state.load().translator.clone();` once per keystroke and threads `&translator.{builtins,keymap,partial_chord}` into the context — the three `&'a` borrows that previously tied the translator to `Editor`'s lifetime are gone. This is the structural prerequisite for slice E (Editor on its own thread): the renderer's input loop no longer needs `&app.editor.X` to translate a keystroke into an `Action`. Workspace check + lattice-host (253) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |
| 3c.final.B.6 | ✅ | — | **Lifecycle + theme finish slice B.** New `LifecycleRenderState` carries `should_quit`, `pending_redraw`, `terminal_width: Option<u16>` (the per-tick "renderer should notice this" signals). Top-level `RenderState` gains a `theme: Theme` field (Theme is `Copy` — every field is `Copy` — so no `Arc` indirection). `Editor::build_render_state` populates both. New test `lifecycle_and_theme_reflect_editor_state` locks the round-trip. Renderer reader migrations: TUI `main_loop`'s `while !app.editor.should_quit` becomes `while !app.editor.render_state.load().lifecycle.should_quit`; the `pending_redraw` check at the top of the loop reads via the same substate (the acknowledge-write stays on `editor.X` but gains a manual `publish_render_state()` so the next iteration's load observes the cleared flag — without that the loop would re-clear every frame). GPUI `on_key_down` reads `should_quit` via RS too. GPUI `paint_pane` rebinds `host_theme` from `rs_guard.theme` instead of `editor.host_theme`; the `cursorline_bg` read reuses the local binding. Write-track holdouts (`terminal_width = Some(size.width)`, `pending_redraw = false`) remain on the editor field — slice C lifts them to `Action::SetTerminalWidth` + `Action::AcknowledgeRedraw`. **Slice B is structurally complete:** every group's enumerated field set from the audit (§5) lives on RenderState; reader-site migration is complete in production code paths apart from the deferred B.3b TUI popup-help wrapper. Workspace check + lattice-host (254) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |
| 3c.final.C | ✅ | — | **Renderer non-dispatch mutations → `Action::*`.** Six new variants on `enum Action`: `SetViewportHeight(u32)`, `EnsureCursorVisible`, `RefreshPaneHighlights`, `DismissPopup`, `SetTerminalWidth(u16)`, `AcknowledgeRedraw`. Each has a one-shot match arm in `Editor::dispatch`'s top-level match that forwards to the existing host helper (`editor.ensure_cursor_visible`, `editor.refresh_pane_highlights`, `editor.dismiss_popup`) or sets the field directly (`viewport_height`, `terminal_width`, `pending_redraw`). The matching App-side wrappers (`App::set_viewport_height`, `App::refresh_pane_highlights`, `GpuiApp::set_viewport_height`, `GpuiApp::ensure_cursor_in_viewport`, `GpuiApp::dismiss_popup`) keep their signatures but now route through `apply(Action::X)` / `dispatch_action(Action::X)` instead of mutating editor state directly. The two raw field writes in `runtime.rs::main_loop` (`editor.terminal_width = ...`, `editor.pending_redraw = false`) become `app.apply(Action::SetTerminalWidth(...))` and `app.apply(Action::AcknowledgeRedraw)`. GPUI `window.rs::render`'s `self.app.editor.refresh_pane_highlights()` becomes `self.app.dispatch_action(Action::RefreshPaneHighlights)`. Dispatch tail publishes RS automatically — no manual `publish_render_state()` calls remain in renderer code for these paths. Workspace check + lattice-host (254) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |
| 3c.final.D | ✅ | _verification only_ | **Sync-answer holdouts: nothing left to migrate.** The audit's §4 enumeration listed three D-bucket cases: per-buffer document handle lookup, per-buffer Oil view extraction, and the `TranslateContext` borrow batch. All three dissolved into RenderState lifts in earlier B groups: the document-handle + Oil-view lookups go through `rs.buffers.registry.X()` (slice B.1), and the translator inputs (`builtins`, `keymap`, `partial_chord`) live on `rs.translator` (slice B.5). No reply-channel infrastructure is needed for slice E. |
| 3c.final.E.3 | ✅ | — | **Mid-size clusters migrated through the `mutate_editor` seam.** Adds the same routing pattern across `app/popup.rs` (6 mut delegates: `open_popup`, `open_floating_popup`, `dismiss_stale_popup_registry`, `seed_help_metadata_locals`, `swap_popup_content`, `dismiss_popup`), `app/options.rs` (5 muts: `rebuild_option_cache`, `recompute_options_for_buffer`, `recompute_active_completion_sources_for`, `do_set`, `drain_option_changes`, `apply_per_language_toml_overrides`), `app/cmdline.rs` (2 muts: `open_completion_popup`, `refresh_completion_popup`), `app/completion.rs` (12 muts: snippet expand / next / prev / reload, completion next / prev / trigger / accept / toggle docs / cancel / filter clear / filter to source), `app/help.rs` (10 muts: `do_open_help_topic`, `do_describe_command`, `do_describe_key`, `do_open_hover`, `do_describe_events`, `do_describe_event`, `do_describe_mode`, `do_customize`, `do_tutor`, `do_customize_edit`), `app/dispatch.rs` (8 muts: `run_tick_pending`, `run_oil_invocation`, `run_file_tree_invocation`, `run_help_invocation`, `run_read_only_motion`, `run_invocation` + out-pattern, `run_document_invocation` + out-pattern, `apply_effect`'s `handle_effect`), `app/picker.rs` (11 muts: `apply_picker_outcome`, `snapshot_lsp_instances`, `open_lsp_locations_picker`, `open_lsp_picker`, `open_picker`, `drain_pending_picker_init`, `drain_pending_live_picker_query`, `seat_picker_from_pairs`, `open_buffer_picker`, `do_picker_dismiss`, `do_picker_accept`), `app/lifecycle.rs` (10 muts: `activate_file_tree`, `activate_oil`, `activate_help_in_pane`, `do_write`, `load_active_pane`, `snapshot_active_document`, `snapshot_active_pane`, `activate_buffer_state`, `jump_to_file_line_col`, `open_help_in_pane`, `seed_empty_document_locals`, `save_blocking`). Pattern for `&str` / `&Path` args: clone to owned via `.to_string()` / `.to_path_buf()` for the `Send + 'static` closure, dereference inside. Pattern for `&mut out: DispatchOutcome` results: build `out` inside the closure, return it from `mutate_editor_with`. lattice-host (257) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green at every checkpoint. |
| 3c.final.E.cleanup | ✅ | — | **Warning sweep + dead-code purge.** After the E.swap landing, the dead_code lint surfaced ~50 warnings across both renderer crates + lattice-mode. Every warned item was an App-side delegate method (1-line `mutate_editor` calls) or a free helper function whose only callers are in `#[cfg(test)] mod tests` blocks — i.e., dead in the lib build by design (production routes around them through the seam) but live for test fixtures. The cleanup: (a) deleted `GpuiApp::finalize_boot` outright (its body was inlined into `GpuiApp::new` to run on the owned Editor before the actor spawns; `apply_per_language_toml_overrides` added to that flow to match the prior end-state); (b) added `#![allow(dead_code)]` at the top of `lattice-ui-tui/src/app.rs` with an explanatory comment, since 80% of the warnings cluster there — the alternative would have been 30+ individual `#[cfg(test)]` gates with no architectural benefit beyond what the swap already enforces; (c) per-item `#[allow(dead_code)]` on `lattice-mode/src/locals.rs::LocalDyn::{as_any_mut, into_any}` + `BufferLocals::{get_mut, remove}` — designed-in API surface called out in the audit but not yet wired to an owner mode; (d) removed an unused `mut` qualifier in an LSP test. **Net warning count: 50 → 0 across `lattice-host`, `lattice-ui-tui`, `lattice-ui-gpui`, `lattice-mode`.** `cargo check --benches` also clean. Plan for the B-extension perf slices (B.7 / B.8 / B.9 / B.10 / B.11) persisted to `docs/dev/operations/3c-final-b-extension.md` so the post-swap follow-up work isn't lost. lattice-host (257) + lattice-ui-tui (1392) + lattice-ui-gpui (23) tests green. |
| 3c.final.E.swap | ✅ | — | **STRUCT SWAP LANDED — `App.editor: Editor → App.editor_actor: EditorActorHandle` (cfg-gated).** The architectural pivot for paramount goal #4 is in: in production builds (`cfg(not(test))`) both renderer peers hold `EditorActorHandle` and reach Editor only through the actor handle's blocking RPCs (`mutate_blocking`, `with_editor`, `mutate_blocking_with`, `apply_blocking`); the `&mut Editor` type literally cannot escape the actor thread, enforced by the type system. **Cfg-gating strategy:** `#[cfg(not(test))] pub editor_actor: EditorActorHandle` + `#[cfg(test)] pub editor: Editor`. Test builds preserve direct `Editor` ownership so fixtures that mutate state without going through the dispatch path keep compiling — this is the audit §7 "test-internals escape hatch" formalised. **Seam helpers (`mutate_editor`, `mutate_editor_with`, `read_editor`) are cfg-gated:** prod bodies route through the actor handle's RPCs; test bodies retain direct closure invocation against the in-process Editor. Visibility flipped from `pub(crate)` to `pub` so benches (which compile as separate crates) can use the same seam. **Constructor refactor:** `App::new` / `GpuiApp::new` now apply boot-time setup directly on the owned Editor BEFORE spawning the actor — the App-side wrappers that previously routed through `mutate_editor` would funnel through the actor mailbox which doesn't exist yet at that point in construction. Renderer-side post-actor wiring (`rebuild_tui_theme` / `rebuild_gpui_theme`) runs against the App after the actor spawns; it reads the already-published RS, no editor borrow needed. **Final missed sites cleaned up:** `app/options.rs` (`set_foldmethod_for_test`, `take_pending_structural_section`, `pending_structural_section_paths`), `app/popup.rs` (`popup_help_links`, `popup_help_anchors`, `popup_help_highlights`, `active_popup_placement` — all test-only, gated `#[cfg(test)]`), `pane_render.rs` (`pane_render_provider` two-step refactor — read active mode chain through `read_editor`, then walk against the renderer-thread registry outside the closure since `ProviderLookup` isn't `Send + 'static`), `render.rs::modeline_is_synthetic` (route via `buffers().registry`), `render.rs::lsp_diagnostics_on_line` (route via `read_editor`), `gpui/window.rs::picker_display_is_minibuffer` + `paint_pane` + `picker_substate`, `gpui/lib.rs::Effect::Global` (build-outcome-in-closure pattern). Bench `lattice-ui-tui::benches::render` routed through the seam. lattice-host (257) + lattice-ui-tui (1392) + lattice-ui-gpui (23) tests green. **What this means architecturally:** paramount goal #4 ("Asynchronicity: nothing blocks the UI — enforced architecturally, not by discipline") is now compile-time enforced for production. The renderer thread cannot construct a `&mut Editor` reference; every mutation goes through the actor mailbox and runs on the dedicated editor thread. Reads either go through wait-free RS sub-state accessors (`ad()`, `panes()`, `buffers()`, `popup()`, etc.) or through `read_editor`'s blocking RPC. **Outstanding follow-up (perf):** the per-frame `read_editor` closures in render.rs / runtime.rs / window.rs each cost a sync mailbox round-trip (~µs). 12-15 reads per frame × ~µs = well under the 8ms 120Hz budget, but the audit-planned hot-path RS lifts (`MessagesRenderState.last`, `ModelineRenderState.{auto_submit_hint,search_pattern,search_direction}`, new fields for `pane_highlights` / `buffer_locals` / `active_modes` / `config`) should follow as a B-extension slice to drop those round-trips to wait-free `Arc` clones. |
| 3c.final.E.5j | ✅ | — | **Renderer crates drained to ZERO — full TUI + GPUI peer migration.** Final sweep across `crates/lattice-ui-tui/src/{render,runtime,app/boot,app/help,app/lifecycle,app/lsp,app/options}.rs` and `crates/lattice-ui-gpui/src/{lib,window}.rs`. Sites cleared: render.rs (16), runtime.rs (2 — picker/completion popup-row counts + per-frame snapshot), app/boot.rs (2 — boot-time doc-id read + initial publish), app/help.rs (2 — popup_buffer + buffer_locals link lookup), app/lifecycle.rs (1 — document_path via published registry), app/lsp.rs (2 — show-message-request map + buffer_uris+supervisor for code-action handle), app/options.rs (2 — `_for_test` setters cfg(test)-gated), gpui/lib.rs (1 — boot publish), gpui/window.rs (9 — render_state loads via App's own Arc, viewport_height via ad(), command_line / search_line via read_editor closures, insert_completion via completion().insert, popup substate via render_state). Recipe catalog from E.5g–E.5i applied uniformly: RS-accessor swaps where the data is already published (`picker_state()`, `completion()`, `popup()`, `panes()`, `buffers().registry`, `render_state.load()`); `read_editor` closures returning owned values for fields not yet on RS (`paint_request`, `buffer_locals.*`, `pane_highlights`, `auto_submit_after_chord`, `search_line`, `last_message`, `active_modes`, `lsp_pending_show_message_requests`, `command_line`); `mutate_editor` with empty closure for boot-time publishes; `#[cfg(test)]` gate for the two `_for_test` config setters. **`help_render_data` signature** changed from `&[T]` to `Vec<T>` (owned) so the closure can return without leaking borrows; callers already used `.cloned()` so no caller-side changes needed. **Net:** TUI + GPUI peer production `(app|self).editor.X` count: **0** across all renderer-thread paths. The struct-swap target (`App.editor: Editor → App.editor_actor: EditorActorHandle`) is now reachable in a single mechanical slice — every production access routes through one of: (a) the four App-helper closures (`mutate_editor` / `mutate_editor_with` / `read_editor` / boot-time `mutate_editor(\|_\|{})`), (b) RS-accessor methods (which load through `App::render_state`, an Arc independent from `editor.render_state`), or (c) `#[cfg(test)] impl App` test-fixture surface. The four closure helpers' bodies are the one place that flips on swap; the cfg(test) gates compile against the audit-planned `App::editor()` / `App::editor_mut()` accessors. lattice-host (257) + lattice-ui-tui (1392) + lattice-ui-gpui (23) tests green. **Caveat for the swap:** the `read_editor` closures in render.rs / runtime.rs / window.rs run per-frame; pre-swap they cost a virtual call (essentially free), but post-swap they become sync mailbox round-trips at the `actor.with_editor(f)` boundary (~µs each). 12-15 reads per frame × ~µs each = well under the 8ms frame budget at 120Hz, but the per-frame paint hot-paths should still get RS lifts (planned audit §5.7 `MessagesRenderState.last`, §5.8 `ModelineRenderState.{auto_submit_hint,search_pattern,search_direction}`, new fields for `pane_highlights` / `buffer_locals` / `active_modes` / `config`) in a follow-up B-extension slice — separating "swap-correct" from "swap-fast" intentionally to keep each commit small. |
| 3c.final.E.5i | ✅ | — | **TUI `app/` tree drained to zero — final 11 production sites migrated by recipe.** Audit of the remaining production `self.editor.X` sites in `crates/lattice-ui-tui/src/app/`: 11 sites across 6 files (motions.rs 4, edit.rs 2, cmdline.rs 2, options.rs 1, popup.rs 1, app.rs 1). Five distinct recipes applied: **A — eager-materialize for `impl Trait` args:** `App::set_message`'s `text: impl Into<String>` couldn't ride through `mutate_editor` (closure bound rejects the impl arg) — fixed by materializing `text.into(): String` outside the closure, then capturing the owned `String` by move. Improvement over the E.5c reverted attempt that left it on direct field access. **B — typed `read_editor` for generic readers:** `App::resolved_option<D: OptionDecl>` routes through `read_editor(move |e| e.resolved_option::<D>(buffer))` after adding `D: 'static` bound; the closure satisfies `Send + 'static` because `BufferId` is `Copy` and `D` is a marker type-parameter. **C — closure-returning bool for short-circuit logic:** `App::do_command_line_complete_or_advance`'s popup-advance step returns `true`/`false` from a `mutate_editor_with` closure to drive the outer if/else without leaking the `&mut state` borrow. **D — build-outcome-in-closure for `&mut DispatchOutcome` callees:** `App::do_global` (pattern + body cloned to owned) and `App::do_insert_text` (s.to_string()) build a local `DispatchOutcome`, pass `&mut out` to the host method inside the closure, return the owned outcome from `mutate_editor_with`. Same pattern slice E.3 documented for this exact shape. **E — RS-accessor migration + publish-discipline ratchet:** `motions.rs`'s 4 sites (`active_pane_content_height` + `help_popup_inner_height`) and `cmdline.rs`'s `chord_capture_active` finally migrated from `self.editor.{pane_tree,popup_placement,modal}` to `self.panes().tree.X` / `self.popup().placement` / `self.ad().modal`. Slice E.2 had reverted this when 2 tests failed; this slice commits to fixing the tests instead. 4 test failures surfaced and fixed by adding explicit `editor.publish_render_state()` after direct field mutations: `active_pane_content_height_subtracts_status_row_in_horizontal_split` (lifecycle.rs, after `pane_tree.split_active`), `help_popup_inner_height_caps_at_twenty_for_cursor_anchored` (help.rs, after `popup_placement` field assignment), `chord_capture_active_only_when_in_chord_arg_slot` (cmdline.rs, after `modal` field assignment), `chord_capture_active_for_canonical_command_name` (cmdline.rs, same). **F — cfg(test) gate for closure-borrow holdout:** `App::with_popup_help_mut<R>` takes `impl FnOnce(&mut HelpBuffer) -> R` without a `Send + 'static` bound; propagating that bound onto every test caller's local-borrow closure would be invasive, and the method's only callers are app.rs's `mod tests`. Moved to `#[cfg(test)] impl App` block in popup.rs. **Net:** TUI `app/` + `app.rs` production `self.editor.X` count: 11 → **0** (verified via `awk` exclusion of `#[cfg(test)]` blocks + doc-comment lines). Cumulative arc from E.5e (60 sites in app/) → E.5i (0 sites). What remains for the full struct-swap target: ~4 sites in `render.rs` production code (`buffer_locals.get`, `pane_highlights.get`, `auto_submit_after_chord`, `last_message` — all candidates for App helpers or RS lifts in a follow-up slice). lattice-host (257) + lattice-ui-tui (1392) + lattice-ui-gpui (23) tests green. **Pattern catalog now closed:** every production-site shape encountered in slices E.5e–E.5i has a documented migration recipe (mutate/read closure, materialize-then-move, build-in-closure, RS-accessor + test publish, cfg(test) gate). The remaining work is mechanical application of these recipes to render.rs + any new sites that surface in audit. |
| 3c.final.E.5h | ✅ | — | **`app/picker.rs` + `app/lifecycle.rs` drained: cfg-gated-dead-code purge + test-fixture gate.** Same audit shape as E.5g: classify each `self.editor.X` site by real production reachability rather than mechanical grep count. **picker.rs (11 raw hits → 0 production):** 8 of the 11 grep hits lived inside `_retired_do_picker_accept_body`, a 248-line function held under `#[cfg(any())]` (always-false → never compiled) as a "historical reference" after G.11 moved the live `do_picker_accept` routing host-side. Git history is the canonical reference — carrying cfg-disabled code in-file just inflates the migration counter without ever running. Deleted the function outright (~250 lines incl. its now-unused `EchoLevel` `#[cfg(any())]` import + the `BufferId` import that was only reached from the retired body). Of the 3 surviving production-counted sites: `App::preview_picker_selection` (L252) had zero callers anywhere — host invokes `Editor::preview_picker_selection` directly from `Editor::dispatch` and routes its `Vec<RendererSignal>` through the outcome surface; deleted as dead. `App::build_picker_context` (L48) had only test-fixture callers (this file's `mod tests` + the entire `picker_sources.rs` module, which is itself documented as TUI test harness re-exporting from `lattice_picker::picker_sources`) and host-internal call sites that route through `Editor::build_picker_context`; moved to `#[cfg(test)] impl App`. `App::bump_live_picker_debounce` (L186) was already `#[allow(dead_code)]` with two test callsites in this file's `mod tests` — moved to the same gated block. **lifecycle.rs (3 production → 0):** `document_folds_for`, `document_last_parsed_text_version_for`, and `document_last_synced_syntax_version_for` were all already `#[allow(dead_code)]` with only a single test caller each (the `document_locals_*` test). Production path uses the host-side copies directly from `pane_highlights.rs`. Moved all three into a `#[cfg(test)] impl App` block. **Net:** picker.rs production sites 11 → **0**; lifecycle.rs production sites 3 → **0**; picker.rs file size dropped by ~250 lines (the cfg(any()) body); cumulative TUI `app/` production sites: 60 (post-E.5f) → ~47 estimated (need a re-count). lattice-host (257) + lattice-ui-tui (1392) + lattice-ui-gpui (23) tests green. Pattern reaffirmed: cfg(any()) "preserved-for-reference" dead code costs more than it's worth — every audit pass treats those hits as real, and the historical reference is one `git log` away. Delete on sight. |
| 3c.final.E.5g | ✅ | — | **`app/completion.rs` drained: dead-delegate sweep + `#[cfg(test)]` gate.** Audit of completion.rs's 8 remaining `self.editor.X` sites surfaced that *none* serve production callers — 4 delegates are entirely dead (zero callers anywhere: `populate_insert_completion_sync`, `maybe_refresh_insert_completion_after_edit`, `expand_snippet_with_lsp_edits`, `expand_snippet`; host invokes its own copies directly inside `Editor::dispatch`) and the other 4 are pure test-fixture surface (`refilter_insert_completion`, `do_completion_accept_then_insert`, `snippet_meta_for`, `effective_completion_for`; their only callers are inside `#[cfg(test)] mod tests` blocks in `app.rs`, `app/dispatch.rs`, `app/options.rs`, and completion.rs itself). Per the audit (`3c-final-audit.md` §7) and CLAUDE.md heuristic #1 (best long-term fit beats easy implementation), shaping host signatures around closure `Send + 'static` bounds — clone-the-ref-arg, owned-DispatchOutcome return, pure-function refilter — would propagate the renderer-thread contract uniformly through methods that no production renderer-thread caller ever uses. Instead: deleted the 4 dead delegates outright (~50 lines incl. stale doc comments claiming "retained for App-side callers" that were migrated host-side in prior slices) and moved the 4 test-only delegates into a dedicated `#[cfg(test)] impl App` block. Production `cfg(not(test))` build no longer compiles those methods at all; `self.editor.X` site count for completion.rs production code: 8 → **0**. The 4 gated delegates retain their `self.editor.X` bodies behind the cfg-gate and carry a comment that they'll flip to the audit's planned `App::editor()` / `App::editor_mut()` cfg-gated accessors when the actor-swap lands. Imports for `Position`, `SnippetCandidateMeta`, `EffectiveCompletionConfig` gated `#[cfg(test)]`; `Buffer` import dropped (only doc-comment references remained). Module docstring updated to reflect the migrated host-side authority over `populate_*` / `refilter_*` / accept-paths / `expand_snippet`. lattice-host (257) + lattice-ui-tui (1392) + lattice-ui-gpui (23) tests green. Pattern: when a renderer-thread `self.editor.X` site turns out to be reachable only from tests, the slice-F escape-hatch (`#[cfg(test)] impl App`) is the design-philosophy-correct landing — not a host-signature refactor that adds allocation cost to production paths to satisfy a constraint that doesn't apply to them. |
| 3c.final.E.5e | ✅ | — | **Methodify-on-host: `sync_keymap_overlays` hoisted.** The keymap-layer-stack reconciliation body (~40 lines, 14 direct `self.editor.X` sites in `app/boot.rs`) moves to a single new `Editor::sync_keymap_overlays` in `lattice_host::dispatch`, with the prior `App::sync_completion_popup_mode_activation` (8 sites, sole caller) inlined into it. Renderer-side `App::sync_keymap_overlays` collapses to a 1-line `mutate_editor` delegate. **Net:** `app/boot.rs` drops to **0 production `self.editor.X` sites** (was 29 at the start of slice E; was 14 before this swap). Cumulative TUI app/ + app.rs: 83 → 60 (23-site drop in one methodify). The hoist also clarifies ownership — the popup-mode activation logic was never renderer-coupled; it was pure editor state machinery that happened to live in the App. lattice-host (257) + lattice-ui-tui (1392) + lattice-ui-gpui (23) tests green. Demonstrates the pattern for the remaining methodify-on-host candidates: the ExCommandRegistry-bound `cmdline.rs` helpers (`do_command_line_describe_under_cursor`, `try_resolve_missing_arg_prompt`), the `picker.rs` accept-routing arms, the `lifecycle.rs` version readers. Each is a 1-method-per-slice hoist where the body's `self.editor.X` count converts to zero in exchange for one `&mut Editor` method on host. |
| 3c.final.E.5d | ✅ | — | **Rule-of-three App helpers + RS-accessor sweep.** Methodified the host's stale `App::push_recent_file` 4-step body to a single `mutate_editor` call routing into the existing `Editor::push_recent_file` (-4 sites in one delete). Added 3 rule-of-three App-side helpers earning their keep by call-site frequency: `App::cursor() / set_cursor()` (8+ readers across `dispatch`/`help`/`search`/`lsp`/Debug); `App::document_buffer_id()` (5+ readers across `lsp`/`boot`/`lifecycle`); `App::command_line()` (5 readers across `cmdline`/`search`/Debug). For fields already mirrored on RenderState by prior B-slices, reused existing accessors instead of adding helpers: `pane_tree.X` → `panes().tree.X` (4 sites in `help`/`lifecycle`), `buffers.X` → `buffers().registry.X` (4 sites in `lifecycle`/`dispatch`), `option_cache.X` → `ad().option_cache.X` (7 sites in `options`), `active_buffer` → `ad().buffer_kind` (4 sites in `dispatch`), `document.snapshot()` → `ad().snapshot.clone()` (5 sites in `picker`/`search`/`help`/Debug + 7 in `picker_sources`), `popup_buffer` → `popup().buffer_id` (5 sites in `help`/`popup`), `modal` → `ad().modal` (3 sites in `cmdline` + Debug), `should_quit` → `render_state.load().lifecycle.should_quit` (2 sites in `dispatch` + GPUI peer), `host_theme` → `render_state.load().theme` (2 sites in `boot`/`dispatch`). `App::buffer_uri` signature change: `Option<&Uri>` → `Option<Uri>` (Uri is internally `Arc<str>`-backed, clone is cheap, returning a borrow through the temp `Arc<BuffersRenderState>` is E0515 anyway). Two test updates required: `chord_capture_active`'s modal read reverted to `self.editor.modal` (test mutates `editor.modal` directly without publishing RS — same precedent as `motions.rs` pane_tree); the `lsp_close_buffer_removes_uri_mapping_for_unattached_buffer` test gains explicit `app.editor.publish_render_state()` after direct field mutations (the swap-safe pattern). `picker_sources.rs` migrated `app.editor.document.snapshot()` (12 sites including test fixtures) to `app.ad().snapshot.clone()`. `render.rs` migrated `app.editor.render_state.load[_full]()` (2 sites) to `app.render_state.load[_full]()` (App owns its own ArcSwap), `app.editor.active_buffer` → `app.ad().buffer_kind` (6 sites), `app.editor.command_line` → `app.command_line()` (1 site), test fixtures' `&app.editor.document.snapshot()` → `&app.ad().snapshot.clone()` (5 sites). Dispatch core in `App::apply`: `self.editor.dispatch(action.clone())` → `self.mutate_editor_with(move \|e\| e.dispatch(action_for_dispatch))` with pre-cloned `action` so the outer `match action {}` at line 203 still has access. Mid-stream regex bug surfaced + fixed: GNU sed `-E` treats `\|` as alternation, not literal pipe — corrected with `[|]` character class. **Cumulative effect this slice:** TUI `app.rs + app/*.rs` production sites: 198 → 83 (115 migrated, 58% reduction). TUI `render.rs` production: ~48 → ~30 (28 sites; remaining are buffer_locals / pane_highlights / auto_submit_after_chord / last_message single-field reads + tests). TUI `picker_sources.rs`: 22 → 12 (10 migrated). GPUI peer: 1 → 0 ✅ (zero direct `self.editor.X` sites left). **GpuiApp::read_editor` helper added** so the gpui crate compiled cleanly through the same ast-grep batch in E.5c. **Test discipline ratcheted:** `lsp_close_buffer_removes_uri_mapping_for_unattached_buffer` is the first test to call `app.editor.publish_render_state()` after direct mutation; pattern documented in two `Slice 3c.final.E.5d` source comments so the next batches can follow it. lattice-host (257) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. **What remains for the actual swap:** ~83 TUI app/ sites (boot.rs deferred multi-step bodies ~25 sites; registry method calls ~8; small fields with 1–2 uses ~50); ~30 render.rs production reads (buffer_locals / pane_highlights / auto_submit_after_chord / last_message — all candidates for an App helper or RS lift in a B-extension slice); ~12 picker_sources test fixtures (legitimate to leave direct). The struct swap target is now reachable in a focused slice E.5e — the next batch closes the remainder of `app/` and adds RS mirrors for the render.rs reads. |
| 3c.final.E.5c | ✅ | — | **ast-grep automated batch + repair pass.** Used `ast-grep run --pattern 'self.editor.$METHOD($$$ARGS)' --rewrite 'self.read_editor(move \|e\| e.$METHOD($$$ARGS))' --update-all` across `crates/lattice-ui-tui/src/` and `crates/lattice-ui-gpui/src/`. **72 rewrites applied** in one shot; 57 cargo-check errors followed. Triage: (a) **29 E0596** `cannot borrow *e as mutable` — host methods need `&mut self`; bulk-sed-fixed `read_editor` → `mutate_editor_with` on the 29 specific lines (no-arg fix since `mutate_editor_with` accepts unit return too). (b) **17 E0521 + 5 E0382** borrowed-arg sites (`&state`, `&buffer`, `&mut out`, `&str`, `&Path`, `&body`) — `Send + 'static` closure bounds force these to fail; reverted to direct `self.editor.X(...)` access via second sed pass using regex `self\.read_editor\(move [|]e[|] e\.(.+)\)` → `self.editor.\1`. **First sed pass had an ERE alternation bug** (`\|` treated as alternation, not literal pipe in GNU sed `-E` mode); repaired with `[|]` character class in a corrective pass across 9 files. (c) **GPUI peer** `GpuiApp::read_editor<F,R>` helper added (paralleling TUI `App::read_editor`) so the gpui dispatch loop's bulk migration compiles; the dispatch core's two `editor.dispatch` calls switched to `mutate_editor_with`; the `do_global` Action arm with `&mut out` reverted to direct access (same `&mut` lifetime issue). (d) **1 E0277** `impl Into<String>` not Send in `App::set_message` — reverted to direct `self.editor.set_message(level, text)`; signature concession until the host method takes `String`. **Net result:** TUI direct-editor sites dropped from ~308 → 198 (35% reduction in one turn); GPUI peer from 6 → 1. **Files touched:** 18 — `app.rs`, `app/{boot,cmdline,completion,dispatch,display,edit,file_tree,folds,help,highlights,lifecycle,lsp,lsp_log_buffers,messages,mode,motions,oil,options,picker,popup,syntax,visual}.rs`, `gpui/src/lib.rs`. **Remaining ~198 TUI sites are field accesses** (`self.editor.cursor`, `self.editor.recent_files`, `self.editor.buffers`, `self.editor.document_buffer_id`, `self.editor.pane_tree`, `self.editor.option_cache`, `self.editor.config`, …) — they require per-field App helpers (getter / setter pairs) for the strict actor swap, not the closure-wrapper pattern. Top field profile: `cursor` (14), `registry` (8), `pane_tree` (8), `option_cache` (8), `document_buffer_id` (8), `popup_buffer` (7), `document` (7), `syntax` (6), `previewing` (6), `buffers` (6), `scroll` (5), `config` (5), `command_line` (5), `recent_files` (4), `modal` (4), `keymap` (4), `active_buffer` (4). The next slice (E.5d) adds those helpers + migrates the call sites. lattice-host (257) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |
| 3c.final.E.swap | ⏸ | _attempted + reverted_ | **Actor-swap landing attempt — reverted to keep workspace green; lessons captured for the focused follow-up.** Added two new actor mailbox variants (`Read`, `MutateWithReply`) carrying type-erased `Box<dyn Any + Send>` payloads so `&self` reads and `&mut self` mutations with non-unit return types can both go through the actor. `EditorActorHandle` gained `with_editor<R>(f) -> R` and `mutate_blocking_with<R>(f) -> R` typed wrappers. `App::read_editor<F,R>(f) -> R` helper added on the renderer side, parallel to the existing `mutate_editor` / `mutate_editor_with`. Attempted the literal struct swap (replace `pub editor: Editor` with `pub editor_actor: EditorActorHandle`, cfg-gated for `cfg(any(test, feature = "test-internals"))`) — produced **308 compile errors** across 18 files (49 in `render.rs`, 34 in `lsp.rs`, 31 in `lifecycle.rs`, 29 in `boot.rs`, 23 in `help.rs`, …). Far higher than the ~80 production-site estimate because many `&self` reads that the prior E sweeps deferred turned out to live in production paths (not `cfg(test)` modules), and `render.rs` has its own production-path access patterns I hadn't counted (49 sites). The cfg-gate of the field alone — which only protects `cfg(test) mod tests` blocks — doesn't help these non-test production functions. Reverted the struct swap; kept the new actor mailbox + handle methods (`Read`, `MutateWithReply`, `with_editor`, `mutate_blocking_with`) since they're additive infrastructure ready for the focused follow-up. Workspace check + lattice-host (257) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green after the revert. **The lesson for the follow-up:** migrate the remaining ~308 production-side `self.editor.X` sites to `read_editor` / `mutate_editor` / `mutate_editor_with` FIRST (one cluster per focused session), THEN do the struct swap once the count is at zero. The swap becomes trivial when every site already routes through helpers — only the helper bodies + `App::new` + the cfg-gated test accessor change. |
| 3c.final.E.5 | ✅ | — | **Heavy-batch sweep through `lsp.rs` + `gpui/lib.rs`.** `app/lsp.rs` migrated from 94 → 30 sites: 27 simple void delegates (`maybe_request_*`, `drain_*`, `do_*_request` family) routed through `mutate_editor`; 9 signal-returning ones (`drain_pending_hover`, `drain_inbound_show_documents`, `drain_inbound_apply_edits`, `drain_pending_rename`, `drain_pending_code_actions`, `drain_pending_signature_help`, `drain_pending_definitions`, `do_lsp_status`, `do_lsp_follow_link_at_cursor`, etc.) via `mutate_editor_with`; 12 with `&str`/`Option<&str>` args (`do_lsp_restart`, `do_lsp_log_clear`, `do_set_lsp_log_level`, `do_open_lsp_log`, `do_open_lsp_trace_log`, `do_toggle_lsp_trace`, `do_lsp_progress_cancel`, `open_lsp_log_in_pane`, `open_lsp_trace_log_in_pane`, `do_lsp_rename_request`, `fan_out_did_change_configuration`, `jump_to_lsp_location`) using the clone-to-owned-then-as_deref pattern; 11 with non-`&str` args (`apply_lsp_completion_accept`, `lsp_close_buffer`, `execute_lsp_command`, `open_show_message_request_picker`, `apply_lsp_code_action`, `apply_rename_workspace_edit`, `apply_lsp_text_edits`, `do_lsp_format_request(bool)`, `do_lsp_call_hierarchy_request(bool)`, `do_lsp_type_hierarchy_request(bool)`, `accept_lsp_code_lens(usize)`, `accept_lsp_color_presentation(usize)`). `gpui/lib.rs` migrated from 64 → 6 sites: most `Effect::Lsp*` and `Effect::*` match arms routed through `mutate_editor` / `mutate_editor_with`; the TranslateContext reads at `dispatch_keystroke` swapped to `rs.translator` (same pattern as TUI runtime.rs); `rebuild_gpui_theme` reads `host_theme` from `rs.theme`; the boot-time `viewport_height` seed + `activate_major_for_buffer_kind` chain bundled via `mutate_editor` / `mutate_editor_with`. **Final remaining ~80 sites** are concentrated in: (a) `&self` boolean reads in lsp.rs `*_enabled_for(buffer_id)` family — need read-side actor API for the swap; (b) the dispatch cores `editor.dispatch(action)` in App::apply and GpuiApp::dispatch_action — the actor swap target itself; (c) keymap-layer multi-step bodies in `boot.rs::sync_keymap_overlays` + `sync_completion_popup_mode_activation` — need refactor before bundling into a closure; (d) `do_global` / `do_insert_text` `&mut out` patterns that interleave host calls with App-side effect application; (e) `app.rs::Debug` impl field reads. **Cumulative through E.0–E.5:** ~250 of ~500 sites migrated; pattern proven across 18 files; lattice-host (257) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green at every checkpoint. The remaining sites are the actor-swap target: they need the swap to land (`App.editor → App.editor_actor`) which compile-breaks them all simultaneously, then refactors each cluster to the post-swap shape (read-side API / accessor methods / typed Action variants). |
| 3c.final.E.4 | ✅ | — | **Boot + LSP + GPUI peer clusters partially migrated.** `app/boot.rs` (2 muts: `sync_host_theme_from_config`, `load_persistent_config`; the complex `sync_completion_keymap_layers` multi-step body and remaining boot-time field reads stay for the final swap). `app/lsp.rs` (9 muts mechanically migrated via `replace_all`: `do_completion_resolve_focused`, `do_lsp_insert_completion_request`, `drain_pending_completion_resolve`, `drain_pending_insert_completion_lsp`, `drain_lsp_log_events`, `drain_lsp_detach_events`, `drain_lsp_progress_events`, `drain_inbound_configuration_requests`, `publish_document_opened_for_active`; the remaining 94 sites are mostly `X_enabled_for(buffer_id)` boolean reads with `&self` receivers + complex multi-step tokio task callbacks). `lattice-ui-gpui/src/lib.rs` gains parallel `GpuiApp::mutate_editor` + `mutate_editor_with` helpers (same shape as the TUI peer); 4 GPUI muts migrated (`rebuild_option_cache`, `publish_document_opened_for_active`, `ensure_subsystem_buffers`, `apply_per_language_toml_overrides`). `lattice-ui-tui/src/runtime.rs` swaps 3 `app.editor.render_state.load()` reads to `app.render_state.load()` (decouples from the `editor` field). **Cumulative through E.4:** ~110 of the audit's ~500 sites migrated; routing pattern proven across 16 files; tests stay green throughout. **What's left for the actor swap itself** (E.swap, deferred to a follow-up): ~390 `self.editor.X` sites mostly in `lsp.rs` boolean-read helpers, the dispatch core (`App::apply`'s `editor.dispatch(action)` line), the keymap-layer manipulation in `boot.rs`, GPUI peer's dispatch core + LSP effect arms, and the Action::Mutate-incompatible `&self` accessors. The swap itself is a localized change to `App::mutate_editor` / `App::mutate_editor_with` bodies (route to `editor_actor.mutate_blocking` instead of in-process), plus replacing `App.editor: Editor` with `App.editor_actor: EditorActorHandle` in `App::new` — but the type-system enforcement of paramount goal #4 only lands once the remaining ~390 sites also migrate (each one becomes a compile error after the swap until it's rewritten). lattice-host (257) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |
| 3c.final.E.2 | ✅ | — | **Routing helpers + 6 small/medium method-call clusters migrated.** Adds two `App` methods that route editor mutations through a single, swap-localizable seam: `App::mutate_editor(F)` for void-return closures and `App::mutate_editor_with<F,R>(F) -> R` for closures returning a `Send + 'static` value (typically `Vec<RendererSignal>` or `Result<_, RuntimeError>`). Both currently call `f(&mut self.editor)` + `publish_render_state()`; post-swap their bodies become `self.editor_actor.mutate_blocking(Box::new(f))` / `.mutate_blocking_with(f)` — one-line change per helper. Forward-compatible `Send + 'static` bounds applied today so callers don't need to retouch on swap. Six clusters migrated via these helpers: `app/oil.rs` (4 muts; 2 `&self` reads deferred), `app/folds.rs` (3 muts; 3 `&self` reads deferred), `app/mode.rs` (6 muts), `app/file_tree.rs` (6 muts; 4 `&self` getters deferred), `app/edit.rs` (5 simple muts: `apply_edit_blocking`, `apply_edit_batch_blocking`, `undo_blocking`, `redo_blocking`, `do_paste`; 2 complex muts `do_global`/`do_insert_text` that take `&mut out: DispatchOutcome` deferred for a careful refactor), `app/motions.rs` (5 muts: `clamp_cursor_to_buffer`, `clamp_cursor_to_active_buffer`, `ensure_cursor_visible`, `do_jump_mark`, `push_position_history`; the 3 pane_tree + popup_placement reads tried via `app.panes()` / `app.popup()` were reverted after 2 test failures — tests mutate `editor.pane_tree` directly without republishing, so the RS-backed reader returned stale geometry; a follow-up adds explicit publishes to those tests then re-migrates). The `&self` read-only delegates need a different routing (actor-side read API) and are queued for the final swap. **Total clusters complete after E.1 + E.2:** 7 (highlights + oil + folds + mode + file_tree + edit + motions); ~35 mutating sites migrated cluster-by-cluster. **Pattern proven, scaled:** further clusters are mechanical applications of `mutate_editor` / `mutate_editor_with`. **Lesson learned:** read-side migrations from `self.editor.X` to `self.X()` (RS-backed accessor) require publish discipline tests don't always have today — those migrations need to land with explicit test publishes. **Remaining sweep:** `app.rs` (16), `cmdline.rs` (16), `popup.rs` (18), `options.rs` (19), `completion.rs` (23), `help.rs` (25), `dispatch.rs` (30), `boot.rs` (31), `picker.rs` (32), `lifecycle.rs` (43), `gpui/lib.rs` (66), `lsp.rs` (103), plus `search.rs` (complex interleaving), `motions.rs` reads (3), `oil.rs`/`folds.rs`/`file_tree.rs` `&self` getters (9). Workspace check + lattice-host (257) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |
| 3c.final.E.1 | ✅ | — | **Proof-of-pattern: `app/highlights.rs` cluster fully migrated.** Adds `App::syntax_visible_spans_cell: Arc<ArcSwap<VisibleSpans>>` mirroring the existing `App::render_state` cache from 3c.atomic.A. Initialized in `App::new` (boot.rs) as a clone of `editor.syntax_visible_spans_cell` — pre-swap this is a direct clone, post-swap the same `Arc` comes from the actor handle. Three reader sites in `app/highlights.rs` migrate from `self.editor.X` to App-cached fields: `refresh_highlights` reads `&self.render_state` + `&self.syntax_visible_spans_cell` for `highlights_worker::recompute`, `highlights_for_buffer_line` reads `self.render_state.load_full()`, `visible_buffer_line_extent` reads `self.ad().snapshot.buffer.line_count()`. One residual `self.editor.publish_render_state()` call in `refresh_highlights` remains — flagged as the "final swap" tail (it becomes `self.editor_actor.apply_blocking(Action::None)` when Editor moves to the actor). **Pattern established for the per-cluster sweep:** each cluster identifies the Arcs/snapshots it needs from Editor, caches them as App fields at construction, migrates reads through those fields. The constructor (`App::new` and the GPUI peer's equivalent) is the one place that changes when the actor swap finally lands. lattice-host (257) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |
| 3c.final.E.0 | ✅ | — | **Actor mailbox extended for the slice-E sweep — infrastructure ready, swap deferred.** `EditorCommand` gains three new variants: `ApplyAndReply { action, reply: oneshot::Sender<()> }` (sync dispatch barrier), `Mutate(Box<dyn FnOnce(&mut Editor) + Send>)` (fire-and-forget closure escape hatch), and `MutateAndReply { closure, reply }` (sync variant). The actor's `run_actor` loop handles each; `Mutate` and `MutateAndReply` publish RS unconditionally after the closure runs so callers' next read sees the mutation. `EditorActorHandle` gains `apply_blocking(action)`, `mutate_blocking(closure)`, `mutate_async(closure)` returning `Result<(), ActorGone>`. New `ActorGone` error type for the "editor thread died" fatal condition. Three new tests lock the contracts: `apply_blocking` publishes a fresh RS before returning, `mutate_blocking` applies the closure and publishes, `mutate_async` lands after a subsequent `apply_blocking` barrier. **Scope reality:** the actual `App.editor: Editor → App.editor_actor: EditorActorHandle` swap touches ~500 `self.editor.X` sites across 14 files. Each cluster (LSP response handlers in `app/lsp.rs`, picker setup in `app/picker.rs`, lifecycle paths in `app/lifecycle.rs`, GPUI App helpers in `lattice-ui-gpui/src/lib.rs`, etc.) needs its own migration strategy (typed Action variant vs closure-wrap vs Arc-handle exposure). That sweep is deferred to slices E.1–E.N as a per-cluster effort, each landing green independently. The closure escape hatch will be retired cluster-by-cluster as typed variants land — the discipline tax exists but is bounded and tracked. lattice-host (257) + lattice-ui-gpui (23) + lattice-ui-tui (1392) tests green. |

**Cumulative test sweep (after 3c.atomic.F):** 228 host + 1410 ui-tui + 21 ui-gpui green. The two original primitives (`render_state` + `per_buffer_cache`) gained the editor-actor module's 10 tests covering ping/pong, apply→publish, drop-joins, idempotent shutdown, silent tick, and each typed setter command publishing a fresh `Arc<RenderState>`.

**Pattern observed across 3c.atomic.* reader migrations:** every migration uncovered at least one out-of-dispatch write site that didn't publish — `set_viewport_height` in D, `open_popup` and `install_help` in E. These were latent bugs masked by the renderer reading directly from `editor.X`; migrating to `ad()` forced each writer to commit visibly to the actor contract. This is the architectural enforcement of paramount goal #4 — by construction the renderer cannot observe writes that haven't been published.

**Status of the post-`:e` UI hang:** Resolved by Slices 1 + 2. The flagship reproduction (`:e crates/lattice-lsp/src/lib.rs` against this workspace's populated `target/`) no longer freezes; the watcher install completes in 384 ms (off the renderer thread) and ignored `target/` events never enter the channel. Diagnostic markers (`tracing::info!` traces around the watcher install + fs-event drain) remain in place for future regression detection.

---

## GPUI cell-grid renderer (2026-05-26)

Anchor: [`../architecture/cell-grid-renderer.md`](../architecture/cell-grid-renderer.md) + design.md §5.6 + paramount goal #1.

The held-j stutter reported on 2026-05-25 (cursor advances a few rows then visibly freezes until key release) traced to `EditorElement::prepaint`'s per-frame `WindowTextSystem::shape_line` calls. The probe (commit `734486d`) confirmed the cost is structural to `shape_line` itself, not call count: a renderer-side content cache (Layer A, retired) reduced calls from 216 to 2 per paint but each remaining call cost the same ~10 ms, conserving total wall time at ~20-30 ms/paint during scroll.

A renderer that ships `shape_line` on the code-buffer hot path cannot hit goal #1's ≤ 8 ms keystroke→glyph budget. The cell-grid + glyph atlas decomposition (alacritty / wezterm / VSCode / browser model) replaces it for code/terminal/file-tree/synthetic buffers; the Shaped path stays for help/markdown/popup surfaces where rich text matters.

| Slice | Status | What lands |
|---|---|---|
| S1 cell substrate | ✅ landed | New `lattice-cells` crate: `Cell` (16-byte), `CellRow`, `CellChunk`, `CellMatrix`, `MatrixVersion`. Pure data, slicing API, 31 unit tests for whole-doc / multi-chunk / fold-elision / out-of-bounds. |
| S2 cell-builder worker | ✅ landed | Tokio worker in `lattice-host`, sibling of `highlights_worker`. Subscribes to (text, syntax, inlay, fold, theme) version cascade via `MatrixVersion`; coalesces via `Notify` permits; rebuilds intersecting chunks (S2.4.b); publishes via `ArcSwap<CellMatrix>` on `RenderState`. Edits past the change point shift `start_source_line` without rebuilding cell content. All sub-slices landed (S2.1–S2.5); S3 (TUI cutover) and S4 (GPU paint_cells) can now consume the matrix. |
| S2.1 plumbing | ✅ landed | `CellsRenderState` in `render_state.rs` (matrix `Arc<ArcSwap<CellMatrix>>`, wake `Notify`, `MatrixVersion` axes, snapshot/syntax/inlay/folds/theme inputs). `Editor.cells_matrix_cell` + `Editor.cells_wake`. `publish_render_state` populates the cells state. Workspace tests green. |
| S2.2 minimal worker | ✅ landed | `cells_worker.rs` sibling of `highlights_worker.rs`. Wake → read RS → build whole-doc `CellMatrix` from rope text alone (ASCII codepoints, no syntax fg, no inlays, no folds). Spawned from `editor_boot.rs` with stable-Arc identity over `cells_matrix_cell` + `cells_wake`. `WorkerDecision` covers `Clear` / `CacheHit` / `Recomputed`; the published matrix carries the publisher's `MatrixVersion` so subsequent wakes short-circuit via `differs_from`. 5 unit tests (clear / cache-hit / recompute / version-bump / empty-text). |
| S2.3 full cell content | ✅ landed | Syntax span → `cell.fg`, inlay-hint splicing, fold elision, theme palette wiring. All three sub-slices (S2.3.a / S2.3.b / S2.3.c) landed; matrix is feature-complete content-wise. S3 (TUI cutover) can now start in parallel with S2.4–S2.5 perf work. |
| S2.3.a syntax fg + theme | ✅ landed | `Hash` derived on host `Theme` / `Style` / `Color` / `NamedColor` / `Modifiers`. `CellsRenderState` gains `theme: Theme`. `dispatch.rs` folds `hash(host_theme)` into `MatrixVersion::theme`. Worker calls `snapshot.highlight_lines(...)` when the syntax snapshot is current, walks per-line spans, resolves `Style → fg` via `theme.syntax_style`. Stale syntax (snapshot text_version < doc text_version) falls back to default fg. 4 tests cover keyword/comment fg, no-handle default-fg, stale-syntax fallback, theme-version-bump rebuild. |
| S2.3.b inlay splicing | ✅ landed | `dispatch.rs` clones the gated inlay-hint Arc into the cells substate (single cache scan, two readers). Worker buckets by line, splices each `(orig_byte, text)` during the char walk: emits one INLAY-flagged `Cell` per inlay char (hard-coded DarkGray fg), records `(orig_byte, char_width)` on `CellRow::inlay_offsets`. Trailing inlays (orig_byte == line_len) splice at EOL; multiple inlays per line sort ascending by byte. Inlay theme slot is follow-up alongside #19. 5 tests cover mid-line splice + flags + offsets, byte ordering, byte-0 leading splice, trailing-EOL splice, inlay-version-bump rebuild. |
| S2.3.c fold elision | ✅ landed | `CellsRenderState` gains `foldenable: bool`; dispatch folds it into `MatrixVersion::folds` for the cells axis (the syntax substate keeps its list-only hash since it doesn't elide). Worker builds `FoldIndex::from_folds(&cells.folds, cells.foldenable)` and skips lines where `line_inside_closed_fold(line) == true`. The fold's `start_line` stays visible (vim semantics — fold marker renders there); interior lines `(start_line, end_line]` are elided. `CellMatrix::source_line_count` preserves the pre-fold count; `visible_line_count` reflects post-fold rows. 4 tests cover closed-fold interior elision, open-fold no-op, foldenable-off disables elision, two non-overlapping closed folds. |
| S2.4 chunked mode | ✅ landed | Chunked mode + incremental rebuild. S2.4.a landed the structural switch; S2.4.b landed the incremental rebuild with prefix-reuse / rebuild zone / suffix-shift partitioning. |
| S2.4.a chunked-mode switch | ✅ landed | `pick_chunk_size(viewport_height, line_count)` returns `WholeDoc` when `viewport_height == 0` or `line_count ≤ 4 × viewport_height`, otherwise `Chunked(next_pow2(2 × viewport_height))` (clamped to a 16-line floor). `build_matrix` builds a `ChunkInputs<'_>` borrow bundle once and `build_chunk_rows(&inputs, start, end)` produces each chunk's rows. 6 tests. |
| S2.4.b incremental rebuild | ✅ landed | New `lattice_cells::EditDelta { start_line, lines_removed, lines_added }` + `CellRow::with_source_line` + `CellChunk::shifted_by` (cell `Arc` payload shared via refcount-only clone). `Editor.last_edit_for_cells` tracks the per-publish-cycle single-edit delta in `publish_document_changed` (cleared on batch / second-edit / undo / redo); `build_render_state` `take()`s it. `CellsRenderState.last_edit` carries it to the worker. `try_incremental_build` checks eligibility (single-text-edit step, other axes unchanged, mode + chunk_size compatible, line-count consistency); on success the chunked path partitions into prefix-reuse (`Arc::clone`), rebuild zone (`build_chunk_rows` over `[rebuild_lo, rebuild_hi)`), and suffix-shift (`shifted_by(net_delta)`). Whole-doc rebuilds the only chunk. New `WorkerDecision::RecomputedIncremental` variant. `build_render_state` / `publish_render_state` / `set_selections_blocking` changed from `&self` to `&mut self`; renderer-side `read_editor(|e| publish_render_state)` → `mutate_editor`. 7 worker tests + 3 substrate helper tests cover the happy paths + eligibility-bail paths. |
| S2.5 coalescing | ✅ landed | tokio `Notify` permit-style coalescing verified end-to-end. A burst of N wakes during one build produces exactly 2 builds (original + tail catch-up against the latest `render_state.load_full()`); no explicit debounce needed. `paint_request` fires on `Recomputed` / `RecomputedIncremental` / `Clear`; `CacheHit` stays silent. Cells + highlights workers write to independent `ArcSwap` cells with no contention. Worker docs now explicit on the coalescing contract. 4 tests cover decision state machine under interleaved wakes, no-cross-talk between cells and spans cells, full end-to-end through the tokio `run` loop, and burst-coalescing via `Notify` permits. |
| S3 TUI cutover | ✅ landed | TUI document-body rendering now exclusively consumes `Arc<CellMatrix>`. Sub-slices: S3.a (substrate modifier bits), S3.b (TUI converter), S3.c.0 (body wiring), S3.c.1–4 (per-overlay validation: whitespace / semantic tokens / bg-layer overlays / fold suffix + inlay splice), S3.c.final (retire RowPrepaint fallback). Highlights worker continues to publish for markdown / help / messages bodies. |
| S3.a cell modifier bits | ✅ landed | `Cell::flags` gains BOLD / ITALIC / UNDERLINE / DIM / REVERSE constants + accessors. `cells_worker::resolve_style(theme, style) -> (fg, flags)` replaces `resolve_fg`; the worker packs `theme.syntax_style(style).modifiers` into cell flags via `modifiers_to_flags`. `ChunkInputs.default_flags` propagates the theme's default modifiers. Inlay cells stay isolated from syntax modifiers. 5 tests. |
| S3.b TUI body consumer | ✅ landed | TUI converter module `lattice_ui_tui::cells_render`: `cell_row_to_combined_spans` (post-inlay columns) + `cell_row_to_source_spans` (INLAY-filtered; source-byte compatible with the legacy body-spans shape). Walks cells once, groups consecutive cells by `(fg, bg, modifier_bits)`, emits one `ratatui::text::Span` per group. `cell_to_style` maps `cell.fg/bg != 0` to `Color::Rgb` and leaves `0` unset (host `Color::Default` semantics); modifier flags map to `Modifier::BOLD/ITALIC/UNDERLINED/DIM/REVERSED`. 13 tests cover the empty case, grouping, modifier table + composition, INLAY filtering, all-INLAY empty, non-ASCII, and a realistic keyword/identifier/paren row. Wiring into `draw_buffer` deferred to S3.c. |
| S3.c TUI cutover | 🔄 in progress | Sliced one overlay at a time. S3.c.0 = body wiring; S3.c.1–4 = per-overlay validation; S3.c.final = retire RowPrepaint fallback. |
| S3.c.0 body wiring | ✅ landed | `CellMatrix::row_at_source_line(target) -> Option<&CellRow>` (linear chunk walk, sub-µs at typical scale). `compose_visible_lines_inner`'s non-messages body branch prefers cell-derived spans via `cell_row_to_source_spans(row)` + `truncate_spans_to_width`; falls back to RowPrepaint when the matrix has no row (boot frames / folded rows / out-of-coverage). Fallback is bit-identical to the pre-cutover shape so matrix-lag windows don't flicker. Downstream overlays consume the body opaquely and continue to work — 1420+ TUI tests still pass. Workspace 3157 / 3157 (was 3154, +3 substrate lookup tests). |
| S3.c.1 whitespace overlay | ✅ landed | 7 focused tests in `cells_render::tests` feed cell-derived source spans through `render::apply_whitespace_decoration` and assert glyph substitution lands at the correct source-byte positions: mid-line space, leading whitespace, trailing whitespace, tab, EOL marker append, all-off noop, multi-fg span boundary. Confirms the classifier walks bytes opaquely regardless of how spans were split. Workspace 3164 / 3164 (was 3157, +7). |
| S3.c.2 semantic tokens | ✅ landed | `apply_semantic_token_overlay` visibility bumped to `pub(crate)` for test access (matches `apply_whitespace_decoration` precedent). 6 focused tests validate the overlay walks cell-derived spans correctly: partial coverage splits into pre/mid/post; full coverage replaces fg across the row; out-of-range no-op; existing BOLD modifier preserved while overlay adds ITALIC; fg replaced + bg from earlier pass preserved; multi-span body with overlay straddling a fg boundary still fires the overlap region's fg-replace. Workspace 3170 / 3170 (was 3164, +6). |
| S3.c.3 bg-layer overlays | ✅ landed | `apply_match_overlay` (visual / hlsearch / current_match / substitute / doc-highlight — REPLACE-style) and `apply_underline_overlay` (diagnostics — ADDITIVE) visibility bumped to `pub(crate)`. 8 focused tests pin the contracts: match overlay splits at partial coverage / covers full row / no-ops out-of-range / walks across multi-span boundaries / sequential overlays REPLACE rather than merge (cell's BOLD dropped when overlay style omits it) / composes by sequence (later overlay overrides earlier bg); underline overlay ADDs UNDERLINED while preserving fg, bg, and existing BOLD on the overlap region. Workspace 3178 / 3178 (was 3170, +8). |
| S3.c.4 fold + inlay splice | ✅ landed | `splice_virtual_text_into_spans` visibility bumped to `pub(crate)`. 7 focused tests pin the contract that the post-overlay inlay splice and the closed-fold suffix both compose with cell-derived bodies: byte-0 prepend / mid-span split / span-boundary clean insert (no split) / past-end append / multi-inlay reverse-byte-order sequencing / fold suffix tail append / fold-suffix-after-inlay-splice ordering. Workspace 3185 / 3185 (was 3178, +7). |
| S3.c.final retire RowPrepaint | ✅ landed | `compose_visible_lines_inner`'s document-body branch retired the RowPrepaint fallback. The cell-derived path is the sole source for document bodies; empty-matrix windows (boot frame before the cell-builder's first publish, or buffer-switch gap) emit `Span::raw(line_text)` — semantically equivalent to the legacy fallback's degraded state. Highlights worker continues to run; `view.visible_rows` stays published for markdown / help / messages bodies in other render functions. Workspace 3185 / 3185 — no regression. S3 closes. |
| S4 GPUI cutover | 🔄 in progress | Sliced like S3.c. S4.0 converter landed; S4.1 wires it into `EditorElement::prepaint` with a 3-way fallback (cells → prepaint → legacy); S4.2 propagates modifier bits into TextRun font/flags; S4.3 retires `shape_row_from_prepaint` for active-pane document bodies; S4.final replaces `shape_line` with a glyph-atlas `paint_cells`. Status line / popup / picker / help stay on the Shaped path throughout. |
| S4.0 cell→TextRun converter | ✅ landed | `lattice-ui-gpui::cells_paint` module (gated behind `window`). `cell_row_to_text_runs(row, font) -> (combined_text, Vec<TextRun>, inlay_offsets)` produces the exact triple `EditorElement::prepaint` feeds into `WindowTextSystem::shape_line`. Walks `row.cells` once, groups consecutive same-fg cells into one `TextRun` (mirrors `build_line_with_inlays`'s collapse), passes `row.inlay_offsets` through verbatim. `make_run_with_color` visibility bumped to `pub(crate)`. S4.0 keeps fg-only modifier coverage to match legacy visual parity; BOLD / ITALIC / UNDERLINE / DIM / REVERSE propagation deferred to S4.2. 8 tests cover empty / single / adjacent-merge / fg-break / inlay-offsets passthrough / inlay-cells-separate-run / realistic keyword+ident+paren row / non-ASCII utf-8 byte length. |
| S4.1 body wiring | ✅ landed | `EditorElement` gained `cell_matrix: Option<Arc<CellMatrix>>` populated from `render_state.cells.matrix.load_full()` at the `window.rs` construction site (active pane only — inactive panes pass `None`, mirroring the `visible_rows` split). `prepaint`'s body added `shape_row_from_cells(row, window)` closure that runs `cell_row_to_text_runs` → `shape_line`. Both dispatch sites (no-gutter fallback walk + gutter-driven walk) now use a 3-way `cells → prepaint → legacy` if-let chain: cells looked up by absolute source line via `CellMatrix::row_at_source_line`, falling through to `shape_row_from_prepaint` and finally `shape_row` on `None` (folded rows / boot frames / buffer-switch gap). 8 cells_paint tests + 363 host + 1461 cells+TUI all green. Mirrors S3.c.0. |
| S4.2 modifier propagation | ✅ landed | `cell_row_to_text_runs` regrouping key changed from `fg` only to `(fg, bg, style_bits)` where `style_bits = flags & (BOLD\|ITALIC\|UNDERLINE\|DIM\|REVERSE)`. Per-cell `TextRun` construction now reads modifier bits: BOLD → `font.weight = FontWeight::BOLD`; ITALIC → `font.style = FontStyle::Italic`; UNDERLINE → `underline = Some(UnderlineStyle { thickness: 1px, color: None, wavy: false })`; DIM → fg + bg RGB channels multiplied by `0.6`; REVERSE → fg ↔ bg swap (with documented bg=0 fallback). `cell.bg != 0` also passes through as `TextRun.background_color`. `INLAY` and `WS_MARKER` are deliberately excluded from `STYLE_FLAGS_MASK` — provenance flags don't break runs. 18 tests (10 grouping + 8 modifier) cover the table and composition cases. |
| S4.3 retire prepaint path | ✅ landed | `shape_row_from_prepaint` closure + plumbing retired in `EditorElement::prepaint`. Both dispatch sites collapse from a 3-way `cells → prepaint → legacy` chain to a 2-way `cells → legacy`. `EditorElement.visible_rows` field, the `RowRun` import, the `active_rows_guard` load in `window.rs`, and the `visible_rows:` initializer all removed. `make_run_with_color` stays `pub(crate)` because `build_line_with_inlays`'s legacy `LineRunBuilder` is the remaining consumer (retires in S4.final). Highlights worker continues to publish `rs.syntax.visible_rows` — TUI's markdown / help / messages bodies still consume it. 1461 cells+TUI + 363 host + 41 cells + 18 cells_paint tests all green. |
| S4.final per-cell paint_glyph | 🔄 in progress | Replaces `shape_line` on the active-pane document body. GPUI 0.2.2 already exposes `Window::paint_glyph` with an internal sprite atlas — we don't reimplement the atlas, only the per-codepoint `GlyphId` resolution + the per-cell paint loop. Sliced into S4.final.a (glyph-id cache), .b (paint_cells path + toggle), .c (hit-testing on cell grid), .d (modifier paint), .e (emoji + font fallback), .f (retire `shape_line` on the document body). Status line / popup / picker / help / gutter stay on the Shaped path throughout. |
| S4.final.a glyph-id cache | ✅ landed | `GlyphResolver` infrastructure in `crates/lattice-ui-gpui/src/glyph_resolver.rs`. `GlyphKey { font_id, ch }` keys a `HashMap` to `Option<ResolvedGlyph>`. `ResolvedGlyph` carries `font_id` (which may differ from `key.font_id` for fallback-resolved glyphs), `glyph_id`, and `is_emoji` (dispatches between `paint_glyph` and `paint_emoji`). `Some(None)` records sticky-unresolvable codepoints so we don't re-run layout for known `.notdef` cases. 9 cache-only unit tests cover empty / insert / sticky-None / distinct keys / re-insert / emoji flag / fallback FontId / clear. Wiring into `GpuiApp` + the resolve path (which calls `WindowTextSystem::layout_line`) lands in S4.final.b. |
| S4.final.b paint_cells path | ✅ landed | `paint_cells_row(...)` walks one `CellRow` emitting per-cell bg `paint_quad` + `paint_glyph` / `paint_emoji`. `GlyphResolver::resolve(ch, font, size, window)` added: cache hit returns; miss runs `layout_line` and extracts the first glyph's `(font_id, id, is_emoji)`. `EditorView` carries `Arc<Mutex<GlyphResolver>>` (shared across panes). `EditorElement` carries the same Arc + prepaint stashes `font`, `font_size`, `text_ascent` for the paint-side `paint_glyph` calls. Body paint loop branches per row: when `paint_cells_enabled()` (env var `LATTICE_PAINT_CELLS=1`) + active pane + matrix covers the source line, `paint_cells_row` runs and `ShapedLine::paint` is skipped for that row. Otherwise legacy `ShapedLine::paint` runs. Cursor + overlays continue to use prepaint ShapedLine metrics until S4.final.c migrates hit-testing. 71 lattice-ui-gpui + 41 cells + 363 host + 1461 cells+TUI all green. |
| S4.final.c cell-grid hit-test | ✅ landed | Survey showed `ShapedLine::closest_index_for_x` was never used and the editor body has no mouse handler today; cursor positioning already uses monospace x/col math. This slice landed *infrastructure* for the future mouse-select handler. New `crate::hit_test` module: `x_to_combined_col`, `col_to_x`, `combined_col_to_byte` (handles inlay anchors with cursor-before-byte semantics; clicks inside an inlay snap to its source-byte anchor). 13 tests cover round-trips, defensive clamps, multi-byte chars, single + multi-inlay composition, EOL inlay clamp. No mouse handler wired in — that's a separate UX slice. |
| S4.final.d modifier paint | ✅ landed | `paint_cells_row` now applies all five modifier bits per cell. `apply_color_modifiers(cell)` runs REVERSE swap (fg↔bg, with `bg=0` documented behaviour) then DIM attenuation (each channel ×0.6 via `dim_channel`). `cell_font_variant(base, cell)` returns a Font with `weight = BOLD` / `style = Italic` per cell — resolved via the existing `GlyphResolver` cache (different FontIds for variants). `cell.is_underline()` emits a 1-px `paint_quad` at `baseline + 2px` (constant offset; `TextSystem::underline_position` isn't public in gpui 0.2.2). 12 unit tests cover the helpers. Visual parity with the TUI cells path and the cells_paint TextRun converter. |
| S4.final.e emoji + fallback | ✅ landed | Survey confirmed `layout_line` already does platform-level fallback (returned `LineLayout.runs[0].font_id` carries the actual rendering font; `glyphs[0].is_emoji` flags colour glyphs) and `paint_cells_row` already dispatches `paint_emoji` vs `paint_glyph`. This slice added: `is_notdef(GlyphId(0))` helper (`#[allow(unsafe_code)]` for the transmute since `GlyphId`'s field is `pub(crate)`); sticky-`None` caching for the three unresolvable cases (empty runs, empty glyphs, `.notdef` glyph); three `tracing::debug!` lines (fallback selection, notdef, empty) for `RUST_LOG=lattice_gpui::glyph_resolver=debug` observability. 1 new test (`is_notdef_detects_zero_glyph_id`); 10 total glyph_resolver tests. |
| S4.final.f retire toggle | ✅ landed (scoped) | `paint_cells_enabled()` env-var toggle retired. `EditorElement::paint`'s active-pane body uses `paint_cells_row` unconditionally when `cell_matrix.is_some()`. Legacy `shape_line` path stays for inactive panes + active-pane transient fallback (boot / buffer-switch / folded). Truly deleting `shape_row` / `build_line_with_inlays` / `LineRunBuilder` / `make_run_with_color` / `cells_paint::cell_row_to_text_runs` requires inactive panes to also flow through cells — deferred follow-up needing the cells worker to publish for non-active buffers. 97 lattice-ui-gpui tests green. |
| S5 bench + tuning | ✅ landed (cells_worker side) | `crates/lattice-host/benches/cells_worker.rs` Criterion bench for `recompute` across three workloads × three line counts. Numbers in `benchmarks.md`: cache_hit ~33 ns flat, incremental_build ~103 µs @ 5k lines, full_build ~1.9 ms @ 5k lines — all well within paramount goal #1's 8 ms keystroke→glyph budget. Paint-side benches (`paint_cells_row`, `GlyphResolver::resolve` miss path) defer (need live GPUI window). |
| S6 cleanup | ✅ landed | Stripped `[held-j-shape]` / `[held-j-render]` / `[held-j-timing]` / `[held-j-paint]` probes and their `Instant::now()` scaffolding from `EditorElement::prepaint` + `paint`, `window.rs::Render::render` + `on_key_down`, and `GpuiApp::dispatch_action`. Held-j on macOS is smooth; WSLg stall is platform-side (compositor backpressure), confirmed via cross-platform reproduction test. S5 Criterion bench + the existing perf benches remain the measurement vehicles going forward. Cell-grid plan closes. |

Estimated calendar: 6–9 weeks across all slices.

**Decoration assignment.** Cells carry codepoint + theme-resolved fg + bg + flags. Inlay hints, syntax colours, fold elision are cell-baked. Cursor, selection, hlsearch, current_match, visual_range, doc_highlights, diagnostic underlines, gutter (line_num, fold_marker, severity), substitute-preview are overlays computed at paint time. Any decoration that doesn't change glyph layout is an overlay → appears on the next paint with zero matrix work.

**Trade-off accepted.** One-frame cursor-cell lag after a typed character: cursor renders at new column immediately (overlay); the cell under the cursor reflects pre-edit content for at most one frame. Invisible during typing; observable only in constructed stress tests. This is the price of going fully async on edits, and it aligns with paramount goal #4 (no blocking on edit/UI thread).

**Layer A (retired).** The shaped-line cache (`crates/lattice-ui-gpui/src/shape_cache.rs`) consolidated `shape_line` calls without reducing per-call cost. Validated empirically; reverted in tree as part of the cell-grid plan kickoff. See `cell-grid-renderer.md` § Trade-offs for the audit trail.

---

## live-picker (debounced re-run on query change)

**`:picker grep` and any future "engine produces the candidate set"
source** -- the source's external program (grep, future LSP
workspace-symbols, future shell-driven enumeration) re-runs on each
debounced keystroke instead of the picker fuzzy-filtering a single
inline batch. Anchor: ../architecture/design.md §5.9.7 ("Live
sources" subsection).

| Slice         | Status | What lands |
|---------------|--------|------------|
| live-pkr.1    | ✅     | **Trait + picker primitive seam.** `PickerSourceSpec.live: bool` (default false), `PickerSourceGenerator::on_query_changed(&ctx, &query) -> Option<SourceResult<PickerInitResult>>` (default `None`). `Picker.live_source_mode` + `set_live_source_mode(bool)`; `Picker::refilter` short-circuits when `live_source_mode == true` (renders `raw` 1:1, no fuzzy scoring, no MRU). `App::seat_picker_from_pairs` reads `entry.spec.live` and flips the picker into live mode. No production source opts in yet; all existing pickers bit-identical. Tests: `live_source_mode_bypasses_fuzzy_refilter`, `live_source_mode_off_keeps_existing_fuzzy_behaviour`. |
| live-pkr.2    | ✅     | **App-side debounce + cancellation.** `LivePickerQueryState` + `InFlightLiveQuery` on `App`, installed by `open_picker` when `entry.spec.live` is true. `LIVE_PICKER_DEBOUNCE = 150ms` constant. New methods: `bump_live_picker_debounce` (called from `PickerAppend` / `PickerBackspace` dispatch), `drain_pending_live_picker_query` (main-loop tick — fires `on_query_changed` on deadline expiry, pumps in-flight rx, stale-result detection via `launched_for_query` vs current `picker.query`). `do_picker_dismiss` cancels any in-flight task. Tested with synthetic `LiveStubSource` (registry sidestep — Arc-shared, so hand-installed `live_picker_query` state). Tests: keystroke→debounce→fire, burst-coalesce, dismiss-clears-state. |
| live-pkr.3    | ✅     | **`GrepSource` flips to live + optional initial pattern.** `spec.live = true`. `pattern` arg now `ArgDefault::None` — no-arg opens empty, arg seeds the prompt. `init()` returns `Inline(empty)` on no-arg, `Future(spawn_blocking(run_grep))` on initial-pattern (first grep no longer blocks UI). `on_query_changed()` returns `Inline(empty)` for empty/whitespace, `Future` for real queries. `spawn_blocking` because `run_grep` shells out via std-sync `Command::output` — running it on the async-runtime workers would pin a worker. Initial-query seed plumbing: `LivePickerQueryState.initial_query: Option<String>` set by `open_picker`, consumed (taken) by `seat_picker_from_pairs` so `:picker grep TODO` opens with `query = "TODO"` ready to extend. Tests: `grep_source_empty_args_returns_empty_inline`, `grep_source_on_query_changed_empty_short_circuits`, `grep_source_spec_is_live`, `open_picker_grep_no_args_installs_live_state`, `open_picker_grep_with_initial_pattern_stashes_query_until_seat`, `picker_grep_seeds_query_on_seat_when_initial_query_stashed`. |
| live-pkr.4    | ✅     | **Docs.** ../architecture/design.md §5.9.7 gains a "Live sources" subsection contrasting the v1 debounced-future shape against the future fully-streaming model; reasons why grep specifically fits Future-per-query rather than incremental Stream. Test-counts row updated. |

**Deferred (post-1.0):** result-cap echo when grep hits `max_hits` and truncates; in-row match-range highlights (the grep backends emit column info — `messages_line_spans`-style span-builder would work); per-source `is_live()` overrides for future sources that want live behaviour conditionally (e.g. workspace-symbols falls back to static when the server has no `workspace/symbol`).

---

## Vim grammar coverage (Phase 1 catalog)

This section enumerates every named primitive in vim's grammar against its
status here. Anchor: ../architecture/design.md §5.2 + the seven unifications in §5.10–§5.12.

### Modal states

| State              | Status           | Anchor    | Notes                                                                                                                                                                                                                                                                                                     |
|--------------------|------------------|-----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Normal             | ✅               | §5.2      | Plus block cursor in TUI                                                                                                                                                                                                                                                                                  |
| Insert             | ✅               | §5.2      | Plus bar cursor                                                                                                                                                                                                                                                                                           |
| Visual (Charwise)  | ✅               | §5.2, B.1 | Selection extends, operators on Range::Selection                                                                                                                                                                                                                                                          |
| Visual (Linewise)  | ✅               | §5.2      |                                                                                                                                                                                                                                                                                                           |
| Visual (Blockwise) | ✅ d/y/c/I/A/>/< + paste | §15:18    | Ctrl-V (or Ctrl-Q on terminals that hijack Ctrl-V) enters; render highlights the rectangle; `d` / `y` / `c` dispatch per-row in the dispatcher with merged Edits + one Blockwise yank. `>` / `<` indent each covered line. `I` / `A` enter Insert at the block's left / right column on the top row; on Esc the typed prefix is replicated to every other row at the same column (rows shorter than the column are skipped, vim's behavior). `YankKind::Blockwise` paste replays each row at the same column. |
| Operator-Pending   | ✅               | §5.2      | Resolved through translate_normal pending state                                                                                                                                                                                                                                                           |
| Command (`:`)      | ✅               | §5.9.10   | Rich minibuffer scope is partial; full spec is post-Phase-1                                                                                                                                                                                                                                               |
| Search (`/`, `?`)  | ✅               | §5.9.10   | Live preview wired (hlsearch on every keystroke); fancy-regex backend                                                                                                                                                                                                                                     |
| Replace (`R`)      | ✅               | §5.2      | Backspace-restore wired                                                                                                                                                                                                                                                                                   |

### Motions (Reflex-class)

| Motion                            | Key            | Status | Anchor                                                |
|-----------------------------------|----------------|--------|-------------------------------------------------------|
| char_left / char_right            | h, l           | ✅     | §5.2.2                                                |
| line_up / line_down               | k, j           | ✅     | §5.2.2                                                |
| line_start / line_end             | 0, $           | ✅     | §5.2.2                                                |
| first_non_blank                   | ^              | ✅     | §5.2.2                                                |
| word_forward                      | w              | ✅     | §5.2.2                                                |
| word_backward                     | b              | ✅     | §5.2.2                                                |
| word_end                          | e              | ✅     | §5.2.2                                                |
| WORD_forward / backward / end     | W, B, E        | ✅     | Whitespace-delimited variants                         |
| paragraph_forward / backward      | }, {           | ✅     | §5.2.2                                                |
| sentence_forward / backward       | ), (           | ✅     |                                                       |
| goto_first_line / goto_last_line  | gg, G          | ✅     | §5.2.2                                                |
| find_char_forward / backward      | f, F           | ✅     | §5.2.2                                                |
| till_char_forward / backward      | t, T           | ✅     | §5.2.2                                                |
| find_repeat / find_repeat_reverse | ;, ,           | ✅     |                                                       |
| viewport_top / middle / bottom    | H, M, L        | ✅     | App-level (needs viewport_height)                     |
| word_search_forward / backward    | *, #           | ✅     | §B.3 informally                                       |
| match_bracket                     | %              | ✅     | App-level                                             |
| jump_history_back / forward       | Ctrl-O, Ctrl-I | ✅     | §5.1.1 unified ring (filtered to AutoJump+PluginPush) |
| mark_history_back / forward       | g;, g,         | ✅     | §5.1.1 unified ring (filtered to NamedMark)           |
| page_down / page_up               | Ctrl-F, Ctrl-B | ✅     | App-level                                             |
| scroll_line_up / down             | Ctrl-Y, Ctrl-E | ✅     | App-level                                             |
| half_page_down / up               | Ctrl-D, Ctrl-U | ✅     | Hardcoded count 10                                    |
| mark jumps                        | 'a, \`a        | ✅     | §5.1.1                                                |

### Operators (Reflex-class for sync prelude)

| Operator     | Key      | Status | Anchor                                     |
|--------------|----------|--------|--------------------------------------------|
| delete       | d, dd, D | ✅     | §5.2.2                                     |
| change       | c, cc, C | ✅     | §5.2.2                                     |
| yank         | y, yy, Y | ✅     | §5.2.2                                     |
| indent_left  | <        | ✅     | §5.2.2                                     |
| indent_right | >        | ✅     | §5.2.2                                     |
| upper        | gU       | ✅     | §5.2.2                                     |
| lower        | gu       | ✅     | §5.2.2                                     |
| toggle_case  | g~       | ✅     | §5.2.2                                     |
| filter       | !        | ⛔     | Subprocess pipe; depends on `:!cmd` (§B.6) |
| format       | gq       | ⛔     | Depends on plugin / formatter              |
| join_lines   | J, gJ    | ✅     | App-level (not a grammar operator)         |

### Text objects

| Text object              | Key       | Status | Anchor                                                                                                                                                                                                  |
|--------------------------|-----------|--------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| inner_word / around_word | iw / aw   | ✅     | §5.2.2; alphanum + `_` run                                                                                                                                                                              |
| inner_WORD / around_WORD | iW / aW   | ✅     | Whitespace-delimited; punctuation kept (`foo.bar` is one WORD). Shares `text_object_inner_word_class` / `_around_word_class` with `iw` / `aw` -- only the byte classifier differs (`is_big_word_byte`). |
| inner_quote_dbl / around | i" / a"   | ✅     |                                                                                                                                                                                                         |
| inner_quote_sgl / around | i' / a'   | ✅     |                                                                                                                                                                                                         |
| inner_quote_btk / around | i\` / a\` | ✅     |                                                                                                                                                                                                         |
| inner_paren / around     | i( / a(   | ✅     |                                                                                                                                                                                                         |
| inner_bracket / around   | i[ / a[   | ✅     |                                                                                                                                                                                                         |
| inner_brace / around     | i{ / a{   | ✅     |                                                                                                                                                                                                         |
| inner_angle / around     | i< / a<   | ✅     | `<…>` pair; reuses `text_object_inner_brackets` / `_around_brackets`. Both `<` and `>` resolve to the same target.                                                                                      |
| inner_tag / around       | it / at   | ✅     | XML/HTML tags                                                                                                                                                                                           |
| inner_paragraph / around | ip / ap   | ✅     |                                                                                                                                                                                                         |
| inner_sentence / around  | is / as   | ✅     |                                                                                                                                                                                                         |

### Counts, registers, ranges, marks, macros, dot-repeat

| Feature                                       | Status                                | Anchor         |
|------------------------------------------------|---------------------------------------|----------------|
| Numeric prefix counts (3w, 5j, 2dw, 2d3w)      | ✅                                    | §5.2.2         |
| Register prefix `"<reg>`                       | ✅                                    | §5.2.2         |
| Named registers `"a-z`, `"A-Z`                 | ✅ (uppercase replaces, append TBD)   | §5.2.2         |
| Numbered registers `"0-"9`                     | ✅ (storage; `"0` auto-populate TBD)  | §5.2.2         |
| Black hole register `"_`                       | ✅                                    |                |
| System / clipboard `"+`, `"*`                  | ⚠️ storage only; no clipboard wire-up  | §15 (deferred) |
| Last-yank `"0`                                 | ⚠️ readable; auto-populate TBD         |                |
| Ex-command ranges (1,5 / % / 'a,'b / patterns) | 🟡 % and CurrentLine work             | §5.2.2         |
| Marks (m / ' / \`)                             | ✅                                    | §5.1.1         |
| Mark for last visual (`'<`, `'>`)              | ⛔                                    |                |
| gv (reselect last visual)                      | ✅                                    |                |
| Macros (q, @, @@)                              | ✅                                    | §5.2.4         |
| Dot-repeat (.)                                 | ✅                                    | §5.2.4         |
| Insert-mode replay in dot-repeat               | ✅                                    | §5.2.4         |

### Search and substitution

| Feature                                    | Status                | Anchor                                                                                                  |
|--------------------------------------------|-----------------------|---------------------------------------------------------------------------------------------------------|
| `/` `?` `n` `N` regex search with wrap     | ✅                    | §5.9.10; backed by `fancy-regex` (DFA + bounded NFA fallback for backrefs)                              |
| `*` / `#` word-search                      | ✅                    | Word is regex-escaped before compile so literals containing `.` `*` `(` etc. don't trigger metachars   |
| Search highlight in buffer                 | ✅                    | §5.6.2                                                                                                  |
| `:s/PAT/REPL/[g]` substitute               | ✅                    | Full regex; replacement template uses `$1`/`${name}`/`$0` (regex crate / fancy-regex syntax, not vim's `\1`) |
| Pattern backrefs (`/(\w+).*\1/`)           | ✅                    | fancy-regex NFA path; bounded by 1M-iteration recursion limit                                            |
| Replacement backrefs (`:s/(a)(b)/$2$1/`)   | ✅                    | `$1` etc. via fancy-regex's `replace_all` template                                                       |
| Search-as-you-type live preview (hlsearch) | ✅                    | every match highlighted; persists after submit; compile errors silenced during preview                  |
| Substitute-as-you-type live preview        | ✅                    | ../architecture/design.md §5.9.10; magenta strike-through overlay on matches as the user types `:s/pat/repl/...`; honours `/g` flag and `%s` scope |
| Search cooperative cancellation            | ✅                    | ../architecture/design.md §5.2.5; search loops poll a `CancellationToken` per chunk + per match; flipped token returns `CoreError::Cancelled` |
| Per-search deadline timer                  | ⛔                    | the cancellation seam is in place; the deadline-flipper (Reflex < 2 ms) is the remaining piece          |

### Ex commands

Unification status (../architecture/design.md §5.2.1, §B.2): every ex-command is now a
registered `ExCommandSpec` peer of motions / operators / text objects.
The `:` parser front-end resolves aliases, looks up by name, calls the
spec's `parse_args` (or builds an `Args::List` directly for the
delimiter-syntax forms `:s/.../`, `:%s/.../`, `:g/.../`, `:v/.../`),
and dispatches through the unified `grammar::execute()`. Apply closures
emit `Effect` variants (`SaveBuffer`, `QuitEditor`, `OpenBuffer`,
`SetOption`, `Substitute`, `Global`, `Echo`, ...); `App::apply_effect`
owns the side effects.

**Kind-prefix form on `:` (§5.2.1 closure)** ✅. Every command --
motion, operator, text-object, ex-command, plugin contribution -- is
reachable from `:` by the `:<kind> <name>` form. Three kind words
(`motion`, `operator`, `text-object`) are reserved on the `:` line
and disambiguate the namespace; ex-commands keep their bare alias
surface unchanged.

```
:motion goto-first-line               # naked motion
:operator delete word-forward         # operator + bare target (motion)
:operator delete inner-word           # operator + bare target (text-object)
:operator delete motion:word-forward  # full canonical form (disambig)
:text-object inner-word               # errors helpfully
:write foo.txt                        # ex-command, vim alias surface
```

Operator targets resolve via implicit-namespace lookup: the bare tail
is tried as `motion:<tail>`, then `text-object:<tail>`, then as a full
canonical name. Plugin-registered names (e.g.
`motion:my-plugin:fancy-jump`) are reachable via
`:motion my-plugin:fancy-jump` -- the second-word colon is part of
the tail, not adjacent to the cmdline colon. See
`crates/lattice-ui-tui/src/excommand.rs::parse_kind_prefixed`.

**`:` surface invariant.** ../architecture/design.md §2.2 explicitly excludes a
function-call / palette / scripting syntax on `:`. The `:` line is
vim's ex-syntax DSL; code paths (plugins, `init.rs`, the Rust
functional API) construct `CommandInvocation` directly via the WIT
host. Two input surfaces, one dispatcher.

**Surface-form gating.** Each `ExCommandSpec` carries a
`surface_form: SurfaceForm` (`Keyword` or `Delimiter { hint }`).
Delimiter-form commands (`ex:substitute`, `ex:global`) are routed by
the parser's delimiter-detection pass; the keyword form
(`:ex:global`) returns `WrongSurfaceForm { name, hint }`, which
surfaces the intended syntax (`:g/pattern/body  (or :v/pattern/body
for inverted)`). The `gen:commands` completion generator filters
delimiter-form commands so the cmdline popup never offers a
candidate that would error when accepted. They remain reachable via
`:describe-command` / `:apropos` for introspection.

| Command                                           | Status                          | Anchor                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|---------------------------------------------------|---------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| :w / :write [path]                                | ✅ registry                     | §5.2.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| :q / :q!                                          | ✅ registry                     | §5.2.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| :wq / :x / :wq! / :x!                             | ✅ registry                     | §5.2.1 (Effect::Many)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| :e / :edit [path] / :e!                           | ✅ registry                     | §5.2.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| :d / :delete                                      | ✅ registry                     | §5.2.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| :noh / :nohl / :nohlsearch                        | ✅ registry                     | §5.2.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| :reg / :registers                                 | ✅ registry                     | §5.2.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| :marks                                            | ✅ registry                     | §5.2.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| :set option=value                                 | ✅                              | §5.12. Typed-options registry in `lattice-config` (renderer-agnostic): `OptionType` trait + `Option<T>` with `ArcSwap<T>` cell + typed `OptionHandle<T>`. Core options (number, relativenumber, wrap, ignorecase, tabstop, foldenable, foldmethod, scrolloff, completion.auto_insert_single) register from `lattice-config::register_core_options`; renderer options register from each renderer crate. App integration: `do_set` ⇒ `config.parse_and_set_command` ⇒ `apply_post_set` for cascades (relativenumber ⇒ number, foldmethod ⇒ recompute, ui.* ⇒ theme refresh).      |
| :s/.../.../[g]                                    | ✅ registry (regex)             | §5.2.1 / §B.2; `Args::List([pattern, replacement, flags])`, scope via Range::CurrentLine/Whole. fancy-regex compile + per-line `replace_all`; replacement template uses `$1`/`${name}`/`$0`.                                                                                                                                                                                                                                                                                                                                                              |
| :g/pattern/cmd                                    | ✅ registry                     | §B.2; `Args::List([pattern, false, ArgValue::Invocation(body)])`. Body parsed once at `:g` parse time (no per-match re-parse); body syntax errors surface before `:g` fires.                                                                                                                                                                                                                                                                                                                                                                                                     |
| :v/pattern/cmd                                    | ✅ registry                     | §B.2; same shape as `:g`, inverted flag set.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| :describe-command                                 | ✅ buffer (popup)               | §5.11; renders `CommandSpec.doc` + each `args_schema` entry's name/kind/doc/default.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| :describe-buffer                                  | ✅ buffer (popup)               | §5.11; path / language / modal / cursor / dirty / line-count / registers / marks / position-history / macros / folds / view options.                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| :describe-key <chord>                             | ✅ buffer (popup)               | §5.11; renders every `KeymapEntry` for the chord through the unified `Introspectable` surface. Each binding shows `Bound at: [[file:keymap.rs:LINE]]` (built-in) -- the row's source captured by the `keymap_entry!` macro -- plus an Action: `[[command:...]]` cross-reference. The `chord` arg uses `ArgKind::Chord`: typing `:describe-key <space>` puts the cmdline into chord-capture mode (raw key events render as canonical tokens -- `Ctrl-c` -> `<C-c>`, `Up` -> `<Up>`); `:describe-key<CR>` with no arg arms a one-shot prompt and the very next chord auto-submits. |
| :keymap                                           | ✅ buffer (popup)               | §5.11; lists all default bindings grouped by mode, every chord linked via `[[key:...]]` for follow-up `:describe-key`.                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| :apropos <pattern>                                | ✅ buffer (popup)               | §5.11; case-insensitive substring over every `CommandSpec.name` + `doc`. Picker UI (§5.9.7) is post-1.0.                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| :describe-option                                  | ✅                              | §5.11 / §5.12. Reads from `lattice-config::ConfigRegistry`'s `ErasedOption` view: name, aliases, type label, current value, default value, enumerated values (where applicable), doc body. Markdown-rendered into a help buffer.                                                                                                                                                                                                                                                                                                                                                |
| :describe-event, :describe-mode                   | ⛔                              | §5.11; each lands when its registry does (event bus §5.10 / modes Phase 8).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| Command-line history (Up/Down)                    | ✅                              | §B.3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| :history-*                                        | ⛔                              | §B.3 (picker UI; Up/Down already works)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| :customize                                        | ⛔                              | §5.12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| :autocmd / :add-hook                              | ⛔                              | §5.10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |

---

## Introspection architecture (../architecture/design.md §5.11)

Help is **buffer-backed from day one**, modeled after emacs's `*Help*`.
A `HelpBuffer` (in `lattice-ui-tui::help`) holds a real
`lattice_core::Buffer` (rope) plus the title, scroll offset, a real
**cursor** (`Position`), and an extracted `Vec<HelpLink>`. The popup
overlay treats the help buffer as a buffer: `j` `k` `h` `l` `0` `$`
`gg` `G` `Ctrl-D` `Ctrl-U` move the cursor and scroll auto-follows.
The terminal cursor is rendered at the screen translation of
`help.cursor` so the user sees their position. **`HelpDisplayMode` enumerates
Popup / Split / Tab / Window** for the per-user display preference;
the popup overlay is one strategy and stays available for transient
surfaces (hover, future doc lookups, error toasts).

Help also lives in the unified [`BufferRegistry`] as
`BufferData::Help(HelpBuffer)`. `App::open_help_in_pane(buffer)`
allocates a `BufferId`, inserts the registry entry, and swaps the
active pane to it -- the in-pane counterpart to `App::open_help`
(popup). The registry holds the durable record (`:ls` / `:bn` /
picker discovery); `App.help_buffer` mirrors the active in-pane
copy so the existing keymap + render paths stay single-path.
Pane-switch hooks (`snapshot_active_pane` / `load_active_pane`)
sync the two at boundaries, mirroring the Document buffer pattern
(`syntax`/`folds` snapshots). De-dup is by title -- re-running
`:lsp-log rust` surfaces the existing buffer rather than allocating
a duplicate. Persistent help views (LSP logs, `:diagnostics`,
`:apropos`, `:describe-*`) migrate to this path in Phase 3
alongside the picker (Phase 2).

### Provenance (§5.11.1)

Every registration / binding / set captures a `SourceLocation`
(`lattice-grammar::source`). `:describe-*` output always includes a
`[[file:...]]` link to where the thing came from -- vim's
`:verbose set` semantics, applied uniformly across commands, keys,
options, events, modes.

| Capture mechanism                           | Used for                                                                                    | Status                                        | Forgery resistance                                                                                        |
|---------------------------------------------|---------------------------------------------------------------------------------------------|-----------------------------------------------|-----------------------------------------------------------------------------------------------------------|
| `#[track_caller]` on `register_*`           | built-in command registrations in `builtins.rs` / `ex_commands.rs` -- zero call-site burden | ✅ implemented                                | compiler-captured, caller cannot supply or override                                                       |
| `keymap_entry!` declarative macro           | static keymap rows (per-row `file!()` + `line!()`)                                          | ✅ implemented                                | macro is the only construction path; `source` field is `pub(crate)`                                       |
| Trusted subsystem builds value              | config loader, plugin host bridge, runtime dispatcher                                       | ⛔ deferred (no trusted subsystems exist yet) | `pub(crate) insert_*` registry methods exist; sibling crates will use sealed-trait re-exports when needed |
| `SourceLocation::synthetic` (cfg-test only) | test fixtures                                                                               | ✅ implemented                                | invisible outside tests                                                                                   |

**Public API invariant**: there is no `pub fn` that takes a
`SourceLocation` parameter and stores it. Verified by:
- The `register_*` methods are all `#[track_caller]` and don't
  accept a source.
- The `pub(crate) insert_*` companions are visibility-gated to
  `lattice-grammar`.
- The `keymap_entry!` macro is the only path that constructs a
  `KeymapEntry`; its `source` field is `pub(crate)`.

**Determinism**: a unit test (`track_caller_captures_register_motion_call_site`) registers a sentinel at a known line and asserts the captured `SourceLocation` matches exactly. Six related tests cover propagation through `#[track_caller]` helpers, the negative case where unmarked helpers shift the captured line inward, and per-row line distinguishing across adjacent registrations. Any refactor that breaks call-site capture (a `dyn Fn` dispatcher around `register_*`, removed `#[track_caller]` on a helper in the chain) fails CI.

### Generic renderer (§5.11.2)

Every `:describe-*` target implements `Introspectable`
(`lattice-grammar::introspect`). One generic
`render_introspection(&dyn Introspectable) -> Vec<String>` produces
the help body in a uniform shape: identifier + kind + doc +
type-specific `extra_sections` + one `[[file:...]]` link per labelled
source (`Defined at:`, `Bound at:`, `Subscribed at:`, `Last set at:`,
`Overridden at:`, `Activated at:`). Adding a new introspection
target -- when typed options / events / modes land -- is one trait
impl plus one new ex-command; the renderer doesn't change.

### Completion pipeline (§5.11.3)

`lattice-completion` is a standalone crate with its own test corpus
(81 tests) wired into the `:`-line via the App's
`completion_registry` + `completion_state`. Four pluggable stages,
vertico-shaped:

| Stage      | Trait                | Built-ins                                                             |
|------------|----------------------|-----------------------------------------------------------------------|
| Generation | `CandidateGenerator` | `gen:commands`, `gen:files` (host-state generators in lattice-ui-tui) |
| Matching   | `CandidateMatcher`   | `match:prefix` (default), `match:substring`, `match:fuzzy`            |
| Ranking    | `CandidateRanker`    | `rank:score` (default), `rank:alphabetical`                           |
| Annotation | `CandidateAnnotator` | `anno:kind-label`, `anno:doc-snippet`                                 |

`CompletionRegistry` registers each stage with `#[track_caller]`
provenance. `CompletionPipeline::run(ctx, query, cache)` walks the
four stages and returns a `Vec<RenderedCandidate>` for the renderer.
Slot detection (`current_slot`) parses the `:`-line into
`CommandLineSlot::CommandName`, `Arg { command_name, arg_index,
arg_spec, .. }`, `DelimiterBody { command_name, body }`,
`UnknownCommand`, `BeyondSchema`, or `Empty`.

**Caching** is opt-in per generator via `cache_key()` returning
`Option<CacheKey>`. `gen:commands` returns a fixed `"gen:commands:v1"`
key (commands don't change at runtime in v1, so cached effectively
forever); `gen:files` keys per-directory with a 1-second TTL.

**Composability** is structural: every stage is a trait, plugins
register impls against the same registry as built-ins, the
default matcher / ranker / annotators are configurable per-user
(`cmdline.matcher = "match:fuzzy"` post-§5.12). The pluggable
shape mirrors emacs's `vertico` / `orderless` / `marginalia` /
`consult` -- composability by design, not retrofit.

**Forgery resistance** mirrors the §5.11.1 invariant: no public
API takes a `SourceLocation` parameter. `register_*` are all
`#[track_caller]`; `pub(crate) insert_*` companions exist for
trusted subsystems (config loader, plugin host bridge) but
deferred until first cross-crate trusted subsystem lands.

The crate doesn't depend on `lattice-ui-tui`, so it's tested
independently. CI runs `cargo test -p lattice-completion` as its
own line item.

**App integration:**

| Capability                               | Status      | Notes                                                                                                                                                                                                                     |
|------------------------------------------|-------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `<Tab>` opens completion popup           | ✅          | Slot-aware: command-name slot uses `gen:commands`; arg slot uses `arg_spec.completion`.                                                                                                                                   |
| `<Tab>` advances candidates while open   | ✅          | wraps at end                                                                                                                                                                                                              |
| `<S-Tab>` moves backward                 | ✅          | wraps at start                                                                                                                                                                                                            |
| `<CR>` accepts selected                  | ✅          | replaces `[replace_start, end)` with the candidate's `text`                                                                                                                                                               |
| `<Esc>` two-stage dismiss                | ✅          | first Esc closes popup; second cancels cmdline                                                                                                                                                                            |
| Typing dismisses popup                   | ✅          | re-trigger with Tab for fresh candidate set                                                                                                                                                                               |
| `<C-h>` describe under cursor            | ✅          | hybrid: word-at-cursor describes self if registered; else parent command at `arg:<name>` anchor                                                                                                                           |
| `<C-u>` clear cmdline                    | ✅          | also dismisses popup                                                                                                                                                                                                      |
| `<C-w>` delete trailing word             | ✅          | strips whitespace then last token                                                                                                                                                                                         |
| Vertico-style popup render               | ✅          | bordered, anchored BELOW the `:` prompt (cmdline shifts up to make room), selected row highlighted, matched byte ranges painted, annotations right-aligned                                                                |
| Alias-preferred candidate text           | ✅          | `gen:commands` returns canonical names (`ex:describe-command`); host post-process rewrites to the user-facing alias (`describe-command`) and recomputes match ranges. Parser accepts both forms via two-stage resolution. |
| Single-candidate auto-insert             | ✅          | `:set completion.auto_insert_single` (default on). When `<Tab>` would open a popup with exactly one candidate, applies it directly instead. Fires only at popup-open boundary; narrowing an open popup to one candidate while typing does NOT auto-insert (vim-style; less surprising). Phase 4.2 (#199) should reuse `App::open_completion_popup` (or factor a shared helper) when wiring Insert-mode / LSP completion so this option stays universal without a second knob. |
| Help overlay dismissed when entering `:` | ✅          | Q16: user can only focus on one thing                                                                                                                                                                                     |
| `<C-b>` / `<C-e>` cursor movement        | ⛔ deferred | needs full cmdline-cursor refactor; v1 cursor stays at end                                                                                                                                                                |
| `<C-r>` register paste                   | ⛔ deferred | needs `Pending::AfterCommandLineRegister` substate                                                                                                                                                                        |
| Completion inside `:s/.../.../` body     | ⛔ deferred | DelimiterBody slot returned; renderer doesn't recurse into pattern/replacement/flags                                                                                                                                      |

**ArgSpec.completion**: `Option<&'static str>` field naming a registered
generator. v1 wires `gen:files` for `:write`/`:edit` paths and
`gen:commands` for `:describe-command`'s name arg. Adding `gen:options`
when typed options land is one schema edit per command.

The default matcher (`match:fuzzy`) is set at registry construction;
users / configs can swap to `match:prefix` or `match:substring` once
typed options land via `cmdline.matcher = "match:prefix"`.

Link markup -- forward-compatible reference syntax in help bodies:

| Markup               | Resolution                           |
|----------------------|--------------------------------------|
| `[[command:NAME]]`   | re-dispatch `:describe-command NAME` |
| `[[key:CHORD]]`      | re-dispatch `:describe-key CHORD`    |
| `[[file:PATH:LINE]]` | open PATH at LINE                    |
| `[[anything-else]]`  | Unresolved (preserved verbatim)      |

The popup renderer is dumb today: links render verbatim. The
follow-link motion + styled link ranges + `[[file:...]]` source
navigation arrive incrementally:

| Capability                               | Status | Notes                                                             |
|------------------------------------------|--------|-------------------------------------------------------------------|
| Buffer-backed help (rope content)        | ✅     | `HelpBuffer.content: Buffer`                                      |
| Link markup defined + parsed             | ✅     | `parse_help_links` returns `Vec<HelpLink>` with byte ranges       |
| Help formatters emit links               | ✅     | `:describe-key`, `:apropos`, `:keymap` reference cross-targets    |
| Display: Popup overlay                   | ✅     | transient surface (hover, doc lookups); reachable via `App::open_help` |
| Display: In-pane (registry-tracked)      | ✅     | `BufferData::Help` + `App::open_help_in_pane` + `activate_buffer` Help arm; call-site migration to in-pane (`:lsp-log`, `:describe-*`, `:diagnostics`) follows in Phase 3 |
| Display: Split / Tab / Window            | 🟡     | in-pane lands with active pane today; multi-pane (split into a sibling pane) follows the picker / Phase 3 |
| Vertico-style picker primitive (§5.9.7)  | ✅     | `lattice-ui-tui::picker::Picker` -- query line + substring filter + selection cursor + `PickerAction` accept tag. **Renderer-agnostic data model**: the only imports are stdlib + `lattice_completion`; host-coupled candidate builders live on the host side (the buffer-source builder is `app::raw_buffer_candidates`; the LSP-source builder is `LspInstanceRow::into_candidate` fed by host-snapshotted rows). `Picker::set_raw_candidates` is the single mutation entry point. Drops into a sibling crate `lattice-picker` with no source edits when a second renderer needs it. Sources: `Buffers`, `LspInstances { prefilter }`. First instantiations: `:b` buffer switcher; `:lsp-log` / `:lsp-server-log` / `:lsp-trace-log` LSP picker (Phase 3). Layout: vertico-style (query takes over cmdline row; candidates render in band immediately below, selected row closest to prompt, no border). Live preview-in-pane for `SwitchToBuffer` (selection-change activates the buffer in active pane without pushing position history; `<Esc>` restores `preview_origin`). Pipeline-driven matcher / ranker / annotators graduate the picker in a follow-up. |
| Cmdline tab-completion: vertico-styled   | ✅     | `:` line tab-completion popup converged with the picker's visual shape. No bordered box / title; candidates render flush in the row band below the cmdline, reusing `candidate_to_line`. The cmdline appends a faint `(n/m)` count suffix when the popup is open, mirroring the picker prompt's inline indicator. |
| LSP picker -- log / trace dispatch       | ✅     | `:lsp-log [server]`, `:lsp-trace-log [server]`, `:lsp-server-log` route through `App::open_lsp_picker` (Phase 3). Single-match short-circuit opens directly; multi-match opens the picker pre-filtered. Opened buffer goes through `open_help_in_pane`. `:lsp-trace` is now a pure toggle (no buffer-open side-effect). |
| LSP log buffers live-tail (Phase 4)      | ✅     | `Event::LspLogPushed` fires on every `LspLogger::log` append; `App::drain_lsp_log_events` (called per main-loop tick) refreshes any open `*lsp*` / `*lsp:<server>*` / `*lsp:<server>:trace*` help buffer from a fresh logger snapshot. Coalesces by scope so a burst of N records resolves to one rebuild per affected scope. Cursor + scroll preserved across the rebuild (clamped to new line bounds); popup hot-path mirror synced. |
| LSP server-name resolution                | ✅     | `:lsp-trace`, `:lsp-log`, `:lsp-trace-log` accept either the canonical actor id (`rust`) or the binary name the user recognises (`rust-analyzer`); resolution priority: running actors -> registered config ids -> config binary file-name (with `.exe` stripped). Unknown names echo the running id list so the user can disambiguate. |
| Pane-aware viewport height                | ✅     | `App::active_pane_content_height(buffer_height)` returns the active pane's content rect (subtracting the per-pane status row in multi-pane mode). Runtime feeds it into `set_viewport_height` so motions / scroll / fold-aware ensure-cursor-visible agree with what the renderer paints. Without this, horizontal/vertical splits clipped the lower / right half of the active pane. |
| `K` / `gd` registered in keymap registry  | ✅     | The input translator already dispatched `K` -> `LspHoverRequest` and `gd` -> `LspDefinitionRequest`, but the keymap registry didn't list them, so `:describe-key K` / `:apropos hover` came back empty. Added entries in `keymap::build_default_keymap`. |
| Styled link ranges in renderer           | ⛔     | renderer ignores `links` today                                    |
| Follow-link motion (e.g. `<CR>` on link) | ⛔     | needs tree-sitter help grammar + link motion                      |
| Help major mode + tree-sitter grammar    | ⛔     | post-Phase-3-extension; sections / code-blocks / link-targets     |
| `SourceLocation` on `CommandSpec`        | ⛔     | needs `register_*` API extension; powers `[[file:...]]` auto-emit |
| `:source-of <command>`                   | ⛔     | depends on `SourceLocation`                                       |
| `:describe-key`                          | ✅     | keymap registry §5.2.3 -- see below                               |
| `:keymap`                                | ✅     | full default keymap, grouped by mode                              |
| `:describe-option`                       | ⛔     | needs typed options registry §5.12                                |
| `:describe-event`                        | ⛔     | needs event bus §5.10                                             |
| `:describe-mode`                         | ⛔     | needs major/minor modes (Phase 8)                                 |

### Keymap registry (../architecture/design.md §5.2.3)

`KeymapEntry { chord, mode, doc, command }` in `lattice-ui-tui::keymap`,
populated as a `&'static [KeymapEntry] DEFAULT_KEYMAP` covering every
chord in `input.rs`. v1 is a *descriptor table* the introspection layer
queries; the input layer (`input.rs::translate`) still owns the
chord-to-Action translation. A drift test
(`keymap_descriptors_dont_drift_from_translate`) walks every descriptor
through `translate()` and asserts a non-`None` Action -- catches
removed/moved bindings before they ship.

Registry-driven dispatch (the input layer *consuming* the keymap
registry rather than running a parallel `match`) is post-1.0 -- it
needs the layered keymap walker (built-in / major-mode / minor-modes /
user / per-buffer) from §5.2.3. The descriptor table is the migration
seed.

---

## Async / actor architecture (../architecture/design.md §5.2.1, §5.6.8, §5.7)

The async core lands in `lattice-runtime`. Every document is owned by
its own tokio task (the **document actor**); mutations route through a
bounded mpsc mailbox; commits publish an immutable `DocumentSnapshot`
to an `arc_swap::ArcSwap` cell that any reader (renderer, future LSP
client, future plugin) loads wait-free.

`lattice_grammar::execute` stays a pure sync function (no tokio
dependency, no async signature). The actor calls it *inside* its own
task via the `Dispatch` message; only the *boundary* between caller
and document is async.

App talks to the actor through `DocumentHandle` (cheap Clone -- an
`mpsc::Sender` + `Arc<PublishedSnapshot>`). The TUI input loop, which
is a blocking crossterm poll, bridges sync→async via
`lattice_runtime::block_on`. Every mutating method returns
`Pending<T>` (a typed wrapper around `oneshot::Receiver`); the App's
`*_blocking` helpers concentrate the bridge in one place.

**Publish-before-reply.** The actor's run loop publishes the new
snapshot *before* sending the `oneshot` reply for any mutation. This
guarantees that any caller observing the reply also observes the new
snapshot via `arc_swap::load` -- without this ordering, callers can
race past their own commit.

| Concern                                                    | Status               | Anchor                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
|------------------------------------------------------------|----------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Document actor / bounded mpsc mailbox                      | ✅                   | §5.7 (`lattice-runtime::DocumentActor`)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| `Pending<T>` returned by every mutating call               | ✅                   | §5.2.1 (`lattice-runtime::Pending`)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Bounded backpressure (`RuntimeError::Busy`)                | ✅                   | §5.2.1 (mailbox cap = 64)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| `arc-swap` published `DocumentSnapshot`                    | ✅                   | §5.6.8 (`PublishedSnapshot`)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Renderer reads via single snapshot load per frame          | ✅                   | §5.6.8 (`render::draw_*`)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| Publish-before-reply ordering                              | ✅                   | §5.6.8 (acquire/release contract)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Sync `lattice_grammar::execute` (runs inside actor)        | ✅                   | §5.2.1 (purity preserved)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| Latency-class declarations (Reflex / Display / Background) | ✅ declarative       | §5.2.5 (`LatencyClass` field on `CommandSpec`; runtime enforcement deferred)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Cancellation token contract                                | ✅ user-Esc          | §5.2.5; `CancellationToken` (Arc<AtomicBool>) plumbed through `dispatch_with_cancel` → grammar dispatcher → operator/motion/text-object contexts → search loops. Deadline-timer flipper (Reflex < 2 ms, Display < 10 ms) is the remaining piece.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| Event bus (observation baseline)                           | ✅                   | §5.10; `EventBus` in lattice-runtime: kind-indexed dispatch, `SubscriptionTarget::Channel` (mpsc) + `Invocation` (queued via `drain_pending_invocations`).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| App-side event publish                                     | ✅                   | §5.10; App publishes `DocumentChanged` (apply_edit / batch / undo / redo), `SelectionsChanged` (set_selections), `ModalModeChanged` (only on actual axis movement), `BeforeSave` + `DocumentSaved` (sync wrapper around save / save_as), `BeforeQuit` (Action::Quit + `:q` after dirty-check), `OptionChanged` (every typed-options registry write — including `:set foo=bar`, `:set nofoo`, and direct `config.set` paths; carries canonical name + old + new formatted strings).                                                                                                                                                                                                                                                                          |
| Config → event bus bridge                                  | ✅                   | §5.10 + §5.12; `lattice-config::ConfigRegistry` exposes `set_event_publisher(EventPublisher)`. App wires the bus at boot via a closure that calls `event_bus.publish(event)`. Subscribers see option changes through `Event::OptionChanged` instead of polling.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| App-side cascade via bus subscription                      | ✅                   | §5.10 + §5.12. App subscribes a `tokio::sync::mpsc::UnboundedReceiver<Event>` filtered to `EventKind::OptionChanged` at boot; `App::drain_option_changes` consumes it and runs the per-option cascade (`relativenumber⇒number`, `foldmethod⇒recompute_folds`, `ui.*⇒sync_theme_from_config`). Drained at the end of `do_set` (synchronous user-visible behaviour preserved) and at the top of every main_loop iteration (backstop for writes outside the keystroke path -- plugin tasks, customize buffer, init.rs). The chained cascade case (`relativenumber⇒number` itself fires another `OptionChanged`) is handled by the drain's `while let Ok` loop. No registry-mutex re-entrancy risk: publisher closure runs after the registry drops every lock. |
| Veto-class hooks (1ms p99)                                 | ⛔                   | §5.2.1 (needs Before-event return-path so handlers can mutate / abort; v1 publish is observation-only)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Events-over-invocation rule                                | ⛔                   | §5.2.5 (needs `:autocmd` and `add-hook` parser front-ends to desugar into `subscribe`)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Interactive arg-prompts (§B.1 phase 2)                     | ✅                   | Submitting bare `:cmd<CR>` with a Required first arg arms a prompt: prefills `:cmd `, surfaces the schema's prompt in the echo area, and waits for typed input (Chord-kind args additionally auto-submit on the next captured chord). Optional-default args take the parser's normal path.                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Multi-pane selection transformation                        | n/a (single-pane v1) | §5.6.8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |

This is **Phase 4 / 7's prerequisite** — LSP clients and the WASM
plugin host can now share `DocumentHandle` with the App; both
register against the same actor, both observe the same snapshot
stream. The remaining ⛔ rows (cancellation, latency classes, hook
classification, event bus) layer on top of the actor without
restructuring it.

---

## Performance posture

| Concern                                     | Status | Anchor                                          |
|---------------------------------------------|--------|-------------------------------------------------|
| Criterion bench harness                     | ✅     | §8.2                                            |
| Render hot-path is viewport-bounded         | ✅     | §8.2 (this commit)                              |
| Actor / runtime benches                     | ✅     | §5.6.8 / §8.2                                   |
| `LatencyClass` declaration on `CommandSpec` | ✅     | §5.2.5                                          |
| Test + clippy CI gate                       | ✅     | (.github/workflows/ci.yml)                      |
| Bench-compile CI gate                       | ✅     | (catches bench rot)                             |
| Bench baseline recording (push to main)     | ✅     | (artifact upload, no diff yet)                  |
| Bench regression detection (>10% threshold) | ⛔     | §8.2 -- needs stable runner                     |
| Per-class budget assertions in CI           | ⛔     | §5.2.5 -- needs cancellation/deadline machinery |
| Allocation discipline check in CI           | ⛔     | §A.6                                            |
| Long-running session benches                | ⛔     | §A.6                                            |
| Cross-platform acceptance suite             | ⛔     | §A.6                                            |

**Render hot path.** `compose_visible_lines` previously did
`buffer.as_string().split('\n').collect::<Vec<String>>()` once per
frame -- O(buffer size) bytes per paint, blowing the §8.2 <2ms frame
budget on any non-trivial buffer. Now uses ropey's O(log n) per-line
API via `Buffer::line(idx)` and materializes only the visible
window (`height` lines, typically 50). 100MB log files now pay the
same per-frame cost as 100-line files.

**Actor benches** (`crates/lattice-runtime/benches/actor.rs`)
characterize the load-bearing async primitives:

| Benchmark                                      | DESIGN target (p99) | Observed (median)                       |
|------------------------------------------------|---------------------|-----------------------------------------|
| `apply_edit` round-trip                        | <100µs              | ~80µs (constant across 10/1k/50k lines) |
| `snapshot_load` (`load_full`)                  | <20ns               | ~17ns (Arc bump path)                   |
| `snapshot_load_cached` (`Cache::load`, steady) | <500ps              | ~305ps                                  |
| `snapshot_post_publish_read`                   | --                  | ~17ns                                   |

The `apply_edit` round-trip meets §8.2 with 20% margin. The renderer
now reads through `arc_swap::Cache::load` per frame (~50× faster than
`load_full`) -- `App` holds a `SnapshotCache` rebuilt on each
document switch, the runtime calls `app.snapshot_cache.load_arc()`
once per frame in `runtime.rs`, and the resulting
`&DocumentSnapshot` is threaded through the active-pane render path
(`compose_visible_lines`, `cursor_screen_position`,
`closed_fold_display_span`, `buffer_line_to_visible_row`,
`draw_mode_line`). Inactive panes render different documents and
still go through `entry.handle.snapshot()` (`load_full`); a per-doc
cache map for inactive panes is queued behind a profiling motivator.
The 50+ `app.document.snapshot()` call sites in keystroke handlers
remain on `load_full` -- each runs ≤1× per keystroke, dominated by
other work, so the migration there is deferred.

**`LatencyClass` declaration** (../architecture/design.md §5.2.5) is now a field on
every `CommandSpec`. `:describe-command` surfaces it under a
"Latency:" section. v1 is purely declarative; the cancellation /
deadline machinery that enforces it lands with the §5.10 event-bus.
Default classifications:

- **Reflex** (<2ms p99): every motion, operator, text-object; cheap
  ex-commands (`:quit`, `:noh`, `:set`, `:delete`, `:s`, `:g`,
  `:v`).
- **Display** (<10ms p99 sync prelude): file I/O (`:write`,
  `:write-quit`, `:edit`); help-buffer builders (`:reg`, `:marks`,
  `:describe-*`, `:apropos`, `:keymap`).
- **Background**: none yet -- the indexer / file-watcher / LSP
  debounce paths arrive in Phases 4-7.

**CI** (`.github/workflows/ci.yml`) gates every push/PR on:

- `cargo test --workspace --locked` + `cargo clippy --workspace
  --tests --locked -- -D warnings` across a cross-platform matrix
  (ubuntu-latest, macos-latest, windows-latest; `fail-fast: false`).
- `cargo fmt --all -- --check` -- rejects unformatted code.
- `cargo doc --no-deps --workspace` with `RUSTDOCFLAGS=-D warnings`
  -- catches broken intra-doc links before merge.
- `cargo bench --workspace --no-run` per platform -- bench-compile
  rot detection.
- `bench-baseline` on push-to-main runs the benches in `--quick`
  mode and uploads the criterion reports as artifacts. Groundwork
  for the regression gate when stable bench infrastructure
  (self-hosted runner or `bencher.dev`) lands.

Current bench coverage: motions (word_forward / backward / end /
first_non_blank / counted), operators (dw / dd / d_whole / yw / cw / diw /
di_paren), search (forward first / last / no-match-with-wrap / backward),
buffer (insert at origin / middle, delete one byte, position round-trip),
runtime actor (apply_edit round-trip / snapshot_load /
snapshot_post_publish_read at 10/1k/50k lines).

---

## Terminal-mode (issue #40, 2026-05-22 → 2026-05-25)

Plan + tracker live in
[`terminal-mode-plan.md`](terminal-mode-plan.md); design in
[`../architecture/terminal-mode.md`](../architecture/terminal-mode.md).
Capsule summary so this ledger stands alone:

- ✅ **T1** — PTY spawn (`portable-pty`); homegrown cell-grid
  reader; new `BufferKind::Terminal` integrated everywhere
  buffers flow (`:terminal` / `:tnew`, picker `<C-s>` / `<C-v>` / `<C-t>`,
  pane-render dispatch, modeline / tabline / `:ls`); `:bd!`
  kills the child.
- ✅ **T2 (substrate)** — homegrown reader retired; alacritty's
  `Term` + `vte::Parser` is the engine. SGR colours (named /
  256 / truecolor), alt-screen, cursor visibility, DECCKM all
  honoured. Cells published with proper `TerminalColor` /
  `CellAttrs`; both renderers (TUI / GPUI) paint per-cell
  fg/bg with adjacent-run coalescing.
- ✅ **T2.a / T2.b / T2.b.0 / T2.c** — `TerminalInsertMode`
  minor mode; full ANSI encoder (printable + Enter/Tab/Backspace
  + arrows + Home/End/PgUp/PgDn + Insert/Delete + F1–F12 +
  Alt-prefix + xterm `1;<n>` modifier parameter);
  `i`/`a`/`I`/`A` entry; `<Esc>` exit gated by
  `terminal.esc-exits` (default `true`); modeline
  `TERMINAL-INSERT`; `<C-c>` exemption sends SIGINT. T2.c
  (2026-05-25): `<C-w>` window-prefix in Normal-in-terminal
  works via the standard keymap (MotionAdapter cleanup);
  `<C-w>` WERASE inside Insert via the encoder's
  `ctrl_char_byte('w')`; `<C-\><C-n>` two-key chord (`<C-\>`
  arms, `<C-n>` confirms; anything else sends `\x1c` + that
  key's PTY bytes); DECCKM bit propagates via
  `SharedTerm::cursor_keys_application_mode` →
  `key_to_ansi_with_mode(_, true)` so arrow keys flip to SS3
  when programs enable application-cursor-keys.
- ✅ **T3 (scrollback + nav + copy)** —
  `terminal.scrollback-lines` typed option (default 10k);
  alacritty history ring sized through it; `SharedTerm` handle
  shared between reader + dispatch; snapshot publishes
  `scroll_offset` / `scrollback_rows`. **MotionAdapter cleanup**
  retired every kind-specific branch from the translate layer:
  standard Normal-mode keymap flows through
  `Editor::run_terminal_invocation` (a peer of
  `run_help_invocation` / `run_oil_invocation`) which routes
  `line_down` / `line_up` / `goto_first_line` /
  `goto_last_line` / `page_down` / `page_up` /
  `scroll_line_down` / `scroll_line_up` to
  `SharedTerm::scroll`. Search: `/pattern<CR>` / `?pattern<CR>`
  / `n` / `N` use `SharedTerm::find_match` (forward / backward)
  and `find_all_matches` for `hlsearch`. Current match paints
  reverse-video; all matches in the visible window paint with
  underline. Visual: `v` / `V` / `<C-v>` enter charwise /
  linewise / blockwise on the cell grid; `h`/`j`/`k`/`l`/`gg`/`G`
  extend; `y` yanks with matching `YankKind` so cross-buffer
  paste handles correctly; `<Esc>` cancels; `gv` reselects.
  Modeline `TERMINAL-VISUAL`. Snap-to-live-edge on Insert
  entry. Jump-list (`<C-o>` / `<C-i>`) restores the recorded
  scroll position when landing back on a Terminal entry
  (`PositionEntry::terminal_scroll_offset`).
- ✅ **T4 polish (4/5)** (2026-05-25) — Resize wired in
  `editor_actor::SetPaneViewport`: on pane geometry change,
  `SharedTerm::resize` updates the alacritty grid + republishes
  the snapshot, and `PtyHandle::resize` triggers SIGWINCH so
  the child re-lays-out. `<C-w>T` (move-pane-to-new-tab)
  landed: new `Action::MovePaneToNewTab` +
  `AppEffect::MovePaneToNewTab` + `do_move_pane_to_new_tab`
  handler (closes pane in current tab when >1 leaves, then
  opens a fresh tab holding the same `BufferId` at the same
  cursor / scroll; no-op echo when the pane is the only one
  in its tab). `:tabterminal [cmd]` (alias `:tabterm`)
  registered as `ex:tabterminal` →
  `AppEffect::TerminalSpawnInNewTab` → `do_new_tab` +
  `TerminalSpawn(cmd)`. `:bd!` already kills the child via
  the existing `TerminalBuffer::Drop` impl (abort reader →
  close PTY master → SIGHUP).
- Queued: T4.2 mouse passthrough (own slice — needs
  mouse-encoder + per-renderer event capture + the
  `terminal.mouse-passthrough` option); T5 plugin surface
  (Phase 7+); word-motions (`w`/`b`/`e`) in Terminal Visual;
  dedicated theme slots (`ui.match-background`,
  `ui.visual-background`) so renderers can distinguish
  match / selection backgrounds.

## In-progress

**Phase 4.2 navigation -- 9/12 shipped + 1 partial.**
**Phase 4.3 -- 3/9 shipped.**

- ✅ hover (`K`), definition (`gd`), declaration (`gD`),
  typeDefinition (`gy`), implementation (`gI`), references
  (`gr`), documentSymbol (`:lsp-symbols`), workspaceSymbol
  (`:lsp-workspace-symbol`).
- 🚧 completion (`:complete` picker bridge -- buffer-level
  Insert-mode completion shell + snippet expansion + lazy
  resolve queued behind it).
- ✅ formatting + rangeFormatting (`:format` / `:format-range`).
- ✅ signatureHelp via `:signature-help` + Insert-mode
  trigger-char autopilot (typing `(` / `,` etc. fires the
  request automatically).
- ✅ rename + prepareRename via `:rename <name>` (alias
  `:rn`). Active buffer applies as one undo unit; cross-file
  edits open via `:e` then apply per-file. WorkspaceEdit
  flattening covers both legacy `changes` map and modern
  `document_changes` shape.
- ✅ willSave / didSave notifications fan out from
  `App::save_blocking`. Each attached server advertising
  the matching capability gets a fire-and-forget
  notification; didSave attaches the post-save rope text
  when `includeText` is set. willSaveWaitUntil typed
  wrapper exists; the App-side block-on-response
  (format-on-save) is queued.
- ✅ `:code-actions` (`:ca`) -- vertico picker over LSP
  code actions; resolves lazy `edit` via
  `codeAction/resolve` when the action arrived without
  inline edit + command; routes Command payloads through
  `workspace/executeCommand`. Edits land via the rename
  apply path (per-file one-undo-unit).
- ✅ onTypeFormatting Insert-mode autopilot. Typing a
  server-advertised trigger character fires
  `textDocument/onTypeFormatting`; edits apply via the
  format channel.
- ✅ willSaveWaitUntil format-on-save: `App::save_blocking`
  blocks for up to 500ms per server collecting pre-save
  edits, applies them as one undo unit, then writes to
  disk. Buggy / slow servers can't hang the save (token
  + timeout).
- Multi-result lookups + `:diagnostics` route through one
  vertico picker (`PickerSource::LspLocations` +
  `PickerAction::JumpToLspLocation`); single-result nav still
  jumps directly per vim convention.
- The four nav flavours share `do_lsp_nav_request(LspNavKind)`
  -- one dispatch path; the kind selects the LSP method and
  drives the kind-aware echo verb ("no implementations found"
  vs. "no definitions found").
- ✅ Tag stack: `gd` family + multi-result picker accept push
  onto `App.tag_stack`; `<C-t>` pops. Distinct from the jump
  list (`<C-o>`/`<C-i>`); the two have different push semantics
  and may have different lengths.
- ✅ Jump list propagation audited: every LSP nav, picker
  accept, search submit, `n`/`N`, help-link follow records
  `(BufferKind, BufferId, Position, source)` so cross-buffer
  walks just work. The previous `active_buffer == Document`
  gates on search submit + repeat search are gone.

Remaining 4.2:
- **Buffer-level Insert-mode completion** -- design spec at
  [`../architecture/insert-completion.md`](../architecture/insert-completion.md). 4.2.g.1
  (shell + buffer-words + popup widget + minor-mode keymap)
  ✅; 4.2.g.2 (LSP source + isIncomplete refresh + typed
  routing payload via `CandidateData::Extension` /
  `App.insert_completion_lsp_meta` sidecar) ✅; 4.2.g.3
  (docs side popup + lazy `completionItem/resolve` +
  `<C-f>` / `<C-b>` paging) ✅; 4.2.g.4 (`lattice-snippet`
  crate -- TextMate JSON parser + render walker + variable
  context + active-snippet state machine + friendly-snippets
  compat; host integration -- `gen:snippet` source, accept
  routing for snippet candidates AND LSP `insertTextFormat ==
  Snippet` items, `<C-x><C-s>` direct expand,
  active-snippet minor mode for `<Tab>` / `<S-Tab>` / `<Esc>`,
  `:snippet-expand` / `:reload-snippets` ex-commands) ✅;
  4.2.g.5 sliced 3 ways for landing -- (1/3) frequency
  ranking (App-side `(text, kind) -> u32` accept map bumped
  in `do_completion_accept`; ranker threads a host-supplied
  lookup closure through, capping the bonus at +50; all
  three refilter sites swapped over) ✅; (2/3) per-source
  priority (RawCandidate gains a `source: Option<SourceId>`
  field; `BufferWordsSource::produce` self-tags, host tags
  the snippet + LSP candidates with constants
  `SNIPPET_SOURCE_ID` / `LSP_COMPLETION_SOURCE_ID`; ranker
  surface renamed `rank_with_frequency` -> `rank_with_bonus`
  taking a single host-composed closure; three new typed
  options `completion.source.{lsp,snippet,buffer-words}.priority`
  with defaults 200 / 150 / 100 per spec §3.4; App's
  `priority_for_source` reads them and the closure adds
  `priority + freq.min(50)` for every candidate;
  unknown-source candidates get 0 priority gracefully) ✅.
  (3a/3) bare TOML loader infrastructure
  (`lattice-config::loader`: walks user config
  `~/.config/lattice/lattice.toml` and project config
  `<root>/.lattice/config.toml`, applies scalar leaves via
  `parse_and_set_command`, buckets structural namespaces
  (`completion.per-language.*`, `plugin.*`) keyed by
  full dotted path; warnings for unknown keys / validation
  rejects / list-at-scalar / read failures, never aborts
  startup; `App.pending_config_structural_sections` +
  `take_pending_structural_section` /
  `pending_structural_section_paths` API for the per-
  language layer + future plugin host to drain;
  `runtime::run` invokes loader between `App::new` and LSP
  boot with workspace root walked up from CWD to first
  `.git` / `.lattice/` marker) ✅;
  (3b/3) per-language overrides
  (`PerLanguageOverrides { sources, auto_trigger,
  auto_insert_single, suppress_in }` in
  `lattice-completion::insert`; spec defaults seeded at
  `App::new` via `per_language_defaults()` --
  markdown / text drop LSP for prose, rust enables auto-fire
  + auto-insert-single; TOML drains
  `[completion.per-language.<lang>]` structural sections via
  `apply_per_language_toml_overrides` with per-key merge onto
  defaults; `effective_completion_for(language)` walks
  per-language -> global option -> spec fallback;
  enforcement at `populate_insert_completion_sync` (skip
  emit for disabled sync sources) and
  `do_lsp_insert_completion_request` (short-circuit before
  the URI lookup); `auto_trigger` and `suppress_in` plumbed
  but not yet enforced -- auto-fire as a feature and
  tree-sitter scope detection are their own slices;
  `canonical_source_id` maps short labels (`lsp`, `snippet`,
  `buffer-words`, `path`, `tree-sitter`) to canonical ids;
  help refresh in `docs/../../user/completion.md` lists the
  built-in defaults table + recognised keys + merge
  semantics) ✅. **4.2.g.5 complete.** 4.2.g.6 (1/2)
  (tree-sitter local-symbol completion source --
  per-language `symbols.scm` queries in
  `crates/lattice-syntax/queries/{rust,python,javascript}/`
  capture definition-position identifiers; `LangRegistry`
  compiles them via `build_config(symbols, ...)` extension;
  `Syntax::collect_symbols()` walks the cached tree and
  returns deduped names; new
  `TREE_SITTER_SYMBOL_SOURCE_ID = "gen:tree-sitter-symbol"`
  constant; new typed option
  `completion.source.tree-sitter.priority` (default 80 per
  spec §3.4); App's `populate_insert_completion_sync`
  emits tagged candidates after buffer-words / snippets;
  `priority_for_source` resolves the new id; both sources
  emit independently when they overlap (cross-source visual
  dedup deferred to 4.2.g.7); help adds a dedicated
  `## Tree-sitter symbols` section with per-language
  capture coverage + ranking-vs-buffer-words explainer)
  ✅; 4.2.g.6 (2/2) (path completion source --
  `Syntax::cursor_in_string_scope` walks tree-sitter
  ancestors against a hardcoded string-shape set
  (`string` / `string_literal` / `raw_string_literal` /
  etc.); `App.completion_in_path_context` flag set by
  `do_completion_trigger` when the cursor is in a string
  scope and `gen:path` is enabled; path-aware anchor walks
  back over alphanumeric + `_-./~+@` until `/` (the
  dir/file boundary); `populate_path_completion` resolves
  the partial path against the document's parent dir or
  CWD, walks via `std::fs::read_dir` capped at 200 entries,
  skips dotfiles + `.git` / `node_modules` / `target` /
  `dist`, emits `File` / `Directory`-kind candidates with
  trailing `/` for directories; LSP fan-out + non-path
  sync sources short-circuit in path-completion mode so
  the popup shows filesystem entries cleanly; new constant
  `PATH_SOURCE_ID = "gen:path"`; new typed option
  `completion.source.path.priority` (default 90 per spec
  §3.4); `priority_for_source` wires the new id; help
  refresh adds a dedicated `## Path completion` section
  covering scope detection, resolution, ignore set, and
  popup behaviour) ✅. **4.2.g.6 complete.** 4.2.g.7 polish
  (sliced as independent items): commit chars
  (`Action::CompletionAcceptThenInsert(char)` routes every
  popup-time char through one handler;
  `effective_commit_chars_for` unions the focused candidate's
  per-item LSP `commitCharacters` with the new typed option
  `completion.extra_commit_chars`; popup layer claims
  unmodified character keys; non-commit chars fall through
  to plain `do_insert_text` so the popup refilters as
  before) ✅; additionalTextEdits coalesce for the LSP
  snippet accept path (new
  `expand_snippet_with_lsp_edits` builds one batch with the
  auto-import edits + the snippet body's main splice,
  reverse-sorts by start position so each edit's original-
  document positions stay valid, applies via
  `apply_edit_batch_blocking` so the whole accept lands as
  ONE undo unit; recovers the snippet's post-batch origin
  by indexing the applied vec at the main edit's
  position-after-sort; non-snippet LSP path was already
  coalesced via `apply_lsp_completion_accept`'s combined
  Vec<TextEdit>) ✅; ghost text for the top-ranked
  candidate (new typed option `completion.ghost_text`,
  default off; new App helper
  `completion_ghost_text_suffix()` returns the
  case-insensitive-prefix suffix when the popup is open
  with a non-empty query, the top candidate matches as
  prefix, and we're not in path-context;
  `compose_visible_lines` appends a dimmed-italic span on
  the cursor's row when the cursor is at end-of-line and
  the helper returns Some) ✅; cross-source visual dedup
  (new `dedup_rendered_by_text` helper retains the first
  occurrence per `raw.text` after the ranker has sorted
  descending, wired at all three refilter sites; the
  surviving row is the highest-ranked one per text, so
  the buffer-words copy of `outer` outranks the
  tree-sitter copy at the spec's 100/80 priority split
  and wins the popup row; selection / navigation / accept
  index the deduped vec naturally) ✅; picker-call-site
  typed routing payload (new `RoutingPayload` enum +
  `PICKER_ROUTING_KIND_ID` const + `Picker.routing_meta`
  sidecar Vec; new `set_raw_candidates_with_routing(items:
  Vec<(RawCandidate, RoutingPayload)>)` zips producer
  pairs into the picker, stamping each candidate with
  `Extension { kind_id, payload: index_le_bytes }`;
  `routing_for(candidate)` decodes the index and returns
  the typed `&RoutingPayload`; producers
  `LspInstanceRow::into_candidate_with_routing`,
  `LspLocationRow::into_candidate_with_routing`,
  App's `raw_buffer_candidates`, the LSP-completion picker
  open, and the code-action picker open all return pairs;
  `do_picker_accept` matches on the typed `RoutingPayload`
  variant; the string parsers `buffer_id_from_text`,
  `lsp_key_from_text`, `jump_target_from_text` are gone;
  `RawCandidate.text` is now user-facing label everywhere;
  no UX change -- pure technical-debt cleanup) ✅.
  **4.2.g.7 complete; 4.2.g done.**
- `completionItem/resolve` (lazy doc / additional edits) --
  shipped as part of 4.2.g.3 + 4.2.g.7.
- `workspaceSymbol/resolve` (lazy location) ✅. Client
  capability now advertises
  `workspace.symbol.resolveSupport.properties = ["location.range"]`;
  `Capabilities::workspace_symbol_resolve_provider` reads
  the server's matching flag from
  `workspaceSymbolProvider.resolveProvider`. New
  `ServerHandle::workspace_symbol_resolve(symbol, token)`
  client method routes the LSP `workspaceSymbol/resolve`
  request. The `workspace_symbol` response type upgrades
  from `Option<Vec<SymbolInformation>>` (legacy-only) to
  `Option<lsp_types::WorkspaceSymbolResponse>` -- the
  spec's `Flat | Nested` union covering both LSP 3.16 and
  3.17+ shapes. App's `do_lsp_workspace_symbol_request`
  handles both: `Flat(Vec<SymbolInformation>)` flows
  through `symbol_information_to_row` (range inline);
  `Nested(Vec<WorkspaceSymbol>)` flows through the new
  `workspace_symbol_to_row(handle, sym, token)` async
  helper which fires `workspaceSymbol/resolve` against
  the originating server when the location came back as
  `WorkspaceLocation` (URI only) and uses the resolved
  range. Eager-resolve at fan-out keeps picker rows
  uniform (every row is `(path, line, col)` resolved);
  the picker's accept path stays unchanged. Servers that
  don't advertise `resolveProvider` fall back to
  `(path, 0, 0)` so the user can still navigate to the
  file. Tests: legacy `Flat` shape round-trips; modern
  `Nested` shape with `WorkspaceLocation` decodes
  correctly; `workspaceSymbol/resolve` round-trip
  upgrades the location.

Remaining 4.3:
- workspace/applyEdit (server-initiated -- inbound channel
  on the actor that calls back into the App's
  `apply_workspace_edit` path) ✅. New
  `lattice-lsp::apply_edit` module exposes
  `ApplyEditBus` (mpsc Sender side cloned into every actor
  at spawn) + `InboundApplyEdit` (server_id + label + edit
  + oneshot for the response) + `ApplyEditOutcome` (the
  reply the App writes back). Actor's request branch routes
  `workspace/applyEdit` through a spawned task that
  dispatches via the bus, awaits the App's oneshot, and
  ferries the LSP `ApplyWorkspaceEditResponse` back to the
  wire; other server-initiated requests still resolve
  inline. Supervisor exposes
  `set_apply_edit_bus` for the App; `LspSupervisor::new`
  now starts with `apply_edit_bus = None` so existing
  tests / mocks that don't care about applyEdit see the
  pre-4.3 METHOD_NOT_FOUND fallback. App's
  `build_lsp_subsystem` creates the bus + receiver pair
  and stashes the receiver in
  `App.pending_apply_edit_rx`; `runtime::main_loop`
  invokes `App::drain_inbound_apply_edits` once per
  iteration alongside the other LSP drains. The drain
  flattens each WorkspaceEdit via the existing
  `flatten_workspace_edit` (same path `:rename` uses),
  applies per-file (active buffer direct, cross-file via
  `:e` then apply), echoes a status summary at Info
  (success) / Warn (partial), and replies via the
  embedded oneshot with `applied: bool` +
  `failure_reason`. Empty edits reply
  `applied: true` with the "empty workspace edit" reason
  so server logs see the no-op. v1 doesn't track
  `failed_change` (atomic-rollback queued for the
  follow-up `apply_workspace_edit_atomic`). Tests:
  lattice-lsp dispatch round-trip + drop-side error +
  oneshot round-trip; App drain applies edits to active
  buffer, replies applied=true on empty edit, and is a
  no-op when the channel is empty.

**4.x edit-path refactor: per-actor DocSync + bus-driven
fan-in.** Diagnostics testing surfaced that edits were being
silently dropped when the App's `try_lock`-on-supervisor
edit path raced the App-spawned debounce task. Architectural
fix (chosen against design goals, not ease of impl): move
`DocSync` into the per-server actor (single-writer mirror;
no shared mutex), enrich `Event::DocumentChanged` with
`path: Option<PathBuf>` + `inserted_text` per `AppliedEdit`,
spawn a per-actor `lattice_lsp::fan_in` task that subscribes
to the bus and forwards each applied edit as
`ActorCmd::RecordEdit` straight into the actor's mailbox.
The UI thread now does one `EventBus::publish` per applied
edit (1.9 µs at three subscribers) and never takes the
supervisor mutex; the actor coalesces on a 50 ms debounce
inside its `select!` loop. `App::lsp_record_edit` and the
App-side debounce task are gone; supervisor `record_edit /
flush / flush_all` survive as thin proxies for tests + the
will-save flush. `LspSupervisor` gained
`set_event_bus(Arc<EventBus>)` (called once at App startup
before any buffer opens) and tracks a per-actor
`SubscriptionId` so shutdown can unsubscribe. New benches
in `crates/lattice-lsp/benches/lsp.rs`:
`lsp_edit_publish_three_subs`, `lsp_edit_propagation_publish_to_recv`,
`lsp_didchange_flush_16_edits`. New tests in
`tests/fan_in.rs` cover end-to-end didChange after publish,
OpenDoc → RecordEdit FIFO, scratch-buffer (no path) skip,
unknown-URI warn-and-skip, 50-edit burst coalesce into one
didChange, shutdown unsubscribes the bus. Architecture
detail in `docs/../architecture/lsp-architecture.md` §5 + new §11
("Edit-path architecture") with the bus → fan-in → actor
diagram.

Update this section when picking up the in-flight item.

**4.x audit pass (post-LSP-edit refactor).** Once the per-actor
DocSync + bus-driven fan-in landed, ran a thorough design-
philosophy audit looking for the same class-of-bug elsewhere
(UI-thread / async contention, state-bearing best-effort
sends, paramount-goal violations). Findings closed in slices
1–6:

- **Slice 1** — retired `Arc<tokio::sync::Mutex<LspSupervisor>>`.
  Reads (`servers_for`, `running_actors`, ...) go wait-free
  through `ArcSwap<SupervisorSnapshot>`; writes route via
  the supervisor task's mailbox. Closed C2 (Insert-mode
  trigger probes), H1 (modeline `try_lock`), H2 (14
  supervisor `try_lock` sites silently dropping work), M5
  (App holding the supervisor mutex across `.await` in
  `drain_pending_lsp_opens`).
- **M1** (parallel agent worktree) — `EventBus::publish`
  snapshots subscriber list under a brief lock, then
  dispatches lock-free. No `Channel(bounded)` subscriber
  can stall the publisher under the inner lock.
- **Slice 2** — `DiagnosticsLayer` swaps inner `Mutex` for
  `Arc<ArcSwap<DiagnosticsSnapshot>>`. Render-frame
  `line_severity` calls (~3000/s on the render thread) drop
  from microseconds + per-call allocation to **25 ns**
  wait-free. Closed C3.
- **Slice 3** — `SyntaxActor`. `Syntax` wraps a
  `SyntaxSnapshot`; `SyntaxHandle` runs reparses on
  `tokio::task::spawn_blocking` with bursts coalesced.
  Renderer / folds / completion read the latest snapshot
  via `ArcSwap` -- tree-sitter parses no longer happen on
  the UI thread (paramount goal #1). Closed C1.
- **Slice 5** — `path_completion_cache` keyed by
  `(dir, mtime)` so consecutive Insert keystrokes inside
  string literals don't re-walk the directory; bounded-
  parallel `willSaveWaitUntil` (one shared 500ms budget
  across N servers via `tokio::task::JoinSet`) caps the
  total UI-thread save block at 500ms regardless of N.
  Closed H5, M4. H4's other FS sites
  (`open_lsp_locations_picker` reads, `:reload-snippets`,
  `:e` `metadata`) stay sync as user-command-triggered
  one-shots; documented as acceptable v1 cost.
- **Slice 6** — document-actor mailbox switches from
  bounded `mpsc::channel(64)` to `unbounded_channel`.
  `RuntimeError::Busy` retired entirely (App-side callers
  were silently discarding it under bursts). Closed H3.
- **Slice 7** — `FrameView` per-render-chain snapshot.
  Reading the architecture honestly: GPUI ships as part
  of 1.0 (TUI is a renderer peer, not the only target),
  so "Rust ownership prevents the race today" stops being
  a valid deferral once a second renderer that runs on a
  separate thread enters the picture. `FrameView::from_app`
  freezes `folds`, `visible_highlights`, and
  `show_line_numbers` once at chain entry; helpers
  (`compose_visible_lines_inner`, `render_gutter_for`,
  `closed_fold_display_span`, `buffer_line_to_visible_row_with`,
  `cursor_screen_position*`, `draw_inactive_document`)
  consume `&FrameView`. Mirror methods
  (`view.fold_start_at_any` / `fold_start_at` /
  `line_inside_closed_fold`) read from the snapshot's frozen
  Arc. Closed M2.
- **Slice 9** — event-driven LSP attach. The LSP open path
  used to park the UI thread on the `initialize` round-trip
  (initial document via `runtime::initialize_lsp_blocking`,
  subsequent `:e <path>` via the
  `pending_lsp_opens`-queue + `block_on(drain)` pattern in
  the main loop). Two block_on sites both violated paramount
  goal #4 (asynchronicity). The audit also surfaced a
  silent-failure bug -- `LspSupervisor::spawn` used
  `tokio::runtime::Handle::try_current()` and dropped
  `cmd_rx` if no ambient runtime existed; in production
  `App::new` runs before any tokio context, so the
  supervisor task never spawned and every write returned
  `LspError::ActorGone`. Both fixed in this slice:
  - `LspSupervisor::spawn` now takes an explicit
	`&tokio::runtime::Handle`; callers pass
	`runtime::lsp_runtime().handle()`. No more
	`try_current()` footgun.
  - Buffer-open is event-driven: `App::new` and
	`App::do_edit` set `BufferId → Uri` eagerly and publish
	[`Event::DocumentOpened { id, path, version, text }`](../crates/lattice-protocol/src/event.rs)
	on the bus. The new `lattice_lsp::attach_driver` module
	subscribes on the LSP runtime, runs a serial
	`recv → supervisor.open_buffer.await` loop, and logs
	failures. UI thread never parks. Single path for
	initial + subsequent opens.
  - Removed: `App::pending_lsp_opens`, `App::queue_lsp_open`,
	`App::drain_pending_lsp_opens`, `App::initialize_lsp`,
	`runtime::initialize_lsp_blocking`,
	`runtime::drain_pending_lsp_opens_blocking`, and the
	main-loop drain step. Net diff is removal-heavy.
  - Closes the no-rust-analyzer-attach regression that
	surfaced as `"server actor is no longer running"` on
	every editor launch.

Deferred with rationale:

- **M3 (input.rs trie-driven dispatch)** — `input.rs` is
  4365 lines of hand-rolled `KeyCode` matching; plugins
  can't bind chords (paramount goal #3 violation: "the
  grammar IS the public command API"). Real present-day
  capability gap, scoped as Slice 8 of this audit pass
  (in progress). The fix replaces the hand-rolled match
  with a trie consuming the `KeymapEntry { chord,
  command_invocation }` table from `keymap.rs`; metadata
  surface in `keymap.rs` becomes the source of truth;
  `input.rs` becomes a chord-string normaliser; plugins
  gain a structured extension point.

`docs/benchmarks.md` got the new perf rows
(`lsp_diagnostics_line_severity_wait_free` at 25ns, the
existing edit-path benches). Tests stayed green at every
slice boundary.

1. **Phase 4: LSP** — diagnostics, completion, hover, go-to-definition,
   references. The cancellation-token plumbing is in place
   (`dispatch_with_cancel` + cooperative search cancellation), so LSP
   request cancellation hooks into existing seams; the remaining work
   is the LSP client (tower-lsp or hand-rolled) + per-server shims.
2. **Computed folds** (per `docs/../../user/folding.md`) — **✅ done for
   all v1 providers except tree-sitter syntax queries.** Manual
   `zf` / `zo` / `zc` / `za` / `zR` / `zM` / `zd` / `zj` / `zk`,
   plus the new `zi` (`:set foldenable!`). Two computed providers:
   `compute_indent_folds` (universal) and `compute_markdown_folds`
   (ATX heading nesting, code-fence aware for both ``` and ~~~).
   `:set foldmethod=manual|indent|markdown|syntax` parses; `Syntax`
   is a v1 cascade (markdown for `.md`, indent otherwise) until the
   tree-sitter scope-query provider lands.

   Beyond storage, the user-facing pieces from `docs/../../user/folding.md`:
   identity-hash recompute (heading text + indent depth) preserves
   closed-state across edits in unrelated sections; closed folds
   render heading-preserved with a dim ` ┄ N lines folded` suffix
   (no `+--- N lines ---` line replacement); gutter glyphs ▾ open
   / ▸ closed; `dd` / `yy` / `cc` / `>>` on a closed fold expand
   to the full fold range as a single undo unit; jump-class motions
   (search, gg / G, H / M / L, marks, Ctrl-O / Ctrl-I, `%`) auto-
   open the destination fold; `:set foldenable` + `zi` short-circuit
   every fold-aware path while preserving closed-state.

   Tree-sitter-driven folds (function bodies, classes, blocks via
   `folds.scm`) remain queued. The `Fold` data type is shared, so
   a tree-sitter provider drops into the existing recompute /
   identity / render plumbing without a redesign.
3. **`:set option=value` + typed options** (§5.12) ✅ done — now
   in its own renderer-agnostic crate. The `lattice-config` crate
   owns the option machinery: an `OptionType` trait (with built-in
   impls for `bool`, `i64`, `String`); `Option<T>` whose value cell
   is an `arc_swap::ArcSwap<T>` for wait-free hot-path reads; an
   `ErasedOption` trait + `ConfigRegistry` backing both typed
   `OptionHandle<T>` access and by-name (`:set foo=bar`) access;
   the `:set` syntax parser; and an `OptionsGenerator` that the
   completion pipeline picks up via the `gen:options` source.
   `register_core_options(&registry) → CoreOptions` registers the
   nine renderer-agnostic options (number, relativenumber, wrap,
   ignorecase, tabstop, foldenable, foldmethod, scrolloff,
   completion.auto_insert_single); each renderer registers its
   own UI-specific options through the same `register` API and
   gets its own typed-handle struct back (`lattice-ui-tui` ships
   `register_tui_options → TuiOptions` for `ui.dim_inactive`,
   `ui.separator`, `ui.separator_color`,
   `ui.statusline_active_fg`, `ui.statusline_inactive_fg`).
   `FoldMethod` lives in `lattice-core::folding` (any renderer
   reads it the same way); the `OptionType for FoldMethod` impl
   lives in `lattice-config::domain` to keep core's dep direction
   inward. `:set name=value` echoes are byte-identical to the
   pre-migration wording, including all error messages
   (`E518: Unknown option`, `E474: not a boolean option`,
   `tabstop out of range [1, 32]: N`). Multi-option `:set`
   syntax (`:set ic hls scs`) still deferred.

   **App integration**: `App.config: Arc<ConfigRegistry>` plus
   typed handle structs (`core_options`, `tui_options`) replace
   the previous duplicated `App.foldmethod` /
   `App.show_line_numbers` / etc. fields. Read-side accessors
   (`app.foldmethod()`, `app.tabstop()`, ...) wrap the indirection
   so call sites read like a field access. `do_set` calls
   `config.parse_and_set_command(...)` and runs `apply_post_set`
   for cascade side effects: `relativenumber` ⇒ `number=true`,
   `foldmethod` ⇒ `recompute_folds()`, every `ui.*` ⇒
   `sync_theme_from_config()` to refresh the cached `Theme`
   `Style` projections.

   **§5.12 amendment landed in ../architecture/design.md (no plugin code yet).**
   Two-layer config codified: `~/.config/lattice/options.toml`
   for static data; `~/.config/lattice/init.rs` compiled to WASM
   Component, loaded by the §5.5 plugin host with a `boot`
   capability, for programmable config. Auto-build on first boot
   (cargo-component under `~/.cache/lattice/`); cache by source
   hash + lattice version + WIT revision. `lattice config build`
   exists as a diagnostic CLI. Project-local code-config deferred
   behind a future per-directory trust prompt; project-local
   `options.toml` supported. Implementation depends on Phase 7
   (plugin host); `lattice-config-api` crate added to the project
   layout as the WIT-bindings reexport user `init.rs` consumes.

3a. **§5.2.1 closure: kind-prefix form on `:`** ✅ done. The legacy
   parser-kind rejection is gone; every command (motion, operator,
   text-object, ex-command, plugin contribution) is reachable from
   `:` via `:<kind> <name>` syntax. Three reserved kind words on
   `:` (`motion`, `operator`, `text-object`); ex-commands keep
   their bare alias surface. Operator targets resolve via
   implicit-namespace lookup. ../architecture/design.md §2.2 codifies the
   no-function-call-syntax-on-`:` invariant; §5.2.1 specifies the
   kind-prefix grammar. See
   `crates/lattice-ui-tui/src/excommand.rs::parse_kind_prefixed`.
4. **Multi-buffer foundations** (§5.9) — the trigger for `HelpDisplayMode`
   beyond `Popup`. Until this lands, all introspection is overlay-rendered.
   - **B.1.a buffer abstraction + active-buffer routing** ✅ done.
	 `BufferKind { Document, Help }` + `BufferId` newtype; `App::active_buffer`
	 decides which cursor a motion / page / scroll / `<C-o>` / `<C-i>`
	 action mutates. Help routes through the same `translate_normal` chord
	 grammar as document buffers; only three buffer-local bindings differ
	 (`Esc` / `q` dismiss, `<CR>` follows the link under the cursor). The
	 unified position-history ring carries `(buffer, buffer_id)` so jump-list
	 walks switch active_buffer cleanly when crossing buffer boundaries.
	 `lattice_grammar::execute_motion_only` exposes a read-only motion
	 dispatch path that resolves a `CommandInvocation` against a bare
	 `Buffer` -- no `Document` / undo / selections required, suitable for
	 help and (later) file-tree / outline / diagnostics views.
   - **B.1.b pane tree + splits** ✅ done. Recursive binary-split
	 `PaneTree` with `<C-w>{s,v,c,q,h,j,k,l,w,W}` chord grammar. Each
	 leaf carries per-pane viewport stash (cursor + scroll); the
	 active pane's stash is hot-loaded into `App::cursor` /
	 `App::scroll` so motion code stays unchanged. Active-pane
	 switches snapshot back into the source pane's stash and load
	 from the destination's. Geometry-aware navigation
	 (`<C-w>{h,j,k,l}`) walks the spatial neighbour computed from
	 `compute_rects`. Inactive-pane rendering is a placeholder
	 until B.1.c brings meaningfully distinct buffer content.
   - **B.1.c multiple Document buffers** ✅ done. Replaced by the
	 unified registry below; original implementation used a
	 dedicated `documents: HashMap<BufferId, DocumentEntry>`.
   - **B.1.d buffer-as-content kinds: file-tree** ✅ done. New
	 `BufferKind::FileTree` variant + `FileTreeBuffer` (rope-
	 backed, same shape as `HelpBuffer`). `:Tree [path]` opens a
	 tree buffer rooted at `path` (or the document's parent dir /
	 cwd); same path de-dups (already-open trees are switched to,
	 not duplicated). Multiple trees coexist (one per
	 distinct root). `:TreeClose` removes from the registry.
	 Standard motions route via the same active-buffer dispatch
	 as Help. `<CR>` on a directory toggles expansion; on a file
	 opens it via the standard `:e FILE` path. Outline + diagnostics
	 panels queue behind their own integrations.
   - **Unified `BufferRegistry`** ✅ done. Documents and file trees
	 live in a single keyspace under `App.buffers`
	 (`HashMap<BufferId, BufferEntry>` with `BufferData::Document |
	 FileTree` discriminant). `:bn` / `:bp` / `:ls` / `:bd` /
	 `:b N` operate on the registry uniformly -- cycling between a
	 document and a tree feels the same as cycling between two
	 documents. Each entry carries `BufferFlags { listed, hidden }`;
	 unlisted buffers are skipped by `:bn` / `:bp`. `:e folder`
	 defers to `:Tree folder` (vim's `:Explore` semantics). Help
	 stays overlay-rendered for now -- moving it into the registry
	 is a follow-up that doesn't require structural change.
	 `DocumentEntry` stashes per-buffer hot-path state (syntax tree,
	 fold list, last-parsed version) when a buffer leaves active;
	 a single `App::activate_buffer_state` lifecycle hook fires on
	 every transition into a document buffer (both `:e <new>` and
	 `:b N` paths), reparsing if needed and seeding folds against
	 the active `foldmethod` so users no longer have to reach for
	 `<C-l>` after switching files. New buffer-level state plugs
	 into the same hook -- no per-option fixups across the
	 activation paths.
   - **Pane visuals** ✅ done. Each pane gets a vim-style status
	 line (active reverse-videoed via theme, inactive dim);
	 `│` separator drawn between vertically split panes. Inactive
	 Document panes keep their tree-sitter syntax highlights
	 (refreshed lazily by `App::refresh_pane_highlights`); a
	 `Theme::inactive_pane_overlay` modifier (default `DIM`)
	 layers on top so focus stays unambiguous without losing
	 color. Customizable via `:set ui.dim_inactive`,
	 `ui.separator`, `ui.separator_color`,
	 `ui.statusline_active_fg`, `ui.statusline_inactive_fg`.
5. **Hover popup + inline completion popup polish** — completion popup
   is wired (vertico-style); hover popup scaffolding ✅ done. New
   `HoverPopup` type with markdown body + buffer-position anchor;
   markdown highlights computed via the shared `LangRegistry`. The
   renderer floats the popup near the cursor (below if room, above
   otherwise). `:hover [text]` opens manually for now (Phase 4 LSP
   will source `text` from `textDocument/hover`); `:HoverClose`
   dismisses.
5b. **Help topic surface (`:help`)** ✅ done.
	`lattice-ui-tui::help_topics` defines a `HelpTopicRegistry`
	keyed by name; bodies are either `Static(&'static str)`
	(built-ins are sourced from `docs/user/*.md` via
	`include_str!` so the binary is self-contained) or
	`Dynamic(closure)` -- the seam for LSP / plugin / config-
	supplied topics. `:help` with no arg opens the registry's
	`index` topic (the README content); `:help <topic>` opens the
	named topic; `:h` is an alias. `<Tab>` enumerates topics via a
	new `gen:help-topics` completion source. `:describe-*`
	appends `See also: [topic](help:topic)` cross-links when a
	topic's `related_command_patterns` substring-matches the
	described command. New `HelpLinkTarget::Topic(name)` variant
	+ `help:` URL scheme so topic links are first-class everywhere
	a help body can render.
6. **Help major mode + tree-sitter grammar** — defines sections,
   link-targets, code-blocks. Needs the help mode registered as a major
   mode, which depends on the modes registry (Phase 8) but the *grammar*
   can be drafted earlier.
7. **Veto-class hooks + actor event publish** (§5.10.2 / §5.2.1) —
   observation-only event bus is in place; pre-mutation hooks
   (`BeforeSave`, `BeforeQuit`) need the mutation/abort return path.
   Actor wiring to publish `DocumentChanged` / `SelectionsChanged` /
   `ModalModeChanged` events. Unblocks autocmds.
8. **Per-`LatencyClass` deadline timers** — Reflex commands observe a
   2 ms deadline, Display commands 10 ms, both via the cancellation
   token already plumbed.
9. **Bench regression gate** — needs a stable runner (self-hosted or
   `bencher.dev`); shared GitHub runners have ~20% bench variance that
   dwarfs a 10% regression signal.
10. **Render-hot-path alloc discipline** — dhat-based assertion that
	steady-state frames produce no allocations.

---

## §15 open questions still load-bearing

These are tracked in ../architecture/design.md §15. Items the implementation has resolved
are crossed out there. Items that influence active tasks:

- §15:18 Folds storage / interaction — feeds the **Computed folds** task above.
- §15:19 Replace mode dispatch (resolved by current Replace impl).
- §15:20 Live evaluation (deferred per §10).
- §15:21 File watcher / auto-revert — unaddressed.
- §15:22 Bookmarks / cross-file marks — current marks are buffer-local.
- §15:23 Function rebinding / advice — unaddressed.
- §15:24 Narrow-to-region — unaddressed.
- §15:25 Snippets / abbrev — unaddressed.
- §15:26 Frames (multi-OS-window) — unaddressed.
- §15:27 Session save / restore — unaddressed.

---

## Test counts (snapshot)

Workspace tests as of the last commit. Coverage by crate:

| Crate                            | Tests |
|----------------------------------|-------|
| lattice-protocol                 | 39    |
| lattice-core (incl. integration) | 128   |
| lattice-grammar                  | 191   |
| lattice-completion               | 124   |
| lattice-config                   | 118   |
| lattice-syntax                   | 82    |
| lattice-runtime                  | 43    |
| lattice-lsp                      | 183   |
| lattice-picker                   | 48    |
| lattice-ui-tui                   | 1403  |
| lattice-host                     | 263   |
| lattice-ui-gpui                  | 25    |

Plus criterion benches for hot paths (search, buffer, motions, operators,
runtime actor) — see `docs/benchmarks.md` for the latest numbers.

---

## Conventions for updating this doc

- Update the **Phase status** table whenever a phase advances.
- Update the **Vim grammar coverage** table when a primitive lands; the
  status column uses ✅ done, 🔄 in progress, 🟡 partial, ⛔ pending,
  ⚠️ usable-with-caveats.
- Update **In-progress** before each commit that lands the in-flight item.
- Move completed items from **Up next** into the appropriate coverage table.
- Update **Test counts** at the end of each session.
- Don't write per-session log entries here — `git log --oneline` is the log.
