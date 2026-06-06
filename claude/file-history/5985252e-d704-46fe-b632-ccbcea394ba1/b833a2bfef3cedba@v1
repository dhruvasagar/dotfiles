<p align="center">
	<img src="./assets/readme-banner.svg" alt="Lattice - a modal, GPU-accelerated, plugin-first text editor in Rust" width="720" />
</p>

A modal, GPU-accelerated, plugin-first text editor written in Rust. Combines
**vim's modal editing power** with **emacs's extensibility model** on a
non-blocking, multi-threaded core where the UI thread does no I/O, no parsing,
and no shaping.

> **Status:** Pre-1.0 / heavy development. Phases 0–4 of the design roadmap
> are landed (foundation, modal engine, terminal UI, tree-sitter, LSP);
> **Phase 5 (GPU rendering + architectural asynchrony enforcement) is structurally complete** —
> the Editor now runs on its own dedicated thread, the `&mut Editor` type
> cannot escape that thread in production builds (compile-time enforced
> via the `EditorActorHandle` swap, slice `3c.final.E.swap`), and both
> TUI and GPUI peers are editable against a live LSP backend
> (rust-analyzer tested). Outstanding Phase 5 work is perf-driven: queued
> RenderState lifts in
> [`docs/dev/operations/3c-final-b-extension.md`](docs/dev/operations/3c-final-b-extension.md).
> The WASM plugin host arrives in Phase 7. See
> [`docs/dev/operations/implementation.md`](docs/dev/operations/implementation.md)
> for the per-feature ledger.

---

## Why another editor?

Three editors dominate today: Vim/Neovim (best modal editing, single-threaded
core, vimscript-only first-class config), Emacs (best extensibility, single-
threaded core, elisp-only first-class config), and VS Code (best plugin
ecosystem, web stack, latency dominated by Electron).

Lattice picks the strongest property from each and rebuilds them on a modern
foundation:

- **Strict vim grammar.** Counts, registers, operators, motions, text
  objects, ex-ranges, dot-repeat, marks, macros — semantics preserved
  exactly. The grammar is the public command API; the default keymap is a
  config file.
- **Emacs-class extensibility through WebAssembly.** Plugins are sandboxed
  WASM components: cross-language, capability-gated, fuel-limited,
  crash-isolated. A misbehaving plugin cannot freeze the editor.
- **Sub-frame input latency.** Keystroke → glyph in <8 ms at 120 Hz. The UI
  thread never blocks. Multi-threaded by construction (one tokio task per
  document, snapshot-based render reads, bounded-mailbox dispatch).
- **GPU-accelerated rendering.** Sub-pixel-precise text, smooth scroll,
  layered paint paths optimized per content type (code vs. rich text vs.
  inline media). TUI is a first-class peer — not a throwaway.

The full design is in [`docs/dev/architecture/design.md`](docs/dev/architecture/design.md) (v0.4, ~2300 lines).

---

## Paramount goals

In priority order when they conflict:

1. **Performance.** Sub-frame input latency. Per-call WASM overhead
   budgeted in CI (typed call < 500 ns p99; grammar-extension round-trip <
   5 µs p99).
2. **Extensibility.** WebAssembly Component Model plugin host from day one.
   WIT is the canonical API. Plugins ship in any language with
   component-model toolchain support (Rust, Zig, Go, AssemblyScript, …).
3. **Extensible vim modal editing.** Strict vim semantics. The grammar
   (operators, motions, text objects, registers, ranges, counts) IS the
   public command API. Adding new motions / text objects / operators is
   first-class — including future tree-sitter-driven variants.
4. **Asynchronicity.** Three-layer architecture (UI / Core / Plugins)
   communicating via typed message passing. Multi-threaded by construction.
   Each plugin instance owns its own `wasmtime::Store` and runs as a tokio
   task; many plugins execute in parallel across cores.

Three deliberate deviations from vim and emacs:

- **Unified command / grammar dispatch.** Vim's `:` ex-command world and
  the functional / plugin world are merged into one `CommandRegistry` with
  one dispatcher. The `:` line stays vim DSL (no function-call / palette
  syntax — explicit non-goal); plugins / `init.rs` / Rust callers
  construct `CommandInvocation` directly via the WIT host. Two input
  surfaces, one substrate. Every command is reachable from `:` via the
  kind-prefix form (`:motion goto-first-line`, `:operator delete word-
  forward`). (DESIGN.md §2.2 + §5.2.1.)
- **Everything is a buffer.** File tree, outline, diagnostics, search
  results, terminal, REPL — all are buffers placed by the user into panes
  via splits. The unified `BufferRegistry` already holds documents and
  file trees today; `:bn` / `:bp` / `:ls` / `:bd` work across kinds. No
  fixed sidebar or bottom-panel concept. (§5.9.)
- **One extension substrate, two config layers.** `options.toml` for static
  data; `init.rs` (compiled to WASM, auto-built on first boot, cached) for
  programmable config. `init.rs` is a plugin with `boot` capability — the
  same WIT, same toolchain, same host as third-party plugins. No vimscript,
  no elisp, no Lua, no embedded scripting language. See DESIGN.md §5.12.

---

## Architecture

Three layers, communicating only via typed messages and wait-free snapshot
loads:

```mermaid
flowchart TD
	UI["<b>UI Layer</b><br/><code>lattice-ui-tui</code> &nbsp;(future GPU renderer)<br/>• Renders snapshots; never blocks; never holds locks<br/>• Translates input → CommandInvocation"]

	Core["<b>Core Layer</b><br/><code>lattice-runtime</code> + <code>lattice-core</code> + <code>lattice-grammar</code><br/>• One DocumentActor per open document (tokio task)<br/>• Owns the writable Document; bounded mpsc mailbox<br/>• Publishes immutable snapshots via arc-swap<br/>• Grammar dispatcher: motions, operators, text objects,<br/>&nbsp;&nbsp;ex-commands, plugin contributions — peers, not<br/>&nbsp;&nbsp;separated worlds"]

	Plugin["<b>Plugin Layer</b> &nbsp;<i>(planned)</i><br/><code>lattice-plugin-host</code><br/>• wasmtime + Component Model + WASI<br/>• One Store per plugin instance, runs as a tokio task<br/>• Capability-gated, fuel-limited, crash-isolated"]

	UI -->|"<b>DocumentHandle</b> (cheap clone)<br/>• snapshot() — wait-free Arc load<br/>• dispatch_with_cancel() — Pending&lt;Effect&gt;<br/>• apply_edit() — Pending&lt;AppliedEdit&gt;"| Core
	Core -.->|"WIT-defined ABI<br/><i>(planned)</i>"| Plugin

	classDef done fill:#1f4d2c,stroke:#2ea043,color:#e6edf3
	classDef planned fill:#3d2a1a,stroke:#bf8700,color:#e6edf3,stroke-dasharray:5 5
	class UI,Core done
	class Plugin planned
```

### Crate map

| Crate                  | Purpose                                                                                                  | Status      |
|------------------------|----------------------------------------------------------------------------------------------------------|-------------|
| `lattice-protocol`     | Bottom-layer types: `Position`, `Range`, `Edit`, `Selection`, `CancellationToken`, `Event`, ID newtypes. | ✅ stable   |
| `lattice-core`         | `Buffer` (ropey-backed), `Document` with batched undo, file I/O, regex search (fancy-regex w/ backrefs). | ✅ stable   |
| `lattice-grammar`      | Vim modal state machine, `CommandRegistry`, dispatcher, built-in motions/operators/text objects/ex-cmds. | ✅ stable   |
| `lattice-runtime`      | `DocumentActor` + `DocumentHandle` (tokio task per doc), arc-swap snapshots, `Pending<T>`, event bus.    | ✅ stable   |
| `lattice-syntax`       | Tree-sitter integration (Rust / Python / JavaScript / Markdown bundled), incremental parse, highlights.  | ✅ stable   |
| `lattice-completion`   | Pluggable completion pipeline: generators, matchers, rankers, annotators.                                | ✅ stable   |
| `lattice-config`       | Typed-options registry (`OptionType` + `ArcSwap`-backed cells), `:set` parser, `gen:options` source.     | ✅ stable   |
| `lattice-mode`         | Major / minor mode trait surface, lifecycle events, mode registry, mode-async epoch.                     | ✅ stable   |
| `lattice-help`         | `HelpContent` + `HelpBuffer` + per-line markdown highlight cache; backing for `:help` and `:describe-*`. | ✅ stable   |
| `lattice-picker`       | Picker primitive: source registry, candidate batches, live-query subsystem, MRU frecency.                | ✅ stable   |
| `lattice-snippet`      | Snippet parser + registry; lazy expansion against `editor.snippet_registry` (ArcSwap).                   | ✅ stable   |
| `lattice-file-tree`    | File-tree buffer kind + per-buffer state types.                                                          | ✅ stable   |
| `lattice-oil`          | Oil-style directory buffer kind + state types.                                                           | ✅ stable   |
| `lattice-lsp`          | LSP client: actor pool, capability fingerprinting, diagnostics layer, supervisor, watcher subscriptions. | ✅ stable   |
| `lattice-host`         | Renderer-agnostic substrate. Owns `Editor`, dispatch, mode lifecycle, options cascade, `RenderState`, `PerBufferCache`, LSP watcher task. | ✅ stable   |
| `lattice-ui-tui`       | Terminal UI peer: crossterm + ratatui, modal cursor, gutter, hlsearch, command line, popups, picker UI.  | ✅ stable   |
| `lattice-ui-gpui`      | GPU UI peer (feature `window`): GPUI + blade rendering, symmetric capability with the TUI peer.          | 🚧 partial parity |
| `lattice-cli`          | Binary entry-point. `--tui` / `--gui` flag routes to either peer; tokio multi-thread main.               | ✅ stable   |
| `lattice-config-macros`| Proc-macro for typed-option / `OptionGroup` registration via `linkme` distributed slices.                | ✅ stable   |
| `lattice-plugin-host`  | WASM Component Model host. **Planned (Phase 7).**                                                        | ⛔ planned  |

---

## Quick start

**Requirements**

- Rust 1.94+ (edition 2024)
- A POSIX terminal that handles 256 colors and bracketed paste

**Build & run**

```sh
cargo build --release
cargo run --release -- README.md
```

The CLI opens the file in the TUI. Editing is full vim modal.

**Run tests**

```sh
cargo test --workspace        # ~1115 tests, sub-second
cargo clippy --workspace      # workspace lints (deny unsafe outside opt-in)
```

**Run benchmarks**

```sh
cargo bench --workspace
```

Numbers are tracked in [`docs/dev/operations/benchmarks.md`](docs/dev/operations/benchmarks.md).

**Editor sanity tour**

In the running editor:

- `i a o O` — Insert / append / open below / open above
- `h j k l w b e gg G` — motions
- `dd 2dd dw daw diw` — delete with operators / counts / text objects
- `yy 2yy p P` — yank / paste
- `u <C-r>` — undo / redo (every operator lands as one undo unit)
- `Ctrl-V` then `Ij<Esc>` — block-visual insert; `>` indents block lines
- `/foo<CR> n N` — incremental search (regex with backrefs)
- `:%s/foo/bar/g` — substitute (`$1`, `${name}` template syntax)
- `:describe-command write` — every primitive carries help metadata
- `:motion goto-first-line` — invoke any motion / operator / text-object by name (chord grammar still preferred for typing)
- `:operator delete word-forward` — operator + bare target (motion / text-object resolved implicitly)
- `<C-w>v` then `<C-w>l` — split vertically and focus the right pane
- `:Tree .` (or `:e some-folder`) — open a folder as a file-tree buffer; `<CR>` toggles directories or opens files
- `:bn` / `:bp` / `:ls` / `:bd` — cycle, list, or close any open buffer (document or tree)
- `:set foldmethod=indent` — auto-fold indented blocks; `zo` / `zc` to open / close
- `:set ui.dim_inactive=off` — turn off the inactive-pane DIM overlay
- `:help` — open the topic index; `:help folding` / `:help buffers` for deep-dive docs (`<Tab>` completes)

---

## What distinguishes Lattice

Among modern editors built on Rust + GPU + tree-sitter + LSP + WASM (the only stack that hits sub-frame latency without compromising extensibility, and one Lattice shares with Zed deliberately), the differentiators are:

- **Vim grammar is the public command API**, not a key mapping over a non-modal core. One dispatcher; ex-commands, chords, and plugin contributions all flow through `CommandInvocation`. Adding a motion *is* extending the grammar.
- **Everything is a buffer, enforced**. File tree, diagnostics list, terminal, `*messages*`, scratch — all are buffers placed by the user into panes via splits. There is no sidebar / bottom-panel concept. Every text operation works on every buffer kind through one code path.
- **WIT is the canonical plugin API today** (not aspirationally). Any Component-Model language speaks the same protocol — Rust, Zig, Go, AssemblyScript. CI-gated overhead budgets (typed-call < 500 ns p99, grammar-extension round-trip < 5 µs p99).
- **Asynchrony is architectural, not disciplinary**. Other editors keep the UI thread free by convention — contributors know which calls might block. Lattice (DESIGN.md §5.7) chooses primitives that make UI-thread blocking *physically impossible* in the steady state: `RenderState` for reads, `Arc<ArcSwapOption<T>>` / `PerBufferCache<T>` for writes, dedicated subsystem tasks for everything else. The architecture stays uniform under feature pressure.

The detailed framing — what Lattice converges with Zed on, where it diverges deliberately, what we evaluated (Zed's `cx.notify()` reactive paint — assessed and deferred; the savings curve doesn't transfer to Lattice's cursor-coupled UI), and what we explicitly don't borrow (imposed layouts, custom rope, single-language extensions) — is in [DESIGN.md Appendix C](docs/dev/architecture/design.md).

---

## Performance commitments

Tracked against [DESIGN.md §8.2](docs/dev/architecture/design.md). Latest measured numbers in
[`docs/dev/operations/benchmarks.md`](docs/dev/operations/benchmarks.md):

| Commitment                   | Target (p99) | Status                                               |
|------------------------------|--------------|------------------------------------------------------|
| Keystroke → buffer mutation  | < 100 µs     | ✅ ~83 µs constant across buffer sizes               |
| Reflex motion / operator     | < 2 ms       | ✅ all under budget on 50k-line buffers              |
| Search (literal pattern)     | < 2 ms       | ✅ all variants under 2 ms on 200k-line buffers      |
| Snapshot load (renderer)     | < 5 ns       | ⚠️ ~17 ns (`load_full` Arc bump — known headroom)    |
| WASM typed call (planned)    | < 500 ns     | ⛔ Phase 7                                           |

The architectural rule: **the UI thread does no I/O, no parsing, no
shaping.** Document mutations route through the actor; renderers read
wait-free snapshots; cancellation is cooperative (Reflex commands observe a
flipped `CancellationToken` within ~100 µs).

---

## Roadmap

11 phases. The detailed status ledger is in
[`docs/dev/operations/implementation.md`](docs/dev/operations/implementation.md); the phase-level summary:

| Phase | Title                                  | Status      |
|-------|----------------------------------------|-------------|
| 0     | Foundation                             | ✅ done     |
| 1     | Modal Editing                          | ✅ done     |
| 2     | Terminal UI Bootstrap                  | ✅ done     |
| 3     | Tree-sitter (Rust / Python / JS / MD)  | ✅ done     |
| 4     | LSP                                    | ✅ done     |
| 5     | GPU Rendering + architectural async    | ✅ structurally complete (Editor on its own thread via `EditorActorHandle`; goal #4 compile-time enforced). Perf B-extension lifts queued. |
| 6     | Document Renderer + UI Components      | ⛔ planned  |
| 7     | Plugin Host (WASM Component Model)     | ⛔ planned  |
| 8     | Major / Minor Modes + Reference Plugins| ⛔ planned  |
| 9     | Rich Buffer Rendering                  | ⛔ planned  |
| 10    | Polish + v1.0                          | ⛔ planned  |

### Detailed feature checklist

The granular pre-Phase-4 polish plan, plus the upcoming Phase 4 work:

**Async runtime + cancellation** (DESIGN.md §5.2, §5.6.8, §5.7)

- [x] `DocumentActor` + bounded-mailbox dispatch (one tokio task per doc)
- [x] `arc-swap` snapshot publish-before-reply contract
- [x] `Pending<T>` typed handles for every mutating call
- [x] Cooperative `CancellationToken` (grammar + actor + search loops)
- [x] Actor stress tests (mailbox saturation, concurrent senders, snapshot ordering)
- [ ] Per-`LatencyClass` deadline timers (Reflex < 2 ms, Display < 10 ms)
- [ ] Plugin async-task host primitive (Phase 7)

**Vim modal editing** (DESIGN.md §5.2)

- [x] Modal state machine: Normal / Insert / Visual / Op-pending / Command / Search / Replace
- [x] Strict vim grammar: counts, registers, operators, motions, text objects, ex-ranges
- [x] Built-in motion / operator / text-object catalog
- [x] Macros (recorded as `CommandInvocation` sequences, not keystrokes)
- [x] Marks, dot-repeat with insert-replay, position-history ring (§5.1.1)
- [x] Search + hlsearch with `fancy-regex` (RE2 + bounded NFA for backrefs)
- [x] Substitute (`:s` / `:%s`) with `$1` / `${name}` template syntax
- [x] Block-visual `d` / `y` / `c` / `I` / `A` / `>` / `<` (per-row dispatch + replicate-on-Esc + single undo unit)
- [x] Manual folds (`zf` / `zo` / `zc` / `za` / `zR` / `zM` / `zd`)
- [x] Counts on linewise ops (`2dd`, `2>>`) collapse to one undo unit
- [x] Substitute live preview (matches highlighted while typing `:s/pat/repl/...`)
- [x] Computed folds — indent fallback (`:set foldmethod=indent`); tree-sitter folds queued

**Unified command / grammar dispatch** (DESIGN.md §5.2.1)

- [x] One `CommandRegistry` for ex-commands, motions, operators, text objects
- [x] `:` line is a parser front-end producing typed `CommandInvocation`s
- [x] Every command is reachable from `:` via the kind-prefix form (`:motion goto-first-line`, `:operator delete word-forward`, `:text-object inner-word`); ex-commands keep their bare alias surface; chord grammar stays the natural compact-typing path
- [x] `:g/pat/body` and `:v/pat/body` parse `body` up front (no per-match re-parse)
- [x] `Range::Selection` resolves to active visual selection
- [x] Interactive arg-prompts via `args_schema` (any required arg arms a prompt; Chord kind auto-submits on next chord)

**Event system + hooks** (DESIGN.md §5.10)

- [x] Typed `Event` catalog in `lattice-protocol`
- [x] `EventBus`: `subscribe(filter, target)`, `unsubscribe`, `publish` (kind-indexed dispatch)
- [x] `SubscriptionTarget::Channel` (mpsc) and `SubscriptionTarget::Invocation`
- [ ] Actor publishes events on edit / save / mode-change
- [ ] `Before*`-event mutation / veto seam (formatters can rewrite content; `BeforeQuit` can abort)
- [ ] `:autocmd` and `add-hook` parser front-ends desugar to `subscribe`

**Self-documenting help** (DESIGN.md §5.11)

- [x] Every command / option / mode / keybinding carries metadata at registration time
- [x] `:describe-command`, `:describe-buffer`, `:describe-key`, `:keymap`, `:apropos`
- [x] `:describe-option`, `:options` (typed options registry)
- [x] `:help [topic]` -- free-form topic surface with `<Tab>` completion; built-ins embedded via `include_str!` from `docs/user/*.md`; `Dynamic` body variant is the seam for LSP / plugin-supplied topics; `:describe-*` views emit "See also" topic cross-links
- [ ] `:describe-event`, `:describe-mode` (each lands when its registry does)

**Configuration** (DESIGN.md §5.12)

- [x] **Renderer-agnostic typed-options crate** (`lattice-config`): `OptionType` trait with primitive impls (`bool`, `i64`, `String`); `Option<T>` with `ArcSwap<T>` value cell for wait-free reads; `OptionHandle<T>` for zero-overhead typed access; `ErasedOption` + `ConfigRegistry` for by-name lookups; `:set` parser; `gen:options` completion source — every consumer (App, plugins, future renderers) registers options through the same API
- [x] `register_core_options(&registry) → CoreOptions` for the nine renderer-agnostic options (`number`, `relativenumber`, `wrap`, `ignorecase`, `tabstop`, `foldenable`, `foldmethod`, `scrolloff`, `completion.auto_insert_single`)
- [x] Renderer-specific options register via the same API: `lattice-ui-tui::register_tui_options(&registry) → TuiOptions` covers `ui.dim_inactive`, `ui.separator`, `ui.separator_color`, `ui.statusline_active_fg`, `ui.statusline_inactive_fg`. Future GUI / web renderers register their own
- [x] `:set name=value`, `:set name`, `:set noname`, `:set name?` parser front-end (drives `ConfigRegistry::parse_and_set_command`)
- [x] `:describe-option <name>` (reads the erased view: name, aliases, type label, default, current value, enumerated values, doc)
- [ ] `options.toml` deserializer (static settings layer)
- [ ] `init.rs` plugin loader (Rust → WASM, auto-built on first boot, cached) — depends on Phase 7 plugin host
- [ ] Customize-as-buffer-view writes back to `options.toml`
- [ ] `lattice config build` diagnostic CLI subcommand
- [ ] Project-local `.lattice/options.toml` (project-local `init.rs` deferred behind a per-directory trust prompt)

**Rendering** (DESIGN.md §5.6)

- [x] TUI renderer (crossterm + ratatui) — first-class peer for headless / SSH
- [x] Display-width-aware cursor placement (CJK / Latin / emoji)
- [x] Tree-sitter highlight emission (Rust / Python / JS / Markdown bundled)
- [x] Markdown grammar with fenced-code injections (` ```rust``` ` blocks highlight as rust)
- [x] Markup `Style` variants for headings (1-6), bold / italic, links, raw — themable from day one
- [ ] GPU compositor (GPUI preferred, wgpu fallback) — Phase 5
- [ ] `EditorRenderer` + `DocumentRenderer` + `TuiRenderer` trait split — Phase 5/6
- [ ] Sprite atlas for icons (file-type, severity, gutter, picker, status) — §5.6.7
- [ ] Rich-buffer rendering (variable fonts within a single buffer) — Phase 9

**LSP** (DESIGN.md §5.4) — Phase 4

- [ ] Diagnostics, completion, hover, go-to-definition, references
- [ ] Cancellation (uses the cancellation-token plumbing already in place)
- [ ] Per-server compatibility shims

**Plugin host** (DESIGN.md §5.5, §9) — Phase 7

- [ ] `wasmtime` + Component Model + WIT bindings
- [ ] AOT module cache; lazy instantiation; capability manifests; fuel limits
- [ ] Per-call overhead bench gates in CI (typed call < 500 ns p99; round-trip < 5 µs p99)
- [ ] Reference plugin: `fuzzy-finder` (validates picker primitive end-to-end)

**Multi-buffer + UI components** (DESIGN.md §5.9) — Phase 6

- [x] **B.1.a** Buffer abstraction + active-buffer routing (help is a regular buffer; `<C-o>` / `<C-i>` walk across buffers)
- [x] **B.1.b** Pane tree + `<C-w>{s,v,c,h,j,k,l,w,W}` window-management chord grammar (incl. `<C-w><C-l>` form)
- [x] **B.1.c** Multiple Document buffers + `:bn` / `:bp` / `:ls` / `:bd` / `:b N`
- [x] **B.1.d** File-tree buffer (`:Tree path`); multiple roots coexist; `:e folder` defers to `:Tree folder`
- [x] Unified `BufferRegistry`: documents and trees in one keyspace; per-buffer `BufferFlags { listed, hidden }` skeleton
- [x] Vim-style pane visuals: per-pane status line (active reverse-videoed, inactive dim), `│` separator between vertical splits, inactive panes keep syntax highlighting with `DIM` overlay; all customizable via `:set ui.*`
- [x] Hover popup scaffolding (`:hover [text]` / `:HoverClose`); LSP-driven hover wiring queues with Phase 4
- [x] Inline completion popup (vertico-style, wired)
- [ ] Picker primitive (file picker as first user)
- [ ] Buffer-backed views: outline, diagnostics-list, scratch, messages, compilation (file-tree shipped)

**CI / engineering** (DESIGN.md §8)

- [x] Workspace lint policy (`unsafe_code = "deny"`, opt-in per module)
- [x] Criterion benches for runtime, motions, operators, search
- [x] Cross-platform CI matrix (Linux / macOS / Windows) + fmt + doc gates
- [x] Bench compile-check (catches bench-code rot per platform)
- [x] Bench baseline artifact recorded on every push to main
- [ ] Bench regression gate (needs stable runner; shared CI variance dwarfs signal)
- [ ] Allocation-discipline checks on the render hot path (dhat-based)

---

## Documentation

| Doc                                       | Purpose                                                  |
|-------------------------------------------|----------------------------------------------------------|
| [`docs/dev/architecture/design.md`](docs/dev/architecture/design.md)        | The design spec (v0.4, authoritative for what to build). |
| [`docs/dev/operations/implementation.md`](docs/dev/operations/implementation.md) | Per-feature status ledger; updated per session.   |
| [`docs/dev/operations/benchmarks.md`](docs/dev/operations/benchmarks.md)| Latest measured numbers vs. §8.2 commitments.            |
| [`docs/dev/operations/verify.md`](docs/dev/operations/verify.md)        | Manual-verification checklist for recently shipped features. |
| [`docs/dev/architecture/lsp-architecture.md`](docs/dev/architecture/lsp-architecture.md) | LSP developer reference (companion to DESIGN.md §5.4). |
| [`docs/dev/notes/lsp-features.md`](docs/dev/notes/lsp-features.md) | Every LSP 3.17 capability + implementation status.  |
| [`docs/user/`](docs/user/)                | User-facing reference (the `:help`-style topic docs).    |
| [`CLAUDE.md`](CLAUDE.md)                  | Conventions for AI-assisted contributions.               |

When something disagrees, **DESIGN.md and IMPLEMENTATION.md are the
authoritative sources** for what should exist and what currently does.

---

## Contributing

The project is open to contributions, but please note the development model:

1. **The design doc is load-bearing.** Significant features need to land in
   `docs/dev/architecture/design.md` first (or be a refinement of an existing section). Open
   an issue describing the design rationale before sending a PR for
   non-trivial work.
2. **The four paramount goals override stylistic preferences when they
   conflict.** Performance, extensibility, vim semantics, asynchronicity —
   in that order.
3. **Commit history is the moving record.** Each commit is a complete unit
   (one feature / one fix / one refactor). Tests for new behavior land in
   the same commit.
4. **No backwards-compatibility shims for vim or emacs configs.** Explicit
   non-goal.
5. **Specific edge cases may be deferred for v1.** The semantics aren't
   altered, but rare register quirks or obscure block-visual behaviors can
   land post-1.0 with explicit tests documenting the gap.

### Good first issues

- Documenting a vim grammar primitive that's missing from
  `docs/dev/operations/implementation.md`'s catalog table.
- Adding a built-in motion / text object behind the existing
  `register_motion` / `register_text_object` API.
- Adding a tree-sitter grammar (look at how `lattice-syntax` wires Rust /
  Python / JS).
- Closing a §15 open question with a small design proposal.

### Development workflow

```sh
# Format + lint + test before pushing.
cargo fmt --all
cargo clippy --workspace --all-targets -- -D warnings
cargo test --workspace
```

The lint policy is workspace-wide: `unsafe_code = "deny"`. The search
module opts in via `#![allow(unsafe_code)]` for one specific
`from_utf8_unchecked` call on a streaming window — every other use must
document its invariant.

---

## License

Licensed under the [MIT License](LICENSE).

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you shall be licensed as above,
without any additional terms or conditions.

---

## Acknowledgements

The design draws on three editors that got things right:

- **Vim / Neovim** — the modal grammar (counts × operators × motions × text
  objects) is one of the great ideas in editor design. We adopt it
  wholesale.
- **Emacs** — the everything-is-a-buffer principle, the self-documenting
  help system, and the customize-as-buffer-view model are all here.
- **Zed** — the GPUI rendering stack is the same one Lattice uses, and the
  modern-Rust editor playbook (GPU rendering, tree-sitter, native LSP,
  WASM extensions) is shared on purpose. Where Lattice diverges
  deliberately — modal grammar as the public API, everything-is-a-buffer
  enforced, WIT as the canonical plugin interface, *architectural* (not
  disciplinary) async — is laid out in
  [DESIGN.md Appendix C](docs/dev/architecture/design.md).
