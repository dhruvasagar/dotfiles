//! `lattice` -- the editor binary.
//!
//! Phase 2: opens a single file, renders it in a terminal viewer with cursor
//! motion. Subsequent phases wire the modal engine, tree-sitter, LSP, and
//! ultimately the GPU UI.
//!
//! ## Why `#[tokio::main]`
//!
//! Slice C.1: `main` is async so a tokio runtime is current from the
//! very first instruction of program execution.
//! `tokio::runtime::Handle::try_current()` succeeds anywhere
//! downstream -- including in `App::new` which constructs
//! `SyntaxHandle`s and (separately) the LSP supervisor handle.
//!
//! Pre-C.1, `main` was synchronous and the runtime didn't exist
//! yet at construction time, so `try_current()` silently failed.
//! Two specific subsystems noticed: LSP papered over with
//! explicit-handle plumbing
//! (`lattice_ui_tui::runtime::lsp_runtime()` + `LspSupervisor::spawn(handle)`);
//! syntax did *not* paper over and the worker was silently
//! never-spawned for the entire lifetime of Option B's
//! incremental-reparse pipeline -- producing the user-visible
//! "highlighting stuck to byte positions" symptom regardless of
//! how correct the algorithm was. C.1 makes that class of bug
//! impossible by ensuring a runtime is always current from start.
//!
//! `lattice_ui_tui::run` stays sync and is called directly from
//! the async main body. It blocks the executor thread for its
//! lifetime; spawned tasks (LSP, syntax, document actor) run on
//! other workers (the multi-thread runtime has `num_cpus` worker
//! threads). `block_on` calls inside the editor route through
//! `lattice_runtime::block_on`, which handles nested-runtime
//! contexts via `block_in_place` (slice C.1.a).

use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::Parser;

use lattice_core::Document;

#[derive(Debug, Parser)]
#[command(version, about = "lattice editor", long_about = None)]
struct Cli {
    /// Path to the file to open. If omitted, an empty buffer is opened.
    file: Option<PathBuf>,

    /// Route to the GPUI renderer (requires the `gui` build feature).
    /// Default until phase 5.last lands; from then on `--gui` becomes
    /// the default and `--tui` opts in to the terminal renderer. Mutually
    /// exclusive with `--tui`.
    #[arg(long, conflicts_with = "tui")]
    gui: bool,

    /// Route to the TUI renderer (terminal). Default until phase 5.last.
    /// Mutually exclusive with `--gui`.
    #[arg(long, conflicts_with = "gui")]
    tui: bool,

    /// Increase log verbosity. Default is `info`; `-v` bumps to
    /// `debug`, `-vv` to `trace`. Mutually-additive with
    /// `--quiet` / `--log-level` (later wins).
    ///
    /// Logs land in BOTH the in-editor `*messages*` buffer
    /// (open with `:messages`) and stderr. Adjust live with
    /// `:set messages.filter=<level>`.
    #[arg(short = 'v', long = "verbose", action = clap::ArgAction::Count)]
    verbose: u8,

    /// Decrease log verbosity. `-q` drops to `warn`, `-qq` to
    /// `error`. Mutually-additive with `--verbose` / `--log-level`.
    #[arg(short = 'q', long = "quiet", action = clap::ArgAction::Count)]
    quiet: u8,

    /// Explicit log level override. Accepts `error` / `warn` /
    /// `info` / `debug` / `trace`, or a full
    /// `tracing-subscriber::EnvFilter` directive (e.g.
    /// `lattice_lsp=debug,lattice_host=info`). Wins over
    /// `-v` / `-q` when both are passed.
    #[arg(long = "log-level", value_name = "LEVEL")]
    log_level: Option<String>,

    /// Issue #36 (2026-05-22): force-enable stderr tracing
    /// output even in TUI mode. By default the TUI peer
    /// disables stderr writes because stderr IS the terminal
    /// ratatui paints into — every `tracing::*` event would
    /// blit a stray line over the screen. This flag is for
    /// users who run TUI with `lattice 2>tracing.log` to
    /// capture events to a file. The GPUI peer always has
    /// stderr enabled (its stderr is a separate stream).
    ///
    /// In any case the `*messages*` buffer ALWAYS captures
    /// events; this flag only controls the EXTRA stderr
    /// write.
    #[arg(long = "stderr-logs")]
    stderr_logs: bool,
}

/// Resolve the final log-level directive from CLI flags +
/// env override. Precedence (last wins):
///   1. Default: `info`.
///   2. `-v` / `-q` counts shift relative to info.
///   3. `--log-level=...` overrides absolutely.
///   4. Pre-existing `LATTICE_LOG` env var trumps flags so
///      `LATTICE_LOG=trace lattice file.txt` works without
///      remembering the CLI flag name.
fn compute_log_level(cli: &Cli) -> String {
    // 4: caller-supplied env wins outright.
    if let Ok(env) = std::env::var("LATTICE_LOG") {
        if !env.is_empty() {
            return env;
        }
    }
    // 3: explicit --log-level overrides verbose/quiet.
    if let Some(spec) = cli.log_level.as_deref() {
        return spec.to_string();
    }
    // 2: verbose/quiet count shifts. Levels in ascending order:
    //    error < warn < info < debug < trace
    //    info index = 2; -v bumps +1, -q drops -1.
    let levels = ["error", "warn", "info", "debug", "trace"];
    let info_idx: i32 = 2;
    let shift = i32::from(cli.verbose) - i32::from(cli.quiet);
    let idx = (info_idx + shift).clamp(0, (levels.len() - 1) as i32) as usize;
    levels[idx].to_string()
}

#[tokio::main(flavor = "multi_thread")]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // 2026-05-22 messages-overhaul: removed the prior
    // `tracing_subscriber::fmt().try_init()` install — it won the
    // global subscriber race and silenced `*messages*`. The
    // composed fmt+MessagesLayer subscriber installed by
    // `install_messages_subscriber` (in editor_boot) now provides
    // BOTH the stderr stream AND the in-editor `*messages*`
    // surface from a single subscriber.
    //
    // CLI computes the boot-time log level from flags + env and
    // hands it to the runtime via `set_boot_log_level`; editor_boot
    // calls `boot_log_level()` inside the install path. Using a
    // `OnceLock<String>` setter avoids `std::env::set_var` (denied
    // by `#![deny(unsafe_code)]` and unsafe-flagged in recent
    // Rust).
    let level = compute_log_level(&cli);
    lattice_runtime::set_boot_log_level(level);

    // Issue #36 (2026-05-22): gate the fmt-to-stderr layer.
    // TUI's stderr is the editor terminal — every event
    // would blit a stray line over ratatui's paint. Defaults
    // OFF for TUI, ON for GPUI. `--stderr-logs` forces ON
    // (e.g. `lattice --tui --stderr-logs 2>tracing.log`).
    let stderr_enabled = cli.stderr_logs || cli.gui;
    lattice_runtime::set_boot_stderr_enabled(stderr_enabled);

    let document = match cli.file {
        Some(path) => {
            Document::open(&path).with_context(|| format!("opening {}", path.display()))?
        }
        None => Document::empty(),
    };

    // Phase 5.9 / 5.8.M: single-binary entry. `--gui` routes to the
    // GPUI peer (feature-gated by `gui`). `--tui` (or no flag) routes
    // to the TUI peer. After phase 5.last achieves full feature parity
    // the default flips to GUI; until then TUI stays the default.
    // `clap`'s `conflicts_with` enforces mutual exclusivity at parse
    // time.
    if cli.gui {
        run_gui(document)
    } else {
        lattice_ui_tui::run(document)
    }
}

/// Route to the GPUI peer. Feature-gated: the `gui` Cargo feature
/// pulls in `lattice-ui-gpui` (with its `window` feature) and
/// exposes the real entry. Without the feature, `--gui` produces a
/// helpful error so users know to rebuild with `--features gui`.
#[cfg(feature = "gui")]
fn run_gui(document: Document) -> Result<()> {
    lattice_ui_gpui::run(document)
}

#[cfg(not(feature = "gui"))]
fn run_gui(_document: Document) -> Result<()> {
    anyhow::bail!(
        "GUI renderer not compiled in. Rebuild with `cargo build --features gui` \
         (Linux: requires `libxcb1-dev libxkbcommon-dev libxkbcommon-x11-dev`)."
    )
}
