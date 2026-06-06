//! Boot / config-load / sync paths the runtime calls before the
//! main loop starts -- the App's once-per-launch infrastructure.
//!
//! Methods that live here:
//! - `App::new` (the once-per-launch constructor) and
//!   `build_lsp_subsystem` (its sub-helper).
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
use lattice_grammar::CommandRegistry;
use lattice_grammar::builtins::populate;
use lattice_lsp::{DiagnosticsLayer, LspLogger, LspSupervisor, LspSupervisorHandle};
use lattice_protocol::position::Position;
use lattice_runtime::{EventBus, spawn_document};
use lattice_syntax::{Lang, LangRegistry};

use std::collections::HashMap;
use std::sync::Arc;

use super::{
    App, BufferData, BufferEntry, BufferFlags, BufferId, BufferKind, DocumentEntry, EchoLevel,
};
use crate::pane::{PaneState, PaneTree};

/// Build a fresh LSP subsystem. Returns the supervisor wrapped
/// in `Arc<Mutex>` for App-side sharing, plus cloned handles
/// to the diagnostics layer + logger so the renderer's
/// per-frame reads can skip the supervisor lock.
/// Configure + spawn the LSP subsystem. The returned handle is
/// what the App holds for the editor's lifetime; reads are
/// wait-free against an `ArcSwap<SupervisorSnapshot>`, writes
/// route through the supervisor task's mailbox. The
/// `Arc<tokio::sync::Mutex<LspSupervisor>>` of the previous
/// shape is gone -- the UI thread can no longer take a
/// supervisor lock by accident (the audit's class-of-bug
/// finding from the LSP-edit refactor).
///
/// `event_bus` is wired in here (pre-spawn) so the supervisor
/// task is born already knowing about it; subsequent actor
/// spawns get their per-actor edit fan-in for free.
fn build_lsp_subsystem(
    event_bus: std::sync::Arc<lattice_runtime::EventBus>,
    runtime_handle: &tokio::runtime::Handle,
) -> (
    LspSupervisorHandle,
    DiagnosticsLayer,
    LspLogger,
    tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::InboundApplyEdit>,
    tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::InboundConfigurationRequest>,
    tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::InboundShowDocument>,
    tokio::sync::mpsc::UnboundedReceiver<lattice_lsp::InboundShowMessageRequest>,
) {
    let logger = LspLogger::with_defaults();
    let mut sup = LspSupervisor::new(logger.clone());
    // Builtin registry: rust-analyzer, pyright, gopls,
    // typescript-language-server, clangd, lua-language-server.
    sup.set_configs(lattice_lsp::builtin_servers());
    let diagnostics = sup.diagnostics().clone();
    // Apply-edit bus (Phase 4.3): App owns the receiver, every
    // actor spawned via the supervisor gets a clone of the
    // sender. Server-initiated `workspace/applyEdit` requests
    // ferry through this channel; the App's drain applies them
    // and replies via the embedded oneshot.
    let (apply_edit_bus, apply_edit_rx) = lattice_lsp::ApplyEditBus::new();
    sup.set_apply_edit_bus(apply_edit_bus);
    // Configuration bus (Phase 4.1 follow-up): same shape as
    // apply-edit. Server-initiated `workspace/configuration`
    // requests ferry through this channel; the App's drain
    // walks the cached TOML tree at `lsp.<section>` for each
    // requested item.
    let (configuration_bus, configuration_rx) = lattice_lsp::ConfigurationBus::new();
    sup.set_configuration_bus(configuration_bus);
    // 4.4.b: window/showDocument bus -- the App's drain opens
    // the requested URI and writes `{ success }` back via the
    // embedded oneshot.
    let (show_document_bus, show_document_rx) = lattice_lsp::ShowDocumentBus::new();
    sup.set_show_document_bus(show_document_bus);
    // 4.4.b: window/showMessageRequest bus -- the App's drain
    // opens a modal action picker; the user's selection (or
    // `None` on dismiss) ferries back via the oneshot.
    let (show_message_request_bus, show_message_request_rx) =
        lattice_lsp::ShowMessageRequestBus::new();
    sup.set_show_message_request_bus(show_message_request_bus);
    // Event bus is wired pre-spawn so every actor born in this
    // supervisor task gets its per-actor edit fan-in
    // automatically (lattice_lsp::fan_in).
    sup.set_event_bus(event_bus.clone());
    // Hand the explicit runtime handle to spawn the supervisor
    // task. `App::new` runs from `runtime::run` *before* the
    // editor's main loop has entered any tokio context, so
    // `tokio::runtime::Handle::try_current()` would (silently)
    // fail and the supervisor task would never start. The
    // explicit handle removes that footgun.
    let handle = sup.spawn(runtime_handle);
    // M-async.5: the attach driver is gone. `LspMode::on_activate`
    // now drives the supervisor's `open_buffer` directly (via the
    // `LspSupervisorHandle` it pulls from `ctx.service::<...>()`).
    // The mode owns its work; the App is no longer a coordinator
    // between `Event::DocumentOpened` and the LSP attach path.
    // The bus's `DocumentOpened` event still fires for other
    // subscribers (project watcher, completion warmer, ...);
    // LSP just doesn't key off it anymore.
    let _ = &event_bus;
    (
        handle,
        diagnostics,
        logger,
        apply_edit_rx,
        configuration_rx,
        show_document_rx,
        show_message_request_rx,
    )
}

/// Boot-time registration of the first-party picker sources
/// the `:picker <source>` ex-command dispatches to. Per slice
/// 13 of the picker design
/// (`docs/dev/architecture/picker.md`) every first-party
/// source is registered with its `PickerSourceGenerator` impl
/// so dispatch resolves through `gen.init()` /
/// `gen.accept()`. Feature-crate sources (LSP, snippet, ...)
/// register themselves through dedicated
/// `register_picker_sources` entry points once their
/// generator impls land.
fn built_in_picker_registry(
    command_registry: Arc<lattice_grammar::CommandRegistry>,
    config: Arc<lattice_config::ConfigRegistry>,
    snippet_registry: Arc<arc_swap::ArcSwap<lattice_snippet::SnippetRegistry>>,
) -> lattice_picker::PickerRegistry {
    let mut reg = lattice_picker::PickerRegistry::new();
    // Host-near first-party sources.
    for generator in crate::picker_sources::first_party_generators(command_registry, config) {
        reg.register_generator(generator);
    }
    // Feature-crate sources -- each crate owns its source impl
    // + a `register_picker_sources` entry point so feature
    // state stays where it lives. Cycle-free: lattice-picker
    // doesn't depend on lattice-snippet (or any feature
    // crate); the host wires the dependency direction here.
    lattice_snippet::picker_sources::register(&mut reg, snippet_registry);
    reg
}

impl App {
    pub fn new(document: Document) -> Self {
        // LSP subsystem: build once + extract shared handles so
        // the App's `lsp_diagnostics` / `lsp_logger` reads land
        // on the same Arc-shared state the supervisor's actors
        // push to.
        // §5.10 event bus. Built before `build_lsp_subsystem`
        // because the supervisor wires its per-actor edit fan-in
        // (lattice_lsp::fan_in) at spawn time using this bus, and
        // the post-spawn handle does not expose `set_event_bus`.
        let event_bus = Arc::new(EventBus::new());
        // Hand `build_lsp_subsystem` the canonical LSP runtime
        // handle so the supervisor task is spawned on a real
        // runtime even when `App::new` is called before
        // `runtime::run` has entered any tokio context. The
        // OnceLock lazily initialises the runtime on first call;
        // every later caller (the per-feature
        // `spawn_on_lsp_runtime` for hover / definition / etc.,
        // the attach driver, every test that exercises the LSP
        // write path) reuses the same instance.
        let runtime_handle = crate::runtime::lsp_runtime().handle().clone();
        let (
            lsp,
            lsp_diagnostics,
            lsp_logger,
            lsp_apply_edit_rx,
            lsp_configuration_rx,
            lsp_show_document_rx,
            lsp_show_message_request_rx,
        ) = build_lsp_subsystem(event_bus.clone(), &runtime_handle);
        let mut registry = CommandRegistry::new();
        let builtins = populate(&mut registry);
        // Register the built-in ex-commands as peers of motions /
        // operators / text objects (DESIGN.md §5.2.1). The returned
        // ids aren't held in App state today -- the parser front-end
        // looks them up by name -- but registering them populates the
        // registry so `:`-line parsing can route to them.
        let _ex_builtins = lattice_grammar::ex_commands::populate(&mut registry);
        // CSM.5: shared snippet-registry handle. Built before
        // the mode registry so `register_snippet_modes` can
        // capture a clone of the outer Arc -- the same outer
        // Arc the App field below holds. `:reload-snippets`
        // updates the inner via `.store()`; the mode + source
        // see the fresh data on the next produce().
        let snippet_registry_handle: std::sync::Arc<
            arc_swap::ArcSwap<lattice_snippet::SnippetRegistry>,
        > = std::sync::Arc::new(arc_swap::ArcSwap::from_pointee(
            lattice_snippet::SnippetRegistry::new(),
        ));
        // M.5.1 (mode-architecture §9.6.1): build the mode registry
        // first so we can iterate it and register a `:<mode-name>`
        // toggle ex-command per mode. The mode registry is then
        // wrapped in `Arc` for the App struct below.
        let mode_registry = {
            let mut mr = lattice_mode::ModeRegistry::new();
            lattice_mode::register_foundation_modes(&mut mr);
            lattice_syntax::register_language_modes(&mut mr);
            lattice_lsp::modes::register_lsp_log_modes(&mut mr);
            // CSM.8a: lsp-completion-mode needs the supervisor
            // handle (its contributed source captures it). The
            // existing `register_lsp_log_modes` is supervisor-
            // free; the completion mode rides on a separate
            // helper called here with the freshly-built `lsp`
            // handle from above.
            lattice_lsp::completion::register_lsp_completion_mode(&mut mr, lsp.clone());
            lattice_oil::register_oil_modes(&mut mr);
            lattice_file_tree::register_file_tree_modes(&mut mr);
            lattice_snippet::register_snippet_modes(&mut mr, snippet_registry_handle.clone());
            crate::modes::register_buffer_kind_modes(&mut mr);
            mr
        };
        register_mode_toggle_commands(&mut registry, &mode_registry);
        let mode_registry = std::sync::Arc::new(mode_registry);
        // App-side action registrations (slice 8.i; see
        // `docs/dev/notes/8i-approach.md`). Each `CommandKind::Action`
        // entry returns `Effect::AppAction(AppEffect::Foo)`;
        // per-mode keymap modules consume the resulting
        // `ActionIds` to build typed `CommandInvocation`s for
        // chord bindings as the legacy `bind_legacy` bridge
        // retires.
        let action_ids = crate::actions::populate(&mut registry, &builtins);
        // §5.11.3 completion pipeline: register the built-in
        // generators / matchers / rankers / annotators and wire
        // sensible defaults (prefix matcher, score ranker, kind +
        // doc annotators).
        let mut completion_registry = lattice_completion::CompletionRegistry::new();
        let _completion_builtins = lattice_completion::populate(&mut completion_registry);
        // Help-topic registry + its completion generator
        // (`gen:help-topics`). Registering here lets `:help <Tab>`
        // enumerate built-in + plugin-supplied topics through the
        // same pipeline `:e <Tab>` and `:describe-command <Tab>`
        // use.
        let help_topics = crate::help_topics::builtin_topics();
        completion_registry.register_generator(
            "gen:help-topics",
            "Every registered free-form help topic (`:help <topic>`).",
            crate::help_topics::HelpTopicsGenerator {
                topics: help_topics.clone(),
            },
        );
        // The §5.10 event bus is built above (before
        // build_lsp_subsystem so the supervisor task can wire
        // its per-actor fan-in pre-spawn). Subsequent setup just
        // attaches more subscribers to the same bus.
        // Subscribe the App's own cascade-handler channel to
        // `OptionChanged` events on the bus. The receiver lives
        // on `App.editor.option_change_rx`; `App::drain_option_changes`
        // pulls from it (called from the main loop + at the end
        // of `do_set`). This decouples cascades from the publish
        // path: any consumer that calls `config.set` -- the
        // cmdline, plugins, the future customize buffer view --
        // triggers the cascade through the same channel.
        let (option_tx, option_change_rx) = tokio::sync::mpsc::unbounded_channel();
        event_bus.subscribe(
            lattice_runtime::EventFilter::kind(lattice_protocol::EventKind::OptionChanged),
            lattice_runtime::SubscriptionTarget::Channel(option_tx),
        );
        // LSP log live-tail (Phase 4): every record the LspLogger
        // appends fires `LspLogPushed` (M.5.3.b: typed event in
        // `lattice-lsp::events`); the App's drain hook refreshes
        // any open `*lsp*` / `*lsp:<server>*` /
        // `*lsp:<server>:trace*` help buffer from the logger
        // snapshot so views update live as records arrive.
        let (lsp_log_tx, lsp_log_event_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspLogPushed>();
        event_bus.subscribe_typed(lsp_log_tx);
        // LSP work-done progress (4.4.c): the actor publishes
        // `LspProgressUpdate` whenever a server sends $/progress.
        // The App accumulates by (server_id, token) and surfaces
        // the most recent active entry in the modeline's LSP
        // segment.
        let (lsp_progress_tx, lsp_progress_event_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspProgressUpdate>();
        event_bus.subscribe_typed(lsp_progress_tx);
        // `LspBufferDetached` subscriber: `LspMode::on_deactivate`
        // publishes this event (Phase 2 of the mode-architecture
        // refactor). The App's per-tick drain calls
        // `lsp_close_buffer` for each event so the wire-level
        // `didClose` + `buffer_uris` cleanup runs *after* the
        // mode lifecycle, not from a hardcoded branch in
        // `deactivate_mode_by_id`. Same shape any future
        // renderer would use -- no special-casing per host.
        let (lsp_detach_tx, lsp_detach_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspBufferDetached>();
        event_bus.subscribe_typed(lsp_detach_tx);
        // M-async.3: mode lifecycle events. The dispatcher
        // publishes `ModeEvent::ModeActivationFailed` for every
        // mode that failed its `on_activate` (or got aborted by
        // a cascade parent's failure); the App's per-tick drain
        // calls `deactivate_mode_by_id` on each so `active_modes`
        // / `mode_guards` reflect the failed activation.
        let (mode_lifecycle_tx, mode_lifecycle_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_mode::ModeEvent>();
        event_bus.subscribe_typed(mode_lifecycle_tx);
        // 4.4.g: `workspace/inlayHint/refresh` subscriber.
        // Server-initiated cache invalidation -- the actor
        // replies `null` inline and publishes
        // `LspInlayHintRefresh`; the App's per-tick drain
        // clears `lsp_inlay_hints_cache` for buffers attached
        // to the requesting server, and the next render tick
        // refills them via `maybe_request_inlay_hint`.
        let (lsp_inlay_refresh_tx, lsp_inlay_refresh_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspInlayHintRefresh>();
        event_bus.subscribe_typed(lsp_inlay_refresh_tx);
        // 4.4.i: `workspace/semanticTokens/refresh` subscriber.
        // Same shape as the inlay-hint refresh: the actor
        // replies `null` inline and publishes
        // `LspSemanticTokensRefresh`; the App's per-tick drain
        // drops `lsp_semantic_tokens_cache` entries for buffers
        // attached to the requesting server, and the next
        // render tick refills them via
        // `maybe_request_semantic_tokens` (forced full path,
        // since the cached `result_id` is gone).
        let (lsp_semantic_tokens_refresh_tx, lsp_semantic_tokens_refresh_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspSemanticTokensRefresh>();
        event_bus.subscribe_typed(lsp_semantic_tokens_refresh_tx);
        // 4.4.j: `workspace/diagnostic/refresh` subscriber. The
        // actor publishes `LspDiagnosticRefresh` on inbound;
        // the App's drain evicts the per-buffer
        // `lsp_pull_diagnostics_cache` entries for attached
        // buffers so the next pump tick issues a fresh
        // `textDocument/diagnostic` (no `previous_result_id`)
        // and the server emits a `Full` report.
        let (lsp_diagnostic_refresh_tx, lsp_diagnostic_refresh_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspDiagnosticRefresh>();
        event_bus.subscribe_typed(lsp_diagnostic_refresh_tx);
        // 4.5.d: `workspace/codeLens/refresh` subscriber. Same
        // pattern as the other refresh events; the drain
        // evicts per-buffer code-lens caches for the
        // requesting server so the next pump tick refetches.
        let (lsp_code_lens_refresh_tx, lsp_code_lens_refresh_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_lsp::LspCodeLensRefresh>();
        event_bus.subscribe_typed(lsp_code_lens_refresh_tx);
        // `*messages*` buffer live-tail subscriber. Every
        // `set_message` publishes a `MessagePushed` event; the
        // App's per-tick drain coalesces and rebuilds the
        // buffer view if `*messages*` is open. Other consumers
        // (plugins, telemetry) can subscribe to the same event
        // via `event_bus.subscribe_typed::<MessagePushed>(...)`.
        let (message_event_tx, message_event_rx) =
            tokio::sync::mpsc::unbounded_channel::<lattice_runtime::MessagePushed>();
        event_bus.subscribe_typed(message_event_tx);
        // msg-mode.1: install the global `tracing::Subscriber`
        // bridge. The `MessagesLayer` captures every
        // `tracing::*` event into this shared `MessagesRing` +
        // publishes `MessagePushed` on the event bus. From
        // here on, `App::set_message` and any
        // `tracing::info!()` / `warn!()` / etc. call anywhere
        // in the editor (or plugins via the WIT bridge in v1+)
        // flows into `*messages*` through one bridge.
        //
        // `install_messages_subscriber` is idempotent: only the
        // first call succeeds (`set_global_default` is
        // process-wide); test setups with multiple App
        // instances share the first-installed layer (the
        // ring/bus from the first App), which is fine because
        // tests use the per-test layer constructor for unit
        // coverage rather than relying on the global install.
        let messages_ring = std::sync::Arc::new(std::sync::Mutex::new(
            crate::app::MessagesRing::default(),
        ));
        // msg-mode.2: install with the typed-option's documented
        // default (`"info"`). The user's TOML override (if any)
        // is applied via `reload_messages_filter` from the
        // option-change cascade once the config layer has
        // loaded.
        let _ = lattice_runtime::install_messages_subscriber(
            messages_ring.clone(),
            event_bus.clone(),
            "info",
        );
        // Wire the logger's publisher to the same bus. The
        // logger lives in `lattice-lsp`; the closure captures an
        // Arc<EventBus> clone so the logger's lifetime is
        // independent of any single App field.
        let bus_for_log = event_bus.clone();
        lsp_logger.set_event_publisher(std::sync::Arc::new(move |event| {
            bus_for_log.publish_typed(event);
        }));
        // Typed-options registry (DESIGN.md §5.12). Single source
        // of truth for every option's *current value*: each
        // `Option<T>` owns a wait-free `ArcSwap<T>` cell that
        // `:set` parses into, hot-path readers load from, and the
        // (future) customize buffer view edits through. Renderer-
        // agnostic options register from `lattice-config`; this
        // renderer's own options register from `crate::tui_options`.
        let config = Arc::new(lattice_config::ConfigRegistry::new());
        // Wire the registry's `OptionChanged` publisher to the
        // event bus (§5.10 + §5.12 unification). Subscribers see
        // every typed-option change as `Event::OptionChanged`
        // instead of having to poll. The closure captures an
        // Arc<EventBus> clone so the registry's lifetime is
        // independent of any single App field.
        let bus_for_publisher = event_bus.clone();
        config.set_event_publisher(std::sync::Arc::new(move |event| {
            bus_for_publisher.publish(event);
        }));
        // M.2.0c: every option (core + TUI-specific) self-
        // registers via the proc-macro-emitted `register_fn`
        // thunks aggregated in `OPTION_DECLS`. One
        // `init_from_linkme()` call boots them all; idempotent
        // if called again.
        config.init_from_linkme();
        // 4.4.o: seed the LSP logger from typed-options
        // defaults. `lsp.log_level` + `lsp.log_capacity` set
        // the boot values; the runtime `:lsp-log-level` /
        // `:lsp-trace` paths still adjust live. Invalid
        // values were filtered by the option validators
        // before they hit the registry; we treat any miss
        // here as "use the built-in default the logger
        // already has."
        if let Some(level_str) = config.get_typed::<lattice_config::core_options::LspLogLevel>()
            && let Some(level) = lattice_lsp::LogLevel::parse(&level_str)
        {
            lsp_logger.set_default_level(level);
        }
        if let Some(cap) = config.get_typed::<lattice_config::core_options::LspLogCapacity>() {
            lsp_logger.set_default_capacity((*cap).max(0) as usize);
        }
        // `gen:options` -- completion source for `:set <Tab>` and
        // `:set name=<Tab>`. Wired to the same `ConfigRegistry` the
        // `:set` parser consults so completions never drift from
        // the canonical option list.
        completion_registry.register_generator(
            "gen:options",
            "Every registered option name + (when applicable) its enumerated values.",
            lattice_config::OptionsGenerator::new(config.clone()),
        );
        // App-state completion sources for the various `:describe-*`
        // / `:customize` / `:lsp-*` commands. Each generator captures
        // the slice of state it needs; names are stable so
        // `ArgSpec::completion` references stay in sync.
        completion_registry.register_generator(
            "gen:modes",
            "Every registered mode (major + minor); used by `:describe-mode <Tab>`.",
            crate::host_generators::ModesGenerator {
                registry: Arc::downgrade(&mode_registry),
            },
        );
        completion_registry.register_generator(
            "gen:events",
            "Every typed event registered via `register_event!`; used by `:describe-event <Tab>`.",
            crate::host_generators::EventsGenerator,
        );
        completion_registry.register_generator(
            "gen:log-levels",
            "The five log levels (`error`/`warn`/`info`/`debug`/`trace`); used by `:lsp-log-level <Tab>`.",
            crate::host_generators::LogLevelsGenerator,
        );
        completion_registry.register_generator(
            "gen:lsp-servers",
            "Currently running LSP server ids; used by `:lsp-log <Tab>` / `:lsp-restart <Tab>` / etc.",
            crate::host_generators::LspServersGenerator { lsp: lsp.clone() },
        );
        completion_registry.register_generator(
            "gen:customize",
            "Group names + mode names; used by `:customize <Tab>`.",
            crate::host_generators::CustomizeNamesGenerator {
                registry: Arc::downgrade(&mode_registry),
            },
        );
        // Picker source registry: built here so the
        // `gen:picker-sources` completion generator can capture a
        // Weak handle that survives every subsequent boot step.
        // Feature crates (lattice-lsp, lattice-snippet, ...) will
        // eventually register their sources through dedicated
        // entry points before this point; slice 13 of the picker
        // design adds the trait that makes that possible.
        //
        // Wrap the command registry in `Arc` early so picker
        // sources that capture it (CommandsSource) can clone
        // here. The `let registry = Arc::new(registry);` below
        // -- now redundant after this move -- is the original
        // wrap-point preserved for downstream reuse.
        let registry = Arc::new(registry);
        let picker_registry: Arc<lattice_picker::PickerRegistry> =
            Arc::new(built_in_picker_registry(
                registry.clone(),
                config.clone(),
                snippet_registry_handle.clone(),
            ));
        // MRU cache load. Failure modes:
        //   - `default_persist_path` returns None: persistence
        //     disabled (sandboxed run); start with empty index.
        //   - Path resolves but file missing: fresh install;
        //     start with empty index.
        //   - File present but undecodable / version mismatch:
        //     log a warning and start fresh (losing MRU is
        //     annoying, refusing to boot is worse).
        // Honor `picker.mru.persist` at boot: when disabled,
        // skip the load step entirely (start fresh) and
        // clear the path so subsequent saves no-op.
        let persist = config
            .get_typed::<lattice_config::core_options::PickerMruPersist>()
            .map(|b| *b)
            .unwrap_or(true);
        let picker_mru_path = if persist {
            lattice_picker::default_persist_path()
        } else {
            None
        };
        // Read the cap from config so users who want
        // long-tail history (or a leaner cache) can tune.
        let mru_cap = config
            .get_typed::<lattice_config::core_options::PickerMruCapPerNamespace>()
            .map(|n| (*n).max(1) as usize)
            .unwrap_or(lattice_picker::DEFAULT_CAP_PER_NAMESPACE);
        let picker_mru = match &picker_mru_path {
            Some(path) => match lattice_picker::PickerMruIndex::load_from(path) {
                Ok(Some(idx)) => {
                    // Loaded indexes carry their own cap; we
                    // don't override mid-life. Future:
                    // re-cap on config change via the option
                    // cascade.
                    idx
                }
                Ok(None) => lattice_picker::PickerMruIndex::with_cap(mru_cap),
                Err(e) => {
                    eprintln!(
                        "lattice: discarding corrupt MRU cache at {}: {e}",
                        path.display(),
                    );
                    lattice_picker::PickerMruIndex::with_cap(mru_cap)
                }
            },
            None => lattice_picker::PickerMruIndex::with_cap(mru_cap),
        };
        completion_registry.register_generator(
            "gen:picker-sources",
            "Every source id registered with the `PickerRegistry`; \
             drives `:picker <Tab>` completion.",
            crate::host_generators::PickerSourcesGenerator {
                registry: Arc::downgrade(&picker_registry),
            },
        );
        // One `LangRegistry` per App, shared between the document
        // buffer's `Syntax` and every `HelpBuffer` we'll spin up
        // for `:describe-*` / `:apropos` / `:keymap` (markdown
        // highlighted with fenced-block language injection).
        let lang_registry = LangRegistry::standard().expect("standard lang registry");
        let lang = Lang::detect_from_path(document.path());
        // Build the underlying `Syntax` synchronously + seed it
        // with one parse of the initial text so the renderer's
        // first frame has highlights without waiting for the
        // worker. After that the handle takes over: subsequent
        // `request_reparse` calls run the parse on a worker
        // thread; the renderer reads the latest snapshot via
        // `ArcSwap`. (Audit slice 3.)
        let initial_text = document.text();
        let initial_text_version = document.text_version();
        // Production: pass the explicit LSP runtime handle so the
        // syntax worker actually starts. `SyntaxHandle::seeded` fell
        // back to `Handle::try_current()` which silently fails when
        // App::new runs before the main loop enters tokio context --
        // the worker would never spawn and Option B's incremental
        // reparse pipeline would be entirely dead. Same shape as
        // the LSP supervisor handle above.
        let syntax: Option<lattice_syntax::SyntaxHandle> =
            match lattice_syntax::Syntax::for_language_with_registry(lang, lang_registry.clone()) {
                Ok(Some(mut s)) => {
                    s.parse_at(&initial_text, initial_text_version);
                    Some(lattice_syntax::SyntaxHandle::seeded_with_runtime(
                        s,
                        &runtime_handle,
                    ))
                }
                _ => None,
            };
        let last_parsed_text_version = initial_text_version;
        // Hand the document to the actor (DESIGN.md §5.7). After
        // this call the only way to read or mutate it is through
        // the returned `DocumentHandle` -- the App holds no other
        // reference. The registry is already `Arc<...>` (wrapped
        // earlier so picker sources that capture it could clone
        // their share); we just hand the actor a clone here.
        let document = spawn_document(document, registry.clone());
        let snapshot_cache = document.snapshot_cache();
        let document_buffer_id = BufferId::next();
        let initial_pane = PaneState {
            id: crate::pane::PaneId::next(),
            buffer: BufferKind::Document,
            buffer_id: document_buffer_id,
            cursor: Position::ZERO,
            scroll: 0,
        };
        let pane_tree = PaneTree::single(initial_pane);
        // Seed the buffer registry with the initial document. The
        // hot-path `self.document` / `self.syntax` /
        // `self.last_parsed_text_version` mirror what's stored
        // here for the active buffer; switching buffers swaps
        // them.
        let buffers = super::BufferRegistry::new();
        buffers.insert(BufferEntry {
            id: document_buffer_id,
            flags: BufferFlags::default(),
            data: BufferData::Document(DocumentEntry {
                id: document_buffer_id,
                handle: document.clone(),
            }),
            name: None,
        });
        // M.3.2.c.4: seed the initial document's buffer-locals so
        // reader-side flips can route through the locals map for
        // inactive buffers uniformly. The active buffer's hot-
        // path fields (App.syntax / App.folds / ...) are still
        // canonical until the readers flip; locals are updated at
        // each de-activation boundary via
        // `seed_document_entry_locals`.
        let mut buffer_locals: HashMap<super::BufferId, lattice_mode::BufferLocals> =
            HashMap::new();
        let mut initial_locals = lattice_mode::BufferLocals::default();
        initial_locals.insert(crate::modes::DocumentSyntax(None));
        initial_locals.insert(crate::modes::DocumentLastParsedTextVersion(0));
        initial_locals.insert(crate::modes::DocumentLastSyncedSyntaxVersion(0));
        initial_locals.insert(crate::modes::DocumentFolds(Vec::new()));
        buffer_locals.insert(document_buffer_id, initial_locals);
        // B'.3: keep a clone of `buffers` outside the Self
        // initialiser so the service-registry block below can
        // also hold one (BufferRegistry is `Clone` via Arc).
        let buffers_for_services = buffers.clone();
        let mut app = Self {
            editor: lattice_host::editor::Editor {
                messages: messages_ring.clone(),
                pending_message_event_rx: Some(message_event_rx),
                option_change_rx: Some(option_change_rx),
                lang_registry: lang_registry.clone(),
                syntax,
                last_parsed_text_version,
                picker_registry: picker_registry.clone(),
                picker_mru,
                picker_mru_path,
                config,
                mode_registry,
                services: {
                    let mut s = lattice_mode::ServiceRegistry::new();
                    s.register(lsp.clone());
                    let store: std::sync::Arc<dyn lattice_mode::BufferStore> =
                        std::sync::Arc::new(buffers_for_services);
                    s.register(lattice_mode::BufferStoreHandle::new(store));
                    s.register(lsp_logger.clone());
                    std::sync::Arc::new(s)
                },
                buffer_locals,
                help_topics,
                registry,
                event_bus: event_bus.clone(),
                builtins,
                action_ids,
                keymap: {
                    let h = crate::keymap_registry::KeymapHandle::new();
                    crate::keymap_replace::register_replace_bindings(&h, &action_ids);
                    crate::keymap_visual::register_visual_bindings(&h, &builtins, &action_ids);
                    crate::keymap_insert::register_insert_bindings(&h, &action_ids);
                    crate::keymap_normal::register_normal_bindings(&h, &builtins, &action_ids);
                    h
                },
                document_buffer_id,
                buffers,
                active_buffer: BufferKind::Document,
                viewport_height: 1,
                lsp,
                lsp_diagnostics,
                lsp_logger,
                lsp_log_event_rx: Some(lsp_log_event_rx),
                lsp_progress_event_rx: Some(lsp_progress_event_rx),
                pending_apply_edit_rx: Some(lsp_apply_edit_rx),
                pending_configuration_rx: Some(lsp_configuration_rx),
                pending_show_document_rx: Some(lsp_show_document_rx),
                pending_show_message_request_rx: Some(lsp_show_message_request_rx),
                pending_lsp_detach_rx: Some(lsp_detach_rx),
                pending_mode_lifecycle_rx: Some(mode_lifecycle_rx),
                pending_inlay_hint_refresh_rx: Some(lsp_inlay_refresh_rx),
                pending_semantic_tokens_refresh_rx: Some(lsp_semantic_tokens_refresh_rx),
                pending_code_lens_refresh_rx: Some(lsp_code_lens_refresh_rx),
                pending_diagnostic_refresh_rx: Some(lsp_diagnostic_refresh_rx),
                ..lattice_host::editor::Editor::default()
            },
            document,
            snapshot_cache,
            pane_tree,
            visible_highlights_key: None,
            folds: Vec::new(),
            pane_render_registry: crate::render::build_pane_render_registry(),
            theme: crate::theme::Theme::default(),
            popup_back_stack: Vec::new(),
            completion_registry,
            completion_state: None,
            insert_completion: None,
            snippet_registry: snippet_registry_handle,
            insert_completion_snippet_meta: Vec::new(),
            completion_accept_freq: std::collections::HashMap::new(),
            pending_config_structural_sections: std::collections::BTreeMap::new(),
            per_language_completion: lattice_completion::per_language_defaults(),
            completion_in_path_context: false,
            active_snippet: None,
            snippet_dirs: Vec::new(),
            lsp_file_watcher: None,
        };
        // Sync derived theme styles from the freshly-registered
        // ui.* options so the renderer's first frame uses the
        // configured colors / separator (rather than the static
        // Theme::default values).
        app.sync_theme_from_config();
        // Populate the hot-path option cache from canonical config
        // values. Subsequent updates flow through the
        // `Event::OptionChanged` cascade in
        // `apply_option_cascade`.
        app.rebuild_option_cache();
        // M.3.1: activate the resolved major mode for the
        // initial document buffer. `resolve_major_mode(kind,
        // lang)` picks the right major (text-mode for
        // Lang::Plain, rust-mode/python-mode/... for typed
        // languages). The activation populates
        // `active_modes[buffer]` and triggers the option-cache
        // recompute so `ResolvedOptions` reflects the major's
        // contributions (e.g. ReadOnly = true for Help).
        app.activate_major_for_buffer_kind(app.editor.document_buffer_id, BufferKind::Document);
        // Initial-document attach. Path-bearing buffers register
        // their URI eagerly (the URI is a deterministic
        // `uri_from_path`; LSP attach is async and doesn't gate
        // the mapping) and publish `Event::DocumentOpened` -- the
        // attach driver wired in `build_lsp_subsystem` consumes
        // it and submits to the supervisor on the LSP runtime,
        // off the UI thread. Path-less scratch buffers publish
        // nothing (no LSP work to drive) and the `buffer_uris`
        // entry stays absent.
        app.publish_document_opened_for_active();
        // Slice B / B'.7: LSP subsystem creates its global `*lsp*`
        // Document buffer eagerly at boot so `:b *lsp*` works
        // before any record has flowed. Per-instance buffers
        // (`*lsp:<server>:<workspace>*`,
        // `*lsp:<server>:<workspace>:trace*`) are created lazily
        // by the ex-command handlers (or by `:lsp-trace` toggle-on)
        // through the same generic `ensure_named_synthetic_document`
        // entry point. The name + mode-id come from `lattice-lsp`;
        // the host adds no subsystem-specific create logic.
        app.ensure_named_synthetic_document(
            lattice_lsp::LSP_SUBSYSTEM_LOG_NAME,
            lattice_lsp::modes::LspLogMode::mode_id(),
            crate::app::App::SYNTHETIC_BUFFER_FLAGS,
        );
        // Slice E: `*messages*` follows the same pattern -- a
        // Document buffer in the registry, read-only, owner-
        // streamed via the MessagePushed event drain. Eager at
        // boot so `:b *messages*` works from t=0.
        app.ensure_messages_buffer();
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
        let want_popup = self.insert_completion.is_some();
        let want_snippet = self.active_snippet.is_some();
        let have_popup = self.editor.completion_popup_layer.is_some();
        let have_snippet = self.editor.snippet_layer.is_some();
        // CSM.K1: `completion-popup-mode` minor reflects popup
        // state (formerly `completion-mode` in CSM.2 -- renamed
        // for the two-mode split where `completion-mode` is now
        // the persistent buffer-participation gate). The
        // keymap-overlay push / pop is the same diff applied to
        // the keymap-registry side; reconcile both here so the
        // two stay in lockstep.
        self.sync_completion_popup_mode_activation(want_popup);
        if want_popup == have_popup && want_snippet == have_snippet {
            return;
        }
        // Re-stack: pop everything, then push in the canonical
        // order (snippet first, popup second).
        if let Some(id) = self.editor.completion_popup_layer.take() {
            self.editor.keymap.pop_layer(id);
        }
        if let Some(id) = self.editor.snippet_layer.take() {
            self.editor.keymap.pop_layer(id);
        }
        if want_snippet {
            let id = self.editor.keymap.push_layer(
                crate::keymap_registry::PushLayerKind::MinorMode,
                "active-snippet",
                crate::keymap_insert::active_snippet_layer_bindings(&self.editor.action_ids),
            );
            self.editor.snippet_layer = Some(id);
        }
        if want_popup {
            let id = self.editor.keymap.push_layer(
                crate::keymap_registry::PushLayerKind::MinorMode,
                "completion-popup",
                crate::keymap_insert::completion_popup_layer_bindings(&self.editor.action_ids),
            );
            self.editor.completion_popup_layer = Some(id);
        }
    }

    /// CSM.K1: bring `completion-popup-mode`'s activation state
    /// on the active document buffer in line with `want_popup`.
    /// Called from `sync_keymap_overlays` so the transient
    /// popup-mode tracks the popup open / close transitions
    /// without each `self.insert_completion = ...` site having
    /// to know about it.
    ///
    /// Per-buffer scope: the popup belongs to the document the
    /// user is typing in. v1 has a single document buffer
    /// (`self.editor.document_buffer_id`); multi-document support
    /// activates this mode on whichever doc owns the popup at
    /// open time when that lands. Deactivation is symmetric.
    fn sync_completion_popup_mode_activation(&mut self, want_popup: bool) {
        let buffer_id = self.editor.document_buffer_id;
        let proto_id = lattice_protocol::ids::BufferId::new(buffer_id.0 as u64);
        let mode_id = lattice_mode::CompletionPopupMode::mode_id();
        let mut active = self.editor.active_modes.remove(&buffer_id).unwrap_or_default();
        let currently = active.has_minor(mode_id);
        if want_popup && !currently {
            let _ = self.editor.mode_registry.activate_minor(
                &mut active,
                &self.editor.mode_guards,
                &self.editor.config,
                &self.editor.event_bus,
                &self.editor.services,
                proto_id,
                mode_id,
                lattice_mode::CapabilitySet::empty(),
            );
        } else if !want_popup && currently {
            let _ = self.editor.mode_registry.deactivate_minor(
                &mut active,
                &self.editor.mode_guards,
                &self.editor.event_bus,
                proto_id,
                mode_id,
            );
        }
        self.editor.active_modes.insert(buffer_id, active);
        // CSM.3: a transition into / out of completion-popup-mode
        // is a mode-set change for the buffer -- recompute the
        // active-sources cache so the engine reads a coherent
        // snapshot. (completion-popup-mode itself doesn't
        // contribute sources; the recompute walks all active
        // modes, so future source-contributing minors that
        // toggle alongside still get picked up.)
        self.recompute_active_completion_sources_for(buffer_id);
    }

    /// Re-derive `App.theme`'s renderer-specific `Style` values
    /// from the current `ui.*` option values in the config. Called
    /// at App-init time (after registration) and on every `:set
    /// ui.*` so the cached theme stays in lockstep with the
    /// canonical primitives in config.
    pub fn sync_theme_from_config(&mut self) {
        use crate::tui_options::{
            UiDimInactive, UiNerdFonts, UiSeparator, UiSeparatorColor, UiStatuslineActiveFg,
            UiStatuslineInactiveFg,
        };
        use lattice_host::ui::theme as host_theme;
        // Phase 5.3: write `self.editor.host_theme` first (the
        // canonical neutral state); then rebuild `self.theme`
        // from it via the From<&host::Theme> adapter. Every
        // user-tweakable field flows through host first so a
        // future GPUI renderer that reads from `host_theme`
        // sees the same values the TUI cache derives from.
        let dim_inactive = *self
            .editor.config
            .get_typed::<UiDimInactive>()
            .expect("UiDimInactive");
        let nerd_fonts = *self.editor.config.get_typed::<UiNerdFonts>().expect("UiNerdFonts");
        let sep = self.editor.config.get_typed::<UiSeparator>().expect("UiSeparator");
        let sep_char = sep.chars().next().unwrap_or('│');
        self.editor.host_theme.dim_inactive_panes = dim_inactive;
        self.editor.host_theme.nerd_fonts = nerd_fonts;
        self.editor.host_theme.pane_separator_vertical = sep_char;
        // ui.separator_color -- color name; host parser returned
        // Ok during validate so unwrap-via-fallback is safe.
        let sep_color = self
            .editor.config
            .get_typed::<UiSeparatorColor>()
            .expect("UiSeparatorColor");
        if let Ok(c) = host_theme::parse_color(&sep_color) {
            self.editor.host_theme.pane_separator = host_theme::Style::empty().fg(c);
        }
        // ui.statusline_active_fg / _inactive_fg -- foreground
        // only; preserve modifiers / background by chaining `.fg(c)`
        // on the current host style (which itself preserves the
        // host-side modifier set).
        let active_fg = self
            .editor.config
            .get_typed::<UiStatuslineActiveFg>()
            .expect("UiStatuslineActiveFg");
        if let Ok(c) = host_theme::parse_color(&active_fg) {
            self.editor.host_theme.pane_status_active.fg = Some(c);
        }
        let inactive_fg = self
            .editor.config
            .get_typed::<UiStatuslineInactiveFg>()
            .expect("UiStatuslineInactiveFg");
        if let Ok(c) = host_theme::parse_color(&inactive_fg) {
            self.editor.host_theme.pane_status_inactive.fg = Some(c);
        }
        // Rebuild the cached TUI-typed adapter. Cheap (every
        // field is Copy); the rebuild fires only on option
        // cascade, not per frame.
        self.theme = crate::theme::Theme::from(&self.editor.host_theme);
    }

    /// Load `~/.editor.config/lattice/lattice.toml` (user) and
    /// `<workspace_root>/.lattice/config.toml` (project) in
    /// precedence order, applying scalar overrides to
    /// `self.editor.config` and bucketing structural sub-tables (per-
    /// language overrides, plugin sections) into
    /// `self.pending_config_structural_sections` for their
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
        // The structural prefixes the App / future plugin host
        // own. The per-language layer drains
        // `completion.per-language.*`; the plugin host (Phase 7)
        // will drain `plugin.*`; `lsp` is bucketed so the
        // loader doesn't fire unknown-option warnings for
        // server-namespaced keys (the cached raw_tree carries
        // the values; `workspace/configuration` walks it).
        let prefixes = ["completion.per-language", "plugin", "lsp"];
        let outcome = lattice_config::load_default_paths(&self.editor.config, workspace_root, &prefixes);
        // Re-derive theme + hot-path option cache after the
        // loader's writes. ui.* and the cached options may have
        // changed; missing this would leave the first frame
        // rendering with stale derived state.
        self.sync_theme_from_config();
        self.rebuild_option_cache();
        // Stash structural sections for the layers that own
        // them. Subsequent slices drain via
        // `take_pending_structural_section(prefix)`.
        for (k, v) in outcome.structural {
            self.pending_config_structural_sections.insert(k, v);
        }
        // Cache the merged TOML tree so
        // `workspace/configuration` can walk server-namespaced
        // keys (Phase 4.1 follow-up). Project files override
        // user files at deep-merge time so an `[lsp.X.Y]`
        // sibling key in the user config survives a project
        // override of `[lsp.X.Z]`.
        self.editor.lsp_config_tree = outcome.raw_tree;
        // Apply editor-side LSP options that live in the same
        // `[lsp]` table as server-namespaced keys. These are scalars
        // the editor consumes itself (not forwarded via
        // `workspace/configuration`); the loader buckets the whole
        // `lsp` subtree as structural so they're reachable here via
        // `lsp_config_tree`.
        self.apply_persistent_lsp_editor_options();
        // Surface a single echo summarising loader diagnostics.
        // The renderer's modeline only shows the latest echo,
        // so multi-warn configs collapse into "<count> issues
        // (first: <body>)". Severity is the max across the run.
        if outcome.messages.is_empty() {
            return;
        }
        let max_level = outcome
            .messages
            .iter()
            .map(|m| m.level)
            .max_by_key(|l| match l {
                lattice_config::LoadMessageLevel::Error => 1,
                lattice_config::LoadMessageLevel::Warning => 0,
            })
            .unwrap_or(lattice_config::LoadMessageLevel::Warning);
        let echo_level = match max_level {
            lattice_config::LoadMessageLevel::Error => EchoLevel::Error,
            lattice_config::LoadMessageLevel::Warning => EchoLevel::Warn,
        };
        let count = outcome.messages.len();
        let first = &outcome.messages[0];
        let body = if count == 1 {
            format!("config: {}: {}", first.source.display(), first.body,)
        } else {
            format!(
                "config: {count} issues (first: {}: {})",
                first.source.display(),
                first.body,
            )
        };
        self.set_message(echo_level, body);
    }
}

/// M.5.1 (mode-architecture §9.6.1): register a `:<mode-name>`
/// toggle ex-command for every mode in `mode_registry`. The
/// command id is the mode name (no `ex:` prefix; the ex-command
/// resolver tries direct registry-name lookup before alias
/// expansion, so `:lsp-mode` resolves directly).
///
/// Toggle apply-fn returns
/// [`lattice_grammar::Effect::ToggleMode { mode_name }`]; the
/// App's effect dispatcher routes that to
/// [`crate::app::App::toggle_mode_by_name`].
fn register_mode_toggle_commands(
    cmd_registry: &mut CommandRegistry,
    mode_registry: &lattice_mode::ModeRegistry,
) {
    use lattice_grammar::args::ArgSpec;
    use lattice_grammar::registry::{ExCommandSpec, SurfaceForm};
    use lattice_grammar::{Args, CommandError, Effect};
    let mut names: Vec<String> = mode_registry
        .iter_meta()
        .map(|(id, _kind)| id.to_string())
        .collect();
    // Sort for deterministic registration order (HashMap iteration
    // is hash-randomized; deterministic boot keeps `:describe-*`
    // and tests stable).
    names.sort();
    for name in names {
        let mode_name = name.clone();
        let cmd_name = name.clone();
        cmd_registry.register_ex_command(
            &cmd_name,
            "Toggle the mode on the active buffer (auto-generated; \
             see `:help modes` for the full mode-system overview).",
            ExCommandSpec {
                latency_class: lattice_grammar::command::LatencyClass::Reflex,
                accepts_bang: false,
                accepts_range: false,
                parse_args: Box::new(|s: &str, _bang: bool| {
                    if s.trim().is_empty() {
                        Ok(Args::None)
                    } else {
                        Err(CommandError::BadArgs(
                            "mode toggle takes no arguments".into(),
                        ))
                    }
                }),
                apply: Box::new(move |_ctx| {
                    Ok(Effect::ToggleMode {
                        mode_name: mode_name.clone(),
                    })
                }),
                args_schema: Vec::<ArgSpec>::new(),
                surface_form: SurfaceForm::Keyword,
            },
        );
    }
}
