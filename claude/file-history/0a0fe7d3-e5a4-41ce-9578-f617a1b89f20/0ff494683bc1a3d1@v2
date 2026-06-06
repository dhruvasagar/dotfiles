//! Built-in ex-commands registered as peers of motions / operators / text
//! objects in the unified `CommandRegistry` (DESIGN.md §5.2.1).
//!
//! Each spec's `apply` callback is intentionally thin: it only packages
//! the parsed args into the matching [`Effect`] variant. The host (`App`)
//! owns the side-effect implementation (file I/O, view options, echo
//! area, document swap, ...) -- this keeps the closures static-state-free
//! so they can later be loaded from a WASM plugin without redesign.
//!
//! Coverage:
//! - Keyword form: `:w[rite]`, `:q[uit]`, `:wq`/`:x`, `:noh[lsearch]`,
//!   `:reg[isters]`, `:marks`, `:d[elete]`, `:set`, `:e[dit]`.
//! - Delimiter-syntax form (Appendix B.2): `:s/.../.../[g]`,
//!   `:%s/.../.../[g]`, `:g/.../.../`, `:v/.../.../`. These use
//!   `Args::List` to carry pattern / replacement / flags / body /
//!   inverted as positional `ArgValue`s; the parser front-end strips
//!   the delimiter prefix and dispatches through the same
//!   `grammar::execute()` as everything else.
//!
//! Aliases (`:w` for `:write`, `:q` for `:quit`, `:e` for `:edit`, ...)
//! are NOT separate registry entries -- they would inflate the
//! `CommandId` namespace and complicate `:describe-command`. Alias
//! resolution is the parser front-end's job (`expand_alias` in
//! `lattice-ui-tui::excommand`).

use crate::args::{ArgDefault, ArgKind, ArgSpec, ArgValue, Args};
use crate::command::LatencyClass;
use crate::effect::{Effect, SubstituteScope};
use crate::error::{CommandError, GrammarResult};
use crate::range::Range;
use crate::registry::{CommandRegistry, ExCommandContext, ExCommandId, ExCommandSpec, SurfaceForm};

/// Set of registered ex-command ids; mirrors the `Builtins` shape for
/// motions / operators / text objects.
#[derive(Debug, Clone, Copy)]
pub struct ExBuiltins {
    pub write: ExCommandId,
    pub quit: ExCommandId,
    pub write_quit: ExCommandId,
    pub no_hlsearch: ExCommandId,
    pub list_registers: ExCommandId,
    pub list_marks: ExCommandId,
    pub delete_line: ExCommandId,
    pub set_option: ExCommandId,
    pub edit: ExCommandId,
    pub substitute: ExCommandId,
    pub global: ExCommandId,
    pub describe_command: ExCommandId,
    pub describe_buffer: ExCommandId,
    pub apropos: ExCommandId,
    pub describe_key: ExCommandId,
    pub list_keymap: ExCommandId,
    pub buffer_next: ExCommandId,
    pub buffer_prev: ExCommandId,
    pub list_buffers: ExCommandId,
    pub buffer_delete: ExCommandId,
    pub file_tree: ExCommandId,
    pub file_tree_close: ExCommandId,
    pub oil: ExCommandId,
    pub describe_option: ExCommandId,
    pub list_options: ExCommandId,
    pub hover: ExCommandId,
    pub hover_close: ExCommandId,
    pub help: ExCommandId,
    pub list_diagnostics: ExCommandId,
    pub next_diagnostic: ExCommandId,
    pub prev_diagnostic: ExCommandId,
    pub lsp_log: ExCommandId,
    pub lsp_trace: ExCommandId,
    pub lsp_status: ExCommandId,
    pub lsp_server_log: ExCommandId,
    pub lsp_restart: ExCommandId,
    pub lsp_log_level: ExCommandId,
    pub lsp_log_clear: ExCommandId,
    pub lsp_symbols: ExCommandId,
    pub lsp_workspace_symbol: ExCommandId,
    pub lsp_format: ExCommandId,
    pub lsp_format_range: ExCommandId,
    pub lsp_signature_help: ExCommandId,
    pub lsp_complete: ExCommandId,
    pub lsp_rename: ExCommandId,
    pub lsp_code_action: ExCommandId,
    pub snippet_expand: ExCommandId,
    pub reload_snippets: ExCommandId,
}

pub fn populate(registry: &mut CommandRegistry) -> ExBuiltins {
    let write = registry.register_ex_command(
        "ex:write",
        "Write the current buffer to disk (`:w [path]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(apply_write),
            args_schema: vec![ArgSpec {
                name: "path",
                kind: ArgKind::String,
                doc: "Destination path. Absent = overwrite current file.",
                prompt: "path:",
                default: ArgDefault::None,
                completion: Some("gen:files"),
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let quit = registry.register_ex_command(
        "ex:quit",
        "Quit the editor (`:q[!]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: true,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(apply_quit),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let write_quit = registry.register_ex_command(
        "ex:write-quit",
        "Write the current buffer and quit (`:wq[!]` / `:x[!]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: true,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(apply_write_quit),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let no_hlsearch = registry.register_ex_command(
        "ex:nohlsearch",
        "Clear the search-highlight overlay (`:noh[lsearch]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::ClearSearchHighlight)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let list_registers = registry.register_ex_command(
        "ex:registers",
        "Show every register's contents (`:reg[isters]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::EchoRegisters)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let list_marks = registry.register_ex_command(
        "ex:marks",
        "Show every set mark's name + position (`:marks`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::EchoMarks)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let delete_line = registry.register_ex_command(
        "ex:delete",
        "Delete the current line including its newline (`:d[elete]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::DeleteCurrentLine)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let set_option = registry.register_ex_command(
        "ex:set",
        "Set a view option (`:set <option>`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_required_string),
            apply: Box::new(apply_set),
            // Single arg slot tied to the `gen:options` completion
            // generator so `:set <Tab>` enumerates option names and
            // `:set foldmethod=<Tab>` enumerates valid values.
            args_schema: vec![ArgSpec {
                name: "option",
                kind: ArgKind::String,
                doc: "Option name, `name=value`, `name?`, or `noname`.",
                prompt: "option:",
                default: ArgDefault::Required,
                completion: Some("gen:options"),
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let edit = registry.register_ex_command(
        "ex:edit",
        "Load a file into the current document (`:e[!] [path]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: true,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(apply_edit),
            args_schema: vec![ArgSpec {
                name: "path",
                kind: ArgKind::String,
                doc: "File path to open. Absent = reload current file.",
                prompt: "path:",
                default: ArgDefault::None,
                completion: Some("gen:files"),
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let substitute = registry.register_ex_command(
        "ex:substitute",
        "Replace pattern with replacement on the current line or `%` whole buffer (`:s/pat/rep/[g]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            // `accepts_range: true` even though v1 only honours
            // CurrentLine and Whole; the parser front-end provides the
            // range from the `s/` vs `%s/` prefix.
            accepts_range: true,
            // The substitute call enters via the parser front-end's
            // delimiter detection, not the keyword form -- parse_args
            // is unreachable for normal `:`-line input. We keep a
            // stub that errors on direct use to prevent surprise from
            // a script invocation.
            parse_args: Box::new(parse_substitute_args_unreachable),
            apply: Box::new(apply_substitute),
            args_schema: vec![
                ArgSpec::required(
                    "pattern",
                    ArgKind::Pattern,
                    "Search pattern (literal in v1; regex post-1.0)",
                ),
                ArgSpec::required(
                    "replacement",
                    ArgKind::String,
                    "Replacement text (empty deletes matches)",
                ),
                ArgSpec {
                    name: "flags",
                    kind: ArgKind::String,
                    doc: "Flags string (currently `g` honoured; others ignored)",
                    prompt: "",
                    default: ArgDefault::Literal(ArgValue::String(String::new())),
                    completion: None,
                },
            ],
            surface_form: SurfaceForm::Delimiter {
                hint: ":s/pattern/replacement/[flags]  (or :%s/.../.../  for whole buffer)",
            },
        },
    );
    let global = registry.register_ex_command(
        "ex:global",
        "Run a command on every line matching (`:g`) or NOT matching (`:v`) a pattern.",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_global_args_unreachable),
            apply: Box::new(apply_global),
            args_schema: vec![
                ArgSpec::required("pattern", ArgKind::Pattern, "Match pattern (literal in v1)"),
                ArgSpec {
                    name: "inverted",
                    kind: ArgKind::Bool,
                    doc: "True for `:v` form -- match lines NOT matching the pattern.",
                    prompt: "",
                    default: ArgDefault::Literal(ArgValue::Bool(false)),
                    completion: None,
                },
                ArgSpec::required(
                    "body",
                    ArgKind::Raw,
                    "Ex-command to run on each matching line (re-parsed per match)",
                ),
            ],
            surface_form: SurfaceForm::Delimiter {
                hint: ":g/pattern/body  (or :v/pattern/body  for inverted)",
            },
        },
    );
    let describe_command = registry.register_ex_command(
        "ex:describe-command",
        "Open the help view for a named command (DESIGN.md §5.11).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_required_string),
            apply: Box::new(apply_describe_command),
            args_schema: vec![ArgSpec {
                name: "name",
                kind: ArgKind::String,
                doc: "Registered command name (`ex:write`, `motion:word-forward`, ...)",
                prompt: "command:",
                default: ArgDefault::Required,
                completion: Some("gen:commands"),
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let describe_buffer = registry.register_ex_command(
        "ex:describe-buffer",
        "Open the help view for the current buffer's state (DESIGN.md §5.11).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::DescribeBuffer)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let apropos = registry.register_ex_command(
        "ex:apropos",
        "Search every registered command's name + doc for a substring (DESIGN.md §5.11).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_required_string),
            apply: Box::new(apply_apropos),
            args_schema: vec![ArgSpec {
                name: "pattern",
                kind: ArgKind::String,
                doc: "Case-insensitive substring matched against name and doc",
                prompt: "apropos:",
                default: ArgDefault::Required,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let describe_key = registry.register_ex_command(
        "ex:describe-key",
        "Open the help view for a key chord (DESIGN.md §5.11).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_required_string),
            apply: Box::new(apply_describe_key),
            args_schema: vec![ArgSpec {
                name: "chord",
                kind: ArgKind::Chord,
                doc: "Chord notation (`j`, `dw`, `<C-d>`, `gg`, `<Esc>`, ...)",
                prompt: "key:",
                default: ArgDefault::Required,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let list_keymap = registry.register_ex_command(
        "ex:keymap",
        "Open the help view listing every default keymap binding by mode (DESIGN.md §5.11).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::ListKeymap)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let buffer_next = registry.register_ex_command(
        "ex:bnext",
        "Cycle to the next open document buffer (`:bn[ext]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::BufferNext)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let buffer_prev = registry.register_ex_command(
        "ex:bprev",
        "Cycle to the previous open document buffer (`:bp[rev]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::BufferPrev)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let list_buffers = registry.register_ex_command(
        "ex:buffers",
        "List every open document buffer (`:ls` / `:buffers`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::ListBuffers)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let _buffer_picker = registry.register_ex_command(
        "ex:buffer-picker",
        "Open the vertico-style buffer switcher (`:b`). Type to filter; `<CR>` to switch.",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::OpenBufferPicker)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let buffer_delete = registry.register_ex_command(
        "ex:bdelete",
        "Close the active document buffer (`:bd[elete][!]`). `!` discards unsaved changes.",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: true,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|ctx| Ok(Effect::BufferDelete { force: ctx.bang })),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let file_tree = registry.register_ex_command(
        "ex:filetree",
        "Open a file-tree buffer (`:Filetree [path]`). Absent = current dir.",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(|ctx| {
                let root = match &ctx.args {
                    Args::String(p) if !p.is_empty() => Some(std::path::PathBuf::from(p.as_str())),
                    _ => None,
                };
                Ok(Effect::OpenFileTree { root })
            }),
            args_schema: vec![ArgSpec {
                name: "root",
                kind: ArgKind::String,
                doc: "Directory to open as the tree root. Absent = current dir.",
                prompt: "root:",
                default: ArgDefault::None,
                completion: Some("gen:files"),
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let file_tree_close = registry.register_ex_command(
        "ex:filetree-close",
        "Dismiss the file-tree buffer (`:FiletreeClose`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::CloseFileTree)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let oil = registry.register_ex_command(
        "ex:oil",
        "Open an oil buffer (`:Oil [path]`). Absent = current dir.",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(|ctx| {
                let dir = match &ctx.args {
                    Args::String(p) if !p.is_empty() => Some(std::path::PathBuf::from(p.as_str())),
                    _ => None,
                };
                Ok(Effect::OpenOil { dir })
            }),
            args_schema: vec![ArgSpec {
                name: "dir",
                kind: ArgKind::String,
                doc: "Directory to open. Absent = current document's parent.",
                prompt: "dir:",
                default: ArgDefault::None,
                completion: Some("gen:files"),
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let describe_option = registry.register_ex_command(
        "ex:describe-option",
        "Open the help view for a typed option (`:describe-option NAME`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_required_string),
            apply: Box::new(|ctx| match &ctx.args {
                Args::String(name) => Ok(Effect::DescribeOption {
                    name: name.to_string(),
                }),
                _ => Err(CommandError::BadArgs("expected option name".into())),
            }),
            args_schema: vec![ArgSpec {
                name: "name",
                kind: ArgKind::String,
                doc: "Registered option name (or alias).",
                prompt: "option:",
                default: ArgDefault::Required,
                completion: Some("gen:options"),
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let list_options = registry.register_ex_command(
        "ex:options",
        "List every registered option (`:options`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::ListOptions)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let hover = registry.register_ex_command(
        "ex:hover",
        "Open a hover popup at the cursor (`:hover [markdown]`). v1 path: feed text manually; \
         Phase 4 LSP will source from `textDocument/hover`.",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(|ctx| {
                let markdown = match &ctx.args {
                    Args::String(s) if !s.is_empty() => s.to_string(),
                    _ => "(empty hover)".to_string(),
                };
                Ok(Effect::OpenHover { markdown })
            }),
            args_schema: vec![ArgSpec {
                name: "markdown",
                kind: ArgKind::String,
                doc: "Markdown body of the hover popup.",
                prompt: "hover:",
                default: ArgDefault::None,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let hover_close = registry.register_ex_command(
        "ex:hover-close",
        "Dismiss the active hover popup (`:HoverClose`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::CloseHover)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    // ---- LSP introspection (Phase 4.1.g) -------------------
    let lsp_log = registry.register_ex_command(
        "ex:lsp-log",
        "Open the LSP subsystem log (`*lsp*`) or a per-server log (`*lsp:<server>*`) (`:lsp-log [server]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(|ctx| {
                let server_id = match &ctx.args {
                    Args::String(s) if !s.is_empty() => Some(s.to_string()),
                    _ => None,
                };
                Ok(Effect::OpenLspLog { server_id })
            }),
            args_schema: vec![ArgSpec {
                name: "server",
                kind: ArgKind::String,
                doc: "Server id (e.g. `rust`, `python`). Absent = subsystem-wide log.",
                prompt: "server:",
                default: ArgDefault::None,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let lsp_trace = registry.register_ex_command(
        "ex:lsp-trace",
        "Toggle JSON-RPC wire trace for a server (pure toggle; view records via `:lsp-trace-log <server>`) (`:lsp-trace <server>`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_required_string),
            apply: Box::new(|ctx| match &ctx.args {
                Args::String(s) if !s.is_empty() => Ok(Effect::ToggleLspTrace {
                    server_id: s.to_string(),
                }),
                _ => Err(CommandError::BadArgs(
                    ":lsp-trace requires a server id".into(),
                )),
            }),
            args_schema: vec![ArgSpec {
                name: "server",
                kind: ArgKind::String,
                doc: "Server id to toggle trace on.",
                prompt: "server:",
                default: ArgDefault::None,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let _lsp_trace_log = registry.register_ex_command(
        "ex:lsp-trace-log",
        "Open the JSON-RPC trace ring for an LSP server (`:lsp-trace-log [server]`). No arg = picker over every running instance; arg = pre-filter (single match short-circuits). Independent of `:lsp-trace`.",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(|ctx| {
                let server_id = match &ctx.args {
                    Args::String(s) if !s.is_empty() => Some(s.to_string()),
                    _ => None,
                };
                Ok(Effect::OpenLspTraceLog { server_id })
            }),
            args_schema: vec![ArgSpec {
                name: "server",
                kind: ArgKind::String,
                doc: "Server id (e.g. `rust`). Absent = picker over every running instance.",
                prompt: "server:",
                default: ArgDefault::None,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let lsp_status = registry.register_ex_command(
        "ex:lsp-status",
        "Render every running LSP server (id, root, pid, uptime) in a help-style buffer (`:lsp-status`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::LspStatus)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let lsp_server_log = registry.register_ex_command(
        "ex:lsp-server-log",
        "Picker-style listing of every running LSP server actor with workspace root + buffer count + capability summary; each row links to its log + trace via `exec:` URLs (`:lsp-server-log`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::LspServerLogListing)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let lsp_restart = registry.register_ex_command(
        "ex:lsp-restart",
        "Force-restart a stuck LSP server. Wired but no-op until the supervisor restart path lands in 4.4 (`:lsp-restart <server>`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_required_string),
            apply: Box::new(|ctx| match &ctx.args {
                Args::String(s) if !s.is_empty() => Ok(Effect::LspRestart {
                    server_id: s.to_string(),
                }),
                _ => Err(CommandError::BadArgs(
                    ":lsp-restart requires a server id".into(),
                )),
            }),
            args_schema: vec![ArgSpec {
                name: "server",
                kind: ArgKind::String,
                doc: "Server id to restart.",
                prompt: "server:",
                default: ArgDefault::None,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let lsp_log_level = registry.register_ex_command(
        "ex:lsp-log-level",
        "Set the subsystem-wide default min log level (or a per-server override) (`:lsp-log-level [server] <level>`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_required_string),
            apply: Box::new(|ctx| match &ctx.args {
                Args::String(s) if !s.is_empty() => {
                    let trimmed = s.trim();
                    let mut parts = trimmed.split_whitespace();
                    let first = parts.next().unwrap_or("");
                    let second = parts.next();
                    let (server_id, level) = match second {
                        Some(level) => (Some(first.to_string()), level.to_string()),
                        None => (None, first.to_string()),
                    };
                    Ok(Effect::SetLspLogLevel { server_id, level })
                }
                _ => Err(CommandError::BadArgs(
                    ":lsp-log-level requires `[server] <level>`".into(),
                )),
            }),
            args_schema: vec![ArgSpec {
                name: "spec",
                kind: ArgKind::String,
                doc: "Either a level (`error`/`warn`/`info`/`debug`/`trace`) for the subsystem default, or `<server> <level>` for a per-server override.",
                prompt: "[server] level:",
                default: ArgDefault::None,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let lsp_log_clear = registry.register_ex_command(
        "ex:lsp-log-clear",
        "Drop the records in `*lsp*` (no arg) or `*lsp:<server>*` (with arg) (`:lsp-log-clear [server]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(|ctx| {
                let server_id = match &ctx.args {
                    Args::String(s) if !s.is_empty() => Some(s.to_string()),
                    _ => None,
                };
                Ok(Effect::LspLogClear { server_id })
            }),
            args_schema: vec![ArgSpec {
                name: "server",
                kind: ArgKind::String,
                doc: "Server id whose ring to clear. Absent = subsystem-wide.",
                prompt: "server:",
                default: ArgDefault::None,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );

    let lsp_symbols = registry.register_ex_command(
        "ex:lsp-symbols",
        "Open a vertico picker over the active document's symbol outline (`:lsp-symbols`, Phase 4.2.e).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::LspDocumentSymbol)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let lsp_format = registry.register_ex_command(
        "ex:lsp-format",
        "Run `textDocument/formatting` on the active buffer's highest-priority LSP server; apply the returned edits as one undo unit (`:format`, Phase 4.3).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::LspFormat)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let lsp_format_range = registry.register_ex_command(
        "ex:lsp-format-range",
        "Run `textDocument/rangeFormatting` over the active Visual selection or the supplied line range (`:[range]format-range`, Phase 4.3).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: true,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::LspFormatRange)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );

    let lsp_code_action = registry.register_ex_command(
        "ex:lsp-code-action",
        "Open a vertico picker over LSP code actions at the cursor / selection (`:code-actions`, Phase 4.3).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: true,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::LspCodeAction)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );

    let lsp_rename = registry.register_ex_command(
        "ex:lsp-rename",
        "Rename the symbol under cursor across the workspace via textDocument/rename. Empty name uses prepareRename's placeholder when advertised (`:rename [new-name]`, Phase 4.3).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(|ctx| {
                let new_name = match &ctx.args {
                    Args::String(s) => s.trim().to_string(),
                    _ => String::new(),
                };
                Ok(Effect::LspRename { new_name })
            }),
            args_schema: vec![ArgSpec {
                name: "new-name",
                kind: ArgKind::String,
                doc: "Replacement identifier. Empty -> use the server's prepareRename placeholder.",
                prompt: "new name:",
                default: ArgDefault::None,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );

    let lsp_complete = registry.register_ex_command(
        "ex:lsp-complete",
        "Open a vertico picker over LSP completion items at the cursor (`:complete`, Phase 4.2.g). Plain-text insert -- snippet expansion / lazy resolve land with buffer-level Insert-mode completion.",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::LspComplete)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );

    let lsp_signature_help = registry.register_ex_command(
        "ex:lsp-signature-help",
        "Open the LSP signature-help popup for the current cursor (`:signature-help`, Phase 4.3). Trigger-character driven in Insert mode -- typing `(` / `,` etc. fires the same request automatically.",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::LspSignatureHelp)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );

    let lsp_workspace_symbol = registry.register_ex_command(
        "ex:lsp-workspace-symbol",
        "Open a vertico picker over workspace symbols matching `query` (server-side substring filter; `:lsp-workspace-symbol [query]`, Phase 4.2.f).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(|ctx| {
                let query = match &ctx.args {
                    Args::String(s) => s.to_string(),
                    _ => String::new(),
                };
                Ok(Effect::LspWorkspaceSymbol { query })
            }),
            args_schema: vec![ArgSpec {
                name: "query",
                kind: ArgKind::String,
                doc: "Server-side substring filter; empty returns the server's idea of \"every workspace symbol\".",
                prompt: "query:",
                default: ArgDefault::None,
                completion: None,
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );

    let list_diagnostics = registry.register_ex_command(
        "ex:diagnostics",
        "Open a help-style buffer listing every workspace diagnostic with clickable per-entry source links (`:diagnostics`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::ListDiagnostics)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let next_diagnostic = registry.register_ex_command(
        "ex:diag-next",
        "Move the cursor to the next diagnostic in the active buffer (wraps; `]d` / `:diag-next` / `:cnext`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::NextDiagnostic)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let prev_diagnostic = registry.register_ex_command(
        "ex:diag-prev",
        "Move the cursor to the previous diagnostic in the active buffer (wraps; `[d` / `:diag-prev` / `:cprev`).",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::PrevDiagnostic)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let snippet_expand = registry.register_ex_command(
        "ex:snippet-expand",
        "Expand the snippet whose prefix matches the word at the cursor (`:snippet-expand`, Phase 4.2.g.4). Surface-form alias of `<C-x><C-s>`. No-op when no snippet matches.",
        ExCommandSpec {
            latency_class: LatencyClass::Reflex,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::SnippetExpand)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let reload_snippets = registry.register_ex_command(
        "ex:reload-snippets",
        "Re-read every snippet file from disk and rebuild the per-language snippet registry (`:reload-snippets`, Phase 4.2.g.4).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_no_args),
            apply: Box::new(|_| Ok(Effect::ReloadSnippets)),
            args_schema: vec![],
            surface_form: SurfaceForm::Keyword,
        },
    );
    let help = registry.register_ex_command(
        "ex:help",
        "Open the topic index or a named help topic (`:help [topic]`).",
        ExCommandSpec {
            latency_class: LatencyClass::Display,
            accepts_bang: false,
            accepts_range: false,
            parse_args: Box::new(parse_optional_path),
            apply: Box::new(|ctx| {
                let topic = match &ctx.args {
                    Args::String(s) if !s.is_empty() => Some(s.to_string()),
                    _ => None,
                };
                Ok(Effect::OpenHelpTopic { topic })
            }),
            args_schema: vec![ArgSpec {
                name: "topic",
                kind: ArgKind::String,
                doc: "Topic name (`folding`, `buffers`, ...). Absent = index.",
                prompt: "topic:",
                default: ArgDefault::None,
                completion: Some("gen:help-topics"),
            }],
            surface_form: SurfaceForm::Keyword,
        },
    );
    ExBuiltins {
        write,
        quit,
        write_quit,
        no_hlsearch,
        list_registers,
        list_marks,
        delete_line,
        set_option,
        edit,
        substitute,
        global,
        describe_command,
        describe_buffer,
        apropos,
        describe_key,
        list_keymap,
        buffer_next,
        buffer_prev,
        list_buffers,
        buffer_delete,
        file_tree,
        file_tree_close,
        oil,
        describe_option,
        list_options,
        hover,
        hover_close,
        help,
        list_diagnostics,
        next_diagnostic,
        prev_diagnostic,
        lsp_log,
        lsp_trace,
        lsp_status,
        lsp_server_log,
        lsp_restart,
        lsp_log_level,
        lsp_log_clear,
        lsp_symbols,
        lsp_workspace_symbol,
        lsp_format,
        lsp_format_range,
        lsp_signature_help,
        lsp_complete,
        lsp_rename,
        lsp_code_action,
        snippet_expand,
        reload_snippets,
    }
}

// ---- parse_args helpers (raw string -> typed Args) ----

fn parse_no_args(rest: &str, _bang: bool) -> GrammarResult<Args> {
    if rest.trim().is_empty() {
        Ok(Args::None)
    } else {
        Err(CommandError::BadArgs(
            "trailing characters after command".into(),
        ))
    }
}

fn parse_optional_path(rest: &str, _bang: bool) -> GrammarResult<Args> {
    let trimmed = rest.trim();
    if trimmed.is_empty() {
        Ok(Args::None)
    } else {
        Ok(Args::String(trimmed.to_string()))
    }
}

fn parse_required_string(rest: &str, _bang: bool) -> GrammarResult<Args> {
    let trimmed = rest.trim();
    if trimmed.is_empty() {
        Err(CommandError::BadArgs("argument required".into()))
    } else {
        Ok(Args::String(trimmed.to_string()))
    }
}

/// `:substitute` and `:global` enter through the `:`-line parser's
/// delimiter detection, not through the generic keyword path -- their
/// args come pre-parsed as `Args::List`. These stubs guard against a
/// caller that registers a keyword alias `:substitute foo`: the parse
/// path errors instead of producing malformed Args::List.
fn parse_substitute_args_unreachable(_rest: &str, _bang: bool) -> GrammarResult<Args> {
    Err(CommandError::BadArgs(
        "use the delimiter form: `:s/pattern/replacement/[flags]`".into(),
    ))
}

fn parse_global_args_unreachable(_rest: &str, _bang: bool) -> GrammarResult<Args> {
    Err(CommandError::BadArgs(
        "use the delimiter form: `:g/pattern/body` (or `:v/...` for inverted)".into(),
    ))
}

// ---- apply closures ----

fn apply_write(ctx: &ExCommandContext) -> GrammarResult<Effect> {
    let path = match &ctx.args {
        Args::None => None,
        Args::String(s) => Some(std::path::PathBuf::from(s)),
        _ => {
            return Err(CommandError::BadArgs(
                "expected optional path string".into(),
            ));
        }
    };
    Ok(Effect::SaveBuffer { path })
}

fn apply_quit(ctx: &ExCommandContext) -> GrammarResult<Effect> {
    Ok(Effect::QuitEditor { force: ctx.bang })
}

fn apply_write_quit(ctx: &ExCommandContext) -> GrammarResult<Effect> {
    // The bang on `:wq!` / `:x!` propagates to the quit step (vim's
    // semantics: force the quit even if the save fails). Save itself is
    // never forced -- writing a path you don't have permission for fails
    // visibly.
    Ok(Effect::Many(vec![
        Effect::SaveBuffer { path: None },
        Effect::QuitEditor { force: ctx.bang },
    ]))
}

fn apply_set(ctx: &ExCommandContext) -> GrammarResult<Effect> {
    match &ctx.args {
        Args::String(s) => Ok(Effect::SetOption { spec: s.clone() }),
        _ => Err(CommandError::BadArgs("expected option string".into())),
    }
}

fn apply_edit(ctx: &ExCommandContext) -> GrammarResult<Effect> {
    let path = match &ctx.args {
        Args::None => None,
        Args::String(s) => Some(std::path::PathBuf::from(s)),
        _ => {
            return Err(CommandError::BadArgs(
                "expected optional path string".into(),
            ));
        }
    };
    Ok(Effect::OpenBuffer {
        path,
        force: ctx.bang,
    })
}

fn apply_substitute(ctx: &ExCommandContext) -> GrammarResult<Effect> {
    let list = ctx
        .args
        .as_list()
        .ok_or_else(|| CommandError::BadArgs("expected Args::List for :substitute".into()))?;
    if list.len() != 3 {
        return Err(CommandError::BadArgs(
            "expected 3 args: pattern, replacement, flags".into(),
        ));
    }
    let pattern = list[0]
        .as_str()
        .ok_or_else(|| CommandError::BadArgs("arg 0 (pattern) must be string-shaped".into()))?
        .to_string();
    let replacement = list[1]
        .as_str()
        .ok_or_else(|| CommandError::BadArgs("arg 1 (replacement) must be string-shaped".into()))?
        .to_string();
    let flags = list[2]
        .as_str()
        .ok_or_else(|| CommandError::BadArgs("arg 2 (flags) must be string-shaped".into()))?;
    let global = flags.contains('g');
    // Scope falls out of the invocation's range: `s/...` -> CurrentLine,
    // `%s/...` -> Whole. The parser front-end set this from the
    // delimiter prefix.
    let scope = match ctx.range {
        Some(Range::Whole) => SubstituteScope::Whole,
        _ => SubstituteScope::CurrentLine,
    };
    Ok(Effect::Substitute {
        scope,
        pattern,
        replacement,
        global,
    })
}

fn apply_describe_command(ctx: &ExCommandContext) -> GrammarResult<Effect> {
    match &ctx.args {
        Args::String(s) => Ok(Effect::DescribeCommand {
            name: s.clone(),
            anchor: None,
        }),
        _ => Err(CommandError::BadArgs("expected command name string".into())),
    }
}

fn apply_apropos(ctx: &ExCommandContext) -> GrammarResult<Effect> {
    match &ctx.args {
        Args::String(s) => Ok(Effect::Apropos { pattern: s.clone() }),
        _ => Err(CommandError::BadArgs("expected pattern string".into())),
    }
}

fn apply_describe_key(ctx: &ExCommandContext) -> GrammarResult<Effect> {
    match &ctx.args {
        Args::String(s) => Ok(Effect::DescribeKey { chord: s.clone() }),
        _ => Err(CommandError::BadArgs(
            "expected chord notation string".into(),
        )),
    }
}

fn apply_global(ctx: &ExCommandContext) -> GrammarResult<Effect> {
    let list = ctx
        .args
        .as_list()
        .ok_or_else(|| CommandError::BadArgs("expected Args::List for :global".into()))?;
    if list.len() != 3 {
        return Err(CommandError::BadArgs(
            "expected 3 args: pattern, inverted, body".into(),
        ));
    }
    let pattern = list[0]
        .as_str()
        .ok_or_else(|| CommandError::BadArgs("arg 0 (pattern) must be string-shaped".into()))?
        .to_string();
    let inverted = list[1]
        .as_bool()
        .ok_or_else(|| CommandError::BadArgs("arg 1 (inverted) must be bool".into()))?;
    let body = list[2]
        .as_invocation()
        .ok_or_else(|| {
            CommandError::BadArgs(
                "arg 2 (body) must be a parsed Invocation -- parser front-end is responsible"
                    .into(),
            )
        })?
        .clone();
    Ok(Effect::Global {
        pattern,
        inverted,
        body: Box::new(body),
    })
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used, clippy::panic)]
    use super::*;
    use crate::CancellationToken;
    use crate::command::CommandInvocation;
    use crate::dispatcher::execute;
    use lattice_core::Document;
    use lattice_protocol::position::Position;

    fn fixture() -> (CommandRegistry, ExBuiltins, Document) {
        let mut registry = CommandRegistry::new();
        let _ = crate::builtins::populate(&mut registry);
        let ex = populate(&mut registry);
        (registry, ex, Document::empty())
    }

    #[test]
    fn write_with_no_path_emits_save_buffer_none() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.write.0).with_args(Args::None);
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        match eff {
            Effect::SaveBuffer { path } => assert!(path.is_none()),
            other => panic!("unexpected effect: {other:?}"),
        }
    }

    #[test]
    fn write_with_path_carries_path() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.write.0).with_args(Args::String("foo.txt".into()));
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        match eff {
            Effect::SaveBuffer { path: Some(p) } => {
                assert_eq!(p, std::path::PathBuf::from("foo.txt"))
            }
            other => panic!("unexpected effect: {other:?}"),
        }
    }

    #[test]
    fn quit_bang_propagates_to_force() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.quit.0).with_bang(true);
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        match eff {
            Effect::QuitEditor { force } => assert!(force),
            other => panic!("unexpected effect: {other:?}"),
        }
    }

    #[test]
    fn quit_no_bang_is_not_forced() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.quit.0);
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        match eff {
            Effect::QuitEditor { force } => assert!(!force),
            other => panic!("unexpected effect: {other:?}"),
        }
    }

    #[test]
    fn write_quit_emits_many_save_then_quit() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.write_quit.0).with_bang(true);
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        match eff {
            Effect::Many(parts) => {
                assert!(matches!(parts[0], Effect::SaveBuffer { .. }));
                assert!(matches!(parts[1], Effect::QuitEditor { force: true }));
            }
            other => panic!("unexpected effect: {other:?}"),
        }
    }

    #[test]
    fn nohlsearch_emits_clear_search_highlight() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.no_hlsearch.0);
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        assert!(matches!(eff, Effect::ClearSearchHighlight));
    }

    #[test]
    fn registers_emits_echo_registers() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.list_registers.0);
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        assert!(matches!(eff, Effect::EchoRegisters));
    }

    #[test]
    fn marks_emits_echo_marks() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.list_marks.0);
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        assert!(matches!(eff, Effect::EchoMarks));
    }

    #[test]
    fn delete_emits_delete_current_line() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.delete_line.0);
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        assert!(matches!(eff, Effect::DeleteCurrentLine));
    }

    #[test]
    fn describe_command_emits_describe_command_effect() {
        let (registry, ex, mut doc) = fixture();
        let inv =
            CommandInvocation::of(ex.describe_command.0).with_args(Args::String("ex:write".into()));
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        match eff {
            Effect::DescribeCommand { name, anchor } => {
                assert_eq!(name, "ex:write");
                assert!(anchor.is_none());
            }
            other => panic!("unexpected effect: {other:?}"),
        }
    }

    #[test]
    fn describe_buffer_emits_describe_buffer_effect() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.describe_buffer.0);
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        assert!(matches!(eff, Effect::DescribeBuffer));
    }

    #[test]
    fn apropos_emits_apropos_effect() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.apropos.0).with_args(Args::String("write".into()));
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        match eff {
            Effect::Apropos { pattern } => assert_eq!(pattern, "write"),
            other => panic!("unexpected effect: {other:?}"),
        }
    }

    #[test]
    fn describe_command_advertises_args_schema() {
        // §B.1 metadata is what makes :describe-command interesting.
        let (registry, ex, _doc) = fixture();
        let spec = registry.lookup(ex.describe_command.0).unwrap();
        assert_eq!(spec.args_schema.len(), 1);
        assert_eq!(spec.args_schema[0].name, "name");
    }

    #[test]
    fn apropos_advertises_args_schema() {
        let (registry, ex, _doc) = fixture();
        let spec = registry.lookup(ex.apropos.0).unwrap();
        assert_eq!(spec.args_schema.len(), 1);
        assert_eq!(spec.args_schema[0].name, "pattern");
    }

    #[test]
    fn set_with_string_arg_carries_spec() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.set_option.0).with_args(Args::String("number".into()));
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        match eff {
            Effect::SetOption { spec } => assert_eq!(spec, "number"),
            other => panic!("unexpected effect: {other:?}"),
        }
    }

    #[test]
    fn set_with_no_args_errors() {
        let (registry, ex, mut doc) = fixture();
        // The dispatcher itself doesn't error here -- parse_args is called
        // by the parser front-end, not the dispatcher. apply with the
        // wrong Args variant errors instead.
        let inv = CommandInvocation::of(ex.set_option.0).with_args(Args::None);
        let err = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap_err();
        assert!(matches!(err, CommandError::BadArgs(_)));
    }

    #[test]
    fn edit_with_path_and_bang_carries_force() {
        let (registry, ex, mut doc) = fixture();
        let inv = CommandInvocation::of(ex.edit.0)
            .with_args(Args::String("/tmp/x".into()))
            .with_bang(true);
        let eff = execute(
            &registry,
            &mut doc,
            Position::ZERO,
            inv,
            &CancellationToken::never(),
        )
        .unwrap();
        match eff {
            Effect::OpenBuffer { path, force } => {
                assert_eq!(path, Some(std::path::PathBuf::from("/tmp/x")));
                assert!(force);
            }
            other => panic!("unexpected effect: {other:?}"),
        }
    }

    #[test]
    fn parse_no_args_rejects_trailing() {
        assert!(matches!(
            parse_no_args("oops", false),
            Err(CommandError::BadArgs(_))
        ));
        assert!(matches!(parse_no_args("", false), Ok(Args::None)));
        assert!(matches!(parse_no_args("   ", false), Ok(Args::None)));
    }

    #[test]
    fn parse_optional_path_returns_some_or_none() {
        assert!(matches!(parse_optional_path("", false), Ok(Args::None)));
        match parse_optional_path("foo.rs", false).unwrap() {
            Args::String(s) => assert_eq!(s, "foo.rs"),
            other => panic!("unexpected args: {other:?}"),
        }
    }

    #[test]
    fn parse_required_string_demands_arg() {
        assert!(matches!(
            parse_required_string("", false),
            Err(CommandError::BadArgs(_))
        ));
        match parse_required_string("number", false).unwrap() {
            Args::String(s) => assert_eq!(s, "number"),
            other => panic!("unexpected args: {other:?}"),
        }
    }
}
