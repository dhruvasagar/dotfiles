//! Your lattice config, compiled to a WASM component and loaded at boot.
//! Edit freely, then rebuild + `:reload-config` (see this dir's build steps, or
//! `docs/user/init.md`). Everything below is illustrative — delete what you
//! don't want.

wit_bindgen::generate!({ world: "init", path: "wit" });

// `Guest` is wit-bindgen's trait for THIS world's exports — the `register_*`
// functions the host calls once at load, plus `on_event`. (The name `Guest` is
// fixed by wit-bindgen.) `Config` is your config — the type that implements them.
// `Event` is re-exported at the crate root by the world's `use types.{event}`
// (refer to it unqualified); other types come from their interface module.
mod config_shape;

use lattice::plugin_host::keymap::BindingMode;
use lattice::plugin_host::plugin_manager::{PluginSource, PluginSpec};
use lattice::plugin_host::types::{EventFilter, EventKind};
use lattice::plugin_host::{config, events, keymap, plugin_manager};

struct Config;

impl Guest for Config {
    // IMMEDIATE — option overrides (also settable in lattice.toml or via `:set`).
    fn register_options() {
        config::set_option("tabstop", "4");

        // auto-pair is a CORE plugin, ON by default. Its options are namespaced
        // by plugin id — set them by their full name here:
        config::set_option("auto-pair.style", "manual"); // manual close-key pairing
        // config::set_option("auto-pair.enabled", "false"); // …or turn it off
    }

    // Plugins to install. The host clones into ~/.cache/lattice/sources/,
    // builds with `cargo build --release --target wasm32-wasip2`, and stages
    // into ~/.config/lattice/plugins/ — then loads, all before the on-disk
    // scan, so a fresh install lands this boot rather than the next.
    //
    // `require` RECORDS and returns; it never blocks boot on a network fetch or
    // a compile. So org's contributions (the language, org-mode, its chords)
    // appear a frame or two after the first paint on a cold install, and
    // instantly once cached.
    fn register_plugins() {
        // org-mode. Not a core plugin on purpose: its tree-sitter grammar is
        // 2.2 MB of generated C fetched at build time, which has no business
        // in every workspace build.
        //
        // No `enable_mode`: org-mode is a MAJOR, activated by the language its
        // own `language` seam registers, not by the enable/disable switch that
        // governs minor modes.
        plugin_manager::require(&PluginSpec {
            name: "org".to_string(),
            // LOCAL while org is under active development: built in place, never
            // copied, so an edit + `:reload-config` picks it up with no clone or
            // fetch in between. Swap to the git source once it settles:
            //
            //     source: PluginSource::Git(GitSource {
            //         url: "https://github.com/dhruvasagar/lattice-org-plugin".into(),
            //         rev: None,   // or pin a rev
            //     }),
            // `~` expands host-side, so this file stays portable across
            // machines rather than naming one home directory.
            source: PluginSource::Local("~/src/dhruvasagar/lattice-org-plugin".to_string()),
            enable_mode: None,
            pinned: false,
        });
    }

    // IMMEDIATE — keybindings layered above the builtin vim grammar.
    fn register_keymap() {
        // <C-s> in Normal → an existing command (binds only if the command exists).
        keymap::register_binding(BindingMode::Normal, "<C-s>", "ex:write");
    }

    // Subscribe deferred / event-flow hooks (handler ids are yours to choose).
    fn register_events() {
        // Plugin OPTIONS go here. `pre-plugin-loaded` fires after the named
        // plugin has declared its options and before it reads any of them, and
        // the load waits for this handler to finish — which is what lets a
        // value reach an option the plugin consumes while loading.
        events::subscribe(
            &EventFilter {
                kinds: Some(vec![EventKind::PrePluginLoaded]),
                path_globs: None,
                major_modes: None,
                minor_modes: None,
            },
            1,
        );

        // MODE-SCOPED options go here — this editor's `add-hook 'org-mode-hook`.
        //
        // `major-entered` fires whenever a buffer enters a major, and
        // `major_modes` filters it HOST-side, so this handler is woken for org
        // buffers and for nothing else. That filter is the whole reason this
        // is cheap: an unfiltered subscription would cross the WASM boundary
        // on every buffer open in the editor to do nothing.
        //
        // Works the same for a built-in major, a core-plugin one, or an
        // external plugin's like org — the mode dispatcher publishes the event
        // and does not know which of those declared the mode.
        events::subscribe(
            &EventFilter {
                kinds: Some(vec![EventKind::MajorEntered]),
                path_globs: None,
                major_modes: Some(vec!["org-mode".to_string()]),
                minor_modes: None,
            },
            2,
        );
    }

    // React to events — e.g. configure a USER plugin as it loads.
    // (Core plugins like auto-pair are on by default — configure them in
    // `register_options` above, not here.)
    fn on_event(handler: u32, ev: Event) {
        // Org is a prose major, so it wants `autowrap=all` — long prose wraps,
        // not just comments. markdown / text / gitcommit declare that natively
        // through `Mode::options()`; org is an external plugin and does not,
        // so it is set here.
        //
        // `set_option_in_buffer`, NOT `set_option`: the latter is the `:set`
        // path and writes the global layer, which would wrap every buffer in
        // the editor and leave nothing to unwrap on leaving org. This writes
        // the buffer-local layer, which is the scope the question has.
        //
        // A `:setlocal autowrap=...` in the buffer still wins over it, which
        // is the right way round — the last word belongs to whoever is
        // actually in the buffer.
        if let (2, Event::MajorEntered(lifecycle)) = (handler, &ev) {
            config::set_option_in_buffer(lifecycle.buffer, "autowrap", "all");
            return;
        }
        if let (1, Event::PrePluginLoaded(name)) = (handler, &ev) {
            match name.as_str() {
                // org's options do not EXIST until org loads, and `set_option`
                // on an unknown option is a logged no-op — so this cannot go in
                // `register_options`, which runs while org is still being
                // fetched and built.
                //
                // `pre-plugin-loaded` rather than `plugin-loaded`, because two
                // of these are read by org WHILE IT LOADS: `todo-keywords`
                // drives the per-keyword theme elements and the generated
                // highlight query, and `todo-keyword-styles` is applied as the
                // theme overrides above them. On `plugin-loaded` both had
                // already been read, so NEXT / WAITING / HOLD rendered
                // unstyled no matter what was set here. The rest are read per
                // use and would work from either, but one place is easier to
                // reason about than two.
                //
                // Full name, not the short one: `set_option` prefixes with the
                // CALLING plugin's id, so a bare `capture-templates` would be
                // looked up as `init.capture-templates` and then as
                // `capture-templates` — neither of which exists.
                "org" => {
                    // Structured: `org.capture-templates` declares a
                    // `list<record>` schema, so the value goes over as a TREE
                    // rather than as TOML in a string. The host validates it
                    // against org's declaration and rejects a bad field with a
                    // path; a typo here is a compile error instead.
                    config_shape::set_option_value(
                        "org.capture-templates",
                        &capture_templates(),
                    );
                    // TC.7: a `list<string>` now. `set_option` still takes a
                    // string and splits it, which keeps a one-path spelling
                    // terse; the typed peer is there when a list is long
                    // enough to want naming.
                    config::set_option("org.agenda-files", "~/src/dhruvasagar/org-files");
                    config::set_option("org.roam-directory", "~/src/dhruvasagar/org-files/roam");
                    // The PKOS capture set, sourced from the SAME template
                    // files emacs reads (OR.14's `body-file`). One copy, so
                    // editing a template in `roam/templates/` changes both
                    // editors and neither can drift from the other.
                    config_shape::set_option_value(
                        "org.roam-capture-templates",
                        &roam_capture_templates(),
                    );
                    // TC.7: both are declared shapes now — `todo-keywords`
                    // a list of emacs' own sequence LINES (the grammar is
                    // org's public spelling and stays), `todo-keyword-styles`
                    // a record per keyword.
                    config_shape::set_option_value("org.todo-keywords", &todo_keywords());
                    config_shape::set_option_value(
                        "org.todo-keyword-styles",
                        &todo_keyword_styles(),
                    );
                    config_shape::set_option_value(
                        "org.agenda-custom-commands",
                        &agenda_custom_commands(),
                    );
                    // `org-agenda-span 'day`. The option is a day COUNT, so
                    // emacs's symbol becomes `0` — "today only", which is what
                    // 'day means. Every `when = "days"` section with no `days`
                    // of its own inherits it, including the one inside the
                    // composite agenda below.
                    config::set_option("org.agenda-span", "0");
                    // `org-startup-with-inline-images t`.
                    config::set_option("org.inline-images", "true");
                }
                _ => {}
            }
        }
    }

    // A periodic wake came due (OC.2). Required by the `events` world whether
    // or not you ever call `wake_every` — the host never calls this unless you
    // arm one, but the export has to exist or the WHOLE component fails to
    // instantiate ("no export `on-wake` found") and every seam in this file
    // goes with it, not just events.
    fn on_wake(_id: u32) {}
}

/// `org-todo-keywords`, ported verbatim from `~/dotfiles/emacs/ds/init-org.el`.
///
/// One sequence per line, in org's own spelling. `sequence:` states are a
/// workflow (order matters, `|` splits not-done from done); `type:` states are
/// a kind, all not-done. A bare list with no prefix would be a sequence, so the
/// prefixes are what keep the third line from being read as a workflow.
///
/// The logging specs (`@`, `!`, `@/!`) are carried through and PARSED, but not
/// yet acted on — no `LOGBOOK` note is written on a state change. They are kept
/// rather than stripped so this file stays a faithful copy of the emacs one and
/// starts working the day logging lands.
///
/// Note that `PHONE` and `MEETING` sit after the `|` on line 2, so they are
/// DONE states. That is what the emacs config says; it is preserved rather than
/// corrected.
fn todo_keywords() -> Vec<String> {
    vec![
        "sequence: TODO(t) NEXT(n) | DONE(d)".to_string(),
        "sequence: WAITING(w@/!) HOLD(h@/!) | CANCELLED(c@/!) PHONE MEETING".to_string(),
        "type: PROJECT TO-READ READING(!/!) TO-WATCH WATCHING(!/!)".to_string(),
    ]
}

/// `org-todo-keyword-faces`, ported exactly.
///
/// `fg=` takes a palette key — which follows a `:colorscheme` swap — or a
/// literal `#rrggbb`. Emacs's "forest green" is a plain palette `green` here;
/// the exact shade is the colourscheme's business, which is the point of naming
/// a key instead of a hex value.
///
/// **This deliberately overrides one lattice default.** Org's built-in styling
/// paints `CANCELLED` dim-overlay rather than green, on the argument that
/// "achieved versus abandoned is the distinction someone scanning an agenda
/// actually wants". The emacs config paints CANCELLED, MEETING and PHONE the
/// same green as DONE, and that is what is ported — a faithful copy was the
/// point. Delete the `CANCELLED:` line to get lattice's treatment back.
///
/// `MEETING`, `PHONE`, `TO-WATCH` and `WATCHING` have no built-in default at
/// all, so they need these lines regardless of that argument.
///
/// One caveat worth knowing before it surprises you: these are applied as theme
/// OVERRIDES, so they beat the active colourscheme — but `:colorscheme`
/// REPLACES the override set, so a swap drops them until the next config
/// reload.
/// `org-todo-keyword-faces`, ported from `~/dotfiles/emacs/ds/init-org.el`.
///
/// A record per keyword rather than a line format: `org.todo-keyword-styles`
/// declares its shape, so a misspelled field is a compile error here and a
/// wrong-typed one is refused by the host with a path.
///
/// Only the fields this config uses are declared. The option accepts `bg`,
/// `italic`, `underline` and `dim` too — a modifier set to `false` CLEARS one
/// inherited from the state's default, which is what the old `no-bold`
/// spelling meant.
#[derive(Debug, Clone, lattice_plugin_sdk::ConfigShape)]
struct KeywordStyle {
    /// The TODO keyword this styles.
    keyword: String,
    /// A palette key, or a literal `#rrggbb`.
    fg: Option<String>,
    /// `true` sets bold, `false` clears an inherited one.
    bold: Option<bool>,
}

fn todo_keyword_styles() -> Vec<KeywordStyle> {
    vec![
        KeywordStyle {
            keyword: "TODO".into(),
            fg: Some("red".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "NEXT".into(),
            fg: Some("blue".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "DONE".into(),
            fg: Some("green".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "WAITING".into(),
            fg: Some("orange".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "HOLD".into(),
            fg: Some("magenta".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "CANCELLED".into(),
            fg: Some("green".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "MEETING".into(),
            fg: Some("green".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "PHONE".into(),
            fg: Some("green".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "PROJECT".into(),
            fg: Some("blue".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "TO-READ".into(),
            fg: Some("yellow".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "READING".into(),
            fg: Some("orange".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "TO-WATCH".into(),
            fg: Some("yellow".into()),
            bold: Some(true),
        },
        KeywordStyle {
            keyword: "WATCHING".into(),
            fg: Some("orange".into()),
            bold: Some(true),
        },
    ]
}

/// `org-agenda-custom-commands` — the dispatcher's named agendas.
///
/// Typed Rust rather than TOML in a string: the option declares a
/// `list<record>` schema, so a misspelled field is a compile error here and a
/// wrong-typed one is refused by the host with a path.
///
/// ## Not a verbatim emacs port
///
/// The first version was one, and it produced commands that did not work. The
/// agenda's job is task tracking, planning and scheduling; these are built for
/// that rather than for fidelity to a config written for a different editor.
///
/// What that changed, and why:
///
/// - **`r` Refile is scoped to what still needs filing.** `tags "refile"` in
///   emacs shows every headline in `refile.org` — 63 of them, 38 in a done
///   state. That is an archive, not an inbox. `/!` narrows it to not-done
///   keywords, which is what "still to file" means.
/// - **`N` Notes is gone.** Notes live in org-roam; an agenda block for them
///   was a view nobody opened.
/// - **`n` Next actions added.** The single most useful GTD block and the one
///   the emacs config expressed through a skip function, which does not port.
///   `/NEXT` is the whole of it here.
/// - **`w` Waiting promoted to its own command.** It was only a block inside
///   the composite; blocked work is worth asking about directly.
///
/// ## Still not here
///
/// Six of the emacs composite's nine blocks are defined by a `bh/skip-*` elisp
/// function — stuck projects, project tasks, non-project tasks, and so on.
/// Skip functions are arbitrary elisp and are permanently out of scope
/// (`docs/dev/architecture/org-agenda.md` §7). Their tag matches alone are not
/// those blocks: three of the six share the identical match
/// `-REFILE-CANCELLED-WAITING-HOLD/!` and are distinguished only by the
/// function, so porting them would give blocks with the right titles, heavily
/// overlapping contents, and no honest reading.
///
/// Also dropped: `b` Birthdays & Anniversaries, which overrides
/// `org-agenda-files` per command — a command names WHICH agenda, never WHERE.

/// One block of a named agenda. Mirrors org's declared section shape.
#[derive(Debug, Clone, lattice_plugin_sdk::ConfigShape)]
struct Block {
    /// The block's header.
    title: String,
    /// `overdue` | `days` | `undated` | `any`.
    when: String,
    /// Days ahead, for `when = "days"`. Absent uses `org.agenda-span`.
    days: Option<i64>,
    /// Restrict to headlines carrying a not-done keyword.
    todo_only: Option<bool>,
    /// Org's tags/todo match — `"-CANCELLED+WAITING|HOLD/!"`.
    r#match: Option<String>,
}

/// One named agenda, reached by its key from the dispatcher.
#[derive(Debug, Clone, lattice_plugin_sdk::ConfigShape)]
struct CustomCommand {
    /// The dispatcher key. A string, not a char: `<Space>` and `C-a` are keys
    /// as readily as `w`.
    key: String,
    /// What the dispatcher row reads.
    description: Option<String>,
    /// The blocks this agenda is, in order.
    section: Option<Vec<Block>>,
}

fn block(title: &str, when: &str, m: Option<&str>, todo_only: bool) -> Block {
    Block {
        title: title.to_string(),
        when: when.to_string(),
        days: None,
        todo_only: todo_only.then_some(true),
        r#match: m.map(str::to_string),
    }
}

fn agenda_custom_commands() -> Vec<CustomCommand> {
    vec![
        // The daily dashboard. `<Space>`, not emacs' literal space: a key of
        // whitespace is indistinguishable from a missing one, so the key is
        // trimmed before use and the menu spells a space `<Space>`. Same
        // keystroke, different spelling.
        CustomCommand {
            key: "<Space>".into(),
            description: Some("Agenda".into()),
            section: Some(vec![
                // `org.agenda-span` is 0, so this is today.
                block("Agenda", "days", None, false),
                block("Next actions", "any", Some("/NEXT"), false),
                // `/!` — the DASHBOARD wants what is actionable, so this is
                // narrowed to not-done keywords. The standalone `r` below is
                // the full inbox; a daily planning view drowned in 38 archived
                // meetings is not a planning view.
                //
                // Lowercase: `refile.org` declares `#+FILETAGS: :refile:` and
                // tag matching is case-sensitive. The uppercase spelling this
                // block used to carry matched nothing at all.
                block("To refile", "any", Some("refile/!"), false),
                block("Waiting and postponed", "any", Some("-CANCELLED+WAITING|HOLD/!"), true),
            ]),
        },
        CustomCommand {
            key: "n".into(),
            description: Some("Next actions".into()),
            section: Some(vec![block("Next actions", "any", Some("/NEXT"), false)]),
        },
        // The full inbox: every headline tagged `refile`, done ones included.
        // That is what emacs' `tags "refile"` shows and what an inbox IS — the
        // 38 entries already in a done state are a signal to archive them, not
        // something to hide. `/!` here would show one row of sixty-three and
        // read as "nothing to file".
        CustomCommand {
            key: "r".into(),
            description: Some("Refile (full inbox)".into()),
            section: Some(vec![block("To refile", "any", Some("refile"), false)]),
        },
        CustomCommand {
            key: "w".into(),
            description: Some("Waiting".into()),
            section: Some(vec![block(
                "Waiting and postponed",
                "any",
                Some("-CANCELLED+WAITING|HOLD/!"),
                true,
            )]),
        },
        CustomCommand {
            key: "h".into(),
            description: Some("Habits".into()),
            section: Some(vec![block(
                "Habits",
                "any",
                Some("STYLE=\"habit\""),
                true,
            )]),
        },
    ]
}

/// Capture templates, ported from `~/dotfiles/emacs/ds/init-org.el`.
///
/// A Rust value, not a TOML string: `org.capture-templates` declares a
/// `list<record>` schema and the value crosses as a tree. See the structs and
/// `capture_templates()` below, and `docs/dev/architecture/org-capture.md` §2.
///
/// A target `file` may start with `~/` — the plugin expands it on both the
/// read and the write path.
///
/// Dropped from the emacs originals, with reasons:
///
/// - `:clock-in` / `:clock-resume` (t, r, m, p) — clocking is not built.
/// - `:immediate-finish` (r, w) — no equivalent; every capture is confirmed.
/// - `%:from` / `%:subject` (r) — these read a mail capture context that only
///   org-protocol / mu4e supply. Ported as `%^{}` prompts so the template
///   still does its job.
/// - `%c` (w) — the org-protocol clipboard. Same treatment: a prompt.
/// - `%(org-id-new)` (v) — there is no elisp, and no named equivalent yet.
///   The `:ID:` line is gone; this one comes back with org-roam.
/// - `%(format-time-string "%<<%Y-%m-%d %a .+1d/3d>>")` (h) — same.
///   `%t` emits `<date Day>` with no way to carry a repeater, and a habit
///   without a repeater is not a habit, so the stamp is prompted instead.
///
/// `org-capture-templates`, ported from `~/dotfiles/emacs/ds/init-org.el`.
///
/// A Rust value rather than a TOML string, because `org.capture-templates` is a
/// STRUCTURED option: it declares a `list<record>` schema and the value crosses
/// as a tree. `#[derive(ConfigShape)]` is what turns these structs into that
/// tree — field names go over kebab-cased, so `clock_in` is org's `clock-in`.
///
/// The gain over the string it replaces is that mistakes move to compile time.
/// A misspelled field, a missing `target`, a body that is not a string: each
/// was a runtime warning buried in the log, and each is now a type error here.
/// The two config homes no longer share one string — `lattice.toml` writes the
/// same templates as `[[org.capture-templates]]` tables — and what they share
/// instead is the schema, which `:describe-option org.capture-templates` shows.

/// Where a capture lands. Mirrors org's declared `target` record.
#[derive(Debug, Clone, Default, lattice_plugin_sdk::ConfigShape)]
struct Target {
    /// Which target shape — org's own vocabulary (`file`, `file+headline`,
    /// `file+olp`, `file+datetree`, and for org-roam `file+head`). Absent keeps
    /// the old inference from `headline`.
    kind: Option<String>,
    /// The file the capture is written to.
    file: String,
    /// Insert under this headline's subtree instead of appending.
    headline: Option<String>,
    /// `file+datetree` only: `day` (default), `week` or `month`.
    tree_type: Option<String>,
    /// `file+datetree` only: headings below the date node to file under.
    sub_olp: Option<Vec<String>>,
}

/// One capture template. `Option<_>` is how a field says it may be omitted —
/// the same rule org's own declaration uses, because both sides derive it from
/// the type.
#[derive(Debug, Clone, Default, lattice_plugin_sdk::ConfigShape)]
struct Template {
    /// The keystroke that selects it in the capture menu.
    key: String,
    /// What the menu row says.
    description: Option<String>,
    /// Where the capture goes.
    target: Target,
    /// The template text, with `%?` / `%U` / `%^{…}` placeholders.
    body: Option<String>,
    /// The template text read from a file instead — org's `(file "…")`.
    /// Mutually exclusive with `body`.
    body_file: Option<String>,
    /// `entry` (default) or `table-line` — a row appended to the first table
    /// under the target.
    ///
    /// `r#type` is Rust's raw-identifier syntax: `type` is a keyword, and the
    /// `r#` prefix lets it be used as a field name anyway. The derive strips
    /// the prefix, so the option key is org's own `type`. See
    /// <https://doc.rust-lang.org/reference/identifiers.html#raw-identifiers>.
    r#type: Option<String>,
    /// The key of the template that creates today's node when this capture
    /// finds it missing (`file+datetree` with a `sub_olp` only).
    seed: Option<String>,
    /// Start a clock on the captured entry (org's `:clock-in`).
    clock_in: Option<bool>,
}

/// `org-roam-capture-templates`, pointed at the template files rather than
/// carrying their text.
///
/// **No "default" template, deliberately** — the same choice the emacs config
/// makes and for the same reason: a node created outside the taxonomy (no
/// `:TYPE:` / `:STATUS:`) is invisible to PKOS queries. The no-metadata path
/// is the inbox (`<leader>oc`), processed into a typed node within a day.
///
/// The SAME [`Template`] type as the capture list, as emacs holds
/// `org-capture-templates` and `org-roam-capture-templates` as two variables of
/// one shape. Each declares the `:target` org-roam requires, spelled exactly as
/// `init-org.el` spells it: `(file "%<%Y%m%d%H%M%S>-${slug}.org")`. The path is
/// relative, so it lands in `org.roam-directory`.
fn roam_capture_templates() -> Vec<Template> {
    // `~` is expanded by the plugin, so this stays portable across machines.
    let t = |key: &str, description: &str, file: &str| Template {
        key: key.into(),
        description: Some(description.into()),
        target: Target {
            kind: Some("file".into()),
            file: "%<%Y%m%d%H%M%S>-${slug}.org".into(),
            ..Default::default()
        },
        body_file: Some(format!(
            "~/src/dhruvasagar/org-files/roam/templates/{file}.org"
        )),
        ..Default::default()
    };
    vec![
        //t("b", "book note", "book_note"),
        //t("p", "project", "project"),
        // The PKOS set. `c`/`m`/`f`/`r`/`s`/`o`/`x` are atomic knowledge
        // nodes; `l` is the index node for a complex piece (a book, a course),
        // and captures taken while reading go into that index rather than into
        // refile.
        t("c", "concept (PKOS)", "pkos-concept"),
        t("m", "mental model (PKOS)", "pkos-mental-model"),
        t("f", "framework (PKOS)", "pkos-framework"),
        t("r", "practice (PKOS)", "pkos-practice"),
        t("s", "source (PKOS)", "pkos-source"),
        t("o", "person (PKOS)", "pkos-person"),
        t("x", "experience (PKOS)", "pkos-experience"),
        t("l", "learning index (PKOS)", "pkos-learning-index"),
    ]
}

/// The one file the habit tracker's day sheets and episode rows share.
const HABIT_TRACKER: &str = "~/src/dhruvasagar/org-files/habit-tracker.org";

fn capture_templates() -> Vec<Template> {
    vec![
        Template {
            key: "t".into(),
            description: Some("todo".into()),
            target: Target {
                file: "/Users/dhruva/src/dhruvasagar/org-files/refile.org".into(),
                headline: None,
                ..Default::default()
            },
            body: Some("* TODO %?\n%U\n%a\n".into()),
            clock_in: None,
            ..Default::default()
        },
        Template {
            key: "r".into(),
            description: Some("respond".into()),
            target: Target {
                file: "/Users/dhruva/src/dhruvasagar/org-files/refile.org".into(),
                headline: None,
                ..Default::default()
            },
            body: Some("* NEXT Respond to %^{From} on %^{Subject}\nSCHEDULED: %t\n%U\n%a\n".into()),
            clock_in: None,
            ..Default::default()
        },
        Template {
            key: "w".into(),
            description: Some("review a link".into()),
            target: Target {
                file: "/Users/dhruva/src/dhruvasagar/org-files/refile.org".into(),
                headline: None,
                ..Default::default()
            },
            body: Some("* TODO Review %^{Link}\n%U\n".into()),
            clock_in: None,
            ..Default::default()
        },
        Template {
            key: "m".into(),
            description: Some("Meeting".into()),
            target: Target {
                file: "/Users/dhruva/src/dhruvasagar/org-files/refile.org".into(),
                headline: None,
                ..Default::default()
            },
            body: Some("* MEETING %? :MEETING:\n%U\n".into()),
            clock_in: None,
            ..Default::default()
        },
        Template {
            key: "p".into(),
            description: Some("Phone call".into()),
            target: Target {
                file: "/Users/dhruva/src/dhruvasagar/org-files/refile.org".into(),
                headline: None,
                ..Default::default()
            },
            body: Some("* PHONE %? :PHONE:\n%U\n".into()),
            clock_in: None,
            ..Default::default()
        },
        Template {
            key: "R".into(),
            description: Some("To Read".into()),
            target: Target {
                file: "/Users/dhruva/src/dhruvasagar/org-files/refile.org".into(),
                headline: None,
                ..Default::default()
            },
            body: Some("* TO-READ %? :READ:\n%U\n".into()),
            clock_in: None,
            ..Default::default()
        },
        Template {
            key: "W".into(),
            description: Some("To Watch".into()),
            target: Target {
                file: "/Users/dhruva/src/dhruvasagar/org-files/refile.org".into(),
                headline: None,
                ..Default::default()
            },
            body: Some("* TO-WATCH %? :WATCH:\n%U\n".into()),
            clock_in: None,
            ..Default::default()
        },
        Template {
            key: "h".into(),
            description: Some("Habit".into()),
            target: Target {
                file: "/Users/dhruva/src/dhruvasagar/org-files/habits.org".into(),
                headline: None,
                ..Default::default()
            },
            body: Some("* NEXT %?\nSCHEDULED: %^{Schedule, e.g. <2026-08-27 Thu .+1d/3d>}\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%U\n%a\n".into()),
            clock_in: None,
            ..Default::default()
        },
        // The habit-episode tracker: `H` files today's sheet under a datetree,
        // `u` appends one urge episode as a row of that day's table — seeding
        // the day from `H` when it is the first capture of the day.
        Template {
            key: "H".into(),
            description: Some("Habit tracker (today)".into()),
            target: Target {
                kind: Some("file+datetree".into()),
                file: HABIT_TRACKER.into(),
                ..Default::default()
            },
            body_file: Some("~/src/dhruvasagar/org-files/templates/habit-episode-tracker.org".into()),
            ..Default::default()
        },
        Template {
            key: "u".into(),
            description: Some("Urge episode (tracker row)".into()),
            target: Target {
                kind: Some("file+datetree".into()),
                file: HABIT_TRACKER.into(),
                sub_olp: Some(vec!["Urge / Habit Episode Tracker".into()]),
                ..Default::default()
            },
            r#type: Some("table-line".into()),
            // The first episode of a day creates the day's sheet from `H`.
            seed: Some("H".into()),
            body: Some("| %^{Time / Situation} | %^{Thoughts / Assumptions} | %^{Urge 0-10} | %^{STOP + NOTICE: what did I notice?} | %^{DELAY: how long?} | %^{ACTION / CHOICE: what did I do?} | %^{After 0-10} |".into()),
            ..Default::default()
        },
    ]
}

export!(Config);
