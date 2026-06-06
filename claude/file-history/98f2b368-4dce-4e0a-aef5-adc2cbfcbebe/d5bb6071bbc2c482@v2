// `linkme`'s distributed slices use `link_section` to aggregate
// items at link time. The macro expansions in this file emit
// such declarations; allow the workspace's `unsafe_code = "deny"`
// lint locally with the same safety rationale documented in
// `lattice-config`'s `option_decl.rs` etc.
#![allow(unsafe_code)]

//! Renderer-neutral UI / theme options.
//!
//! Phase 5.5.E.6: relocated from `lattice-ui-tui::tui_options` so
//! the host can read these values directly when synchronising
//! [`crate::ui::theme::Theme`] from config. Every option here
//! drives the renderer-neutral [`crate::ui::theme::Theme`]; the
//! adapter into a renderer's native style type lives in that
//! renderer's crate.
//!
//! Self-registration via `linkme` -- the registry's
//! `init_from_linkme()` walks the slice at App boot regardless of
//! which crate emitted the entries, so linking the host crate
//! into any renderer picks these up automatically.
//!
//! Storage shape: every option stores its current *primitive*
//! value (`bool` for the toggle, `String` for the color name and
//! the separator glyph) in the config. The renderer-specific
//! derived style table (TUI's [`ratatui::style::Style`], GPUI's
//! `Hsla`, ...) is kept as a cached projection on the renderer
//! and refreshed after every `:set` cascade via the
//! `RendererSignal::ThemeChanged` signal.

use crate::ui::theme::parse_color;

// Validators referenced by `#[validate(...)]` attributes below.
fn validate_separator(s: &String) -> Result<(), String> {
    if s.chars().count() == 1 {
        Ok(())
    } else {
        Err(format!(
            "ui.separator must be one character, got `{s}` ({} chars)",
            s.chars().count()
        ))
    }
}

fn validate_color(s: &String) -> Result<(), String> {
    parse_color(s).map(|_| ())
}

fn validate_font_size(n: &i64) -> Result<(), String> {
    if *n >= 4 && *n <= 96 {
        Ok(())
    } else {
        Err(format!("ui.font_size must be in range [4, 96], got {n}"))
    }
}

// Group binding: TUI-specific UI options under the `appearance`
// group (theme / colors / sprite icons). User customizes via
// `:customize appearance`.
lattice_config::options! {
    group = lattice_config::Appearance;

    /// Apply a `DIM` overlay on inactive panes' syntax-highlighted
    /// text so the active pane stands out without losing color.
    #[name("ui.dim_inactive")]
    pub UiDimInactive: bool = true;

    /// Character drawn in the column separating side-by-side
    /// panes (default `│`).
    #[name("ui.separator")]
    #[validate(validate_separator)]
    pub UiSeparator: String = String::from("│");

    /// Foreground color of the pane separator. Accepts named ANSI
    /// colors (red, blue, darkgray, ...) and `default` for the
    /// terminal default.
    #[name("ui.separator_color")]
    #[validate(validate_color)]
    pub UiSeparatorColor: String = String::from("darkgray");

    /// Foreground color of the active pane's status line.
    #[name("ui.statusline_active_fg")]
    #[validate(validate_color)]
    pub UiStatuslineActiveFg: String = String::from("default");

    /// Foreground color of inactive panes' status lines.
    #[name("ui.statusline_inactive_fg")]
    #[validate(validate_color)]
    pub UiStatuslineInactiveFg: String = String::from("darkgray");

    /// Whether to render file-type icons as Nerd Fonts v3 glyphs.
    ///
    /// `true` -- expects a Nerd Font (FiraCode Nerd Font, JetBrains
    /// Mono Nerd Font, Hack Nerd Font, ...) configured as the
    /// terminal font; produces the rich per-language icon set.
    ///
    /// `false` (default) -- renders a BMP-block fallback palette
    /// (◆ ≡ ◇ ■ ♪ ▶ ·) that works in every modern monospace font.
    /// Pick this if you see `?` boxes in the file tree / oil.
    #[name("ui.nerd_fonts")]
    pub UiNerdFonts: bool = false;

    /// Font family used by the GPUI (native window) renderer.
    /// Accepts a single font family name. The font MUST be a
    /// monospace typeface — proportional fonts produce incorrect
    /// cursor placement because the advance-width measurement
    /// assumes all glyphs share the same cell width.
    ///
    /// macOS ships "Menlo" (system monospace since 10.6). Other
    /// common choices: "Monaco", "JetBrains Mono", "Fira Code",
    /// "Cascadia Code", "Hack". The TUI renderer does not read
    /// this option — its font is configured in the terminal
    /// emulator.
    #[name("ui.font_family")]
    pub UiFontFamily: String = String::from("Menlo");

    /// Font size in points for the GPUI (native window) renderer.
    /// Must be a positive integer. The TUI renderer ignores this
    /// option — font size is controlled by the terminal emulator.
    #[name("ui.font_size")]
    #[validate(validate_font_size)]
    pub UiFontSize: i64 = 14;
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used, clippy::panic)]
    use super::*;
    use lattice_config::ConfigRegistry;

    #[test]
    fn type_keyed_reads_work_post_boot() {
        let r = ConfigRegistry::new();
        r.init_from_linkme();
        assert!(*r.get_typed::<UiDimInactive>().unwrap());
        assert_eq!(r.get_typed::<UiSeparator>().unwrap().as_str(), "│");
        assert_eq!(
            r.get_typed::<UiSeparatorColor>().unwrap().as_str(),
            "darkgray"
        );
        assert_eq!(
            r.get_typed::<UiStatuslineActiveFg>().unwrap().as_str(),
            "default"
        );
        assert_eq!(
            r.get_typed::<UiStatuslineInactiveFg>().unwrap().as_str(),
            "darkgray"
        );
        // BMP fallback is the default so the first frame renders
        // in any terminal font.
        assert!(!*r.get_typed::<UiNerdFonts>().unwrap());
    }

    #[test]
    fn nerd_fonts_flag_parses_on_off() {
        let r = ConfigRegistry::new();
        r.init_from_linkme();
        r.parse_and_set_command("ui.nerd_fonts=on").unwrap();
        assert!(*r.get_typed::<UiNerdFonts>().unwrap());
        r.parse_and_set_command("ui.nerd_fonts=off").unwrap();
        assert!(!*r.get_typed::<UiNerdFonts>().unwrap());
    }

    #[test]
    fn separator_validate_rejects_multi_char_strings() {
        let r = ConfigRegistry::new();
        r.init_from_linkme();
        let err = r.parse_and_set_command("ui.separator=ab").unwrap_err();
        let msg = format!("{err}");
        assert!(msg.contains("must be one character"), "got `{msg}`");
    }

    #[test]
    fn separator_color_validate_rejects_invalid_color_names() {
        let r = ConfigRegistry::new();
        r.init_from_linkme();
        let err = r
            .parse_and_set_command("ui.separator_color=puce")
            .unwrap_err();
        let msg = format!("{err}");
        // host theme parser's error wording: "unknown color: puce".
        assert!(msg.contains("puce"), "got `{msg}`");
    }
}
