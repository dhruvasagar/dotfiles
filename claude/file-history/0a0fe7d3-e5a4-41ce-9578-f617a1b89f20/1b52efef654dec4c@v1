//! Icon and color resolver shared by FileTree and OilBuffer renderers.

use std::path::Path;
use ratatui::style::{Color, Modifier, Style};

use crate::theme::Theme;

/// Returns `(glyph, style)` for a directory or file entry.
/// When `nerd_fonts` is false the glyph is `""` (no column rendered)
/// and only the style carries visual differentiation.
pub fn icon_for_entry(path: &Path, is_dir: bool, nerd_fonts: bool, theme: &Theme) -> (&'static str, Style) {
    if is_dir {
        let glyph = if nerd_fonts { "󰉋 " } else { "" };
        return (glyph, theme.file_tree_dir_style);
    }
    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
    let ext  = path.extension().and_then(|e| e.to_str()).unwrap_or("");
    let is_hidden = name.starts_with('.');
    let (glyph, color) = if nerd_fonts {
        nerd_glyph_and_color(name, ext)
    } else {
        ("", ext_color(ext))
    };
    let base_style = Style::new().fg(color);
    let style = if is_hidden {
        theme.file_tree_hidden_style
    } else {
        base_style
    };
    (glyph, style)
}

fn nerd_glyph_and_color(name: &str, ext: &str) -> (&'static str, Color) {
    match ext {
        "rs"                            => ("󱘗 ", Color::from_u32(0xFF8C00)), // orange
        "toml"                          => (" ", Color::Yellow),
        "json" | "jsonc"                => (" ", Color::Yellow),
        "md" | "mdx"                    => ("󰍔 ", Color::White),
        "sh" | "bash" | "zsh" | "fish" => (" ", Color::Green),
        "py"                            => (" ", Color::Yellow),
        "js" | "mjs" | "cjs"           => (" ", Color::Yellow),
        "ts" | "tsx"                    => (" ", Color::Blue),
        "jsx"                           => (" ", Color::Cyan),
        "html" | "htm"                  => (" ", Color::from_u32(0xFF6600)),
        "css" | "scss" | "sass"         => (" ", Color::Magenta),
        "go"                            => (" ", Color::Cyan),
        "c" | "h"                       => (" ", Color::Blue),
        "cpp" | "cxx" | "cc" | "hpp"   => (" ", Color::Blue),
        "java"                          => (" ", Color::from_u32(0xFF8C00)),
        "kt" | "kts"                    => (" ", Color::Magenta),
        "swift"                         => (" ", Color::from_u32(0xFF5533)),
        "zig"                           => (" ", Color::Yellow),
        "lua"                           => (" ", Color::Blue),
        "vim"                           => (" ", Color::Green),
        "yaml" | "yml"                  => (" ", Color::Green),
        "xml"                           => ("󰗀 ", Color::from_u32(0xFF8C00)),
        "sql"                           => (" ", Color::Blue),
        "lock"                          => ("󰌾 ", Color::DarkGray),
        "gitignore" | "gitmodules"
        | "gitattributes"               => ("󰒓 ", Color::DarkGray),
        "dockerfile" | "containerfile"  => (" ", Color::Blue),
        _                               => (" ", Color::Reset),
    }
}

fn ext_color(ext: &str) -> Color {
    match ext {
        "rs"                            => Color::from_u32(0xFF8C00),
        "toml" | "json" | "jsonc"       => Color::Yellow,
        "sh" | "bash" | "zsh" | "fish"
        | "go" | "kt" | "kts"          => Color::Green,
        "py" | "js" | "mjs" | "cjs"    => Color::Yellow,
        "ts" | "tsx" | "c" | "h"
        | "cpp" | "cxx" | "hpp"        => Color::Blue,
        "md" | "mdx"                    => Color::White,
        _                              => Color::Reset,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::theme::Theme;
    use std::path::PathBuf;

    fn theme() -> Theme { Theme::default() }

    #[test]
    fn directory_returns_dir_style() {
        let (glyph, style) = icon_for_entry(&PathBuf::from("src"), true, true, &theme());
        assert_eq!(glyph, "󰉋 ");
        assert_eq!(style, theme().file_tree_dir_style);
    }

    #[test]
    fn nerd_fonts_false_returns_empty_glyph_for_dir() {
        let (glyph, _) = icon_for_entry(&PathBuf::from("src"), true, false, &theme());
        assert_eq!(glyph, "");
    }

    #[test]
    fn rust_file_gets_orange_glyph() {
        let (glyph, style) = icon_for_entry(&PathBuf::from("main.rs"), false, true, &theme());
        assert_eq!(glyph, "󱘗 ");
        assert_eq!(style.fg, Some(Color::from_u32(0xFF8C00)));
    }

    #[test]
    fn hidden_file_uses_hidden_style() {
        let (_, style) = icon_for_entry(&PathBuf::from(".gitignore"), false, true, &theme());
        assert_eq!(style, theme().file_tree_hidden_style);
    }

    #[test]
    fn unknown_ext_falls_back_to_default_file_glyph() {
        let (glyph, _) = icon_for_entry(&PathBuf::from("Makefile"), false, true, &theme());
        assert_eq!(glyph, " ");
    }
}
