//! Icon and color resolver shared by FileTree and OilBuffer renderers.
//!
//! Glyph strings are Nerd Fonts v3 Unicode codepoints. When `nerd_fonts`
//! is false the glyph is `""` and only the style carries visual
//! differentiation. Resolution order: exact filename → extension → default.

use std::path::Path;
use ratatui::style::{Color, Style};

use crate::theme::Theme;

/// Returns `(glyph, style)` for a directory or file entry.
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
        ("", ext_color(name, ext))
    };
    let base_style = Style::new().fg(color);
    let style = if is_hidden { theme.file_tree_hidden_style } else { base_style };
    (glyph, style)
}

fn nerd_glyph_and_color(name: &str, ext: &str) -> (&'static str, Color) {
    // ── 1. Exact filename matches ─────────────────────────────────────────
    match name {
        // Build systems
        "Makefile" | "makefile" | "GNUmakefile" | "BSDmakefile" | "Makefile.am" =>
            return (" ", Color::from_u32(0x6D8086)),
        "CMakeLists.txt" =>
            return (" ", Color::from_u32(0x6D8086)),
        // Containers
        "Dockerfile" | "dockerfile" | "Containerfile" | "containerfile" =>
            return ("󰡨 ", Color::from_u32(0x458EE6)),
        "docker-compose.yml" | "docker-compose.yaml" | "compose.yml" | "compose.yaml" =>
            return ("󰡨 ", Color::from_u32(0x458EE6)),
        ".dockerignore" =>
            return ("󰡨 ", Color::DarkGray),
        // VCS
        ".gitignore" | ".gitattributes" | ".gitmodules" | ".gitconfig" | ".gitmessage" =>
            return ("󰒓 ", Color::from_u32(0x41535B)),
        // License
        "LICENSE" | "LICENCE" | "LICENSE.txt" | "LICENCE.txt" |
        "LICENSE.md" | "LICENCE.md" =>
            return (" ", Color::Yellow),
        // Config
        ".editorconfig" => return (" ", Color::DarkGray),
        ".env" | ".envrc" => return (" ", Color::from_u32(0xFAF743)),
        // Node.js
        "package.json" => return (" ", Color::from_u32(0xE8274B)),
        "package-lock.json" => return (" ", Color::DarkGray),
        ".npmrc" | ".nvmrc" | ".node-version" => return (" ", Color::from_u32(0xE8274B)),
        // TS/JS config
        "tsconfig.json" | "tsconfig.base.json" | "tsconfig.build.json" =>
            return (" ", Color::from_u32(0x519ABA)),
        "jsconfig.json" => return (" ", Color::from_u32(0xCBCB41)),
        ".eslintrc" | ".eslintrc.json" | ".eslintrc.js" |
        ".eslintrc.yml" | ".eslintrc.cjs" =>
            return (" ", Color::from_u32(0x4B32C3)),
        ".prettierrc" | ".prettierrc.json" | ".prettierrc.js" | ".prettierrc.yml" =>
            return (" ", Color::from_u32(0x56B3B4)),
        "vite.config.ts" | "vite.config.js" =>
            return (" ", Color::from_u32(0xBD34FE)),
        "vitest.config.ts" | "vitest.config.js" =>
            return (" ", Color::from_u32(0xBD34FE)),
        "webpack.config.js" | "webpack.config.ts" =>
            return ("󰜫 ", Color::from_u32(0x519ABA)),
        "next.config.js" | "next.config.ts" | "next.config.mjs" =>
            return (" ", Color::White),
        "nuxt.config.ts" | "nuxt.config.js" =>
            return (" ", Color::from_u32(0x00C58E)),
        "svelte.config.js" | "svelte.config.ts" =>
            return (" ", Color::from_u32(0xFF3E00)),
        "astro.config.mjs" | "astro.config.ts" =>
            return (" ", Color::from_u32(0xE8274B)),
        "tailwind.config.js" | "tailwind.config.ts" | "tailwind.config.cjs" =>
            return (" ", Color::from_u32(0x38BDF8)),
        "postcss.config.js" | "postcss.config.cjs" =>
            return (" ", Color::DarkGray),
        // Python
        "requirements.txt" | "requirements-dev.txt" | "requirements-test.txt" =>
            return (" ", Color::from_u32(0xFFBC03)),
        "Pipfile" | "Pipfile.lock" => return (" ", Color::from_u32(0xFFBC03)),
        "pyproject.toml" => return (" ", Color::from_u32(0xFFBC03)),
        "setup.py" | "setup.cfg" | "MANIFEST.in" => return (" ", Color::from_u32(0xFFBC03)),
        "uv.lock" => return (" ", Color::from_u32(0xDE5FE9)),
        // Ruby
        "Gemfile" | "Gemfile.lock" => return (" ", Color::from_u32(0xFF5252)),
        "Rakefile" => return (" ", Color::from_u32(0xFF5252)),
        // Go
        "go.mod" | "go.sum" | "go.work" => return (" ", Color::from_u32(0x519ABA)),
        // Rust
        "Cargo.toml" | "Cargo.lock" => return ("󱘗 ", Color::from_u32(0xFF8C00)),
        // Homebrew
        "Brewfile" | "Brewfile.lock.json" => return (" ", Color::from_u32(0xFBB040)),
        // Justfile (just task runner)
        "Justfile" | "justfile" => return (" ", Color::from_u32(0x6D8086)),
        // Heroku
        "Procfile" => return (" ", Color::DarkGray),
        _ => {}
    }

    // ── 2. Extension matches ──────────────────────────────────────────────
    match ext {
        // --- Systems languages ---
        "rs"                                        => ("󱘗 ", Color::from_u32(0xDEA584)),
        "c" | "h"                                   => (" ", Color::from_u32(0xA8B9CC)),
        "cpp" | "cxx" | "cc" | "c++"
        | "hpp" | "hxx" | "hh" | "h++"             => (" ", Color::from_u32(0xF34B7D)),
        "cs" | "csx"                                => ("󰌛 ", Color::from_u32(0x596706)),
        "java"                                      => (" ", Color::from_u32(0xCC3E44)),
        "kt" | "kts"                                => (" ", Color::from_u32(0x7F52FF)),
        "swift"                                     => (" ", Color::from_u32(0xE37933)),
        "go"                                        => (" ", Color::from_u32(0x519ABA)),
        "zig"                                       => (" ", Color::from_u32(0xF7A41D)),
        "nim" | "nims" | "nimble"                   => (" ", Color::from_u32(0xF3D400)),
        "v" | "vv"                                  => (" ", Color::from_u32(0x5D87BF)),
        "odin"                                      => (" ", Color::from_u32(0x60AEEF)),
        "asm" | "s" | "S" | "nasm"                 => (" ", Color::DarkGray),

        // --- Scripting / dynamic languages ---
        "py" | "pyw" | "pyi"                        => (" ", Color::from_u32(0xFFBC03)),
        "rb" | "erb" | "gemspec" | "rake"           => (" ", Color::from_u32(0xFF5252)),
        "php" | "php3" | "php4" | "php5" | "phtml" => (" ", Color::from_u32(0xA074C4)),
        "lua"                                       => (" ", Color::from_u32(0x51A0CF)),
        "pl" | "pm" | "t" | "pod"                  => (" ", Color::from_u32(0x519ABA)),
        "r" | "rmd" | "rnw" | "rproj"              => (" ", Color::from_u32(0x2266BA)),
        "jl"                                        => (" ", Color::from_u32(0xA270BA)),
        "dart"                                      => (" ", Color::from_u32(0x03589C)),
        "groovy" | "gvy"                            => (" ", Color::from_u32(0x4298B8)),
        "tcl" | "tk"                                => (" ", Color::DarkGray),

        // --- Functional languages ---
        "hs" | "lhs"                                => (" ", Color::from_u32(0xA074C4)),
        "ex" | "exs" | "heex" | "leex"             => (" ", Color::from_u32(0xA074C4)),
        "erl" | "hrl"                               => (" ", Color::from_u32(0xB83998)),
        "clj" | "cljs" | "cljc" | "edn"            => (" ", Color::from_u32(0x8DC149)),
        "scala" | "sbt" | "sc"                      => (" ", Color::from_u32(0xCC3E44)),
        "ml" | "mli" | "mll" | "mly"               => (" ", Color::from_u32(0xE37933)),
        "fs" | "fsi" | "fsx" | "fsproj"            => (" ", Color::from_u32(0x519ABA)),
        "elm"                                       => (" ", Color::from_u32(0x1293D8)),
        "cr"                                        => (" ", Color::DarkGray),
        "d" | "di"                                  => (" ", Color::from_u32(0xB03931)),
        "purs"                                      => (" ", Color::from_u32(0x14213D)),
        "rkt" | "rktl" | "rktd"                    => (" ", Color::from_u32(0x9F1D20)),
        "lisp" | "lsp" | "el" | "elc"              => (" ", Color::from_u32(0x9B59B6)),
        "scm" | "ss"                                => (" ", Color::from_u32(0x9B59B6)),

        // --- Shell / scripting ---
        "sh" | "bash" | "zsh" | "ksh" | "csh" | "tcsh" =>
            (" ", Color::from_u32(0x4D5A5E)),
        "fish"                                      => (" ", Color::from_u32(0x4D5A5E)),
        "ps1" | "psm1" | "psd1" | "ps1xml"         => ("󰨊 ", Color::from_u32(0x4273CA)),
        "bat" | "cmd"                               => (" ", Color::from_u32(0xC1F12E)),

        // --- Web / frontend ---
        "html" | "htm" | "xhtml" | "shtml"          => (" ", Color::from_u32(0xE44D26)),
        "css"                                       => (" ", Color::from_u32(0x42A5F5)),
        "scss" | "sass"                             => (" ", Color::from_u32(0xF55385)),
        "less"                                      => (" ", Color::from_u32(0x563D7C)),
        "js" | "mjs" | "cjs"                        => (" ", Color::from_u32(0xCBCB41)),
        "ts"                                        => (" ", Color::from_u32(0x519ABA)),
        "tsx"                                       => (" ", Color::from_u32(0x1354BF)),
        "jsx"                                       => (" ", Color::from_u32(0x519ABA)),
        "vue"                                       => (" ", Color::from_u32(0x8DC149)),
        "svelte"                                    => (" ", Color::from_u32(0xFF3E00)),
        "astro"                                     => (" ", Color::from_u32(0xE8274B)),
        "njk"                                       => ("󰗀 ", Color::from_u32(0x1BA354)),
        "hbs" | "handlebars" | "mustache"           => (" ", Color::from_u32(0xF7931E)),
        "ejs"                                       => (" ", Color::from_u32(0xA91E50)),
        "pug" | "jade"                              => (" ", Color::from_u32(0xE44B23)),
        "coffee" | "litcoffee"                      => (" ", Color::from_u32(0xCBCB41)),
        "wasm"                                      => (" ", Color::from_u32(0x5C4CDB)),
        "wat"                                       => (" ", Color::from_u32(0x5C4CDB)),

        // --- Config / data ---
        "json" | "jsonc" | "json5"                  => (" ", Color::from_u32(0xCBCB41)),
        "yaml" | "yml"                              => (" ", Color::from_u32(0x6D8086)),
        "toml"                                      => (" ", Color::from_u32(0x9C4221)),
        "xml" | "xsl" | "xslt" | "xsd" | "dtd"
        | "rss" | "atom" | "plist"                  => ("󰗀 ", Color::from_u32(0xE37933)),
        "ini" | "cfg" | "conf" | "config"
        | "properties" | "prop" | "rc"             => (" ", Color::from_u32(0x6D8086)),
        "env" | "envrc" | "dotenv"                  => (" ", Color::from_u32(0xFAF743)),
        "csv" | "tsv"                               => (" ", Color::from_u32(0x89E051)),

        // --- Database / query ---
        "sql" | "mysql" | "pgsql" | "psql" | "plsql" | "hql" =>
            (" ", Color::from_u32(0xDAD8D8)),
        "sqlite" | "sqlite3" | "db"                 => (" ", Color::from_u32(0x003B57)),
        "prisma"                                    => (" ", Color::from_u32(0x5A67D8)),
        "graphql" | "gql"                           => (" ", Color::from_u32(0xE535AB)),

        // --- Docs / text ---
        "md" | "mdx" | "markdown"                   => ("󰍔 ", Color::White),
        "rst"                                       => (" ", Color::White),
        "txt" | "text"                              => (" ", Color::White),
        "pdf"                                       => (" ", Color::from_u32(0xB30B00)),
        "doc" | "docx" | "odt" | "rtf"             => (" ", Color::from_u32(0x295394)),
        "xls" | "xlsx" | "ods" | "numbers"          => (" ", Color::from_u32(0x217346)),
        "ppt" | "pptx" | "odp" | "keynote"          => (" ", Color::from_u32(0xB7472A)),
        "tex" | "ltx" | "sty" | "bib" | "cls"      => (" ", Color::DarkGray),
        "org"                                       => (" ", Color::from_u32(0x77AA99)),
        "adoc" | "asciidoc"                         => (" ", Color::White),
        "man" | "roff" | "groff"                    => (" ", Color::DarkGray),
        "ipynb"                                     => (" ", Color::from_u32(0xF37726)),

        // --- Images ---
        "png" | "jpg" | "jpeg" | "gif" | "bmp"
        | "webp" | "tiff" | "tif" | "ico" | "psd"
        | "xcf" | "heic" | "heif" | "avif"          => (" ", Color::from_u32(0xA074C4)),
        "svg" | "svgz"                              => ("󰜡 ", Color::from_u32(0xFFB13B)),
        "ai" | "eps"                                => (" ", Color::from_u32(0xFF7C00)),
        "sketch" | "fig" | "figma"                  => (" ", Color::from_u32(0xFDB300)),

        // --- Video / audio ---
        "mp4" | "mkv" | "avi" | "mov" | "wmv"
        | "flv" | "webm" | "m4v" | "ogv"           => (" ", Color::from_u32(0xFD971F)),
        "mp3" | "ogg" | "flac" | "wav" | "m4a"
        | "aac" | "wma" | "opus" | "aiff"           => ("󰎆 ", Color::from_u32(0x00AFFF)),

        // --- Archives ---
        "zip" | "tar" | "gz" | "tgz" | "bz2"
        | "xz" | "zst" | "lz4" | "7z" | "rar"
        | "cab" | "iso"                             => (" ", Color::from_u32(0xECA517)),
        "deb" | "rpm" | "pkg" | "apk" | "snap"     => (" ", Color::from_u32(0xECA517)),

        // --- Fonts ---
        "ttf" | "otf" | "woff" | "woff2" | "eot"  => (" ", Color::from_u32(0xFFAFF3)),

        // --- Certificates / crypto ---
        "pem" | "key" | "crt" | "cer" | "der"
        | "p12" | "pfx" | "p8" | "pub"             => ("󰌇 ", Color::from_u32(0xD4AF37)),
        "gpg" | "asc" | "sig"                       => ("󰌇 ", Color::from_u32(0xD4AF37)),

        // --- Binaries ---
        "exe" | "msi" | "app"                       => (" ", Color::from_u32(0x9F0500)),
        "dll" | "so" | "dylib" | "lib" | "a"       => (" ", Color::DarkGray),
        "o" | "obj" | "pyc" | "class"              => (" ", Color::DarkGray),

        // --- Patch / diff ---
        "patch" | "diff"                            => (" ", Color::from_u32(0x41535B)),

        // --- VCS ---
        "gitignore" | "gitmodules" | "gitattributes" | "gitconfig" =>
            ("󰒓 ", Color::from_u32(0x41535B)),

        // --- Infrastructure / cloud ---
        "dockerfile" | "containerfile"              => ("󰡨 ", Color::from_u32(0x458EE6)),
        "tf" | "tfvars"                             => (" ", Color::from_u32(0x844FBA)),
        "hcl"                                       => (" ", Color::from_u32(0x844FBA)),
        "nix"                                       => (" ", Color::from_u32(0x7EB7E4)),
        "proto" | "protobuf"                        => (" ", Color::from_u32(0x5BA4CF)),
        "thrift"                                    => (" ", Color::DarkGray),
        "k8s" | "helm"                              => (" ", Color::from_u32(0x326CE5)),

        // --- Build files ---
        "mk" | "mak"                                => (" ", Color::from_u32(0x6D8086)),
        "cmake"                                     => (" ", Color::from_u32(0x6D8086)),
        "bazel" | "bzl"                             => (" ", Color::from_u32(0x76D275)),
        "ninja"                                     => (" ", Color::DarkGray),
        "gradle"                                    => (" ", Color::from_u32(0x4298B8)),

        // --- Misc dev ---
        "vim" | "vimrc" | "nvimrc"                  => (" ", Color::from_u32(0x019833)),
        "lock"                                      => ("󰌾 ", Color::DarkGray),
        "log"                                       => (" ", Color::DarkGray),
        "pid"                                       => (" ", Color::DarkGray),
        "tmp" | "temp"                              => (" ", Color::DarkGray),
        "bak" | "backup" | "old"                    => (" ", Color::DarkGray),
        "swp" | "swo"                               => (" ", Color::DarkGray),
        "mdump" | "dmp"                             => (" ", Color::DarkGray),

        // --- Default ---
        _                                           => (" ", Color::Reset),
    }
}

fn ext_color(name: &str, ext: &str) -> Color {
    // Exact name overrides for no-icon mode
    match name {
        "Makefile" | "makefile" | "CMakeLists.txt" => return Color::from_u32(0x6D8086),
        "Dockerfile" | "dockerfile" => return Color::Blue,
        ".gitignore" | ".gitattributes" | ".gitmodules" => return Color::DarkGray,
        "LICENSE" | "LICENCE" => return Color::Yellow,
        _ => {}
    }
    match ext {
        "rs"                                    => Color::from_u32(0xDEA584),
        "c" | "h" | "cpp" | "cxx" | "hpp"      => Color::Blue,
        "cs" | "java" | "scala"                 => Color::from_u32(0xCC3E44),
        "kt" | "kts"                            => Color::from_u32(0x7F52FF),
        "swift"                                 => Color::from_u32(0xE37933),
        "go"                                    => Color::Cyan,
        "zig" | "nim"                           => Color::Yellow,
        "py" | "pyw" | "pyi"                    => Color::from_u32(0xFFBC03),
        "rb" | "erb"                            => Color::from_u32(0xFF5252),
        "php"                                   => Color::from_u32(0xA074C4),
        "lua"                                   => Color::Blue,
        "r" | "jl"                              => Color::Blue,
        "dart"                                  => Color::Cyan,
        "hs" | "lhs"                            => Color::from_u32(0xA074C4),
        "ex" | "exs"                            => Color::from_u32(0xA074C4),
        "erl" | "hrl"                           => Color::from_u32(0xB83998),
        "clj" | "cljs"                          => Color::Green,
        "js" | "mjs" | "cjs" | "coffee"        => Color::from_u32(0xCBCB41),
        "ts" | "tsx" | "jsx"                    => Color::Blue,
        "html" | "htm"                          => Color::from_u32(0xE44D26),
        "css" | "less"                          => Color::Blue,
        "scss" | "sass"                         => Color::from_u32(0xF55385),
        "vue" | "svelte"                        => Color::Green,
        "toml"                                  => Color::Yellow,
        "json" | "jsonc" | "json5"              => Color::Yellow,
        "yaml" | "yml"                          => Color::Green,
        "xml"                                   => Color::from_u32(0xE37933),
        "sh" | "bash" | "zsh" | "fish"         => Color::Green,
        "ps1" | "psm1"                          => Color::Blue,
        "vim" | "vimrc"                         => Color::Green,
        "md" | "mdx" | "rst" | "txt"           => Color::White,
        "sql"                                   => Color::DarkGray,
        "graphql" | "gql"                       => Color::from_u32(0xE535AB),
        "tf" | "hcl"                            => Color::from_u32(0x844FBA),
        "nix"                                   => Color::Blue,
        "prisma"                                => Color::from_u32(0x5A67D8),
        "lock"                                  => Color::DarkGray,
        "log"                                   => Color::DarkGray,
        "png" | "jpg" | "jpeg" | "gif" | "svg" |
        "bmp" | "webp" | "ico"                  => Color::from_u32(0xA074C4),
        "mp4" | "mkv" | "avi" | "mov"          => Color::from_u32(0xFD971F),
        "mp3" | "flac" | "wav" | "ogg"         => Color::Cyan,
        "zip" | "tar" | "gz" | "bz2" | "7z" | "rar" => Color::from_u32(0xECA517),
        "pdf"                                   => Color::from_u32(0xB30B00),
        _                                       => Color::Reset,
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
        assert_eq!(style.fg, Some(Color::from_u32(0xDEA584)));
    }

    #[test]
    fn hidden_file_uses_hidden_style() {
        let (_, style) = icon_for_entry(&PathBuf::from(".gitignore"), false, true, &theme());
        assert_eq!(style, theme().file_tree_hidden_style);
    }

    #[test]
    fn unknown_ext_falls_back_to_default_file_glyph() {
        let (glyph, _) = icon_for_entry(&PathBuf::from("binary.bin"), false, true, &theme());
        assert_eq!(glyph, " ");
    }

    #[test]
    fn dockerfile_exact_name_match() {
        let (glyph, style) = icon_for_entry(&PathBuf::from("Dockerfile"), false, true, &theme());
        assert_eq!(glyph, "󰡨 ");
        assert_eq!(style.fg, Some(Color::from_u32(0x458EE6)));
    }

    #[test]
    fn makefile_exact_name_match() {
        let (glyph, _) = icon_for_entry(&PathBuf::from("Makefile"), false, true, &theme());
        assert_eq!(glyph, " ");
    }

    #[test]
    fn typescript_tsx_gets_distinct_color() {
        let (_, ts_style) = icon_for_entry(&PathBuf::from("app.ts"), false, true, &theme());
        let (_, tsx_style) = icon_for_entry(&PathBuf::from("App.tsx"), false, true, &theme());
        assert_ne!(ts_style.fg, tsx_style.fg);
    }

    #[test]
    fn image_extensions_resolve() {
        for ext in &["png", "jpg", "gif", "webp", "svg"] {
            let path = PathBuf::from(format!("img.{ext}"));
            let (glyph, _) = icon_for_entry(&path, false, true, &theme());
            assert!(!glyph.is_empty(), "no glyph for .{ext}");
        }
    }

    #[test]
    fn archive_extensions_resolve() {
        for ext in &["zip", "tar", "gz", "7z", "rar"] {
            let path = PathBuf::from(format!("archive.{ext}"));
            let (glyph, _) = icon_for_entry(&path, false, true, &theme());
            assert!(!glyph.is_empty(), "no glyph for .{ext}");
        }
    }

    #[test]
    fn nerd_fonts_false_returns_empty_glyph_for_file() {
        let (glyph, _) = icon_for_entry(&PathBuf::from("main.rs"), false, false, &theme());
        assert_eq!(glyph, "");
    }

    #[test]
    fn graphql_gets_pink_color() {
        let (_, style) = icon_for_entry(&PathBuf::from("schema.graphql"), false, true, &theme());
        assert_eq!(style.fg, Some(Color::from_u32(0xE535AB)));
    }

    #[test]
    fn license_exact_name_match() {
        let (glyph, _) = icon_for_entry(&PathBuf::from("LICENSE"), false, true, &theme());
        assert_eq!(glyph, " ");
    }
}
