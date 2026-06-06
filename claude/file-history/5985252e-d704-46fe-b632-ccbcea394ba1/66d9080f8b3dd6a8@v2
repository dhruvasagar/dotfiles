# Lattice — Brand Assets

This directory contains the Lattice logo system and its asset variants.

## The mark

The mark is built from three glyphs: an **L** (for Lattice), a **7** (the L's
180° rotation), and a block **cursor** that sits inside the frame. The L
and 7 touch at one corner each, leaving two opposite-corner notches that
give the mark 180° rotational symmetry.

Every dimension derives from a single 20-unit module:

| Element        | Dimensions |
| -------------- | ---------- |
| Stroke width   | 20         |
| Frame width    | 100        |
| Frame height   | 120        |
| Cursor         | 20 × 48    |
| Corner notch   | 20 × 20    |

## Palette

The entire brand is one color plus an accent:

| Role          | Hex       | Notes                                            |
| ------------- | --------- | ------------------------------------------------ |
| Brand blue    | `#1f6feb` | Mark, wordmark, banner — one color, everywhere   |
| Cursor accent | `#f59e0b` | The only warm color; carries the focal point     |
| Body slate    | `#64748b` | Tagline and supporting text only                 |

Brand blue works on both light and dark backgrounds without modification.
The amber cursor stays the same in all contexts. There are no light/dark
variants — one logo, one palette, every context.

A previous two-color wordmark (navy on light, off-white on dark) was
explored and rejected because it required a `<picture>` element trick on
GitHub and a CSS workaround everywhere else. Unifying the wordmark with
the mark's blue solves the problem structurally: the brand becomes one
color decision.

## Clear space

Reserve a minimum margin of one stroke-width (20% of the mark's width)
on all sides. Don't place text, borders, or other graphic elements
inside this zone.

## Minimum sizes

- **Mark alone:** 16px wide minimum (use `favicon-16.svg`, which is
  hand-tuned for pixel-perfect rendering at this size)
- **Lockup (mark + wordmark):** 120px wide minimum
- **Banner:** 600px wide minimum

## Files

### Marks

- `lattice-mark.svg` — primary mark, 100×120 viewBox, scale to any size
- `lattice-mark-mono.svg` — monochrome variant using `currentColor` for
  the frame and 60% opacity for the cursor. Use this for single-color
  contexts (embroidery, screen-printing, embossing). Color is controlled
  by the parent's `color` CSS property.
- `lattice-mark-512.png` — 512×614 PNG render of the master mark

### Lockup

- `lattice-lockup.svg` — mark + "Lattice" wordmark. Same file for light
  and dark backgrounds; the brand blue works on both.
- `lattice-lockup-1200.png` — 1200×350 PNG fallback

### README

- `readme-banner.svg` — wide banner with mark, wordmark, and tagline
- `readme-banner.png` — 1280×280 PNG fallback

### Favicons

- `favicon.ico` — multi-resolution ICO containing 16, 32, and 64px
- `favicon-16.svg` — pixel-tuned variant for 16×16 rendering
- `favicon-32.svg` — for 32×32
- `favicon-64.svg` — for 64×64 and above
- `favicon-16.png`, `favicon-32.png`, `favicon-64.png` — PNG counterparts
- `apple-touch-icon.png` — 180×180, for iOS home screen

### App bundle (desktop)

- `lattice.icns` — macOS `.icns` file (10 sizes from 16 to 1024px@2x) for
  the macOS `.app` bundle. Generated via `iconutil` from `lattice.iconset/`.
  Used by `cargo bundle --features gui` automatically.
- `lattice.iconset/` — iconset directory used as the source for `lattice.icns`.
  Regenerate with:
  ```sh
  sips -Z 512 assets/lattice-mark-512.png --out /tmp/lattice-scaled.png
  sips -p 512 512 /tmp/lattice-scaled.png --out /tmp/lattice-512sq.png
  # … populate lattice.iconset/ at all standard sizes …
  iconutil -c icns assets/lattice.iconset --output assets/lattice.icns
  ```
- `linux/com.lattice-editor.lattice.desktop` — XDG `.desktop` entry for Linux
  desktop environments. Install system-wide with:
  ```sh
  sudo cp assets/linux/com.lattice-editor.lattice.desktop \
           /usr/share/applications/
  sudo cp assets/favicon-64.png \
           /usr/share/icons/hicolor/64x64/apps/com.lattice-editor.lattice.png
  gtk-update-icon-cache -f /usr/share/icons/hicolor/
  ```

## Web integration

Drop into a project's `<head>`:

```html
<link rel="icon" type="image/x-icon" href="/favicon.ico">
<link rel="icon" type="image/svg+xml" href="/favicon-64.svg">
<link rel="apple-touch-icon" href="/apple-touch-icon.png">
```

For the README, a single line:

```markdown
![Lattice](./assets/readme-banner.svg)
```

No `<picture>` element needed — the banner works on both GitHub themes.

## Don'ts

- Don't alter the proportions — every dimension is derived from the
  20-unit module
- Don't add corner radius — was tested, rejected; sharp corners are part
  of the mark's identity
- Don't change the cursor color (the amber is the only saturated accent
  and carries the entire focal-point load)
- Don't separate the cursor from the frame — it lives inside the frame,
  not beside it
- Don't tilt or rotate the mark — the 180° symmetry means rotation
  produces the same image; intermediate rotations look broken
- Don't introduce light/dark color variants — the unified blue is the
  locked spec, and it works on both backgrounds
- Don't substitute a darker blue or navy for the wordmark to "improve
  contrast" — the unified blue is a brand decision, not a fallback

## Accessibility notes

The brand blue (`#1f6feb`) on white (`#ffffff`) has a contrast ratio of
3.9:1 — sufficient for large display text (WCAG AA: 3:1) but below the
4.5:1 threshold for body copy. Use brand blue for the wordmark and large
display contexts; use a darker color (e.g. `#0d2a4a` or `#1e293b`) for
running prose, links, or any text smaller than ~18pt.

On dark backgrounds (`#1a1a1a`), the contrast ratio is 5.4:1 — passes
WCAG AA for normal text. Brand blue works comfortably for headings and
display text in both themes.
