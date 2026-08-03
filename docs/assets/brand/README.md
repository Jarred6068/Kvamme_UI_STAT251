# Brand assets

University of Idaho branding for the STAT251-03 course site.

Everything a page needs must live inside this repo (see the root `CLAUDE.md`), so
these files are **vendored copies**, not references to a sibling checkout.

## Files

| File | Origin | Edit? |
|---|---|---|
| `uidaho-branding.css` | Verbatim copy from [`ui-iids/brand-assets`](https://github.com/ui-iids/brand-assets) `styles/uidaho-branding.css`, commit `5588eaa` (repo HEAD `b6c29b8`, 2026-06-25) | **No.** Re-copy from upstream instead. |
| `site.css` | Written for this site | Yes |
| `theme-init.html` | Written for this site | Yes |
| `theme-toggle.html` | Written for this site | Yes |

## The rule that governs this directory

From the upstream brand guidance:

> Always reference variables; never hardcode `#191919` or `#F1B300`.

`uidaho-branding.css` defines the entire official palette — four colours:

| Token | Value | Role |
|---|---|---|
| `--uidaho-pride-gold` | `#F1B300` | primary |
| `--uidaho-silver` | `#808080` | secondary / muted |
| `--uidaho-white` | `#FFFFFF` | surface |
| `--uidaho-black` | `#191919` | text, navbar, hero |

Metallic Gold is deliberately excluded upstream (special-use only, requires
U of I Creative Services).

`site.css` never redefines a `--uidaho-*` token. It adds a local `--site-*`
semantic layer that *points at* them and re-points per theme — the sanctioned
recolouring mechanism.

## Gold contrast policy

`#F1B300` is **1.88:1 on white** and **9.37:1 on `#191919`**. Consequently:

- **Light mode:** gold is decorative only — rules, borders, fills, underlines.
  Never gold text on a white background.
- **Dark mode:** gold is legal as body/link text.
- **Buttons:** gold fill with **black** text in both modes. White-on-gold is also
  1.88:1 and must never be used.
- **Focus rings:** gold alone is insufficient in light mode; pair it with a
  text-coloured outline.

## Three things this site invents

The brand does not define them; they are derived in-family here and are
candidates for an upstream PR rather than a permanent local fork:

1. **Dark-mode tokens.** All derived by alpha-compositing the four palette
   colours over black (`rgba(var(--uidaho-white-rgb), .06)` etc.), matching the
   brand's own `rgba(white,.06)` hover and `rgba(silver,.4)` separator idioms.
2. **A monospace stack.** Needed for knitted R code; upstream defines only
   `--uidaho-brand-font-sans`.
3. **`<table>` styling.** Upstream uses row-lists, not real tables; a course
   site full of kableExtra output needs the real thing.

## Constraints on `site.css`

- **No `url()` anywhere.** The 29 subdirectory pages render self-contained,
  which inlines this stylesheet; pandoc then re-resolves `url()` against a
  different base and the reference breaks silently.
- **Keep the `--site-brand-version` declaration in `:root`.** It is how you
  verify a rendered page actually picked up the branding, and it has to stay a
  *declaration* — pandoc strips CSS comments when it inlines this stylesheet
  into the self-contained subdirectory pages, so a comment-based marker
  vanishes on exactly the pages most worth checking.

  A coverage check must also accept *either* form: top-level pages **link** the
  stylesheet (their HTML contains `brand/site.css`), while subdirectory pages
  **inline** it (their HTML contains `--site-brand-version`). See the header
  comment in `site.css` for the working command.
