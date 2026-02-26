# Scientific Observatory UI Redesign

**Date:** 2026-02-26
**Status:** Approved

## Summary

Full visual redesign of the AQUABC Shiny app from generic Bootstrap to a custom "Bathymetric Observatory" dark theme. Includes CSS extraction, layout restructuring, typography upgrade, and removal of the shinyswatch theme system.

## Aesthetic Direction

Dark, immersive interface evoking an underwater research station control panel.

### Color Palette

| Variable | Value | Usage |
|----------|-------|-------|
| `--bg-deep` | `#0f1923` | Page background |
| `--bg-surface` | `#162231` | Sidebar, elevated surfaces |
| `--bg-card` | `rgba(22, 34, 49, 0.7)` | Card backgrounds (glassmorphism) |
| `--border` | `rgba(14, 165, 233, 0.15)` | Card borders, dividers |
| `--accent` | `#0ea5e9` | Active states, links, highlights |
| `--accent-glow` | `rgba(14, 165, 233, 0.3)` | Sidebar glow, focus rings |
| `--text` | `#e2e8f0` | Primary text |
| `--text-muted` | `#94a3b8` | Secondary text, descriptions |
| `--success` | `#10b981` | Run, Save actions |
| `--danger` | `#ef4444` | Stop, Delete actions |
| `--warning` | `#f59e0b` | Rebuild, caution actions |

### Typography

- **Headers:** Instrument Serif (Google Fonts) — scientific journal feel
- **Body:** IBM Plex Sans — designed for data-dense interfaces
- **Data/code:** IBM Plex Mono — parameter names, values, logs

### Visual Effects

- Cards: `backdrop-filter: blur(12px)` with semi-transparent backgrounds
- Sidebar active: glowing left border via `box-shadow: inset 3px 0 8px var(--accent-glow)`
- Buttons: CSS variable-based colors, no `!important` overrides
- Staggered card fade-in on page load
- Subtle pulse animation on Run button when model is running
- Header: slow animated gradient shift (20s cycle)

## Structural Changes

### Dashboard (col_widths 2/2/8 -> redesigned)
- Full-width horizontal status bar: build status | executable | last run | timer
- Below: 50/50 split for Quick Actions card and Run Log card
- Simulation config summary becomes compact horizontal bar

### Parameters (col_widths 4/8 -> full width)
- Category selector becomes horizontal pill/tab bar
- Parameter list gets full width
- Each parameter row: name + description + value on aligned grid

### Header Bar
- Subtle gradient background
- Version badge
- Water-wave SVG decoration along bottom edge

## What Gets Removed

- All `ui.tags.style(...)` inline CSS blocks from app.py
- `shinyswatch` dependency and all theme-switching logic
- `get_theme_css()`, `get_saved_theme()`, `AVAILABLE_THEMES`
- Settings offcanvas theme selector, `dynamic_theme_css` output
- `apply_theme` button, `.aquabc_theme` file handling

## What Gets Created

- `shiny_app/www/aquabc.css` — single external stylesheet
- Google Fonts `<link>` tag in page head

## What Stays the Same

- All 13 navigation panel IDs and conditional logic
- All reactive server logic (`@reactive`, `@render`, `@effect`)
- All Python imports and backend logic
- Sidebar collapse mechanism and JavaScript
- All offcanvas panels (help, changelog) — restyled only
- All Plotly chart outputs
